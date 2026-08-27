#!/usr/bin/env python3
"""Create Japanese subtitle sidecars from Japanese audio in video files.

The Whisper model is loaded once and reused for the whole folder. Existing
video, audio, and embedded English subtitle streams are never modified.

System tools required by either method:
    sudo apt install python3-venv ffmpeg mkvtoolnix mpv

Method 1 - install one reusable command (run from this file's folder):
    mkdir -p ~/.local/share/japanese-subs
    cp japanese_subs.py ~/.local/share/japanese-subs/
    python3 -m venv ~/.local/share/japanese-subs/.venv
    ~/.local/share/japanese-subs/.venv/bin/python -m pip install faster-whisper

Add this line to ~/.bashrc, then open a new terminal:
    alias japanese-subs="$HOME/.local/share/japanese-subs/.venv/bin/python $HOME/.local/share/japanese-subs/japanese_subs.py"

Run from a folder containing videos:
    japanese-subs

Or process another folder and its subfolders:
    japanese-subs /path/to/episodes --recursive

Method 2 - local setup in the current video/script folder:
    python3 -m venv .venv
    ./.venv/bin/python -m pip install faster-whisper
    ./.venv/bin/python japanese_subs.py

Useful options:
    --recursive             Include subfolders
    --test [SECONDS]        Create a short .test.srt sample (default: 60s)
    --mux                   Also create bilingual MKV files
    --sync-offset SECONDS   Shift subtitles later (+) or earlier (-)
    --model large-v3        Maximum accuracy, but requires much more memory
    --device cuda --compute-type float16   Use a compatible NVIDIA GPU
"""

from __future__ import annotations

import argparse
import json
import math
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Optional


VIDEO_EXTENSIONS = {".mp4", ".mkv", ".m4v", ".webm"}
SRT_TIMING = re.compile(
    r"(?m)^\d{2,}:\d{2}:\d{2},\d{3} --> \d{2,}:\d{2}:\d{2},\d{3}\s*$"
)


@dataclass(frozen=True)
class Job:
    video: Path
    subtitle: Path
    muxed_video: Path


@dataclass(frozen=True)
class AudioStream:
    """An audio stream numbered as ffmpeg's 0:a:N selector expects."""

    audio_index: int
    container_index: int
    language: str
    title: str
    is_default: bool
    start_time: float


@dataclass(frozen=True)
class SubtitleCue:
    start: float
    end: float
    text: str


class ActivityReporter:
    """Print a periodic heartbeat for work that exposes no measurable progress."""

    def __init__(
        self,
        message: str,
        ongoing_message: str,
        interval: float = 60.0,
    ) -> None:
        self.message = message
        self.ongoing_message = ongoing_message
        self.interval = interval
        self.started = 0.0
        self.stop_event = threading.Event()
        self.thread: Optional[threading.Thread] = None

    def __enter__(self) -> "ActivityReporter":
        self.started = time.monotonic()
        print(self.message, flush=True)
        self.thread = threading.Thread(target=self._report, daemon=True)
        self.thread.start()
        return self

    def _report(self) -> None:
        while not self.stop_event.wait(self.interval):
            elapsed = format_duration(time.monotonic() - self.started)
            print(
                f"{self.ongoing_message} | elapsed {elapsed} | ETA unavailable",
                flush=True,
            )

    def __exit__(self, _type: object, _value: object, _traceback: object) -> None:
        self.stop_event.set()
        if self.thread is not None:
            self.thread.join()


def format_duration(seconds: float) -> str:
    if 0 < seconds < 1:
        return f"{seconds:.1f}s"
    total_seconds = max(0, round(seconds))
    hours, remainder = divmod(total_seconds, 3600)
    minutes, secs = divmod(remainder, 60)
    if hours:
        return f"{hours}h {minutes:02d}m {secs:02d}s"
    if minutes:
        return f"{minutes}m {secs:02d}s"
    return f"{secs}s"


def positive_seconds(value: str) -> float:
    try:
        seconds = float(value)
    except ValueError as error:
        raise argparse.ArgumentTypeError("must be a number of seconds") from error
    if not math.isfinite(seconds) or seconds <= 0:
        raise argparse.ArgumentTypeError("must be greater than zero")
    return seconds


def progress_segments(
    segments: Iterable[object], total_duration: float
) -> Iterable[object]:
    """Report clear, rounded transcription progress at most once per minute."""
    total_duration = max(0.0, float(total_duration))
    processed_seconds = 0.0
    started = time.monotonic()
    stop_event = threading.Event()
    completed = False

    total_label = format_duration(total_duration) if total_duration else "unknown"
    print(
        f"  Transcribing Japanese audio (media duration: {total_label})...",
        flush=True,
    )

    def report() -> None:
        while not stop_event.wait(60.0):
            elapsed = time.monotonic() - started
            processed = min(processed_seconds, total_duration) if total_duration else 0
            if processed > 0 and total_duration > 0:
                percent = min(99, round(processed * 100 / total_duration))
                speed = processed / max(elapsed, 0.001)
                remaining = max(0.0, total_duration - processed) / max(speed, 0.001)
                print(
                    f"  Progress: {percent}% | audio "
                    f"{format_duration(processed)}/{format_duration(total_duration)} "
                    f"| elapsed {format_duration(elapsed)} "
                    f"| ETA {format_duration(remaining)}",
                    flush=True,
                )
            else:
                print(
                    "  Progress: waiting for first chunk "
                    f"| elapsed {format_duration(elapsed)} | ETA unavailable",
                    flush=True,
                )

    thread = threading.Thread(target=report, daemon=True)
    thread.start()
    try:
        for segment in segments:
            try:
                processed_seconds = max(processed_seconds, float(segment.end))
            except (AttributeError, TypeError, ValueError):
                pass
            yield segment
        completed = True
    finally:
        stop_event.set()
        thread.join()
        if completed:
            elapsed = time.monotonic() - started
            print(
                f"  Transcription complete | elapsed {format_duration(elapsed)}",
                flush=True,
            )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Transcribe Japanese audio with Faster-Whisper, keeping the original "
            "video and embedded English subtitles untouched."
        )
    )
    parser.add_argument(
        "input",
        nargs="?",
        default=".",
        type=Path,
        help="A video file or folder of videos (default: current folder).",
    )
    parser.add_argument(
        "--model",
        default="turbo",
        help=(
            "Whisper model name (default: turbo; use large-v3 only when maximum "
            "accuracy matters and sufficient memory is available)."
        ),
    )
    parser.add_argument(
        "--device",
        choices=("cpu", "cuda", "auto"),
        default="cpu",
        help="Inference device (default: cpu). Use cuda for an NVIDIA GPU.",
    )
    parser.add_argument(
        "--compute-type",
        default=None,
        help=(
            "CTranslate2 compute type. Defaults to int8 on CPU, float16 on CUDA, "
            "or auto with --device auto."
        ),
    )
    parser.add_argument(
        "--subs-dir",
        type=Path,
        help=(
            "Optional subtitle output folder. By default each SRT is written "
            "beside its video with exactly the same basename."
        ),
    )
    parser.add_argument(
        "--recursive",
        action="store_true",
        help="Search subfolders and preserve their relative layout in the output.",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Replace existing generated SRT/MKV outputs.",
    )
    parser.add_argument(
        "--no-vad",
        action="store_true",
        help="Disable voice-activity filtering (try this if quiet lines are missed).",
    )
    parser.add_argument(
        "--initial-prompt",
        help="Optional Japanese names/terms that Whisper should spell consistently.",
    )
    parser.add_argument(
        "--audio-stream",
        type=int,
        metavar="N",
        help=(
            "Zero-based audio stream to transcribe (as shown by this script). "
            "By default, a sole stream is used; with multiple streams, a uniquely "
            "tagged Japanese stream is selected. Requires ffmpeg/ffprobe."
        ),
    )
    parser.add_argument(
        "--sync-offset",
        type=float,
        default=0.0,
        metavar="SECONDS",
        help=(
            "Shift all generated subtitle times by this many seconds: positive "
            "values display later, negative values earlier (default: 0)."
        ),
    )
    parser.add_argument(
        "--test",
        nargs="?",
        const=60.0,
        type=positive_seconds,
        metavar="SECONDS",
        help=(
            "Quickly transcribe only the beginning of each video and write a "
            "separate .test.srt file (default sample: 60 seconds)."
        ),
    )
    parser.add_argument(
        "--mux",
        action=argparse.BooleanOptionalAction,
        default=False,
        help=(
            "Also remux each original and Japanese SRT into a new bilingual MKV "
            "(default: disabled)."
        ),
    )
    parser.add_argument(
        "--mux-dir",
        type=Path,
        help="MKV output folder (default: INPUT/bilingual).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show discovered inputs and outputs without transcribing or muxing.",
    )
    return parser.parse_args()


def default_compute_type(device: str) -> str:
    if device == "cpu":
        return "int8"
    if device == "cuda":
        return "float16"
    return "auto"


def is_inside(path: Path, directory: Path) -> bool:
    try:
        path.resolve().relative_to(directory.resolve())
        return True
    except ValueError:
        return False


def discover_videos(
    input_path: Path,
    recursive: bool,
    excluded_dirs: Iterable[Path],
) -> list[Path]:
    input_path = input_path.expanduser().resolve()
    if input_path.is_file():
        if input_path.suffix.lower() not in VIDEO_EXTENSIONS:
            raise ValueError(f"Unsupported video extension: {input_path.suffix}")
        return [input_path]
    if not input_path.is_dir():
        raise FileNotFoundError(f"Input does not exist: {input_path}")

    candidates = input_path.rglob("*") if recursive else input_path.iterdir()
    excluded = [folder.expanduser().resolve() for folder in excluded_dirs]
    videos = [
        path.resolve()
        for path in candidates
        if path.is_file()
        and path.suffix.lower() in VIDEO_EXTENSIONS
        and not path.name.casefold().endswith(".bilingual.mkv")
        and not any(is_inside(path, folder) for folder in excluded)
    ]
    return sorted(videos, key=lambda path: str(path).casefold())


def make_jobs(
    videos: Iterable[Path],
    input_root: Path,
    subs_root: Optional[Path],
    mux_root: Path,
    test_mode: bool = False,
) -> list[Job]:
    jobs: list[Job] = []
    subtitle_sources: dict[Path, Path] = {}
    mux_sources: dict[Path, Path] = {}
    for video in videos:
        if input_root.is_dir():
            try:
                relative_parent = video.parent.relative_to(input_root)
            except ValueError as error:
                raise ValueError(
                    f"Video resolves outside the input folder: {video}"
                ) from error
        else:
            relative_parent = Path()
        subtitle_name = f"{video.stem}{'.test' if test_mode else ''}.srt"
        subtitle = (
            video.with_name(subtitle_name)
            if subs_root is None
            else subs_root / relative_parent / subtitle_name
        )
        muxed = mux_root / relative_parent / f"{video.stem}.bilingual.mkv"

        if subtitle in subtitle_sources:
            raise ValueError(
                "Two videos would write the same subtitle file: "
                f"{subtitle_sources[subtitle]} and {video}"
            )
        if muxed in mux_sources:
            raise ValueError(
                "Two videos would write the same MKV file: "
                f"{mux_sources[muxed]} and {video}"
            )
        subtitle_sources[subtitle] = video
        mux_sources[muxed] = video
        jobs.append(Job(video=video, subtitle=subtitle, muxed_video=muxed))
    return jobs


def srt_timestamp(seconds: float) -> str:
    if not math.isfinite(seconds):
        raise ValueError(f"Invalid non-finite subtitle timestamp: {seconds}")
    total_ms = max(0, round(seconds * 1000))
    hours, remainder = divmod(total_ms, 3_600_000)
    minutes, remainder = divmod(remainder, 60_000)
    secs, milliseconds = divmod(remainder, 1000)
    return f"{hours:02d}:{minutes:02d}:{secs:02d},{milliseconds:03d}"


def srt_is_usable(subtitle: Path) -> bool:
    """Reject empty, unreadable, or obviously incomplete existing SRT files."""
    try:
        if not subtitle.is_file() or subtitle.stat().st_size == 0:
            return False
        with subtitle.open("r", encoding="utf-8-sig") as source:
            sample = source.read(65_536)
    except (OSError, UnicodeError):
        return False
    return SRT_TIMING.search(sample) is not None


def normalize_subtitle_text(text: str) -> str:
    return " ".join(text.replace("\n", " ").split()).strip()


def join_word_texts(parts: Iterable[str]) -> str:
    # Faster-Whisper word strings include any required leading spaces. Joining
    # first preserves spaces in Latin text without adding spaces to Japanese.
    return normalize_subtitle_text("".join(parts))


def wrap_subtitle_text(text: str, max_line_characters: int = 16) -> str:
    """Wrap a cue onto at most two balanced lines without changing its text."""
    text = normalize_subtitle_text(text)
    if len(text) <= max_line_characters:
        return text

    # Cues are limited to 28 characters below, so a break in this range keeps
    # both lines within the 16-character display target. Prefer punctuation or
    # whitespace, but Japanese can also be safely wrapped between characters.
    first_possible = max(1, len(text) - max_line_characters)
    last_possible = min(max_line_characters, len(text) - 1)
    target = len(text) / 2
    # A fallback segment without word timings can exceed the normal 28-character
    # cue limit. In that rare case, balance two lines instead of failing.
    positions = (
        range(first_possible, last_possible + 1)
        if first_possible <= last_possible
        else range(1, len(text))
    )
    natural_breaks = [
        position
        for position in positions
        if text[position - 1] in "、。，．！？!?…；;：:"
        or text[position - 1].isspace()
        or text[position].isspace()
    ]
    if natural_breaks:
        choices = natural_breaks
    elif first_possible <= last_possible:
        choices = list(range(first_possible, last_possible + 1))
    else:
        choices = list(range(1, len(text)))
    split_at = min(choices, key=lambda position: abs(position - target))
    left = text[:split_at].rstrip()
    right = text[split_at:].lstrip()
    if not left or not right:
        return text
    return f"{left}\n{right}"


def subtitle_cues(
    segments: Iterable[object], timestamp_offset: float = 0.0
) -> Iterable[SubtitleCue]:
    """Build short cues from word timestamps instead of coarse raw segments."""
    max_duration = 5.0
    max_characters = 28
    # A short silence is a useful, non-destructive hint that a speaker turn may
    # have occurred. It can split one speaker's speech too, but never invents a
    # speaker label or changes the recognized words.
    break_gap = 0.55
    sentence_endings = ("。", "！", "？", "!", "?")

    for segment in segments:
        timed_words: list[tuple[float, float, str]] = []
        for word in getattr(segment, "words", None) or []:
            raw_start = getattr(word, "start", None)
            raw_end = getattr(word, "end", None)
            if raw_start is None or raw_end is None:
                continue
            try:
                start = float(raw_start)
                end = float(raw_end)
            except (TypeError, ValueError):
                continue
            text = str(getattr(word, "word", ""))
            if not math.isfinite(start) or not math.isfinite(end) or not text.strip():
                continue
            timed_words.append((start, max(end, start + 0.001), text))

        if not timed_words:
            text = normalize_subtitle_text(str(getattr(segment, "text", "")))
            if text:
                raw_start = float(getattr(segment, "start")) + timestamp_offset
                start = max(0.0, raw_start)
                end = float(getattr(segment, "end")) + timestamp_offset
                yield SubtitleCue(
                    start,
                    max(end, start + 0.001),
                    wrap_subtitle_text(text),
                )
            continue

        group: list[tuple[float, float, str]] = []

        def make_cue(words: list[tuple[float, float, str]]) -> SubtitleCue:
            raw_start = words[0][0] + timestamp_offset
            start = max(0.0, raw_start)
            end = words[-1][1] + timestamp_offset
            return SubtitleCue(
                start=start,
                end=max(end, start + 0.001),
                text=wrap_subtitle_text(
                    join_word_texts(item[2] for item in words)
                ),
            )

        for word in timed_words:
            if group:
                gap = word[0] - group[-1][1]
                candidate_text = join_word_texts(
                    [item[2] for item in group] + [word[2]]
                )
                candidate_duration = word[1] - group[0][0]
                if (
                    gap >= break_gap
                    or candidate_duration > max_duration
                    or len(candidate_text) > max_characters
                ):
                    yield make_cue(group)
                    group = []

            group.append(word)
            current_text = join_word_texts(item[2] for item in group)
            current_duration = group[-1][1] - group[0][0]
            if current_text.endswith(sentence_endings) and current_duration >= 1.0:
                yield make_cue(group)
                group = []

        if group:
            yield make_cue(group)


def write_srt_atomic(subtitle: Path, cues: Iterable[SubtitleCue]) -> int:
    subtitle.parent.mkdir(parents=True, exist_ok=True)
    temporary = subtitle.with_suffix(subtitle.suffix + ".tmp")
    cue_count = 0

    try:
        # The UTF-8 BOM makes encoding unambiguous to SMPlayer and older players.
        with temporary.open("w", encoding="utf-8-sig", newline="\n") as output:
            for cue in cues:
                if not cue.text:
                    continue
                cue_count += 1
                output.write(
                    f"{cue_count}\n"
                    f"{srt_timestamp(cue.start)} --> {srt_timestamp(cue.end)}\n"
                    f"{cue.text}\n\n"
                )

        if cue_count == 0:
            raise RuntimeError("Whisper returned no subtitle cues")
        temporary.replace(subtitle)
    except Exception:
        temporary.unlink(missing_ok=True)
        raise

    return cue_count


def probe_audio_streams(video: Path) -> list[AudioStream]:
    if shutil.which("ffprobe") is None:
        raise RuntimeError(
            "ffprobe was not found (install the ffmpeg package on Linux Mint)"
        )

    command = [
        "ffprobe",
        "-v",
        "error",
        "-select_streams",
        "a",
        "-show_entries",
        (
            "stream=index,start_time:stream_tags=language,title:"
            "stream_disposition=default"
        ),
        "-of",
        "json",
        str(video),
    ]
    try:
        result = subprocess.run(
            command,
            check=True,
            capture_output=True,
            text=True,
        )
        payload = json.loads(result.stdout)
    except subprocess.CalledProcessError as error:
        detail = (error.stderr or "ffprobe could not read the file").strip()
        raise RuntimeError(f"Could not inspect audio streams: {detail}") from error
    except (json.JSONDecodeError, TypeError, KeyError) as error:
        raise RuntimeError("ffprobe returned invalid audio-stream information") from error

    streams: list[AudioStream] = []
    for audio_index, raw in enumerate(payload.get("streams", [])):
        tags = raw.get("tags") or {}
        disposition = raw.get("disposition") or {}
        streams.append(
            AudioStream(
                audio_index=audio_index,
                container_index=int(raw.get("index", audio_index)),
                language=str(tags.get("language", "und")),
                title=str(tags.get("title", "")),
                is_default=bool(disposition.get("default", 0)),
                start_time=parse_optional_float(raw.get("start_time")),
            )
        )
    return streams


def parse_optional_float(value: object) -> float:
    try:
        parsed = float(value)
    except (TypeError, ValueError):
        return 0.0
    return parsed if math.isfinite(parsed) else 0.0


def describe_audio_stream(stream: AudioStream) -> str:
    details = [f"language={stream.language or 'und'}"]
    if stream.title:
        details.append(f"title={stream.title!r}")
    if stream.is_default:
        details.append("default")
    if abs(stream.start_time) >= 0.001:
        details.append(f"timeline start={stream.start_time:+.3f}s")
    return f"audio #{stream.audio_index} ({', '.join(details)})"


def looks_japanese(stream: AudioStream) -> bool:
    language = stream.language.casefold().replace("_", "-")
    title = stream.title.casefold()
    return (
        language in {"ja", "jp", "jpn", "japanese"}
        or language.startswith("ja-")
        or "japanese" in title
        or "日本語" in stream.title
    )


def choose_audio_stream(video: Path, requested: Optional[int]) -> AudioStream:
    streams = probe_audio_streams(video)
    if not streams:
        raise RuntimeError("No audio stream was found")

    if requested is not None:
        if requested < 0:
            raise ValueError("--audio-stream cannot be negative")
        if requested >= len(streams):
            choices = "; ".join(describe_audio_stream(item) for item in streams)
            raise ValueError(
                f"--audio-stream {requested} does not exist. Available: {choices}"
            )
        return streams[requested]

    if len(streams) == 1:
        return streams[0]

    japanese = [item for item in streams if looks_japanese(item)]
    if len(japanese) == 1:
        return japanese[0]
    default_japanese = [item for item in japanese if item.is_default]
    if len(default_japanese) == 1:
        return default_japanese[0]

    choices = "; ".join(describe_audio_stream(item) for item in streams)
    reason = (
        "none is uniquely tagged as Japanese"
        if not japanese
        else "more than one is tagged as Japanese"
    )
    raise RuntimeError(
        f"This file has multiple audio streams and {reason}. "
        f"Choose one with --audio-stream N. Available: {choices}"
    )


def selected_audio(
    video: Path, stream: Optional[int], test_seconds: Optional[float] = None
) -> tuple[Optional[tempfile.TemporaryDirectory], AudioStream]:
    """Return selected 16 kHz audio when selection or clipping requires it."""
    chosen = choose_audio_stream(video, stream)
    print(f"  Using {describe_audio_stream(chosen)}")
    if chosen.audio_index == 0 and test_seconds is None:
        return None, chosen
    if shutil.which("ffmpeg") is None:
        raise RuntimeError(
            "ffmpeg was not found (install the ffmpeg package on Linux Mint)"
        )

    temporary = tempfile.TemporaryDirectory(prefix="one-pace-audio-")
    wav = Path(temporary.name) / "selected.wav"
    command = [
        "ffmpeg",
        "-nostdin",
        "-hide_banner",
        "-loglevel",
        "error",
        "-i",
        str(video),
        "-map",
        f"0:a:{chosen.audio_index}",
        "-vn",
        "-ac",
        "1",
        "-ar",
        "16000",
    ]
    if test_seconds is not None:
        print(
            f"  Test mode: extracting the first {format_duration(test_seconds)}."
        )
        command.extend(["-t", f"{test_seconds:.3f}"])
    command.extend(
        [
            "-y",
            str(wav),
        ]
    )
    try:
        subprocess.run(command, check=True)
    except Exception:
        temporary.cleanup()
        raise
    return temporary, chosen


def transcribe_job(model: object, job: Job, args: argparse.Namespace) -> int:
    audio_temp, chosen = selected_audio(job.video, args.audio_stream, args.test)
    try:
        audio_source = (
            Path(audio_temp.name) / "selected.wav" if audio_temp else job.video
        )
        transcribe_options = {
            "language": "ja",
            "task": "transcribe",
            "beam_size": 5,
            "vad_filter": not args.no_vad,
            "condition_on_previous_text": True,
            "initial_prompt": args.initial_prompt,
            "word_timestamps": True,
        }
        segments, info = model.transcribe(str(audio_source), **transcribe_options)
        timestamp_offset = chosen.start_time + args.sync_offset
        if abs(timestamp_offset) >= 0.001:
            print(
                f"  Applying timeline offset {timestamp_offset:+.3f}s "
                "(media timing plus --sync-offset)."
            )
        measured_segments = progress_segments(segments, float(info.duration))
        cues = subtitle_cues(measured_segments, timestamp_offset)
        return write_srt_atomic(job.subtitle, cues)
    finally:
        if audio_temp is not None:
            audio_temp.cleanup()


def remux(job: Job, model_name: str, overwrite: bool) -> None:
    if shutil.which("mkvmerge") is None:
        raise RuntimeError("--mux requires mkvmerge (install the mkvtoolnix package)")

    job.muxed_video.parent.mkdir(parents=True, exist_ok=True)
    if job.muxed_video.exists():
        if not overwrite:
            print(f"  MKV already exists; skipping: {job.muxed_video}")
            return

    # Write beside the destination and replace only after mkvmerge succeeds. This
    # preserves an existing good output if remuxing is interrupted or fails.
    with tempfile.TemporaryDirectory(
        prefix=".japanese-subs-mux-", dir=job.muxed_video.parent
    ) as temporary_dir:
        temporary_output = Path(temporary_dir) / job.muxed_video.name
        command = [
            "mkvmerge",
            "-o",
            str(temporary_output),
            str(job.video),
            "--language",
            "0:jpn",
            "--track-name",
            f"0:Japanese (Whisper {model_name})",
            "--default-track-flag",
            "0:0",
            str(job.subtitle),
        ]
        result = subprocess.run(command, check=False)
        # mkvmerge uses 0 for success, 1 for success with warnings, and 2 for error.
        if result.returncode >= 2:
            raise RuntimeError(f"mkvmerge failed with exit code {result.returncode}")
        if not temporary_output.is_file() or temporary_output.stat().st_size == 0:
            raise RuntimeError("mkvmerge did not create a usable output file")
        if result.returncode == 1:
            print("  mkvmerge completed with warnings; see its output above.")
        temporary_output.replace(job.muxed_video)


def main() -> int:
    args = parse_args()
    if args.test is not None and args.mux:
        print("Test mode creates a sample SRT only; --mux will be ignored.")
        args.mux = False
    input_path = args.input.expanduser().resolve()
    input_root = input_path if input_path.is_dir() else input_path.parent
    subs_root = (
        args.subs_dir.expanduser().resolve() if args.subs_dir is not None else None
    )
    mux_root = (args.mux_dir or input_root / "bilingual").expanduser().resolve()
    compute_type = args.compute_type or default_compute_type(args.device)

    try:
        videos = discover_videos(
            input_path,
            recursive=args.recursive,
            excluded_dirs=(
                folder
                for folder in (subs_root, mux_root)
                if folder is not None
                if folder != input_root and is_inside(folder, input_root)
            ),
        )
        jobs = make_jobs(
            videos,
            input_root,
            subs_root,
            mux_root,
            test_mode=args.test is not None,
        )
    except (FileNotFoundError, OSError, ValueError) as error:
        print(f"Error: {error}", file=sys.stderr)
        return 2

    if not videos:
        print("No supported videos found.", file=sys.stderr)
        return 1

    print(f"Found {len(jobs)} video(s).")
    if subs_root is None:
        print("Japanese subtitles: beside each video, with the same basename")
    else:
        print(f"Japanese subtitles: {subs_root}")
    if args.mux:
        print(f"Bilingual MKVs:     {mux_root}")

    if args.dry_run:
        for job in jobs:
            print(f"\nVideo: {job.video}")
            print(f"  SRT: {job.subtitle}")
            if args.mux:
                print(f"  MKV: {job.muxed_video}")
        return 0

    needs_transcription = [
        job for job in jobs if args.overwrite or not srt_is_usable(job.subtitle)
    ]

    model = None
    if needs_transcription:
        try:
            from faster_whisper import WhisperModel
        except ImportError:
            print(
                "Error: faster-whisper is not installed in this Python environment.\n"
                "Run: python -m pip install faster-whisper",
                file=sys.stderr,
            )
            return 2

        download_hint = (
            " First use downloads about 3.1 GB; download time has no reliable ETA."
            if args.model == "large-v3"
            else " First use may need to download the model."
        )
        try:
            with ActivityReporter(
                f"Preparing the {args.model} model on {args.device} with compute "
                f"type {compute_type}.{download_hint}",
                (
                    "  Model setup still running (download/cache/load)"
                ),
                interval=60.0,
            ):
                model = WhisperModel(
                    args.model,
                    device=args.device,
                    compute_type=compute_type,
                )
            print("Model ready.")
        except Exception as error:
            print(f"Could not load Whisper model: {error}", file=sys.stderr)
            return 2

    failures = 0
    for index, job in enumerate(jobs, start=1):
        print(f"\n[{index}/{len(jobs)}] {job.video.name}")
        try:
            if args.overwrite or not srt_is_usable(job.subtitle):
                assert model is not None
                cue_count = transcribe_job(model, job, args)
                print(f"  Wrote {cue_count} cues: {job.subtitle}")
            else:
                print(f"  SRT already exists; reusing: {job.subtitle}")

            if args.mux:
                remux(job, args.model, args.overwrite)
                if job.muxed_video.exists():
                    print(f"  MKV ready: {job.muxed_video}")
        except Exception as error:
            failures += 1
            print(f"  FAILED: {error}", file=sys.stderr)

    succeeded = len(jobs) - failures
    print(f"\nFinished: {succeeded} succeeded, {failures} failed.")
    if succeeded:
        print(
            "For mpv, select English as --sid and Japanese as --secondary-sid; "
            "the secondary subtitle is displayed at the top."
        )
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())

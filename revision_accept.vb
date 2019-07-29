' stops tracking, accepts all tracked changes and colors them red

Sub tracked_to_red()
    ActiveDocument.TrackRevisions = False
    For Each storyRange In ActiveDocument.StoryRanges
        For Each Change In storyRange.Revisions
            Set myRange = Change.Range
            myRange.Revisions.AcceptAll
            myRange.Font.Color = wdColorRed
        Next
    Next
End Sub
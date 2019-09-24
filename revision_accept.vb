' stops tracking, accepts all tracked changes and colors them red
' open MS Word document, press Alt-F11 to open VBA, open new module
' copy-paste the code below and run with F5 (or select "Run" in menu)

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

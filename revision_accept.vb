' stops tracking, accepts all tracked changes and colors them blue
' open MS Word document, press Alt-F11 to open VBA, open new module
' copy-paste the code below and run with F5 (or select "Run" in menu)
' after running, delete the macro before (or when) saving the file
' (note: you can easily change color, e.g. use wdColorRed for red)

Sub tracked_to_color()
    ActiveDocument.TrackRevisions = False
    For Each storyRange In ActiveDocument.StoryRanges
        For Each Change In storyRange.Revisions
            Set myRange = Change.Range
            myRange.Revisions.AcceptAll
            myRange.Font.Color = wdColorBlue
        Next
    Next
End Sub

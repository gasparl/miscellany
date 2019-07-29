Sub tracked_to_red()           
    ActiveDocument.TrackRevisions = False    
    For Each Change In ActiveDocument.Revisions        
        Set myRange = Change.Range
        myRange.Revisions.AcceptAll
        myRange.Font.Color = wdColorRed
    Next    
End Sub
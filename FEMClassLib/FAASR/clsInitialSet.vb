Partial Public Class clsFEM

    'Sub InitialSet(ByVal I1 As Integer, ByRef IDCase As String, ByRef lutty As Integer, ByVal WorkingDir As String)    ' YC 121219-4
    Sub InitialSet(ByVal I1 As Integer, ByRef IDCase As String, ByRef lutty As Integer, ByVal WorkingDir As String, ByVal Filenameonly As String)


        Dim FileName As String


        'FileName$ = WorkingDir & "\FAASR3d.txt" ' YC 121219-4
        FileName$ = WorkingDir & "\" & "FAASR3d" & "-" & Filenameonly & ".txt"


        lutty = 59
        FileOpen(lutty, FileName, OpenMode.Output, , , 1024)
        If I1 = 1 Then
            IDCase = "1DSYM"
        ElseIf I1 = 2 Then
            IDCase = "2DSYM"
        ElseIf I1 = 3 Then
            IDCase = "3DSYM"
        ElseIf I1 = 4 Then
            IDCase = "4DNSY"
        ElseIf I1 = 5 Then
            IDCase = "5DSYM"
        End If

    End Sub

End Class

Partial Public Class clsFEM

    Sub WinYield(ByRef StopFEDFAA As Short, ByVal FEDFAAStopped As Short)

        Dim I, J As Short
        Dim GlobalFindAtom As String = "StopFEDFAA"
        StopFEDFAA = 0
        I = GlobalFindAtomA(GlobalFindAtom)

        If FEDFAAStopped = -1 Then
            J = GlobalDeleteAtomA(I)
            Exit Sub
        End If

        If I <> 0 Then
            I = GlobalDeleteAtomA(I)
            StopFEDFAA = -1
            Exit Sub
        End If

    End Sub
End Class

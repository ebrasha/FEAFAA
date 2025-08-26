Partial Public Class clsFEM

    Declare Function GlobalFindAtomA Lib "kernel32.dll" (ByVal GlobalFindAtom As String) As Short
    Declare Function GlobalDeleteAtomA Lib "kernel32.dll" (ByVal GlobalDeleteAtom As Short) As Short

    Sub DelAtom(ByRef StopFEDFAA As Short, ByRef FEDFAAStopped As Short)

        Dim I, J As Short
        Dim GlobalFindAtom As String = "StopFEDFAA"

        J = GlobalFindAtomA(GlobalFindAtom)

        Do While J <> 0
            If J <> 0 Then
                I = GlobalDeleteAtomA(J)
            End If
            J = GlobalFindAtomA(GlobalFindAtom)
        Loop

        StopFEDFAA = 0
        FEDFAAStopped = 0

    End Sub

End Class


Partial Public Class clsInput

    Sub matin(ByRef lhex() As Integer, ByRef rdc(,) As Double, ByRef incomp() As Integer)
        Dim ncon(60) As Integer

        Dim bulkmx As Double
        ReDim clsCom.rdc(4, clsCom.nmmat), clsCom.incomp(clsCom.nmmat)
        Call com.ancon(ncon)
        For J = 1 To clsCom.nmmat

            If clsCom.matype(J) <> 56 Then
                If clsCom.matype(J) <> 1 And clsCom.matype(J) <> 4 And J <> clsCom.nmmat Then clsCom.den(J) = 0
                clsCom.incomp(J) = 1
            Else
                clsCom.den(J) = clsCom.den(J) / 100000000
                clsCom.incomp(J) = 0
            End If

            bulkmx = 0 : clsCom.rdc(1, J) = 0 : clsCom.rdc(2, J) = 0
            lhex(1) = clsCom.matype(J)
            Dim model As Integer = lhex(1)
            lhex(9) = Math.Max(ncon(model), lhex(9))
            Call blkmax(clsCom.matype, J, clsCom.prop, bulkmx, clsCom.rdc(3, J), clsCom.rdc(4, J))
            If clsCom.matype(J) = 13 Then clsCom.matype(J) = 7
        Next J
        rdc = clsCom.rdc : incomp = clsCom.incomp
    End Sub

End Class

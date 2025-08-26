Partial Public Class clsFEM

    Sub Setstep2(ByVal IStep As Integer, ByVal nload0 As Integer, ByVal nod0() As Integer, ByVal idirn0() As Integer, ByVal ncur0() As Integer, ByVal fac0() As Double)
        Dim I, J, nload As Integer
        ' clsCnlds
        Dim nod(nload0), idirn(nload0), ncur(nload0) As Integer
        Dim fac(nload0) As Double
        J = 1
        For I = 1 To nload0
            If ncur0(I) = IStep Then
                nod(J) = nod0(I)
                idirn(J) = idirn0(I)
                ncur(J) = 1
                fac(J) = fac0(I)
                J = J + 1
            End If
        Next
        nload = J - 1
        clsCom.nload = nload
        clsCom.nod = nod : clsCom.idirn = idirn : clsCom.ncur = ncur : clsCom.fac = fac
    End Sub

End Class

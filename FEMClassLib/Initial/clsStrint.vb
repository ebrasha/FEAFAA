Partial Public Class clsInitial

    Public Sub strint(ByRef nel As Integer, ByRef nblk As Integer, ByRef ihv() As Integer, ByRef ncon As Integer)

        Dim model, nn, kk As Integer
        model = lhex(1)
        nn = ihv(nblk + nel) - ihv(nblk + 1) + 1
        kk = nn + 8
        If model = 1 Or model = 56 Or model = 57 Or model = 58 Then
            Call s1int(anegb1.Skip(nn).ToArray(), ncon)
        ElseIf model = 3 Then
            Call s1int(anegb1.Skip(nn).ToArray(), ncon)
        ElseIf model = 4 Then
            'Call s4int(anegb1(nn), anegb1(nn), temo, trefm, ncon)
        ElseIf model = 6 Then
            Call s1int(anegb1.Skip(nn).ToArray(), ncon)
        Else
            ' Call com.adios(2)
        End If
    End Sub
End Class

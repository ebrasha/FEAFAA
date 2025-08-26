Partial Public Class clsPrintOut  
    Sub s6out(ByRef sig() As Double, ByRef nstart As Integer, ByRef stress() As Double)

        For i = 1 To 6
            stress(i) = sig(nstart - 1 + i)
            'nstate = 4     ' YC 102418-012819
            'ft = 0
40:     Next i

        nstate = 4   ' YC 102418-012819
        ft = 0

    End Sub
End Class




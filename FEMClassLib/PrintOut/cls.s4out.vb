Partial Public Class clsPrintOut  
    Sub s4out(ByRef sig() As Double, ByRef nstart As Integer, ByRef temp As Double)

        For i = 1 To 6
            stress(i) = sig(nstart - 1 + i)
            nstate = 5
            ft = temp
40:     Next i

    End Sub
End Class





Partial Public Class clsSolve

    Public Sub update(ByRef un2() As Double, ByVal un3() As Double, ByVal lauto As Boolean, ByVal neql As Integer)
        Dim i As Integer

        For i = 1 To neql
            un2(i) = un2(i) + un3(i)
        Next
    End Sub
End Class

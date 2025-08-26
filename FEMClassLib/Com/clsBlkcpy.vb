Partial Public Class clsCom

    Public Sub blkcpy(ByRef A() As Double, ByRef B() As Double, ByVal n As Integer)
        For I = 1 To n
            B(I) = A(I)
        Next
    End Sub
    Public Sub blkcpy(ByRef A(,) As Double, ByRef B(,) As Double, ByVal m As Integer, ByVal n As Integer)
        For I = 1 To m
            For J = 1 To n
                B(I, J) = A(I, J)
            Next
        Next
    End Sub
    Sub blkcpy(ByRef A() As Integer, ByRef B() As Double, ByVal n As Integer)
        Dim I As Integer

        For I = 1 To n
            B(I) = A(I)
        Next
    End Sub
    Sub blkcpy2D(ByRef A(,) As Double, ByRef B(,) As Double, ByVal n1 As Integer, ByVal n2 As Integer)
        Dim I1, I2 As Integer

        For I1 = 1 To n1
            For I2 = 1 To n2
                B(I1, I2) = A(I1, I2)
            Next I2
        Next I1
    End Sub
End Class

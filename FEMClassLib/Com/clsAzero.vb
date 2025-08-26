Partial Public Class clsCom

    Public Sub azero(ByRef A() As Double, ByVal n As Integer)
        For I = 1 To n
            A(I) = 0
        Next
    End Sub

    Public Sub azero(ByRef A() As Integer, ByVal n As Integer)
        For I = 1 To n
            A(I) = 0
        Next
    End Sub
    Public Sub azero(ByRef A(,) As Double, ByVal m As Integer, ByVal n As Integer)
        For I = 1 To m
            For J = 1 To n
                A(I, J) = 0
            Next
        Next
    End Sub
    Sub azero(ByRef A(,) As Integer, ByRef m As Integer, ByRef n As Integer)
        For I = 1 To m
            For J = 1 To n
                A(I, J) = 0
            Next J
        Next I
    End Sub
    Sub azero(ByRef A(,,) As Double, ByRef m As Integer, ByRef n As Integer, ByRef l As Integer)
        For I = 1 To m
            For J = 1 To n
                For K = 1 To l
                    A(I, J, K) = 0
                Next K
            Next J
        Next I
    End Sub
End Class

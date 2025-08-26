Partial Public Class clsInitial

    Public Sub s1int(ByRef wa() As Double, ByRef n As Integer)

        Dim I, ln As Integer
        ln = 8 * n
        For I = 1 To ln
            wa(I) = 0
        Next
    End Sub
End Class

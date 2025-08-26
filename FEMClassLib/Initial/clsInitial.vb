Partial Public Class clsInitial

    Public Sub itrpd2(p, dehv, npoint, dehv1, sclf, n, zr, d2d1)
    End Sub
   
    Public Function TTO(matrix As Double(,), n As Integer) As Double()
        Dim number_of_rows As Integer = matrix.GetLength(0)
        Dim values As Double() = Nothing
        For i As Integer = 0 To number_of_rows - 1
            ReDim Preserve values(i)
            values(i) = matrix(i, n)
        Next
        Return values
    End Function
End Class

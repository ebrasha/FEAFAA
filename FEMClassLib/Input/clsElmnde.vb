Partial Public Class clsInput

    Public Sub elmnde(ByRef ixde(,) As Integer, ByRef sclf() As Double, ByVal dehv(,) As Double,
                 ByRef x(,) As Double, ByRef nmelde As Integer)
        Dim i, j, k, inc, inc0, m, mprint As Integer
        i = 0
        mprint = 0
        For m = 1 To nmelde
            If sclf(m) = 0 Then sclf(m) = 1
            inc = 1
20:         i = i + 1
            If m - i > 0 Then GoTo 180
            If m - i < 0 Then
                ixde(1, i) = ixde(1, i - 1) + inc0
                ixde(2, i) = ixde(2, i - 1) + inc0
                ixde(3, i) = ixde(3, i - 1)
                sclf(i) = sclf(i - 1)
                GoTo 80
            End If
50:         inc0 = inc
80:         If mprint > 0 Then GoTo 100
            mprint = 50
100:        mprint = mprint - 1
            If m - i > 0 Then GoTo 170
            If m - i < 0 Then GoTo 20
160:        If nmelde - i >= 0 Then GoTo 180
        Next m
170:    'Call adios(2)
180:    For i = 1 To nmelde
            dehv(1, i) = 0
            dehv(2, i) = 0
            dehv(3, i) = 0
            dehv(4, i) = 0
            dehv(5, i) = 0
            dehv(6, i) = 0
            dehv(8, i) = 0
            dehv(9, i) = 0
            dehv(10, i) = 0
            j = ixde(1, i)
            k = ixde(2, i)
            dehv(7, i) = Math.Sqrt((x(1, j) - x(1, k)) ^ 2 + (x(2, j) - x(2, k)) ^ 2 + (x(3, j) - x(3, k)) ^ 2)
            dehv(11, i) = dehv(7, i)
        Next
        ' Call Check2D(dehv, 11, nmelde)
    End Sub

End Class

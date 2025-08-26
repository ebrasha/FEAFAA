Partial Public Class clsInput

    Public Sub countn(ByVal irect(,) As Integer, ByRef mnn() As Integer, ByRef n As Integer, ByVal nrt As Integer)

        Dim i, j, k As Integer

        n = 0
        If nrt = 0 Then Exit Sub
        n = 1
        mnn(1) = irect(1, 1)
        For i = 1 To nrt
            For j = 1 To 4
                For k = 1 To n
                    If mnn(k) = irect(j, i) Then GoTo 80
                Next k
                n = n + 1
                mnn(n) = irect(j, i)
80:         Next j
        Next i

    End Sub

    Public Sub azero(ByRef a() As Integer, ByRef n As Integer)

        Dim i As Integer

        For i = 1 To n
            a(i) = 0
        Next i

    End Sub
End Class

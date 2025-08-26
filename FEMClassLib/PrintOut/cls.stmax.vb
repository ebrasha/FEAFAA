Partial Public Class clsPrintOut  
    Sub stmax(ByRef nnd As Integer, ByRef ss(,) As Double, ByRef mn() As Integer, ByRef smax1 As Double, ByRef smax8 As Double)


        Dim i, l, j, im, jm As Integer

        smax1 = 0
        smax8 = 0

        For i = 1 To nnd
            If (ss(7, i) > 0) Then
                GoTo 100

                For l = 1 To 6
                    ss(l, i) = ss(l, i) / ss(7, i)
                Next l
                ss(7, i) = Math.Sqrt(Math.Abs(0.5 * ((ss(1, i) - ss(2, i)) ^ 2 + (ss(2, i) - ss(3, i)) ^ 2 + (ss(3, i) - ss(1, i)) ^ 2) + 3 * (ss(4, i) ^ 2 + ss(5, i) ^ 2 + ss(6, i) ^ 2)))

100:
                If (mn(i) = 1) Then
                    For j = 1 To 2
                        If (ss(j, i) > smax1) Then
                            smax1 = ss(j, i)
                            im = i
                            jm = j
                        End If
                    Next j
                End If

                If (mn(i) = 12) Then
                    For j = 1 To 2
                        If (ss(j, i) > smax8) Then
                            smax8 = ss(j, i)
                            im = i
                            jm = j
                        End If
                    Next j
                End If

            End If
        Next i

    End Sub
End Class




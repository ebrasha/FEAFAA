
Partial Public Class clsInput

    Sub sillin()

        Dim ntyp, numsv As Integer
        numsv = clsCom.numsv
        iacc = 0
        clsCom.iacc = iacc
        For J = 1 To numsv
            stfsf(J) = 10.0
            tdeath(J) = -1.0
            tbury(J) = -1.0
            ntyp = 3
            Dim pendn As Double = pend(J)
            If pend(J) <= 0.0 Then
                pend(J) = 1.0E+20
                pendn = -1.0
            End If
            pend(J) = -pend(J)
            If sfact(J) = 0.0 Then sfact(J) = 0.001
            If sfact(J) > 0.001 Then
                lprtbd = True
            Else
                lprtbd = False
            End If
                If tdeath(J) < 0.0 Then tdeath(J) = 9.999E+29
                If tbury(J) < 0.0 Then tbury(J) = 9.999E+29
                If ntyp = 4 Then
                    tdeath(J) = 9.999E+29
                    tbury(J) = 9.999E+29
                End If
        Next J
    End Sub

End Class

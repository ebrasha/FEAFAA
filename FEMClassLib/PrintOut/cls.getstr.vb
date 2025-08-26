Partial Public Class clsPrintOut
    Sub getstr(ByRef model As Integer, ByRef ihv() As Integer, ByRef ncon As Integer, ByRef neln As Integer, ByVal anegb1() As Double, ByRef stress() As Double)

        Dim npt, nn, mm As Integer

        npt = 8
        nn = ihv(neln) - ihv(neln - nel + 1) + 1 + (ipt - 1) * ncon
        mm = ihv(neln) - ihv(neln - nel + 1) + 1 + 8 + (ipt - 1) * (ncon - 1)

        If (model = 1 Or model = 56 Or model = 57 Or model = 58) Then
            'Call s6out(anegb1(nn))
            Call s6out(anegb1, nn, stress)
        ElseIf (model = 4) Then
            'Call s4out(anegb1(mm), anegb1(mm + 7))
            Call s4out(anegb1, mm, anegb1(mm + 7))
        ElseIf (model = 6) Then
            'Call s6out(anegb1(nn))
            Call s6out(anegb1, nn, stress)
        Else

            Dim fmt1000 As String = " *******************************************************" & NL1 &
                                    " *                 - FATAL ERROR -                     *" & NL1 &
                                    " *   Material model type " & model & " not implemented (getstr)  *" & NL1 &
                                    " *******************************************************" & NL1 & NL1 & NL1
            PrintLine(lutty, fmt1000)

            Call adios(2)
        End If

    End Sub
End Class




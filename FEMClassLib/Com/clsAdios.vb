Partial Public Class clsCom

    Sub adios(ByRef n As Integer)


        Dim unit As Double
        Dim subtot(3) As Double
        Dim i As Integer

        unit = 1.0

        If (lki = lkj) Then GoTo 10
        Call timing(cpuio, cpuip, lki, 3)

10:     If (lkj = 2) Then lki = 1
        If (lkj = 4) Then lki = 5
        If (lkj > 5) Then lki = 5
        'If (lki <> lkj) Then

        Call timing(cpuio, cpuip, lki, 3)
        Call timing(cpuio, cpuip, 20, 3)
        Call timing(cpuio, cpuip, 11, 3)

        For i = 1 To 3
            cpuio(i, 3) = cpuio(i, 3) + cpuio(i, 4)
20:     Next i

        subtot(1) = 0.0
        subtot(2) = 0.0
        subtot(3) = 0.0

        For j = 2 To 8 Step 2
            subtot(1) = subtot(1) + cpuio(1, j)
            subtot(2) = subtot(2) + cpuio(2, j)
            subtot(3) = subtot(3) + cpuio(3, j)
30:     Next j

        ' Call header(lutty)

        Dim fmt60, fmt61, fmt62 As String

        fmt60 = " T I M I N G   I N F O R M A T I O N " & NL1 &
                "                                                      cpu       sys       i/o" & NL1 & NL1 &
                "     equation reordering / element blocking       " & LPad(10, Format(cpuio(1, 2), "0.000")) & LPad(10, Format(cpuio(2, 2), "0.000")) & LPad(10, Format(cpuio(3, 2), "0.000")) & NL1 & NL1 &
                "     element and constitutive data initialization " & LPad(10, Format(cpuio(1, 3), "0.000")) & LPad(10, Format(cpuio(2, 3), "0.000")) & LPad(10, Format(cpuio(3, 3), "0.000")) & NL1 & NL1 &
                "     sliding interface data initialization        " & LPad(10, Format(cpuio(1, 12), "0.000")) & LPad(10, Format(cpuio(2, 12), "0.000")) & LPad(10, Format(cpuio(3, 12), "0.000")) & NL1 & NL1 &
                "     input file reading / initialization          " & LPad(10, Format(cpuio(1, 1), "0.000")) & LPad(10, Format(cpuio(2, 1), "0.000")) & LPad(10, Format(cpuio(3, 1), "0.000")) & NL1 & NL1 &
                "     overhead (restart file, file opening etc.)   " & LPad(10, Format(cpuio(1, 21) - (cpuio(1, 2) + cpuio(1, 3) + cpuio(1, 12) + cpuio(1, 1)), "0.000")) & LPad(10, Format(cpuio(2, 21) - (cpuio(2, 2) + cpuio(2, 3) + cpuio(2, 12) + cpuio(2, 1)), "0.000")) & LPad(10, Format(cpuio(3, 21) - (cpuio(3, 2) + cpuio(3, 3) + cpuio(3, 12) + cpuio(3, 1)), "0.000")) & NL1 &
                "                                                     _______   _______   _______" & NL1 & NL1 &
                "   i n p u t   a n d  i n i t i a l i z a t i o n " & LPad(10, Format(cpuio(1, 21), "0.000")) & LPad(10, Format(cpuio(2, 21), "0.000")) & LPad(10, Format(cpuio(3, 21), "0.000")) & NL1 & NL1 & NL1



        fmt61 = "     initial equilibrium solutions for time steps " & LPad(10, Format(cpuio(1, 20), "0.000")) & LPad(10, Format(cpuio(2, 20), "0.000")) & LPad(10, Format(cpuio(3, 20), "0.000")) & NL1 & NL1 &
                "     equilibrium iterations during time steps     " & LPad(10, Format(cpuio(1, 7), "0.000")) & LPad(10, Format(cpuio(2, 7), "0.000")) & LPad(10, Format(cpuio(3, 7), "0.000")) & NL1 & NL1 &
                "     high speed printer output                    " & LPad(10, Format(cpuio(1, 9), "0.000")) & LPad(10, Format(cpuio(2, 9), "0.000")) & LPad(10, Format(cpuio(3, 9), "0.000")) & NL1 & NL1 &
                "      plotfile generation                         " & LPad(10, Format(cpuio(1, 10), "0.000")) & LPad(10, Format(cpuio(2, 10), "0.000")) & LPad(10, Format(cpuio(3, 10), "0.000")) & NL1 &
                "                                                     _______   _______   _______" & NL1 & NL1 &
                "   s o l u t i o n   p h a s e                    " & LPad(10, Format(cpuio(1, 5), "0.000")) & LPad(10, Format(cpuio(2, 5), "0.000")) & LPad(10, Format(cpuio(3, 5), "0.000")) & NL1 & NL1 & NL1 &
                " *********************************************************************************" & NL1 &
                " * T O T A L    S O L U T I O N   T I M E         " & LPad(10, Format(cpuio(1, 11), " 0.000")) & LPad(10, Format(cpuio(2, 11), "0.000")) & LPad(10, Format(cpuio(3, 11), "0.000")) & " *" & NL1 &
                " *********************************************************************************" & NL1 & NL1 &
                "    total solution time = input and initialization + solution phase" & NL1 & NL1 &
                "    Solution phase for:" & NL1 &
                "     Linear    Analysis: one equilibrium solution per time step" & NL1 &
                "     Nonlinear Analysis: one initial equilibrium solution plus" & NL1 &
                "                         equilibium. iters. per time step" & NL1 & NL1 & NL1



        fmt62 = "   O T H E R  S U B T O T A L S" & NL1 & NL1 &
                "     sliding interfaces (searching, assembly etc.)" & LPad(10, Format(cpuio(1, 13), "0.000")) & LPad(10, Format(cpuio(2, 13), "0.000")) & LPad(10, Format(cpuio(3, 13), "0.000")) & NL1 & NL1 &
                "     solid elements (strain calcs, assembly etc.) " & LPad(10, Format(cpuio(1, 14), "0.000")) & LPad(10, Format(cpuio(2, 14), "0.000")) & LPad(10, Format(cpuio(3, 14), "0.000")) & NL1 &
                "       constitutive evaluations                   " & LPad(10, Format(cpuio(1, 15), "0.000")) & LPad(10, Format(cpuio(2, 15), "0.000")) & LPad(10, Format(cpuio(3, 15), "0.000")) & NL1 & NL1 &
                "     beam elements                                " & LPad(10, Format(cpuio(1, 16), "0.000")) & LPad(10, Format(cpuio(2, 16), "0.000")) & LPad(10, Format(cpuio(3, 16), "0.000")) & NL1 &
                "       constitutive evaluations                   " & LPad(10, Format(cpuio(1, 17), "0.000")) & LPad(10, Format(cpuio(2, 17), "0.000")) & LPad(10, Format(cpuio(3, 17), "0.000")) & NL1 & NL1 &
                "     shell elements                               " & LPad(10, Format(cpuio(1, 18), "0.000")) & LPad(10, Format(cpuio(2, 18), "0.000")) & LPad(10, Format(cpuio(3, 18), "0.000")) & NL1 &
                "       constitutive evaluations                   " & LPad(10, Format(cpuio(1, 19), "0.000")) & LPad(10, Format(cpuio(2, 19), "0.000")) & LPad(10, Format(cpuio(3, 19), "0.000")) & NL1 & NL1 &
                "     linear equation solver subtotal              " & LPad(10, Format(cpuio(1, 19), "0.000")) & LPad(10, Format(cpuio(2, 19), "0.000")) & LPad(10, Format(cpuio(3, 19), "0.000")) & NL1 &
                "       linear eq. solving for initial eq. iter.   " & LPad(10, Format(subtot(1), "0.000")) & LPad(10, Format(subtot(2), "0.000")) & LPad(10, Format(subtot(3), "0.000")) & NL1 &
                "       linear eq. solving during eq. iter.        " & LPad(10, Format(cpuio(1, 8), "0.000")) & LPad(10, Format(cpuio(2, 8), "0.000")) & LPad(10, Format(cpuio(3, 8), "0.000")) & NL1 & NL1 &
                "     linear equation solver init (column hts. etc)" & LPad(10, Format(cpuio(1, 4), "0.000")) & LPad(10, Format(cpuio(2, 4), "0.000")) & LPad(10, Format(cpuio(3, 4), "0.000")) & NL1 & NL1 & NL1

        PrintLine(lutty, fmt60)
        PrintLine(lutty, fmt61)
        PrintLine(lutty, fmt62)

        Dim nstepd, ncs As Integer
        Dim avgi As Double

        nstepd = Math.Max(1, ncstep)
        ncs = Math.Min(1600, ncstep)

        avgi = itrtot / nstepd

        Dim fmt100 As String
        fmt100 = " n o n l i n e a r   i t e r a t i o n  i n f o r m a t i o n" & NL1 &
                 "     number of time steps completed . . . . . . . . . . " & LPad(7, Format(ncstep, "0")) & NL1 & NL1 &
                 "     total number of equilibrium iterations . . . . . . " & LPad(7, Format(itrtot, "0")) & NL1 & NL1 &
                 "     average number of equilibrium iterations . . . . . " & LPad(7, Format(avgi, "0.00")) & NL1 & NL1 &
                 "     total number of stiffness formations . . . . . . . " & LPad(7, Format(irftot, "0")) & NL1 & NL1 & NL1

        PrintLine(lutty, fmt100)

        Dim icalld As Integer
        Dim avgitr As Double

        icalld = Math.Max(1, icalls)
        avgitr = numlit / icalld

        Dim fmt110 As String
        fmt110 = " i t e r a t i v e  s o l u t i o n  i n f o r m a t i o n" & NL1 &
                 "     number of calls to conjugate gradients . . . . . . " & LPad(7, Format(icalls, "0")) & NL1 & NL1 &
                 "     total number of iterations . . . . . . . . . . . . " & LPad(7, Format(numlit, "0")) & NL1 & NL1 &
                 "     average number of iterations/solve . . . . . . . . " & LPad(7, Format(avgitr, "0.0")) & NL1 & NL1 &
                 "     maximum number of iterations to solve  . . . . . . " & LPad(7, Format(maxlit, "0")) & NL1 & NL1 & NL1

        PrintLine(lutty, fmt110)

        If (ncstep = 0) Then
            avlitr(1, 1) = 1
            avlitr(2, 1) = numlit
        End If

        For i = ncs To 2 Step -1
            avlitr(2, i) = (avlitr(2, i) - avlitr(2, i - 1)) / Math.Max(avlitr(1, i), unit)
            avlitr(2, 1) = avlitr(2, 1) / Math.Max(avlitr(1, 1), unit)
35:     Next i

        'Call header(lutty)

        Dim fmt120, fmt130 As String
        fmt120 = " solution statistics"

        PrintLine(lutty, fmt120)

        For i = 1 To ncs
            fmt130 = "      step " & LPad(4, Format(i, "0")) & "   nonlinear iterations = " & LPad(5, Format(avlitr(1, i), "0.")) & "average pcg iterations = " & LPad(5, Format(avlitr(2, i), "0.0"))
            PrintLine(lutty, fmt130)
36:     Next i

        Dim tctm As Double
        tctm = cpuio(1, 11) + cpuio(2, 11) + cpuio(3, 11)

        Dim fmt90 As String
        fmt90 = "total time charged for run (w/ overhead) =" & LPad(15, Format(tctm, "0.000"))
        PrintLine(lutty, fmt90)


        If (n <> 1) Then GoTo 40
        Dim fmt70 As String
        fmt70 = " n o r m a l   t e r m i n a t i o n"
        PrintLine(lutty, fmt70)

        GoTo 50
40:     If (n <> 2) Then GoTo 50
        Dim fmt80 As String
        fmt80 = " e r r o r    t e r m i n a t i o n"
        PrintLine(lutty, fmt80)

50:     Dim phony As Double
        phony = -999999

        'Call exita(idim(n, 1)) 'idim(n,1)=n-1 if positive or 0=max(n-1,0)
        Call exita(Math.Max(n - 1, 0))


    End Sub
End Class

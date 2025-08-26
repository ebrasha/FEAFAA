

Partial Public Class clsPrintOut

    'Sub prtrs(ByVal mpri As Integer, ByRef nnel As Integer, ByVal nstep As Integer, ByRef x(,) As Double, lhex() As Integer,
    '          ByVal ncon() As Integer, ByVal idroer() As Integer, ByVal incflg As Integer, ByVal nh04 As Integer, ByVal nh05 As Integer,
    '          ByVal nblk As Integer, ByVal ihv() As Integer, ByVal numelg() As Integer, ByVal nelpg As Integer, ByVal anegb1() As Double,
    '          ByRef nprint As Integer, ByVal incpr As Integer, ByRef st(,,,) As Double, ByVal timep As Double, ByVal path As String,
    '          ByRef Stress1() As Double, ByRef Stress8() As Double, ByVal ModelOut As Integer)  'v3.0 005/YC 092820-2
    Sub prtrs(ByVal mpri As Integer, ByRef nnel As Integer, ByVal nstep As Integer, ByRef x(,) As Double, lhex() As Integer,
            ByVal ncon() As Integer, ByVal idroer() As Integer, ByVal incflg As Integer, ByVal nh04 As Integer, ByVal nh05 As Integer,
            ByVal nblk As Integer, ByVal ihv() As Integer, ByVal numelg() As Integer, ByVal nelpg As Integer, ByVal anegb1() As Double,
            ByRef nprint As Integer, ByVal incpr As Integer, ByVal timep As Double, ByVal path As String,
            ByRef Stress1() As Double, ByRef Stress8() As Double, ByVal ModelOut As Integer)



        If mpri <= 0 Then GoTo 1001 ' YC 10248-012819

        Dim FileNo As Integer, FileName As String
        If nstep = 0 Then GoTo 1001
        If ModelOut = 1 Then    ' QW 08-14-2019
            Dim s As String, ns As Integer
            If clsCom.Ntimestep = 1 Then
                For ns = 2 To 20
                    s = path & "Output-Hexahedron Element-Step " & ns & ".txt"
                    If System.IO.File.Exists(s) = True Then
                        System.IO.File.Delete(s)
                    End If
                Next
            End If

            FileNo = FreeFile()
            FileName = path & "Output-Hexahedron Element-Step " & clsCom.Ntimestep & ".txt"
            FileOpen(FileNo, FileName, OpenMode.Output)
        End If          ' QW 08-14-2019
        'numnp = 8000
        Dim ss(7, numnp) As Double
        Dim mn(numnp) As Integer
        Dim sMax1, sMax8 As Double   ' QW 04-11-2019
        Dim i, j, l As Integer

        numnp1 = numnp 'YC see prts.f    common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp1,imem,ioro
        For i = 1 To 7
            For j = 1 To numnp1
                ss(i, j) = 0
            Next j
        Next i

        Dim nloop As Integer
        Dim numel As Integer = lhex(2)

        nloop = numel
        nnelst = nnel

        nloop = numelh
        Dim n, neln, njk As Integer

        Dim ast(8, 9) As Double
        Dim mtype, newmax As Integer
        Dim bot, top, str_bot, ss11, ss88, effs2 As Double
        Dim fmt110, fmt120, fmt130, fmt140 As String

        Dim astsv(8) As String
        Dim astate() As String = {"", "elastic", "plastic", "failure", "       ", "temp"}
        Dim lelas As Boolean


        For n = 1 To nloop
            nnel = nnel + 1
            njk = n
            'nel = njk ' YC see prts.f    common/bk56/stress(6),strain(6),d(6,6),ipt,njk,nstate
            neln = nnel
            Call oocbsh(n, neln, njk, lhex(2), idroer, incflg, nh04, nh05, nblk, ihv, numelg, nelpg)
            nel = njk
            If (mpri <= 0) Then GoTo 16
            For i = 1 To lpbh
                If (nnel >= ipelem(1, i, 1) And nnel <= ipelem(2, i, 1)) Then GoTo 16
                GoTo 80
            Next i
16:


            mtype = ixh(1, neln)
            lhex(1) = matype(mtype)



            For i = 1 To 8
                ipt = i
                Call getstr(lhex(1), ihv, ncon(lhex(1)), neln, anegb1, stress)

                If (mpri <= 0) Then GoTo 50

                ast(i, 2) = stress(1)
                ast(i, 3) = stress(2)
                ast(i, 4) = stress(3)
                ast(i, 5) = stress(4)
                ast(i, 6) = stress(5)
                ast(i, 7) = stress(6)

                newmax = 1
                If (i = 8) Then

                    If (newmax = 1) Then
                        Call stnod(numnp1, neln, nloop, ixh, ast, ss, mn)
                    Else
                        If (mtype = 12) Then
                            bot = (ast(1, 3) + ast(2, 3) + ast(3, 3) + ast(4, 3)) / 4
                            top = (ast(5, 3) + ast(6, 3) + ast(7, 3) + ast(8, 3)) / 4
                            str_bot = (bot + top) / 2 + (bot - top) / 2 / 0.5773502691896

                            If (str_bot > sMax8) Then       ' QW 04-11-2019
                                sMax8 = str_bot
                                ss88 = str_bot
                                nnn8(nstep) = nnel
                            End If

                        End If

                        If (mtype = 1) Then
                            bot = (ast(1, 3) + ast(2, 3) + ast(3, 3) + ast(4, 3)) / 4
                            top = (ast(5, 3) + ast(6, 3) + ast(7, 3) + ast(8, 3)) / 4
                            str_bot = (bot + top) / 2 + (bot - top) / 2 / 0.5773502691896

                            If (str_bot > sMax1) Then       ' QW 04-11-2019
                                sMax1 = str_bot
                                ss11 = str_bot
                                nnn(nstep) = nnel
                            End If
                        End If

                    End If
                End If


                If (nprint > 0) Then GoTo 20
                nprint = 40
                If ModelOut = 1 Then    ' QW 08-14-2019
                    Call header(FileNo)

                    fmt110 = " e l e m e n t   s t r e s s   c a l c u l a t i o n s  f o r   s t e p " & LPad(5, Format(clsCom.Ntimestep, "0")) & " ( time = " & LPad(10, Format(timep, "0.0000E+00")) & " )"
                    PrintLine(FileNo, fmt110)

                    'If (nstep < 5) Then     ' YC 121219-5
                    fmt120 = " element  stress      sig-xx     sig-yy     sig-zz     sig-xy     sig-yz     sig-zx              yield" & NL1 &
                             " num/ipt   state                                                                       effsg     function"
                    PrintLine(FileNo, fmt120)
                    'End If
                End If          ' QW 08-14-2019

20:             nprint = nprint - 1
                effs2 = 0.5 * ((stress(1) - stress(2)) ^ 2 + (stress(2) - stress(3)) ^ 2 + (stress(3) - stress(1)) ^ 2) + 3.0 * (stress(4) ^ 2 + stress(5) ^ 2 + stress(6) ^ 2)
                astsv(i) = astate(nstate)
                ast(i, 8) = Math.Sqrt(Math.Abs(effs2))
                ast(i, 9) = ft

                If (ipt < 8) Then GoTo 60

                If ModelOut = 1 Then    ' QW 08-14-2019
                    'If (nstep < 5) Then     ' YC 121219-5
                    fmt130 = LPad(5, Format(nnel, "0")) & "-" & LPad(2, Format(mtype, "0"))
                    PrintLine(FileNo, fmt130)
                    'End If
                End If  ' QW 08-14-2019

                If (incflg = 1) Then
                    lelas = True
                    For l = 1 To 8
40:                     If (ast(l, 9) <> 0.0) Then lelas = False
                    Next l
                End If


                If (incflg = 1 And lelas) Then
                    If (incpr = 1) Then
                        'If (nstep < 5) Then     ' YC 121219-5
                        For l = 1 To 8
                            fmt140 = "     " & LPad(2, Format(l, "0")) & "  " & LPad(7, astsv(l)) & " "
                            For j = 2 To 9
                                fmt140 = fmt140 & LPad(11, Format(ast(l, j), "0.000E+00"))

                                If IsFAASR3DPlot And j <= 8 Then 'YC tecplot format file,YC 102418-012819
                                    st(nnel, j - 1, l, nstep) = ast(l, j)
                                End If

                            Next j
44:                         If ModelOut = 1 Then        ' QW 08-14-2019
                                PrintLine(FileNo, fmt140)
                            End If  ' QW 08-14-2019
                        Next l
                        'End If

                    End If



                    For j = 2 To 9
                        For l = 2 To 8
41:                         ast(1, j) = ast(1, j) + ast(l, j)
                        Next
                    Next j

                    For j = 2 To 9
42:                     ast(1, j) = 0.125 * ast(1, j)
                    Next j

                    l = 9
                    If ModelOut = 1 Then ' QW 08-14-2019
                        'If (nstep < 5) Then     ' YC 121219-5
                        fmt140 = "     " & LPad(2, Format(l, "0")) & "  " & LPad(7, astsv(1)) & " "
                        For j = 2 To 9
                            fmt140 = fmt140 & LPad(11, Format(ast(1, j), "0.000E+00"))
                        Next j
                        PrintLine(FileNo, fmt140)
                        'End If
                    End If    ' QW 08-14-2019
                    nprint = nprint + 7

                Else
                    If ModelOut = 1 Then ' QW 08-14-2019
                        'If (nstep < 5) Then     ' YC 121219-5
                        For l = 1 To 8
                            fmt140 = "     " & LPad(2, Format(l, "0")) & "  " & LPad(7, astsv(l)) & " "
                            For j = 2 To 9
                                fmt140 = fmt140 & LPad(11, Format(ast(l, j), "0.000E+00"))
                            Next j
43:                         PrintLine(FileNo, fmt140)
                        Next l
                        'End If
                    End If    ' QW 08-14-2019
                End If

                GoTo 60

50:             strain(1) = ft

60:         Next i

            If (kpri <= 0) Then GoTo 80

            locstr = 1

80:     Next n


        If ModelOut = 1 Then FileClose(FileNo)   ' YC 102418-012819 ' QW 08-14-2019


        If (mpri > 0 And newmax = 1) Then
            Call stmaxp(path, numnp1, x, ss, mn, nstep, ModelOut)
            'Call stmax(numnp1, ss, mn, sMaxStress1(nstep), sMaxStress8(nstep))
            Call stmax(numnp1, ss, mn, sMax1, sMax8)  ' QW 04-11-2019
            'Stress1(clsCom.Ntimestep) = sMax1
            'Stress8(clsCom.Ntimestep) = sMax8
            Stress1(nstep) = sMax1
            Stress8(nstep) = sMax8
        End If

        ' YC 102418-012819
        'If (kpri <= 0 Or locstr = 1) Then Exit Sub     
        'FileClose(FileNo)
        'If (kpri <= 0 Or locstr = 1) Then Return

        'If (newmax = 1) Then GoTo 1001
1001:
        Return
        ' YC 102418-012819 END


    End Sub
End Class




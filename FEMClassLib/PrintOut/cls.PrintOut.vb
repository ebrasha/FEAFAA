Partial Public Class clsPrintOut  


    Public com As New clsCom
    Private objInit As New clsInitial

    Public WorkingDir As String

    Public lutty As Integer 'file "FAASR3D STATUS.txt"

    Public VersionNo, CompileDate As String



    'input (nikein) from clsCom
    ' Public ControlCards
    Public nmmat, numnp, numelh, numsv, inpsd, ntime As Integer
    Public nlcur, nptm, nload, numpc, numdc, nrcc As Integer

    ' Public MaterialCards
    Public matype() As Integer
    Public den(), prop(,) As Double

    ' Public NodeCards
    Public idp(,) As Integer
    Public nob() As Integer
    Public x(,) As Double

    ' Public ElementCards
    'Public ia() As Integer 'YC?
    'Public Shared ixh(,) As Integer 'by YC 
    Public ixh(,), ixe(,) As Integer

    ' Public SpringCards
    Public nmmtde, nmelde, nmmass As Integer
    Public mtypde(), ixde(,) As Integer
    Public cmde(,), sclf() As Double

    ' Public InterfaceCards
    Public nrttlm, nrttls, iacc, ifl, nifd, nsntl, nmntl, nsmmax As Integer
    Public iparm(,), iaug(), ifd(), ngap(), irects(,), irectm(,), nsf(,) As Integer
    Public fric(,), pend(), altol(,), sfact(), tdeath(), tbury(), stfsf() As Double

    ' Public LoadCards
    Public nod(), idirn(), ncur(), npc(), nptst, itemp, itread As Integer 'itemp YC?
    Public fac(), pld(), tmode(), tbase() As Double
    'input (nikein) from clsCom END

    Public dt As Double
    Public StopFEDFAA, FEDFAAStopped, IDTyp, I1 As Short

    'input passed by class "slove"
    Public mpri, kpri As Integer
    Public incflg, incpr, iadd, maxsiz, nhxic, lpbh As Integer
    Public numelg(2), ipelem(3, 10, 5) As Integer

    Public nstep As Integer, timep As Double

    'Output-Nodal Displacement.txt "prtdv"
    Public neql, npb As Integer
    Public ipnode(,) As Integer '
    Public un2(), temo() As Double

    'Output-Hexahedron Element.txt "prtrs"
    Public nnel As Integer
    Public lhex(), ihv(), ncon(), idroer() As Integer
    Public anegb1(), anegb2() As Double

    'Output-Discrete Element.txt "prde"
    Public dehva(,) As Double
    'input passed by class "slove" END





    Private sMaxStress1(80), sMaxStress8(80) As Double 'maximum stress, output






    Private StartTime As Double
    Private NL1 As String = Environment.NewLine
    Private IsFAASR3DPlot As Boolean = 1 'NikePlot to FAASR3DPlot by YC 102418-012819



    'verified variables
    Private numnp1, nel As Integer 'shared variable

    'TecPlot format file
    Public snl(,,) As Double 'all nodes' displacement & temperature as each step
    Public st(,,,) As Double ' all elements' stress at each step

    Private stress(6), strain(6) As Double 'stress/strain components
    'verified variables END



    'not verified variables
    'Output-Nodal Reaction Force.txt "prtrf"
    Private numudc, iterp2 As Integer
    Private noded(), idid(), rfa() As Integer

    Private cpuio(3, 24), cpuip(3, 24) As Double 'time recording


    'Public dtx0, dtx1 As Double     ' "update"

    Private lstriout As Boolean     ' "prtout"
    Private tim(7) As Double
    Private nbtot, iacflg, iaddbs, lpri, npri, lcsts As Integer

    Private nnn(80), nnn8(80) As Integer  ' "prtrs"
    Private nnelst, njk As Integer


    Private length, nh04, nh05, nh06, nh07, nh08, nh09 As Integer

    Private avlitr(2, 1600) As Double ' "adios"
    Private ncstep, numlit, maxlit, itrtot, irftot, icalls As Integer

    Private numblk, iadd0 As Integer '"oocbsh"


    Private locstr As Integer ' "prtout", "prtrs"

    Private ft As Double ' "prtrs", "s6out", "s4out"
    Private nstate As Integer

    Private nprint As Integer ' "prtout", "prtrs", "prde"

    Private nbc As Integer ' "prtout", "oocbsh"

    Private ipt As Integer ' "prtrs", "getstr"

    Private lki, lkj As Integer ' "adios", "timing"
    ' not verified variables END



    '    Sub ParaTransfer(ByRef lutty As Integer, ByRef VersionNo As String, ByRef CompileDate As String,
    '               ByRef mpri As Integer, ByRef incflg As Integer, ByRef incpr As Integer,
    '               ByRef nstep As Integer, ByRef timep As Double,
    '     ByRef neql As Integer, ByRef un2() As Double, ByRef idp(,) As Integer, ByRef itemp As Integer, ByRef temo() As Double, ByRef npb As Integer, ByRef ipnode(,) As Integer,
    'ByRef idroer() As Integer, ByRef lhex() As Integer, ByRef matype() As Integer, ByRef ihv() As Integer, ByRef ncon() As Integer, ByRef anegb1() As Double,
    '    ByRef mtypde() As Integer, ByRef dehva(,) As Double,
    '            ByRef sMaxStress1() As Double, ByRef sMaxStress8() As Double)
    Sub ParaTransfer(ByRef objSolve As Object)
        ' to read in inputs by YC

        lutty = objSolve.lutty
        VersionNo = objSolve.VersionNo
        CompileDate = objSolve.CompileDate

        mpri = objSolve.mpri
        kpri = objSolve.kpri
        incflg = objSolve.incflg
        incpr = objSolve.incpr

        nstep = objSolve.nstep
        timep = objSolve.timep

        neql = objSolve.neql
        un2 = objSolve.un2
        idp = objSolve.idp
        itemp = objSolve.itemp
        temo = objSolve.temo
        npb = objSolve.npb
        ipnode = objSolve.ipnode

        idroer = objSolve.idroer
        lhex = objSolve.lhex
        matype = objSolve.matype
        ihv = objSolve.ihv
        ncon = objSolve.ncon
        anegb1 = objSolve.anegb1
        anegb2 = objSolve.anegb2

        mtypde = objSolve.mtypde
        dehva = objSolve.dehva


        iadd = objSolve.iadd
        maxsiz = objSolve.maxsiz
        nhxic = objSolve.nhxic
        numelg = objSolve.numelg
        lpbh = objSolve.lpbh
        ipelem = objSolve.ipelem

        'WorkingDir0 = clsCom.WorkingDir

        'numnp = clsCom.numnp
        'numelh = clsCom.numelh
        'nmelde = clsCom.nmelde 'discrete (spring) elememt 

        'ntime = clsCom.ntime
        'nload = clsCom.nload

        'nlcur = clsCom.nlcur
        'nmmtde = clsCom.nmmtde
        'numsv = clsCom.numsv

        'nmmat = clsCom.nmmat

        'itemp = clsCom.itemp

        WorkingDir = objSolve.WorkingDir

        numnp = objSolve.numnp
        numelh = objSolve.numelh
        nmelde = objSolve.nmelde 'discrete (spring) elememt 

        ntime = objSolve.ntime
        nload = objSolve.nload

        nlcur = objSolve.nlcur
        nmmtde = objSolve.nmmtde
        numsv = objSolve.numsv

        nmmat = objSolve.nmmat

        itemp = objSolve.itemp



        'Dim i, j As Integer
        'ReDim matype(25), mtypde(6) 'dimension YC?
        'For i = 1 To nmmat
        '    matype(i) = clsCom.matype(i - 1)
        'Next i

        'For i = 1 To nmmtde
        '    mtypde(i) = clsCom.mtypde(i - 1)
        'Next i


        'ReDim x(3, numnp) 'node
        'For i = 1 To numnp
        '    BCIndex(i) = clsCom.BCIndex(i)

        '    For j = 1 To 3
        '        x(j, i) = clsCom.x(j - 1, i - 1)
        '    Next j

        '    'For j = 1 To 6
        '    '    idp(j, i) = clsCom.idp(j - 1, i - 1)
        '    'Next j
        'Next i
        ' BCIndex = clsCom.BCIndex ' YC? 102418-012819           ' QW 12-12-2018-
        x = objSolve.x


        'ReDim ia(9, numelh) 'element
        'For i = 1 To numelh
        '    For j = 1 To 9
        '        'ia(j, i) = clsCom.ia(9 * (i - 1) + j - 1)
        '        ia(j, i) = clsCom.ixh(j - 1, i - 1)
        '    Next j
        'Next i
        ixh = objSolve.ixh


        'ReDim ixde(3, nmelde) 'spring/discrete element
        'For i = 1 To nmelde
        '    For j = 1 To 3
        '        ixde(j, i) = clsCom.ixde(j - 1, i - 1)
        '    Next j
        'Next i
        ixde = objSolve.ixde


        'ReDim nod(nload), idirn(nload), ncur(nload), fac(nload)
        'For i = 1 To nload
        '    nod(i) = clsCom.nod(i - 1)
        '    idirn(i) = clsCom.idirn(i - 1)
        '    ncur(i) = clsCom.ncur(i - 1)
        '    fac(i) = clsCom.fac(i - 1)
        'Next i
        nod = objSolve.nod
        idirn = objSolve.idirn
        ncur = objSolve.ncur
        fac = objSolve.fac


        If IsFAASR3DPlot Then
            ReDim Preserve snl(numnp, 4, ntime), st(numelh, 7, 8, ntime) ' node displacement, element IP stress
        End If

    End Sub

    Sub gethexg(ByRef n As Integer, ByRef numelg() As Integer, ByRef ihv() As Integer, ByRef nelpg As Integer, ByRef nblk As Integer, ByRef length As Integer)

        Dim nelg, nelgp1, ihvg, ihvgp1 As Integer

        nelg = numelg(n)
        nelgp1 = numelg(n + 1)
        ihvg = ihv(nelg)
        ihvgp1 = ihv(nelgp1)

        nelpg = nelgp1 - nelg
        nblk = nelg - 1
        length = ihvgp1 - ihvg + nelpg * incflg * 333

        If (incflg = 1) Then
            nh05 = nh04 + ihvgp1 - ihvg
            nh06 = nh05 + 297 * nelpg
            nh07 = nh06 + 9 * nelpg
            nh08 = nh07 + 9 * nelpg
            nh09 = nh08 + 9 * nelpg
        End If

    End Sub

    Sub blkcpy(ByRef a() As Double, ByRef b() As Double, ByRef n As Integer)

        Dim i As Integer

        For i = 1 To n
            b(i) = a(i)
        Next i

    End Sub

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

        Call header(lutty)

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

        Call header(lutty)

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

    Sub exita(ByRef n As Integer)

        PrintLine(lutty, "  program stop" & LPad(5, Format(n, "0")) & NL1 & NL1 & NL1)

    End Sub

    Sub timing(ByRef cpuio(,) As Double, ByRef cpuip(,) As Double, ByRef k As Integer, ByRef l As Integer)

        Dim t(3), tp(3) As Double
        Dim i, j As Integer

        Call timuse(t(1), t(3), t(2))

        'GoTo (20,50,70,90) l
        If l = 1 Then GoTo 20
        If l = 2 Then GoTo 50
        If l = 3 Then GoTo 70
        If l = 4 Then GoTo 90


20:     For i = 1 To k
            For j = 1 To 3
                cpuio(j, i) = 0.0
                cpuip(j, i) = t(j)
                tp(j) = t(j)
            Next j
        Next i
        lki = k
        lkj = 0
        Exit Sub


50:     For j = 1 To 3
            cpuip(j, k) = t(j)
            tp(j) = t(j)
        Next j
        lki = k
        Exit Sub


70:     For j = 1 To 3
            cpuio(j, k) = cpuio(j, k) + t(j) - cpuip(j, k)
            tp(j) = t(j)
        Next j
        lkj = k
        Exit Sub

90:     For j = 1 To 3
            cpuio(j, k) = cpuio(j, k) + t(j) - tp(j)
            tp(j) = t(j)
        Next j
        lkj = k
        Exit Sub

    End Sub

    Sub timuse(ByRef t1 As Double, ByRef t2 As Double, ByRef t3 As Double)

        Dim hpfac As Double
        Dim ttm(8) As Double

        hpfac = 100

        t1 = ttm(1) / hpfac
        t2 = 0
        t3 = ttm(2) / hpfac

        'Call CPU_TIME(t3) 'YC?
        t3 = Microsoft.VisualBasic.Timer - StartTime

    End Sub

    Sub unpkid(ByRef idu() As Integer, ByRef idp(,) As Integer, ByRef inode As Integer, ByRef itp As Integer)

        Dim nit() As Integer = {"0", "6", "9"}

        Dim i, n As Integer

        n = nit(itp)
        For i = 1 To n
            idu(i) = idp(i, inode)
10:     Next i

    End Sub

    'Sub NikePlot()
    Sub FAASR3DPlot(ByVal path As String)   ' YC 102418-012819

        Dim ifg As Integer

        ifg = 1

        If (ifg >= 1 And ifg <= 4 Or ifg = 6 Or ifg = 7) Then
            Call n3dhsp(ifg, 1, path)
        End If

    End Sub

    Public Function LPad(ByRef N As Integer, ByRef SS As String) As String
        ' Adds leading spaces to variant string SS to make it N characters long.
        ' Used to format output to a file. #### characters in a Format function
        ' do not force spaces like QuickBasic.
        ' Typically, SS = Format(XX, "0.00")
        Dim ITemp As Short
        ITemp = CShort(Len(SS))
        If ITemp > N Then N = ITemp ' Length = Len if Len > N
        LPad = Space(N - ITemp) & SS
    End Function

End Class

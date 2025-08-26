Imports System.IO
Partial Public Class clsPrintOut

    'Sub prtout(ByVal FilenameOnly As String, ByRef kpri As Integer, ByRef mpri As Integer, ByRef npri As Integer, ByRef iadd As Integer, ByRef maxsiz As Integer, ByRef Stress1() As Double, ByRef Stress8() As Double,
    '          ByVal incflg As Integer, lhex() As Integer, ByVal numelg() As Integer, ByVal ihv() As Integer, ByVal nh04 As Integer, ByVal nh05 As Integer, ByVal nstep As Integer, ByVal idroer() As Integer,
    '          ByVal ncon() As Integer, ByVal anegb1() As Double, ByVal anegb2() As Double, ByVal incpr As Integer, ByVal dehva(,) As Double, ByVal timep As Double, ByVal npb As Integer, ByVal ipnode(,) As Integer, ByVal un2() As Double,
    '           ByVal ModelOut As Integer)  ' YC 121219-2
    Sub prtout(ByVal FilenameOnly As String, ByRef kpri As Integer, ByRef mpri As Integer, ByRef npri As Integer, ByRef iadd As Integer, ByRef maxsiz As Integer, ByRef Stress1() As Double, ByRef Stress8() As Double,
              ByVal incflg As Integer, lhex() As Integer, ByVal numelg() As Integer, ByVal ihv() As Integer, ByVal nh04 As Integer, ByVal nh05 As Integer, ByVal nstep As Integer, ByVal idroer() As Integer,
              ByVal ncon() As Integer, ByVal anegb1() As Double, ByVal anegb2() As Double, ByVal incpr As Integer, ByVal dehva(,) As Double, ByVal timep As Double, ByVal npb As Integer, ByVal ipnode(,) As Integer, ByVal un2() As Double,
               ByVal ModelOut As Integer, ByVal temo() As Double)


        Call com.comvari()
        ' Public ControlCards,  MaterialCards, ElementCards
        nmmat = clsCom.nmmat : numnp = clsCom.numnp : numelh = clsCom.numelh : numsv = clsCom.numsv
        inpsd = clsCom.numelh : ntime = clsCom.ntime : nlcur = clsCom.nlcur : nptm = clsCom.nptm
        nload = clsCom.nload : numpc = clsCom.numpc : numdc = clsCom.numdc : nrcc = clsCom.nrcc
        matype = clsCom.matype : idp = clsCom.idp : ixh = clsCom.ixh : ixe = clsCom.ixe : dt = clsCom.dt
        den = clsCom.den : prop = clsCom.prop : x = clsCom.x : nob = clsCom.nob

        ' Public SpringCards
        nmmtde = clsCom.nmmtde : nmelde = clsCom.nmelde : nmmass = clsCom.nmmass
        mtypde = clsCom.mtypde : ixde = clsCom.ixde
        cmde = clsCom.cmde : sclf = clsCom.sclf

        ' Public InterfaceCards
        'nrttlm = com.nrttlm : nrttls = com.nrttls : iacc = com.iacc : ifl = com.ifl : nifd = com.nifd : nsntl = com.nsntl : nmntl = com.nmntl
        'iparm = com.iparm : iaug = com.iaug : ifd = com.ifd : ngap = com.ngap : irects = com.irects : irectm = com.irectm : nsf = com.nsf
        'fric = com.fric : pend = com.pend : altol = com.altol : sfact = com.sfact : tdeath = com.tdeath : tbury = com.tbury : stfsf = com.stfsf
        ' nsmmax = com.nsmmax

        ' Public LoadCards
        nod = clsCom.nod : idirn = clsCom.idirn : ncur = clsCom.ncur : npc = clsCom.npc : nptst = clsCom.nptst : itemp = clsCom.itemp : itread = clsCom.itread
        fac = clsCom.fac : pld = clsCom.pld : tmode = clsCom.tmode : tbase = clsCom.tbase

        ' Public PathSet
        StopFEDFAA = clsCom.StopFEDFAA : FEDFAAStopped = clsCom.FEDFAAStopped : IDTyp = clsCom.IDTyp : I1 = clsCom.I1
        WorkingDir = clsCom.WorkingDir


        'Call ParaTransfer() ' to transfer parameter from previous class by YC

        Dim ifctr, icnt, icaddr, icaddi, iaph, nword, nelpg, nblk As Integer
        Dim phony As Double


        'ReDim snl(numnp, 4, ntime), st(numelh, 7, 8, ntime)     'v3.0 005/YC 092820-2
        ReDim Preserve snl(numnp, 4, ntime), st(numelh, 7, 8, ntime)


        Dim path As String = WorkingDir & "PrintOut-" & FilenameOnly & "\"
        If ModelOut = 1 Then
            If Not Directory.Exists(path) Then
                Directory.CreateDirectory(path)
            End If
        End If
        lstriout = False

        'If mpri < 0 Then GoTo 10   ' YC 102418-012819
        If mpri <= 0 Then GoTo 10

        lpri = 0

        'Call prtrf(path, idp, noded, idid, rfa, numudc, timep)
        If ModelOut = 1 Then Call prtdv(un2, idp, numnp, itemp, temo, npb, ipnode, nstep, timep, path) ' QW 08-14-2019
        GoTo 20

10:     npri = 0
        tim(1) = timep
        ifctr = iadd / maxsiz

        icnt = 1 + 6 * numnp + 7 * numelh
        'If (iacflg <> 0) Then icnt = icnt + 3 * numnp
        If ((iadd + icnt) > (maxsiz * (ifctr + 1))) Then
            If ((iadd Mod maxsiz) <> 0) Then
                iadd = (ifctr + 1) * maxsiz
            End If
        End If

        iadd = iadd + 1

20:     locstr = 1
        lcsts = 1
        icaddr = 0

        nnel = 0
        nprint = 0

        nbc = 1
        iaddbs = icaddr
        'iaddbs = nhxic
        icaddi = 0

        nbtot = 1
        nword = 8 * lhex(9)
        If (incflg = 1) Then nword = nword + 333

        Call objInit.gethexg(1, numelg, ihv, nelpg, nblk, length, incflg, nh04, nh05)
        lhex(2) = nelpg

        Call blkcpy(anegb2, anegb1, length)            ' QW 12-12-2018-
        icaddi = icaddi + length


        'Call prtrs(mpri, nnel, nstep, x, lhex, ncon, idroer, incflg, nh04, nh05, nblk, ihv, numelg, nelpg, anegb1, nprint, incpr, st, timep, path, Stress1, Stress8, ModelOut)     'v3.0 005/YC 092820-2
        Call prtrs(mpri, nnel, nstep, x, lhex, ncon, idroer, incflg, nh04, nh05, nblk, ihv, numelg, nelpg, anegb1, nprint, incpr, timep, path, Stress1, Stress8, ModelOut)


130:    If (nmelde <> 0) Then
            nprint = 0
            If ModelOut = 1 Then Call prde(mtypde, ixde, dehva, nmelde, x, nstep, timep, path)
        End If
        ' QW 08-14-2019

        'If (mpri < 0.0) Then       ' YC 102418-012819
        If (mpri <= 0.0) Then

            iaph = iadd
            ifctr = iadd / maxsiz
            If ((iadd + icnt) > (maxsiz * (ifctr + 1))) Then
                If ((iadd Mod maxsiz) <> 0) Then
                    iaph = (ifctr + 1) * maxsiz
                End If
            End If
            phony = -99999.0
        End If


        'If (kpri < 0) Then kpri = 1     ' YC 102418-012819
        'If (mpri < 0) Then mpri = 1
        If (kpri <= 0) Then kpri = 1
        If (mpri <= 0) Then mpri = 1 : Return


        'FileClose(lutty)  ' YC 102418-012819

        If nstep < ntime Then Return
        ' QW 08-14-2019
        If IsFAASR3DPlot And ModelOut = 1 Then 'tecplot format file YC 073018, YC 102418-012819
            Call FAASR3DPlot(path)
        End If

        'Stress1 = sMaxStress1 : Stress8 = sMaxStress8    ' YC 102418-012819

    End Sub
End Class




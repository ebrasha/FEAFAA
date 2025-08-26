Partial Public Class clsInitial
    ' From the class of clsCom 
    Public nmmat, numnp, numelh, numsv, inpsd, ntime, nlcur, nptm, nload, numpc, numdc, nrcc As Integer
    Public matype(), idp(,), ixh(,), incomp() As Integer
    Public den(), prop(,), x(,) As Double
    Public dt, tollin As Double
    Public nmmtde, nmelde, nmmass, mbsize, nhxblk, kpri As Integer
    Public mtypde(), ixde(,) As Integer
    Public cmde(,), sclf() As Double
    Public nrttlm, nrttls, iacc, ifl, nifd, nsntl, nmntl As Integer
    Public iparm(,), iaug(), ifd(), ngap(), irects(,), irectm(,), nsf(,) As Integer
    Public fric(,), pend(), altol(,), sfact(), tdeath(), tbury(), stfsf() As Double
    Public nod(), idirn(), ncur(), npc(), nptst, itemp, itread As Integer
    Public fac(), pld(), tmode(), tbase() As Double
    Public StopFEDFAA, FEDFAAStopped, IDTyp, I1 As Short
    Public WorkingDir, FilenameOnly As String
    '
    Public a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, delt, alfa, ectl, cvtl, rctl, tolls As Double
    Public nprm(8), numelg(2), nwebuf, neql, nodep(,), npb, ipnode(,) As Integer
    Public al20(), gass(,), gps(), gpt() As Double
    Public kht(), irow(), icol(), igrad() As Integer
    Public hh(4), xn(,), yn(,), zn(,), pmult(,), strt(), rf() As Double
    Public h8(8, 9), pr(8, 9), ps(8, 9), pt(8, 9), hix(8, 9), hiy(8, 9), hiz(8, 9) As Double
    Public h0(4, 4), p1(4, 4), p2(4, 4) As Double
    Public iihv, inhxg, ntlen, ntlenf, nblk, nsnode, nthpz, icnt1, icnt2, irfreq, maxref As Integer
    Public ihv(), lcp(), lcd() As Integer
    Public sic(,,), dic(,), usic(,), dusic(,), ric(,) As Double
    Public anegb1(), anegb2() As Double
    Public p(), dehv(,), dehva(,), un4(), fval(), temo(), temp() As Double
    Public nnns, n2g, llls, lutty As Integer
    Public told, tnew, time, extime As Double
    Public incflg, nh04, nh05, ilimit, nstep, mthsol, lcspf, numudc, iprec, incpr, emodl2 As Integer
    Public lhex(9) As Integer
    Public ncon(60) As Integer
    Public dtx0, riksf, alpha, ptime, gax, gay, gaz, stepsv As Double
    Public t1 As String
    Public idid(), idflad() As Integer
    Public stfs(), stfm(), amad(), xx1(20), xx2(20), xx3(20) As Double
    Public neesf, ksizsf, itrlmt, mpri, ipri, jpri, maxsiz, iadd, nalsf As Integer
    Public id(,), ix(,), ixx(9), iyy(9) As Integer
    Public nsv(), msr(), nsegs(), nsegm(), lnsv(), lmsr(), ilocs(), ilocm(), irtls(), irtlm() As Integer
    Public iblock(), ipoint(), ireord(), idroer(), ihxblk() As Integer
    Public bNIKE3D, laugon, lprtbd As Boolean
    Public com As New clsCom
    Public Sub comvari(ByVal nik As Object)
        ' Public ControlCards,  MaterialCards, ElementCards
        nmmat = clsCom.nmmat : numnp = clsCom.numnp : numelh = clsCom.numelh : numsv = clsCom.numsv
        inpsd = clsCom.numelh : ntime = clsCom.ntime : nlcur = clsCom.nlcur : nptm = clsCom.nptm
        nload = clsCom.nload : numpc = clsCom.numpc : numdc = clsCom.numdc : nrcc = clsCom.nrcc
        matype = clsCom.matype : idp = clsCom.idp : ixh = clsCom.ixh : dt = clsCom.dt
        den = clsCom.den : prop = clsCom.prop : x = clsCom.x : incomp = clsCom.incomp

        ' Public SpringCards
        nmmtde = clsCom.nmmtde : nmelde = clsCom.nmelde : nmmass = clsCom.nmmass
        mtypde = clsCom.mtypde : ixde = clsCom.ixde
        cmde = clsCom.cmde : sclf = clsCom.sclf

        ' Public InterfaceCards
        nrttlm = nik.nrttlm : nrttls = nik.nrttls : iacc = nik.iacc : ifl = nik.ifl : nifd = nik.nifd : nsntl = nik.nsntl : nmntl = nik.nmntl
        iparm = nik.iparm : iaug = nik.iaug : ifd = nik.ifd : ngap = nik.ngap : irects = nik.irects : irectm = nik.irectm : nsf = nik.nsf
        fric = nik.fric : pend = nik.pend : altol = nik.altol : sfact = nik.sfact : tdeath = nik.tdeath : tbury = nik.tbury : stfsf = nik.stfsf

        ' Public LoadCards
        nod = clsCom.nod : idirn = clsCom.idirn : ncur = clsCom.ncur : npc = clsCom.npc : nptst = clsCom.nptst : itemp = clsCom.itemp : itread = clsCom.itread
        fac = clsCom.fac : pld = clsCom.pld : tmode = clsCom.tmode : tbase = clsCom.tbase

        ' Public PathSet
        StopFEDFAA = clsCom.StopFEDFAA : FEDFAAStopped = clsCom.FEDFAAStopped : IDTyp = clsCom.IDTyp : I1 = clsCom.I1
        WorkingDir = clsCom.WorkingDir : FilenameOnly = clsCom.FilenameOnly

    End Sub
    Sub intial(ByVal nik As Object)
        Call comvari(nik)
        Call com.ancon(ncon)
        neql = nik.neql
        nwebuf = nik.nwebuf
        incflg = nik.incflg
        ntlen = nik.ntlen : idroer = nik.idroer
        delt = nik.delt : dt = nik.dt : ilimit = nik.ilimit : nsnode = nik.nsnode : lutty = nik.lutty : tolls = nik.tolls : ectl = nik.ectl : cvtl = nik.cvtl : rctl = nik.rctl : mthsol = nik.mthsol
        neesf = nik.neesf : ksizsf = nik.ksizsf : mbsize = nik.mbsize : dehv = nik.dehv : nhxblk = nik.nhxblk : ihxblk = nik.ihxblk : npb = nik.npb : ipnode = nik.ipnode
        nsv = nik.nsv : msr = nik.msr : nsegs = nik.nsegs : nsegm = nik.nsegm : lnsv = nik.lnsv : lmsr = nik.lmsr : mpri = nik.mpri : ipri = nik.ipri : jpri = nik.jpri : maxsiz = nik.maxsiz : iadd = nik.iadd
        ilocs = nik.ilocs : ilocm = nik.ilocm : irtls = nik.irtls : irtlm = nik.irtlm : stfs = nik.stfs : stfm = nik.stfm : stepsv = nik.stepsv : incpr = nik.incpr : icnt1 = nik.icnt1 : icnt2 = nik.icnt2
        hh = nik.hh : id = nik.id : ix = nik.ix : ixx = nik.ixx : iyy = nik.iyy : xx1 = nik.xx1 : xx2 = nik.xx2 : xx3 = nik.xx3 : irfreq = nik.irfreq : bNIKE3D = nik.bNIKE3D : maxref = nik.maxref
        a0 = nik.a0 : a1 = nik.a1 : a2 = nik.a2 : a3 = nik.a3 : a4 = nik.a4 : a5 = nik.a5 : a6 = nik.a6 : a7 = nik.a7 : a8 = nik.a8 : a9 = nik.a9 : a10 = nik.a10 : delt = nik.delt : alfa = nik.alfa
        lprtbd = nik.lprtbd : laugon = nik.laugon : emodl2 = nik.emodl2 : nalsf = nik.nalsf
        Dim pp(2, 4) As Double
        'ReDim anegb1(nwebuf), anegb2(nwebuf)
        Dim nhexg, nwbuf, nelpg, icaddi, nhxic, length, itcurv As Integer
        ReDim fval(2 * nlcur), temo(numnp), temp(numnp)
        ReDim ihv(numelh + 1), kht(neql)
        ReDim lcp(numpc), nodep(8, numpc), xn(8, numpc), yn(8, numpc), zn(8, numpc), zn(8, numpc), pmult(8, numpc), strt(numpc)
        ReDim p(3 * nlcur * nptm), dehva(11, nmelde), al20(62 * neql), un4(neql)
        ReDim idid(numdc), amad(numdc), idflad(numdc), rf(numdc), lcd(numdc)
        Dim zero As Double
        Call Reinit()
        
        Call com.azero(al20, 62 * neql)
        Call com.azero(kht, neql)

        'Call Check2D(ixh, 9, numelh)

        '    i n i t i a l i z e   h e x a h e d r o n s
        For I = 1 To 9

            Dim h1(8), hix1(8), hiy1(8), hiz1(8), pr1(8), ps1(8), pt1(8) As Double
            h1 = TTO(h8, I) : hix1 = TTO(hix, I) : hiy1 = TTO(hiy, I) : hiz1 = TTO(hiz, I) : pr1 = TTO(pr, I) : ps1 = TTO(ps, I) : pt1 = TTO(pt, I)
            Call basis8(gass(1, I), gass(2, I), gass(3, I), h1, hix1, hiy1, hiz1, pr1, ps1, pt1)
            For J = 1 To 8
                h8(J, I) = h1(J) : hix(J, I) = hix1(J) : hiy(J, I) = hiy1(J) : hiz(J, I) = hiz1(J) : pr(J, I) = pr1(J) : ps(J, I) = ps1(J) : pt(J, I) = pt1(J)
            Next
        Next I

        For lst = 1 To 4
            Call basis4(gps(lst), gpt(lst), hh, pp)
            For I = 1 To 4
                h0(I, lst) = hh(I)
                p1(I, lst) = pp(1, I)
                p2(I, lst) = pp(2, I)
            Next I
        Next lst
        If numelh = 0 Then GoTo 190
        inhxg = ntlen
        Call in3dhhv(ixh, ihv, matype, numelg, nhexg, nwbuf)
        ntlen = ntlen + nhexg + 1
        lhex(3) = nelpg
        lhex(4) = numelh
        '     storage allocation for hexahedron element groups in working buffer
        nh04 = 1
        '.... initialize consititutive data
        icaddi = 0
        nhxic = ntlen
        ntlen = ntlen + nwbuf
        Call gethexg(1, numelg, ihv, nelpg, nblk, length, incflg, nh04, nh05)
        lhex(2) = nelpg
        ReDim anegb1(length), anegb2(length)    ' QW 12-12-2018-
        'Call Check1D(lhex, 9)
        Call com.get_time(t1)
        Call in3dh(idp, x, matype, den, prop, ixh, kht, ihv, numnp, nmmat)
        ReDim sic(33, 9, nelpg), dic(9, nelpg), usic(9, nelpg), dusic(9, nelpg), ric(9, nelpg)
        If incflg = 1 Then
            Call com.azero(sic, 33, 9, nelpg)
            Call com.azero(dic, 9, nelpg)
            Call com.azero(usic, 9, nelpg)
            Call com.azero(dusic, 9, nelpg)
            Call com.azero(ric, 9, nelpg)
        End If
        Call com.blkcpy(anegb1, anegb2, length)
        icaddi = icaddi + length
        nblk = nblk + lhex(2)
        '   ... initialize discrete elements ...
190:
        If nmelde <> 0 Then
            Call initde(npc, pld, mtypde, cmde, ixde, sclf, dehv, nmelde, x, kht, idp, numnp, nmmat)
            Call com.get_time(t1)
            Call com.blkcpy(dehv, dehva, 11, nmelde)
        End If
        ' set variables in block solver's common blocks
        ntlenf = -neql
        nnns = nprm(1)
        llls = nprm(8)
        n2g = 64
        ' initialize displacement increment
        Call com.azero(un4, neql)
        ' ... set and check load curves at t=0 ...
        If nlcur <> 0 Then
            zero = 0.0
            Call ldcset(nlcur, fval, npc, pld, zero)
            Call cklc(nload, ncur, numpc, lcp, numdc, lcd, fval, told, time, dt, itemp, nstep)
        End If
        ' in case of thermal analysis read in or compute temperatures
        If itemp > 0 Or itread > 0 Then
            tnew = 0.0
            itcurv = itemp
            If itread > 0 Then itcurv = itread
            zero = 0.0
            ' Call temth(tnew, temp, tnew, temp, npc, pld, itcurv, zero, tmode, tbase)   ' Do it later. QW 9-24-2018
        End If

    End Sub
    
End Class

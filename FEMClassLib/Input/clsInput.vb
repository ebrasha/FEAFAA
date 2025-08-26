Partial Public Class clsInput
    ' From the class of clsCom 
    Public nmmat, numnp, numelh, numsv, inpsd, ntime, nlcur, nptm, nload, numpc, numdc, nrcc As Integer
    Public matype(), idp(,), ixh(,), ixe(,), incomp() As Integer
    Public den(), prop(,), x(,), rdc(,) As Double
    Public dt As Double
    Public nmmtde, nmelde, nmmass, neql, mbsize, nhxblk, nsmmax As Integer
    Public mtypde(), ixde(,) As Integer
    Public cmde(,), sclf() As Double
    Public nrttlm, nrttls, iacc, ifl, nifd, nsntl, nmntl As Integer
    Public iparm(,), iaug(), ifd(), ngap(), irects(,), irectm(,), nsf(,) As Integer
    Public fric(,), pend(), altol(,), sfact(), tdeath(), tbury(), stfsf() As Double
    Public nod(), idirn(), ncur(), npc(), nptst, itemp, itread, icnt1, icnt2, irfreq As Integer
    Public fac(), pld(), tmode(), tbase() As Double
    Public StopFEDFAA, FEDFAAStopped, IDTyp, I1 As Short
    Public WorkingDir, FilenameOnly As String
    '
    Public a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, delt, alfa, ectl, cvtl, rctl, tolls, stepsv As Double
    Public model, nwebuf, nweb, incflg, nstl, nmtl, issca, ilimit, negb3, nh04, incpr, emodl2 As Integer
    Public mpdiag, mpelsf, neesf, ksizsf, mpcgus, mpcgp, mpcgz, mpcgap, ntlen, nsnode, mpri, ipri, jpri, maxsiz, iadd As Integer
    Public nsv(), msr(), nsegs(), nsegm(), lnsv(), lmsr(), ilocs(), ilocm(), irtls(), irtlm() As Integer
    Public stfs(), stfm(), hh(4), xx1(20), xx2(20), xx3(20), dehv(,) As Double
    Public id(,), ix(,), ixx(9), iyy(9), lutty, mthsol, maxref, nalsf As Integer
    Public iblock(), ipoint(), ireord(), idroer(), npb, ipnode(,), ihxblk(), lhex(9) As Integer
    Public bNIKE3D, laugon, lprtbd As Boolean
    Public com As New clsCom
    'Public rea As New clsRead

    Public Sub comvari()
        ' Public ControlCards,  MaterialCards, ElementCards
        nmmat = clsCom.nmmat : numnp = clsCom.numnp : numelh = clsCom.numelh : numsv = clsCom.numsv
        inpsd = clsCom.numelh : ntime = clsCom.ntime : nlcur = clsCom.nlcur : nptm = clsCom.nptm
        nload = clsCom.nload : numpc = clsCom.numpc : numdc = clsCom.numdc : nrcc = clsCom.nrcc
        matype = clsCom.matype : idp = clsCom.idp : ixh = clsCom.ixh : ixe = clsCom.ixe : dt = clsCom.dt
        den = clsCom.den : prop = clsCom.prop : x = clsCom.x : rdc = clsCom.rdc

        ' Public SpringCards
        nmmtde = clsCom.nmmtde : nmelde = clsCom.nmelde : nmmass = clsCom.nmmass
        mtypde = clsCom.mtypde : ixde = clsCom.ixde
        cmde = clsCom.cmde : sclf = clsCom.sclf

        ' Public InterfaceCards
        nrttlm = clsCom.nrttlm : nrttls = clsCom.nrttls : iacc = clsCom.iacc : ifl = clsCom.ifl : nifd = clsCom.nifd : nsntl = clsCom.nsntl : nmntl = clsCom.nmntl
        iparm = clsCom.iparm : iaug = clsCom.iaug : ifd = clsCom.ifd : ngap = clsCom.ngap : irects = clsCom.irects : irectm = clsCom.irectm : nsf = clsCom.nsf
        fric = clsCom.fric : pend = clsCom.pend : altol = clsCom.altol : sfact = clsCom.sfact : tdeath = clsCom.tdeath : tbury = clsCom.tbury : stfsf = clsCom.stfsf
        nsmmax = clsCom.nsmmax

        ' Public LoadCards
        nod = clsCom.nod : idirn = clsCom.idirn : ncur = clsCom.ncur : npc = clsCom.npc : nptst = clsCom.nptst : itemp = clsCom.itemp : itread = clsCom.itread
        fac = clsCom.fac : pld = clsCom.pld : tmode = clsCom.tmode : tbase = clsCom.tbase

        ' Public PathSet
        StopFEDFAA = clsCom.StopFEDFAA : FEDFAAStopped = clsCom.FEDFAAStopped : IDTyp = clsCom.IDTyp : I1 = clsCom.I1
        WorkingDir = clsCom.WorkingDir : FilenameOnly = clsCom.FilenameOnly

    End Sub
    Public Sub inputi(ByVal lutty0 As Integer)
        Call comvari()
        Call matin(lhex, rdc, incomp)
        Call Node()
        Call sillin()
        Call slavin()

        ' In the class of clsInput
        Dim maxtl As Integer = Math.Max(nmntl, nsntl)
        Dim cst(3, nsntl) As Double
        lutty = lutty0

        Dim llc, lresrt, nrtm, nrts, nmn, nsn, nty, nst, mst As Integer
        Dim det, hs(4), ht(4), vn(4), vs(4), vt(4), vx(4, 4, 5), fv(4), tsk(4, 4), ssk(4, 4), enxn(4, 4), dum(6) As Double
        Dim ux(20), uy(20), uz(20) As Double
        Dim xs, ys, zs, sig(4), amx, amy, amz, resltl, result As Double
        Dim ind As Integer
        Dim xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123,
            xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p As Double
        Dim i1, i2, i3, i4, i5, i6, nn As Integer

        'ReDim dehv(11, nmelde), ihxblk(numelh), ipnode(2, 8)   ' v3.0 003/062920-3 YC
        ReDim dehv(11, nmelde), ipnode(2, 8)

        Call buffersize1(nn)

        ReDim ihxblk(nhxblk)  ' v3.0 003/062920-3 YC


        Call elmnde(ixde, sclf, dehv, x, nmelde)
        Call testnd(idp, numnp, ixh, numelh, ixde, nmelde)
        ReDim iblock(numelh), ipoint(numelh), ireord(numelh), idroer(numelh)
        ReDim nsv(nsntl), msr(nmntl), nsegs(numsv + nsntl), nsegm(numsv + nmntl)
        ReDim ilocs(maxtl), ilocm(maxtl), irtls(maxtl), irtlm(maxtl)
        ReDim id(6, numnp), ix(9, numelh)
        For ind = 1 To numsv
            If iaug(ind) <> 0.0 Then laugon = True ' QW 08-28-2019
            If sfact(ind) > 0.001 Then ' from sillin
                lprtbd = True
            Else
                lprtbd = False
            End If
            Call nodesm(nsv, msr, irects, irectm, iparm, ind)
        Next
        For ind = 1 To numsv
            Call nsgmnt(iparm, irects, irectm, nsv, msr, nsegs, nsegm, ind)
        Next
        ReDim lnsv(nstl), lmsr(nmtl), stfs(nrttls), stfm(nrttlm)
        Call intsld(x, lnsv, lmsr, ilocs, ilocm, stfs, stfm, irtls, irtlm, nsv, msr, nsegs, nsegm, ixh, cst, rdc, iparm, irects, irectm,
                    xx1, xx2, xx3, det, emodl2, ixx, xs, ys, zs, amx, amy, amz, vn, llc, lresrt, resltl, result, nrtm, nrts, nmn, nsn, nty, nst, mst, hh,
                    xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)
        Call eqnum(idp, x, numnp, neql, i1, i2, i3, i4, i5, i6)
        'Call Check2D(x, 3, numnp)
        'Note: idp changed from eqnum. ixh changed from blck83
        Dim ieq(neql) As Integer
        Call blck83(idp, id, ixh, ix, iblock, ipoint, ieq, ireord, numnp, neql, numelh, mbsize, nhxblk, ihxblk, idroer)
        Call buffersize2(nn)
        ntlen = nn
    End Sub
    Public Sub buffersize1(ByRef nn As Integer)
        Dim itargt, negb1, negb2, n000, n001, nh01 As Integer
        Dim nm3, l20, kd03 As Integer
        'In the control card 7, default delt=cnwmk(1)=0.5, alfa=cnwmk(2)=0.25
        'In control card 3, dt = time step size
        delt = 0.5 : alfa = 0.25
        bNIKE3D = True
        'Rayleigh damping parameters rdc(1,nmmat - 1)=0, input from sub "matin"
        chgint(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, delt, alfa, dt)
        nhxblk = 750 : neesf = 15 : ksizsf = 121 ' set in reinit
        mthsol = 1 : ilimit = 30     ' BFGS. Max. No. of iterations between reforms in Control card 6   
        cvtl = 0.000001 : ectl = 0.01 : rctl = 10000000000.0 : tolls = 0.9     ' Convergence tolerance in control card 6
        incflg = 1          'with incompatible mode
        nstl = 0 : nmtl = 0 : mbsize = 64 : issca = 1 : stepsv = 1.0 : incpr = 1 : icnt1 = 1 : icnt2 = 1
        npb = 1 : ipnode(1, 1) = 1 : ipnode(2, 1) = numnp
        mpri = 1 : ipri = 1 : jpri = 1 : maxsiz = 20 * 262144 : iadd = 64 + 3 * numnp : irfreq = -1 : maxref = 15
        nwebuf = Math.Min(330 * (numelh), 24576)
        nwebuf = Math.Max(nwebuf, 9 * numnp)
        nweb = Math.Max(60000000, 1200 * numnp) / 8
        nwebuf = Math.Max(3 * numnp, nweb)
        If incflg = 1 Then
            itargt = 64 * (8 * 15 + 333) / 2
            nwebuf = CInt(nwebuf / itargt + 1) * itargt
        End If
        negb1 = 1
        negb2 = negb1 + nwebuf
        n000 = negb2 + nwebuf
        n001 = n000 + 6 * numnp
        nh01 = n001 + 3 * numnp
        nh04 = nh01 + 10 * numelh + 1
        nm3 = nh04 + 56 * nmmat


        nwebuf = nwebuf + nwebuf    'v3.0 003/062920-1 YC


        kd03 = nm3 + 24 * nmmat + 1000 * nmmat + 25 * nmmtde
        l20 = kd03 + 26 * nmelde
        n001 = n001 + 4 * nrcc
        nalsf = numsv
        nn = l20 + 18 * numsv

    End Sub
    Public Sub buffersize2(ByRef nn As Integer)

        Dim l01, l02, l03, l04, l05, l06, l07, l08, l09, l10, l11, l12, l13, l15, l17, l18, l20 As Integer
        Dim jfl, nwebe1, nwebe2, mpelhx, nwtob, n1, n10, n20 As Integer
        l01 = nn + 7 * numsv
        l02 = l01 + 4 * nrttls
        l03 = l02 + 4 * nrttlm
        l04 = l03 + 4 * Math.Max(nrttlm, nrttls)
        l04 = l03 + nsntl
        l05 = l04 + nmntl
        l06 = l05 + numsv + nsntl
        l07 = l06 + numsv + nmntl
        l08 = l07 + nstl
        l09 = l08 + nmtl
        l10 = l09 + nsntl
        l11 = l10 + nmntl
        l12 = l11 + nrttls
        l13 = l12 + nrttlm
        l15 = l13 + 2 * Math.Max(nmntl, nsntl)
        l17 = l15 + 6 * nsmmax
        l18 = l17 + 3 * nsntl
        jfl = Math.Max(1, ifl)
        l20 = l18 + 12 * jfl + 6 * nsntl + 6 * nmntl + issca * (nsntl + nmntl) + 6 * nifd
        l20 = l20 + 2 * numelh
        nsnode = nsntl + nmntl
        negb3 = l20 + 2 * ilimit * neql + neql
        nwtob = 4
        mpdiag = negb3 + neql + 21
        mpelhx = mpdiag + 21 * numnp + 1
        nwebe1 = (mpelhx - mpdiag) * nwtob
        mpelsf = mpelhx + 324 * numelh
        n1 = mpelsf + (neesf + ksizsf) * nsnode + 1
        nwebe2 = (n1 - mpelhx) * nwtob
        mpcgus = n1
        mpcgp = mpcgus + neql + 1
        mpcgz = mpcgp + neql + 1
        mpcgap = mpcgz + neql + 1
        n10 = mpcgap + 10 * neql + 2 * nlcur + 8 * nload
        n20 = n10 + nlcur + 1 + nptst + 42 * numpc + 8 * numdc
        If itemp = 0 Then GoTo 97
        n20 = n20 + 2 * (numnp + 1 + itread * numnp)
97:     nn = n20

    End Sub
End Class

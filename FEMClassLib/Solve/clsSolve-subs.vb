Partial Public Class clsSolve

    Public nwebuf As Integer

    Public nsntl, nmntl, nstl, nmtl, nhxblk, jfl As Integer

    ' need verification by YC? 102418

    Public FEDFAAStopped, IDTyp, I1 As Short, WorkingDir, FilenameOnly As String

    Public itread As Integer    ' YC 121219



    Sub ParaTransfer(ByVal ini As Object)  ' ini added by YC? 102418


        Dim altol(,), stfsf(), pld() As Double


        'Dim inpsd, nptm, nmmtde, iacc, nifd, ngap(), nptst, itread As Integer  ' YC 121219
        Dim inpsd, nptm, nmmtde, iacc, nifd, ngap(), nptst As Integer


        ' Public ControlCards,  MaterialCards, ElementCards
        nmmat = ini.nmmat : numnp = ini.numnp : numelh = ini.numelh : numsv = ini.numsv
        inpsd = ini.inpsd : ntime = ini.ntime : nlcur = ini.nlcur : nptm = ini.nptm
        nload = ini.nload : numpc = ini.numpc : numdc = ini.numdc : nrcc = ini.nrcc
        matype = ini.matype : idp = ini.idp : ixh = ini.ixh : dt = ini.dt
        den = ini.den : prop = ini.prop : xyz = ini.x : incomp = ini.incomp

        ' Public SpringCards
        nmmtde = ini.nmmtde : nmelde = ini.nmelde : nmmass = ini.nmmass
        mtypde = ini.mtypde : ixde = ini.ixde
        cmde = ini.cmde : sclf = ini.sclf

        ' Public InterfaceCards
        nrttlm = ini.nrttlm : nrttls = ini.nrttls : iacc = ini.iacc : ifl = ini.ifl : nifd = ini.nifd : nsntl = ini.nsntl : nmntl = ini.nmntl
        iparm = ini.iparm : iaug = ini.iaug : ifd = ini.ifd : ngap = ini.ngap : irects = ini.irects : irectm = ini.irectm : nsf = ini.nsf
        fric = ini.fric : pend = ini.pend : altol = ini.altol : sfact = ini.sfact : tdeath = ini.tdeath : tbury = ini.tbury : stfsf = ini.stfsf

        ' Public LoadCards
        nod = ini.nod : idirn = ini.idirn : ncur = ini.ncur : npc = ini.npc : nptst = ini.nptst : itemp = ini.itemp : itread = ini.itread
        fac = ini.fac : pld = ini.pld : tmode = ini.tmode : tbase = ini.tbase

        ' Public PathSet
        StopFEDFAA = ini.StopFEDFAA : FEDFAAStopped = ini.FEDFAAStopped : IDTyp = ini.IDTyp : I1 = ini.I1
        WorkingDir = ini.WorkingDir : FilenameOnly = clsCom.FilenameOnly

        stress1 = clsCom.stress1 : stress8 = clsCom.stress8
        '
        a0 = ini.a0 : a1 = ini.a1 : a2 = ini.a2 : a3 = ini.a3 : a4 = ini.a4 : a5 = ini.a5 : a6 = ini.a6 : a7 = ini.a7 : a8 = ini.a8 : a9 = ini.a9 : a10 = ini.a10 : delt = ini.delt : alfa = ini.alfa
        nstep = ini.nstep : p = ini.pld : fval = ini.fval : neql = ini.neql : kht = ini.kht : numelg = ini.numelg : ihv = ini.ihv
        nblk = ini.nblk : lhex = ini.lhex : anegb1 = ini.anegb1 : anegb2 = ini.anegb2 : incflg = ini.incflg : nsnode = ini.nsnode
        sic = ini.sic : dic = ini.dic : usic = ini.usic : dusic = ini.dusic : ncon = ini.ncon : nh04 = ini.nh04 : nh05 = ini.nh05
        hh = ini.hh : pr = ini.pr : ps = ini.ps : pt = ini.pt : hix = ini.hix : hiy = ini.hiy : hiz = ini.hiz : ilimit = ini.ilimit : maxref = ini.maxref
        gass = ini.gass : irow = ini.irow : icol = ini.icol : igrad = ini.igrad : nthpz = ini.nthpz : gaz = ini.gaz : tolls = ini.tolls : ectl = ini.ectl : cvtl = ini.cvtl : rctl = ini.rctl : mthsol = ini.mthsol
        ric = ini.ric : idid = ini.idid : lcd = ini.lcd : amad = ini.amad : idflad = ini.idflad : kpri = ini.kpri
        alpha = ini.alpha : ptime = ini.ptime : tnew = ini.tnew : lcspf = ini.lcspf : numudc = ini.numudc : lutty = ini.lutty
        nodep = ini.nodep : xn = ini.xn : yn = ini.yn : zn = ini.zn : pmult = ini.pmult : strt = ini.strt
        rf = ini.rf : temp = ini.temp : dehv = ini.dehv : neesf = ini.neesf : ksizsf = ini.ksizsf : mbsize = ini.mbsize
        nsv = ini.nsv : msr = ini.msr : nsegs = ini.nsegs : nsegm = ini.nsegm : lnsv = ini.lnsv : lmsr = ini.lmsr
        ilocs = ini.ilocs : ilocm = ini.ilocm : irtls = ini.irtls : irtlm = ini.irtlm : stfs = ini.stfs : stfm = ini.stfm
        id = ini.id : ix = ini.ix : ixx = ini.ixx : iyy = ini.iyy : xx1 = ini.xx1 : xx2 = ini.xx2 : xx3 = ini.xx3 : iprec = ini.iprec
        dehva = ini.dehva : h8 = ini.h8 : itrlmt = ini.itrlmt : tollin = ini.tollin : nhxblk = ini.nhxblk : ihxblk = ini.ihxblk : mpri = ini.mpri : ipri = ini.ipri : jpri = ini.jpri : maxsiz = ini.maxsiz : iadd = ini.iadd
        idroer = ini.idroer : stepsv = ini.stepsv : incpr = ini.incpr : npb = ini.npb : ipnode = ini.ipnode : icnt1 = ini.icnt1 : icnt2 = ini.icnt2 : irfreq = ini.irfreq : bNIKE3D = ini.bNIKE3D
        lprtbd = ini.lprtbd : emodl2 = ini.emodl2 : nalsf = ini.nalsf

        temo = ini.temo ' YC 121219

    End Sub


    Sub get_time(ByRef time1 As String)

        time1 = Microsoft.VisualBasic.TimeString

    End Sub


    Sub azero(ByRef a() As Integer, ByRef n1 As Integer)
        '1-D Integer

        Dim i As Integer

        For i = 1 To n1
            a(i) = 0
        Next i

    End Sub

    Sub azero(ByRef a() As Double, ByRef n1 As Integer)
        '1-D Double

        Dim i As Integer

        For i = 1 To n1
            a(i) = 0
        Next i

    End Sub

    Sub azero(ByRef a(,) As Integer, ByRef n1 As Integer, ByRef n2 As Integer)
        '2-D Integer

        Dim i, j As Integer

        For i = 1 To n1
            For j = 1 To n2
                a(i, j) = 0
            Next j
        Next i

    End Sub

    Sub azero(ByRef a(,) As Double, ByRef n1 As Integer, ByRef n2 As Integer)
        '2-D Double

        Dim i, j As Integer

        For i = 1 To n1
            For j = 1 To n2
                a(i, j) = 0
            Next j
        Next i

    End Sub

    Sub azero(ByRef a(,,) As Double, ByRef n1 As Integer, ByRef n2 As Integer, ByRef n3 As Integer)
        '3-D Double

        Dim i, j, k As Integer

        For i = 1 To n1
            For j = 1 To n2
                For k = 1 To n3
                    a(i, j, k) = 0
                Next k
            Next j
        Next i

    End Sub
    Sub blkcpy(ByRef A() As Integer, ByRef B() As Integer, ByVal n As Integer)      ' QW 12-12-2018-
        Dim I As Integer

        For I = 1 To n
            B(I) = A(I)
        Next
    End Sub

    Sub blkcpy(ByRef A() As Double, ByRef B() As Double, ByVal n As Integer)
        Dim I As Integer

        For I = 1 To n
            B(I) = A(I)
        Next
    End Sub

    Sub blkcpy(ByRef A() As Integer, ByRef B() As Double, ByVal n As Integer)
        Dim I As Integer

        For I = 1 To n
            B(I) = A(I)
        Next
    End Sub

    Sub blkcpy(ByRef A() As Double, ByRef B() As Integer, ByVal n As Integer)
        Dim I As Integer

        For I = 1 To n
            B(I) = A(I)
        Next
    End Sub

    Sub blkcpy2D(ByRef A(,) As Double, ByRef B(,) As Double, ByVal n1 As Integer, ByVal n2 As Integer)
        Dim I1, I2 As Integer

        For I1 = 1 To n1
            For I2 = 1 To n2
                B(I1, I2) = A(I1, I2)
            Next I2
        Next I1
    End Sub

    Public Sub chgint()

        a0 = 1 / (alfa * dt * dt)
        a1 = (delt / alfa) / dt
        a2 = 1 / (alfa * dt)
        a3 = 0.5 / alfa - 1
        a4 = (delt / alfa) - 1
        a5 = dt * (0.5 * (delt / alfa) - 1)
        a6 = a0
        a7 = -a2
        a8 = -a3
        a9 = dt * (1 - delt)
        a10 = delt * dt
    End Sub



    Private nh06, nh07, nh08, nh09 As Integer         'YC 092018



    'YC 092018
    'Private xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p,
    '       xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214,
    '       xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p As Double

    Private xxx, yyy, zzz, fs1, fs2, fs3, ft1, ft2, ft3 As Double
    'YC 092018 END

    Public Sub st(ByRef n1 As Long, ByRef n2 As Long, ByRef n3 As Long, ByRef s1 As Double,
              ByRef t1 As Double, ByRef ier As Integer)

        'Dim magn As Long   'YC 092018
        Dim magn As Double

        Dim half = 0.5
        Dim one = 1.0
        Dim tol, cb, arg, brg, t2, scl, t1new, t1est, t1old, fprime, sct1, sct2, sct3 As Double
        Dim iter As Integer

        Dim sp, sm As Double 'YC 092018

        xx112 = xx1(0) - xx1(1)
        xx134 = xx1(2) - xx1(3)
        xx212 = xx2(0) - xx2(1)
        xx234 = xx2(2) - xx2(3)
        xx312 = xx3(0) - xx3(1)
        xx334 = xx3(2) - xx3(3)
        xx114 = xx1(0) - xx1(3)
        xx123 = xx1(1) - xx1(2)
        xx214 = xx2(0) - xx2(3)
        xx223 = xx2(1) - xx2(2)
        xx314 = xx3(0) - xx3(3)
        xx323 = xx3(1) - xx3(2)
        xx112p = xx1(0) + xx1(1)
        xx134p = xx1(2) + xx1(3)
        xx212p = xx2(0) + xx2(1)
        xx234p = xx2(2) + xx2(3)
        xx312p = xx3(0) + xx3(1)
        xx334p = xx3(2) + xx3(3)
        x1423 = -xx114 - xx123
        y1423 = -xx214 - xx223
        z1423 = -xx314 - xx323
        x1423p = xx114 - xx123
        y1423p = xx214 - xx223
        z1423p = xx314 - xx323

        'check for triangular segments

        If ixx(2) = ixx(3) Then GoTo 10 ' QW 12-12-2018- change ix() as ixx()
        Dim anum = -xx112 * (2 * xs - xx112p) - xx212 * (2 * ys - xx212p) - xx312 * (2 * zs - xx312p)
        s1 = anum / (xx112 * xx112 + xx212 * xx212 + xx312 * xx312)
        Dim h1 = 0.5 - 0.5 * s1
        Dim h2 = 0.5 + 0.5 * s1
        xxx = h1 * xx1(0) + h2 * xx1(1)
        yyy = h1 * xx2(0) + h2 * xx2(1)
        zzz = h1 * xx3(0) + h2 * xx3(1)
        ft1 = x1423 + s1 * x1423p
        ft2 = y1423 + s1 * y1423p
        ft3 = z1423 + s1 * z1423p
        Dim ftn1 = 0.25 * (ft1 * (xs - xxx) + ft2 * (ys - yyy) + ft3 * (zs - zzz))
        anum = xx134 * (2 * xs - xx134p) + xx234 * (2 * ys - xx234p) + xx334 * (2 * zs - xx334p)
        s1 = anum / (xx134 * xx134 + xx234 * xx234 + xx334 * xx334)
        Dim h3 = 0.5 + 0.5 * s1
        Dim h4 = 0.5 - 0.5 * s1
        xxx = h3 * xx1(2) + h4 * xx1(3)
        yyy = h3 * xx2(2) + h4 * xx2(3)
        zzz = h3 * xx3(2) + h4 * xx3(3)
        ft1 = x1423 + s1 * x1423p
        ft2 = y1423 + s1 * y1423p
        ft3 = z1423 + s1 * z1423p
        Dim ftn3 = 0.25 * (ft1 * (xs - xxx) + ft2 * (ys - yyy) + ft3 * (zs - zzz))
        fs1 = xx134 - xx112
        fs2 = xx234 - xx212
        fs3 = xx334 - xx312
        anum = fs1 * (4 * xs - xx112p - xx134p) + fs2 * (4 * ys - xx212p - xx234p) + fs3 * (4 * zs - xx312p - xx334p)
        s1 = anum / (fs1 * fs1 + fs2 * fs2 + fs3 * fs3)
        sp = 0.25 * (1.0 + s1)
        sm = 0.25 * (1.0 - s1)
        xxx = sm * (xx1(0) + xx1(3)) + sp * (xx1(1) + xx1(2))
        yyy = sm * (xx2(0) + xx2(3)) + sp * (xx2(1) + xx2(2))
        zzz = sm * (xx3(0) + xx3(3)) + sp * (xx3(1) + xx3(2))
        ft1 = x1423 + s1 * x1423p
        ft2 = y1423 + s1 * y1423p
        ft3 = z1423 + s1 * z1423p
        Dim ftn2 = ft1 * (xs - xxx) + ft2 * (ys - yyy) + ft3 * (zs - zzz)
        Dim ca = ftn1 - 0.5 * ftn2 + ftn3
        t1 = -1 - 2 * ftn1 / (ftn3 - ftn1)
        GoTo 20
10:     tol = 1.15
        ftn1 = funct(s1, -tol)
        ftn2 = funct(s1, tol)
        t1 = -1.15 - 2.3 * ftn1 / (ftn2 - ftn1)
        If Math.Abs(t1 - 1.0) < 0.0000000001 Then t1 = 0.99 '1E-10 = 0.0000000001 or we can put CDbl(1E-10)
        GoTo 30
20:     cb = ftn1 - ftn3
        arg = cb * cb - 2 * ca * ftn2
        brg = Math.Min(arg, Math.Abs(ca) - 0.0001 * one)
        t2 = (cb - Math.Sqrt(Math.Abs(arg))) / (2 * ca + 0.00000001)     'vax
        scl = 0.5 + sign(half, brg)
        t1new = scl * t2 + (1 - scl) * t1
        t1est = t1new
        iter = 1
24:     ftn1 = funct(s1, t1new)
        ftn2 = funct(s1, t1new + 0.00001)

        '   ... check for ftn1=ftn2 ...
        If Math.Abs(ftn1 - ftn2) < 0.00001 * Math.Abs(ftn1 + ftn2) Then
            t1 = t1est
            GoTo 40
        End If

        fprime = 0.0001 + 100000 * (ftn2 - ftn1)                                 'vax
        t1old = t1new
        t1new = t1old - 0.5 * (ftn1 + ftn2) / fprime
        If Math.Abs(t1new - t1old) < 0.0001 Then GoTo 28
        iter = iter + 1
        If iter < 8 Then GoTo 24 'vax
        t1 = t1est
        GoTo 30
28:     t1 = t1new
30:     ftn1 = funct(s1, t1)
        If Math.Abs(s1) > 1.02 Then GoTo 40
        If Math.Abs(t1) > 1.02 Then GoTo 40
        GoTo 50
40:     If nty = 2 Then
            ier = 1
            Return
        End If
        slvage(ier, s1, t1)
50:     sct1 = fs2 * ft3 - fs3 * ft2
        sct2 = fs3 * ft1 - fs1 * ft3
        sct3 = fs1 * ft2 - fs2 * ft1
        magn = Math.Sqrt(sct1 * sct1 + sct2 * sct2 + sct3 * sct3)
        n1 = sct1 / magn
        n2 = sct2 / magn
        n3 = sct3 / magn



    End Sub

    'YC 092018
    Private tp, tm, ps1, ps2, ps3, sp, sm, pt1, pt2, pt3 As Double
    Private h_43(20 - 1) As Double
    'YC 092018 END

    Public Function funct(ByRef s As Double, ByRef t As Double) As Double

        ' Dim h(20 - 1) As Double     ' QW 12-12-2018-

        tp = 0.25 * (1.0 + t)
        tm = 0.25 * (1.0 - t)
        ps1 = tp * xx134 - tm * xx112
        ps2 = tp * xx234 - tm * xx212
        ps3 = tp * xx334 - tm * xx312
        s = (ps1 * (xs - tm * xx112p - tp * xx134p) + ps2 * (ys - tm * xx212p - tp * xx234p) +
            ps3 * (zs - tm * xx312p - tp * xx334p)) / (ps1 * ps1 + ps2 * ps2 + ps3 * ps3)
        sp = 1.0 + s
        sm = 1.0 - s
        hh(1) = tm * sm
        hh(2) = tm * sp
        hh(3) = tp * sp
        hh(4) = tp * sm
        xxx = hh(1) * xx1(1) + hh(2) * xx1(2) + hh(3) * xx1(3) + hh(4) * xx1(4)
        yyy = hh(1) * xx2(1) + hh(2) * xx2(2) + hh(3) * xx2(3) + hh(4) * xx2(4)
        zzz = hh(1) * xx3(1) + hh(2) * xx3(2) + hh(3) * xx3(3) + hh(4) * xx3(4)
        pt1 = x1423 + s * x1423p
        pt2 = y1423 + s * y1423p
        pt3 = z1423 + s * z1423p
        funct = pt1 * (xs - xxx) + pt2 * (ys - yyy) + pt3 * (zs - zzz)

    End Function


    Public Sub slvage(ByRef ier As Integer, ByRef s As Decimal, ByRef t As Decimal)
        Dim dist1, dist2, xavg, yavg, zavg, dists, distd, anum As Double

        Dim h(20 - 1) As Double

        dist1 = Math.Pow((xx1(0) - xx1(2)), 2) + Math.Pow((xx2(0) - xx2(2)), 2) + Math.Pow((xx3(0) - xx3(2)), 2)
        dist2 = Math.Pow((xx1(1) - xx1(3)), 2) + Math.Pow((xx2(1) - xx2(3)), 2) + Math.Pow((xx3(1) - xx3(3)), 2)
        xavg = 0.25 * (xx1(0) + xx1(1) + xx1(2) + xx1(3))
        yavg = 0.25 * (xx2(0) + xx2(1) + xx2(2) + xx2(3))
        zavg = 0.25 * (xx3(0) + xx3(1) + xx3(2) + xx3(3))
        dists = Math.Sqrt(Math.Pow((xs - xavg), 2) + Math.Pow((ys - yavg), 2) + Math.Pow((zs - zavg), 2))
        distd = Math.Sqrt(Math.Max(dist1, dist2))
        If dists < 0.7 * distd Then GoTo 10
        ier = 1
        Return
10:     If Math.Abs(t) > 1.0 Then t = t / Math.Abs(t)
        If ixx(2) <> ixx(3) Then GoTo 20
        If t > 0.99 Then s = 0
        If Math.Abs(t - 1.0) < 0.0000000001 Then t = 0.99
20:     tp = 0.25 * (1.0 + t)
        tm = 0.25 * (1.0 - t)
        xxx = 0.0
        yyy = 0.0
        zzz = 0.0
        ps1 = tp * xx134 - tm * xx112
        ps2 = tp * xx234 - tm * xx212
        ps3 = tp * xx334 - tm * xx312
        anum = ps1 * (xs - tm * xx112p - tp * xx134p) + ps2 * (ys - tm * xx212p - tp * xx234p) + ps3 * (zs - tm * xx312p - tp * xx334p)
        s = anum / (ps1 * ps1 + ps2 * ps2 + ps3 * ps3)
        If Math.Abs(s) > 1 Then s = s / Math.Abs(s)
        sp = 1.0 + s
        sm = 1.0 - s
        h(0) = tm * sm
        h(1) = tm * sp
        h(2) = tp * sp
        h(3) = tp * sm
        For ind1 = 0 To 3
            xxx = xxx + h(ind1) * xx1(ind1)
            yyy = yyy + h(ind1) * xx2(ind1)
            zzz = zzz + h(ind1) * xx3(ind1)
        Next
        pt1 = -sm * xx114 - sp * xx123
        pt2 = -sm * xx214 - sp * xx223
        pt3 = -sm * xx314 - sp * xx323

    End Sub


    Public Function sign(x As Double, y As Double) As Double
        Return If(y >= 0, Math.Abs(x), -Math.Abs(x))
    End Function


    Public Sub unpkid(ByRef idu() As Integer, ByRef idp() As Integer, itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp)
        For ind1 = 1 To n
            idu(ind1) = idp(ind1)
        Next
    End Sub

    ''' <summary>
    ''' overloaded to handle 2D array
    ''' </summary>
    Public Sub unpkid(ByRef idu(,) As Integer, iduInd1 As Integer, iduInd2 As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idu(ind1 + iduInd1 - 1, iduInd2) = idp(ind1 + idpInd1 - 1, idpInd2)
        Next
    End Sub
    Public Sub unpkid(ByRef idu As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        ' idu = idp(ind1 + idpInd1, idpInd2)
        idu = idp(idpInd1, idpInd2)
    End Sub
    Public Sub unpkid(ByRef idu() As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idu(ind1) = idp(ind1 + idpInd1 - 1, idpInd2)
        Next
    End Sub

    ''' <summary>
    ''' pack Integer data 6 Or 9 terms
    ''' </summary>
    ''' <param name="idu"></param>
    ''' <param name="idp"></param>
    ''' <param name="itp"></param>
    Public Sub packid(ByRef idu() As Integer, ByRef idp() As Integer, itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idp(ind1) = idu(ind1)
        Next
    End Sub

    ''' <summary>
    ''' overloaded to handle 2D array
    ''' </summary>
    Public Sub packid(ByRef idu(,) As Integer, iduInd1 As Integer, iduInd2 As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idp(ind1 + idpInd1 - 1, idpInd2) = idu(ind1 + iduInd1 - 1, iduInd2)
        Next
    End Sub

    Public Sub packid(ByRef idu As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        ' idp(ind1 + idpInd1, idpInd2) = idu
        idp(idpInd1, idpInd2) = idu
    End Sub

    Public Sub packid(ByRef idu() As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idp(ind1 + idpInd1 - 1, idpInd2) = idu(ind1)
        Next
    End Sub




    Sub WINYIELD(StopFEDFAA, FEDFAAStopped)

    End Sub

    Sub lnesch(ui, usi, r, tvc2, u, tvc1, tt, _step, g, engc, rlnew, riksf, alfa)
    End Sub



    ' YC 121219
    'Public Sub temth(told, temo, tnew, temp, npc, p, itcurv, zero, tmode, tbase)

    'End Sub

    'Public Sub s4main(prop, anegb1, anegb12, anegb13, anegb14, temo, temp, anegb15, ln)

    'End Sub
    ' YC 121219 END

    Public Sub s6main(prop, anegb1, anegb12, ln)

    End Sub

    Public Sub fe3dh(un2, un3, un4, ia, x, matype, den, fval, idp, abdg, hxdata1, hxdata2, icnt2, numnp, nmmat)

    End Sub

    Public Sub itrpde(p, du, npoint, force, dfdu, sclf)

    End Sub

    Public Sub itrpd2(p, du, npoint, force, sclf, i, dehv, d2d1)

    End Sub

    Sub shapef(hh, ss, tt)
    End Sub

    Sub stn(n1, n2, n3, ss, tt, ierr)
    End Sub

    Sub fcalca(fni, fxi, fyi, fzi, fric, fdat, iseg, l, ss,
                      tt, n1, n2, n3, irect, dt, stft, x, fdx, fdy, fdz, fmax, fmag,
                      sfact, xls, xls2, xgt)
    End Sub

End Class

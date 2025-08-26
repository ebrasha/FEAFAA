'This file contains all the methods of slave2.f
Partial Public Class clsSolve

    ' YC 092018
    Public istold As Integer, lprtbd As Boolean
    Public emodl2 As Double, amx, amy, amz As Double


    'Private ix(10) As Integer
    'Private xx1(20), xx2(20), xx3(20) As Double
    'Private xs, ys, zs As Double
    ' YC 092018 END


    ''' <summary>
    ''' to determine if slidesurface penalty element is active
    ''' </summary>
    ''' <param name="x"></param>
    ''' <param name="rhs"></param>
    ''' <param name="irect"></param>
    ''' <param name="lmsr"></param>
    ''' <param name="msr"></param>
    ''' <param name="nsv"></param>
    ''' <param name="iloc"></param>
    ''' <param name="irtl"></param>
    ''' <param name="stf"></param>
    ''' <param name="nsn"></param>
    ''' <param name="nmn"></param>
    ''' <param name="nty"></param>
    ''' <param name="idp"></param>
    ''' <param name="lmn"></param>
    ''' <param name="s"></param>
    ''' <param name="irects"></param>
    ''' <param name="lnsv"></param>
    ''' <param name="fric"></param>
    ''' <param name="fdat"></param>
    ''' <param name="iseg"></param>
    ''' <param name="abdg"></param>
    ''' <param name="sfdata"></param>
    ''' <param name="kfstor"></param>
    ''' <param name="lfstor"></param>
    ''' <param name="iebuf"></param>
    ''' <param name="xls"></param>
    ''' <param name="xgn"></param>
    ''' <param name="xgt"></param>
    ''' <param name="sfact"></param>
    ''' <param name="pend"></param>
    ''' <param name="laugon"></param>
    ''' <param name="numnp"></param>
    ''' <param name="fval"></param>
    ''' <param name="adeath"></param>
    ''' <param name="ifd"></param>
    ''' <param name="r_old"></param>
    ''' <param name="ss_old"></param>
    ''' <param name="l_old"></param>
    Public Sub slave2(ByRef x(,) As Double, ByRef rhs() As Double, ByRef irect(,) As Integer,
                      ByRef lmsr() As Integer, ByRef msr() As Integer, ByRef nsv() As Integer,
                      ByRef iloc() As Integer, ByRef irtl() As Integer, ByRef stf() As Double,
                      ByRef nsn As Integer, ByRef nmn As Integer, ByRef nty As Integer, ByRef idp(,) As Integer,
                      ByRef lmn() As Integer, ByRef s() As Double, ByRef lnsv() As Integer,
                      ByRef fric() As Double, ByRef fdat(,) As Double, ByRef iseg(,) As Integer,
                      ByRef abdg(,) As Double, ByRef sfdata1(,) As Double, ByRef sfdata2(,) As Integer, ByRef kfstor As Integer,
                      ByRef lfstor As Integer, ByRef iebuf As Integer, ByRef xls(,) As Double,
                      ByRef xgn As Double, ByRef xgt As Double, ByRef sfact As Double,
                      ByRef pend As Double, ByRef laugon As Boolean, ByRef numnp As Integer,
                      ByRef fval() As Double, ByRef adeath As Double, ByRef ifd As Integer,
                      ByRef r_old(,) As Double, ByRef ss_old(,) As Double, ByRef l_old() As Integer)


        'Dim id(5) As Integer, e(5) As Double, xrb(2, 4) As Double  ' YC 102418
        Dim id(6) As Integer, e(6) As Double, xrb(3, 5) As Double

        Dim i, j, k, l, nn, j1, j2, j3, k1, k2, k3, ierr, idebug, ierrt, jj, lmmax As Integer
        Dim lsh, lnk, lrbr, lrbk, lifd As Boolean
        Dim ans, fmag, fxi, fyi, fzi, fni, detv, dx, pi, paramm, paraml, dx1, area As Double
        Dim f1, f2, f3, sst, ttt, amxt, amyt, amzt, errn, errnt, effstf, asf, stft As Double
        Dim fdx, fdy, fdz, fmax, r(24) As Double

        'Dim n1, n2, n3 As Double, r(24) As Double    'YC 092018

        'c ... determine interface force death options
        lifd = False
        If ifd <> 0 Then
            If adeath <> 1.0 Then lifd = True ' begin killing off interface force
            If lelstf And lfpass Then lifd = False ' need at least an initial force calc

            'If Not lifd And lelstf Then Call objNik3d.azero(r_old, 3 * nsn) ' YC 092018
            If Not lifd And lelstf Then Call azero(r_old, 3 - 1, nsn - 1)

        End If

        Dim zero = 0.0
        Dim fcoeff = Math.Pow(fric(0), 2) + Math.Pow(fric(1), 2) + Math.Pow(fric(2), 2)
        melemt = 0
        xgn = 0.0
        xgt = 0.0


        'Call Check5(n1, n2, n3, ss, tt)

        ' loop over slave node indices

        'For ii = 0 To nsn - 1      ' YC 102418
        For ii = 1 To nsn
            'Call Check1(ii, irtl(ii))
            If iphase = 3 Then
                If ii = 5 Then
                    ii = ii
                End If
            End If
            lrbr = False
            lrbk = False
            i = nsv(ii)                   ' slave node number for slave index ii
            j = iloc(ii)                  ' master node index for slave index ii
            k = msr(j)                    ' master node number for index j     
            l = irtl(ii)                  ' master facet for slave index ii
            If lifd Then l = l_old(ii) ' use old master facet
            ans = 1000000.0
            fmag = 0.0
            lsh = (fcoeff = 0)
            lnk = True

            ' gather master and slave coords into local arrays

            'For jj = 0 To 3     ' YC 102418
            For jj = 1 To 4        ' QW 12-12-2018-

                nn = irect(jj, l)
                ixx(jj) = nn                ' QW 12-12-2018- change ix() as ixx()

                'YC 102418
                'xx1(jj) = x(0, nn)
                'xx2(jj) = x(1, nn)
                'xx3(jj) = x(2, nn)
                xx1(jj) = x(1, nn)
                xx2(jj) = x(2, nn)
                xx3(jj) = x(3, nn)
                'YC 102418 END

            Next

            'YC 102418
            'xs = x(0, i)
            'ys = x(1, i)
            'zs = x(2, i)
            xs = x(1, i)
            ys = x(2, i)
            zs = x(3, i)
            'YC 102418 END

            If lifd Then            ' skip penetration step

                ' YC 102418
                'ss = ss_old(0, ii)    
                'tt = ss_old(1, ii)
                ss = ss_old(1, ii)
                tt = ss_old(2, ii)
                ' YC 102418 END

                Call shapef(hh, ss, tt)    ' get shape function values    ' QW 12-12-2018- . h chnge to hh

                ' YC 102418
                'fxi = r_old(0, ii) * adeath ' kill off last force vector       
                'fyi = r_old(1, ii) * adeath
                'fzi = r_old(2, ii) * adeath
                fxi = r_old(1, ii) * adeath
                fyi = r_old(2, ii) * adeath
                fzi = r_old(3, ii) * adeath
                ' YC 102418 END

                fni = Math.Sqrt(fxi * fxi + fyi * fyi + fzi * fzi)
                If laugon Then       ' zero out lagrange mults

                    ' YC 102418
                    'xls(0, ii) = 0
                    'xls(1, ii) = 0
                    'xls(2, ii) = 0
                    xls(1, ii) = 0
                    xls(2, ii) = 0
                    xls(3, ii) = 0
                    ' YC 102418 END

                End If

                '       goto  200             ! skip to rhs calcs   ' YC 102418
                GoTo 200

            End If

            'c.... check for penetration

            'If k <> ixx(0) Then GoTo 20 ' YC 102418
            If k <> ixx(1) Then GoTo 20
            k1 = 1
            k2 = 2
            k3 = 4
            GoTo 50

            '20:         If k <> ixx(1) Then GoTo 30 ' YC 102418
20:         If k <> ixx(2) Then GoTo 30
            k1 = 2
            k2 = 3
            k3 = 1
            GoTo 50

            '30:         If k <> ixx(2) Then GoTo 40 ' YC 102418
30:         If k <> ixx(3) Then GoTo 40
            k1 = 3
            k2 = 4
            k3 = 2

            'If ixx(2) = ixx(3) Then k2 = 1 ' YC 102418
            If ixx(3) = ixx(4) Then k2 = 1

            GoTo 50

            '40:         If k <> ixx(3) Then GoTo 50 ' YC 102418
40:         If k <> ixx(4) Then GoTo 50
            k1 = 4
            k2 = 1
            k3 = 3



50:         If nty = 1 Then GoTo 60
            Call ptimeSub(k1, k2, k3, detv)

            'Call objNik3d.chardx(dx) ' YC 092018
            Call objNik3d.chardx(dx, xx1, xx2, xx3)

            If detv / dx > 50 Then GoTo 102

            'c.... compute contact point and unit normal to master segment

60:         ierr = 0

            idebug = 0
            '
            If idebug <> 1 Then

                'If istold <> 1 Then Call objNik3d.st(n1, n2, n3, ss, tt, ierr)     'YC 092018
                'If istold <> 1 Then Call st(n1, n2, n3, ss, tt, ierr)
                If istold <> 1 Then Call objNik3d.st(n1, n2, n3, ss, tt, ierr,
                    nty, xx1, xx2, xx3, ixx, xs, ys, zs, hh,
            xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p,
            xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p, amx, amy, amz)


                If istold = 1 Then Call stn(n1, n2, n3, ss, tt, ierr)
            End If

            If idebug = 1 Then
                If istold <> 1 Then

                    Call stn(n1, n2, n3, ss, tt, ierr)
                    f1 = n1
                    f2 = n2
                    f3 = n3
                    sst = ss
                    ttt = tt
                    amxt = amx
                    amyt = amy
                    amzt = amz
                    ierrt = ierr
                    ierr = 0

                    'Call objNik3d.st(n1, n2, n3, ss, tt, ierr)     'YC 092018
                    Call st(n1, n2, n3, ss, tt, ierr)
                    '
                End If
                '
                If istold = 1 Then

                    'Call objNik3d.st(n1, n2, n3, ss, tt, ierr)     'YC 092018
                    Call st(n1, n2, n3, ss, tt, ierr)
                    f1 = n1
                    f2 = n2
                    f3 = n3
                    sst = ss
                    ttt = tt
                    amxt = amx
                    amyt = amy
                    amzt = amz
                    ierrt = ierr
                    ierr = 0
                    Call stn(n1, n2, n3, ss, tt, ierr)
                End If

                If ierr <> ierrt Then
                    errn = (xs - amx) * n1 + (ys - amy) * n2 + (zs - amz) * n3
                    errnt = (xs - amxt) * f1 + (ys - amyt) * f2 + (zs - amzt) * f3
                    '         write(100,79)i,ierr,ierrt,ss,sst,tt,ttt,errn,errnt
                    '79       format(/,3(i5),6(1pe11.3),/)
                End If
                '
                If ierr = ierrt And ierr = 0 Then
                    If Math.Abs(ss - sst) > 0.0001 Or Math.Abs(tt - ttt) > 0.0001 Or (f1 * n1 + f2 * n2 + f3 * n3) < 0.99 Then
                        errn = (xs - amx) * n1 + (ys - amy) * n2 + (zs - amz) * n3
                        errnt = (xs - amxt) * f1 + (ys - amyt) * f2 + (zs - amzt) * f3
                        '          write(100,69)i,ss,sst,tt,ttt,n1,n2,n3,f1,f2,f3,
                        '     &                 amx,amy,amz,amxt,amyt,amzt,xs,ys,zs,errn,errnt
                        '69        format(/,i5,10(1pe11.3),/,5x,11(1pe11.3),/)
                    End If
                End If

            End If    ' end debug

            If ierr = 1 Then GoTo 102

            'c.... check for penetration

            ans = n1 * (xs - amx) + n2 * (ys - amy) + n3 * (zs - amz)

            If ans < pend Then GoTo 140

            effstf = stf(l) * adeath
            '
            If nty = 1 Then GoTo 70

            If ilimit = 1 Then dx = 0 ' ilimit=1 => Full Newton Raphson
            If laugon Then
                'TODO - check continue logic. Continue main for loop?
                Continue For
            Else
                If ans < dx And ans > 0 Then
                    asf = 10.0
                    effstf = effstf / asf
                End If

                If ans > dx Then GoTo 102
                If ans < dx And ans > 0 Then effstf = (1.0 - ans / dx) * effstf ' since stiffness
                If ans > 0 Then ans = 0 ' still used
            End If

70:         If laugon Then

                'fni = Math.Min(zero, xls(0, ii) + ans * stf(l) * adeath)        ' YC 102418
                'xls(0, ii) = fni
                fni = Math.Min(zero, xls(1, ii) + ans * stf(l) * adeath)
                xls(1, ii) = fni

                If fni < 0.0 Then
                    'c         ... traction ...
                    xgn = Math.Max(xgn, (-1.0 * ans))
                    lsh = True
                    lnk = False
                Else
                    'c         ... no traction ...
                    If ans < dx Then
                        'c           ... small gap ...
                        lsh = True
                        lnk = False
                        effstf = (1.0 - ans / dx) * effstf
                    ElseIf ans = 0 Then
                        lnk = False
                    Else
                        'c           ... big gap ...
                        GoTo 102
                    End If
                End If
            Else
                fni = ans * stf(l) * adeath
            End If

            fxi = n1 * fni
            fyi = n2 * fni
            fzi = n3 * fni

            If fcoeff <> 0 Then    ' begin friction branch.  ' QW 12-12-2018- the fdat, iseg will be changed later
                If laugon Then
                    stft = stf(l) * adeath

                    'Call fcalca(fni, fxi, fyi, fzi, fric, fdat(0, ii), iseg(0, ii), l, ss,
                    '      tt, n1, n2, n3, irect, dt, stft, x, fdx, fdy, fdz, fmax, fmag,
                    '      sfact, xls(1, ii), xls(2, ii), xgt)      ' YC 102418
                    Call fcalca(fni, fxi, fyi, fzi, fric, fdat(1, ii), iseg(1, ii), l, ss,
                                tt, n1, n2, n3, irect, dt, stft, x, fdx, fdy, fdz, fmax, fmag,
                                sfact, xls(2, ii), xls(3, ii), xgt)

                Else
                    If lprtbd Then

                        'Call objNik3d.areal(dx1, area)  'YC 092018
                        Call objNik3d.areal(dx1, area, xx1, xx2, xx3, detv)  ' detv see ptime()

                        pi = 3.1415926537
                        paramm = 700.0
                        paraml = Math.Pow(10, (-(paramm / emodl2)))
                        stft = (paraml / (1.0 - paraml)) * area * adeath / (2.0 * (4.0 / pi))
                    Else
                        stft = 0.01 * stf(l) * adeath
                    End If

                    Dim fdat_ii(10) As Double, iseg_ii(2) As Integer
                    Call objComsub.ArrayExtract1Dfrom2D(fdat, 1 - 1, ii - 1, fdat_ii, 10)
                    Call objComsub.ArrayExtract1Dfrom2D(iseg, 1 - 1, ii - 1, iseg_ii, 2)
                    Call fcalc(fni, fxi, fyi, fzi, fric, fdat_ii, iseg_ii, l, ss, tt,
                                n1, n2, n3, amx, amy, amz, irect, dt, stft, x, fdx, fdy, fdz,
                                fmax, fmag)
                    Call objComsub.ArrayInsert1Dto2D(fdat_ii, 10, fdat, 1 - 1, ii - 1)
                    Call objComsub.ArrayInsert1Dto2D(iseg_ii, 2, iseg, 1 - 1, ii - 1)
                    ' Call fcalc(fni, fxi, fyi, fzi, fric, fdat, iseg, l, ss, tt,
                    ' n1, n2, n3, amx, amy, amz, irect, dt, stft, x, fdx, fdy, fdz,
                    ' fmax, fmag, ii)
                End If
            End If                     ' End friction branch
            'c  ....  at beginning of time step store last converged contact force,
            'c        isoparm coords and master facet number
            If ifd <> 0 And (Not lifd) And lelstf Then

                ' YC 102418
                'r_old(0, ii) = fxi
                'r_old(1, ii) = fyi
                'r_old(2, ii) = fzi
                'ss_old(0, ii) = ss
                'ss_old(1, ii) = tt
                r_old(1, ii) = fxi
                r_old(2, ii) = fyi
                r_old(3, ii) = fzi
                ss_old(1, ii) = ss
                ss_old(2, ii) = tt
                ' YC 102418 END

                l_old(ii) = l
            End If

200:

            'c   ... update force (master nodes)

            'For jj = 0 To 3         ' YC 102418
            For jj = 1 To 4
                j3 = 3 * jj
                j2 = j3 - 1
                j1 = j2 - 1
                r(3 + j1) = fxi * hh(jj)          ' QW 12-12-2018- . h_43 chnge to hh
                r(3 + j2) = fyi * hh(jj)
                r(3 + j3) = fzi * hh(jj)
            Next


            'c   ... comment bsigeteqi calls when including brick bsi nodes
            'c       generalized forces using exkcbs.f

            'Call clsInput.unpkid(id, idp, 0, i, 1)    'YC 092018,  YC 102418
            Call unpkid(id, idp, 1, i, 1)

            'lmn(0) = id(0)     '   YC 102418
            'lmn(1) = id(1)
            'lmn(2) = id(2)
            lmn(1) = id(1)
            lmn(2) = id(2)
            lmn(3) = id(3)

            'For jj = 0 To 3    '   YC 102418
            For jj = 1 To 4

                'Call clsInput.unpkid(id, idp, 0, ixx(jj), 1)    'YC 092018, YC 102418
                Call unpkid(id, idp, 1, ixx(jj), 1)

                'lmn(3 + 3 * (jj + 1) - 2 - 1) = id(0)   '   YC 102418
                'lmn(3 + 3 * (jj + 1) - 1 - 1) = id(1)
                'lmn(3 + 3 * (jj + 1) - 1) = id(2)
                lmn(3 + 3 * jj - 2) = id(1)
                lmn(3 + 3 * jj - 1) = id(2)
                lmn(3 + 3 * jj) = id(3)

            Next
            'c
            'c.... update force (slave nodes)

            'r(0) = -fxi     '   YC 102418
            'r(1) = -fyi
            'r(2) = -fzi
            r(1) = -fxi
            r(2) = -fyi
            r(3) = -fzi

            'lmmax = lmn(0)  '   YC 102418
            lmmax = lmn(1)

            'For kk = 1 To 14     '   YC 102418
            For kk = 2 To 15
                lmmax = Math.Max(lmmax, lmn(kk))
            Next
            If lmmax > neql Then
                lrbr = True

                '   YC 102418
                'xrb(0, 1) = xs
                'xrb(1, 1) = ys
                'xrb(2, 1) = zs
                'xrb(0, 1) = xx1(0)
                'xrb(1, 1) = xx2(0)
                'xrb(2, 1) = xx3(0)
                'xrb(0, 2) = xx1(1)
                'xrb(1, 2) = xx2(1)
                'xrb(2, 2) = xx3(1)
                'xrb(0, 3) = xx1(2)
                'xrb(1, 3) = xx2(2)
                'xrb(2, 3) = xx3(2)
                'xrb(0, 4) = xx1(3)
                'xrb(1, 4) = xx2(3)
                'xrb(2, 4) = xx3(3)
                xrb(1, 1) = xs
                xrb(2, 1) = ys
                xrb(3, 1) = zs
                xrb(1, 2) = xx1(1)
                xrb(2, 2) = xx2(1)
                xrb(3, 2) = xx3(1)
                xrb(1, 3) = xx1(2)
                xrb(2, 3) = xx2(2)
                xrb(3, 3) = xx3(2)
                xrb(1, 4) = xx1(3)
                xrb(2, 4) = xx2(3)
                xrb(3, 4) = xx3(3)
                xrb(1, 5) = xx1(4)
                xrb(2, 5) = xx2(4)
                xrb(3, 5) = xx3(4)
                '   YC 102418 END

            Else
                'c        ... update rhs ...

                'For kk = 0 To 14
                For kk = 1 To 15
                    jj = lmn(kk)
                    'If jj = 5045 Then
                    'Call Check4(ii, kk, rhs(jj), r(kk), 1)
                    'End If


                    If jj = 0 Then Continue For
                    rhs(jj) = rhs(jj) + r(kk)
                Next
            End If

            'If iphase = 3 Then
            'Call Checkrhs2(lmn, r, ii)
            'End If


            If lifd Then GoTo 140 ' skip stiffness for interface force death

            'c   ... zero history? ...
102:        If laugon Then
                If lsh Then GoTo 105
            Else
                If ans <= 0.0 Or fcoeff = 0.0 Then GoTo 105
            End If

            'iseg(1, ii) = 0        ' YC 102418
            'fdat(7, ii) = 0.0
            'fdat(8, ii) = 0.0
            'fdat(9, ii) = 0.0
            iseg(2, ii) = 0
            fdat(8, ii) = 0
            fdat(9, ii) = 0
            fdat(10, ii) = 0

            'c   ... add stiffness? ...
105:        If laugon Then
                If lnk Then GoTo 140
            Else
                If (ans > dx) Then GoTo 140
            End If

            'c   ... uncomment following lines when including brick bsi nodes
            'c       generalized forces using exkcbs.f

            If iphase - 2 <= 0 Then GoTo 110 Else GoTo 140
110:        If iref = 0 Then GoTo 120 Else GoTo 140
120:        If fcoeff = 0 Then Call nstiff(effstf, n1, n2, n3, e)
            If fcoeff > 0 Then Call fstiff(n1, n2, n3, effstf, fxi, fyi, fzi, fni, fmax, fmag, e, stft)
            Call stfinf(hh, e, s)       ' QW 03-26-2019 . h_43 chnge to hh
            'Call Check1D(e, 6, istep)
            'Call Check1D(hh, 4, istep)

            lrbk = True                   ' used as lstif (i.e. reform) in exkcrb
            If lrbr Then GoTo 140
            'c

            ' YC 102418
            'lmn(0) = i
            'lmn(1) = ixx(0)
            'lmn(2) = ixx(1)
            'lmn(3) = ixx(2)
            'lmn(4) = ixx(3)
            lmn(1) = i
            lmn(2) = ixx(1)
            lmn(3) = ixx(2)
            lmn(4) = ixx(3)
            lmn(5) = ixx(4)
            ' YC 102418 END



            ' QW 12-12-2018-
            'Dim diag(21, numnp) As Double
            'For i = 1 To 21
            'For j = 1 To numnp
            'diag(i, j) = abdg((j - 1) * 21 + i)
            'Next
            'Next
            'For i = 1 To n
            'Call bdg53(diag, s, lmn)
            Call bdg53(abdg, s, lmn)
            'For i = 1 To 21
            'For j = 1 To numnp
            'abdg((j - 1) * 21 + i) = diag(i, j)
            'Next
            'Next
            'If ii = 1 Then
            'Call Check2D(abdg, 6, numnp, istep)
            'End If

            ' YC 092018
            'Call blkcpy(s, sfdata(kfstor), ksizsf)
            'Call blkcpy(lmn, sfdata(lfstor), neesf)

            'Dim sfdata_kfstor(ksizsf) As Double
            'Call blkcpy(s, sfdata_kfstor, ksizsf)
            'Call objComsub.ArrayInsert1Dto1D(sfdata_kfstor, ksizsf, sfdata1, kfstor - 1)

            'Dim sfdata_lfstor(neesf) As Integer          ' QW 12-12-2018-

            'Call blkcpy(lmn, sfdata_lfstor, neesf)
            'Call objComsub.ArrayInsert1Dto1D(sfdata_lfstor, neesf, sfdata2, lfstor - 1)
            ' YC 092018 END
            For i = 1 To ksizsf
                sfdata1(i, kfstor) = s(i)
            Next
            For i = 1 To neesf
                sfdata2(i, lfstor) = lmn(i)
            Next
            kfstor = kfstor + 1
            lfstor = lfstor + 1

            'kfstor = kfstor + ksizsf
            'lfstor = lfstor + neesf
            iebuf = iebuf + 1
            numelf = numelf + 1
            'Call Check4(ii, nsn, nmn, numelf)
            If numelf = 3030 Then
                n = n
            End If
140:
            '
            'c
            'c   ... uncomment following lines when including brick bsi nodes
            'c       generalized forces using exkcbs.f
            'c
            'c c    ... bsidofc returns lmmax = 0 if no rigid body nodes and sets bsi  pngv
            'c c    ... flag idbsf for exkcbsi also lrbr set = false in exkcbsi        pngv
            '
        Next

    End Sub


End Class

'  ref org fortran code 
'cidp# 1 "slave2.F"
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine slave2(x,rhs,irect,lmsr,msr,nsv,iloc,irtl,stf,
'     & nsn,nmn,nty,idp,lmn,s,irects,lnsv,fric,
'     & fdat,iseg,diag,sfdata,kfstor,lfstor,iebuf,
'     & xls,xgn,xgt,sfact,pend,laugon,numnp,fval,adeath,
'     & ifd,r_old,ss_old,l_old)
'c      
'      implicit double precision (a-h,o-z)                              
'c
'c===> module to determine if slidesurface penalty element is active
'c
'      common/double/iprec
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,
'     &            nipmx,nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk12/dtx0,dt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
'      
'
'      common/bk20/kpri,nstep,ite,ilimit,iref
'
'c      
'      common/bk43/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
'      common/bk44/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
'      common/bk45/xs,ys,zs,sig(3),epx,mx,ix(10),iy(10)
'      common/bk46/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
'      common/bk58/nsgcnt
'      common/bk59/r(24)
'      common/fissl1/melemt,nnns,n2g,llls     
'      common/elcnts/numelf
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye3/neehx,ksizhx,neesf,ksizsf
'      common/bkneq/neql
'            
'      logical lfpass,llpass,lelstf
'      common/ndinit/lfpass,llpass
'      common/elstfb/lelstf
'      common/stold/istold
'	common/partbd/emodl2,lprtbd !added drb 12/02
'      dimension x(3,*),rhs(*),irect(4,*),lmsr(*),msr(*),nsv(*),iloc(*),
'     & irtl(*),stf(*),idp(6,*),lmn(*),s(*),id(6),irects(4,*),
'     & lnsv(*),fric(*),iseg(2,*),fdat(10,*),e(6),fval(*),
'     & diag(*),sfdata(*),xls(3,*),xrb(3,5),
'     & r_old(3,nsn),ss_old(2,nsn),l_old(nsn)
'      logical lsh,lnk,laugon,lrbr,lrbk,lbsibr,lifd
'	logical lprtbd !added drb 12/02
'
'      double precision n1,n2,n3
'
'c ... determine interface force death options
'       lifd=.false.
'      if(ifd.ne.0) then
'       if (adeath.ne.1.0) lifd=.true.     ! begin killing off interface force
'       if(lelstf.and.lfpass) lifd=.false. ! need at least an initial force calc
'       if(.not.lifd .and. lelstf) call azero(r_old,3*nsn)
'      endif
'
'      zero = 0.0
'      fcoeff=fric(1)**2+fric(2)**2+fric(3)**2
'      melemt=0
'      xgn=0.0
'      xgt=0.0
'      do 1000 ii=1,nsn            ! loop over slave node indices
'      lrbr=.false.
'      lrbk=.false.
'      i=nsv(ii)                   ! slave node number for slave index ii
'      j=iloc(ii)                  ! master node index for slave index ii
'      k=msr(j)                    ! master node number for index j
'      l=irtl(ii)                  ! master facet for slave index ii
'      if(lifd) l=l_old(ii)        ! use old master facet
'      ans=1.e+6
'      fmag=0.
'      lsh=(fcoeff.eq.0)
'      lnk=.true.
'
'      do 10 jj=1,4      ! gather master and slave coords into local arrays
'      nn=irect(jj,l)
'      ix(jj)=nn
'      xx1(jj)=x(1,nn)
'      xx2(jj)=x(2,nn)
' 10   xx3(jj)=x(3,nn)
'      xs=x(1,i)
'      ys=x(2,i)
'      zs=x(3,i)
'
'      if(lifd) then            ! skip penetration step
'       ss = ss_old(1,ii)
'       tt = ss_old(2,ii)
'       call shapef(h,ss,tt)    ! get shape function values
'       fxi = r_old(1,ii)*adeath ! kill off last force vector
'       fyi = r_old(2,ii)*adeath
'       fzi = r_old(3,ii)*adeath
'       fni = sqrt(fxi*fxi+fyi*fyi+fzi*fzi)
'       if(laugon) then       ! zero out lagrange mults
'        xls(1,ii)=0
'        xls(2,ii)=0
'        xls(3,ii)=0
'       endif
'       goto  200             ! skip to rhs calcs
'      endif
'c
'c.... check for penetration
'c
'      if(k.ne.ix(1)) go to 20
'      k1=1
'      k2=2
'      k3=4
'      go to 50
' 20   if(k.ne.ix(2)) go to 30
'      k1=2
'      k2=3
'      k3=1
'      go to 50
' 30   if(k.ne.ix(3)) go to 40
'      k1=3
'      k2=4
'      k3=2
'      if(ix(3).eq.ix(4)) k2=1
'      go to 50
' 40   if(k.ne.ix(4)) go to 50
'      k1=4
'      k2=1
'      k3=3
' 50   if(nty.eq.1) go to 60
'      call ptime(k1,k2,k3,detv)
'      call chardx(dx)
'      if(detv/dx.gt.50) go to 102
'c
'c.... compute contact point and unit normal to master segment
'c
' 60   ierr=0
'
'
'      idebug=0
'
'      if(idebug.ne.1) then
'
'      if(istold.ne.1) call st (n1,n2,n3,ss,tt,ierr)
'      if(istold.eq.1) call stn(n1,n2,n3,ss,tt,ierr)
'
'      endif
'
'      if(idebug.eq.1) then
'      if(istold.ne.1) then
'        call stn(n1,n2,n3,ss,tt,ierr)
'        f1=n1
'        f2=n2
'        f3=n3
'        sst=ss
'        ttt=tt
'        amxt=amx
'        amyt=amy
'        amzt=amz
'        ierrt=ierr
'        ierr=0
'        call st (n1,n2,n3,ss,tt,ierr)
'
'      endif
'
'      if(istold.eq.1) then
'        call st (n1,n2,n3,ss,tt,ierr)
'        f1=n1
'        f2=n2
'        f3=n3
'        sst=ss
'        ttt=tt
'        amxt=amx
'        amyt=amy
'        amzt=amz
'        ierrt=ierr
'        ierr=0
'        call stn(n1,n2,n3,ss,tt,ierr)
'
'       endif
'
'        if(ierr.ne.ierrt) then
'          errn =(xs - amx )*n1 + (ys - amy )*n2 + (zs - amz )*n3
'          errnt=(xs - amxt)*f1 + (ys - amyt)*f2 + (zs - amzt)*f3
'         write(100,79)i,ierr,ierrt,ss,sst,tt,ttt,errn,errnt
'79       format(/,3(i5),6(1pe11.3),/)
'        endif
'
'        if(ierr.eq.ierrt.and.ierr.eq.0) then
'         if(abs(ss-sst).gt.0.0001 .or. abs(tt-ttt).gt.0.0001 .or.
'     &     (f1*n1 + f2*n2 + f3*n3) .lt. 0.99 ) then
'          errn =(xs - amx )*n1 + (ys - amy )*n2 + (zs - amz )*n3
'          errnt=(xs - amxt)*f1 + (ys - amyt)*f2 + (zs - amzt)*f3
'          write(100,69)i,ss,sst,tt,ttt,n1,n2,n3,f1,f2,f3,
'     &                 amx,amy,amz,amxt,amyt,amzt,xs,ys,zs,errn,errnt
'69        format(/,i5,10(1pe11.3),/,5x,11(1pe11.3),/)
'         endif
'        endif
'
'      endif    ! end debug
'
'      if(ierr.eq.1) go to 102
'c
'c.... check for penetration
'
'      ans=n1*(xs-amx)+n2*(ys-amy)+n3*(zs-amz)
'
'      if(ans.lt.pend) goto 140
'c
'      effstf = stf(l)*adeath
'
'      if(nty.eq.1)  go to 70
'c
'      if(ilimit.eq.1) dx=0  ! ilimit=1 => Full Newton Raphson
'      if(laugon) then
'        continue
'      else
'c
'		if(ans.lt.dx.and.ans.gt.0) then						! added by Qiang 
'			asf=10.											! added by Qiang 
'			effstf=effstf/asf								! added by Qiang 
'		end if
'c
'		if(ans.gt.dx) go to 102
'		if(ans.lt.dx.and.ans.gt.0) effstf = (1.0-ans/dx)*effstf ! since stiffness
'		if(ans.gt.0.) ans=0.                                    ! still used
'      endif
'c
'   70 if(laugon) then
'        fni=min(zero,xls(1,ii)+ans*stf(l)*adeath)
'        xls(1,ii)=fni
'        if(fni.lt.0.0) then
'c         ... traction ...
'          xgn=max(xgn,(-1.*ans))
'          lsh=.true.
'          lnk=.false.
'        else
'c         ... no traction ...
'          if(ans.lt.dx) then
'c           ... small gap ...
'            lsh=.true.
'            lnk=.false.
'            effstf = (1.0-ans/dx)*effstf
'          elseif(ans.eq.0) then
'            lnk=.false.
'          else
'c           ... big gap ...
'            goto 102
'          endif
'        endif
'      else
'        fni=ans*stf(l)*adeath
'      endif
'c
'      fxi=n1*fni
'      fyi=n2*fni
'      fzi=n3*fni
'c
'       if (fcoeff.ne.0) then    ! begin friction branch
'         if (laugon) then
'          stft=stf(l)*adeath
'          call fcalca(fni,fxi,fyi,fzi,fric,fdat(1,ii),iseg(1,ii),l,ss,
'     &                tt,n1,n2,n3,irect,dt,stft,x,fdx,fdy,fdz,fmax,fmag,
'     &                sfact,xls(2,ii),xls(3,ii),xgt)
'         else
'		if(lprtbd)then !added drb 12/02
'			call areal(dx1,area)
'			pi=3.1415926537
'			paramm=700.
'			paraml=10**(-(paramm/emodl2))
'			stft=(paraml/(1.-paraml))*area*adeath/(2.*(4./pi))
'		else
'          stft=0.01*stf(l)*adeath
'		endif
'          call fcalc(fni,fxi,fyi,fzi,fric,fdat(1,ii),iseg(1,ii),l,ss,tt,
'     &               n1,n2,n3,amx,amy,amz,irect,dt,stft,x,fdx,fdy,fdz,
'     &               fmax,fmag)
'         endif
'       endif                     ! end friction branch
'c  ....  at beginning of time step store last converged contact force,
'c        isoparm coords and master facet number
'       if(ifd.ne.0 .and. (.not.lifd) .and. lelstf) then
'        r_old(1,ii)=fxi
'        r_old(2,ii)=fyi
'        r_old(3,ii)=fzi
'        ss_old(1,ii)=ss
'        ss_old(2,ii)=tt
'        l_old(ii)=l
'       endif
'c      endif                     ! end .not. udch branch
'
'  200 continue
'
'c      if (.not.udch) then       ! begin .not. udch branch
'c   ... update force (master nodes)
'       do 80 jj=1,4
'       j3=3*jj
'       j2=j3-1
'       j1=j2-1
'       r(3+j1)=fxi*h(jj)
'       r(3+j2)=fyi*h(jj)
' 80    r(3+j3)=fzi*h(jj)
'c      endif                     ! end .not. udch branch
'c
'c   ... comment bsigeteqi calls when including brick bsi nodes
'c       generalized forces using exkcbs.f
'c
'       call unpkid(id,idp(1,i),1)
'       lmn(1)=id(1)
'       lmn(2)=id(2)
'       lmn(3)=id(3)
'
'       do 90 jj=1,4
'       call unpkid(id,idp(1,ix(jj)),1)
'       lmn(3+3*jj-2)=id(1)
'       lmn(3+3*jj-1)=id(2)
'       lmn(3+3*jj  )=id(3)
'
'   90  continue
'c
'c.... update force (slave nodes)
'c
'        r(1)=-fxi
'        r(2)=-fyi
'        r(3)=-fzi
'        lmmax=lmn(1)
'        do 95 kk=2,15
'        lmmax=max(lmmax,lmn(kk))
'   95   continue
'       if (lmmax.gt.neql) then
'         lrbr=.true.
'         xrb(1,1)=xs
'         xrb(2,1)=ys
'         xrb(3,1)=zs
'         xrb(1,2)=xx1(1)
'         xrb(2,2)=xx2(1)
'         xrb(3,2)=xx3(1)
'         xrb(1,3)=xx1(2)
'         xrb(2,3)=xx2(2)
'         xrb(3,3)=xx3(2)
'         xrb(1,4)=xx1(3)
'         xrb(2,4)=xx2(3)
'         xrb(3,4)=xx3(3)
'         xrb(1,5)=xx1(4)
'         xrb(2,5)=xx2(4)
'         xrb(3,5)=xx3(4)
'       else
'
'c        ... update rhs ...
'         do 100 kk=1,15
'         jj=lmn(kk)
'         if (jj.eq.0) go to 100
'         rhs(jj)=rhs(jj)+r(kk)
'  100    continue
'       endif
'c      endif                    ! end .not. udch branch
'
'      if(lifd) goto 140    ! skip stiffness for interface force death
'c
'c   ... zero history? ...
'  102 if (laugon) then
'         if (lsh) goto 105
'      else
'         if (ans.le.0..or.fcoeff.eq.0.) go to 105
'      endif
'c      if (.not.udch) then
'      iseg(2,ii)=0
'      fdat(8,ii)=0.
'      fdat(9,ii)=0.
'      fdat(10,ii)=0.
'c      endif
'c
'c   ... add stiffness? ...
'  105 if (laugon) then
'         if (lnk) goto 140
'      else
'         if (ans.gt.dx) goto 140
'      endif
'c
'c   ... uncomment following lines when including brick bsi nodes
'c       generalized forces using exkcbs.f
'
'      if (iphase-2) 110,110,140
'  110 if (iref) 140,120,140
'  120 if (fcoeff.eq.0.) call nstiff(effstf,n1,n2,n3,e)
'      if (fcoeff.gt.0.) call fstiff(n1,n2,n3,effstf,fxi,fyi,fzi,
'c     1    fni,fmax,fmag,e,alpha*stft)
'     1    fni,fmax,fmag,e,stft)
'      call stfinf(n1,n2,n3,h,e,s)
'      lrbk=.true.                   ! used as lstif (i.e. reform) in exkcrb
'      if (lrbr) goto 140
'c
'
'         lmn(1) = i                                                     
'         lmn(2) = ix(1)                                                 
'         lmn(3) = ix(2)                                                 
'         lmn(4) = ix(3)                                                 
'         lmn(5) = ix(4)                                                 
'         call bdg53(diag,s,lmn)                                         b_n_m
'         call blkcpy(s,sfdata(kfstor),ksizsf)                           
'         call blkcpy(lmn,sfdata(lfstor),neesf)                          
'         kfstor = kfstor + ksizsf                                       
'         lfstor = lfstor + neesf                                        
'         iebuf = iebuf + 1                                              
'      numelf=numelf+1
' 140  continue
'
'c
'c   ... uncomment following lines when including brick bsi nodes
'c       generalized forces using exkcbs.f
'c
'c c    ... bsidofc returns lmmax = 0 if no rigid body nodes and sets bsi  pngv
'c c    ... flag idbsf for exkcbsi also lrbr set = false in exkcbsi        pngv
'
'1000  continue
'2000   format(4(i10,a2))
'      return
'      end

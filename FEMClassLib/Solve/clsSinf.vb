'This file contains all the methods of sinf.f
Partial Public Class clsSolve

    
    'Public nsnode, ksizsf As Integer
    'Public nrttlm, nrttls As Integer

    'Private nrtm, nrts, nmn, nsn, nty, nst, mst As Integer



    ''' <summary>
    ''' to drive slidesurface routines
    ''' </summary>
    ''' <param name="u"></param>
    ''' <param name="rhs"></param>
    ''' <param name="x"></param>
    ''' <param name="fval"></param>
    ''' <param name="idp"></param>
    ''' <param name="ndm"></param>
    ''' <param name="ndm2"></param>
    ''' <param name="iparm"></param>
    ''' <param name="irects"></param>
    ''' <param name="irectm"></param>
    ''' <param name="nsv"></param>
    ''' <param name="msr"></param>
    ''' <param name="nsegs"></param>
    ''' <param name="nsegm"></param>
    ''' <param name="lnsv"></param>
    ''' <param name="lmsr"></param>
    ''' <param name="ilocs"></param>
    ''' <param name="ilocm"></param>
    ''' <param name="stfs"></param>
    ''' <param name="stfm"></param>
    ''' <param name="irtls"></param>
    ''' <param name="irtlm"></param>
    ''' <param name="fric"></param>
    ''' <param name="iseg"></param>
    ''' <param name="fdat"></param>
    ''' <param name="abdg"></param>
    ''' <param name="sfdata"></param>
    ''' <param name="sftemp"></param>
    ''' <param name="xls"></param>
    ''' <param name="xlm"></param>
    ''' <param name="xgn"></param>
    ''' <param name="sfact"></param>
    ''' <param name="iaug"></param>
    ''' <param name="pend"></param>
    ''' <param name="tdeath"></param>
    ''' <param name="tbury"></param>
    ''' <param name="ifd"></param>
    ''' <param name="r_old"></param>
    ''' <param name="ss_old"></param>
    ''' <param name="l_old"></param>
    Public Sub sinf(ByRef u() As Double, ByRef rhs() As Double, ByRef x(,) As Double, ByRef fval() As Double,
                    ByRef idp(,) As Integer, ByRef ndm As Integer, ByRef ndm2 As Integer, ByRef iparm(,) As Integer,
                    ByRef irects(,) As Integer, ByRef irectm(,) As Integer, ByRef nsv() As Integer, ByRef msr() As Integer,
                    ByRef nsegs() As Integer, ByRef nsegm() As Integer, ByRef lnsv() As Integer, ByRef lmsr() As Integer,
                    ByRef ilocs() As Integer, ByRef ilocm() As Integer, ByRef stfs() As Double, ByRef stfm() As Double,
                    ByRef irtls1() As Integer, ByRef irtlm1() As Integer, ByRef fric(,) As Double, ByRef iseg(,) As Integer,
                    ByRef fdat(,) As Double, ByRef abdg(,) As Double, ByRef sfdata1(,) As Double, ByRef sfdata2(,) As Integer, ByRef sftemp(,) As Double,
                    ByRef ientmp(,) As Integer, ByRef xls() As Double, ByRef xlm() As Double, ByRef xgn() As Double, ByRef sfact() As Double,
                    ByRef iaug() As Integer, ByRef pend() As Double, ByRef tdeath() As Double, ByRef tbury() As Double,
                    ByRef ifd() As Integer, ByRef r_old() As Double, ByRef ss_old() As Double, ByRef l_old() As Integer)

        Dim id(6), lmm(15) As Integer, su(309), stfs_k9(), stfm_k10() As Double    ' QW 12-12-2018-
        Dim lmsr_k8(), lnsv_k3(), nsv_k4(), msr_k5(), ilocs_k4(), ilocm_k5(), irtlm_k4(), irtls_k5() As Integer   ' QW 12-12-2018-
        Dim nsegs_k2(), nsegm_k7(), lold() As Integer             ' QW 12-12-2018-
        Dim iebuf, kfstor, lfstor, idn, ipass As Integer
        Dim fcoeff, adeath, timed As Double
        Dim laug, ldeath As Boolean

        Dim irectm_k6(4 * nrttlm), irectm_k6_2D(4, nrttlm) As Integer   ' YC 092018
        Dim irects_k1(4 * nrttls), irects_k1_2D(4, nrttls) As Integer

        Dim zero = 0.0
        Call bsinxbs0()
        Dim lstif = (iref = 0 And iphase < 3)
        If lstif Then
            numelf = 0
            iebuf = 0
        End If
        kfstor = 1 : lfstor = 1
        'lfstor = 1 + nsnode * ksizsf        ' QW 12-12-2018-

        Dim i As Integer    ' YC 102418
        'Call Check2D(irects, 4, nrttls)
        'For i = 0 To numnp - 1     ' YC 102418
        For i = 1 To numnp

            '       call unpkid(id,idp(1,i),1)
            'clsInput.unpkid(id, idp, 0, i, 1) 'YC 092018, YC102418
            Call unpkid(id, idp, 1, i, 1)

            'For j = 0 To 2     ' YC 102418
            For j = 1 To 3
                idn = id(j)
                If idn = 0 Then Continue For
                x(j, i) = x(j, i) + u(idn)
            Next
        Next

        '     sliding interface algorithm

        Dim k1 = 1, k2 = 1, k3 = 1, k4 = 1, k5 = 1, k6 = 1, k7 = 1, k8 = 1, k9 = 1, k10 = 1
        Dim k11 = 1, k12 = 0, k13 = 0, k14 = 1, k15 = 1, k16 = 1, k17 = 1

        Dim n As Integer ' YC 102418

        'For n = 0 To numsv - 1 ' YC 102418
        For n = 1 To numsv

            ' YC 102418
            'nrts = iparm(0, n)
            'nrtm = iparm(1, n)
            'nsn = iparm(2, n)
            'nmn = iparm(3, n)
            'nty = iparm(4, n)
            'nst = iparm(5, n)
            'mst = iparm(6, n)
            nrts = iparm(1, n)
            nrtm = iparm(2, n)
            nsn = iparm(3, n)
            nmn = iparm(4, n)
            nty = iparm(5, n)
            nst = iparm(6, n)
            mst = iparm(7, n)
            ' YC 102418 END

            'ipass = 1 * clsInput.sign(1, nty)   ' check for single pass  'YC 092018
            ipass = 1 * objComsub.sign(1, nty)   ' check for single pass

            nty = Math.Abs(nty)

            If nty = 8 Then nty = 3
            fcoeff = Math.Pow(fric(1, n), 2) + Math.Pow(fric(2, n), 2) + Math.Pow(fric(3, n), 2)
            laug = False
            If iaug(n) <> 0 Then laug = True

            'For i = 0 To 3 * nsn - 1   ' YC 102418
            For i = 1 To 3 * nsn
                xls(k12 + 3 * nsn + i) = xls(k12 + i)
            Next

            'For i = 0 To 3 * nmn - 1    ' YC 102418
            For i = 1 To 3 * nmn
                xlm(k13 + 3 * nmn + i) = xlm(k13 + i)
            Next

            ldeath = False
            adeath = 1.0
            If lelstf Then
                timed = timep
            Else
                timed = time
            End If

            If timed > tdeath(n) Then
                ldeath = True
                If tbury(n) <= tdeath(n) Then
                    adeath = 0.0
                Else
                    adeath = Math.Max(zero, (tbury(n) - timed) / (tbury(n) - tdeath(n)))
                End If
                If timep > tbury(n) Or (time > tbury(n) And Not lelstf) Then GoTo 56
            End If

            '  ...check for rigid body contact

            If nty = 4 Then GoTo 45
            If nty <> 2 Then GoTo 40

            '  ... tied slide surface processed

            GoTo 50
40:

            '  ... type 3 and 1 slide surface processed

            '    ... find master node and facet for each slave node

            If lstif Or ldeath Then GoTo 42

            For i = 1 To 4                          ' QW 12-12-2018-
                For J = 1 To nrttlm - k6 + 1
                    irectm_k6_2D(i, J) = irectm(i, k6 + J - 1)
                Next
            Next
            For i = 1 To 4                          ' QW 12-12-2018-
                For J = 1 To nrttls - k1 + 1
                    irects_k1_2D(i, J) = irects(i, k1 + J - 1)
                Next
            Next
            ReDim lmsr_k8(lmsr.Length - k8), nsv_k4(nsv.Length - k4), msr_k5(msr.Length - k5), ilocs_k4(ilocs.Length - k4), irtlm_k4(irtlm1.Length - k4)
            ReDim nsegm_k7(nsegm.Length - k7)
            Array.Copy(nsegm, k7, nsegm_k7, 1, nsegm.Length - k7)
            Array.Copy(lmsr, k8, lmsr_k8, 1, lmsr.Length - k8)
            Array.Copy(nsv, k4, nsv_k4, 1, nsv.Length - k4)
            Array.Copy(msr, k5, msr_k5, 1, msr.Length - k5)
            Array.Copy(ilocs, k4, ilocs_k4, 1, ilocs.Length - k4)
            Array.Copy(irtlm1, k4, irtlm_k4, 1, irtlm1.Length - k4)
            Call slave1(x, irectm_k6_2D, lmsr_k8, msr_k5, nsv_k4, ilocs_k4, irtlm_k4, nsegm_k7, nsn, nmn)
            Array.Copy(nsegm_k7, 1, nsegm, k7, nsegm.Length - k7)
            Array.Copy(lmsr_k8, 1, lmsr, k8, lmsr.Length - k8)
            Array.Copy(nsv_k4, 1, nsv, k4, nsv.Length - k4)
            Array.Copy(msr_k5, 1, msr, k5, msr.Length - k5)
            Array.Copy(ilocs_k4, 1, ilocs, k4, ilocs.Length - k4)
            Array.Copy(irtlm_k4, 1, irtlm1, k4, irtlm1.Length - k4)

            If ipass = 1 Then  'check for single pass
                ReDim lnsv_k3(lnsv.Length - k3), nsegs_k2(nsegs.Length - k2), ilocm_k5(ilocm.Length - k5), irtls_k5(irtls1.Length - k5)
                Array.Copy(nsegs, k2, nsegs_k2, 1, nsegs.Length - k2)
                Array.Copy(lnsv, k3, lnsv_k3, 1, lnsv.Length - k3)
                Array.Copy(nsv, k4, nsv_k4, 1, nsv.Length - k4)
                Array.Copy(msr, k5, msr_k5, 1, msr.Length - k5)
                Array.Copy(ilocm, k5, ilocm_k5, 1, ilocm.Length - k5)
                Array.Copy(irtls1, k5, irtls_k5, 1, irtls1.Length - k5)
                Call slave1(x, irects_k1_2D, lnsv_k3, nsv_k4, msr_k5, ilocm_k5, irtls_k5, nsegs_k2, nmn, nsn)
                Array.Copy(nsegs_k2, 1, nsegs, k2, nsegs.Length - k2)
                Array.Copy(lnsv_k3, 1, lnsv, k3, lnsv.Length - k3)
                Array.Copy(nsv_k4, 1, nsv, k4, nsv.Length - k4)
                Array.Copy(msr_k5, 1, msr, k5, msr.Length - k5)
                Array.Copy(ilocm_k5, 1, ilocm, k5, ilocm.Length - k5)
                Array.Copy(irtls_k5, 1, irtls1, k5, irtls1.Length - k5)

            End If
            '
            'c  ... make slave node thickness current since ethik is ref. config thickness
            '
            'c    ... calculate gap and apply force for each slave-master pair
            'c 
42:
            ' YC 092018
            'Call slave2(x, rhs, irectm.Skip(k6).ToArray(), lmsr.Skip(k8).ToArray(), msr.Skip(k5).ToArray(),
            '            nsv.Skip(k4).ToArray(), ilocs.Skip(k4).ToArray(), irtlm.Skip(k4).ToArray(),
            '            stfm.Skip(k10).ToArray(), nsn, nmn, nty, idp, lmm, su,
            '            irects.Skip(k1).ToArray(), lnsv.Skip(k3).ToArray(), fric(1, n), fdat(1, k11), iseg(1, k11),
            '            abdg, sfdata, kfstor, lfstor, iebuf,
            '            xls.Skip(k12 + 3 * nsn + 1), xgn.Skip(4 * n - 3).ToArray(), xgn.Skip(4 * n - 2).ToArray(),
            '            sfact.Skip(n).ToArray(), pend.Skip(n).ToArray(), laug, numnp, fval, adeath,
            '            ifd.Skip(n).ToArray(), r_old.Skip(k15).ToArray(), ss_old.Skip(k16).ToArray(),
            '            l_old.Skip(k17).ToArray())

            'Dim irectm_k6(4 * nrttlm - 1), irectm_k6_2D(4 - 1, nrttlm - 1) As Integer
            'Call objComsub.ArrayExtract1Dfrom1D(irectm, k6 - 1, irectm_k6, 4 * nrttlm)
            'Call objComsub.ArrayConvert1Dto2D(irectm_k6, irectm_k6_2D, 4, nrttlm)

            'Dim irects_k1(4 * nrttls - 1), irects_k1_2D(4 - 1, nrttls - 1) As Integer
            'Call objComsub.ArrayExtract1Dfrom1D(irects, k1 - 1, irects_k1, 4 * nrttls)
            'Call objComsub.ArrayConvert1Dto2D(irects_k1, irects_k1_2D, 4, nrttls)

            For i = 1 To 4                          ' QW 12-12-2018-
                For J = 1 To nrttlm - k6 + 1
                    irectm_k6_2D(i, J) = irectm(i, k6 + J - 1)
                Next
            Next
            For i = 1 To 4                          ' QW 12-12-2018-
                For J = 1 To nrttls - k1 + 1
                    irects_k1_2D(i, J) = irects(i, k1 + J - 1)
                Next
            Next

            ReDim lmsr_k8(lmsr.Length - k8), lnsv_k3(lnsv.Length - k3), nsv_k4(nsv.Length - k4), msr_k5(msr.Length - k5), ilocs_k4(ilocs.Length - k4), irtlm_k4(irtlm1.Length - k4)
            ReDim stfm_k10(stfm.Length - k10), lold(l_old.Length - k17)
            Array.Copy(stfm, k10, stfm_k10, 1, stfm.Length - k10)
            Array.Copy(lmsr, k8, lmsr_k8, 1, lmsr.Length - k8)
            Array.Copy(lnsv, k3, lnsv_k3, 1, lnsv.Length - k3)
            Array.Copy(nsv, k4, nsv_k4, 1, nsv.Length - k4)
            Array.Copy(msr, k5, msr_k5, 1, msr.Length - k5)
            Array.Copy(ilocs, k4, ilocs_k4, 1, ilocs.Length - k4)
            Array.Copy(irtlm1, k4, irtlm_k4, 1, irtlm1.Length - k4)
            Array.Copy(l_old, k17, lold, 1, l_old.Length - k17)

            Dim fric_n(3) As Double
            Call objComsub.ArrayExtract1Dfrom2D(fric, 1 - 1, n - 1, fric_n, 3)

            Dim fdat_k11(10, nsn + nmn) As Double
            Call objComsub.ArrayExtract2Dfrom2D(fdat, 1 - 1, k11 - 1, fdat_k11, 10, nsn + nmn)

            Dim iseg_k11(2, nsn + nmn) As Integer
            Call objComsub.ArrayExtract2Dfrom2D(iseg, 1 - 1, k11 - 1, iseg_k11, 2, nsn + nmn)

            Dim xls_k12_3nsn_1(3 * nsntl), xls_k12_3nsn_1_2D(3, nsntl) As Double
            Call objComsub.ArrayExtract1Dfrom1D(xls, k12 + 3 * nsn + 1 - 1, xls_k12_3nsn_1, 3 * nsntl)
            Call objComsub.ArrayConvert1Dto2D(xls_k12_3nsn_1, xls_k12_3nsn_1_2D, 3, nsntl)

            Dim r_old_k15(3 * nsn), r_old_k15_2D(3, nsn) As Double
            Call objComsub.ArrayExtract1Dfrom1D(r_old, k15 - 1, r_old_k15, 3 * nsn)
            Call objComsub.ArrayConvert1Dto2D(r_old_k15, r_old_k15_2D, 3, nsn)

            Dim ss_old_k16(2 * nsn), ss_old_k16_2D(2, nsn) As Double
            Call objComsub.ArrayExtract1Dfrom1D(ss_old, k16 - 1, ss_old_k16, 2 * nsn)
            Call objComsub.ArrayConvert1Dto2D(ss_old_k16, ss_old_k16_2D, 2, nsn)

            'Call slave2(x, rhs, irectm_k6_2D, lmsr.Skip(k8 - 1).ToArray(), msr.Skip(k5 - 1).ToArray(),
            '           nsv.Skip(k4 - 1).ToArray(), ilocs.Skip(k4 - 1).ToArray(), irtlm.Skip(k4 - 1).ToArray(),
            '           stfm.Skip(k10 - 1).ToArray(), nsn, nmn, nty, idp, lmm, su,
            '           irects_k1_2D, lnsv.Skip(k3 - 1).ToArray(), fric_n, fdat, iseg,
            '           abdg, sfdata, kfstor, lfstor, iebuf,
            '           xls_k12_3nsn_1_2D, xgn(4 * (n + 1) - 3 - 1), xgn(4 * (n + 1) - 2 - 1),
            '           sfact(n), pend(n), laug, numnp, fval, adeath,
            '           ifd(n), r_old_k15_2D, ss_old_k16_2D, l_old.Skip(k17 - 1).ToArray())
            ' YC 092018 END
            ' QW 12-12-2018-
            Call slave2(x, rhs, irectm_k6_2D, lmsr_k8, msr_k5, nsv_k4, ilocs_k4, irtlm_k4, stfm_k10,
                        nsn, nmn, nty, idp, lmm, su, lnsv_k3, fric_n, fdat_k11, iseg_k11,
                       abdg, sfdata1, sfdata2, kfstor, lfstor, iebuf, xls_k12_3nsn_1_2D, xgn(4 * n - 3), xgn(4 * n - 2),
                       sfact(n), pend(n), laug, numnp, fval, adeath, ifd(n), r_old_k15_2D, ss_old_k16_2D, lold)
            Array.Copy(stfm_k10, 1, stfm, k10, stfm.Length - k10)
            Array.Copy(lmsr_k8, 1, lmsr, k8, lmsr.Length - k8)
            Array.Copy(lnsv_k3, 1, lnsv, k3, lnsv.Length - k3)
            Array.Copy(nsv_k4, 1, nsv, k4, nsv.Length - k4)
            Array.Copy(msr_k5, 1, msr, k5, msr.Length - k5)
            Array.Copy(ilocs_k4, 1, ilocs, k4, ilocs.Length - k4)
            Array.Copy(irtlm_k4, 1, irtlm1, k4, irtlm1.Length - k4)
            Array.Copy(lold, 1, l_old, k17, l_old.Length - k17)

            Call objComsub.ArrayInsert2Dto2D(fdat_k11, 10, nsn + nmn, fdat, 1 - 1, k11 - 1)
            Call objComsub.ArrayInsert2Dto2D(iseg_k11, 2, nsn + nmn, iseg, 1 - 1, k11 - 1)

            'Call Check2D(irects, 4, nrttls)
            If fcoeff <> 0 Then k11 = k11 + nsn
            If nty = 3 And ipass = 1 Then 'check for single pass

                ' YC 092018
                'Call slave2(x, rhs, irects.Skip(k1).ToArray(), lnsv.Skip(k3).ToArray(), nsv.Skip(k4).ToArray(),
                '            msr.Skip(k5).ToArray(), ilocm.Skip(k5).ToArray(), irtls.Skip(k5).ToArray(),
                '            stfs.Skip(k9).ToArray(), nmn, nsn, nty, idp, lmm, su,
                '            irectm.Skip(k6).ToArray(), lmsr.Skip(k8).ToArray(), fric(1, n), fdat(1, k11), iseg(1, k11),
                '            abdg, sfdata, kfstor, lfstor, iebuf,
                '            xlm.Skip(k13 + 3 * nmn + 1).ToArray(), xgn.Skip(4 * n - 1).ToArray(), xgn.Skip(4 * n).ToArray(),
                '            sfact.Skip(n).ToArray(), pend.Skip(n).ToArray(), laug, numnp, fval, adeath,
                '            ifd.Skip(n).ToArray(), r_old.Skip(k15 + 3 * nsn).ToArray(), ss_old.Skip(k16 + 2 * nsn).ToArray(),
                '            l_old.Skip(k17 + nsn).ToArray())

                ReDim ilocm_k5(ilocm.Length - k5), irtls_k5(irtls1.Length - k5), stfs_k9(stfs.Length - k9), lold(l_old.Length - k17 - nsn)
                Array.Copy(stfs, k9, stfs_k9, 1, stfs.Length - k9)
                Array.Copy(ilocm, k5, ilocm_k5, 1, ilocm.Length - k5)
                Array.Copy(irtls1, k5, irtls_k5, 1, irtls1.Length - k5)
                Array.Copy(l_old, k17 + nsn, lold, 1, l_old.Length - k17 - nsn)

                Call objComsub.ArrayExtract1Dfrom2D(fric, 1 - 1, n - 1, fric_n, 3)
                Call objComsub.ArrayExtract2Dfrom2D(fdat, 1 - 1, k11 - 1, fdat_k11, 10, nsn + nmn)
                Call objComsub.ArrayExtract2Dfrom2D(iseg, 1 - 1, k11 - 1, iseg_k11, 2, nsn + nmn)

                Dim xlm_k13_3nmn_1(3 * nmntl), xlm_k13_3nmn_1_2D(3, nmntl) As Double
                Call objComsub.ArrayExtract1Dfrom1D(xlm, k13 + 3 * nmn + 1 - 1, xlm_k13_3nmn_1, 3 * nmntl)
                Call objComsub.ArrayConvert1Dto2D(xlm_k13_3nmn_1, xlm_k13_3nmn_1_2D, 3, nmntl)

                Dim r_old_k15_3nsn(3 * nmn), r_old_k15_3nsn_2D(3, nmn) As Double
                Call objComsub.ArrayExtract1Dfrom1D(r_old, k15 + 3 * nsn - 1, r_old_k15_3nsn, 3 * nmn)
                Call objComsub.ArrayConvert1Dto2D(r_old_k15_3nsn, r_old_k15_3nsn_2D, 3, nmn)

                Dim ss_old_k16_2nsn(2 * nmn), ss_old_k16_2nsn_2D(2, nmn) As Double
                Call objComsub.ArrayExtract1Dfrom1D(ss_old, k16 + 2 * nsn - 1, ss_old_k16_2nsn, 2 * nmn)
                Call objComsub.ArrayConvert1Dto2D(ss_old_k16_2nsn, ss_old_k16_2nsn_2D, 2, nmn)

                'Call slave2(x, rhs, irects_k1_2D, lnsv.Skip(k3 - 1).ToArray(), nsv.Skip(k4 - 1).ToArray(),
                '        msr.Skip(k5 - 1).ToArray(), ilocm.Skip(k5 - 1).ToArray(), irtls.Skip(k5 - 1).ToArray(),
                '       stfs.Skip(k9 - 1).ToArray(), nmn, nsn, nty, idp, lmm, su,
                '       irectm_k6_2D, lmsr.Skip(k8 - 1).ToArray(), fric_n, fdat, iseg,
                '       abdg, sfdata, kfstor, lfstor, iebuf,
                '       xlm_k13_3nmn_1_2D, xgn(4 * n - 3), xgn(4 * n - 2),
                '       sfact(n), pend(n), laug, numnp, fval, adeath,
                '       ifd(n), r_old_k15_3nsn_2D, ss_old_k16_2nsn_2D, l_old.Skip(k17 + nsn - 1).ToArray())
                ' YC 092018 END
                Call slave2(x, rhs, irects_k1_2D, lnsv_k3, nsv_k4, msr_k5, ilocm_k5, irtls_k5, stfs_k9,
                            nmn, nsn, nty, idp, lmm, su, lmsr_k8, fric_n, fdat_k11, iseg_k11,
                            abdg, sfdata1, sfdata2, kfstor, lfstor, iebuf, xlm_k13_3nmn_1_2D, xgn(4 * n - 3), xgn(4 * n - 2),
                            sfact(n), pend(n), laug, numnp, fval, adeath, ifd(n), r_old_k15_3nsn_2D, ss_old_k16_2nsn_2D, lold)

                'Array.Copy(stfs_k9, 1, stfs, k9, stfm.Length - k9)  'v3.0 002/YC 052620-2
                Array.Copy(stfs_k9, 1, stfs, k9, stfs.Length - k9)


                Array.Copy(ilocm_k5, 1, ilocm, k5, ilocm.Length - k5)
                Array.Copy(irtls_k5, 1, irtls1, k5, irtls1.Length - k5)
                Array.Copy(lold, 1, l_old, k17 + nsn, l_old.Length - k17 - nsn)

                Call objComsub.ArrayInsert2Dto2D(fdat_k11, 10, nsn + nmn, fdat, 1 - 1, k11 - 1)
                Call objComsub.ArrayInsert2Dto2D(iseg_k11, 2, nsn + nmn, iseg, 1 - 1, k11 - 1)

            End If
            If fcoeff <> 0 Then k11 = k11 + nmn

            '    ... update slave node thicknesses (after force calc from elstf)
            If lelstf Then
                '        if (bNike3dMsg) write(10,*) 'ik02 call thickup 3 in sub sinf'
                If nmn <> 0 Then
                    '        if (bNike3dMsg) write(10,*) 'ik02 call thickup 4 in sub sinf'	  
                End If
            End If
            GoTo 50

45:
            'write(10,*) 'ik02 call sscntc in sub sinf'
            If fcoeff <> 0 Then k11 = k11 + nsn
50:

            If adeath < 1.0 Then

                'For i = 0 To 3 * nsn - 1       ' YC 102418
                For i = 1 To 3 * nsn
                    xls(k12 + 3 * nsn + i) = xls(k12 + i) * adeath
                Next

                'For i = 0 To 3 * nmn - 1       ' YC 102418
                For i = 1 To 3 * nmn
                    xlm(k13 + 3 * nmn + i) = xlm(k13 + i) * adeath
                Next
            End If

56:         k1 = k1 + nrts
            k2 = k2 + 1 + nsn
            k3 = k3 + nst
            k4 = k4 + nsn
            k5 = k5 + nmn
            k6 = k6 + nrtm
            k7 = k7 + 1 + nmn
            k8 = k8 + mst
            k9 = k9 + nrts
            k10 = k10 + nrtm
            k12 = k12 + 6 * nsn
            k13 = k13 + 6 * nmn
            k14 = k14 + nsn + nmn
            k15 = k15 + 3 * (nsn + nmn) * ifd(n)
            k16 = k16 + 2 * (nsn + nmn) * ifd(n)
            k17 = k17 + 1 * (nsn + nmn) * ifd(n)

            'Call Check4(k12, nsn, nmn, numelf)
        Next

        If iref = 0 And iphase < 3 And numelf > 0 Then


            ' Define sfdata as two parts: sfdata1 -- double; sfdata2 -- integer.          ' QW 12-12-2018-
            ' Dim s2(ksizsf, numelf), sftemp2(ksizsf, numelf) As Double
            'Dim ien2(neesf, numelf), ientmp2(neesf, numelf) As Integer

            'Call objComsub.ArrayConvert1Dto2D(sfdata1, s2, ksizsf, numelf)
            'Call objComsub.ArrayConvert1Dto2D(sftemp, sftemp2, ksizsf, numelf)
            'Call objComsub.ArrayConvert1Dto2D(sfdata2, ien2, neesf, numelf)
            'Call objComsub.ArrayConvert1Dto2D(ientmp, ientmp2, neesf, numelf)

            'Call ssblck(sfdata(1), sfdata(nsnode * ksizsf + 1), sftemp(1), sftemp(nsnode * ksizsf + 1), numelf)
            ' Call ssblck(s2, ien2, sftemp2, ientmp2, numelf)
            Call ssblck(sfdata1, sfdata2, sftemp, ientmp, numelf)
            'Call objComsub.ArrayConvert2Dto1D(s2, ksizsf, numelf, sfdata1)
            'Call objComsub.ArrayConvert2Dto1D(sftemp2, ksizsf, numelf, sftemp)
            'Call objComsub.ArrayConvert2Dto1D(ien2, neesf, numelf, sfdata2)
            'Call objComsub.ArrayConvert2Dto1D(ientmp2, neesf, numelf, ientmp)
            'Call Check2DT(s2, ksizsf, 10)

        End If

        '   ... assemble brick shell int. stiffness in auxillary storage ..

    End Sub


End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine sinf(u,rhs,x,fval,idp,ndm,ndm2,
'     & iparm,irects,irectm,nsv,msr,nsegs,nsegm,lnsv,lmsr,ilocs,
'     & ilocm,stfs,stfm,irtls,irtlm,fric,iseg,fdat,
'     & diag,sfdata,sftemp,xls,xlm,xgn,sfact,iaug,  
'     & pend,tdeath,tbury,
'     & ifd,r_old,ss_old,l_old)
'c      
'      implicit double precision (a-h,o-z)                             
'c
'c===> module to drive slidesurface routines
'c
'      common/double/iprec
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e,
'     & n20f,n20g
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk20/kpri,nstep,ite,ilimit,iref
'      common/bk29/tim(7),iterp1,iterp2,time,timep,lpri,npri
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      logical udch
'      common/bk37/numsv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
'c      
'      common/fissl1/melemt,nnns,n2g,llls
'      common/elcnts/numelf
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye3/neehx,ksizhx,neesf,ksizsf
'      common/ebye5/mbsize,nhxblk,ihxblk(750),nsnode,nsfblk,isfblk(750)
'      common/block02/bNIKE3D, bNike3dMsg
'	logical bNIKE3D, bNike3dMsg
'      common/elstfb/lelstf
'c      
'      logical lelstf,laug,lstif,ldeath
'      logical lfpass,llpass
'      common/ndinit/lfpass,llpass
'
'      dimension u(*),rhs(*),x(3,*),fval(*),idp(6,*),
'     1 iparm(7,*),irects(*),irectm(*),nsv(*),msr(*),
'     2nsegm(*),lnsv(*),lmsr(*),ilocs(*),ilocm(*),stfs(*),stfm(*),
'     3irtls(*),irtlm(*),nsegs(*),id(6),fric(3,*),iseg(2,*),
'     4fdat(10,*),
'     5 diag(*),sfdata(*),sftemp(*),lmm(15),su(309),
'     &xls(*),xlm(*),xgn(*),sfact(*),iaug(*),pend(*),tdeath(*),tbury(*),
'     &ifd(*),r_old(*),ss_old(*),l_old(*)
'c
'      
'      zero = 0.0
'      call bsinxbs0          
'      lstif = (iref.eq.0 .and. iphase.lt.3)
'      if (lstif) then                                                   
'         numelf=0                   
'         iebuf = 0                                                     
'      endif                                                            
'      kfstor = 1                                                       
'         lfstor = 1 + nsnode*ksizsf                                    
'                                                           
'       do 10 i=1,numnp
'       call unpkid(id,idp(1,i),1)
'       do 10 j=1,3
'       idn=id(j)
'       if(idn.eq.0) go to 10
'       x(j,i)=x(j,i)+u(idn)
'   10  continue
'
'c     sliding interface algorithm
'c
'      k1=1
'      k2=1
'      k3=1
'      k4=1
'      k5=1
'      k6=1
'      k7=1
'      k8=1
'      k9=1
'      k10=1
'      k11=1
'      k12=0
'      k13=0
'      k14=1
'      k15=1
'      k16=1
'      k17=1
'      do 60 n=1,numsv
'      nrts=iparm(1,n)
'      nrtm=iparm(2,n)
'      nsn =iparm(3,n)
'      nmn =iparm(4,n)
'      nty =iparm(5,n)
'      nst =iparm(6,n)
'      mst =iparm(7,n)
'
'      ipass=1*isign(1,nty)   ! check for single pass
'      nty=abs(nty)
'
'      if(nty.eq.8) then
'       nty = 3
'      endif
'      fcoeff=fric(1,n)**2+fric(2,n)**2+fric(3,n)**2
'      laug=.false.
'      if (iaug(n).ne.0) laug=.true.
'
'      do 34 i=1,3*nsn
'      xls(k12+3*nsn+i)=xls(k12+i)
'   34 continue
'      do 36 i=1,3*nmn
'      xlm(k13+3*nmn+i)=xlm(k13+i)
'   36 continue
'c
'      ldeath=.false.
'      adeath=1.0
'      if(lelstf) then
'       timed = timep
'      else
'       timed = time
'      endif
'
'      if (timed.gt.tdeath(n)) then
'        ldeath=.true.
'        if (tbury(n).le.tdeath(n)) then
'          adeath=0.0
'        else
'          adeath=max(zero,(tbury(n)-timed)/(tbury(n)-tdeath(n)))
'        endif
'        if (timep.gt.tbury(n) .or.
'     &      (time.gt.tbury(n) .and. .not.lelstf)) goto 56
'      endif
'   38 continue
'c
'c  ...check for rigid body contact
'
'      if (nty.eq.4) go to 45
'      if (nty.ne.2) go to 40
'
'c  ... tied slide surface processed
'c ----- 09-06-2016 QW
'c -----
'      go to 50
'   40 continue
'
'c  ... type 3 and 1 slide surface processed
'
'c    ... find master node and facet for each slave node
'
'      if ((lstif).or.ldeath) goto 42
'      call slave1(x,irectm(k6),lmsr(k8),msr(k5),nsv(k4),
'     1 ilocs(k4),irtlm(k4),nsegm(k7),nsn,nmn)
'      if(ipass.eq.1) then    ! check for single pass
'      call slave1(x,irects(k1),lnsv(k3),nsv(k4),msr(k5),
'     1 ilocm(k5),irtls(k5),nsegs(k2),nmn,nsn)
'      endif
'
'c  ... make slave node thickness current since ethik is ref. config thickness
'
'c    ... calculate gap and apply force for each slave-master pair
'c 
'   42 call slave2(x,rhs,irectm(k6),lmsr(k8),msr(k5),nsv(k4),
'     & ilocs(k4),irtlm(k4),stfm(k10),nsn,nmn,nty,idp,lmm,su,
'     & irects(k1),lnsv(k3),fric(1,n),fdat(1,k11),iseg(1,k11),
'     & diag,sfdata,kfstor,lfstor,iebuf,
'     & xls(k12+3*nsn+1),xgn(4*n-3),xgn(4*n-2),sfact(n),pend(n),laug,
'     & numnp,fval,adeath,
'     & ifd(n),r_old(k15),ss_old(k16),l_old(k17))
'      if (fcoeff.ne.0.) k11=k11+nsn
'      if(nty.eq.3.and.ipass.eq.1)    ! check for single pass
'     &call slave2(x,rhs,irects(k1),lnsv(k3),nsv(k4),msr(k5),
'     & ilocm(k5),irtls(k5),stfs(k9),nmn,nsn,nty,idp,lmm,su,
'     & irectm(k6),lmsr(k8),fric(1,n),fdat(1,k11),iseg(1,k11),
'     & diag,sfdata,kfstor,lfstor,iebuf,
'     & xlm(k13+3*nmn+1),xgn(4*n-1),xgn(4*n),sfact(n),pend(n),laug,
'     & numnp,fval,adeath,
'     & ifd(n),r_old(k15+3*nsn),ss_old(k16+2*nsn),l_old(k17+nsn))
'
'      if (fcoeff.ne.0) k11=k11+nmn
'c    ... update slave node thicknesses (after force calc from elstf)
'       if (lelstf) then
'        if (bNike3dMsg) write(10,*) 'ik02 call thickup 3 in sub sinf'
'       if(nmn.ne.0) then
'        if (bNike3dMsg) write(10,*) 'ik02 call thickup 4 in sub sinf'	  
'	  end if
'      endif
'      go to 50
'
'   45 write(10,*) 'ik02 call sscntc in sub sinf'
'      if (fcoeff.ne.0.) k11=k11+nsn
'   50 continue
'c
'      if (adeath.lt.1.0) then
'        do 52 i=1,3*nsn
'        xls(k12+3*nsn+i)=xls(k12+i)*adeath
'   52   continue
'        do 54 i=1,3*nmn
'        xlm(k13+3*nmn+i)=xlm(k13+i)*adeath
'   54   continue
'      endif
'c
'   56 k1=k1+4*nrts
'      k2=k2+1+nsn
'      k3=k3+nst
'      k4=k4+nsn
'      k5=k5+nmn
'      k6=k6+4*nrtm
'      k7=k7+1+nmn
'      k8=k8+mst
'      k9=k9+nrts
'      k10=k10+nrtm
'      k12=k12+6*nsn
'      k13=k13+6*nmn
'      k14=k14+nsn+nmn
'      k15=k15+3*(nsn+nmn)*ifd(n)
'      k16=k16+2*(nsn+nmn)*ifd(n)
'      k17=k17+1*(nsn+nmn)*ifd(n)
'c
'   60 continue
'c
'      if (iref.eq.0 .and. iphase.lt.3 .and. numelf.gt.0) then 
'          
'          call ssblck(sfdata(1),sfdata(nsnode*ksizsf+1),              
'     &               sftemp(1),sftemp(nsnode*ksizsf+1),numelf)
'      endif                                                            
'
'c   ... assemble brick shell int. stiffness in auxillary storage ..
'         
'200   format(4(i10,a2))         
'840   format(e15.5,3(a2,e15.5),a2,i10)  
'850   format(i10,2(a2,i10),3(a2,e15.5))         
'c
'      return
'      end

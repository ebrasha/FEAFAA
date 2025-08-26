'This file contains all the methods of s4main.f
Partial Public Class clsSolve

    ''' <summary>
    ''' for stress point integration of J-2 thermo-plasticity
    ''' </summary>
    ''' <param name="prop"></param>
    ''' <param name="nodes"></param>
    ''' <param name="sig"></param>
    ''' <param name="tmpo"></param>
    ''' <param name="epx"></param>
    ''' <param name="told"></param>
    ''' <param name="tnew"></param>
    ''' <param name="tref"></param>
    ''' <param name="ln"></param>
    ''' 

    'Public Sub s4main(prop(,) As Double, nodes(,) As Double, sig(,) As Double,
    '                  tmpo(,) As Double, epx(,) As Double, told() As Double,
    '                  tnew() As Double, tref(,) As Double, ln As Integer)  ' YC 121219
    Public Sub s4main(prop(,) As Double, nodes(,) As Integer, sig(,) As Double,
                      tmpo(,) As Double, epx(,) As Double, told() As Double,
                      tnew() As Double, tref(,) As Double, ln As Integer)

        'Dim p(5), da(5), thrmo(4), thrmn(4), d(5, 5), dt, sclbk, davge As Double   ' YC 121219
        Dim p(6), da(6), thrmo(5), thrmn(5), d(6, 6), dt, sclbk, davge As Double

        Dim a, b, q11, scale0, scale1, pl As Double
        Dim t1l, t2l, t3l, t4l, t5l, t6l, aj2l, ak2l, sclel, fac1, aj1l, depsl As Double
        Dim av, r1, r2, r3, r4, r5, r6, w1, w2, w3, w4, w5, w6, w11, w1w11, w2w11, w3w11, w4w11, w5w11 As Double
        Dim d11, d12, d13, d14, d15, d16, d22, d23, d24, d25, d26, d33, d34, d35, d36, d44, d45, d46, d55, d56, d66 As Double


        Dim i, k As Integer ' YC 121219
        Dim toldg(64), tnewg(64), deltem(64), midtem(64), ctmp(64), otmp(64), ym_1(64), pv(64), qs(64), qh(64), alpha(64), dymt(64), dpvt(64), dqst(64), dhmt(64), dalt(64), ak(64), ax(64), phi(64), alphan(64), daltn(64), deltmn(64), midtmn(64) As Double   'common/vect16/
        Dim s1(64), s2(64), s3(64), s4(64), s5(64), s6(64), s7(64), teps(64), c11i(64), c12i(64), d11t(64), d12t(64), d44t(64), h1(64), h2(64), h3(64), h4(64), h5(64), h6(64), depi(64), tepsn(64), sigtrm(64) As Double  'common/vect14/
        Dim g1(64), g2(64), g3(64), g4(64), g5(64), g6(64), pr_1(64), aj2(64), sj2(64), scale_5(64), q1(64), q2(64), q3(64) As Double    'common/vect5/
        Dim c11, c21, c31, c12, c22, c32, c13, c23, c33, c44, c55, c66 As Double  'common/bk56/
        Dim h = h8 'common/bk32/ YC?
        Dim c_bk56(6, 6) As Double  'common/bk56/ YC?


        Dim third = -1.0 / 3.0
        Dim half = 0.5
        Dim one = 1.0

        'Call objSolve.rotat1(sig, ln)  ' YC 121219
        Call rotat1(sig, ln)

        Dim ielas = 0
        If (prop(1, 5) + prop(2, 5)) = 0.0 Then ielas = 1
        If ipt > 1 And incflg = 0 Then GoTo 180

        If itemp < 1 Or itread <> 0 Then GoTo 20
        For i = mft To mlt
            toldg(i) = told(1)
            tnewg(i) = tnew(1)
        Next
        GoTo 50

20:     For i = mft To mlt
            toldg(i) = 0.0
            tnewg(i) = 0.0
            If incflg = 0 Then                                                      'incmode
                'c....... element-averaged temperature for b-bar
                For k = 1 To 8
                    toldg(i) = toldg(i) + 0.125 * told(nodes(k, i))
                    tnewg(i) = tnewg(i) + 0.125 * tnew(nodes(k, i))
                Next
            Else                                                                    'incmode
                'c....... shape ftn interpolation of temperature                         incmode
                For k = 1 To 8                                                      'incmode
                    toldg(i) = toldg(i) + h(k, ipt) * told(nodes(k, i))             'incmode
                    tnewg(i) = tnewg(i) + h(k, ipt) * tnew(nodes(k, i))              'incmode
                Next
            End If                                                                  'incmode
        Next

50:     If iphase <> 3 Then GoTo 80
60:     For i = mft To mlt
            ctmp(i) = tnewg(i)
            otmp(i) = tmpo(1, i)
            deltem(i) = ctmp(i) - otmp(i)
            midtem(i) = 0.5 * (ctmp(i) + otmp(i))
        Next
        GoTo 100
80:     If iteref <> 0 Then GoTo 60
        For i = mft To mlt
            ctmp(i) = toldg(i)
            otmp(i) = tmpo(1, i)
            deltem(i) = ctmp(i) - otmp(i)
            midtem(i) = 0.5 * (ctmp(i) + otmp(i))
        Next

100:    If prop(1, 1) <> 0.0 Or prop(2, 1) <> 0.0 Then GoTo 120
        For i = mft To mlt
            ym_1(i) = prop(1, 2)
            pv(i) = prop(1, 3)
            qs(i) = prop(1, 5)
            qh(i) = prop(1, 6)
            alpha(i) = prop(1, 4)
            dymt(i) = 0.0
            dpvt(i) = 0.0
            dqst(i) = 0.0
            dhmt(i) = 0.0
            dalt(i) = 0.0
        Next
        GoTo 150

120:    Dim iul = mlt

        'c ... if itemp gt 0 all nodes have same temp unless itread ne 0
        'c ... then nodal temps prescribed in file

        If itemp > 0 And itread = 0 Then iul = mft
        For i = mft To iul
            'TODO - pass proper arrays

            ' YC 121219
            'Call intrpm(prop, prop(1, 2), otmp(i), prop(1, 2), thrmo)  
            'Call intrpm(prop, prop(1, 2), ctmp(i), prop(1, 2), thrmn)

            Dim prop_1_1_1D(8), prop_1_2_1D(8), prop_1_2(8, 5) As Double
            Call objComsub.ArrayExtract1Dfrom2D(prop, 1 - 1, 1 - 1, prop_1_1_1D, 8)
            Call objComsub.ArrayExtract1Dfrom2D(prop, 1 - 1, 2 - 1, prop_1_2_1D, 8)
            Call objComsub.ArrayExtract2Dfrom2D(prop, 1 - 1, 2 - 1, prop_1_2, 8, 5)

            Call intrpm(prop_1_1_1D, prop_1_2_1D, otmp(i), prop_1_2, thrmo)
            Call intrpm(prop_1_1_1D, prop_1_2_1D, ctmp(i), prop_1_2, thrmn)
            ' YC 121219 END

            dt = deltem(i)
            If Math.Abs(dt) < 1.0E-20 Then dt = 1.0
            ym_1(i) = (thrmo(1) + thrmn(1)) / 2.0
            pv(i) = (thrmo(2) + thrmn(2)) / 2.0
            qs(i) = (thrmo(4) + thrmn(4)) / 2.0
            qh(i) = (thrmo(5) + thrmn(5)) / 2.0
            alpha(i) = (thrmo(3) + thrmn(3)) / 2.0
            dymt(i) = (thrmn(1) - thrmo(1)) / dt
            dpvt(i) = (thrmn(2) - thrmo(2)) / dt
            dqst(i) = (thrmn(4) - thrmo(4)) / dt
            dhmt(i) = (thrmn(5) - thrmo(5)) / dt
            dalt(i) = thrmn(3) - thrmo(3)
        Next
        If itemp < 1 Or itread <> 0 Then GoTo 150
        For i = mft To mlt
            ym_1(i) = ym_1(mft)
            pv(i) = pv(mft)
            qs(i) = qs(mft)
            qh(i) = qh(mft)
            alpha(i) = alpha(mft)
            dymt(i) = dymt(mft)
            dpvt(i) = dpvt(mft)
            dqst(i) = dqst(mft)
            dhmt(i) = dhmt(mft)
            dalt(i) = dalt(mft)
        Next

150:    For i = mft To mlt
            s1(i) = (1.0 + pv(i)) * (1.0 - 2.0 * pv(i))
            s2(i) = (1.0 - pv(i)) / s1(i)
            s3(i) = ym_1(i) / (1.0 + pv(i))
            s4(i) = s3(i) / 2.0
            s5(i) = s3(i) / (1.0 - 2.0 * pv(i))
            s6(i) = s5(i) * pv(i)
            s7(i) = s5(i) - s6(i)
            teps(i) = alpha(i) * deltem(i) + dalt(i) * (midtem(i) - tref(1, i))
        Next

        For i = mft To mlt
            c11i(i) = 1.0 / ym_1(i)
            c12i(i) = -pv(i) / ym_1(i)
            d11t(i) = dymt(i) * s2(i) + dpvt(i) * ((s2(i) * (1.0 + 4.0 * pv(i)) * ym_1(i) - ym_1(i)) / s1(i))
            d12t(i) = dymt(i) * pv(i) / s1(i) + dpvt(i) * ((ym_1(i) + pv(i) * ym_1(i) * (1.0 + 4.0 * pv(i)) / s1(i)) / s1(i))
            d44t(i) = (dymt(i) - dpvt(i) * s3(i)) / (2.0 * (1.0 + pv(i)))
        Next

180:    For i = mft To mlt
            h1(i) = c11i(i) * sig(1, i) + c12i(i) * (sig(2, i) + sig(3, i))
            h2(i) = c11i(i) * sig(2, i) + c12i(i) * (sig(1, i) + sig(3, i))
            h3(i) = c11i(i) * sig(3, i) + c12i(i) * (sig(1, i) + sig(2, i))
            h4(i) = sig(4, i) / s4(i)
            h5(i) = sig(5, i) / s4(i)
            h6(i) = sig(6, i) / s4(i)
        Next

        For i = mft To mlt
            dsave(1, 1, i) = s7(i)
            dsave(1, 2, i) = s6(i)
            dsave(1, 3, i) = s6(i)
            dsave(1, 4, i) = 0.0
            dsave(1, 5, i) = 0.0
            dsave(1, 6, i) = 0.0
            dsave(2, 1, i) = s6(i)
            dsave(2, 2, i) = s7(i)
            dsave(2, 3, i) = s6(i)
            dsave(2, 4, i) = 0.0
            dsave(2, 5, i) = 0.0
            dsave(2, 6, i) = 0.0
            dsave(3, 1, i) = s6(i)
            dsave(3, 2, i) = s6(i)
            dsave(3, 3, i) = s7(i)
            dsave(3, 4, i) = 0.0
            dsave(3, 5, i) = 0.0
            dsave(3, 6, i) = 0.0
            dsave(4, 1, i) = 0.0
            dsave(4, 2, i) = 0.0
            dsave(4, 3, i) = 0.0
            dsave(4, 4, i) = s4(i)
            dsave(4, 5, i) = 0.0
            dsave(4, 6, i) = 0.0
            dsave(5, 1, i) = 0.0
            dsave(5, 2, i) = 0.0
            dsave(5, 3, i) = 0.0
            dsave(5, 4, i) = 0.0
            dsave(5, 5, i) = s4(i)
            dsave(5, 6, i) = 0.0
            dsave(6, 1, i) = 0.0
            dsave(6, 2, i) = 0.0
            dsave(6, 3, i) = 0.0
            dsave(6, 4, i) = 0.0
            dsave(6, 5, i) = 0.0
            dsave(6, 6, i) = s4(i)
        Next

        For i = mft To mlt
            g1(i) = d11t(i) * h1(i) + d12t(i) * (h2(i) + h3(i))
            g2(i) = d11t(i) * h2(i) + d12t(i) * (h1(i) + h3(i))
            g3(i) = d11t(i) * h3(i) + d12t(i) * (h2(i) + h1(i))
            g4(i) = d44t(i) * h4(i)
            g5(i) = d44t(i) * h5(i)
            g6(i) = d44t(i) * h6(i)
        Next

        For i = mft To mlt
            h1(i) = d1(i) - teps(i)
            h2(i) = d2(i) - teps(i)
            h3(i) = d3(i) - teps(i)
            h4(i) = d4(i)
            h5(i) = d5(i)
            h6(i) = d6(i)
        Next

        If ielas = 1 Then GoTo 260

        For i = mft To mlt
            ak(i) = qs(i) + qh(i) * epx(1, i)
            pr_1(i) = third * (sig(1, i) + sig(2, i) + sig(3, i))
            sig(1, i) = sig(1, i) + pr_1(i)
            sig(2, i) = sig(2, i) + pr_1(i)
            sig(3, i) = sig(3, i) + pr_1(i)
            aj2(i) = -sig(1, i) * sig(2, i) - sig(2, i) * sig(3, i) - sig(3, i) * sig(1, i) +
                Math.Pow(sig(4, i), 2) + Math.Pow(sig(5, i), 2) + Math.Pow(sig(6, i), 2)
        Next
        For i = mft To mlt
            sj2(i) = Math.Sqrt(3.0 * Math.Abs(aj2(i))) + 1.0E-20
            scale_5(i) = Math.Min(one, ak(i) / sj2(i))
        Next

        For i = mft To mlt
            epx(1, i) = epx(1, i) + (1.0 - scale_5(i)) * sj2(i) / (3.0 * s4(i) + qh(i))
            ak(i) = qs(i) + qh(i) * epx(1, i)
            ax(i) = 1.5 / (ak(i) * ak(i))
            sig(1, i) = scale_5(i) * sig(1, i) - pr_1(i)
            sig(2, i) = scale_5(i) * sig(2, i) - pr_1(i)
            sig(3, i) = scale_5(i) * sig(3, i) - pr_1(i)
            sig(4, i) = scale_5(i) * sig(4, i)
            sig(5, i) = scale_5(i) * sig(5, i)
            sig(6, i) = scale_5(i) * sig(6, i)
        Next

260:    For i = mft To mlt
            t1(i) = sig(1, i) + s7(i) * h1(i) + s6(i) * h2(i) + s6(i) * h3(i)
            t2(i) = sig(2, i) + s6(i) * h1(i) + s7(i) * h2(i) + s6(i) * h3(i)
            t3(i) = sig(3, i) + s6(i) * h1(i) + s6(i) * h2(i) + s7(i) * h3(i)
            t4(i) = sig(4, i) + s4(i) * h4(i)
            t5(i) = sig(5, i) + s4(i) * h5(i)
            t6(i) = sig(6, i) + s4(i) * h6(i)
        Next

        For i = mft To mlt
            t1(i) = t1(i) + g1(i) * deltem(i)
            t2(i) = t2(i) + g2(i) * deltem(i)
            t3(i) = t3(i) + g3(i) * deltem(i)
            t4(i) = t4(i) + g4(i) * deltem(i)
            t5(i) = t5(i) + g5(i) * deltem(i)
            t6(i) = t6(i) + g6(i) * deltem(i)
        Next

        If ielas = 1 Then GoTo 350

        For i = mft To mlt
            pr_1(i) = third * (t1(i) + t2(i) + t3(i))
            q1(i) = t1(i) + pr_1(i)
            q2(i) = t2(i) + pr_1(i)
            q3(i) = t3(i) + pr_1(i)
        Next

        For i = mft To mlt
            phi(i) = ax(i) * (q1(i) * q1(i) + q2(i) * q2(i) + q3(i) * q3(i) + 2.0 *
                (Math.Pow(t4(i), 2) + Math.Pow(t5(i), 2) + Math.Pow(t6(i), 2))) - 1.0
        Next

        d(1, 4) = 0.0
        d(4, 1) = 0.0
        d(2, 4) = 0.0
        d(4, 2) = 0.0
        d(3, 4) = 0.0
        d(4, 3) = 0.0
        d(1, 4) = 0.0
        d(1, 5) = 0.0
        d(1, 6) = 0.0
        d(2, 4) = 0.0
        d(2, 5) = 0.0
        d(2, 6) = 0.0
        d(3, 4) = 0.0
        d(3, 5) = 0.0
        d(3, 6) = 0.0
        d(4, 1) = 0.0
        d(4, 2) = 0.0
        d(4, 3) = 0.0
        d(4, 5) = 0.0
        d(4, 6) = 0.0
        d(5, 1) = 0.0
        d(5, 2) = 0.0
        d(5, 3) = 0.0
        d(5, 4) = 0.0
        d(5, 6) = 0.0
        d(6, 1) = 0.0
        d(6, 2) = 0.0
        d(6, 3) = 0.0
        d(6, 4) = 0.0
        d(6, 5) = 0.0

        For l = mft To mlt
            scale0 = 0.0
            scale1 = 1.0
            If phi(l) <= 0.0 Then GoTo 330
            d(1, 1) = s7(l)
            d(1, 2) = s6(l)
            d(1, 3) = s6(l)
            d(2, 1) = s6(l)
            d(2, 2) = s7(l)
            d(2, 3) = s6(l)
            d(3, 1) = s6(l)
            d(3, 2) = s6(l)
            d(3, 3) = s7(l)
            d(4, 4) = s4(l)
            d(5, 5) = s4(l)
            d(6, 6) = s4(l)


            c_bk56 = d   'equivalence (c11,d), YC 121219


            If iphase = 3 Then GoTo 320

            'c     compute yield surface contact point

            sclbk = -1.0
            da(1) = t1(l) - sig(1, l)
            da(2) = t2(l) - sig(2, l)
            da(3) = t3(l) - sig(3, l)
            p(4) = t4(l) - sig(4, l)
            p(5) = t5(l) - sig(5, l)
            p(6) = t6(l) - sig(6, l)
            davge = third * (da(1) + da(2) + da(3))
            p(1) = da(1) + davge
            p(2) = da(2) + davge
            p(3) = da(3) + davge
            a = ax(l) * (p(1) * p(1) + p(2) * p(2) + p(3) * p(3) + 2.0 * (p(4) * p(4) + p(5) * p(5) + p(6) * p(6)))
            b = ax(l) * (p(1) * q1(l) + p(2) * q2(l) + p(3) * q3(l) + 2.0 * (p(4) * t4(l) + p(5) * t5(l) + p(6) * t6(l)))
            q11 = b * b - a * phi(l)
            If (q11 < 0.0) Then GoTo 310
            sclbk = (-b + Math.Sqrt(q11)) / a
            sclbk = Math.Max(-one, sclbk)
310:        scale0 = -sclbk
            scale1 = 1.0 - scale0

320:        pl = third * (t1(l) + t2(l) + t3(l))
            t1l = pl + t1(l)
            t2l = pl + t2(l)
            t3l = pl + t3(l)
            t4l = t4(l)
            t5l = t5(l)
            t6l = t6(l)
            aj2l = 1.5 * (Math.Pow(t1l, 2) + Math.Pow(t2l, 2) + Math.Pow(t3l, 2)) +
                3.0 * (Math.Pow(t4l, 2) + Math.Pow(t5l, 2) + Math.Pow(t6l, 2))
            ak2l = aj2l - ak(l) * ak(l)

            'sclel = 0.5 + clsNik3di.sign(half, ak2l)    ' YC 121219
            sclel = 0.5 + objComsub.sign(half, ak2l)

            fac1 = 3.0 * s4(l)
            aj1l = Math.Sqrt(aj2l) + 1.0 - sclel
            depi(l) = sclel * (aj1l - ak(l)) / (fac1 + qh(l))
            epx(1, l) = epx(1, l) + depi(l)
            depsl = fac1 * depi(l) / aj1l
            sig(1, l) = t1(l) - depsl * t1l
            sig(2, l) = t2(l) - depsl * t2l
            sig(3, l) = t3(l) - depsl * t3l
            sig(4, l) = t4(l) - depsl * t4l
            sig(5, l) = t5(l) - depsl * t5l
            sig(6, l) = t6(l) - depsl * t6l

            If iphase = 3 Then Continue For

            av = third * (sig(1, l) + sig(2, l) + sig(3, l))
            r1 = sig(1, l) + av
            r2 = sig(2, l) + av
            r3 = sig(3, l) + av
            r4 = 2.0 * sig(4, l)
            r5 = 2.0 * sig(5, l)
            r6 = 2.0 * sig(6, l)

            c11 = c_bk56(1, 1) : c12 = c_bk56(1, 2) : c13 = c_bk56(1, 3)    ' YC 121219
            c21 = c_bk56(2, 1) : c22 = c_bk56(2, 2) : c23 = c_bk56(2, 3)
            c31 = c_bk56(3, 1) : c32 = c_bk56(3, 2) : c33 = c_bk56(3, 3)
            c44 = c_bk56(4, 4) : c55 = c_bk56(5, 5) : c66 = c_bk56(6, 6)

            w1 = c11 * r1 + c12 * r2 + c13 * r3
            w2 = c21 * r1 + c22 * r2 + c23 * r3
            w3 = c31 * r1 + c32 * r2 + c33 * r3
            w4 = c44 * r4
            w5 = c55 * r5
            w6 = c66 * r6
            w11 = r1 * w1 + r2 * w2 + r3 * w3 + r4 * w4 + r5 * w5 + r6 * w6
            w11 = w11 + ak(l) * ak(l) * qh(l) / 2.25
            w11 = 1.0 / w11
            w1w11 = w1 * w11
            w2w11 = w2 * w11
            w3w11 = w3 * w11
            w4w11 = w4 * w11
            w5w11 = w5 * w11
            d11 = c11 - w1w11 * w1
            d12 = c12 - w1w11 * w2
            d13 = c13 - w1w11 * w3
            d14 = -w1w11 * w4
            d15 = -w1w11 * w5
            d16 = -w1w11 * w6
            d22 = c22 - w2w11 * w2
            d23 = c23 - w2w11 * w3
            d24 = -w2w11 * w4
            d25 = -w2w11 * w5
            d26 = -w2w11 * w6
            d33 = c33 - w3w11 * w3
            d34 = -w3w11 * w4
            d35 = -w3w11 * w5
            d36 = -w3w11 * w6
            d44 = c44 - w4w11 * w4
            d45 = -w4w11 * w5
            d46 = -w4w11 * w6
            d55 = c55 - w5w11 * w5
            d56 = -w5w11 * w6
            d66 = c66 - w6 * w11 * w6
            dsave(1, 1, l) = d11 * scale0 + c11 * scale1
            dsave(1, 2, l) = d12 * scale0 + c12 * scale1
            dsave(1, 3, l) = d13 * scale0 + c13 * scale1
            dsave(1, 4, l) = d14 * scale0
            dsave(1, 5, l) = d15 * scale0
            dsave(1, 6, l) = d16 * scale0
            dsave(2, 1, l) = dsave(1, 2, l)
            dsave(2, 2, l) = d22 * scale0 + c22 * scale1
            dsave(2, 3, l) = d23 * scale0 + c23 * scale1
            dsave(2, 4, l) = d24 * scale0
            dsave(2, 5, l) = d25 * scale0
            dsave(2, 6, l) = d26 * scale0
            dsave(3, 1, l) = dsave(1, 3, l)
            dsave(3, 2, l) = dsave(2, 3, l)
            dsave(3, 3, l) = d33 * scale0 + c33 * scale1
            dsave(3, 4, l) = d34 * scale0
            dsave(3, 5, l) = d35 * scale0
            dsave(3, 6, l) = d36 * scale0
            dsave(4, 1, l) = dsave(1, 4, l)
            dsave(4, 2, l) = dsave(2, 4, l)
            dsave(4, 3, l) = dsave(3, 4, l)
            dsave(4, 4, l) = d44 * scale0 + c44 * scale1
            dsave(4, 5, l) = d45 * scale0
            dsave(4, 6, l) = d46 * scale0
            dsave(5, 1, l) = dsave(1, 5, l)
            dsave(5, 2, l) = dsave(2, 5, l)
            dsave(5, 3, l) = dsave(3, 5, l)
            dsave(5, 4, l) = dsave(4, 5, l)
            dsave(5, 5, l) = d55 * scale0 + c55 * scale1
            dsave(5, 6, l) = d56 * scale0
            dsave(6, 1, l) = dsave(1, 6, l)
            dsave(6, 2, l) = dsave(2, 6, l)
            dsave(6, 3, l) = dsave(3, 6, l)
            dsave(6, 4, l) = dsave(4, 6, l)
            dsave(6, 5, l) = dsave(5, 6, l)
            dsave(6, 6, l) = d66 * scale0 + c66 * scale1

            'c     rotate constitutive matrix

            Call tranfc(l)

            Continue For

330:        sig(1, l) = t1(l)
            sig(2, l) = t2(l)
            sig(3, l) = t3(l)
            sig(4, l) = t4(l)
            sig(5, l) = t5(l)
            sig(6, l) = t6(l)

        Next

        GoTo 370

350:    For i = mft To mlt
            depi(i) = 0.0
            sig(1, i) = t1(i)
            sig(2, i) = t2(i)
            sig(3, i) = t3(i)
            sig(4, i) = t4(i)
            sig(5, i) = t5(i)
            sig(6, i) = t6(i)
        Next

        'c     update stresses, strains, effective plastic strain

370:

        'c
        'Call objSolve.rotat2(sig, ln)  ' YC 121219
        Call rotat2(sig, ln)

        For i = mft To mlt
            tmpo(1, i) = toldg(i)
            sig11s(i) = sig(1, i)
            sig22s(i) = sig(2, i)
            sig33s(i) = sig(3, i)
            sig12s(i) = sig(4, i)
            sig23s(i) = sig(5, i)
            sig31s(i) = sig(6, i)
        Next

        If iteref <> 0 Then Return
        If iphase = 3 Then Return

        'c     modify stress state

        If ipt > 1 And incflg = 0 Then GoTo 450
        If prop(1, 1) <> 0.0 Or prop(2, 1) <> 0.0 Then GoTo 400
        For i = mft To mlt
            alphan(i) = prop(1, 4)
            daltn(i) = 0.0
        Next
        GoTo 430
400:    For i = mft To iul
            'TODO - SENT PROPER ARRAYS

            ' YC 121219
            'Call intrpm(prop, prop(1, 2), ctmp(i), prop(1, 2), thrmo)
            'Call intrpm(prop, prop(1, 2), tnewg(i), prop(1, 2), thrmn)

            Dim prop_1_1_1D(8), prop_1_2_1D(8), prop_1_2(8, 5) As Double
            Call objComsub.ArrayExtract1Dfrom2D(prop, 1 - 1, 1 - 1, prop_1_1_1D, 8)
            Call objComsub.ArrayExtract1Dfrom2D(prop, 1 - 1, 2 - 1, prop_1_2_1D, 8)
            Call objComsub.ArrayExtract2Dfrom2D(prop, 1 - 1, 2 - 1, prop_1_2, 8, 5)
            Call intrpm(prop_1_1_1D, prop_1_2_1D, ctmp(i), prop_1_2, thrmo)
            Call intrpm(prop_1_1_1D, prop_1_2_1D, tnewg(i), prop_1_2, thrmn)
            ' YC 121219 END

            alphan(i) = (thrmo(3) + thrmn(3)) / 2.0
            daltn(i) = thrmn(3) - thrmo(3)
        Next
        If itemp < 1 Or itread <> 0 Then GoTo 430
        For i = mft To mlt
            alphan(i) = alphan(mft)
            daltn(i) = daltn(mft)
        Next
430:    For i = mft To mlt
            deltmn(i) = tnewg(i) - ctmp(i)
            midtmn(i) = 0.5 * (tnewg(i) + ctmp(i))
            tepsn(i) = alphan(i) * deltmn(i) + daltn(i) * (midtmn(i) - tref(1, i))
            sigtrm(i) = (dsave(1, 1, i) + dsave(1, 2, i) + dsave(1, 3, i)) * tepsn(i)
        Next
450:    For i = mft To mlt
            sig11s(i) = sig11s(i) - sigtrm(i)
            sig22s(i) = sig22s(i) - sigtrm(i)
            sig33s(i) = sig33s(i) - sigtrm(i)
        Next

    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine s4main(prop,nodes,sig,tmpo,epx,told,tnew,tref,ln)
'c
'
'      implicit double precision (a-h,o-z)                             
'
'c
'c===> module for stress point integration of J-2 thermo-plasticity
'c
'      common/double/iprec
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk08/itherm,itemp,n20,n21
'      logical ldns
'      common/bk21/rhsn,rhsvn,cvtl,ectl,tolls,rctl,ldns,men,maxref,iteref
'      common/bk32/h(8,9),partr(8,9),parts(8,9),partt(8,9)
'      common/bk56/
'     1 c11,c21,c31,c41,c51,c61,c12,c22,c32,c42,c52,c62,
'     2 c13,c23,c33,c43,c53,c63,c14,c24,c34,c44,c54,c64,
'     3 c15,c25,c35,c45,c55,c65,c16,c26,c36,c46,c56,c66,ipt
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      common/vect5/
'     1 g1(64),g2(64),g3(64),g4(64),g5(64),g6(64),q1(64),q2(64),
'     2 q3(64),scale(64),pr(64),aj2(64),sj2(64),dum5(2880)
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect3/
'     1 sig11s(64),sig22s(64),sig33s(64),sig12s(64),sig23s(64),
'     2 sig31s(64)
'      common/vect8/
'     1 dsave(6,6,64)
'      common/vect14/sigtrm(64),s1(64),s2(64),s3(64),s4(64),s5(64),
'     1 s6(64),s7(64),c11i(64),c12i(64),d11t(64),d12t(64),
'     2 d44t(64),h1(64),h2(64),h3(64),h4(64),h5(64),h6(64),
'     3 teps(64),tepsn(64),t1(64),t2(64),t3(64),t4(64),t5(64),
'     4 t6(64),depi(64),dum14(40,64)
'      common/vect15/
'     1 d1(64),d2(64),d3(64),d4(64),d5(64),d6(64),dum15(384)
'      common/vect16/toldg(64),tnewg(64),deltem(64),midtem(64),
'     1 ctmp(64),otmp(64),ym(64),pv(64),qs(64),qh(64),alpha(64),
'     2 dpvt(64),dqst(64),dhmt(64),dalt(64),phi(64),ak(64),ax(64),
'     3 dymt(64),deltmn(64),midtmn(64),alphan(64),daltn(64),
'     & dum16(6,64)
'
'      dimension sig(ln,*),nodes(ln*iprec,*),prop(8,*),told(*),tnew(*),
'     1 p(6),da(6),thrmo(5),thrmn(5),d(6,6),
'     2 epx(ln,*),tref(ln,*),tmpo(ln,*)
'      common/tin/itread,itrpm,itrp2
'      equivalence (c11,d)
'
'      double precision midtem,midtmn
'c
'      third=-1.d0/3.
'      half = 0.5
'      one = 1.0
'c
'      call rotat1 (sig,ln)
'c
'      ielas=0
'      if ((prop(1,5)+prop(2,5)).eq.0.0) ielas=1
'      if (ipt.gt.1 .and. incflg.eq.0) go to 180                         incmode
'c
'      if (itemp.lt.1.or.itread.ne.0) go to 20
'      do 10 i=mft,mlt
'      toldg(i)=told(1)
'   10 tnewg(i)=tnew(1)
'      go to 50
'c
'   20 do 40 i=mft,mlt
'      toldg(i)=0.0
'      tnewg(i)=0.0
'      if (incflg.eq.0) then                                             incmode
'c....... element-averaged temperature for b-bar
'         do 30 k=1,8
'         toldg(i)=toldg(i)+0.125*told(nodes(k,i))
'   30    tnewg(i)=tnewg(i)+0.125*tnew(nodes(k,i))
'      else                                                              incmode
'c....... shape ftn interpolation of temperature                         incmode
'         do 31 k=1,8                                                    incmode
'         toldg(i)=toldg(i)+h(k,ipt)*told(nodes(k,i))                    incmode
'   31    tnewg(i)=tnewg(i)+h(k,ipt)*tnew(nodes(k,i))                    incmode
'      endif                                                             incmode
'   40 continue
'c
'   50 if (iphase.ne.3) go to 80
'   60 do 70 i=mft,mlt
'      ctmp(i)=tnewg(i)
'      otmp(i)=tmpo(1,i)
'      deltem(i)=ctmp(i)-otmp(i)
'      midtem(i)=.5*(ctmp(i)+otmp(i))
'   70 continue
'      go to 100
'   80 if (iteref.ne.0) go to 60
'      do 90 i=mft,mlt
'      ctmp(i)=toldg(i)
'      otmp(i)=tmpo(1,i)
'      deltem(i)=ctmp(i)-otmp(i)
'      midtem(i)=.5*(ctmp(i)+otmp(i))
'   90 continue
'c
'  100 if (prop(1,1).ne.0..or.prop(2,1).ne.0.) go to 120
'      do 110 i=mft,mlt
'      ym(i)=prop(1,2)
'      pv(i)=prop(1,3)
'      qs(i)=prop(1,5)
'      qh(i)=prop(1,6)
'      alpha(i)=prop(1,4)
'      dymt(i)=0.
'      dpvt(i)=0.
'      dqst(i)=0.
'      dhmt(i)=0.
'      dalt(i)=0.
'  110 continue
'      go to 150
'c
'  120 iul=mlt
'
'c ... if itemp gt 0 all nodes have same temp unless itread ne 0
'c ... then nodal temps prescribed in file
'
'      if (itemp.gt.0.and.itread.eq.0) iul=mft
'      do 130 i=mft,iul
'      call intrpm (prop,prop(1,2),otmp(i),prop(1,2),thrmo)
'      call intrpm (prop,prop(1,2),ctmp(i),prop(1,2),thrmn)
'      dt=deltem(i)
'      if (abs(dt).lt.1.e-20) dt=1.0
'      ym(i)=(thrmo(1)+thrmn(1))/2.0
'      pv(i)=(thrmo(2)+thrmn(2))/2.0
'      qs(i)=(thrmo(4)+thrmn(4))/2.0
'      qh(i)=(thrmo(5)+thrmn(5))/2.0
'      alpha(i)=(thrmo(3)+thrmn(3))/2.0
'      dymt(i)=(thrmn(1)-thrmo(1))/dt
'      dpvt(i)=(thrmn(2)-thrmo(2))/dt
'      dqst(i)=(thrmn(4)-thrmo(4))/dt
'      dhmt(i)=(thrmn(5)-thrmo(5))/dt
'      dalt(i)=thrmn(3)-thrmo(3)
'  130 continue
'      if (itemp.lt.1.or.itread.ne.0) go to 150
'      do 140 i=mft,mlt
'      ym(i)=ym(mft)
'      pv(i)=pv(mft)
'      qs(i)=qs(mft)
'      qh(i)=qh(mft)
'      alpha(i)=alpha(mft)
'      dymt(i)=dymt(mft)
'      dpvt(i)=dpvt(mft)
'      dqst(i)=dqst(mft)
'      dhmt(i)=dhmt(mft)
'      dalt(i)=dalt(mft)
'  140 continue
'c
'  150 do 160 i=mft,mlt
'      s1(i)=(1.+pv(i))*(1.0-2.0*pv(i))
'      s2(i)=(1.-pv(i))/s1(i)
'      s3(i)=ym(i)/(1.+pv(i))
'      s4(i)=s3(i)/2.
'      s5(i)=s3(i)/(1.-2.*pv(i))
'      s6(i)=s5(i)*pv(i)
'      s7(i)=s5(i)-s6(i)
'      teps(i)=alpha(i)*deltem(i)+dalt(i)*(midtem(i)-tref(1,i))
'  160 continue
'c
'      do 170 i=mft,mlt
'      c11i(i)=1.0/ym(i)
'      c12i(i)=-pv(i)/ym(i)
'      d11t(i)=dymt(i)*s2(i)+dpvt(i)*((s2(i)*(1.0+4.0*pv(i))*ym(i)-ym(i))
'     1 /s1(i))
'      d12t(i)=dymt(i)*pv(i)/s1(i)+dpvt(i)*((ym(i)+pv(i)*ym(i)*(1.+4.*pv
'     1 (i))/s1(i))/s1(i))
'  170 d44t(i)=(dymt(i)-dpvt(i)*s3(i))/(2.*(1.+pv(i)))
'c
'  180 do 190 i=mft,mlt
'      h1(i)=c11i(i)*sig(1,i)+c12i(i)*(sig(2,i)+sig(3,i))
'      h2(i)=c11i(i)*sig(2,i)+c12i(i)*(sig(1,i)+sig(3,i))
'      h3(i)=c11i(i)*sig(3,i)+c12i(i)*(sig(1,i)+sig(2,i))
'      h4(i)=sig(4,i)/s4(i)
'      h5(i)=sig(5,i)/s4(i)
'  190 h6(i)=sig(6,i)/s4(i)
'c
'      do 200 i=mft,mlt
'      dsave(1,1,i)=s7(i)
'      dsave(1,2,i)=s6(i)
'      dsave(1,3,i)=s6(i)
'      dsave(1,4,i)=0.
'      dsave(1,5,i)=0.
'      dsave(1,6,i)=0.
'      dsave(2,1,i)=s6(i)
'      dsave(2,2,i)=s7(i)
'      dsave(2,3,i)=s6(i)
'      dsave(2,4,i)=0.
'      dsave(2,5,i)=0.
'      dsave(2,6,i)=0.
'      dsave(3,1,i)=s6(i)
'      dsave(3,2,i)=s6(i)
'      dsave(3,3,i)=s7(i)
'      dsave(3,4,i)=0.
'      dsave(3,5,i)=0.
'      dsave(3,6,i)=0.
'      dsave(4,1,i)=0.
'      dsave(4,2,i)=0.
'      dsave(4,3,i)=0.
'      dsave(4,4,i)=s4(i)
'      dsave(4,5,i)=0.
'      dsave(4,6,i)=0.
'      dsave(5,1,i)=0.
'      dsave(5,2,i)=0.
'      dsave(5,3,i)=0.
'      dsave(5,4,i)=0.
'      dsave(5,5,i)=s4(i)
'      dsave(5,6,i)=0.
'      dsave(6,1,i)=0.
'      dsave(6,2,i)=0.
'      dsave(6,3,i)=0.
'      dsave(6,4,i)=0.
'      dsave(6,5,i)=0.
'      dsave(6,6,i)=s4(i)
'  200 continue
'c
'      do 210 i=mft,mlt
'      g1(i)=d11t(i)*h1(i)+d12t(i)*(h2(i)+h3(i))
'      g2(i)=d11t(i)*h2(i)+d12t(i)*(h1(i)+h3(i))
'      g3(i)=d11t(i)*h3(i)+d12t(i)*(h2(i)+h1(i))
'      g4(i)=d44t(i)*h4(i)
'      g5(i)=d44t(i)*h5(i)
'  210 g6(i)=d44t(i)*h6(i)
'c
'      do 220 i=mft,mlt
'      h1(i)=d1(i)-teps(i)
'      h2(i)=d2(i)-teps(i)
'      h3(i)=d3(i)-teps(i)
'      h4(i)=d4(i)
'      h5(i)=d5(i)
'  220 h6(i)=d6(i)
'c
'      if (ielas.eq.1) go to 260
'c
'      do 230 i=mft,mlt
'      ak(i)=qs(i)+qh(i)*epx(1,i)
'      pr(i)=third*(sig(1,i)+sig(2,i)+sig(3,i))
'      sig(1,i)=sig(1,i)+pr(i)
'      sig(2,i)=sig(2,i)+pr(i)
'      sig(3,i)=sig(3,i)+pr(i)
'      aj2(i)=-sig(1,i)*sig(2,i)-sig(2,i)*sig(3,i)-sig(3,i)*sig(1,i)
'     1       +sig(4,i)**2+sig(5,i)**2+sig(6,i)**2
'  230 continue
'      do 240 i=mft,mlt
'      sj2(i)=sqrt(3.*abs(aj2(i)))+1.e-20
'  240 scale(i)=min(one,ak(i)/sj2(i))
'c
'      do 250 i=mft,mlt
'      epx(1,i)=epx(1,i)+(1.-scale(i))*sj2(i)/(3.*s4(i)+qh(i))
'      ak(i)=qs(i)+qh(i)*epx(1,i)
'      ax(i)=1.5/(ak(i)*ak(i))
'      sig(1,i)=scale(i)*sig(1,i)-pr(i)
'      sig(2,i)=scale(i)*sig(2,i)-pr(i)
'      sig(3,i)=scale(i)*sig(3,i)-pr(i)
'      sig(4,i)=scale(i)*sig(4,i)
'      sig(5,i)=scale(i)*sig(5,i)
'      sig(6,i)=scale(i)*sig(6,i)
'  250 continue
'c
'  260 do 270 i=mft,mlt
'      t1(i)=sig(1,i)+s7(i)*h1(i)+s6(i)*h2(i)+s6(i)*h3(i)
'      t2(i)=sig(2,i)+s6(i)*h1(i)+s7(i)*h2(i)+s6(i)*h3(i)
'      t3(i)=sig(3,i)+s6(i)*h1(i)+s6(i)*h2(i)+s7(i)*h3(i)
'      t4(i)=sig(4,i)+s4(i)*h4(i)
'      t5(i)=sig(5,i)+s4(i)*h5(i)
'  270 t6(i)=sig(6,i)+s4(i)*h6(i)
'c
'      do 280 i=mft,mlt
'      t1(i)=t1(i)+g1(i)*deltem(i)
'      t2(i)=t2(i)+g2(i)*deltem(i)
'      t3(i)=t3(i)+g3(i)*deltem(i)
'      t4(i)=t4(i)+g4(i)*deltem(i)
'      t5(i)=t5(i)+g5(i)*deltem(i)
'  280 t6(i)=t6(i)+g6(i)*deltem(i)
'c
'      if (ielas.eq.1) go to 350
'c
'      do 290 i=mft,mlt
'      pr(i)=third*(t1(i)+t2(i)+t3(i))
'      q1(i)=t1(i)+pr(i)
'      q2(i)=t2(i)+pr(i)
'      q3(i)=t3(i)+pr(i)
'  290 continue
'      do 300 i=mft,mlt
'  300 phi(i)=ax(i)*(q1(i)*q1(i)+q2(i)*q2(i)+q3(i)*q3(i)+2.0*(t4(i)**2
'     1      +t5(i)**2+t6(i)**2))-1.0000
'c
'      d(1,4)=0.
'      d(4,1)=0.
'      d(2,4)=0.
'      d(4,2)=0.
'      d(3,4)=0.
'      d(4,3)=0.
'      d(1,4)=0.
'      d(1,5)=0.
'      d(1,6)=0.
'      d(2,4)=0.
'      d(2,5)=0.
'      d(2,6)=0.
'      d(3,4)=0.
'      d(3,5)=0.
'      d(3,6)=0.
'      d(4,1)=0.
'      d(4,2)=0.
'      d(4,3)=0.
'      d(4,5)=0.
'      d(4,6)=0.
'      d(5,1)=0.
'      d(5,2)=0.
'      d(5,3)=0.
'      d(5,4)=0.
'      d(5,6)=0.
'      d(6,1)=0.
'      d(6,2)=0.
'      d(6,3)=0.
'      d(6,4)=0.
'      d(6,5)=0.
'      do 340 l=mft,mlt
'      scale0=0.0
'      scale1=1.0
'      if (phi(l).le.0.0) go to 330
'      d(1,1)=s7(l)
'      d(1,2)=s6(l)
'      d(1,3)=s6(l)
'      d(2,1)=s6(l)
'      d(2,2)=s7(l)
'      d(2,3)=s6(l)
'      d(3,1)=s6(l)
'      d(3,2)=s6(l)
'      d(3,3)=s7(l)
'      d(4,4)=s4(l)
'      d(5,5)=s4(l)
'      d(6,6)=s4(l)
'c
'      if (iphase.eq.3) go to 320
'c
'c     compute yield surface contact point
'c
'      sclbk=-1.0
'      da(1)=t1(l)-sig(1,l)
'      da(2)=t2(l)-sig(2,l)
'      da(3)=t3(l)-sig(3,l)
'      p(4)=t4(l)-sig(4,l)
'      p(5)=t5(l)-sig(5,l)
'      p(6)=t6(l)-sig(6,l)
'      davge=third*(da(1)+da(2)+da(3))
'      p(1)=da(1)+davge
'      p(2)=da(2)+davge
'      p(3)=da(3)+davge
'      a=ax(l)*(p(1)*p(1)+p(2)*p(2)+p(3)*p(3)+2.0*(p(4)*p(4)
'     1 +p(5)*p(5)+p(6)*p(6)))
'      b=ax(l)*(p(1)*q1(l)+p(2)*q2(l)+p(3)*q3(l)+2.0*(p(4)*t4(l)
'     1 +p(5)*t5(l)+p(6)*t6(l)))
'      q11=b*b-a*phi(l)
'      if (q11.lt.0.0) go to 310
'      sclbk=(-b+sqrt(q11))/a
'      sclbk=max(-one,sclbk)
'  310 scale0=-sclbk
'      scale1=1.-scale0
'c
'  320 pl =third*(t1(l)+t2(l)+t3(l))
'      t1l=pl+t1(l)
'      t2l=pl+t2(l)
'      t3l=pl+t3(l)
'      t4l=t4(l)
'      t5l=t5(l)
'      t6l=t6(l)
'      aj2l=1.5*(t1l**2+t2l**2+t3l**2)+3.*(t4l**2+t5l**2+t6l**2)
'      ak2l=aj2l-ak(l)*ak(l)
'      sclel=.50e0+ sign(half,ak2l)
'      fac1=3.*s4(l)
'      aj1l=sqrt(aj2l)+1.0-sclel
'      depi(l) =sclel*(aj1l-ak(l))/(fac1+qh(l))
'      epx(1,l)=epx(1,l)+depi(l)
'      depsl   =fac1*depi(l)/aj1l
'      sig(1,l)=t1(l) -depsl*t1l
'      sig(2,l)=t2(l) -depsl*t2l
'      sig(3,l)=t3(l) -depsl*t3l
'      sig(4,l)=t4(l) -depsl*t4l
'      sig(5,l)=t5(l) -depsl*t5l
'      sig(6,l)=t6(l) -depsl*t6l
'c
'      if (iphase.eq.3) go to 340
'c
'      av=third*(sig(1,l)+sig(2,l)+sig(3,l))
'      r1=sig(1,l)+av
'      r2=sig(2,l)+av
'      r3=sig(3,l)+av
'      r4=2.0*sig(4,l)
'      r5=2.0*sig(5,l)
'      r6=2.0*sig(6,l)
'      w1=c11*r1+c12*r2+c13*r3
'      w2=c21*r1+c22*r2+c23*r3
'      w3=c31*r1+c32*r2+c33*r3
'      w4=c44*r4
'      w5=c55*r5
'      w6=c66*r6
'      w11=r1*w1+r2*w2+r3*w3+r4*w4+r5*w5+r6*w6
'      w11=w11+ak(l)*ak(l)*qh(l)/2.25
'      w11=1.0/w11
'      w1w11=w1*w11
'      w2w11=w2*w11
'      w3w11=w3*w11
'      w4w11=w4*w11
'      w5w11=w5*w11
'      d11=c11-w1w11*w1
'      d12=c12-w1w11*w2
'      d13=c13-w1w11*w3
'      d14=   -w1w11*w4
'      d15=   -w1w11*w5
'      d16=   -w1w11*w6
'      d22=c22-w2w11*w2
'      d23=c23-w2w11*w3
'      d24=   -w2w11*w4
'      d25=   -w2w11*w5
'      d26=   -w2w11*w6
'      d33=c33-w3w11*w3
'      d34=   -w3w11*w4
'      d35=   -w3w11*w5
'      d36=   -w3w11*w6
'      d44=c44-w4w11*w4
'      d45=   -w4w11*w5
'      d46=   -w4w11*w6
'      d55=c55-w5w11*w5
'      d56=   -w5w11*w6
'      d66=c66-w6*w11*w6
'      dsave(1,1,l)=d11*scale0+c11*scale1
'      dsave(1,2,l)=d12*scale0+c12*scale1
'      dsave(1,3,l)=d13*scale0+c13*scale1
'      dsave(1,4,l)=d14*scale0
'      dsave(1,5,l)=d15*scale0
'      dsave(1,6,l)=d16*scale0
'      dsave(2,1,l)=dsave(1,2,l)
'      dsave(2,2,l)=d22*scale0+c22*scale1
'      dsave(2,3,l)=d23*scale0+c23*scale1
'      dsave(2,4,l)=d24*scale0
'      dsave(2,5,l)=d25*scale0
'      dsave(2,6,l)=d26*scale0
'      dsave(3,1,l)=dsave(1,3,l)
'      dsave(3,2,l)=dsave(2,3,l)
'      dsave(3,3,l)=d33*scale0+c33*scale1
'      dsave(3,4,l)=d34*scale0
'      dsave(3,5,l)=d35*scale0
'      dsave(3,6,l)=d36*scale0
'      dsave(4,1,l)=dsave(1,4,l)
'      dsave(4,2,l)=dsave(2,4,l)
'      dsave(4,3,l)=dsave(3,4,l)
'      dsave(4,4,l)=d44*scale0+c44*scale1
'      dsave(4,5,l)=d45*scale0
'      dsave(4,6,l)=d46*scale0
'      dsave(5,1,l)=dsave(1,5,l)
'      dsave(5,2,l)=dsave(2,5,l)
'      dsave(5,3,l)=dsave(3,5,l)
'      dsave(5,4,l)=dsave(4,5,l)
'      dsave(5,5,l)=d55*scale0+c55*scale1
'      dsave(5,6,l)=d56*scale0
'      dsave(6,1,l)=dsave(1,6,l)
'      dsave(6,2,l)=dsave(2,6,l)
'      dsave(6,3,l)=dsave(3,6,l)
'      dsave(6,4,l)=dsave(4,6,l)
'      dsave(6,5,l)=dsave(5,6,l)
'      dsave(6,6,l)=d66*scale0+c66*scale1
'c
'c     rotate constitutive matrix
'c
'      call tranfc (l)
'c
'      go to 340
'c
'  330 sig(1,l)=t1(l)
'      sig(2,l)=t2(l)
'      sig(3,l)=t3(l)
'      sig(4,l)=t4(l)
'      sig(5,l)=t5(l)
'      sig(6,l)=t6(l)
'c
'  340 continue
'c
'      go to 370
'c
'  350 do 360 i=mft,mlt
'      depi(i)=0.
'      sig(1,i)=t1(i)
'      sig(2,i)=t2(i)
'      sig(3,i)=t3(i)
'      sig(4,i)=t4(i)
'      sig(5,i)=t5(i)
'  360 sig(6,i)=t6(i)
'c
'c     update stresses, strains, effective plastic strain
'c
'  370 continue
'
'c
'      call rotat2 (sig,ln)
'c
'      do 380 i=mft,mlt
'      tmpo(1,i)=toldg(i)
'      sig11s(i)=sig(1,i)
'      sig22s(i)=sig(2,i)
'      sig33s(i)=sig(3,i)
'      sig12s(i)=sig(4,i)
'      sig23s(i)=sig(5,i)
'  380 sig31s(i)=sig(6,i)
'c
'      if (iteref.ne.0) return
'      if (iphase.eq.3) return
'c
'c     modify stress state
'c
'      if (ipt.gt.1 .and. incflg.eq.0) go to 450                        
'      if (prop(1,1).ne.0..or.prop(2,1).ne.0.) go to 400
'      do 390 i=mft,mlt
'      alphan(i)=prop(1,4)
'      daltn(i)=0.
'  390 continue
'      go to 430
'  400 do 410 i=mft,iul
'      call intrpm (prop,prop(1,2),ctmp(i),prop(1,2),thrmo)
'      call intrpm (prop,prop(1,2),tnewg(i),prop(1,2),thrmn)
'      alphan(i)=(thrmo(3)+thrmn(3))/2.0
'      daltn(i)=thrmn(3)-thrmo(3)
'  410 continue
'      if (itemp.lt.1.or.itread.ne.0) go to 430
'      do 420 i=mft,mlt
'      alphan(i)=alphan(mft)
'  420 daltn(i)=daltn(mft)
'  430 do 440 i=mft,mlt
'      deltmn(i)=tnewg(i)-ctmp(i)
'      midtmn(i)=.5*(tnewg(i)+ctmp(i))
'      tepsn(i)=alphan(i)*deltmn(i)+daltn(i)*(midtmn(i)-tref(1,i))
'      sigtrm(i)=(dsave(1,1,i)+dsave(1,2,i)+dsave(1,3,i))*tepsn(i)
'  440 continue
'  450 do 460 i=mft,mlt
'      sig11s(i)=sig11s(i)-sigtrm(i)
'      sig22s(i)=sig22s(i)-sigtrm(i)
'      sig33s(i)=sig33s(i)-sigtrm(i)
'  460 continue
'c
'      return
'      end

'This file contains all the methods of stfic.f
Imports System.Text

Partial Public Class clsSolve


    
    Public ibar, numdc As Integer


    Private model As Integer  ' YC?


    Private pxa1(64), pxa2(64), pxa3(64), pxa4(64), pxa5(64), pxa6(64), pxa7(64), pxa8(64),
        pya1(64), pya2(64), pya3(64), pya4(64), pya5(64), pya6(64), pya7(64), pya8(64),
        pza1(64), pza2(64), pza3(64), pza4(64), pza5(64), pza6(64), pza7(64), pza8(64) As Double

    Private volume(64) As Double

    Private g11(64), g21(64), g31(64),
        g12(64), g22(64), g32(64),
        g13(64), g23(64), g33(64),
        g14(64), g24(64), g34(64),
        g15(64), g25(64), g35(64),
        g16(64), g26(64), g36(64),
        g17(64), g27(64), g37(64),
        g18(64), g28(64), g38(64) As Double

    Private h11(64), h21(64), h31(64),
        h12(64), h22(64), h32(64),
        h13(64), h23(64), h33(64),
        h14(64), h24(64), h34(64),
        h15(64), h25(64), h35(64),
        h16(64), h26(64), h36(64),
        h17(64), h27(64), h37(64),
        h18(64), h28(64), h38(64) As Double

    Private hic11(64), hic21(64), hic31(64),
        hic12(64), hic22(64), hic32(64),
        hic13(64), hic23(64), hic33(64) As Double

    Private diavg(64), dfavg(64), volnd(64), volcd(64) As Double

    Private x12(64), x34(64), x56(64), x78(64),
        y12(64), y34(64), y56(64), y78(64),
        z12(64), z34(64), z56(64), z78(64),
        x14(64), x23(64), x58(64), x67(64),
        y14(64), y23(64), y58(64), y67(64),
        z14(64), z23(64), z58(64), z67(64),
        x15(64), x26(64), x37(64), x48(64),
        y15(64), y26(64), y37(64), y48(64),
        z15(64), z26(64), z37(64), z48(64) As Double

    Private xic1(64), xic2(64), xic3(64),
        yic1(64), yic2(64), yic3(64),
      zic1(64), zic2(64), zic3(64) As Double

    Private vlinc(64) As Double

    Private di1(64, 8), di2(64, 8), di3(64, 8), di4(64, 8),
        di5(64, 8), di6(64, 8), di7(64, 8), di8(64, 8), di9(64, 8) As Double

    Private dt1(64, 8), dt2(64, 8), dt3(64, 8), dt4(64, 8),
        dt5(64, 8), dt6(64, 8), dt7(64, 8), dt8(64, 8), dt9(64, 8) As Double

    Private dd1(64, 8), dd2(64, 8), dd3(64, 8), dd4(64, 8),
  dd5(64, 8), dd6(64, 8), dd7(64, 8), dd8(64, 8), dd9(64, 8) As Double

    Private volgp(64) As Double

    Private f1(64), f2(64), f3(64), f4(64), f5(64), f6(64), f7(64), f8(64), f9(64) As Double

    Private r11(64), r12(64), r13(64), r21(64), r22(64), r23(64), r31(64), r32(64), r33(64) As Double

    Private q11(64), q12(64), q13(64), q21(64), q22(64), q23(64), q31(64), q32(64), q33(64) As Double

    Private s11(64), s12(64), s13(64), s21(64), s22(64), s23(64), s31(64), s32(64), s33(64) As Double  ' vect0 vs. vect5

    'Private d1(64), d2(64), d3(64), d4(64), d5(64), d6(64) As Double

    Private sig11s(64), sig22s(64), sig33s(64), sig12s(64), sig23s(64), sig31s(64) As Double

    Private dsave2(36, 64) As Double

    Private pxm1(64), pxm2(64), pxm3(64), pxm4(64), pxm5(64), pxm6(64), pxm7(64), pxm8(64),
        pym1(64), pym2(64), pym3(64), pym4(64), pym5(64), pym6(64), pym7(64), pym8(64),
        pzm1(64), pzm2(64), pzm3(64), pzm4(64), pzm5(64), pzm6(64), pzm7(64), pzm8(64) As Double

    'Private s(324, 64) As Double  'YC?  lmm(648,64)=s(324,64), s(,)=lmm(49:648,64)=s(25:324,64)
    Private s(300, 64) As Double

    Private px1(64), px2(64), px3(64), px4(64), px5(64), px6(64), px7(64), px8(64),
 py1(64), py2(64), py3(64), py4(64), py5(64), py6(64), py7(64), py8(64),
  pz1(64), pz2(64), pz3(64), pz4(64), pz5(64), pz6(64), pz7(64), pz8(64) As Double

    Private pxic1(64), pxic2(64), pxic3(64),
  pyic1(64), pyic2(64), pyic3(64),
  pzic1(64), pzic2(64), pzic3(64) As Double

    Private sig11(64), sig22(64), sig33(64), sig12(64), sig23(64), sig31(64) As Double

    Private rov(64) As Double






    Private trsg(64) As Double

    Private ipt, mft, mlt, nel, nelsub As Integer


    Dim d1(64), d2(64), d3(64), d4(64), d5(64), d6(64) As Double   ' YC 121219


    ''' <summary>
    ''' drive incompatible hex residual/stiffness formation
    ''' </summary>
    ''' <param name="nmel"></param>
    Public Sub stfic(ByRef nmel As Integer, ByVal ng As Integer)

        'Dim d1(64), d2(64), d3(64), d4(64), d5(64), d6(64) As Double   ' YC 121219


        Dim i, j, k As Integer    ' YC 102418
        If ng = 71 Then
            ng = ng
        End If
        model = lhex(1) '  equivalence (lhex(1),model) YC 102418

        'change pxa1 to pxa1,,,, pza8 by YC 092018
        'Call azero(pxa1, 1536)  '1536=24*64
        Call azero(pxa1, 64)
        Call azero(pxa2, 64)
        Call azero(pxa3, 64)
        Call azero(pxa4, 64)
        Call azero(pxa5, 64)
        Call azero(pxa6, 64)
        Call azero(pxa7, 64)
        Call azero(pxa8, 64)
        Call azero(pya1, 64)
        Call azero(pya2, 64)
        Call azero(pya3, 64)
        Call azero(pya4, 64)
        Call azero(pya5, 64)
        Call azero(pya6, 64)
        Call azero(pya7, 64)
        Call azero(pya8, 64)
        Call azero(pza1, 64)
        Call azero(pza2, 64)
        Call azero(pza3, 64)
        Call azero(pza4, 64)
        Call azero(pza5, 64)
        Call azero(pza6, 64)
        Call azero(pza7, 64)
        Call azero(pza8, 64)
        'change pxa1 to pxa1,,,, pza8 by YC 092018 END

        Call azero(volume, 64)


        '#Region "geometry at n+1"      'YC? 092018

        'c     note: the flags in columns 73-80 denote either a geometrically
        'c           linear formulation ("linear","lininc") or a geometrically
        'c           nonlinear formulation ("nonlin","noninc")

        For i = lft To llt
            g11(i) = f11(i)
            g21(i) = f21(i)
            g31(i) = f31(i)
            g12(i) = f12(i)
            g22(i) = f22(i)
            g32(i) = f32(i)
            g13(i) = f13(i)
            g23(i) = f23(i)
            g33(i) = f33(i)
            g14(i) = f14(i)
            g24(i) = f24(i)
            g34(i) = f34(i)
            g15(i) = f15(i)
            g25(i) = f25(i)
            g35(i) = f35(i)
            g16(i) = f16(i)
            g26(i) = f26(i)
            g36(i) = f36(i)
            g17(i) = f17(i)
            g27(i) = f27(i)
            g37(i) = f37(i)
            g18(i) = f18(i)
            g28(i) = f28(i)
            g38(i) = f38(i)
            '   20 continue
        Next
        '#End Region 'YC? 092018


        '#Region "geometry at n+1/2" 'YC? 092018
        For i = lft To llt

            h11(i) = g11(i)
            h21(i) = g21(i)
            h31(i) = g31(i)
            h12(i) = g12(i)
            h22(i) = g22(i)
            h32(i) = g32(i)
            h13(i) = g13(i)
            h23(i) = g23(i)
            h33(i) = g33(i)
            h14(i) = g14(i)
            h24(i) = g24(i)
            h34(i) = g34(i)
            h15(i) = g15(i)
            h25(i) = g25(i)
            h35(i) = g35(i)
            h16(i) = g16(i)
            h26(i) = g26(i)
            h36(i) = g36(i)
            h17(i) = g17(i)
            h27(i) = g27(i)
            h37(i) = g37(i)
            h18(i) = g18(i)
            h28(i) = g28(i)
            h38(i) = g38(i)
            hic11(i) = 0.0
            hic21(i) = 0.0
            hic31(i) = 0.0
            hic12(i) = 0.0
            hic22(i) = 0.0
            hic32(i) = 0.0
            hic13(i) = 0.0
            hic23(i) = 0.0
            hic33(i) = 0.0

        Next
        '#End Region    'YC? 092018


        '#Region "reference coordinates"  'YC? 092018
        For i = lft To llt
            diavg(i) = 0.0
            dfavg(i) = 0.0
            volnd(i) = 0.0
            volcd(i) = 0.0
            x12(i) = f11(i) - f12(i)
            x34(i) = f13(i) - f14(i)
            x56(i) = f15(i) - f16(i)
            x78(i) = f17(i) - f18(i)
            x14(i) = f11(i) - f14(i)
            x23(i) = f12(i) - f13(i)
            x58(i) = f15(i) - f18(i)
            x67(i) = f16(i) - f17(i)
            x15(i) = f11(i) - f15(i)
            x26(i) = f12(i) - f16(i)
            x37(i) = f13(i) - f17(i)
            x48(i) = f14(i) - f18(i)
            y12(i) = f21(i) - f22(i)
            y34(i) = f23(i) - f24(i)
            y56(i) = f25(i) - f26(i)
            y78(i) = f27(i) - f28(i)
            y14(i) = f21(i) - f24(i)
            y23(i) = f22(i) - f23(i)
            y58(i) = f25(i) - f28(i)
            y67(i) = f26(i) - f27(i)
            y15(i) = f21(i) - f25(i)
            y26(i) = f22(i) - f26(i)
            y37(i) = f23(i) - f27(i)
            y48(i) = f24(i) - f28(i)
            z12(i) = f31(i) - f32(i)
            z34(i) = f33(i) - f34(i)
            z56(i) = f35(i) - f36(i)
            z78(i) = f37(i) - f38(i)
            z14(i) = f31(i) - f34(i)
            z23(i) = f32(i) - f33(i)
            z58(i) = f35(i) - f38(i)
            z67(i) = f36(i) - f37(i)
            z15(i) = f31(i) - f35(i)
            z26(i) = f32(i) - f36(i)
            z37(i) = f33(i) - f37(i)
            z48(i) = f34(i) - f38(i)
            xic1(i) = 0.0
            xic2(i) = 0.0
            xic3(i) = 0.0
            yic1(i) = 0.0
            yic2(i) = 0.0
            yic3(i) = 0.0
            zic1(i) = 0.0
            zic2(i) = 0.0
            zic3(i) = 0.0
        Next
        '#End Region    'YC? 092018


        '#Region "define gradient matrix With respect To reference coordinates"     'YC? 092018

        Call stric1()

        Dim lst As Integer ' YC 102418

        'For lst = 0 To 7    ' YC 102418
        For lst = 1 To 8
            ipt = lst
            Call stric2(vlinc, ipt, lst)

            '      do 45 i=lft,llt
            For i = lft To llt
                If mtype(i) <> 56 Then
                    rov(i) = ro(i) * vlinc(i)
                    ym(1, i) = ym(1, i) + rov(i) * h8(1, lst)
                    ym(2, i) = ym(2, i) + rov(i) * h8(2, lst)
                    ym(3, i) = ym(3, i) + rov(i) * h8(3, lst)
                    ym(4, i) = ym(4, i) + rov(i) * h8(4, lst)
                    ym(5, i) = ym(5, i) + rov(i) * h8(5, lst)
                    ym(6, i) = ym(6, i) + rov(i) * h8(6, lst)
                    ym(7, i) = ym(7, i) + rov(i) * h8(7, lst)
                    ym(8, i) = ym(8, i) + rov(i) * h8(8, lst)
                End If
            Next

            '#Region "compute gradient of displacement over step"  'YC? 092018
            For i = lft To llt
                di1(i, ipt) = px1(i) * fx1(i) + px2(i) * fx2(i) + px3(i) * fx3(i) + px4(i) * fx4(i) +
                    px5(i) * fx5(i) + px6(i) * fx6(i) + px7(i) * fx7(i) + px8(i) * fx8(i)
                di2(i, ipt) = py1(i) * fy1(i) + py2(i) * fy2(i) + py3(i) * fy3(i) + py4(i) * fy4(i) +
                    py5(i) * fy5(i) + py6(i) * fy6(i) + py7(i) * fy7(i) + py8(i) * fy8(i)
                di3(i, ipt) = pz1(i) * fz1(i) + pz2(i) * fz2(i) + pz3(i) * fz3(i) + pz4(i) * fz4(i) +
                    pz5(i) * fz5(i) + pz6(i) * fz6(i) + pz7(i) * fz7(i) + pz8(i) * fz8(i)
                di4(i, ipt) = py1(i) * fx1(i) + py2(i) * fx2(i) + py3(i) * fx3(i) + py4(i) * fx4(i) +
                    py5(i) * fx5(i) + py6(i) * fx6(i) + py7(i) * fx7(i) + py8(i) * fx8(i)
                di5(i, ipt) = pz1(i) * fx1(i) + pz2(i) * fx2(i) + pz3(i) * fx3(i) + pz4(i) * fx4(i) +
                    pz5(i) * fx5(i) + pz6(i) * fx6(i) + pz7(i) * fx7(i) + pz8(i) * fx8(i)
                di6(i, ipt) = px1(i) * fy1(i) + px2(i) * fy2(i) + px3(i) * fy3(i) + px4(i) * fy4(i) +
                    px5(i) * fy5(i) + px6(i) * fy6(i) + px7(i) * fy7(i) + px8(i) * fy8(i)
                di7(i, ipt) = pz1(i) * fy1(i) + pz2(i) * fy2(i) + pz3(i) * fy3(i) + pz4(i) * fy4(i) +
                    pz5(i) * fy5(i) + pz6(i) * fy6(i) + pz7(i) * fy7(i) + pz8(i) * fy8(i)
                di8(i, ipt) = px1(i) * fz1(i) + px2(i) * fz2(i) + px3(i) * fz3(i) + px4(i) * fz4(i) +
                    px5(i) * fz5(i) + px6(i) * fz6(i) + px7(i) * fz7(i) + px8(i) * fz8(i)
                di9(i, ipt) = py1(i) * fz1(i) + py2(i) * fz2(i) + py3(i) * fz3(i) + py4(i) * fz4(i) +
                    py5(i) * fz5(i) + py6(i) * fz6(i) + py7(i) * fz7(i) + py8(i) * fz8(i)
            Next

            For i = lft To llt
                di1(i, ipt) = di1(i, ipt) + pxic1(i) * fxic1(i) + pxic2(i) * fxic2(i) + pxic3(i) * fxic3(i)
                di2(i, ipt) = di2(i, ipt) + pyic1(i) * fyic1(i) + pyic2(i) * fyic2(i) + pyic3(i) * fyic3(i)
                di3(i, ipt) = di3(i, ipt) + pzic1(i) * fzic1(i) + pzic2(i) * fzic2(i) + pzic3(i) * fzic3(i)
                di4(i, ipt) = di4(i, ipt) + pyic1(i) * fxic1(i) + pyic2(i) * fxic2(i) + pyic3(i) * fxic3(i)
                di5(i, ipt) = di5(i, ipt) + pzic1(i) * fxic1(i) + pzic2(i) * fxic2(i) + pzic3(i) * fxic3(i)
                di6(i, ipt) = di6(i, ipt) + pxic1(i) * fyic1(i) + pxic2(i) * fyic2(i) + pxic3(i) * fyic3(i)
                di7(i, ipt) = di7(i, ipt) + pzic1(i) * fyic1(i) + pzic2(i) * fyic2(i) + pzic3(i) * fyic3(i)
                di8(i, ipt) = di8(i, ipt) + pxic1(i) * fzic1(i) + pxic2(i) * fzic2(i) + pxic3(i) * fzic3(i)
                di9(i, ipt) = di9(i, ipt) + pyic1(i) * fzic1(i) + pyic2(i) * fzic2(i) + pyic3(i) * fzic3(i)
            Next
            '#End Region    'YC? 092018


            '#Region "compute gradient of geometry at step n+1"  'YC? 092018
            For i = lft To llt
                dt1(i, ipt) = px1(i) * g11(i) + px2(i) * g12(i) + px3(i) * g13(i) + px4(i) * g14(i) +
                    px5(i) * g15(i) + px6(i) * g16(i) + px7(i) * g17(i) + px8(i) * g18(i)
                dt2(i, ipt) = py1(i) * g21(i) + py2(i) * g22(i) + py3(i) * g23(i) + py4(i) * g24(i) +
                    py5(i) * g25(i) + py6(i) * g26(i) + py7(i) * g27(i) + py8(i) * g28(i)
                dt3(i, ipt) = pz1(i) * g31(i) + pz2(i) * g32(i) + pz3(i) * g33(i) + pz4(i) * g34(i) +
                    pz5(i) * g35(i) + pz6(i) * g36(i) + pz7(i) * g37(i) + pz8(i) * g38(i)
                dt4(i, ipt) = py1(i) * g11(i) + py2(i) * g12(i) + py3(i) * g13(i) + py4(i) * g14(i) +
                    py5(i) * g15(i) + py6(i) * g16(i) + py7(i) * g17(i) + py8(i) * g18(i)
                dt5(i, ipt) = pz1(i) * g11(i) + pz2(i) * g12(i) + pz3(i) * g13(i) + pz4(i) * g14(i) +
                    pz5(i) * g15(i) + pz6(i) * g16(i) + pz7(i) * g17(i) + pz8(i) * g18(i)
                dt6(i, ipt) = px1(i) * g21(i) + px2(i) * g22(i) + px3(i) * g23(i) + px4(i) * g24(i) +
                    px5(i) * g25(i) + px6(i) * g26(i) + px7(i) * g27(i) + px8(i) * g28(i)
                dt7(i, ipt) = pz1(i) * g21(i) + pz2(i) * g22(i) + pz3(i) * g23(i) + pz4(i) * g24(i) +
                    pz5(i) * g25(i) + pz6(i) * g26(i) + pz7(i) * g27(i) + pz8(i) * g28(i)
                dt8(i, ipt) = px1(i) * g31(i) + px2(i) * g32(i) + px3(i) * g33(i) + px4(i) * g34(i) +
                    px5(i) * g35(i) + px6(i) * g36(i) + px7(i) * g37(i) + px8(i) * g38(i)
                dt9(i, ipt) = py1(i) * g31(i) + py2(i) * g32(i) + py3(i) * g33(i) + py4(i) * g34(i) +
                    py5(i) * g35(i) + py6(i) * g36(i) + py7(i) * g37(i) + py8(i) * g38(i)
            Next

            For i = lft To llt
                dt1(i, ipt) = dt1(i, ipt) + pxic1(i) * exic1(i) + pxic2(i) * exic2(i) + pxic3(i) * exic3(i)
                dt2(i, ipt) = dt2(i, ipt) + pyic1(i) * eyic1(i) + pyic2(i) * eyic2(i) + pyic3(i) * eyic3(i)
                dt3(i, ipt) = dt3(i, ipt) + pzic1(i) * ezic1(i) + pzic2(i) * ezic2(i) + pzic3(i) * ezic3(i)
                dt4(i, ipt) = dt4(i, ipt) + pyic1(i) * exic1(i) + pyic2(i) * exic2(i) + pyic3(i) * exic3(i)
                dt5(i, ipt) = dt5(i, ipt) + pzic1(i) * exic1(i) + pzic2(i) * exic2(i) + pzic3(i) * exic3(i)
                dt6(i, ipt) = dt6(i, ipt) + pxic1(i) * eyic1(i) + pxic2(i) * eyic2(i) + pxic3(i) * eyic3(i)
                dt7(i, ipt) = dt7(i, ipt) + pzic1(i) * eyic1(i) + pzic2(i) * eyic2(i) + pzic3(i) * eyic3(i)
                dt8(i, ipt) = dt8(i, ipt) + pxic1(i) * ezic1(i) + pxic2(i) * ezic2(i) + pxic3(i) * ezic3(i)
                dt9(i, ipt) = dt9(i, ipt) + pyic1(i) * ezic1(i) + pyic2(i) * ezic2(i) + pyic3(i) * ezic3(i)
            Next
            '#End Region        'YC? 092018

            '#Region "compute gradient of geometry at step n"       'YC? 092018
            For i = lft To llt
                dfavg(i) = dfavg(i) + vlinc(i) * (dt1(i, ipt) * (dt2(i, ipt) * dt3(i, ipt) - dt7(i, ipt) * dt9(i, ipt)) +
                            dt6(i, ipt) * (dt5(i, ipt) * dt9(i, ipt) - dt4(i, ipt) * dt3(i, ipt)) +
                            dt8(i, ipt) * (dt4(i, ipt) * dt7(i, ipt) - dt5(i, ipt) * dt2(i, ipt)))
                dt1(i, ipt) = dt1(i, ipt) - di1(i, ipt)
                dt2(i, ipt) = dt2(i, ipt) - di2(i, ipt)
                dt3(i, ipt) = dt3(i, ipt) - di3(i, ipt)
                dt4(i, ipt) = dt4(i, ipt) - di4(i, ipt)
                dt5(i, ipt) = dt5(i, ipt) - di5(i, ipt)
                dt6(i, ipt) = dt6(i, ipt) - di6(i, ipt)
                dt7(i, ipt) = dt7(i, ipt) - di7(i, ipt)
                dt8(i, ipt) = dt8(i, ipt) - di8(i, ipt)
                dt9(i, ipt) = dt9(i, ipt) - di9(i, ipt)
                volnd(i) = volnd(i) + vlinc(i)
            Next
            '#End Region        'YC? 092018

        Next lst 'lst added by YC 102418
        '#End Region        'YC? 092018


        '#Region "coordinates at n+1/2"         'YC? 092018
        For i = lft To llt
            x12(i) = h11(i) - h12(i)
            x34(i) = h13(i) - h14(i)
            x56(i) = h15(i) - h16(i)
            x78(i) = h17(i) - h18(i)
            x14(i) = h11(i) - h14(i)
            x23(i) = h12(i) - h13(i)
            x58(i) = h15(i) - h18(i)
            x67(i) = h16(i) - h17(i)
            x15(i) = h11(i) - h15(i)
            x26(i) = h12(i) - h16(i)
            x37(i) = h13(i) - h17(i)
            x48(i) = h14(i) - h18(i)
            y12(i) = h21(i) - h22(i)
            y34(i) = h23(i) - h24(i)
            y56(i) = h25(i) - h26(i)
            y78(i) = h27(i) - h28(i)
            y14(i) = h21(i) - h24(i)
            y23(i) = h22(i) - h23(i)
            y58(i) = h25(i) - h28(i)
            y67(i) = h26(i) - h27(i)
            y15(i) = h21(i) - h25(i)
            y26(i) = h22(i) - h26(i)
            y37(i) = h23(i) - h27(i)
            y48(i) = h24(i) - h28(i)
            z12(i) = h31(i) - h32(i)
            z34(i) = h33(i) - h34(i)
            z56(i) = h35(i) - h36(i)
            z78(i) = h37(i) - h38(i)
            z14(i) = h31(i) - h34(i)
            z23(i) = h32(i) - h33(i)
            z58(i) = h35(i) - h38(i)
            z67(i) = h36(i) - h37(i)
            z15(i) = h31(i) - h35(i)
            z26(i) = h32(i) - h36(i)
            z37(i) = h33(i) - h37(i)
            z48(i) = h34(i) - h38(i)
            xic1(i) = hic11(i)
            xic2(i) = hic12(i)
            xic3(i) = hic13(i)
            yic1(i) = hic21(i)
            yic2(i) = hic22(i)
            yic3(i) = hic23(i)
            zic1(i) = hic31(i)
            zic2(i) = hic32(i)
            zic3(i) = hic33(i)
        Next
        '#End Region        'YC? 092018


        '#Region "define gradient matrix with respect to geometry at n+1/2"         'YC? 092018

        Call stric1()

        'For lst = 0 To 7   ' YC 102418
        For lst = 1 To 8    ' QW 12-12-2018-
            ipt = lst
            Call stric2(vlinc, ipt, lst)

            'c.... compute gradient of displacement over step

            For i = lft To llt
                dd1(i, ipt) = px1(i) * fx1(i) + px2(i) * fx2(i) + px3(i) * fx3(i) + px4(i) * fx4(i) +
                    px5(i) * fx5(i) + px6(i) * fx6(i) + px7(i) * fx7(i) + px8(i) * fx8(i)
                dd2(i, ipt) = py1(i) * fy1(i) + py2(i) * fy2(i) + py3(i) * fy3(i) + py4(i) * fy4(i) +
                    py5(i) * fy5(i) + py6(i) * fy6(i) + py7(i) * fy7(i) + py8(i) * fy8(i)
                dd3(i, ipt) = pz1(i) * fz1(i) + pz2(i) * fz2(i) + pz3(i) * fz3(i) + pz4(i) * fz4(i) +
                    pz5(i) * fz5(i) + pz6(i) * fz6(i) + pz7(i) * fz7(i) + pz8(i) * fz8(i)
                dd4(i, ipt) = py1(i) * fx1(i) + py2(i) * fx2(i) + py3(i) * fx3(i) + py4(i) * fx4(i) +
                    py5(i) * fx5(i) + py6(i) * fx6(i) + py7(i) * fx7(i) + py8(i) * fx8(i)
                dd5(i, ipt) = pz1(i) * fx1(i) + pz2(i) * fx2(i) + pz3(i) * fx3(i) + pz4(i) * fx4(i) +
                    pz5(i) * fx5(i) + pz6(i) * fx6(i) + pz7(i) * fx7(i) + pz8(i) * fx8(i)
                dd6(i, ipt) = px1(i) * fy1(i) + px2(i) * fy2(i) + px3(i) * fy3(i) + px4(i) * fy4(i) +
                    px5(i) * fy5(i) + px6(i) * fy6(i) + px7(i) * fy7(i) + px8(i) * fy8(i)
                dd7(i, ipt) = pz1(i) * fy1(i) + pz2(i) * fy2(i) + pz3(i) * fy3(i) + pz4(i) * fy4(i) +
                    pz5(i) * fy5(i) + pz6(i) * fy6(i) + pz7(i) * fy7(i) + pz8(i) * fy8(i)
                dd8(i, ipt) = px1(i) * fz1(i) + px2(i) * fz2(i) + px3(i) * fz3(i) + px4(i) * fz4(i) +
                    px5(i) * fz5(i) + px6(i) * fz6(i) + px7(i) * fz7(i) + px8(i) * fz8(i)
                dd9(i, ipt) = py1(i) * fz1(i) + py2(i) * fz2(i) + py3(i) * fz3(i) + py4(i) * fz4(i) +
                    py5(i) * fz5(i) + py6(i) * fz6(i) + py7(i) * fz7(i) + py8(i) * fz8(i)
            Next

            For i = lft To llt
                dd1(i, ipt) = dd1(i, ipt) + pxic1(i) * fxic1(i) + pxic2(i) * fxic2(i) + pxic3(i) * fxic3(i)
                dd2(i, ipt) = dd2(i, ipt) + pyic1(i) * fyic1(i) + pyic2(i) * fyic2(i) + pyic3(i) * fyic3(i)
                dd3(i, ipt) = dd3(i, ipt) + pzic1(i) * fzic1(i) + pzic2(i) * fzic2(i) + pzic3(i) * fzic3(i)
                dd4(i, ipt) = dd4(i, ipt) + pyic1(i) * fxic1(i) + pyic2(i) * fxic2(i) + pyic3(i) * fxic3(i)
                dd5(i, ipt) = dd5(i, ipt) + pzic1(i) * fxic1(i) + pzic2(i) * fxic2(i) + pzic3(i) * fxic3(i)
                dd6(i, ipt) = dd6(i, ipt) + pxic1(i) * fyic1(i) + pxic2(i) * fyic2(i) + pxic3(i) * fyic3(i)
                dd7(i, ipt) = dd7(i, ipt) + pzic1(i) * fyic1(i) + pzic2(i) * fyic2(i) + pzic3(i) * fyic3(i)
                dd8(i, ipt) = dd8(i, ipt) + pxic1(i) * fzic1(i) + pxic2(i) * fzic2(i) + pxic3(i) * fzic3(i)
                dd9(i, ipt) = dd9(i, ipt) + pyic1(i) * fzic1(i) + pyic2(i) * fzic2(i) + pyic3(i) * fzic3(i)
            Next
            'Call Check1D32(pxic1, fxic1, dd1, 1, ipt, istep)
            For i = lft To llt
                diavg(i) = diavg(i) + vlinc(i) * (dd1(i, ipt) + dd2(i, ipt) + dd3(i, ipt))
                volcd(i) = volcd(i) + vlinc(i)
            Next

        Next
        '#End Region        'YC? 092018


        '#Region "coordinates at n+1"       'YC? 092018

        For i = lft To llt
            diavg(i) = diavg(i) / (volcd(i) + 1.0E-30)
            dfavg(i) = dfavg(i) / (volnd(i) + 1.0E-30)
            x12(i) = g11(i) - g12(i)
            x34(i) = g13(i) - g14(i)
            x56(i) = g15(i) - g16(i)
            x78(i) = g17(i) - g18(i)
            x14(i) = g11(i) - g14(i)
            x23(i) = g12(i) - g13(i)
            x58(i) = g15(i) - g18(i)
            x67(i) = g16(i) - g17(i)
            x15(i) = g11(i) - g15(i)
            x26(i) = g12(i) - g16(i)
            x37(i) = g13(i) - g17(i)
            x48(i) = g14(i) - g18(i)
            y12(i) = g21(i) - g22(i)
            y34(i) = g23(i) - g24(i)
            y56(i) = g25(i) - g26(i)
            y78(i) = g27(i) - g28(i)
            y14(i) = g21(i) - g24(i)
            y23(i) = g22(i) - g23(i)
            y58(i) = g25(i) - g28(i)
            y67(i) = g26(i) - g27(i)
            y15(i) = g21(i) - g25(i)
            y26(i) = g22(i) - g26(i)
            y37(i) = g23(i) - g27(i)
            y48(i) = g24(i) - g28(i)
            z12(i) = g31(i) - g32(i)
            z34(i) = g33(i) - g34(i)
            z56(i) = g35(i) - g36(i)
            z78(i) = g37(i) - g38(i)
            z14(i) = g31(i) - g34(i)
            z23(i) = g32(i) - g33(i)
            z58(i) = g35(i) - g38(i)
            z67(i) = g36(i) - g37(i)
            z15(i) = g31(i) - g35(i)
            z26(i) = g32(i) - g36(i)
            z37(i) = g33(i) - g37(i)
            z48(i) = g34(i) - g38(i)

            xic1(i) = 0.0
            xic2(i) = 0.0
            xic3(i) = 0.0
            yic1(i) = 0.0
            yic2(i) = 0.0
            yic3(i) = 0.0
            zic1(i) = 0.0
            zic2(i) = 0.0
            zic3(i) = 0.0
        Next
        '#End Region        'YC? 092018


        '#Region "define gradient matrix with respect to geometry at n+1"       'YC? 092018

        Call stric1()

        'For lst = 0 To 7       ' YC 102418
        For lst = 1 To 8
            If lst = 7 Then
                lst = lst
            End If
            ipt = lst
            Call stric2(volgp, ipt, lst)

            Dim volmin = 10000000000.0

            For i = lft To llt
                volmin = Math.Min(volmin, volgp(i))
            Next

            If volmin < 0.0 Then
                Dim sb As New StringBuilder()
                sb.AppendLine(" ************************************************************")
                sb.AppendLine(" *                     - WARNING -                          *")
                sb.AppendLine(" *     Negative hex element volume computed in STFNF        *")
                sb.AppendLine(" ************************************************************")
                Dim msg990 = sb.ToString()
                Return
            End If

            'c.... polar decomposition at step n

            For i = lft To llt
                f1(i) = dt1(i, ipt)
                f2(i) = dt2(i, ipt)
                f3(i) = dt3(i, ipt)
                f4(i) = dt4(i, ipt)
                f5(i) = dt5(i, ipt)
                f6(i) = dt6(i, ipt)
                f7(i) = dt7(i, ipt)
                f8(i) = dt8(i, ipt)
                f9(i) = dt9(i, ipt)
            Next

            Call getvrt(r11, r12, r13, r21, r22, r23, r31, r32, r33)

            'c.... polar decomposition at step n+1/2

            For i = lft To llt
                f1(i) = dt1(i, ipt) + 0.5 * di1(i, ipt)
                f2(i) = dt2(i, ipt) + 0.5 * di2(i, ipt)
                f3(i) = dt3(i, ipt) + 0.5 * di3(i, ipt)
                f4(i) = dt4(i, ipt) + 0.5 * di4(i, ipt)
                f5(i) = dt5(i, ipt) + 0.5 * di5(i, ipt)
                f6(i) = dt6(i, ipt) + 0.5 * di6(i, ipt)
                f7(i) = dt7(i, ipt) + 0.5 * di7(i, ipt)
                f8(i) = dt8(i, ipt) + 0.5 * di8(i, ipt)
                f9(i) = dt9(i, ipt) + 0.5 * di9(i, ipt)
            Next

            Call getvrt(q11, q12, q13, q21, q22, q23, q31, q32, q33)

            'c.... polar decomposition at step n+1

            For i = lft To llt
                f1(i) = dt1(i, ipt) + di1(i, ipt)
                f2(i) = dt2(i, ipt) + di2(i, ipt)
                f3(i) = dt3(i, ipt) + di3(i, ipt)
                f4(i) = dt4(i, ipt) + di4(i, ipt)
                f5(i) = dt5(i, ipt) + di5(i, ipt)
                f6(i) = dt6(i, ipt) + di6(i, ipt)
                f7(i) = dt7(i, ipt) + di7(i, ipt)
                f8(i) = dt8(i, ipt) + di8(i, ipt)
                f9(i) = dt9(i, ipt) + di9(i, ipt)
            Next

            Call getvrt(s11, s12, s13, s21, s22, s23, s31, s32, s33)

            'c.... rotate incremental deformation gradient by r at n+1/2
            'Dim dd1_a(63), dd2_a(63), dd3_a(63), dd4_a(63), dd5_a(63), dd6_a(63), dd7_a(63), dd8_a(63), dd9_a(63) As Double    ' YC 102418
            Dim dd1_a(64), dd2_a(64), dd3_a(64), dd4_a(64), dd5_a(64), dd6_a(64), dd7_a(64), dd8_a(64), dd9_a(64) As Double

            For i = lft To llt
                dd1_a(i) = dd1(i, ipt)
                dd2_a(i) = dd2(i, ipt)
                dd3_a(i) = dd3(i, ipt)
                dd4_a(i) = dd4(i, ipt)
                dd5_a(i) = dd5(i, ipt)
                dd6_a(i) = dd6(i, ipt)
                dd7_a(i) = dd7(i, ipt)
                dd8_a(i) = dd8(i, ipt)
                dd9_a(i) = dd9(i, ipt)
            Next

            Call rotstr(dd1_a, dd2_a, dd3_a, dd4_a, dd5_a, dd6_a, dd7_a, dd8_a, dd9_a)

            For i = lft To llt
                dd1(i, ipt) = dd1_a(i)
                dd2(i, ipt) = dd2_a(i)
                dd3(i, ipt) = dd3_a(i)
                dd4(i, ipt) = dd4_a(i)
                dd5(i, ipt) = dd5_a(i)
                dd6(i, ipt) = dd6_a(i)
                dd7(i, ipt) = dd7_a(i)
                dd8(i, ipt) = dd8_a(i)
                dd9(i, ipt) = dd9_a(i)
            Next

            For i = lft To llt
                d1(i) = dd1(i, ipt)
                d2(i) = dd2(i, ipt)
                d3(i) = dd3(i, ipt)
                d4(i) = dd4(i, ipt)
                d5(i) = dd5(i, ipt)
                d6(i) = dd6(i, ipt)
            Next
            If llt = 3 Then
                llt = llt

            End If
            mft = 1
            mlt = 1
            Dim llt1 = llt - 1
            nel = nftm1 + 1

            If lft = llt Then GoTo 260

            For i = lft To llt1

                'If matp(i) = matp(i + 1) Then Continue For  ' QW 12-12-2018
                If matp(i) = matp(i + 1) Then GoTo 250
                model = mtype(mft)
                nelsub = nftm1 + mft

                If model <> 0 Then
                    Call sslcs(ihv, d1, d2, d3, d4, d5, d6)
                End If

                mft = mlt + 1

                'mlt = mlt + 1  'YC 102418
250:            mlt = mlt + 1
            Next

260:        model = mtype(mft)

            lhex(1) = model         '  equivalence (lhex(1),model) YC 102418

            nelsub = nftm1 + mft

            'Call Check4(ng, lst, nel, model)

            'If nel = 5313 Then
            'Stop
            'End If
            Call sslcs(ihv, d1, d2, d3, d4, d5, d6)

            For i = lft To llt
                sig11(i) = sig11s(i) * volgp(i)
                sig22(i) = sig22s(i) * volgp(i)
                sig33(i) = sig33s(i) * volgp(i)
                sig12(i) = sig12s(i) * volgp(i)
                sig23(i) = sig23s(i) * volgp(i)
                sig31(i) = sig31s(i) * volgp(i)
            Next

            If ibar = 0 Then GoTo 300
            Dim third = -1.0 / 3.0

            For i = lft To llt
                trsg(i) = sig11(i) + sig22(i) + sig33(i)
                pxm1(i) = sclo(i) * (pxa1(i) + third * px1(i))
                pxm2(i) = sclo(i) * (pxa2(i) + third * px2(i))
                pxm3(i) = sclo(i) * (pxa3(i) + third * px3(i))
                pxm4(i) = sclo(i) * (pxa4(i) + third * px4(i))
                pxm5(i) = sclo(i) * (pxa5(i) + third * px5(i))
                pxm6(i) = sclo(i) * (pxa6(i) + third * px6(i))
                pxm7(i) = sclo(i) * (pxa7(i) + third * px7(i))
                pxm8(i) = sclo(i) * (pxa8(i) + third * px8(i))
                pym1(i) = sclo(i) * (pya1(i) + third * py1(i))
                pym2(i) = sclo(i) * (pya2(i) + third * py2(i))
                pym3(i) = sclo(i) * (pya3(i) + third * py3(i))
                pym4(i) = sclo(i) * (pya4(i) + third * py4(i))
                pym5(i) = sclo(i) * (pya5(i) + third * py5(i))
                pym6(i) = sclo(i) * (pya6(i) + third * py6(i))
                pym7(i) = sclo(i) * (pya7(i) + third * py7(i))
                pym8(i) = sclo(i) * (pya8(i) + third * py8(i))
                pzm1(i) = sclo(i) * (pza1(i) + third * pz1(i))
                pzm2(i) = sclo(i) * (pza2(i) + third * pz2(i))
                pzm3(i) = sclo(i) * (pza3(i) + third * pz3(i))
                pzm4(i) = sclo(i) * (pza4(i) + third * pz4(i))
                pzm5(i) = sclo(i) * (pza5(i) + third * pz5(i))
                pzm6(i) = sclo(i) * (pza6(i) + third * pz6(i))
                pzm7(i) = sclo(i) * (pza7(i) + third * pz7(i))
                pzm8(i) = sclo(i) * (pza8(i) + third * pz8(i))
            Next

            For i = lft To llt
                rx1(i) = rx1(i) + pxm1(i) * trsg(i)
                ry1(i) = ry1(i) + pym1(i) * trsg(i)
                rz1(i) = rz1(i) + pzm1(i) * trsg(i)
                rx2(i) = rx2(i) + pxm2(i) * trsg(i)
                ry2(i) = ry2(i) + pym2(i) * trsg(i)
                rz2(i) = rz2(i) + pzm2(i) * trsg(i)
                rx3(i) = rx3(i) + pxm3(i) * trsg(i)
                ry3(i) = ry3(i) + pym3(i) * trsg(i)
                rz3(i) = rz3(i) + pzm3(i) * trsg(i)
                rx4(i) = rx4(i) + pxm4(i) * trsg(i)
                ry4(i) = ry4(i) + pym4(i) * trsg(i)
                rz4(i) = rz4(i) + pzm4(i) * trsg(i)
                rx5(i) = rx5(i) + pxm5(i) * trsg(i)
                ry5(i) = ry5(i) + pym5(i) * trsg(i)
                rz5(i) = rz5(i) + pzm5(i) * trsg(i)
                rx6(i) = rx6(i) + pxm6(i) * trsg(i)
                ry6(i) = ry6(i) + pym6(i) * trsg(i)
                rz6(i) = rz6(i) + pzm6(i) * trsg(i)
                rx7(i) = rx7(i) + pxm7(i) * trsg(i)
                ry7(i) = ry7(i) + pym7(i) * trsg(i)
                rz7(i) = rz7(i) + pzm7(i) * trsg(i)
                rx8(i) = rx8(i) + pxm8(i) * trsg(i)
                ry8(i) = ry8(i) + pym8(i) * trsg(i)
                rz8(i) = rz8(i) + pzm8(i) * trsg(i)
            Next

            'c.... construct internal force
300:
            For i = lft To llt
                rx1(i) = rx1(i) + px1(i) * sig11(i) + py1(i) * sig12(i) + pz1(i) * sig31(i)
                ry1(i) = ry1(i) + py1(i) * sig22(i) + px1(i) * sig12(i) + pz1(i) * sig23(i)
                rz1(i) = rz1(i) + pz1(i) * sig33(i) + px1(i) * sig31(i) + py1(i) * sig23(i)
                rx2(i) = rx2(i) + px2(i) * sig11(i) + py2(i) * sig12(i) + pz2(i) * sig31(i)
                ry2(i) = ry2(i) + py2(i) * sig22(i) + px2(i) * sig12(i) + pz2(i) * sig23(i)
                rz2(i) = rz2(i) + pz2(i) * sig33(i) + px2(i) * sig31(i) + py2(i) * sig23(i)
                rx3(i) = rx3(i) + px3(i) * sig11(i) + py3(i) * sig12(i) + pz3(i) * sig31(i)
                ry3(i) = ry3(i) + py3(i) * sig22(i) + px3(i) * sig12(i) + pz3(i) * sig23(i)
                rz3(i) = rz3(i) + pz3(i) * sig33(i) + px3(i) * sig31(i) + py3(i) * sig23(i)
                rx4(i) = rx4(i) + px4(i) * sig11(i) + py4(i) * sig12(i) + pz4(i) * sig31(i)
                ry4(i) = ry4(i) + py4(i) * sig22(i) + px4(i) * sig12(i) + pz4(i) * sig23(i)
                rz4(i) = rz4(i) + pz4(i) * sig33(i) + px4(i) * sig31(i) + py4(i) * sig23(i)
                rx5(i) = rx5(i) + px5(i) * sig11(i) + py5(i) * sig12(i) + pz5(i) * sig31(i)
                ry5(i) = ry5(i) + py5(i) * sig22(i) + px5(i) * sig12(i) + pz5(i) * sig23(i)
                rz5(i) = rz5(i) + pz5(i) * sig33(i) + px5(i) * sig31(i) + py5(i) * sig23(i)
                rx6(i) = rx6(i) + px6(i) * sig11(i) + py6(i) * sig12(i) + pz6(i) * sig31(i)
                ry6(i) = ry6(i) + py6(i) * sig22(i) + px6(i) * sig12(i) + pz6(i) * sig23(i)
                rz6(i) = rz6(i) + pz6(i) * sig33(i) + px6(i) * sig31(i) + py6(i) * sig23(i)
                rx7(i) = rx7(i) + px7(i) * sig11(i) + py7(i) * sig12(i) + pz7(i) * sig31(i)
                ry7(i) = ry7(i) + py7(i) * sig22(i) + px7(i) * sig12(i) + pz7(i) * sig23(i)
                rz7(i) = rz7(i) + pz7(i) * sig33(i) + px7(i) * sig31(i) + py7(i) * sig23(i)
                rx8(i) = rx8(i) + px8(i) * sig11(i) + py8(i) * sig12(i) + pz8(i) * sig31(i)
                ry8(i) = ry8(i) + py8(i) * sig22(i) + px8(i) * sig12(i) + pz8(i) * sig23(i)
                rz8(i) = rz8(i) + pz8(i) * sig33(i) + px8(i) * sig31(i) + py8(i) * sig23(i)

                'If ng = 1 And (lst = 1 Or lst = 2) And i = 1 Then
                'Call Check3(rx1, px1, sig11, 1)
                'End If


            Next

            For i = lft To llt
                rxic1(i) = rxic1(i) + pxic1(i) * sig11(i) + pyic1(i) * sig12(i) + pzic1(i) * sig31(i)
                ryic1(i) = ryic1(i) + pyic1(i) * sig22(i) + pxic1(i) * sig12(i) + pzic1(i) * sig23(i)
                rzic1(i) = rzic1(i) + pzic1(i) * sig33(i) + pxic1(i) * sig31(i) + pyic1(i) * sig23(i)
                rxic2(i) = rxic2(i) + pxic2(i) * sig11(i) + pyic2(i) * sig12(i) + pzic2(i) * sig31(i)
                ryic2(i) = ryic2(i) + pyic2(i) * sig22(i) + pxic2(i) * sig12(i) + pzic2(i) * sig23(i)
                rzic2(i) = rzic2(i) + pzic2(i) * sig33(i) + pxic2(i) * sig31(i) + pyic2(i) * sig23(i)
                rxic3(i) = rxic3(i) + pxic3(i) * sig11(i) + pyic3(i) * sig12(i) + pzic3(i) * sig31(i)
                ryic3(i) = ryic3(i) + pyic3(i) * sig22(i) + pxic3(i) * sig12(i) + pzic3(i) * sig23(i)
                rzic3(i) = rzic3(i) + pzic3(i) * sig33(i) + pxic3(i) * sig31(i) + pyic3(i) * sig23(i)
            Next
            'Call Check1D3(rx1, px1, sig11, 1, istep)
            If numdc = 0 Then GoTo 320
            If iphase = 2 Then GoTo 340


320:        If iphase - 2 > 0 Then Continue For
330:        If iref <> 0 Then Continue For

            'c.... compute linear stiffness matrix
340:
            ' QW 12-12-2018
            For i = 1 To 6
                For j = 1 To 6
                    For k = lft To llt
                        dsave(i, j, k) = dsave(i, j, k) * volgp(k)
                    Next
                    For k = 1 To 64
                        dsave2((j - 1) * 6 + i, k) = dsave(i, j, k)
                    Next
                Next
            Next '
            '
            ' Call Check2DT(dsave2, 36, 3)
            'Call Check1D3(rx1, px1, sig11, 1)
            'For j = 1 To 36
            'For i = lft To llt
            'dsave2(j, i) = dsave2(j, i) * volgp(i)
            'Next
            'Next 
            '
            ' change pxm1 to pxm1,,,pzm8 by YC 092018
            'If ibar = 0 Then Call azero(pxm1, 1536)  '1536=24*64
            If ibar = 0 Then
                Call azero(pxm1, 64)
                Call azero(pxm2, 64)
                Call azero(pxm3, 64)
                Call azero(pxm4, 64)
                Call azero(pxm5, 64)
                Call azero(pxm6, 64)
                Call azero(pxm7, 64)
                Call azero(pxm8, 64)
                Call azero(pym1, 64)
                Call azero(pym2, 64)
                Call azero(pym3, 64)
                Call azero(pym4, 64)
                Call azero(pym5, 64)
                Call azero(pym6, 64)
                Call azero(pym7, 64)
                Call azero(pym8, 64)
                Call azero(pzm1, 64)
                Call azero(pzm2, 64)
                Call azero(pzm3, 64)
                Call azero(pzm4, 64)
                Call azero(pzm5, 64)
                Call azero(pzm6, 64)
                Call azero(pzm7, 64)
                Call azero(pzm8, 64)
            End If
            ' change pxm1 to pxm1,,,pzm8 by YC 092018 END

            ' YC 092018
            'Call btdb(s(25, 1), dsave2(1, 1))
            'Call bdbic1(px1, pxic1, dsave2, ekic, nmel)
            'Call bdbic2(pxic1, dsave2, ekic, nmel)

            'Dim d_3D(6, 6, 64) As Double
            'Call objComsub.ArrayConvert2Dto3D(dsave2, 36, 64, d_3D, 6, 6, 64)

            Call btdb(s, dsave, ng)

            Dim ps_1D(nmel * 8 * 3), pi_1D(nmel * 3 * 3) As Double
            Dim ps(nmel, 8, 3), pi(nmel, 3, 3) As Double
            objComsub.ArrayInsert1Dto1D(px1, nmel, ps_1D, nmel * (8 * 0 + 0))
            objComsub.ArrayInsert1Dto1D(px2, nmel, ps_1D, nmel * (8 * 0 + 1))
            objComsub.ArrayInsert1Dto1D(px3, nmel, ps_1D, nmel * (8 * 0 + 2))
            objComsub.ArrayInsert1Dto1D(px4, nmel, ps_1D, nmel * (8 * 0 + 3))
            objComsub.ArrayInsert1Dto1D(px5, nmel, ps_1D, nmel * (8 * 0 + 4))
            objComsub.ArrayInsert1Dto1D(px6, nmel, ps_1D, nmel * (8 * 0 + 5))
            objComsub.ArrayInsert1Dto1D(px7, nmel, ps_1D, nmel * (8 * 0 + 6))
            objComsub.ArrayInsert1Dto1D(px8, nmel, ps_1D, nmel * (8 * 0 + 7))
            objComsub.ArrayInsert1Dto1D(py1, nmel, ps_1D, nmel * (8 * 1 + 0))
            objComsub.ArrayInsert1Dto1D(py2, nmel, ps_1D, nmel * (8 * 1 + 1))
            objComsub.ArrayInsert1Dto1D(py3, nmel, ps_1D, nmel * (8 * 1 + 2))
            objComsub.ArrayInsert1Dto1D(py4, nmel, ps_1D, nmel * (8 * 1 + 3))
            objComsub.ArrayInsert1Dto1D(py5, nmel, ps_1D, nmel * (8 * 1 + 4))
            objComsub.ArrayInsert1Dto1D(py6, nmel, ps_1D, nmel * (8 * 1 + 5))
            objComsub.ArrayInsert1Dto1D(py7, nmel, ps_1D, nmel * (8 * 1 + 6))
            objComsub.ArrayInsert1Dto1D(py8, nmel, ps_1D, nmel * (8 * 1 + 7))
            objComsub.ArrayInsert1Dto1D(pz1, nmel, ps_1D, nmel * (8 * 2 + 0))
            objComsub.ArrayInsert1Dto1D(pz2, nmel, ps_1D, nmel * (8 * 2 + 1))
            objComsub.ArrayInsert1Dto1D(pz3, nmel, ps_1D, nmel * (8 * 2 + 2))
            objComsub.ArrayInsert1Dto1D(pz4, nmel, ps_1D, nmel * (8 * 2 + 3))
            objComsub.ArrayInsert1Dto1D(pz5, nmel, ps_1D, nmel * (8 * 2 + 4))
            objComsub.ArrayInsert1Dto1D(pz6, nmel, ps_1D, nmel * (8 * 2 + 5))
            objComsub.ArrayInsert1Dto1D(pz7, nmel, ps_1D, nmel * (8 * 2 + 6))
            objComsub.ArrayInsert1Dto1D(pz8, nmel, ps_1D, nmel * (8 * 2 + 7))
            objComsub.ArrayConvert1Dto3D(ps_1D, ps, nmel, 8, 3)

            objComsub.ArrayInsert1Dto1D(pxic1, nmel, pi_1D, nmel * (3 * 0 + 0))
            objComsub.ArrayInsert1Dto1D(pxic2, nmel, pi_1D, nmel * (3 * 0 + 1))
            objComsub.ArrayInsert1Dto1D(pxic3, nmel, pi_1D, nmel * (3 * 0 + 2))
            objComsub.ArrayInsert1Dto1D(pyic1, nmel, pi_1D, nmel * (3 * 1 + 0))
            objComsub.ArrayInsert1Dto1D(pyic2, nmel, pi_1D, nmel * (3 * 1 + 1))
            objComsub.ArrayInsert1Dto1D(pyic3, nmel, pi_1D, nmel * (3 * 1 + 2))
            objComsub.ArrayInsert1Dto1D(pzic1, nmel, pi_1D, nmel * (3 * 2 + 0))
            objComsub.ArrayInsert1Dto1D(pzic2, nmel, pi_1D, nmel * (3 * 2 + 1))
            objComsub.ArrayInsert1Dto1D(pzic3, nmel, pi_1D, nmel * (3 * 2 + 2))
            objComsub.ArrayConvert1Dto3D(pi_1D, pi, nmel, 3, 3)

            Call bdbic1(ps, pi, dsave, ekic, nmel)

            Call bdbic2(pi, dsave, ekic, nmel)

            'YC? 092018 END

            'Next       'YC 102418
410:    Next lst

        '#End Region        'YC? 092018


    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine stfic(nmel,*)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to drive incompatible hex residual/stiffness formation
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk10/iphase,nelgp,imass,lhex(9)
'
'      
'      common/bk20/kpri,nstep,ite,ilimit,iref
'      common/bk31/s(324,1)
'      common/bk32/h(8,9),pr(8,9),ps(8,9),pt(8,9)
'      common/bk35/numdc,numudc,nrcc
'      common/bk56/c(6,6),ipt,nel,nelsub
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect0/
'     1 q11(64),q12(64),q13(64),q21(64),q22(64),q23(64),
'     2 q31(64),q32(64),q33(64),r11(64),r12(64),r13(64),
'     3 r21(64),r22(64),r23(64),r31(64),r32(64),r33(64),
'     4 s11(64),s12(64),s13(64),s21(64),s22(64),s23(64),
'     5 s31(64),s32(64),s33(64)
'      common/vect1/
'     1 rx1(64),ry1(64),rz1(64),rx2(64),ry2(64),rz2(64),
'     2 rx3(64),ry3(64),rz3(64),rx4(64),ry4(64),rz4(64),
'     3 rx5(64),ry5(64),rz5(64),rx6(64),ry6(64),rz6(64),
'     4 rx7(64),ry7(64),rz7(64),rx8(64),ry8(64),rz8(64),
'     5 mtype(64),matp(64)
'      common/vect2/
'     1 ex1(64),ey1(64),ez1(64),ex2(64),ey2(64),ez2(64),
'     2 ex3(64),ey3(64),ez3(64),ex4(64),ey4(64),ez4(64),
'     3 ex5(64),ey5(64),ez5(64),ex6(64),ey6(64),ez6(64),
'     4 ex7(64),ey7(64),ez7(64),ex8(64),ey8(64),ez8(64),
'     5 fx1(64),fy1(64),fz1(64),fx2(64),fy2(64),fz2(64),
'     6 fx3(64),fy3(64),fz3(64),fx4(64),fy4(64),fz4(64),
'     7 fx5(64),fy5(64),fz5(64),fx6(64),fy6(64),fz6(64),
'     8 fx7(64),fy7(64),fz7(64),fx8(64),fy8(64),fz8(64)
'      common/vect3/
'     1 sig11s(64),sig22s(64),sig33s(64),sig12s(64),sig23s(64),
'     2 sig31s(64),dd1(64,8),dd2(64,8),dd3(64,8),dd4(64,8),
'     3 dd5(64,8),dd6(64,8),dd7(64,8),dd8(64,8),dd9(64,8),
'     4 di1(64,8),di2(64,8),di3(64,8),di4(64,8),di5(64,8),
'     5 di6(64,8),di7(64,8),di8(64,8),di9(64,8),dt1(64,8),
'     6 dt2(64,8),dt3(64,8),dt4(64,8),dt5(64,8),dt6(64,8),
'     7 dt7(64,8),dt8(64,8),dt9(64,8)
'      common/vect4/
'     1 px1(64),px2(64),px3(64),px4(64),
'     2 px5(64),px6(64),px7(64),px8(64),
'     3 py1(64),py2(64),py3(64),py4(64),
'     4 py5(64),py6(64),py7(64),py8(64),
'     5 pz1(64),pz2(64),pz3(64),pz4(64),
'     6 pz5(64),pz6(64),pz7(64),pz8(64)
'      common/vect5/
'     1 f1(64),f2(64),f3(64),f4(64),f5(64),f6(64),
'     2 f7(64),f8(64),f9(64),wxx(64),wyy(64),wzz(64),deti(64),dum5(2880)
'      common/vect6/
'     1 g11(64),g21(64),g31(64),g12(64),g22(64),g32(64),
'     2 g13(64),g23(64),g33(64),g14(64),g24(64),g34(64),
'     3 g15(64),g25(64),g35(64),g16(64),g26(64),g36(64),
'     4 g17(64),g27(64),g37(64),g18(64),g28(64),g38(64),
'     5 h11(64),h21(64),h31(64),h12(64),h22(64),h32(64),
'     6 h13(64),h23(64),h33(64),h14(64),h24(64),h34(64),
'     7 h15(64),h25(64),h35(64),h16(64),h26(64),h36(64),
'     8 h17(64),h27(64),h37(64),h18(64),h28(64),h38(64),
'     9 f11(64),f21(64),f31(64),f12(64),f22(64),f32(64),
'     & f13(64),f23(64),f33(64),f14(64),f24(64),f34(64),
'     & f15(64),f25(64),f35(64),f16(64),f26(64),f36(64),
'     & f17(64),f27(64),f37(64),f18(64),f28(64),f38(64)
'      common/vect7/
'     1 diavg(64),dfavg(64),volnd(64),volcd(64),vlinc(64),volgp(64),
'     2 dimod(64),sig11(64),sig22(64),sig33(64),sig12(64),sig23(64),
'     3 sig31(64)
'      common/vect8/
'     1 d(36,64)
'      common/vect9/scale(24,64),ym(8,64),ro(64),rov(64),tmecc(64)
'      common/vect13/
'     1 x12(64),x34(64),x56(64),x78(64),y12(64),y34(64),
'     2 y56(64),y78(64),z12(64),z34(64),z56(64),z78(64),
'     3 x14(64),x23(64),x58(64),x67(64),y14(64),y23(64),
'     4 y58(64),y67(64),z14(64),z23(64),z58(64),z67(64),
'     6 x15(64),x26(64),x37(64),x48(64),y15(64),y26(64),
'     7 y37(64),y48(64),z15(64),z26(64),z37(64),z48(64),dum13(1536)
'      common/vect15/
'     1 d1(64),d2(64),d3(64),d4(64),d5(64),d6(64),dum15(384)
'      common/vect17/betap(64),dum17(192,64)
'      common/vect18/sclo(64),dum18(512)
'      common/vect19/
'     1 svvl(64,8),volume(64),px(64,8,8),py(64,8,8),pz(64,8,8),
'     2 pxa1(64),pxa2(64),pxa3(64),pxa4(64),pxa5(64),pxa6(64),
'     3 pxa7(64),pxa8(64),pya1(64),pya2(64),pya3(64),pya4(64),
'     4 pya5(64),pya6(64),pya7(64),pya8(64),pza1(64),pza2(64),
'     5 pza3(64),pza4(64),pza5(64),pza6(64),pza7(64),pza8(64),
'     6 pxm1(64),pxm2(64),pxm3(64),pxm4(64),pxm5(64),pxm6(64),
'     7 pxm7(64),pxm8(64),pym1(64),pym2(64),pym3(64),pym4(64),
'     8 pym5(64),pym6(64),pym7(64),pym8(64),pzm1(64),pzm2(64),
'     9 pzm3(64),pzm4(64),pzm5(64),pzm6(64),pzm7(64),pzm8(64),trsg(64),
'     & dum19(384)
'      common/vect52/
'     1 xic1(64),xic2(64),xic3(64),yic1(64),yic2(64),yic3(64),
'     2 zic1(64),zic2(64),zic3(64),pxic1(64),pxic2(64),pxic3(64),
'     3 pyic1(64),pyic2(64),pyic3(64),pzic1(64),pzic2(64),pzic3(64)
'      common/vect54/ekic(33,9,64)
'      common/vect55/
'     1 hic11(64),hic21(64),hic31(64),hic12(64),hic22(64),hic32(64),
'     2 hic13(64),hic23(64),hic33(64),exic1(64),eyic1(64),ezic1(64),
'     3 exic2(64),eyic2(64),ezic2(64),exic3(64),eyic3(64),ezic3(64),
'     4 fxic1(64),fyic1(64),fzic1(64),fxic2(64),fyic2(64),fzic2(64),
'     5 fxic3(64),fyic3(64),fzic3(64),rxic1(64),ryic1(64),rzic1(64),
'     6 rxic2(64),ryic2(64),rzic2(64),rxic3(64),ryic3(64),rzic3(64)
'      
'      common/histvar/iihv
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh
'      
'      common/in3/ia(81000),ihv(9001),numelg(2)              ! QW 11-11-2015
'      common/aa/anegb1(7508928),anegb2(7508928)
'c      
'c      
'      equivalence (lhex(1),model)
'
'	common/fissle_nasa_ichk0/ibar
'
'c
'      call azero(pxa1,1536)
'      call azero(volume,64)
'c
'c.... geometry at n+1
'c
'c     note: the flags in columns 73-80 denote either a geometrically
'c           linear formulation ("linear","lininc") or a geometrically
'c           nonlinear formulation ("nonlin","noninc")
'c
'      do 20 i=lft,llt
'
'      g11(i)=f11(i)                                             
'      g21(i)=f21(i)                                             
'      g31(i)=f31(i)                                             
'      g12(i)=f12(i)                                             
'      g22(i)=f22(i)                                             
'      g32(i)=f32(i)                                             
'      g13(i)=f13(i)                                             
'      g23(i)=f23(i)                                             
'      g33(i)=f33(i)                                             
'      g14(i)=f14(i)                                             
'      g24(i)=f24(i)                                             
'      g34(i)=f34(i)                                             
'      g15(i)=f15(i)                                             
'      g25(i)=f25(i)                                             
'      g35(i)=f35(i)                                             
'      g16(i)=f16(i)                                             
'      g26(i)=f26(i)                                             
'      g36(i)=f36(i)                                             
'      g17(i)=f17(i)                                             
'      g27(i)=f27(i)                                             
'      g37(i)=f37(i)                                             
'      g18(i)=f18(i)                                             
'      g28(i)=f28(i)                                             
'      g38(i)=f38(i)                                             
'   20 continue
'c
'c.... geometry at n+1/2
'      do 30 i=lft,llt
'
'      h11(i)=g11(i)                                             
'      h21(i)=g21(i)                                             
'      h31(i)=g31(i)                                             
'      h12(i)=g12(i)                                             
'      h22(i)=g22(i)                                             
'      h32(i)=g32(i)                                             
'      h13(i)=g13(i)                                             
'      h23(i)=g23(i)                                             
'      h33(i)=g33(i)                                             
'      h14(i)=g14(i)                                             
'      h24(i)=g24(i)                                             
'      h34(i)=g34(i)                                             
'      h15(i)=g15(i)                                             
'      h25(i)=g25(i)                                             
'      h35(i)=g35(i)                                             
'      h16(i)=g16(i)                                             
'      h26(i)=g26(i)                                             
'      h36(i)=g36(i)                                             
'      h17(i)=g17(i)                                             
'      h27(i)=g27(i)                                             
'      h37(i)=g37(i)                                             
'      h18(i)=g18(i)                                             
'      h28(i)=g28(i)                                             
'      h38(i)=g38(i)                                             
'      hic11(i)=0.                                                   
'      hic21(i)=0.                                                   
'      hic31(i)=0.                                                   
'      hic12(i)=0.                                                   
'      hic22(i)=0.                                                   
'      hic32(i)=0.                                                   
'      hic13(i)=0.                                                   
'      hic23(i)=0.                                                   
'      hic33(i)=0.                                                   
'   30 continue
'c
'c.... reference coordinates
'      do 40 i=lft,llt
'      diavg(i)=0.
'      dfavg(i)=0.
'      volnd(i)=0.
'      volcd(i)=0.
'      x12(i)=f11(i)-f12(i)
'      x34(i)=f13(i)-f14(i)
'      x56(i)=f15(i)-f16(i)
'      x78(i)=f17(i)-f18(i)
'      x14(i)=f11(i)-f14(i)
'      x23(i)=f12(i)-f13(i)
'      x58(i)=f15(i)-f18(i)
'      x67(i)=f16(i)-f17(i)
'      x15(i)=f11(i)-f15(i)
'      x26(i)=f12(i)-f16(i)
'      x37(i)=f13(i)-f17(i)
'      x48(i)=f14(i)-f18(i)
'      y12(i)=f21(i)-f22(i)
'      y34(i)=f23(i)-f24(i)
'      y56(i)=f25(i)-f26(i)
'      y78(i)=f27(i)-f28(i)
'      y14(i)=f21(i)-f24(i)
'      y23(i)=f22(i)-f23(i)
'      y58(i)=f25(i)-f28(i)
'      y67(i)=f26(i)-f27(i)
'      y15(i)=f21(i)-f25(i)
'      y26(i)=f22(i)-f26(i)
'      y37(i)=f23(i)-f27(i)
'      y48(i)=f24(i)-f28(i)
'      z12(i)=f31(i)-f32(i)
'      z34(i)=f33(i)-f34(i)
'      z56(i)=f35(i)-f36(i)
'      z78(i)=f37(i)-f38(i)
'      z14(i)=f31(i)-f34(i)
'      z23(i)=f32(i)-f33(i)
'      z58(i)=f35(i)-f38(i)
'      z67(i)=f36(i)-f37(i)
'      z15(i)=f31(i)-f35(i)
'      z26(i)=f32(i)-f36(i)
'      z37(i)=f33(i)-f37(i)
'      z48(i)=f34(i)-f38(i)
'      xic1(i)=0.
'      xic2(i)=0.
'      xic3(i)=0.
'      yic1(i)=0.
'      yic2(i)=0.
'      yic3(i)=0.
'      zic1(i)=0.
'      zic2(i)=0.
'      zic3(i)=0.
'   40 continue
'c
'c.... define gradient matrix with respect to reference coordinates
'      call stric1
'      do 75 lst=1,8
'      ipt=lst
'      call stric2 (vlinc)
'c
'      do 45 i=lft,llt
'	if(mtype(i).ne.56) then						! Added by Qiang
'	rov(i) =ro(i)*vlinc(i)
'      ym(1,i)=ym(1,i)+rov(i)*h(1,lst)
'      ym(2,i)=ym(2,i)+rov(i)*h(2,lst)
'      ym(3,i)=ym(3,i)+rov(i)*h(3,lst)
'      ym(4,i)=ym(4,i)+rov(i)*h(4,lst)
'      ym(5,i)=ym(5,i)+rov(i)*h(5,lst)
'      ym(6,i)=ym(6,i)+rov(i)*h(6,lst)
'      ym(7,i)=ym(7,i)+rov(i)*h(7,lst)
'      ym(8,i)=ym(8,i)+rov(i)*h(8,lst)
'	end if										! Added by Qiang
'   45 continue
'c
'c.... compute gradient of displacement over step
'      do 50 i=lft,llt
'      di1(i,ipt)=px1(i)*fx1(i)+px2(i)*fx2(i)+px3(i)*fx3(i)+px4(i)*fx4(i)
'     1          +px5(i)*fx5(i)+px6(i)*fx6(i)+px7(i)*fx7(i)+px8(i)*fx8(i)
'      di2(i,ipt)=py1(i)*fy1(i)+py2(i)*fy2(i)+py3(i)*fy3(i)+py4(i)*fy4(i)
'     1          +py5(i)*fy5(i)+py6(i)*fy6(i)+py7(i)*fy7(i)+py8(i)*fy8(i)
'      di3(i,ipt)=pz1(i)*fz1(i)+pz2(i)*fz2(i)+pz3(i)*fz3(i)+pz4(i)*fz4(i)
'     1          +pz5(i)*fz5(i)+pz6(i)*fz6(i)+pz7(i)*fz7(i)+pz8(i)*fz8(i)
'      di4(i,ipt)=py1(i)*fx1(i)+py2(i)*fx2(i)+py3(i)*fx3(i)+py4(i)*fx4(i)
'     1          +py5(i)*fx5(i)+py6(i)*fx6(i)+py7(i)*fx7(i)+py8(i)*fx8(i)
'      di5(i,ipt)=pz1(i)*fx1(i)+pz2(i)*fx2(i)+pz3(i)*fx3(i)+pz4(i)*fx4(i)
'     1          +pz5(i)*fx5(i)+pz6(i)*fx6(i)+pz7(i)*fx7(i)+pz8(i)*fx8(i)
'      di6(i,ipt)=px1(i)*fy1(i)+px2(i)*fy2(i)+px3(i)*fy3(i)+px4(i)*fy4(i)
'     1          +px5(i)*fy5(i)+px6(i)*fy6(i)+px7(i)*fy7(i)+px8(i)*fy8(i)
'      di7(i,ipt)=pz1(i)*fy1(i)+pz2(i)*fy2(i)+pz3(i)*fy3(i)+pz4(i)*fy4(i)
'     1          +pz5(i)*fy5(i)+pz6(i)*fy6(i)+pz7(i)*fy7(i)+pz8(i)*fy8(i)
'      di8(i,ipt)=px1(i)*fz1(i)+px2(i)*fz2(i)+px3(i)*fz3(i)+px4(i)*fz4(i)
'     1          +px5(i)*fz5(i)+px6(i)*fz6(i)+px7(i)*fz7(i)+px8(i)*fz8(i)
'      di9(i,ipt)=py1(i)*fz1(i)+py2(i)*fz2(i)+py3(i)*fz3(i)+py4(i)*fz4(i)
'     1          +py5(i)*fz5(i)+py6(i)*fz6(i)+py7(i)*fz7(i)+py8(i)*fz8(i)
'   50 continue
'      do 55 i=lft,llt
'      di1(i,ipt)=di1(i,ipt)+pxic1(i)*fxic1(i)+pxic2(i)*fxic2(i)
'     1          +pxic3(i)*fxic3(i)
'      di2(i,ipt)=di2(i,ipt)+pyic1(i)*fyic1(i)+pyic2(i)*fyic2(i)
'     1          +pyic3(i)*fyic3(i)
'      di3(i,ipt)=di3(i,ipt)+pzic1(i)*fzic1(i)+pzic2(i)*fzic2(i)
'     1          +pzic3(i)*fzic3(i)
'      di4(i,ipt)=di4(i,ipt)+pyic1(i)*fxic1(i)+pyic2(i)*fxic2(i)
'     1          +pyic3(i)*fxic3(i)
'      di5(i,ipt)=di5(i,ipt)+pzic1(i)*fxic1(i)+pzic2(i)*fxic2(i)
'     1          +pzic3(i)*fxic3(i)
'      di6(i,ipt)=di6(i,ipt)+pxic1(i)*fyic1(i)+pxic2(i)*fyic2(i)
'     1          +pxic3(i)*fyic3(i)
'      di7(i,ipt)=di7(i,ipt)+pzic1(i)*fyic1(i)+pzic2(i)*fyic2(i)
'     1          +pzic3(i)*fyic3(i)
'      di8(i,ipt)=di8(i,ipt)+pxic1(i)*fzic1(i)+pxic2(i)*fzic2(i)
'     1          +pxic3(i)*fzic3(i)
'      di9(i,ipt)=di9(i,ipt)+pyic1(i)*fzic1(i)+pyic2(i)*fzic2(i)
'     1          +pyic3(i)*fzic3(i)
'   55 continue
'c
'c.... compute gradient of geometry at step n+1
'      do 60 i=lft,llt
'      dt1(i,ipt)=px1(i)*g11(i)+px2(i)*g12(i)+px3(i)*g13(i)+px4(i)*g14(i)
'     1          +px5(i)*g15(i)+px6(i)*g16(i)+px7(i)*g17(i)+px8(i)*g18(i)
'      dt2(i,ipt)=py1(i)*g21(i)+py2(i)*g22(i)+py3(i)*g23(i)+py4(i)*g24(i)
'     1          +py5(i)*g25(i)+py6(i)*g26(i)+py7(i)*g27(i)+py8(i)*g28(i)
'      dt3(i,ipt)=pz1(i)*g31(i)+pz2(i)*g32(i)+pz3(i)*g33(i)+pz4(i)*g34(i)
'     1          +pz5(i)*g35(i)+pz6(i)*g36(i)+pz7(i)*g37(i)+pz8(i)*g38(i)
'      dt4(i,ipt)=py1(i)*g11(i)+py2(i)*g12(i)+py3(i)*g13(i)+py4(i)*g14(i)
'     1          +py5(i)*g15(i)+py6(i)*g16(i)+py7(i)*g17(i)+py8(i)*g18(i)
'      dt5(i,ipt)=pz1(i)*g11(i)+pz2(i)*g12(i)+pz3(i)*g13(i)+pz4(i)*g14(i)
'     1          +pz5(i)*g15(i)+pz6(i)*g16(i)+pz7(i)*g17(i)+pz8(i)*g18(i)
'      dt6(i,ipt)=px1(i)*g21(i)+px2(i)*g22(i)+px3(i)*g23(i)+px4(i)*g24(i)
'     1          +px5(i)*g25(i)+px6(i)*g26(i)+px7(i)*g27(i)+px8(i)*g28(i)
'      dt7(i,ipt)=pz1(i)*g21(i)+pz2(i)*g22(i)+pz3(i)*g23(i)+pz4(i)*g24(i)
'     1          +pz5(i)*g25(i)+pz6(i)*g26(i)+pz7(i)*g27(i)+pz8(i)*g28(i)
'      dt8(i,ipt)=px1(i)*g31(i)+px2(i)*g32(i)+px3(i)*g33(i)+px4(i)*g34(i)
'     1          +px5(i)*g35(i)+px6(i)*g36(i)+px7(i)*g37(i)+px8(i)*g38(i)
'      dt9(i,ipt)=py1(i)*g31(i)+py2(i)*g32(i)+py3(i)*g33(i)+py4(i)*g34(i)
'     1          +py5(i)*g35(i)+py6(i)*g36(i)+py7(i)*g37(i)+py8(i)*g38(i)
'   60 continue
'      do 65 i=lft,llt
'      dt1(i,ipt)=dt1(i,ipt)+pxic1(i)*exic1(i)+pxic2(i)*exic2(i)
'     1          +pxic3(i)*exic3(i)
'      dt2(i,ipt)=dt2(i,ipt)+pyic1(i)*eyic1(i)+pyic2(i)*eyic2(i)
'     1          +pyic3(i)*eyic3(i)
'      dt3(i,ipt)=dt3(i,ipt)+pzic1(i)*ezic1(i)+pzic2(i)*ezic2(i)
'     1          +pzic3(i)*ezic3(i)
'      dt4(i,ipt)=dt4(i,ipt)+pyic1(i)*exic1(i)+pyic2(i)*exic2(i)
'     1          +pyic3(i)*exic3(i)
'      dt5(i,ipt)=dt5(i,ipt)+pzic1(i)*exic1(i)+pzic2(i)*exic2(i)
'     1          +pzic3(i)*exic3(i)
'      dt6(i,ipt)=dt6(i,ipt)+pxic1(i)*eyic1(i)+pxic2(i)*eyic2(i)
'     1          +pxic3(i)*eyic3(i)
'      dt7(i,ipt)=dt7(i,ipt)+pzic1(i)*eyic1(i)+pzic2(i)*eyic2(i)
'     1          +pzic3(i)*eyic3(i)
'      dt8(i,ipt)=dt8(i,ipt)+pxic1(i)*ezic1(i)+pxic2(i)*ezic2(i)
'     1          +pxic3(i)*ezic3(i)
'      dt9(i,ipt)=dt9(i,ipt)+pyic1(i)*ezic1(i)+pyic2(i)*ezic2(i)
'     1          +pyic3(i)*ezic3(i)
'   65 continue
'c
'c.... compute gradient of geometry at step n
'      do 70 i=lft,llt
'      dfavg(i)=dfavg(i)+vlinc(i)
'     1      *(dt1(i,ipt)*(dt2(i,ipt)*dt3(i,ipt)-dt7(i,ipt)*dt9(i,ipt))
'     2      + dt6(i,ipt)*(dt5(i,ipt)*dt9(i,ipt)-dt4(i,ipt)*dt3(i,ipt))
'     3      + dt8(i,ipt)*(dt4(i,ipt)*dt7(i,ipt)-dt5(i,ipt)*dt2(i,ipt)))
'      dt1(i,ipt)=dt1(i,ipt)-di1(i,ipt)
'      dt2(i,ipt)=dt2(i,ipt)-di2(i,ipt)
'      dt3(i,ipt)=dt3(i,ipt)-di3(i,ipt)
'      dt4(i,ipt)=dt4(i,ipt)-di4(i,ipt)
'      dt5(i,ipt)=dt5(i,ipt)-di5(i,ipt)
'      dt6(i,ipt)=dt6(i,ipt)-di6(i,ipt)
'      dt7(i,ipt)=dt7(i,ipt)-di7(i,ipt)
'      dt8(i,ipt)=dt8(i,ipt)-di8(i,ipt)
'      dt9(i,ipt)=dt9(i,ipt)-di9(i,ipt)
'      volnd(i)=volnd(i)+vlinc(i)
'   70 continue
'   75 continue
'c
'c.... coordinates at n+1/2
'      do 76 i=lft,llt
'      x12(i)=h11(i)-h12(i)
'      x34(i)=h13(i)-h14(i)
'      x56(i)=h15(i)-h16(i)
'      x78(i)=h17(i)-h18(i)
'      x14(i)=h11(i)-h14(i)
'      x23(i)=h12(i)-h13(i)
'      x58(i)=h15(i)-h18(i)
'      x67(i)=h16(i)-h17(i)
'      x15(i)=h11(i)-h15(i)
'      x26(i)=h12(i)-h16(i)
'      x37(i)=h13(i)-h17(i)
'      x48(i)=h14(i)-h18(i)
'      y12(i)=h21(i)-h22(i)
'      y34(i)=h23(i)-h24(i)
'      y56(i)=h25(i)-h26(i)
'      y78(i)=h27(i)-h28(i)
'      y14(i)=h21(i)-h24(i)
'      y23(i)=h22(i)-h23(i)
'      y58(i)=h25(i)-h28(i)
'      y67(i)=h26(i)-h27(i)
'      y15(i)=h21(i)-h25(i)
'      y26(i)=h22(i)-h26(i)
'      y37(i)=h23(i)-h27(i)
'      y48(i)=h24(i)-h28(i)
'      z12(i)=h31(i)-h32(i)
'      z34(i)=h33(i)-h34(i)
'      z56(i)=h35(i)-h36(i)
'      z78(i)=h37(i)-h38(i)
'      z14(i)=h31(i)-h34(i)
'      z23(i)=h32(i)-h33(i)
'      z58(i)=h35(i)-h38(i)
'      z67(i)=h36(i)-h37(i)
'      z15(i)=h31(i)-h35(i)
'      z26(i)=h32(i)-h36(i)
'      z37(i)=h33(i)-h37(i)
'      z48(i)=h34(i)-h38(i)
'      xic1(i)=hic11(i)
'      xic2(i)=hic12(i)
'      xic3(i)=hic13(i)
'      yic1(i)=hic21(i)
'      yic2(i)=hic22(i)
'      yic3(i)=hic23(i)
'      zic1(i)=hic31(i)
'      zic2(i)=hic32(i)
'      zic3(i)=hic33(i)
'   76 continue
'c
'c.... define gradient matrix with respect to geometry at n+1/2
'      call stric1
'      do 100 lst=1,8
'      ipt=lst
'      call stric2 (vlinc)
'c
'c.... compute gradient of displacement over step
'      do 80 i=lft,llt
'      dd1(i,ipt)=px1(i)*fx1(i)+px2(i)*fx2(i)+px3(i)*fx3(i)+px4(i)*fx4(i)
'     1          +px5(i)*fx5(i)+px6(i)*fx6(i)+px7(i)*fx7(i)+px8(i)*fx8(i)
'      dd2(i,ipt)=py1(i)*fy1(i)+py2(i)*fy2(i)+py3(i)*fy3(i)+py4(i)*fy4(i)
'     1          +py5(i)*fy5(i)+py6(i)*fy6(i)+py7(i)*fy7(i)+py8(i)*fy8(i)
'      dd3(i,ipt)=pz1(i)*fz1(i)+pz2(i)*fz2(i)+pz3(i)*fz3(i)+pz4(i)*fz4(i)
'     1          +pz5(i)*fz5(i)+pz6(i)*fz6(i)+pz7(i)*fz7(i)+pz8(i)*fz8(i)
'      dd4(i,ipt)=py1(i)*fx1(i)+py2(i)*fx2(i)+py3(i)*fx3(i)+py4(i)*fx4(i)
'     1          +py5(i)*fx5(i)+py6(i)*fx6(i)+py7(i)*fx7(i)+py8(i)*fx8(i)
'      dd5(i,ipt)=pz1(i)*fx1(i)+pz2(i)*fx2(i)+pz3(i)*fx3(i)+pz4(i)*fx4(i)
'     1          +pz5(i)*fx5(i)+pz6(i)*fx6(i)+pz7(i)*fx7(i)+pz8(i)*fx8(i)
'      dd6(i,ipt)=px1(i)*fy1(i)+px2(i)*fy2(i)+px3(i)*fy3(i)+px4(i)*fy4(i)
'     1          +px5(i)*fy5(i)+px6(i)*fy6(i)+px7(i)*fy7(i)+px8(i)*fy8(i)
'      dd7(i,ipt)=pz1(i)*fy1(i)+pz2(i)*fy2(i)+pz3(i)*fy3(i)+pz4(i)*fy4(i)
'     1          +pz5(i)*fy5(i)+pz6(i)*fy6(i)+pz7(i)*fy7(i)+pz8(i)*fy8(i)
'      dd8(i,ipt)=px1(i)*fz1(i)+px2(i)*fz2(i)+px3(i)*fz3(i)+px4(i)*fz4(i)
'     1          +px5(i)*fz5(i)+px6(i)*fz6(i)+px7(i)*fz7(i)+px8(i)*fz8(i)
'      dd9(i,ipt)=py1(i)*fz1(i)+py2(i)*fz2(i)+py3(i)*fz3(i)+py4(i)*fz4(i)
'     1          +py5(i)*fz5(i)+py6(i)*fz6(i)+py7(i)*fz7(i)+py8(i)*fz8(i)
'   80 continue
'      do 85 i=lft,llt
'      dd1(i,ipt)=dd1(i,ipt)+pxic1(i)*fxic1(i)+pxic2(i)*fxic2(i)
'     1          +pxic3(i)*fxic3(i)
'      dd2(i,ipt)=dd2(i,ipt)+pyic1(i)*fyic1(i)+pyic2(i)*fyic2(i)
'     1          +pyic3(i)*fyic3(i)
'      dd3(i,ipt)=dd3(i,ipt)+pzic1(i)*fzic1(i)+pzic2(i)*fzic2(i)
'     1          +pzic3(i)*fzic3(i)
'      dd4(i,ipt)=dd4(i,ipt)+pyic1(i)*fxic1(i)+pyic2(i)*fxic2(i)
'     1          +pyic3(i)*fxic3(i)
'      dd5(i,ipt)=dd5(i,ipt)+pzic1(i)*fxic1(i)+pzic2(i)*fxic2(i)
'     1          +pzic3(i)*fxic3(i)
'      dd6(i,ipt)=dd6(i,ipt)+pxic1(i)*fyic1(i)+pxic2(i)*fyic2(i)
'     1          +pxic3(i)*fyic3(i)
'      dd7(i,ipt)=dd7(i,ipt)+pzic1(i)*fyic1(i)+pzic2(i)*fyic2(i)
'     1          +pzic3(i)*fyic3(i)
'      dd8(i,ipt)=dd8(i,ipt)+pxic1(i)*fzic1(i)+pxic2(i)*fzic2(i)
'     1          +pxic3(i)*fzic3(i)
'      dd9(i,ipt)=dd9(i,ipt)+pyic1(i)*fzic1(i)+pyic2(i)*fzic2(i)
'     1          +pyic3(i)*fzic3(i)
'   85 continue
'c
'      do 90 i=lft,llt
'      diavg(i)=diavg(i)+vlinc(i)*(dd1(i,ipt)+dd2(i,ipt)+dd3(i,ipt))
'      volcd(i)=volcd(i)+vlinc(i)
'   90 continue
'c
'  100 continue
'c
'c.... coordinates at n+1
'      do 120 i=lft,llt
'      diavg(i)=diavg(i)/(volcd(i)+1.e-30)
'      dfavg(i)=dfavg(i)/(volnd(i)+1.e-30)
'      x12(i)=g11(i)-g12(i)
'      x34(i)=g13(i)-g14(i)
'      x56(i)=g15(i)-g16(i)
'      x78(i)=g17(i)-g18(i)
'      x14(i)=g11(i)-g14(i)
'      x23(i)=g12(i)-g13(i)
'      x58(i)=g15(i)-g18(i)
'      x67(i)=g16(i)-g17(i)
'      x15(i)=g11(i)-g15(i)
'      x26(i)=g12(i)-g16(i)
'      x37(i)=g13(i)-g17(i)
'      x48(i)=g14(i)-g18(i)
'      y12(i)=g21(i)-g22(i)
'      y34(i)=g23(i)-g24(i)
'      y56(i)=g25(i)-g26(i)
'      y78(i)=g27(i)-g28(i)
'      y14(i)=g21(i)-g24(i)
'      y23(i)=g22(i)-g23(i)
'      y58(i)=g25(i)-g28(i)
'      y67(i)=g26(i)-g27(i)
'      y15(i)=g21(i)-g25(i)
'      y26(i)=g22(i)-g26(i)
'      y37(i)=g23(i)-g27(i)
'      y48(i)=g24(i)-g28(i)
'      z12(i)=g31(i)-g32(i)
'      z34(i)=g33(i)-g34(i)
'      z56(i)=g35(i)-g36(i)
'      z78(i)=g37(i)-g38(i)
'      z14(i)=g31(i)-g34(i)
'      z23(i)=g32(i)-g33(i)
'      z58(i)=g35(i)-g38(i)
'      z67(i)=g36(i)-g37(i)
'      z15(i)=g31(i)-g35(i)
'      z26(i)=g32(i)-g36(i)
'      z37(i)=g33(i)-g37(i)
'      z48(i)=g34(i)-g38(i)
'
'      xic1(i)=0.                                                    
'      xic2(i)=0.                                                    
'      xic3(i)=0.                                                    
'      yic1(i)=0.                                                    
'      yic2(i)=0.                                                    
'      yic3(i)=0.                                                    
'      zic1(i)=0.                                                    
'      zic2(i)=0.                                                    
'      zic3(i)=0.                                                    
'  120 continue
'c
'c.... define gradient matrix with respect to geometry at n+1
'      call stric1
'      do 410 lst=1,8
'      ipt=lst
'      call stric2 (volgp)
'c
'      volmin=1.e10
'      do 150 i=lft,llt
'      volmin=min(volmin,volgp(i))
'  150 continue
'c
'      if(volmin.lt.0.0) then
'        write(lutty,990)
'        return 1
'      endif
'c
'c.... polar decomposition at step n
'c
'      do 210 i=lft,llt
'      f1(i)=dt1(i,ipt)
'      f2(i)=dt2(i,ipt)
'      f3(i)=dt3(i,ipt)
'      f4(i)=dt4(i,ipt)
'      f5(i)=dt5(i,ipt)
'      f6(i)=dt6(i,ipt)
'      f7(i)=dt7(i,ipt)
'      f8(i)=dt8(i,ipt)
'      f9(i)=dt9(i,ipt)
'  210 continue
'c
'      call getvrt(r11,r12,r13,r21,r22,r23,r31,r32,r33)
'c
'c.... polar decomposition at step n+1/2
'      do 220 i=lft,llt
'      f1(i)=dt1(i,ipt)+.50*di1(i,ipt)
'      f2(i)=dt2(i,ipt)+.50*di2(i,ipt)
'      f3(i)=dt3(i,ipt)+.50*di3(i,ipt)
'      f4(i)=dt4(i,ipt)+.50*di4(i,ipt)
'      f5(i)=dt5(i,ipt)+.50*di5(i,ipt)
'      f6(i)=dt6(i,ipt)+.50*di6(i,ipt)
'      f7(i)=dt7(i,ipt)+.50*di7(i,ipt)
'      f8(i)=dt8(i,ipt)+.50*di8(i,ipt)
'      f9(i)=dt9(i,ipt)+.50*di9(i,ipt)
'  220 continue
'c
'      call getvrt(q11,q12,q13,q21,q22,q23,q31,q32,q33)
'c
'c.... polar decomposition at step n+1
'      do 230 i=lft,llt
'      f1(i)=dt1(i,ipt)+di1(i,ipt)
'      f2(i)=dt2(i,ipt)+di2(i,ipt)
'      f3(i)=dt3(i,ipt)+di3(i,ipt)
'      f4(i)=dt4(i,ipt)+di4(i,ipt)
'      f5(i)=dt5(i,ipt)+di5(i,ipt)
'      f6(i)=dt6(i,ipt)+di6(i,ipt)
'      f7(i)=dt7(i,ipt)+di7(i,ipt)
'      f8(i)=dt8(i,ipt)+di8(i,ipt)
'      f9(i)=dt9(i,ipt)+di9(i,ipt)
'  230 continue
'c
'      call getvrt(s11,s12,s13,s21,s22,s23,s31,s32,s33)
'c
'c.... rotate incremental deformation gradient by r at n+1/2
'      call rotstr (dd1(1,ipt),dd2(1,ipt),dd3(1,ipt),dd4(1,ipt),
'     1  dd5(1,ipt),dd6(1,ipt),dd7(1,ipt),dd8(1,ipt),dd9(1,ipt))
'c
'      do 240 i=lft,llt
'      d1(i)=dd1(i,ipt)
'      d2(i)=dd2(i,ipt)
'      d3(i)=dd3(i,ipt)
'      d4(i)=dd4(i,ipt)
'      d5(i)=dd5(i,ipt)
'      d6(i)=dd6(i,ipt)
'  240 continue
'c
'      mft=1
'      mlt=1
'      llt1=llt-1
'      nel=nftm1+1
'      if (lft.eq.llt) go to 260
'      do 250 i=lft,llt1
'c
'      if (matp(i).eq.matp(i+1)) go to 250
'      model=mtype(mft)
'      nelsub=nftm1+mft
'c
'      if (model.ne.0) then
'          call sslcs(ihv,*991)
'
'      end if    
'c
'
'      mft=mlt+1
'  250 mlt=mlt+1
'  260 model=mtype(mft)
'      nelsub=nftm1+mft
'c
'      call sslcs(ihv,*991)
'
'      do 280 i=lft,llt
'      sig11(i)=sig11s(i)*volgp(i)
'      sig22(i)=sig22s(i)*volgp(i)
'      sig33(i)=sig33s(i)*volgp(i)
'      sig12(i)=sig12s(i)*volgp(i)
'      sig23(i)=sig23s(i)*volgp(i)
'  280 sig31(i)=sig31s(i)*volgp(i)
'c
'      if (ibar.eq.0) go to 300
'      third=-1.d0/3.
'      do 292 i=lft,llt
'      trsg(i)=sig11(i)+sig22(i)+sig33(i)
'      pxm1(i)=sclo(i)*(pxa1(i)+third*px1(i))
'      pxm2(i)=sclo(i)*(pxa2(i)+third*px2(i))
'      pxm3(i)=sclo(i)*(pxa3(i)+third*px3(i))
'      pxm4(i)=sclo(i)*(pxa4(i)+third*px4(i))
'      pxm5(i)=sclo(i)*(pxa5(i)+third*px5(i))
'      pxm6(i)=sclo(i)*(pxa6(i)+third*px6(i))
'      pxm7(i)=sclo(i)*(pxa7(i)+third*px7(i))
'      pxm8(i)=sclo(i)*(pxa8(i)+third*px8(i))
'      pym1(i)=sclo(i)*(pya1(i)+third*py1(i))
'      pym2(i)=sclo(i)*(pya2(i)+third*py2(i))
'      pym3(i)=sclo(i)*(pya3(i)+third*py3(i))
'      pym4(i)=sclo(i)*(pya4(i)+third*py4(i))
'      pym5(i)=sclo(i)*(pya5(i)+third*py5(i))
'      pym6(i)=sclo(i)*(pya6(i)+third*py6(i))
'      pym7(i)=sclo(i)*(pya7(i)+third*py7(i))
'      pym8(i)=sclo(i)*(pya8(i)+third*py8(i))
'      pzm1(i)=sclo(i)*(pza1(i)+third*pz1(i))
'      pzm2(i)=sclo(i)*(pza2(i)+third*pz2(i))
'      pzm3(i)=sclo(i)*(pza3(i)+third*pz3(i))
'      pzm4(i)=sclo(i)*(pza4(i)+third*pz4(i))
'      pzm5(i)=sclo(i)*(pza5(i)+third*pz5(i))
'      pzm6(i)=sclo(i)*(pza6(i)+third*pz6(i))
'      pzm7(i)=sclo(i)*(pza7(i)+third*pz7(i))
'      pzm8(i)=sclo(i)*(pza8(i)+third*pz8(i))
'  292 continue
'      do 296 i=lft,llt
'      rx1(i)=rx1(i)+pxm1(i)*trsg(i)
'      ry1(i)=ry1(i)+pym1(i)*trsg(i)
'      rz1(i)=rz1(i)+pzm1(i)*trsg(i)
'      rx2(i)=rx2(i)+pxm2(i)*trsg(i)
'      ry2(i)=ry2(i)+pym2(i)*trsg(i)
'      rz2(i)=rz2(i)+pzm2(i)*trsg(i)
'      rx3(i)=rx3(i)+pxm3(i)*trsg(i)
'      ry3(i)=ry3(i)+pym3(i)*trsg(i)
'      rz3(i)=rz3(i)+pzm3(i)*trsg(i)
'      rx4(i)=rx4(i)+pxm4(i)*trsg(i)
'      ry4(i)=ry4(i)+pym4(i)*trsg(i)
'      rz4(i)=rz4(i)+pzm4(i)*trsg(i)
'      rx5(i)=rx5(i)+pxm5(i)*trsg(i)
'      ry5(i)=ry5(i)+pym5(i)*trsg(i)
'      rz5(i)=rz5(i)+pzm5(i)*trsg(i)
'      rx6(i)=rx6(i)+pxm6(i)*trsg(i)
'      ry6(i)=ry6(i)+pym6(i)*trsg(i)
'      rz6(i)=rz6(i)+pzm6(i)*trsg(i)
'      rx7(i)=rx7(i)+pxm7(i)*trsg(i)
'      ry7(i)=ry7(i)+pym7(i)*trsg(i)
'      rz7(i)=rz7(i)+pzm7(i)*trsg(i)
'      rx8(i)=rx8(i)+pxm8(i)*trsg(i)
'      ry8(i)=ry8(i)+pym8(i)*trsg(i)
'      rz8(i)=rz8(i)+pzm8(i)*trsg(i)
'  296 continue
'c
'c.... construct internal force
'  300 do 310 i=lft,llt
'      rx1(i)=rx1(i)+px1(i)*sig11(i)+py1(i)*sig12(i)+pz1(i)*sig31(i)
'      ry1(i)=ry1(i)+py1(i)*sig22(i)+px1(i)*sig12(i)+pz1(i)*sig23(i)
'      rz1(i)=rz1(i)+pz1(i)*sig33(i)+px1(i)*sig31(i)+py1(i)*sig23(i)
'      rx2(i)=rx2(i)+px2(i)*sig11(i)+py2(i)*sig12(i)+pz2(i)*sig31(i)
'      ry2(i)=ry2(i)+py2(i)*sig22(i)+px2(i)*sig12(i)+pz2(i)*sig23(i)
'      rz2(i)=rz2(i)+pz2(i)*sig33(i)+px2(i)*sig31(i)+py2(i)*sig23(i)
'      rx3(i)=rx3(i)+px3(i)*sig11(i)+py3(i)*sig12(i)+pz3(i)*sig31(i)
'      ry3(i)=ry3(i)+py3(i)*sig22(i)+px3(i)*sig12(i)+pz3(i)*sig23(i)
'      rz3(i)=rz3(i)+pz3(i)*sig33(i)+px3(i)*sig31(i)+py3(i)*sig23(i)
'      rx4(i)=rx4(i)+px4(i)*sig11(i)+py4(i)*sig12(i)+pz4(i)*sig31(i)
'      ry4(i)=ry4(i)+py4(i)*sig22(i)+px4(i)*sig12(i)+pz4(i)*sig23(i)
'      rz4(i)=rz4(i)+pz4(i)*sig33(i)+px4(i)*sig31(i)+py4(i)*sig23(i)
'      rx5(i)=rx5(i)+px5(i)*sig11(i)+py5(i)*sig12(i)+pz5(i)*sig31(i)
'      ry5(i)=ry5(i)+py5(i)*sig22(i)+px5(i)*sig12(i)+pz5(i)*sig23(i)
'      rz5(i)=rz5(i)+pz5(i)*sig33(i)+px5(i)*sig31(i)+py5(i)*sig23(i)
'      rx6(i)=rx6(i)+px6(i)*sig11(i)+py6(i)*sig12(i)+pz6(i)*sig31(i)
'      ry6(i)=ry6(i)+py6(i)*sig22(i)+px6(i)*sig12(i)+pz6(i)*sig23(i)
'      rz6(i)=rz6(i)+pz6(i)*sig33(i)+px6(i)*sig31(i)+py6(i)*sig23(i)
'      rx7(i)=rx7(i)+px7(i)*sig11(i)+py7(i)*sig12(i)+pz7(i)*sig31(i)
'      ry7(i)=ry7(i)+py7(i)*sig22(i)+px7(i)*sig12(i)+pz7(i)*sig23(i)
'      rz7(i)=rz7(i)+pz7(i)*sig33(i)+px7(i)*sig31(i)+py7(i)*sig23(i)
'      rx8(i)=rx8(i)+px8(i)*sig11(i)+py8(i)*sig12(i)+pz8(i)*sig31(i)
'      ry8(i)=ry8(i)+py8(i)*sig22(i)+px8(i)*sig12(i)+pz8(i)*sig23(i)
'      rz8(i)=rz8(i)+pz8(i)*sig33(i)+px8(i)*sig31(i)+py8(i)*sig23(i)
'  310 continue
'      do 315 i=lft,llt
'      rxic1(i)=rxic1(i)+pxic1(i)*sig11(i)+pyic1(i)*sig12(i)
'     1 +pzic1(i)*sig31(i)
'      ryic1(i)=ryic1(i)+pyic1(i)*sig22(i)+pxic1(i)*sig12(i)
'     1 +pzic1(i)*sig23(i)
'      rzic1(i)=rzic1(i)+pzic1(i)*sig33(i)+pxic1(i)*sig31(i)
'     1 +pyic1(i)*sig23(i)
'      rxic2(i)=rxic2(i)+pxic2(i)*sig11(i)+pyic2(i)*sig12(i)
'     1 +pzic2(i)*sig31(i)
'      ryic2(i)=ryic2(i)+pyic2(i)*sig22(i)+pxic2(i)*sig12(i)
'     1 +pzic2(i)*sig23(i)
'      rzic2(i)=rzic2(i)+pzic2(i)*sig33(i)+pxic2(i)*sig31(i)
'     1 +pyic2(i)*sig23(i)
'      rxic3(i)=rxic3(i)+pxic3(i)*sig11(i)+pyic3(i)*sig12(i)
'     1 +pzic3(i)*sig31(i)
'      ryic3(i)=ryic3(i)+pyic3(i)*sig22(i)+pxic3(i)*sig12(i)
'     1 +pzic3(i)*sig23(i)
'      rzic3(i)=rzic3(i)+pzic3(i)*sig33(i)+pxic3(i)*sig31(i)
'     1 +pyic3(i)*sig23(i)
'  315 continue
'c
'      if(numdc.eq.0)  go to 320
'      if(iphase.eq.2) go to 340
'  320 if(iphase-2) 330,330,410
'  330 if(iref)     410,340,410
'c
'c.... compute linear stiffness matrix
'  340 continue
'      do 360 j=1,36
'      do 360 i=lft,llt
'      d(j,i)=d(j,i)*volgp(i)
'  360 continue
'
'      if(ibar.eq.0) call azero(pxm1,1536)
'      call btdb(s(25,1),d(1,1))
'      call bdbic1(px1,pxic1,d,ekic,nmel)
'      call bdbic2(pxic1,d,ekic,nmel)
'c
'c
'  410 continue
'c
'      return
'c
'  991 continue
'      return 1
'
'  990 format(/
'     &' ************************************************************',/
'     &' *                     - WARNING -                          *',/
'     &' *     Negative hex element volume computed in STFNF        *',/
'     &' ************************************************************')
'      end

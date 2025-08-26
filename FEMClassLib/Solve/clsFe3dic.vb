'This file contains all the methods of fe3dic.f
Partial Public Class clsSolve


    
    'Public numelh, ksizhx, neehx, iprec As Integer

    Public stepsv, stepls As Double

    'Public idid(), lcd(), idflad() As Integer, amad() As Double



    Private lcmpr As Boolean

    Private lft, llt, nftm1 As Integer

    'Private lmm(628, 64) As Integer 'YC?  lmm(648,64)=s(324,64), lmm()=lmm(1:24,64)
    Private lmm(24, 64) As Integer

    Private rx1(64), ry1(64), rz1(64),
     rx2(64), ry2(64), rz2(64),
     rx3(64), ry3(64), rz3(64),
     rx4(64), ry4(64), rz4(64),
     rx5(64), ry5(64), rz5(64),
     rx6(64), ry6(64), rz6(64),
     rx7(64), ry7(64), rz7(64),
     rx8(64), ry8(64), rz8(64) As Double

    Private mtype(64), matp(64) As Integer

    Private ex1(64), ey1(64), ez1(64),
        ex2(64), ey2(64), ez2(64),
        ex3(64), ey3(64), ez3(64),
        ex4(64), ey4(64), ez4(64),
        ex5(64), ey5(64), ez5(64),
        ex6(64), ey6(64), ez6(64),
        ex7(64), ey7(64), ez7(64),
        ex8(64), ey8(64), ez8(64),
        fx1(64), fy1(64), fz1(64),
        fx2(64), fy2(64), fz2(64),
        fx3(64), fy3(64), fz3(64),
        fx4(64), fy4(64), fz4(64),
        fx5(64), fy5(64), fz5(64),
        fx6(64), fy6(64), fz6(64),
        fx7(64), fy7(64), fz7(64),
        fx8(64), fy8(64), fz8(64) As Double



    Private rxic1(64), ryic1(64), rzic1(64),
        rxic2(64), ryic2(64), rzic2(64),
        rxic3(64), ryic3(64), rzic3(64) As Double

    Private ekic(33, 9, 64) As Double

    Private ix1(64), ix2(64), ix3(64), ix4(64), ix5(64), ix6(64), ix7(64), ix8(64) As Integer

    Private f11(64), f21(64), f31(64),
        f12(64), f22(64), f32(64),
        f13(64), f23(64), f33(64),
        f14(64), f24(64), f34(64),
        f15(64), f25(64), f35(64),
        f16(64), f26(64), f36(64),
        f17(64), f27(64), f37(64),
        f18(64), f28(64), f38(64) As Double

    Private sclo(64), dro(64) As Double

    Private scale(24, 64) As Double

    Private fxic1(64), fyic1(64), fzic1(64),
        fxic2(64), fyic2(64), fzic2(64),
        fxic3(64), fyic3(64), fzic3(64) As Double

    Private exic1(64), eyic1(64), ezic1(64),
        exic2(64), eyic2(64), ezic2(64),
        exic3(64), eyic3(64), ezic3(64) As Double

    Private rmx1(64), rmy1(64), rmz1(64),
        rmx2(64), rmy2(64), rmz2(64),
     rmx3(64), rmy3(64), rmz3(64),
     rmx4(64), rmy4(64), rmz4(64),
     rmx5(64), rmy5(64), rmz5(64),
     rmx6(64), rmy6(64), rmz6(64),
      rmx7(64), rmy7(64), rmz7(64),
      rmx8(64), rmy8(64), rmz8(64) As Double






    'Overload to handle 1D-2D input

    Public Sub fe3dic(ByRef d() As Double, ByRef rhs() As Double, ByRef usi() As Double,
                      ByRef ixp(,) As Integer, ByVal xyz(,) As Double, ByRef matype() As Integer,
                     ByRef rho() As Double, ByRef fval() As Double, ByRef id(,) As Integer,
                     ByRef diag(,) As Double, ByRef hxdata1(,) As Double, ByRef hxdata2(,) As Integer, ByRef icnt2 As Integer,
                     ByRef tvc2() As Double, ByRef sic(,,) As Double, ByRef dic(,) As Double,
                     ByRef usic(,) As Double, ByRef dusic(,) As Double,
                     ByRef ric(,) As Double, ByRef numnp As Integer, ByRef nmmat As Integer)

        'Dim numel = lhex(1)    ' YC 102418
        'numelh = lhex(3)
        Dim numel = lhex(2)
        numelh = lhex(4)

        Dim i As Integer    ' YC 102418

        Dim _step = 0.0

        Dim kstore = 1 + nblk * ksizhx
        'Dim lstore = 1 + numelh * ksizhx + nblk * neehx
        Dim lstore = 1 + nblk * neehx ' QW 12-12-2018-

        Dim nelg As Integer = Fix((numel - 1) / 64) + 1  ' QW 12-12-2018-
        Dim nmel = 64
        Dim nauxrb = 0
        lcmpr = False

        Dim sic_nft(33, 9, 64) As Double  '64 vs llt by YC? 092018
        ' QW 12-12-2018-

        ' YC 102418
        'For ng = 0 To nelg - 1     
        'lft = 0
        'llt = Math.Min(64, numel - 64 * (ng - 1)) - 1 
        For ng = 1 To nelg
            lft = 1
            llt = Math.Min(64, numel - 64 * (ng - 1))

            ' YC 102418 END

            Dim nft = 1 + (ng - 1) * 64
            Dim nlt = Math.Min(numel, ng * 64)


            nftm1 = nft - 1
            Dim kfirst = kstore
            Dim lfirst = lstore

            'c.... zero compatable arrays
            'Call azero(lmm, 324 * nmel) '324*64 for double = 628*64 for integer by YC? 092018
            Call azero(lmm, 24, nmel)
            Call azero(s, 300, nmel)

            'change rx1 to rx1,,,, rz8 by YC 092018
            'Call azero(rx1, 24 * nmel)  
            Call azero(rx1, nmel)
            Call azero(ry1, nmel)
            Call azero(rz1, nmel)
            Call azero(rx2, nmel)
            Call azero(ry2, nmel)
            Call azero(rz2, nmel)
            Call azero(rx3, nmel)
            Call azero(ry3, nmel)
            Call azero(rz3, nmel)
            Call azero(rx4, nmel)
            Call azero(ry4, nmel)
            Call azero(rz4, nmel)
            Call azero(rx5, nmel)
            Call azero(ry5, nmel)
            Call azero(rz5, nmel)
            Call azero(rx6, nmel)
            Call azero(ry6, nmel)
            Call azero(rz6, nmel)
            Call azero(rx7, nmel)
            Call azero(ry7, nmel)
            Call azero(rz7, nmel)
            Call azero(rx8, nmel)
            Call azero(ry8, nmel)
            Call azero(rz8, nmel)
            'change rx1 to rx1,,,, rz8 by YC 092018 END

            'For i = 0 To 63        ' YC 102418
            For i = 1 To 64
                mtype(i) = 0
                matp(i) = 0
            Next

            'change ex1 to ex1,,,, fz8 by YC 092018
            'Call azero(ex1, 48 * nmel)
            Call azero(ex1, nmel)
            Call azero(ey1, nmel)
            Call azero(ez1, nmel)
            Call azero(ex2, nmel)
            Call azero(ey2, nmel)
            Call azero(ez2, nmel)
            Call azero(ex3, nmel)
            Call azero(ey3, nmel)
            Call azero(ez3, nmel)
            Call azero(ex4, nmel)
            Call azero(ey4, nmel)
            Call azero(ez4, nmel)
            Call azero(ex5, nmel)
            Call azero(ey5, nmel)
            Call azero(ez5, nmel)
            Call azero(ex6, nmel)
            Call azero(ey6, nmel)
            Call azero(ez6, nmel)
            Call azero(ex7, nmel)
            Call azero(ey7, nmel)
            Call azero(ez7, nmel)
            Call azero(ex8, nmel)
            Call azero(ey8, nmel)
            Call azero(ez8, nmel)
            Call azero(fx1, nmel)
            Call azero(fy1, nmel)
            Call azero(fz1, nmel)
            Call azero(fx2, nmel)
            Call azero(fy2, nmel)
            Call azero(fz2, nmel)
            Call azero(fx3, nmel)
            Call azero(fy3, nmel)
            Call azero(fz3, nmel)
            Call azero(fx4, nmel)
            Call azero(fy4, nmel)
            Call azero(fz4, nmel)
            Call azero(fx5, nmel)
            Call azero(fy5, nmel)
            Call azero(fz5, nmel)
            Call azero(fx6, nmel)
            Call azero(fy6, nmel)
            Call azero(fz6, nmel)
            Call azero(fx7, nmel)
            Call azero(fy7, nmel)
            Call azero(fz7, nmel)
            Call azero(fx8, nmel)
            Call azero(fy8, nmel)
            Call azero(fz8, nmel)
            'change ex1 to ex1,,,, fz8 by YC 092018 END

            'change ym to ym,ro by YC 092018
            'Call azero(ym, 9 * nmel)   
            Call azero(ym, 8, nmel)
            Call azero(ro, nmel)
            'change ym to ym,ro by YC 092018 END


            'c.... zero incompatible arrays
            'change rxic1 to rxic1,,,rzic3 by YC 092018
            'Call azero(rxic1, 9 * nmel)
            Call azero(rxic1, nmel)
            Call azero(ryic1, nmel)
            Call azero(rzic1, nmel)
            Call azero(rxic2, nmel)
            Call azero(ryic2, nmel)
            Call azero(rzic2, nmel)
            Call azero(rxic3, nmel)
            Call azero(ryic3, nmel)
            Call azero(rzic3, nmel)
            'change rxic1 to rxic1,,,rzic3 by YC 092018 END


            'Call azero(ekic, 297 * nmel)  'YC 092018
            Call azero(ekic, 33, 9, nmel)

            'c.... unpack element connectivities
            Dim lblk = nblk + nft
            'Call unpci(ixp(0, lblk))

            Call unpci(ixp, 0, lblk - 1)   ' YC 102418
            'Call unpci(ixp, 1, lblk)

            'c.... test for hexagonal or tetragonal elements                         
            Dim ldgen2 = ldgen

            If Not ldgen Then
                For i = lft To llt
                    If ix5(i) = ix6(i) Then ldgen2 = True
                Next
            End If

            'c.... gather initial coordinates into local vectors
            For i = lft To llt
                ' YC 102418
                'f11(i) = xyz(0, ix1(i))
                'f21(i) = xyz(1, ix1(i))
                'f31(i) = xyz(2, ix1(i))
                'f12(i) = xyz(0, ix2(i))
                'f22(i) = xyz(1, ix2(i))
                'f32(i) = xyz(2, ix2(i))
                'f13(i) = xyz(0, ix3(i))
                'f23(i) = xyz(1, ix3(i))
                'f33(i) = xyz(2, ix3(i))
                'f14(i) = xyz(0, ix4(i))
                'f24(i) = xyz(1, ix4(i))
                'f34(i) = xyz(2, ix4(i))
                'f15(i) = xyz(0, ix5(i))
                'f25(i) = xyz(1, ix5(i))
                'f35(i) = xyz(2, ix5(i))
                'f16(i) = xyz(0, ix6(i))
                'f26(i) = xyz(1, ix6(i))
                'f36(i) = xyz(2, ix6(i))
                'f17(i) = xyz(0, ix7(i))
                'f27(i) = xyz(1, ix7(i))
                'f37(i) = xyz(2, ix7(i))
                'f18(i) = xyz(0, ix8(i))
                'f28(i) = xyz(1, ix8(i))
                'f38(i) = xyz(2, ix8(i))
                f11(i) = xyz(1, ix1(i))
                f21(i) = xyz(2, ix1(i))
                f31(i) = xyz(3, ix1(i))
                f12(i) = xyz(1, ix2(i))
                f22(i) = xyz(2, ix2(i))
                f32(i) = xyz(3, ix2(i))
                f13(i) = xyz(1, ix3(i))
                f23(i) = xyz(2, ix3(i))
                f33(i) = xyz(3, ix3(i))
                f14(i) = xyz(1, ix4(i))
                f24(i) = xyz(2, ix4(i))
                f34(i) = xyz(3, ix4(i))
                f15(i) = xyz(1, ix5(i))
                f25(i) = xyz(2, ix5(i))
                f35(i) = xyz(3, ix5(i))
                f16(i) = xyz(1, ix6(i))
                f26(i) = xyz(2, ix6(i))
                f36(i) = xyz(3, ix6(i))
                f17(i) = xyz(1, ix7(i))
                f27(i) = xyz(2, ix7(i))
                f37(i) = xyz(3, ix7(i))
                f18(i) = xyz(1, ix8(i))
                f28(i) = xyz(2, ix8(i))
                f38(i) = xyz(3, ix8(i))
                ' YC 102418 END

                ' If ng = 1 And i = 1 Then
                ' Call Check3(f31, f35, f11, i)
                ' End If
            Next
            'If ng = 1 Then
            'Call Check2DT(xyz, 3, numnp)
            'End If

            'c     gather lm array into local vectors

            For i = lft To llt

                'For j = 0 To 2                    ' YC 102418
                For j = 1 To 3
                    lmm(j, i) = id(j, ix1(i))
                    lmm(j + 3, i) = id(j, ix2(i))
                    lmm(j + 6, i) = id(j, ix3(i))
                    lmm(j + 9, i) = id(j, ix4(i))
                    lmm(j + 12, i) = id(j, ix5(i))
                    lmm(j + 15, i) = id(j, ix6(i))
                    lmm(j + 18, i) = id(j, ix7(i))
                    lmm(j + 21, i) = id(j, ix8(i))
                Next
            Next

            'c       expand packed integers

            For i = lft To llt
                mtype(i) = 0
                sclo(i) = 1.0
                If matp(i) <> 0 Then
                    mtype(i) = matype(matp(i))
                    ro(i) = rho(matp(i))

                    'c	------ Start of changed by Qiang 03-09-2009 ------------	 
                    'c	Comments:
                    'c		The density of infinite element is used for indicator of direction.
                    'c		Move and store the indicator of infinite direction to a new array.
                    If mtype(i) = 56 Then ro(i) = 0.0
                    dro(i) = rho(matp(i))

                    'c	------- End of changed by Qiang ------------------------

                End If
                If mtype(i) = 2 OrElse mtype(i) = 7 OrElse mtype(i) = 14 Then sclo(i) = 0.0
            Next

            For i = lft To llt

                'change clsInput to objComSub by YC 092018
                'scale(0, i) = CDbl(1 + clsInput.sign(0, lmm(0, i) - 1)) / 2.0
                'scale(1, i) = CDbl(1 + clsInput.sign(0, lmm(1, i) - 1)) / 2.0
                'scale(2, i) = CDbl(1 + clsInput.sign(0, lmm(2, i) - 1)) / 2.0
                'scale(3, i) = CDbl(1 + clsInput.sign(0, lmm(3, i) - 1)) / 2.0
                'scale(4, i) = CDbl(1 + clsInput.sign(0, lmm(4, i) - 1)) / 2.0
                'scale(5, i) = CDbl(1 + clsInput.sign(0, lmm(5, i) - 1)) / 2.0
                'scale(6, i) = CDbl(1 + clsInput.sign(0, lmm(6, i) - 1)) / 2.0
                'scale(7, i) = CDbl(1 + clsInput.sign(0, lmm(7, i) - 1)) / 2.0
                'scale(8, i) = CDbl(1 + clsInput.sign(0, lmm(8, i) - 1)) / 2.0
                'scale(9, i) = CDbl(1 + clsInput.sign(0, lmm(9, i) - 1)) / 2.0
                'scale(10, i) = CDbl(1 + clsInput.sign(0, lmm(10, i) - 1)) / 2.0
                'scale(11, i) = CDbl(1 + clsInput.sign(0, lmm(11, i) - 1)) / 2.0
                'scale(12, i) = CDbl(1 + clsInput.sign(0, lmm(12, i) - 1)) / 2.0
                'scale(13, i) = CDbl(1 + clsInput.sign(0, lmm(13, i) - 1)) / 2.0
                'scale(14, i) = CDbl(1 + clsInput.sign(0, lmm(14, i) - 1)) / 2.0
                'scale(15, i) = CDbl(1 + clsInput.sign(0, lmm(15, i) - 1)) / 2.0
                'scale(16, i) = CDbl(1 + clsInput.sign(0, lmm(16, i) - 1)) / 2.0
                'scale(17, i) = CDbl(1 + clsInput.sign(0, lmm(17, i) - 1)) / 2.0
                'scale(18, i) = CDbl(1 + clsInput.sign(0, lmm(18, i) - 1)) / 2.0
                'scale(19, i) = CDbl(1 + clsInput.sign(0, lmm(19, i) - 1)) / 2.0
                'scale(20, i) = CDbl(1 + clsInput.sign(0, lmm(20, i) - 1)) / 2.0
                'scale(21, i) = CDbl(1 + clsInput.sign(0, lmm(21, i) - 1)) / 2.0
                'scale(22, i) = CDbl(1 + clsInput.sign(0, lmm(22, i) - 1)) / 2.0
                'scale(23, i) = CDbl(1 + clsInput.sign(0, lmm(23, i) - 1)) / 2.0
                scale(1, i) = CDbl(1 + objComsub.sign(1, lmm(1, i) - 1)) / 2.0              ' QW 12-12-2018
                scale(2, i) = CDbl(1 + objComsub.sign(1, lmm(2, i) - 1)) / 2.0
                scale(3, i) = CDbl(1 + objComsub.sign(1, lmm(3, i) - 1)) / 2.0
                scale(4, i) = CDbl(1 + objComsub.sign(1, lmm(4, i) - 1)) / 2.0
                scale(5, i) = CDbl(1 + objComsub.sign(1, lmm(5, i) - 1)) / 2.0
                scale(6, i) = CDbl(1 + objComsub.sign(1, lmm(6, i) - 1)) / 2.0
                scale(7, i) = CDbl(1 + objComsub.sign(1, lmm(7, i) - 1)) / 2.0
                scale(8, i) = CDbl(1 + objComsub.sign(1, lmm(8, i) - 1)) / 2.0
                scale(9, i) = CDbl(1 + objComsub.sign(1, lmm(9, i) - 1)) / 2.0
                scale(10, i) = CDbl(1 + objComsub.sign(1, lmm(10, i) - 1)) / 2.0
                scale(11, i) = CDbl(1 + objComsub.sign(1, lmm(11, i) - 1)) / 2.0
                scale(12, i) = CDbl(1 + objComsub.sign(1, lmm(12, i) - 1)) / 2.0
                scale(13, i) = CDbl(1 + objComsub.sign(1, lmm(13, i) - 1)) / 2.0
                scale(14, i) = CDbl(1 + objComsub.sign(1, lmm(14, i) - 1)) / 2.0
                scale(15, i) = CDbl(1 + objComsub.sign(1, lmm(15, i) - 1)) / 2.0
                scale(16, i) = CDbl(1 + objComsub.sign(1, lmm(16, i) - 1)) / 2.0
                scale(17, i) = CDbl(1 + objComsub.sign(1, lmm(17, i) - 1)) / 2.0
                scale(18, i) = CDbl(1 + objComsub.sign(1, lmm(18, i) - 1)) / 2.0
                scale(19, i) = CDbl(1 + objComsub.sign(1, lmm(19, i) - 1)) / 2.0
                scale(20, i) = CDbl(1 + objComsub.sign(1, lmm(20, i) - 1)) / 2.0
                scale(21, i) = CDbl(1 + objComsub.sign(1, lmm(21, i) - 1)) / 2.0
                scale(22, i) = CDbl(1 + objComsub.sign(1, lmm(22, i) - 1)) / 2.0
                scale(23, i) = CDbl(1 + objComsub.sign(1, lmm(23, i) - 1)) / 2.0
                scale(24, i) = CDbl(1 + objComsub.sign(1, lmm(24, i) - 1)) / 2.0
                'change clsInput to objComSub by YC 092018 END

            Next


            For i = lft To llt

                'YC 102418 
                'ex1(i) = scale(0, i) * d(lmm(0, i))
                'ey1(i) = scale(1, i) * d(lmm(1, i))
                'ez1(i) = scale(2, i) * d(lmm(2, i))
                'ex2(i) = scale(3, i) * d(lmm(3, i))
                'ey2(i) = scale(4, i) * d(lmm(4, i))
                'ez2(i) = scale(5, i) * d(lmm(5, i))
                'ex3(i) = scale(6, i) * d(lmm(6, i))
                'ey3(i) = scale(7, i) * d(lmm(7, i))
                'ez3(i) = scale(8, i) * d(lmm(8, i))
                'ex4(i) = scale(9, i) * d(lmm(9, i))
                'ey4(i) = scale(10, i) * d(lmm(10, i))
                'ez4(i) = scale(11, i) * d(lmm(11, i))
                'ex5(i) = scale(12, i) * d(lmm(12, i))
                'ey5(i) = scale(13, i) * d(lmm(13, i))
                'ez5(i) = scale(14, i) * d(lmm(14, i))
                'ex6(i) = scale(15, i) * d(lmm(15, i))
                'ey6(i) = scale(16, i) * d(lmm(16, i))
                'ez6(i) = scale(17, i) * d(lmm(17, i))
                'ex7(i) = scale(18, i) * d(lmm(18, i))
                'ey7(i) = scale(19, i) * d(lmm(19, i))
                'ez7(i) = scale(20, i) * d(lmm(20, i))
                'ex8(i) = scale(21, i) * d(lmm(21, i))
                'ey8(i) = scale(22, i) * d(lmm(22, i))
                'ez8(i) = scale(23, i) * d(lmm(23, i))
                ex1(i) = scale(1, i) * d(lmm(1, i))
                ey1(i) = scale(2, i) * d(lmm(2, i))
                ez1(i) = scale(3, i) * d(lmm(3, i))
                ex2(i) = scale(4, i) * d(lmm(4, i))
                ey2(i) = scale(5, i) * d(lmm(5, i))
                ez2(i) = scale(6, i) * d(lmm(6, i))
                ex3(i) = scale(7, i) * d(lmm(7, i))
                ey3(i) = scale(8, i) * d(lmm(8, i))
                ez3(i) = scale(9, i) * d(lmm(9, i))
                ex4(i) = scale(10, i) * d(lmm(10, i))
                ey4(i) = scale(11, i) * d(lmm(11, i))
                ez4(i) = scale(12, i) * d(lmm(12, i))
                ex5(i) = scale(13, i) * d(lmm(13, i))
                ey5(i) = scale(14, i) * d(lmm(14, i))
                ez5(i) = scale(15, i) * d(lmm(15, i))
                ex6(i) = scale(16, i) * d(lmm(16, i))
                ey6(i) = scale(17, i) * d(lmm(17, i))
                ez6(i) = scale(18, i) * d(lmm(18, i))
                ex7(i) = scale(19, i) * d(lmm(19, i))
                ey7(i) = scale(20, i) * d(lmm(20, i))
                ez7(i) = scale(21, i) * d(lmm(21, i))
                ex8(i) = scale(22, i) * d(lmm(22, i))
                ey8(i) = scale(23, i) * d(lmm(23, i))
                ez8(i) = scale(24, i) * d(lmm(24, i))
                'YC 102418 END

            Next

            For i = lft To llt

                'YC 102418
                'fx1(i) = scale(0, i) * usi(lmm(0, i))
                'fy1(i) = scale(1, i) * usi(lmm(1, i))
                'fz1(i) = scale(2, i) * usi(lmm(2, i))
                'fx2(i) = scale(3, i) * usi(lmm(3, i))
                'fy2(i) = scale(4, i) * usi(lmm(4, i))
                'fz2(i) = scale(5, i) * usi(lmm(5, i))
                'fx3(i) = scale(6, i) * usi(lmm(6, i))
                'fy3(i) = scale(7, i) * usi(lmm(7, i))
                'fz3(i) = scale(8, i) * usi(lmm(8, i))
                'fx4(i) = scale(9, i) * usi(lmm(9, i))
                'fy4(i) = scale(10, i) * usi(lmm(10, i))
                'fz4(i) = scale(11, i) * usi(lmm(11, i))
                'fx5(i) = scale(12, i) * usi(lmm(12, i))
                'fy5(i) = scale(13, i) * usi(lmm(13, i))
                'fz5(i) = scale(14, i) * usi(lmm(14, i))
                'fx6(i) = scale(15, i) * usi(lmm(15, i))
                'fy6(i) = scale(16, i) * usi(lmm(16, i))
                'fz6(i) = scale(17, i) * usi(lmm(17, i))
                'fx7(i) = scale(18, i) * usi(lmm(18, i))
                'fy7(i) = scale(19, i) * usi(lmm(19, i))
                'fz7(i) = scale(20, i) * usi(lmm(20, i))
                'fx8(i) = scale(21, i) * usi(lmm(21, i))
                'fy8(i) = scale(22, i) * usi(lmm(22, i))
                'fz8(i) = scale(23, i) * usi(lmm(23, i))
                fx1(i) = scale(1, i) * usi(lmm(1, i))
                fy1(i) = scale(2, i) * usi(lmm(2, i))
                fz1(i) = scale(3, i) * usi(lmm(3, i))
                fx2(i) = scale(4, i) * usi(lmm(4, i))
                fy2(i) = scale(5, i) * usi(lmm(5, i))
                fz2(i) = scale(6, i) * usi(lmm(6, i))
                fx3(i) = scale(7, i) * usi(lmm(7, i))
                fy3(i) = scale(8, i) * usi(lmm(8, i))
                fz3(i) = scale(9, i) * usi(lmm(9, i))
                fx4(i) = scale(10, i) * usi(lmm(10, i))
                fy4(i) = scale(11, i) * usi(lmm(11, i))
                fz4(i) = scale(12, i) * usi(lmm(12, i))
                fx5(i) = scale(13, i) * usi(lmm(13, i))
                fy5(i) = scale(14, i) * usi(lmm(14, i))
                fz5(i) = scale(15, i) * usi(lmm(15, i))
                fx6(i) = scale(16, i) * usi(lmm(16, i))
                fy6(i) = scale(17, i) * usi(lmm(17, i))
                fz6(i) = scale(18, i) * usi(lmm(18, i))
                fx7(i) = scale(19, i) * usi(lmm(19, i))
                fy7(i) = scale(20, i) * usi(lmm(20, i))
                fz7(i) = scale(21, i) * usi(lmm(21, i))
                fx8(i) = scale(22, i) * usi(lmm(22, i))
                fy8(i) = scale(23, i) * usi(lmm(23, i))
                fz8(i) = scale(24, i) * usi(lmm(24, i))
                'YC 102418 END

            Next

            If ibkflg = 0 OrElse ibkflg = 1 Then

                'c.... now know converged value of line search parameter
                'c     so update history variables for incompatible increments over _step

                For i = lft To llt

                    ' YC 102418
                    'usic(0, nftm1 + i) = usic(0, nftm1 + i) + stepsv * dusic(0, nftm1 + i)
                    'usic(1, nftm1 + i) = usic(1, nftm1 + i) + stepsv * dusic(1, nftm1 + i)
                    'usic(2, nftm1 + i) = usic(2, nftm1 + i) + stepsv * dusic(2, nftm1 + i)
                    'usic(3, nftm1 + i) = usic(3, nftm1 + i) + stepsv * dusic(3, nftm1 + i)
                    'usic(4, nftm1 + i) = usic(4, nftm1 + i) + stepsv * dusic(4, nftm1 + i)
                    'usic(5, nftm1 + i) = usic(5, nftm1 + i) + stepsv * dusic(5, nftm1 + i)
                    'usic(6, nftm1 + i) = usic(6, nftm1 + i) + stepsv * dusic(6, nftm1 + i)
                    'usic(7, nftm1 + i) = usic(7, nftm1 + i) + stepsv * dusic(7, nftm1 + i)
                    'usic(8, nftm1 + i) = usic(8, nftm1 + i) + stepsv * dusic(8, nftm1 + i)
                    usic(1, nftm1 + i) = usic(1, nftm1 + i) + stepsv * dusic(1, nftm1 + i)
                    usic(2, nftm1 + i) = usic(2, nftm1 + i) + stepsv * dusic(2, nftm1 + i)
                    usic(3, nftm1 + i) = usic(3, nftm1 + i) + stepsv * dusic(3, nftm1 + i)
                    usic(4, nftm1 + i) = usic(4, nftm1 + i) + stepsv * dusic(4, nftm1 + i)
                    usic(5, nftm1 + i) = usic(5, nftm1 + i) + stepsv * dusic(5, nftm1 + i)
                    usic(6, nftm1 + i) = usic(6, nftm1 + i) + stepsv * dusic(6, nftm1 + i)
                    usic(7, nftm1 + i) = usic(7, nftm1 + i) + stepsv * dusic(7, nftm1 + i)
                    usic(8, nftm1 + i) = usic(8, nftm1 + i) + stepsv * dusic(8, nftm1 + i)
                    usic(9, nftm1 + i) = usic(9, nftm1 + i) + stepsv * dusic(9, nftm1 + i)
                    ' YC 102418

                Next
            End If

            If ibkflg = 1 Then

                'c.... a global backsolve has just occured, so solve for
                'c     corresponding incompatible displacement increments

                For i = lft To llt

                    'YC 102418
                    'rx1(i) = scale(0, i) * tvc2(lmm(0, i))
                    'ry1(i) = scale(1, i) * tvc2(lmm(1, i))
                    'rz1(i) = scale(2, i) * tvc2(lmm(2, i))
                    'rx2(i) = scale(3, i) * tvc2(lmm(3, i))
                    'ry2(i) = scale(4, i) * tvc2(lmm(4, i))
                    'rz2(i) = scale(5, i) * tvc2(lmm(5, i))
                    'rx3(i) = scale(6, i) * tvc2(lmm(6, i))
                    'ry3(i) = scale(7, i) * tvc2(lmm(7, i))
                    'rz3(i) = scale(8, i) * tvc2(lmm(8, i))
                    'rx4(i) = scale(9, i) * tvc2(lmm(9, i))
                    'ry4(i) = scale(10, i) * tvc2(lmm(10, i))
                    'rz4(i) = scale(11, i) * tvc2(lmm(11, i))
                    'rx5(i) = scale(12, i) * tvc2(lmm(12, i))
                    'ry5(i) = scale(13, i) * tvc2(lmm(13, i))
                    'rz5(i) = scale(14, i) * tvc2(lmm(14, i))
                    'rx6(i) = scale(15, i) * tvc2(lmm(15, i))
                    'ry6(i) = scale(16, i) * tvc2(lmm(16, i))
                    'rz6(i) = scale(17, i) * tvc2(lmm(17, i))
                    'rx7(i) = scale(18, i) * tvc2(lmm(18, i))
                    'ry7(i) = scale(19, i) * tvc2(lmm(19, i))
                    'rz7(i) = scale(20, i) * tvc2(lmm(20, i))
                    'rx8(i) = scale(21, i) * tvc2(lmm(21, i))
                    'ry8(i) = scale(22, i) * tvc2(lmm(22, i))
                    'rz8(i) = scale(23, i) * tvc2(lmm(23, i))
                    rx1(i) = scale(1, i) * tvc2(lmm(1, i))
                    ry1(i) = scale(2, i) * tvc2(lmm(2, i))
                    rz1(i) = scale(3, i) * tvc2(lmm(3, i))
                    rx2(i) = scale(4, i) * tvc2(lmm(4, i))
                    ry2(i) = scale(5, i) * tvc2(lmm(5, i))
                    rz2(i) = scale(6, i) * tvc2(lmm(6, i))
                    rx3(i) = scale(7, i) * tvc2(lmm(7, i))
                    ry3(i) = scale(8, i) * tvc2(lmm(8, i))
                    rz3(i) = scale(9, i) * tvc2(lmm(9, i))
                    rx4(i) = scale(10, i) * tvc2(lmm(10, i))
                    ry4(i) = scale(11, i) * tvc2(lmm(11, i))
                    rz4(i) = scale(12, i) * tvc2(lmm(12, i))
                    rx5(i) = scale(13, i) * tvc2(lmm(13, i))
                    ry5(i) = scale(14, i) * tvc2(lmm(14, i))
                    rz5(i) = scale(15, i) * tvc2(lmm(15, i))
                    rx6(i) = scale(16, i) * tvc2(lmm(16, i))
                    ry6(i) = scale(17, i) * tvc2(lmm(17, i))
                    rz6(i) = scale(18, i) * tvc2(lmm(18, i))
                    rx7(i) = scale(19, i) * tvc2(lmm(19, i))
                    ry7(i) = scale(20, i) * tvc2(lmm(20, i))
                    rz7(i) = scale(21, i) * tvc2(lmm(21, i))
                    rx8(i) = scale(22, i) * tvc2(lmm(22, i))
                    ry8(i) = scale(23, i) * tvc2(lmm(23, i))
                    rz8(i) = scale(24, i) * tvc2(lmm(24, i))
                    'YC 102418 END
                Next

                'YC 092018
                'Call recvri(rx1, sic(0, nft), dusic(0, nft), ric(0, nft), lft, llt)

                Call objComsub.ArrayExtract3Dfrom3D(sic, 0, 0, nft - 1, sic_nft, 33, 9, llt)
                'Call objComsub.ArrayConvert2Dto3D(sic_nft, 297, llt, sic_nft_3D, 33, 9, llt)

                Dim dusic_nft(9, llt), ric_nft(9, llt) As Double
                Call objComsub.ArrayExtract2Dfrom2D(dusic, 0, nft - 1, dusic_nft, 9, llt)
                Call objComsub.ArrayExtract2Dfrom2D(ric, 0, nft - 1, ric_nft, 9, llt)

                Dim delus(64, 24), delus_1D(64 * 24) As Double
                Call objComsub.ArrayInsert1Dto1D(rx1, 64, delus_1D, 64 * (0 * 3 + 0))   ' QW 12-12-2018-
                Call objComsub.ArrayInsert1Dto1D(ry1, 64, delus_1D, 64 * (0 * 3 + 1))
                Call objComsub.ArrayInsert1Dto1D(rz1, 64, delus_1D, 64 * (0 * 3 + 2))
                Call objComsub.ArrayInsert1Dto1D(rx2, 64, delus_1D, 64 * (1 * 3 + 0))
                Call objComsub.ArrayInsert1Dto1D(ry2, 64, delus_1D, 64 * (1 * 3 + 1))
                Call objComsub.ArrayInsert1Dto1D(rz2, 64, delus_1D, 64 * (1 * 3 + 2))
                Call objComsub.ArrayInsert1Dto1D(rx3, 64, delus_1D, 64 * (2 * 3 + 0))
                Call objComsub.ArrayInsert1Dto1D(ry3, 64, delus_1D, 64 * (2 * 3 + 1))
                Call objComsub.ArrayInsert1Dto1D(rz3, 64, delus_1D, 64 * (2 * 3 + 2))
                Call objComsub.ArrayInsert1Dto1D(rx4, 64, delus_1D, 64 * (3 * 3 + 0))
                Call objComsub.ArrayInsert1Dto1D(ry4, 64, delus_1D, 64 * (3 * 3 + 1))
                Call objComsub.ArrayInsert1Dto1D(rz4, 64, delus_1D, 64 * (3 * 3 + 2))
                Call objComsub.ArrayInsert1Dto1D(rx5, 64, delus_1D, 64 * (4 * 3 + 0))
                Call objComsub.ArrayInsert1Dto1D(ry5, 64, delus_1D, 64 * (4 * 3 + 1))
                Call objComsub.ArrayInsert1Dto1D(rz5, 64, delus_1D, 64 * (4 * 3 + 2))
                Call objComsub.ArrayInsert1Dto1D(rx6, 64, delus_1D, 64 * (5 * 3 + 0))
                Call objComsub.ArrayInsert1Dto1D(ry6, 64, delus_1D, 64 * (5 * 3 + 1))
                Call objComsub.ArrayInsert1Dto1D(rz6, 64, delus_1D, 64 * (5 * 3 + 2))
                Call objComsub.ArrayInsert1Dto1D(rx7, 64, delus_1D, 64 * (6 * 3 + 0))
                Call objComsub.ArrayInsert1Dto1D(ry7, 64, delus_1D, 64 * (6 * 3 + 1))
                Call objComsub.ArrayInsert1Dto1D(rz7, 64, delus_1D, 64 * (6 * 3 + 2))
                Call objComsub.ArrayInsert1Dto1D(rx8, 64, delus_1D, 64 * (7 * 3 + 0))
                Call objComsub.ArrayInsert1Dto1D(ry8, 64, delus_1D, 64 * (7 * 3 + 1))
                Call objComsub.ArrayInsert1Dto1D(rz8, 64, delus_1D, 64 * (7 * 3 + 2))
                Call objComsub.ArrayConvert1Dto2D(delus_1D, delus, 64, 24)

                Call recvri(delus, sic_nft, dusic_nft, ric_nft, lft, llt)
                'YC  092018 END
                Call objComsub.ArrayInsert2Dto2D(dusic_nft, 9, llt, dusic, 0, nft - 1)
                ' QW 12-12-2018-
                'change rx1 to rx1,,,, rz8 by YC 092018
                'Call azero(rx1, 24 * nmel)
                Call azero(rx1, nmel)
                Call azero(ry1, nmel)
                Call azero(rz1, nmel)
                Call azero(rx2, nmel)
                Call azero(ry2, nmel)
                Call azero(rz2, nmel)
                Call azero(rx3, nmel)
                Call azero(ry3, nmel)
                Call azero(rz3, nmel)
                Call azero(rx4, nmel)
                Call azero(ry4, nmel)
                Call azero(rz4, nmel)
                Call azero(rx5, nmel)
                Call azero(ry5, nmel)
                Call azero(rz5, nmel)
                Call azero(rx6, nmel)
                Call azero(ry6, nmel)
                Call azero(rz6, nmel)
                Call azero(rx7, nmel)
                Call azero(ry7, nmel)
                Call azero(rz7, nmel)
                Call azero(rx8, nmel)
                Call azero(ry8, nmel)
                Call azero(rz8, nmel)
                'change rx1 to rx1,,,, rz8 by YC 092018 END


                _step = 1.0
            End If

            If ibkflg = 2 Then _step = stepls
            'Call Check13(ng, stepls, _step)
            For i = lft To llt

                ' YC 102418
                'fxic1(i) = usic(0, nftm1 + i) + _step * dusic(0, nftm1 + i)
                'fyic1(i) = usic(1, nftm1 + i) + _step * dusic(1, nftm1 + i)
                'fzic1(i) = usic(2, nftm1 + i) + _step * dusic(2, nftm1 + i)
                'fxic2(i) = usic(3, nftm1 + i) + _step * dusic(3, nftm1 + i)
                'fyic2(i) = usic(4, nftm1 + i) + _step * dusic(4, nftm1 + i)
                'fzic2(i) = usic(5, nftm1 + i) + _step * dusic(5, nftm1 + i)
                'fxic3(i) = usic(6, nftm1 + i) + _step * dusic(6, nftm1 + i)
                'fyic3(i) = usic(7, nftm1 + i) + _step * dusic(7, nftm1 + i)
                'fzic3(i) = usic(8, nftm1 + i) + _step * dusic(8, nftm1 + i)
                'exic1(i) = dic(0, nftm1 + i) + fxic1(i)
                'eyic1(i) = dic(1, nftm1 + i) + fyic1(i)
                'ezic1(i) = dic(2, nftm1 + i) + fzic1(i)
                'exic2(i) = dic(3, nftm1 + i) + fxic2(i)
                'eyic2(i) = dic(4, nftm1 + i) + fyic2(i)
                'ezic2(i) = dic(5, nftm1 + i) + fzic2(i)
                'exic3(i) = dic(6, nftm1 + i) + fxic3(i)
                'eyic3(i) = dic(7, nftm1 + i) + fyic3(i)
                'ezic3(i) = dic(8, nftm1 + i) + fzic3(i)
                fxic1(i) = usic(1, nftm1 + i) + _step * dusic(1, nftm1 + i)
                fyic1(i) = usic(2, nftm1 + i) + _step * dusic(2, nftm1 + i)
                fzic1(i) = usic(3, nftm1 + i) + _step * dusic(3, nftm1 + i)
                fxic2(i) = usic(4, nftm1 + i) + _step * dusic(4, nftm1 + i)
                fyic2(i) = usic(5, nftm1 + i) + _step * dusic(5, nftm1 + i)
                fzic2(i) = usic(6, nftm1 + i) + _step * dusic(6, nftm1 + i)
                fxic3(i) = usic(7, nftm1 + i) + _step * dusic(7, nftm1 + i)
                fyic3(i) = usic(8, nftm1 + i) + _step * dusic(8, nftm1 + i)
                fzic3(i) = usic(9, nftm1 + i) + _step * dusic(9, nftm1 + i)
                exic1(i) = dic(1, nftm1 + i) + fxic1(i)
                eyic1(i) = dic(2, nftm1 + i) + fyic1(i)
                ezic1(i) = dic(3, nftm1 + i) + fzic1(i)
                exic2(i) = dic(4, nftm1 + i) + fxic2(i)
                eyic2(i) = dic(5, nftm1 + i) + fyic2(i)
                ezic2(i) = dic(6, nftm1 + i) + fzic2(i)
                exic3(i) = dic(7, nftm1 + i) + fxic3(i)
                eyic3(i) = dic(8, nftm1 + i) + fyic3(i)
                ezic3(i) = dic(9, nftm1 + i) + fzic3(i)
                ' YC 102418 END

            Next
            'Call Check1D33(usic, dusic, fxic1, 1, nftm1, istep)
            If ibkflg = 0 Then

                'c.... new timestep is beginning, so update history variables
                'c     for total incompatible displacements and zero increments

                For i = lft To llt

                    ' YC 102418
                    'dic(0, nftm1 + i) = dic(0, nftm1 + i) + usic(0, nftm1 + i)
                    'dic(1, nftm1 + i) = dic(1, nftm1 + i) + usic(1, nftm1 + i)
                    'dic(2, nftm1 + i) = dic(2, nftm1 + i) + usic(2, nftm1 + i)
                    'dic(3, nftm1 + i) = dic(3, nftm1 + i) + usic(3, nftm1 + i)
                    'dic(4, nftm1 + i) = dic(4, nftm1 + i) + usic(4, nftm1 + i)
                    'dic(5, nftm1 + i) = dic(5, nftm1 + i) + usic(5, nftm1 + i)
                    'dic(6, nftm1 + i) = dic(6, nftm1 + i) + usic(6, nftm1 + i)
                    'dic(7, nftm1 + i) = dic(7, nftm1 + i) + usic(7, nftm1 + i)
                    'dic(8, nftm1 + i) = dic(8, nftm1 + i) + usic(8, nftm1 + i)
                    dic(1, nftm1 + i) = dic(1, nftm1 + i) + usic(1, nftm1 + i)
                    dic(2, nftm1 + i) = dic(2, nftm1 + i) + usic(2, nftm1 + i)
                    dic(3, nftm1 + i) = dic(3, nftm1 + i) + usic(3, nftm1 + i)
                    dic(4, nftm1 + i) = dic(4, nftm1 + i) + usic(4, nftm1 + i)
                    dic(5, nftm1 + i) = dic(5, nftm1 + i) + usic(5, nftm1 + i)
                    dic(6, nftm1 + i) = dic(6, nftm1 + i) + usic(6, nftm1 + i)
                    dic(7, nftm1 + i) = dic(7, nftm1 + i) + usic(7, nftm1 + i)
                    dic(8, nftm1 + i) = dic(8, nftm1 + i) + usic(8, nftm1 + i)
                    dic(9, nftm1 + i) = dic(9, nftm1 + i) + usic(9, nftm1 + i)
                    ' YC 102418 END

                Next


                'change usic(0,nft) by YC? 092018
                'Call azero(usic(0, nft), 9 * (llt - lft + 1))
                Dim usic_nft(9, llt - lft + 1) As Double
                Call objComsub.ArrayExtract2Dfrom2D(usic, 0, nft - 1, usic_nft, 9, llt - lft + 1)

                Call azero(usic_nft, 9, llt - lft + 1)

                Call objComsub.ArrayInsert2Dto2D(usic_nft, 9, llt - lft + 1, usic, 0, nft - 1)
                'change usic(0,nft) by YC? 092018 END


                _step = 0.0

            End If

            'c.... update stress, stress divergence; compute element stiffnesses

            Call stfic(nmel, ng)

            Call ldbdy(fval)

            If iphase < 3 AndAlso iref = 0 Then

                'c....... save unfactored k(ci) block in global storage

                'YC? 092018
                'Call blkcpy(ekic, sic(0, nft), llt * 297)

                'Dim ekic_1D(llt * 297), sic_nft_1D(llt * 297) As Double
                'Call objComsub.ArrayConvert3Dto1D(ekic, 33, 9, llt, ekic_1D)
                'Call blkcpy(ekic_1D, sic_nft_1D, 33 * 9 * llt)
                'Call objComsub.ArrayConvert1Dto2D(sic_nft_1D, sic_nft, 297, llt)
                'Call objComsub.ArrayInsert2Dto2D(sic_nft, 297, llt, sic, 1 - 1, nft - 1)
                'YC? 092018 END

                'sic_nft = ekic    ' QW 12-12-2018-
                Dim I1, J1, K1 As Integer
                For I1 = 1 To 33
                    For J1 = 1 To 9
                        For K1 = 1 To 64
                            sic_nft(I1, J1, K1) = ekic(I1, J1, K1)
                        Next
                    Next
                Next

                'c
                'c....... perform static condensation

                'YC 092018
                'Call statcn(lmm(23 * iprec + 1, 1), ekic, lft, llt)
                Call statcn(s, ekic, lft, llt)
                'YC 092018 END

                'c....... save factored k(ii) block in global storage



                'YC 092018
                'Call moveii(sic(0, nft), ekic, lft, llt)

                'Call objComsub.ArrayConvert2Dto3D(sic_nft, 297, llt, sic_nft_3D, 33, 9, llt)

                Call moveii(sic_nft, ekic, lft, llt)

                'Call objComsub.ArrayConvert3Dto2D(sic_nft_3D, 33, 9, llt, sic_nft, 297, llt)
                'YC 092018 END

                Call objComsub.ArrayInsert3Dto3D(sic_nft, 33, 9, llt, sic, 0, 0, nft - 1)   ' QW 12-12-2018-

            End If

            'c.... save incompatible residual in global storage

            For i = lft To llt

                ' YC 102418
                'ric(0, nftm1 + i) = -rxic1(i)
                'ric(1, nftm1 + i) = -ryic1(i)
                'ric(2, nftm1 + i) = -rzic1(i)
                'ric(3, nftm1 + i) = -rxic2(i)
                'ric(4, nftm1 + i) = -ryic2(i)
                'ric(5, nftm1 + i) = -rzic2(i)
                'ric(6, nftm1 + i) = -rxic3(i)
                'ric(7, nftm1 + i) = -ryic3(i)
                'ric(8, nftm1 + i) = -rzic3(i)
                ric(1, nftm1 + i) = -rxic1(i)
                ric(2, nftm1 + i) = -ryic1(i)
                ric(3, nftm1 + i) = -rzic1(i)
                ric(4, nftm1 + i) = -rxic2(i)
                ric(5, nftm1 + i) = -ryic2(i)
                ric(6, nftm1 + i) = -rzic2(i)
                ric(7, nftm1 + i) = -rxic3(i)
                ric(8, nftm1 + i) = -ryic3(i)
                ric(9, nftm1 + i) = -rzic3(i)
                ' YC 102418

            Next

            'c.... sum contribution of incompatible residual to global residual


            'YC 092018?
            'Call cndres(rx1, rxic1, sic(0, nft), lft, llt)

            Dim r(64, 24), r_1D(64 * 24) As Double
            Call objComsub.ArrayInsert1Dto1D(rx1, 64, r_1D, 64 * (0 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ry1, 64, r_1D, 64 * (0 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rz1, 64, r_1D, 64 * (0 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rx2, 64, r_1D, 64 * (1 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ry2, 64, r_1D, 64 * (1 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rz2, 64, r_1D, 64 * (1 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rx3, 64, r_1D, 64 * (2 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ry3, 64, r_1D, 64 * (2 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rz3, 64, r_1D, 64 * (2 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rx4, 64, r_1D, 64 * (3 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ry4, 64, r_1D, 64 * (3 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rz4, 64, r_1D, 64 * (3 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rx5, 64, r_1D, 64 * (4 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ry5, 64, r_1D, 64 * (4 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rz5, 64, r_1D, 64 * (4 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rx6, 64, r_1D, 64 * (5 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ry6, 64, r_1D, 64 * (5 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rz6, 64, r_1D, 64 * (5 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rx7, 64, r_1D, 64 * (6 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ry7, 64, r_1D, 64 * (6 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rz7, 64, r_1D, 64 * (6 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rx8, 64, r_1D, 64 * (7 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ry8, 64, r_1D, 64 * (7 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rz8, 64, r_1D, 64 * (7 * 3 + 2))
            Call objComsub.ArrayConvert1Dto2D(r_1D, r, 64, 24)

            Dim rxic(64, 9), rxic_1D(64 * 9) As Double
            Call objComsub.ArrayInsert1Dto1D(rxic1, 64, rxic_1D, 64 * (0 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ryic1, 64, rxic_1D, 64 * (0 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rzic1, 64, rxic_1D, 64 * (0 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rxic2, 64, rxic_1D, 64 * (1 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ryic2, 64, rxic_1D, 64 * (1 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rzic2, 64, rxic_1D, 64 * (1 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rxic3, 64, rxic_1D, 64 * (2 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(ryic3, 64, rxic_1D, 64 * (2 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rzic3, 64, rxic_1D, 64 * (2 * 3 + 2))
            Call objComsub.ArrayConvert1Dto2D(rxic_1D, rxic, 64, 9)
            'Call objComsub.ArrayConvert2Dto3D(sic_nft, 297, llt, sic_nft_3D, 33, 9, llt)

            Call cndres(r, rxic, sic_nft, lft, llt)

            ' YC 102418-012819
            Call objComsub.ArrayConvert2Dto1D(rxic, 64, 9, rxic_1D)
            Call objComsub.ArrayExtract1Dfrom1D(rxic_1D, 64 * (0 * 3 + 0), rxic1, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rxic_1D, 64 * (0 * 3 + 1), ryic1, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rxic_1D, 64 * (0 * 3 + 2), rzic1, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rxic_1D, 64 * (1 * 3 + 0), rxic2, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rxic_1D, 64 * (1 * 3 + 1), ryic2, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rxic_1D, 64 * (1 * 3 + 2), rzic2, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rxic_1D, 64 * (2 * 3 + 0), rxic3, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rxic_1D, 64 * (2 * 3 + 1), ryic3, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rxic_1D, 64 * (2 * 3 + 2), rzic3, 64)

            Call objComsub.ArrayConvert2Dto1D(r, 64, 24, r_1D)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (0 * 3 + 0), rx1, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (0 * 3 + 1), ry1, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (0 * 3 + 2), rz1, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (1 * 3 + 0), rx2, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (1 * 3 + 1), ry2, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (1 * 3 + 2), rz2, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (2 * 3 + 0), rx3, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (2 * 3 + 1), ry3, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (2 * 3 + 2), rz3, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (3 * 3 + 0), rx4, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (3 * 3 + 1), ry4, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (3 * 3 + 2), rz4, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (4 * 3 + 0), rx5, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (4 * 3 + 1), ry5, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (4 * 3 + 2), rz5, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (5 * 3 + 0), rx6, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (5 * 3 + 1), ry6, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (5 * 3 + 2), rz6, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (6 * 3 + 0), rx7, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (6 * 3 + 1), ry7, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (6 * 3 + 2), rz7, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (7 * 3 + 0), rx8, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (7 * 3 + 1), ry8, 64)
            Call objComsub.ArrayExtract1Dfrom1D(r_1D, 64 * (7 * 3 + 2), rz8, 64)

            'Call objComsub.ArrayConvert3Dto2D(sic_nft_3D, 33, 9, llt, sic_nft, 297, llt)

            'c     displacement boundary conditions

            'Call dispbc(icnt2, fval, rx1, rmx1, lmm(23 * iprec + 1, 1), lmm(0, 1), idid, lcd, amad, idflad, rf, iprec)
            Dim rm(64, 24), rm_1D(64 * 24) As Double
            Call objComsub.ArrayInsert1Dto1D(rmx1, 64, rm_1D, 64 * (0 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(rmy1, 64, rm_1D, 64 * (0 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rmz1, 64, rm_1D, 64 * (0 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rmx2, 64, rm_1D, 64 * (1 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(rmy2, 64, rm_1D, 64 * (1 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rmz2, 64, rm_1D, 64 * (1 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rmx3, 64, rm_1D, 64 * (2 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(rmy3, 64, rm_1D, 64 * (2 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rmz3, 64, rm_1D, 64 * (2 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rmx4, 64, rm_1D, 64 * (3 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(rmy4, 64, rm_1D, 64 * (3 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rmz4, 64, rm_1D, 64 * (3 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rmx5, 64, rm_1D, 64 * (4 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(rmy5, 64, rm_1D, 64 * (4 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rmz5, 64, rm_1D, 64 * (4 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rmx6, 64, rm_1D, 64 * (5 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(rmy6, 64, rm_1D, 64 * (5 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rmz6, 64, rm_1D, 64 * (5 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rmx7, 64, rm_1D, 64 * (6 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(rmy7, 64, rm_1D, 64 * (6 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rmz7, 64, rm_1D, 64 * (6 * 3 + 2))
            Call objComsub.ArrayInsert1Dto1D(rmx8, 64, rm_1D, 64 * (7 * 3 + 0))
            Call objComsub.ArrayInsert1Dto1D(rmy8, 64, rm_1D, 64 * (7 * 3 + 1))
            Call objComsub.ArrayInsert1Dto1D(rmz8, 64, rm_1D, 64 * (7 * 3 + 2))
            Call objComsub.ArrayConvert1Dto2D(rm_1D, rm, 64, 24)

            Call dispbc(icnt2, fval, r, rm, s, lmm, idid, lcd, amad, idflad, rf, iprec)
            'YC 092018? END

            Call objComsub.ArrayConvert2Dto1D(rm, 64, 24, rm_1D)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (0 * 3 + 0), rmx1, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (0 * 3 + 1), rmy1, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (0 * 3 + 2), rmz1, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (1 * 3 + 0), rmx2, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (1 * 3 + 1), rmy2, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (1 * 3 + 2), rmz2, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (2 * 3 + 0), rmx3, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (2 * 3 + 1), rmy3, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (2 * 3 + 2), rmz3, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (3 * 3 + 0), rmx4, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (3 * 3 + 1), rmy4, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (3 * 3 + 2), rmz4, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (4 * 3 + 0), rmx5, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (4 * 3 + 1), rmy5, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (4 * 3 + 2), rmz5, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (5 * 3 + 0), rmx6, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (5 * 3 + 1), rmy6, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (5 * 3 + 2), rmz6, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (6 * 3 + 0), rmx7, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (6 * 3 + 1), rmy7, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (6 * 3 + 2), rmz7, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (7 * 3 + 0), rmx8, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (7 * 3 + 1), rmy8, 64)
            Call objComsub.ArrayExtract1Dfrom1D(rm_1D, 64 * (7 * 3 + 2), rmz8, 64)
            ' YC 102418-012819 END

            For i = lft To llt

                ' YC 102418
                'rhs(lmm(0, i)) = rhs(lmm(0, i)) - scale(0, i) * rx1(i)
                'rhs(lmm(1, i)) = rhs(lmm(1, i)) - scale(1, i) * ry1(i)
                'rhs(lmm(2, i)) = rhs(lmm(2, i)) - scale(2, i) * rz1(i)
                'rhs(lmm(3, i)) = rhs(lmm(3, i)) - scale(3, i) * rx2(i)
                'rhs(lmm(4, i)) = rhs(lmm(4, i)) - scale(4, i) * ry2(i)
                'rhs(lmm(5, i)) = rhs(lmm(5, i)) - scale(5, i) * rz2(i)
                'rhs(lmm(6, i)) = rhs(lmm(6, i)) - scale(6, i) * rx3(i)
                'rhs(lmm(7, i)) = rhs(lmm(7, i)) - scale(7, i) * ry3(i)
                'rhs(lmm(8, i)) = rhs(lmm(8, i)) - scale(8, i) * rz3(i)
                'rhs(lmm(9, i)) = rhs(lmm(9, i)) - scale(9, i) * rx4(i)
                'rhs(lmm(10, i)) = rhs(lmm(10, i)) - scale(10, i) * ry4(i)
                'rhs(lmm(11, i)) = rhs(lmm(11, i)) - scale(11, i) * rz4(i)
                'rhs(lmm(12, i)) = rhs(lmm(12, i)) - scale(12, i) * rx5(i)
                'rhs(lmm(13, i)) = rhs(lmm(13, i)) - scale(13, i) * ry5(i)
                'rhs(lmm(14, i)) = rhs(lmm(14, i)) - scale(14, i) * rz5(i)
                'rhs(lmm(15, i)) = rhs(lmm(15, i)) - scale(15, i) * rx6(i)
                'rhs(lmm(16, i)) = rhs(lmm(16, i)) - scale(16, i) * ry6(i)
                'rhs(lmm(17, i)) = rhs(lmm(17, i)) - scale(17, i) * rz6(i)
                'rhs(lmm(18, i)) = rhs(lmm(18, i)) - scale(18, i) * rx7(i)
                'rhs(lmm(19, i)) = rhs(lmm(19, i)) - scale(19, i) * ry7(i)
                'rhs(lmm(20, i)) = rhs(lmm(20, i)) - scale(20, i) * rz7(i)
                'rhs(lmm(21, i)) = rhs(lmm(21, i)) - scale(21, i) * rx8(i)
                'rhs(lmm(22, i)) = rhs(lmm(22, i)) - scale(22, i) * ry8(i)
                'rhs(lmm(23, i)) = rhs(lmm(23, i)) - scale(23, i) * rz8(i)
                rhs(lmm(1, i)) = rhs(lmm(1, i)) - scale(1, i) * rx1(i)
                rhs(lmm(2, i)) = rhs(lmm(2, i)) - scale(2, i) * ry1(i)
                rhs(lmm(3, i)) = rhs(lmm(3, i)) - scale(3, i) * rz1(i)
                rhs(lmm(4, i)) = rhs(lmm(4, i)) - scale(4, i) * rx2(i)
                rhs(lmm(5, i)) = rhs(lmm(5, i)) - scale(5, i) * ry2(i)
                rhs(lmm(6, i)) = rhs(lmm(6, i)) - scale(6, i) * rz2(i)
                rhs(lmm(7, i)) = rhs(lmm(7, i)) - scale(7, i) * rx3(i)
                rhs(lmm(8, i)) = rhs(lmm(8, i)) - scale(8, i) * ry3(i)
                rhs(lmm(9, i)) = rhs(lmm(9, i)) - scale(9, i) * rz3(i)
                rhs(lmm(10, i)) = rhs(lmm(10, i)) - scale(10, i) * rx4(i)
                rhs(lmm(11, i)) = rhs(lmm(11, i)) - scale(11, i) * ry4(i)
                rhs(lmm(12, i)) = rhs(lmm(12, i)) - scale(12, i) * rz4(i)
                rhs(lmm(13, i)) = rhs(lmm(13, i)) - scale(13, i) * rx5(i)
                rhs(lmm(14, i)) = rhs(lmm(14, i)) - scale(14, i) * ry5(i)
                rhs(lmm(15, i)) = rhs(lmm(15, i)) - scale(15, i) * rz5(i)
                rhs(lmm(16, i)) = rhs(lmm(16, i)) - scale(16, i) * rx6(i)
                rhs(lmm(17, i)) = rhs(lmm(17, i)) - scale(17, i) * ry6(i)
                rhs(lmm(18, i)) = rhs(lmm(18, i)) - scale(18, i) * rz6(i)
                rhs(lmm(19, i)) = rhs(lmm(19, i)) - scale(19, i) * rx7(i)
                rhs(lmm(20, i)) = rhs(lmm(20, i)) - scale(20, i) * ry7(i)
                rhs(lmm(21, i)) = rhs(lmm(21, i)) - scale(21, i) * rz7(i)
                rhs(lmm(22, i)) = rhs(lmm(22, i)) - scale(22, i) * rx8(i)
                rhs(lmm(23, i)) = rhs(lmm(23, i)) - scale(23, i) * ry8(i)
                rhs(lmm(24, i)) = rhs(lmm(24, i)) - scale(24, i) * rz8(i)
                ' YC 102418 END
                'If iphase = 3 Then
                'Call Checkrhs(rhs, lmm, ng, i)
                'Dim rhsc = dotprd(rhs, rhs)
                'Call Checkrdot(rhsc, ng, i)
                'End If

            Next



            ' similar to clsStfic 320:, YC 102418
            'If iphase - 2 > 0 Then Continue For

            'If iref <> 0 Then Continue For

            If iphase - 2 < 0 Then
                GoTo 70
            ElseIf iphase - 2 = 0 Then
                GoTo 70
            ElseIf iphase - 2 > 0 Then
                GoTo 90
            End If

70:         If iref < 0 Then
                GoTo 90
            ElseIf iref = 0 Then
                GoTo 80
            ElseIf iref > 0 Then
                GoTo 90
            End If
            ' YC 102418 END


            'c....... store element arrays and form global block-diagonal           
            'c                                                                      
80:         For i = lft To llt

                ' YC? 092018
                'Call blkcpy(lmm(23 * iprec + 1, i), hxdata(kstore), ksizhx)
                'Call Check2D(hxdata1, ksizhx, 20)
                'Dim s_i_1D(ksizhx)
                'Dim hxdata_kstore(ksizhx) As Double
                'Call objComsub.ArrayExtract1Dfrom2D(s, 0, i - 1, s_i_1D, ksizhx)
                'Call objComsub.ArrayExtract1Dfrom2D(s, 0, i, hxdata_kstore, ksizhx)            ' QW 12-12-2018
                'Array.Copy(hxdata_kstore, 1, hxdata1, kstore, ksizhx)
                'Call objComsub.ArrayInsert2Dto2D(s, ksizhx, llt, hxdata1, 0, kstore - 1)
                For j = 1 To ksizhx
                    hxdata1(j, kstore + i - 1) = s(j, i)
                Next
                'Array.Copy(s, 0, hxdata1, kstore - 1, asize)
                'Call Check2DT(hxdata1, ksizhx, 10)
                'Call objComsub.ArrayInsert1Dto1D(hxdata_kstore, ksizhx, hxdata1, kstore - 1)

                'Call blkcpy(lmm(0, i), hxdata(lstore), neehx)
                'Dim lmm_i_1D(neehx) As Integer
                'Dim hxdata_lstore(neehx) As Integer
                'Call objComsub.ArrayExtract1Dfrom2D(lmm, 0, i - 1, lmm_i_1D, neehx)
                'Call objComsub.ArrayInsert2Dto2D(lmm, neehx, llt, hxdata2, 0, lstore - 1)      ' QW 12-12-2018
                For j = 1 To neehx
                    hxdata2(j, kstore + i - 1) = lmm(j, i)
                Next
                'Array.Copy(hxdata_lstore, 1, hxdata2, lstore, neehx)
                'Call Check2DT(hxdata2, neehx, lstore + 10)
                ' Call objComsub.ArrayInsert1Dto1D(hxdata_lstore, neehx, hxdata2, lstore - 1)
                ' YC? 092018 END

                'kstore = kstore + llt
                'lstore = lstore + llt
            Next

            'Call unpci(ixp(0, lblk))
            'Call unpci(ixp, 0, lblk - 1)   ' YC 102418
            Call unpci(ixp, 0, lblk - 1)

            ' YC? 092018
            'Call bdg83(lft, llt, diag, hxdata(kfirst), hxdata(lfirst), ix1, ldgen2)

            'Dim diag_2D(21, numnp) As Double
            'objComsub.ArrayConvert1Dto2D(diag, diag_2D, 21, numnp)

            Dim ien_1D(64 * 8), ien(64, 8) As Integer
            Call objComsub.ArrayInsert1Dto1D(ix1, 64, ien_1D, 64 * 0)
            Call objComsub.ArrayInsert1Dto1D(ix2, 64, ien_1D, 64 * 1)
            Call objComsub.ArrayInsert1Dto1D(ix3, 64, ien_1D, 64 * 2)
            Call objComsub.ArrayInsert1Dto1D(ix4, 64, ien_1D, 64 * 3)
            Call objComsub.ArrayInsert1Dto1D(ix5, 64, ien_1D, 64 * 4)
            Call objComsub.ArrayInsert1Dto1D(ix6, 64, ien_1D, 64 * 5)
            Call objComsub.ArrayInsert1Dto1D(ix7, 64, ien_1D, 64 * 6)
            Call objComsub.ArrayInsert1Dto1D(ix8, 64, ien_1D, 64 * 7)
            Call objComsub.ArrayConvert1Dto2D(ien_1D, ien, 64, 8)

            'Call bdg83(lft, llt, diag_2D, s, lmm, ien, ldgen2)
            Call bdg83(lft, llt, diag, s, lmm, ien, ldgen2)
            ' Call objComsub.ArrayConvert2Dto1D(diag_2D, 21, numnp, diag)

            'Dim hxdata_lfirst(llt * neehx) As Integer                             ' QW 12-12-2018
            'Dim I1, J1 As Integer
            'For I1 = 1 To neehx
            'For J1 = lft To llt
            'hxdata_lfirst((J1 - lft) * neehx + I1) = lmm(I1, J1)
            'Next J1
            'Next I1
            'Array.Copy(hxdata_lfirst, 1, hxdata2, lfirst, (llt - lft + 1) * neehx)

            For i = lft To llt
                'Call objComsub.ArrayInsert2Dto2D(lmm, neehx, llt, hxdata2, 0, lfirst - 1)
                For j = 1 To neehx
                    hxdata2(j, kstore + i - 1) = lmm(j, i)
                Next
                'lfirst = lfirst + llt
            Next
            kstore = kstore + llt                   ' QW 12-12-2018-

            'Call Check2D1(abdg, 2, 1921)
            ' YC? 092018 END 
            'Call Check2DT(r, 64, 24)
            'Next ng        'YC 102418
90:     Next ng

        'c   ... assemble rigid body elements in auxiliary storage ...
        If nauxrb <> 0 Then
            melemt = nauxrb
        End If

    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine fe3dic(d,rhs,usi,ixp,xyz,matype,rho,fval,
'     &                 id,diag,hxdata,icnt2,tvc2,sic,dic,usic,dusic,
'     &                 ric,numnp,nmmat,*)
'c
'      implicit double precision (a-h,o-z)                             
'c
'c===> module to drive hexahedral continuum element w/ incompatible modes
'c
'      common/bk01/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n14a
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e,
'     & n20f,n20g
'      common/bk10/iphase,nelgp,imass,lhex(9)     
'      common/bk20/kpri,nstep,ite,ilimit,iref
'      common/bk31/lmm(648,1)
'      common/double/iprec
'      common/bk35/numdc,numudc,nrcc
'c      
'      common/bk59/r(24)
'      common/fissl1/melemt,nnns,n2g,llls
'      common/block/nblk
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect1/
'     1 rx1(64),ry1(64),rz1(64),rx2(64),ry2(64),rz2(64),
'     2 rx3(64),ry3(64),rz3(64),rx4(64),ry4(64),rz4(64),
'     3 rx5(64),ry5(64),rz5(64),rx6(64),ry6(64),rz6(64),
'     4 rx7(64),ry7(64),rz7(64),rx8(64),ry8(64),rz8(64),
'     5 mtype(64),matp(64)
'      common/vect1alpha/
'     1 rmx1(64),rmy1(64),rmz1(64),rmx2(64),rmy2(64),rmz2(64),
'     2 rmx3(64),rmy3(64),rmz3(64),rmx4(64),rmy4(64),rmz4(64),
'     3 rmx5(64),rmy5(64),rmz5(64),rmx6(64),rmy6(64),rmz6(64),
'     4 rmx7(64),rmy7(64),rmz7(64),rmx8(64),rmy8(64),rmz8(64)
'      common/vect2/
'     1 ex1(64),ey1(64),ez1(64),ex2(64),ey2(64),ez2(64),
'     2 ex3(64),ey3(64),ez3(64),ex4(64),ey4(64),ez4(64),
'     3 ex5(64),ey5(64),ez5(64),ex6(64),ey6(64),ez6(64),
'     4 ex7(64),ey7(64),ez7(64),ex8(64),ey8(64),ez8(64),
'     5 fx1(64),fy1(64),fz1(64),fx2(64),fy2(64),fz2(64),
'     6 fx3(64),fy3(64),fz3(64),fx4(64),fy4(64),fz4(64),
'     7 fx5(64),fy5(64),fz5(64),fx6(64),fy6(64),fz6(64),
'     8 fx7(64),fy7(64),fz7(64),fx8(64),fy8(64),fz8(64)
'      common/vect4/
'     1 ix1(64),ix2(64),ix3(64),ix4(64),ix5(64),ix6(64),ix7(64),ix8(64),
'     & dum4(16,64)
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
'      common/vect9/scale(24,64),ym(8,64),ro(64),rov(64),tmecc(64)
'      common/vect17/betap(64),dum17(192,64)
'      common/vect18/sclo(64),dum18(512)
'      common/vect54/ekic(33,9,64)
'      common/vect55/
'     1 hic11(64),hic21(64),hic31(64),hic12(64),hic22(64),hic32(64),
'     2 hic13(64),hic23(64),hic33(64),exic1(64),eyic1(64),ezic1(64),
'     3 exic2(64),eyic2(64),ezic2(64),exic3(64),eyic3(64),ezic3(64),
'     4 fxic1(64),fyic1(64),fzic1(64),fxic2(64),fyic2(64),fzic2(64),
'     5 fxic3(64),fyic3(64),fzic3(64),rxic1(64),ryic1(64),rzic1(64),
'     6 rxic2(64),ryic2(64),rzic2(64),rxic3(64),ryic3(64),rzic3(64)
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye3/neehx,ksizhx,neesf,ksizsf
'c
'      logical ldgen2 
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      common/bkneq/neql
'      logical lcmpr
'      common/bkrb3/icmpr(64),lcmpr
'      common/ibc/noded(1000),idid(1000),lcd(1000),amad(1000),
'     & idflad(1000),rf(1000),rfa(1000)
'c
'      logical lelstf
'      common/elstfb/lelstf
'c      
'      common/aa/anegb1(7508928),anegb2(7508928)
'      logical lstif,lskip
'c
'c	------------- Added by Qiang, 03-09-2010 --------------------------
'	common/inf/dro(64)					
'c	-------------------------------------------------------------------
'c
'      dimension d(*),rhs(*),xyz(3,*),matype(*),
'     1   usi(*),rho(*),id(6,*),fval(*),ixp(9,*), 
'     2   tvc2(*),sic(297,*),dic(9,*),usic(9,*),dusic(9,*),ric(9,*),   
'     3   diag(*),hxdata(*)
'c
'      equivalence (lhex(2),numel),(lhex(4),numelh)                   
'c
'            kstore = 1 + nblk*ksizhx                                   
'            lstore = 1 + numelh*ksizhx + nblk*neehx                    
'c
'      nelg=(numel-1)/64+1
'      nmel=64
'      nauxrb=0
'      lcmpr=.false.
'c
'      do 90 ng=1,nelg
'      lft=1
'      llt=min0(64,numel-64*(ng-1))
'      nft=1+(ng-1)*64
'      nlt=min0(numel,ng*64)
'      nftm1=nft-1
'      kfirst = kstore                                                  
'      lfirst = lstore                                                  
'c
'c.... zero compatable arrays
'      call azero(lmm,324*nmel)
'      call azero(rx1,24 *nmel)
'
'      do 6 i=1,64
'       mtype(i)=0
'    6  matp(i)=0
'      call azero(ex1,48 *nmel)
'      call azero(ym ,9  *nmel)
'c.... zero incompatible arrays
'      call azero(rxic1,9*nmel)
'      call azero(ekic,297*nmel)
'c
'c.... unpack element connectivities
'      lblk=nblk+nft
'      call unpci(ixp(1,lblk))
'c
'c
'c.... test for hexagonal or tetragonal elements                         
'      ldgen2 = ldgen                                                   
'
'      if (.not.ldgen) then
'         do 8 i=lft,llt                                               
'    8    if (ix5(i).eq.ix6(i)) ldgen2 = .true.                        
'      endif                                                           
'c
'c.... gather initial coordinates into local vectors
'      do  9 i=lft,llt
'      f11(i)=xyz(1,ix1(i))
'      f21(i)=xyz(2,ix1(i))
'      f31(i)=xyz(3,ix1(i))
'      f12(i)=xyz(1,ix2(i))
'      f22(i)=xyz(2,ix2(i))
'      f32(i)=xyz(3,ix2(i))
'      f13(i)=xyz(1,ix3(i))
'      f23(i)=xyz(2,ix3(i))
'      f33(i)=xyz(3,ix3(i))
'      f14(i)=xyz(1,ix4(i))
'      f24(i)=xyz(2,ix4(i))
'      f34(i)=xyz(3,ix4(i))
'      f15(i)=xyz(1,ix5(i))
'      f25(i)=xyz(2,ix5(i))
'      f35(i)=xyz(3,ix5(i))
'      f16(i)=xyz(1,ix6(i))
'      f26(i)=xyz(2,ix6(i))
'      f36(i)=xyz(3,ix6(i))
'      f17(i)=xyz(1,ix7(i))
'      f27(i)=xyz(2,ix7(i))
'      f37(i)=xyz(3,ix7(i))
'      f18(i)=xyz(1,ix8(i))
'      f28(i)=xyz(2,ix8(i))
'      f38(i)=xyz(3,ix8(i))
'    9 continue
'c
'c     gather lm array into local vectors
'c
'      do 10 i=lft,llt
'      do 10 j=1,3                                                       vax
'      lmm(j ,i)=id(j,ix1(i))                                            vax
'      lmm(j+3 ,i)=id(j,ix2(i))                                          vax
'      lmm(j+6 ,i)=id(j,ix3(i))                                          vax
'      lmm(j+9,i)=id(j,ix4(i))                                           vax
'      lmm(j+12,i)=id(j,ix5(i))                                          vax
'      lmm(j+15,i)=id(j,ix6(i))                                          vax
'      lmm(j+18,i)=id(j,ix7(i))                                          vax
'      lmm(j+21,i)=id(j,ix8(i))                                          vax
'   10 continue
'c
'c       expand packed integers
'c
'c
'      do 20 i=lft,llt
'      mtype(i)=0
'      sclo(i) =1.0
'      if (matp(i).ne.0) then
'         mtype(i)=matype(matp(i))
'         ro(i)   =rho(matp(i))
'c
'c	------ Start of changed by Qiang 03-09-2009 ------------	 
'c	Comments:
'c		The density of infinite element is used for indicator of direction.
'c		Move and store the indicator of infinite direction to a new array.
'		if(mtype(i).eq.56) ro(i)=0.
'		dro(i)=rho(matp(i))
'c	------- End of changed by Qiang ------------------------
'c
'      endif
'   20 if (mtype(i).eq.2.or.mtype(i).eq.7.or.mtype(i).eq.14) sclo(i)=0.
'c
'      do 30 i=lft,llt
'      scale(1,i) =float((1+isign(1,lmm(1, i)-1))/2)
'      scale(2,i) =float((1+isign(1,lmm(2, i)-1))/2)
'      scale(3,i) =float((1+isign(1,lmm(3, i)-1))/2)
'      scale(4,i) =float((1+isign(1,lmm(4, i)-1))/2)
'      scale(5,i) =float((1+isign(1,lmm(5, i)-1))/2)
'      scale(6,i) =float((1+isign(1,lmm(6, i)-1))/2)
'      scale(7,i) =float((1+isign(1,lmm(7, i)-1))/2)
'      scale(8,i) =float((1+isign(1,lmm(8, i)-1))/2)
'      scale(9,i) =float((1+isign(1,lmm(9, i)-1))/2)
'      scale(10,i)=float((1+isign(1,lmm(10,i)-1))/2)
'      scale(11,i)=float((1+isign(1,lmm(11,i)-1))/2)
'      scale(12,i)=float((1+isign(1,lmm(12,i)-1))/2)
'      scale(13,i)=float((1+isign(1,lmm(13,i)-1))/2)
'      scale(14,i)=float((1+isign(1,lmm(14,i)-1))/2)
'      scale(15,i)=float((1+isign(1,lmm(15,i)-1))/2)
'      scale(16,i)=float((1+isign(1,lmm(16,i)-1))/2)
'      scale(17,i)=float((1+isign(1,lmm(17,i)-1))/2)
'      scale(18,i)=float((1+isign(1,lmm(18,i)-1))/2)
'      scale(19,i)=float((1+isign(1,lmm(19,i)-1))/2)
'      scale(20,i)=float((1+isign(1,lmm(20,i)-1))/2)
'      scale(21,i)=float((1+isign(1,lmm(21,i)-1))/2)
'      scale(22,i)=float((1+isign(1,lmm(22,i)-1))/2)
'      scale(23,i)=float((1+isign(1,lmm(23,i)-1))/2)
'      scale(24,i)=float((1+isign(1,lmm(24,i)-1))/2)
'   30 continue
'c
'
'      do 40 i=lft,llt
'      ex1(i)=scale(1,i) *d(lmm(1, i))
'      ey1(i)=scale(2,i) *d(lmm(2, i))
'      ez1(i)=scale(3,i) *d(lmm(3, i))
'      ex2(i)=scale(4,i) *d(lmm(4, i))
'      ey2(i)=scale(5,i) *d(lmm(5, i))
'      ez2(i)=scale(6,i) *d(lmm(6, i))
'      ex3(i)=scale(7,i) *d(lmm(7, i))
'      ey3(i)=scale(8,i) *d(lmm(8, i))
'      ez3(i)=scale(9,i) *d(lmm(9, i))
'      ex4(i)=scale(10,i)*d(lmm(10,i))
'      ey4(i)=scale(11,i)*d(lmm(11,i))
'      ez4(i)=scale(12,i)*d(lmm(12,i))
'      ex5(i)=scale(13,i)*d(lmm(13,i))
'      ey5(i)=scale(14,i)*d(lmm(14,i))
'      ez5(i)=scale(15,i)*d(lmm(15,i))
'      ex6(i)=scale(16,i)*d(lmm(16,i))
'      ey6(i)=scale(17,i)*d(lmm(17,i))
'      ez6(i)=scale(18,i)*d(lmm(18,i))
'      ex7(i)=scale(19,i)*d(lmm(19,i))
'      ey7(i)=scale(20,i)*d(lmm(20,i))
'      ez7(i)=scale(21,i)*d(lmm(21,i))
'      ex8(i)=scale(22,i)*d(lmm(22,i))
'      ey8(i)=scale(23,i)*d(lmm(23,i))
'      ez8(i)=scale(24,i)*d(lmm(24,i))
'   40 continue
'c
'      do 50 i=lft,llt
'      fx1(i)=scale(1,i) *usi(lmm(1, i))
'      fy1(i)=scale(2,i) *usi(lmm(2, i))
'      fz1(i)=scale(3,i) *usi(lmm(3, i))
'      fx2(i)=scale(4,i) *usi(lmm(4, i))
'      fy2(i)=scale(5,i) *usi(lmm(5, i))
'      fz2(i)=scale(6,i) *usi(lmm(6, i))
'      fx3(i)=scale(7,i) *usi(lmm(7, i))
'      fy3(i)=scale(8,i) *usi(lmm(8, i))
'      fz3(i)=scale(9,i) *usi(lmm(9, i))
'      fx4(i)=scale(10,i)*usi(lmm(10,i))
'      fy4(i)=scale(11,i)*usi(lmm(11,i))
'      fz4(i)=scale(12,i)*usi(lmm(12,i))
'      fx5(i)=scale(13,i)*usi(lmm(13,i))
'      fy5(i)=scale(14,i)*usi(lmm(14,i))
'      fz5(i)=scale(15,i)*usi(lmm(15,i))
'      fx6(i)=scale(16,i)*usi(lmm(16,i))
'      fy6(i)=scale(17,i)*usi(lmm(17,i))
'      fz6(i)=scale(18,i)*usi(lmm(18,i))
'      fx7(i)=scale(19,i)*usi(lmm(19,i))
'      fy7(i)=scale(20,i)*usi(lmm(20,i))
'      fz7(i)=scale(21,i)*usi(lmm(21,i))
'      fx8(i)=scale(22,i)*usi(lmm(22,i))
'      fy8(i)=scale(23,i)*usi(lmm(23,i))
'      fz8(i)=scale(24,i)*usi(lmm(24,i))
'   50 continue
'c
'      if (ibkflg.eq.0 .or. ibkflg.eq.1) then
'c
'c.... now know converged value of line search parameter
'c     so update history variables for incompatible increments over step
'c
'      do 52 i=lft,llt
'      usic(1,nftm1+i) = usic(1,nftm1+i) + stepsv*dusic(1,nftm1+i)
'      usic(2,nftm1+i) = usic(2,nftm1+i) + stepsv*dusic(2,nftm1+i)
'      usic(3,nftm1+i) = usic(3,nftm1+i) + stepsv*dusic(3,nftm1+i)
'      usic(4,nftm1+i) = usic(4,nftm1+i) + stepsv*dusic(4,nftm1+i)
'      usic(5,nftm1+i) = usic(5,nftm1+i) + stepsv*dusic(5,nftm1+i)
'      usic(6,nftm1+i) = usic(6,nftm1+i) + stepsv*dusic(6,nftm1+i)
'      usic(7,nftm1+i) = usic(7,nftm1+i) + stepsv*dusic(7,nftm1+i)
'      usic(8,nftm1+i) = usic(8,nftm1+i) + stepsv*dusic(8,nftm1+i)
'      usic(9,nftm1+i) = usic(9,nftm1+i) + stepsv*dusic(9,nftm1+i)
'   52 continue
'      endif
'c
'      if (ibkflg.eq.1) then
'c
'c.... a global backsolve has just occured, so solve for
'c     corresponding incompatible displacement increments
'c
'
'      do 54 i=lft,llt
'      rx1(i)=scale(1,i) *tvc2(lmm(1, i))
'      ry1(i)=scale(2,i) *tvc2(lmm(2, i))
'      rz1(i)=scale(3,i) *tvc2(lmm(3, i))
'      rx2(i)=scale(4,i) *tvc2(lmm(4, i))
'      ry2(i)=scale(5,i) *tvc2(lmm(5, i))
'      rz2(i)=scale(6,i) *tvc2(lmm(6, i))
'      rx3(i)=scale(7,i) *tvc2(lmm(7, i))
'      ry3(i)=scale(8,i) *tvc2(lmm(8, i))
'      rz3(i)=scale(9,i) *tvc2(lmm(9, i))
'      rx4(i)=scale(10,i)*tvc2(lmm(10,i))
'      ry4(i)=scale(11,i)*tvc2(lmm(11,i))
'      rz4(i)=scale(12,i)*tvc2(lmm(12,i))
'      rx5(i)=scale(13,i)*tvc2(lmm(13,i))
'      ry5(i)=scale(14,i)*tvc2(lmm(14,i))
'      rz5(i)=scale(15,i)*tvc2(lmm(15,i))
'      rx6(i)=scale(16,i)*tvc2(lmm(16,i))
'      ry6(i)=scale(17,i)*tvc2(lmm(17,i))
'      rz6(i)=scale(18,i)*tvc2(lmm(18,i))
'      rx7(i)=scale(19,i)*tvc2(lmm(19,i))
'      ry7(i)=scale(20,i)*tvc2(lmm(20,i))
'      rz7(i)=scale(21,i)*tvc2(lmm(21,i))
'      rx8(i)=scale(22,i)*tvc2(lmm(22,i))
'      ry8(i)=scale(23,i)*tvc2(lmm(23,i))
'      rz8(i)=scale(24,i)*tvc2(lmm(24,i))
'   54 continue
'c
'      call recvri(rx1,sic(1,nft),dusic(1,nft),ric(1,nft),lft,llt)
'      call azero(rx1,24*nmel)
'      step = 1.0
'      endif
'c
'      if (ibkflg.eq.2) step = stepls
'c
'      do 55 i=lft,llt
'      fxic1(i) = usic(1,nftm1+i) + step*dusic(1,nftm1+i)
'      fyic1(i) = usic(2,nftm1+i) + step*dusic(2,nftm1+i)
'      fzic1(i) = usic(3,nftm1+i) + step*dusic(3,nftm1+i)
'      fxic2(i) = usic(4,nftm1+i) + step*dusic(4,nftm1+i)
'      fyic2(i) = usic(5,nftm1+i) + step*dusic(5,nftm1+i)
'      fzic2(i) = usic(6,nftm1+i) + step*dusic(6,nftm1+i)
'      fxic3(i) = usic(7,nftm1+i) + step*dusic(7,nftm1+i)
'      fyic3(i) = usic(8,nftm1+i) + step*dusic(8,nftm1+i)
'      fzic3(i) = usic(9,nftm1+i) + step*dusic(9,nftm1+i)
'      exic1(i) = dic(1,nftm1+i) + fxic1(i)
'      eyic1(i) = dic(2,nftm1+i) + fyic1(i)
'      ezic1(i) = dic(3,nftm1+i) + fzic1(i)
'      exic2(i) = dic(4,nftm1+i) + fxic2(i)
'      eyic2(i) = dic(5,nftm1+i) + fyic2(i)
'      ezic2(i) = dic(6,nftm1+i) + fzic2(i)
'      exic3(i) = dic(7,nftm1+i) + fxic3(i)
'      eyic3(i) = dic(8,nftm1+i) + fyic3(i)
'      ezic3(i) = dic(9,nftm1+i) + fzic3(i)
'   55 continue
'c
'      if (ibkflg.eq.0) then
'c
'c.... new timestep is beginning, so update history variables
'c     for total incompatible displacements and zero increments
'c
'      do 53 i=lft,llt
'      dic(1,nftm1+i) = dic(1,nftm1+i) + usic(1,nftm1+i)
'      dic(2,nftm1+i) = dic(2,nftm1+i) + usic(2,nftm1+i)
'      dic(3,nftm1+i) = dic(3,nftm1+i) + usic(3,nftm1+i)
'      dic(4,nftm1+i) = dic(4,nftm1+i) + usic(4,nftm1+i)
'      dic(5,nftm1+i) = dic(5,nftm1+i) + usic(5,nftm1+i)
'      dic(6,nftm1+i) = dic(6,nftm1+i) + usic(6,nftm1+i)
'      dic(7,nftm1+i) = dic(7,nftm1+i) + usic(7,nftm1+i)
'      dic(8,nftm1+i) = dic(8,nftm1+i) + usic(8,nftm1+i)
'      dic(9,nftm1+i) = dic(9,nftm1+i) + usic(9,nftm1+i)
'   53 continue
'      call azero(usic(1,nft),9*(llt-lft+1))
'      step = 0.0
'      endif
'c
'c
'c.... update stress, stress divergence; compute element stiffnesses
'c
'      call stfic (nmel,*991)
'c
'      call ldbdy(fval)
'c
'      if (iphase.lt.3 .and. iref.eq.0) then
'c
'c....... save unfactored k(ci) block in global storage
'c
'         call blkcpy(ekic,sic(1,nft),llt*297)
'c
'c....... perform static condensation
'c
'         call statcn(lmm(24*iprec+1,1),ekic,lft,llt)
'c
'c....... save factored k(ii) block in global storage
'c
'         call moveii(sic(1,nft),ekic,lft,llt)
'c
'      endif
'c
'c.... save incompatible residual in global storage
'c                                                                        
'      do 57 i=lft,llt
'      ric(1,nftm1+i) = -rxic1(i)
'      ric(2,nftm1+i) = -ryic1(i)
'      ric(3,nftm1+i) = -rzic1(i)
'      ric(4,nftm1+i) = -rxic2(i)
'      ric(5,nftm1+i) = -ryic2(i)
'      ric(6,nftm1+i) = -rzic2(i)
'      ric(7,nftm1+i) = -rxic3(i)
'      ric(8,nftm1+i) = -ryic3(i)
'      ric(9,nftm1+i) = -rzic3(i)
'   57 continue
'c
'c.... sum contribution of incompatible residual to global residual
'c
'c
'      call cndres(rx1,rxic1,sic(1,nft),lft,llt)
'c
'c     displacement boundary conditions
'c
'      call dispbc(icnt2,fval,rx1,rmx1,lmm(24*iprec+1,1),lmm(1,1),
'     1 idid,lcd,amad,idflad,rf,iprec)
'c
'c
'      do 65 i=lft,llt
'      rhs(lmm(1,i)) =rhs(lmm(1,i)) -scale(1,i) *rx1(i)
'      rhs(lmm(2,i)) =rhs(lmm(2,i)) -scale(2,i) *ry1(i)
'      rhs(lmm(3,i)) =rhs(lmm(3,i)) -scale(3,i) *rz1(i)
'      rhs(lmm(4,i)) =rhs(lmm(4,i)) -scale(4,i) *rx2(i)
'      rhs(lmm(5,i)) =rhs(lmm(5,i)) -scale(5,i) *ry2(i)
'      rhs(lmm(6,i)) =rhs(lmm(6,i)) -scale(6,i) *rz2(i)
'      rhs(lmm(7,i)) =rhs(lmm(7,i)) -scale(7,i) *rx3(i)
'      rhs(lmm(8,i)) =rhs(lmm(8,i)) -scale(8,i) *ry3(i)
'      rhs(lmm(9,i)) =rhs(lmm(9,i)) -scale(9,i) *rz3(i)
'      rhs(lmm(10,i))=rhs(lmm(10,i))-scale(10,i)*rx4(i)
'      rhs(lmm(11,i))=rhs(lmm(11,i))-scale(11,i)*ry4(i)
'      rhs(lmm(12,i))=rhs(lmm(12,i))-scale(12,i)*rz4(i)
'      rhs(lmm(13,i))=rhs(lmm(13,i))-scale(13,i)*rx5(i)
'      rhs(lmm(14,i))=rhs(lmm(14,i))-scale(14,i)*ry5(i)
'      rhs(lmm(15,i))=rhs(lmm(15,i))-scale(15,i)*rz5(i)
'      rhs(lmm(16,i))=rhs(lmm(16,i))-scale(16,i)*rx6(i)
'      rhs(lmm(17,i))=rhs(lmm(17,i))-scale(17,i)*ry6(i)
'      rhs(lmm(18,i))=rhs(lmm(18,i))-scale(18,i)*rz6(i)
'      rhs(lmm(19,i))=rhs(lmm(19,i))-scale(19,i)*rx7(i)
'      rhs(lmm(20,i))=rhs(lmm(20,i))-scale(20,i)*ry7(i)
'      rhs(lmm(21,i))=rhs(lmm(21,i))-scale(21,i)*rz7(i)
'      rhs(lmm(22,i))=rhs(lmm(22,i))-scale(22,i)*rx8(i)
'      rhs(lmm(23,i))=rhs(lmm(23,i))-scale(23,i)*ry8(i)
'      rhs(lmm(24,i))=rhs(lmm(24,i))-scale(24,i)*rz8(i)
'   65 continue
'c
'      if (iphase-2) 70,70,90
'   70 if (iref) 90,80,90
'c
'c                                                                       
'c....... store element arrays and form global block-diagonal           
'c                                                                      
'   80    do 85 i=lft,llt            
'c            write(150,860) i,',',kstore,',',ksizhx,',',lstore,',',neehx
'            call blkcpy(lmm(24*iprec+1,i),hxdata(kstore),ksizhx)       
'            call blkcpy(lmm(1,i),hxdata(lstore),neehx)                 
'            kstore = kstore + ksizhx                                   
'            lstore = lstore + neehx                                    
'   85    continue                                                      
'         call unpci (ixp(1,lblk))                                      
'         call bdg83(lft,llt,diag,hxdata(kfirst),                       
'     &              hxdata(lfirst),ix1,ldgen2)                         
'c
'   90 continue
'c
'c   ... assemble rigid body elements in auxiliary storage ...
'      if (nauxrb.ne.0) then
'         melemt=nauxrb
'
'      endif
'
'      return
'c
'  991 continue
'
'      return 1
'      end

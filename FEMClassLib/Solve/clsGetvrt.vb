'clsGetvrt
'This file contains all the methods of getvrt.f
Partial Public Class clsSolve

    Dim c11(64), c22(64), c33(64), c12(64), c13(64), c23(64) As Double

    Dim c1212(64), c2323(64), c1313(64), c2313(64), c1223(64), c1213(64) As Double

    Dim s11_vect5(64), s22_vect5(64), s33_vect5(64), s12_vect5(64), s13_vect5(64), s23_vect5(64) As Double

    Dim ui1(64), ui2(64), ui3(64), ui1s(64) As Double

    Dim q(64), r_vect5(64), xmod(64), scl1(64) As Double

    Dim scl2(64), scl0(64), sdetm(64), ct3(64), st3(64) As Double

    Dim lamda1(64), lamda2(64), lamda3(64) As Double

    Dim li1(64), li2(64), li3(64) As Double

    Dim si11(64), si22(64), si33(64), si12(64), si13(64), si23(64) As Double

    Dim ui11(64), ui22(64), ui33(64), ui12(64), ui13(64), ui23(64) As Double



    ''' <summary>
    ''' compute rotation matrix from polar decomposition of deformation gradient
    ''' </summary>
    ''' <param name="q11"></param>
    ''' <param name="q12"></param>
    ''' <param name="q13"></param>
    ''' <param name="q21"></param>
    ''' <param name="q22"></param>
    ''' <param name="q23"></param>
    ''' <param name="q31"></param>
    ''' <param name="q32"></param>
    ''' <param name="q33"></param>
    Public Sub getvrt(ByRef q11() As Double, ByRef q12() As Double, ByRef q13() As Double,
                      ByRef q21() As Double, ByRef q22() As Double, ByRef q23() As Double,
                      ByRef q31() As Double, ByRef q32() As Double, ByRef q33() As Double)

        Dim o3 = 0.33333333333333331

        Dim zero = 0.0
        Dim one = 1.0
        Dim half = 0.5
        Dim eps = 1.0E-30
        Dim root3 = Math.Sqrt(3.0)

        'moved to avoid duplicate name by YC 102418
        Dim f11(), f22(), f33(), f12(), f13(), f21(), f23(), f31(), f32() As Double
        f11 = f1 : f22 = f2 : f33 = f3 : f12 = f4 : f13 = f5 : f21 = f6 : f23 = f7 : f31 = f8 : f32 = f9 'vect5

        Dim s11(64), s22(64), s33(64), s12(64), s13(64), s23(64) As Double
        s11 = s11_vect5 : s22 = s22_vect5 : s33 = s33_vect5 : s12 = s12_vect5 : s13 = s13_vect5 : s23 = s23_vect5 : r = r_vect5
        '  by YC 102418 END


        'For i = lft - 1 To llt - 1    'YC 102418
        For i = lft To llt
            c11(i) = f11(i) * f11(i) + f21(i) * f21(i) + f31(i) * f31(i)
            c12(i) = f11(i) * f12(i) + f21(i) * f22(i) + f31(i) * f32(i)
            c13(i) = f11(i) * f13(i) + f21(i) * f23(i) + f31(i) * f33(i)
            c23(i) = f12(i) * f13(i) + f22(i) * f23(i) + f32(i) * f33(i)
            c22(i) = f12(i) * f12(i) + f22(i) * f22(i) + f32(i) * f32(i)
            c33(i) = f13(i) * f13(i) + f23(i) * f23(i) + f33(i) * f33(i)
        Next

        'For i = lft - 1 To llt - 1     'YC 102418
        For i = lft To llt
            c1212(i) = c12(i) * c12(i)
            c1313(i) = c13(i) * c13(i)
            c2323(i) = c23(i) * c23(i)
            c2313(i) = c23(i) * c13(i)
            c1223(i) = c12(i) * c23(i)
            c1213(i) = c12(i) * c13(i)
            s11(i) = c22(i) * c33(i) - c2323(i)
            ui1(i) = o3 * (c11(i) + c22(i) + c33(i))
            ui2(i) = s11(i) + c11(i) * c22(i) + c33(i) * c11(i) - c1212(i) - c1313(i)
            ui3(i) = c11(i) * s11(i) + c12(i) * (c2313(i) - c12(i) * c33(i)) +
                    c13(i) * (c1223(i) - c22(i) * c13(i))
            ui1s(i) = ui1(i) * ui1(i)
        Next

        'For i = lft - 1 To llt - 1  'YC 102418
        For i = lft To llt

            q(i) = Math.Sqrt(-Math.Min(o3 * ui2(i) - ui1s(i), zero))
            'q(i) = Math.Sqrt(Math.Max(Math.Abs(o3 * ui2(i) - ui1s(i)), zero))   ' YC? 102418

            r(i) = 0.5 * (ui3(i) - ui1(i) * ui2(i)) + ui1(i) * ui1s(i)
            xmod(i) = q(i) * q(i) * q(i)

            'scl1(i) = 0.5 + clsInput.sign(half, xmod(i) - eps)  ' YC 092018
            scl1(i) = 0.5 + objComsub.sign(half, xmod(i) - eps) ' YC? 102418 
        Next

        'For i = lft - 1 To llt - 1  'YC 102418
        For i = lft To llt

            'scl2(i) = 0.5 + clsInput.sign(half, xmod(i) - Math.Abs(r(i)))  ' by YC 092018
            scl2(i) = 0.5 + objComsub.sign(half, xmod(i) - Math.Abs(r(i)))

            scl0(i) = Math.Min(scl1(i), scl2(i))
            scl1(i) = 1 - scl0(i)
            sdetm(i) = Math.Acos(r(i) / (xmod(i) + scl1(i))) * o3
            q(i) = scl0(i) * q(i)
            ct3(i) = q(i) * Math.Cos(sdetm(i))
            st3(i) = q(i) * root3 * Math.Sin(sdetm(i))
        Next

        'For i = lft - 1 To llt - 1     'YC 102418
        For i = lft To llt
            sdetm(i) = scl1(i) * Math.Sqrt(Math.Max(zero, r(i)))
            lamda1(i) = Math.Sqrt(2.0 * (ct3(i) + sdetm(i)) + ui1(i))
            lamda2(i) = Math.Sqrt(-ct3(i) + st3(i) - sdetm(i) + ui1(i))
            lamda3(i) = Math.Sqrt(-ct3(i) - st3(i) - sdetm(i) + ui1(i))
        Next

        'For i = lft - 1 To llt - 1      'YC 102418
        For i = lft To llt
            sdetm(i) = lamda1(i) * lamda2(i)
            li1(i) = lamda1(i) + lamda2(i) + lamda3(i)
            li2(i) = sdetm(i) + lamda2(i) * lamda3(i) + lamda3(i) * lamda1(i)
            li3(i) = sdetm(i) * lamda3(i) / li1(i)
        Next

        'For i = lft - 1 To llt - 1     'YC 102418
        For i = lft To llt
            s11(i) = c11(i) + li3(i)
            s22(i) = c22(i) + li3(i)
            s33(i) = c33(i) + li3(i)
            s12(i) = c2313(i) - c12(i) * s33(i)
            s13(i) = c1223(i) - s22(i) * c13(i)
            s23(i) = -c2323(i) + s22(i) * s33(i)
            sdetm(i) = 1 / (li1(i) * (s11(i) * s23(i) + c12(i) * s12(i) + c13(i) * s13(i)))
        Next

        'For i = lft - 1 To llt - 1     'YC 102418
        For i = lft To llt
            c11(i) = c11(i) + li2(i)
            c22(i) = c22(i) + li2(i)
            c33(i) = c33(i) + li2(i)
            si11(i) = sdetm(i) * s23(i)
            si12(i) = sdetm(i) * s12(i)
            si13(i) = sdetm(i) * s13(i)
            si22(i) = sdetm(i) * (s11(i) * s33(i) - c1313(i))
            si23(i) = sdetm(i) * (-s11(i) * c23(i) + c1213(i))
            si33(i) = sdetm(i) * (s11(i) * s22(i) - c1212(i))
        Next

        'For i = lft - 1 To llt - 1     'YC 102418
        For i = lft To llt
            s12(i) = c12(i) * si12(i)
            s13(i) = c13(i) * si13(i)
            s23(i) = c23(i) * si23(i)
            ui11(i) = c11(i) * si11(i) + s12(i) + s13(i)
            ui22(i) = s12(i) + c22(i) * si22(i) + s23(i)
            ui33(i) = s13(i) + s23(i) + c33(i) * si33(i)
            ui12(i) = c11(i) * si12(i) + c12(i) * si22(i) + c13(i) * si23(i)
            ui13(i) = c11(i) * si13(i) + c12(i) * si23(i) + c13(i) * si33(i)
            ui23(i) = c12(i) * si13(i) + c22(i) * si23(i) + c23(i) * si33(i)
        Next

        'For i = lft - 1 To llt - 1     'YC 102418
        For i = lft To llt
            q11(i) = f11(i) * ui11(i) + f12(i) * ui12(i) + f13(i) * ui13(i)
            q12(i) = f11(i) * ui12(i) + f12(i) * ui22(i) + f13(i) * ui23(i)
            q13(i) = f11(i) * ui13(i) + f12(i) * ui23(i) + f13(i) * ui33(i)
            q21(i) = f21(i) * ui11(i) + f22(i) * ui12(i) + f23(i) * ui13(i)
            q22(i) = f21(i) * ui12(i) + f22(i) * ui22(i) + f23(i) * ui23(i)
            q23(i) = f21(i) * ui13(i) + f22(i) * ui23(i) + f23(i) * ui33(i)
            q31(i) = f31(i) * ui11(i) + f32(i) * ui12(i) + f33(i) * ui13(i)
            q32(i) = f31(i) * ui12(i) + f32(i) * ui22(i) + f33(i) * ui23(i)
            q33(i) = f31(i) * ui13(i) + f32(i) * ui23(i) + f33(i) * ui33(i)
        Next


        's11_vect5 = s11 : s22_vect5 = s22 : s33_vect5 = s33 : s12_vect5 = s12 : s13_vect5 = s13 : s23_vect5 = s23 : r_vect5 = r 'added to avoid duplicate name by YC 102418

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine getvrt(q11,q12,q13,q21,q22,q23,q31,q32,q33)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to compute rotation matrix from polar decomposition
'c     of deformation gradient
'c
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect5/
'     1 f11(64),f22(64),f33(64),f12(64),f13(64),f21(64),
'     2 f23(64),f31(64),f32(64),c11(64),c22(64),c33(64),
'     3 c12(64),c13(64),c23(64),c1212(64),c2323(64),c1313(64),
'     4 li1(64),li2(64),li3(64),ui1(64),ui2(64),ui3(64),
'     5 s11(64),s22(64),s33(64),s12(64),s13(64),s23(64),
'     6 ui11(64),ui22(64),ui33(64),ui12(64),ui13(64),ui23(64),
'     7 si11(64),si22(64),si33(64),si12(64),si13(64),si23(64),
'     8 lamda1(64),lamda2(64),lamda3(64),r(64),q(64),sdetm(64),
'     9 xmod(64),ui1s(64),scl1(64),scl2(64),scl0(64),ct3(64),st3(64),
'     & c2313(64),c1223(64),c1213(64)
'c
'      dimension q11(*),q12(*),q13(*),q21(*),q22(*),q23(*),q31(*),
'     2 q32(1),q33(1)
'
'      double precision li1,li2,li3,lamda1,lamda2,lamda3
'
'c
'      data o3/.333333333333333333333/
'c
'      zero=0.0
'      one=1.0
'      half = 0.5
'      eps = 1.e-30
'      root3=sqrt(3.d0)
'      do 10 i=lft,llt
'      c11(i)=f11(i)*f11(i)+f21(i)*f21(i)+f31(i)*f31(i)
'      c12(i)=f11(i)*f12(i)+f21(i)*f22(i)+f31(i)*f32(i)
'      c13(i)=f11(i)*f13(i)+f21(i)*f23(i)+f31(i)*f33(i)
'      c23(i)=f12(i)*f13(i)+f22(i)*f23(i)+f32(i)*f33(i)
'      c22(i)=f12(i)*f12(i)+f22(i)*f22(i)+f32(i)*f32(i)
'   10 c33(i)=f13(i)*f13(i)+f23(i)*f23(i)+f33(i)*f33(i)
'      do 20 i=lft,llt
'      c1212(i)=c12(i)*c12(i)
'      c1313(i)=c13(i)*c13(i)
'      c2323(i)=c23(i)*c23(i)
'      c2313(i)=c23(i)*c13(i)
'      c1223(i)=c12(i)*c23(i)
'      c1213(i)=c12(i)*c13(i)
'      s11(i)=c22(i)*c33(i)-c2323(i)
'      ui1(i)=o3*(c11(i)+c22(i)+c33(i))
'      ui2(i)=s11(i)+c11(i)*c22(i)+c33(i)*c11(i)-c1212(i)-c1313(i)
'      ui3(i)=c11(i)*s11(i)+c12(i)*(c2313(i)-c12(i)*c33(i))
'     1      +c13(i)*(c1223(i)-c22(i)*c13(i))
'   20 ui1s(i)=ui1(i)*ui1(i)
'      do 30 i=lft,llt
'      q(i)    =sqrt(-min(o3*ui2(i)-ui1s(i),zero))
'      r(i)    =0.5*(ui3(i)-ui1(i)*ui2(i))+ui1(i)*ui1s(i)
'      xmod(i) =q(i)*q(i)*q(i)
'   30 scl1(i) =.5e0+sign(half,xmod(i)-eps)
'      do 40 i=lft,llt
'      scl2(i) =.5e0+sign(half,xmod(i)-abs(r(i)))
'      scl0(i) =min(scl1(i),scl2(i))
'      scl1(i) =1.-scl0(i)
'      sdetm(i)=acos(r(i)/(xmod(i)+scl1(i)))*o3
'      q(i)  =scl0(i)*q(i)
'      ct3(i)=q(i)*cos(sdetm(i))
'   40 st3(i)=q(i)*root3*sin(sdetm(i))
'      do 50 i=lft,llt
'      sdetm(i)=scl1(i)*sqrt(max(zero,r(i)))
'      lamda1(i)=sqrt(2.000*(ct3(i)+sdetm(i))+ui1(i))
'      lamda2(i)=sqrt(-ct3(i)+st3(i)-sdetm(i)+ui1(i))
'   50 lamda3(i)=sqrt(-ct3(i)-st3(i)-sdetm(i)+ui1(i))
'      do 60 i=lft,llt
'      sdetm(i)=lamda1(i)*lamda2(i)
'      li1(i)=lamda1(i)+lamda2(i)+lamda3(i)
'      li2(i)= sdetm(i)+lamda2(i)*lamda3(i)+lamda3(i)*lamda1(i)
'   60 li3(i)= sdetm(i)*lamda3(i)/li1(i)
'      do 70 i=lft,llt
'      s11(i)= c11(i)+li3(i)
'      s22(i)= c22(i)+li3(i)
'      s33(i)= c33(i)+li3(i)
'      s12(i)= c2313(i)-c12(i)*s33(i)
'      s13(i)= c1223(i)-s22(i)*c13(i)
'      s23(i)=-c2323(i)+s22(i)*s33(i)
'   70 sdetm(i)=1./(li1(i)*(s11(i)*s23(i)+c12(i)*s12(i)+c13(i)*s13(i)))
'      do 90 i=lft,llt
'      c11(i)=c11(i)+li2(i)
'      c22(i)=c22(i)+li2(i)
'      c33(i)=c33(i)+li2(i)
'      si11(i)=sdetm(i)*s23(i)
'      si12(i)=sdetm(i)*s12(i)
'      si13(i)=sdetm(i)*s13(i)
'      si22(i)=sdetm(i)*( s11(i)*s33(i)-c1313(i))
'      si23(i)=sdetm(i)*(-s11(i)*c23(i)+c1213(i))
'   90 si33(i)=sdetm(i)*( s11(i)*s22(i)-c1212(i))
'      do 100 i=lft,llt
'      s12(i)=c12(i)*si12(i)
'      s13(i)=c13(i)*si13(i)
'      s23(i)=c23(i)*si23(i)
'      ui11(i)=c11(i)*si11(i)+s12(i)+s13(i)
'      ui22(i)=s12(i)+c22(i)*si22(i)+s23(i)
'      ui33(i)=s13(i)+s23(i)+c33(i)*si33(i)
'      ui12(i)=c11(i)*si12(i)+c12(i)*si22(i)+c13(i)*si23(i)
'      ui13(i)=c11(i)*si13(i)+c12(i)*si23(i)+c13(i)*si33(i)
'  100 ui23(i)=c12(i)*si13(i)+c22(i)*si23(i)+c23(i)*si33(i)
'      do 120 i=lft,llt
'      q11(i)=f11(i)*ui11(i)+f12(i)*ui12(i)+f13(i)*ui13(i)
'      q12(i)=f11(i)*ui12(i)+f12(i)*ui22(i)+f13(i)*ui23(i)
'      q13(i)=f11(i)*ui13(i)+f12(i)*ui23(i)+f13(i)*ui33(i)
'      q21(i)=f21(i)*ui11(i)+f22(i)*ui12(i)+f23(i)*ui13(i)
'      q22(i)=f21(i)*ui12(i)+f22(i)*ui22(i)+f23(i)*ui23(i)
'      q23(i)=f21(i)*ui13(i)+f22(i)*ui23(i)+f23(i)*ui33(i)
'      q31(i)=f31(i)*ui11(i)+f32(i)*ui12(i)+f33(i)*ui13(i)
'      q32(i)=f31(i)*ui12(i)+f32(i)*ui22(i)+f33(i)*ui23(i)
'  120 q33(i)=f31(i)*ui13(i)+f32(i)*ui23(i)+f33(i)*ui33(i)
'      return
'      end

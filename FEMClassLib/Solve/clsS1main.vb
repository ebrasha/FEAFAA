'This file contains all the methods of s1main.f
Partial Public Class clsSolve


    
    Private davg(64), p_vect4(64), dsave(6, 6, 64) As Double

    Dim d11, d21, d31, d41, d51, d61,
        d12, d22, d32, d42, d52, d62,
        d13, d23, d33, d43, d53, d63,
        d14, d24, d34, d44, d54, d64,
        d15, d25, d35, d45, d55, d65,
        d16, d26, d36, d46, d56, d66 As Double

    'Private bk56_c(6, 6) As Double



    ''' <summary>
    ''' stress point integration of hypo-elasticity model
    ''' </summary>
    ''' <param name="prop"></param>
    ''' <param name="sig"></param>
    ''' <param name="ln"></param>
    Public Sub s1main(ByRef prop() As Double, ByRef sig(,) As Double, ByRef ln As Integer,
                      ByVal d1() As Double, ByVal d2() As Double, ByVal d3() As Double,
                      ByVal d4() As Double, ByVal d5() As Double, ByVal d6() As Double)

        Dim p() As Double = p_vect4

        'Dim d(5, 5) As Double   ' YC 102418
        Dim d(6, 6) As Double

        'Dim third = 0.333333333333333  'YC 102418
        Dim third = -0.333333333333333

        'YC 102418
        'Dim qb = prop(4)
        'Dim qh = prop(0) * prop(3) / (prop(0) - prop(3))
        'Dim qs = prop(2)
        'Dim ym = prop(0)
        'Dim pr = prop(1)
        Dim qb = prop(5)
        Dim qh = prop(1) * prop(4) / (prop(1) - prop(4))
        Dim qs = prop(3)
        Dim ym = prop(1)
        Dim pr = prop(2)
        'YC 102418 END

        Dim g = ym / (1.0 + pr)
        Dim qa = 1.0 - qb
        Dim gd2 = 0.5 * g
        Dim blk = -ym / (1.0 - 2.0 * pr)
        Dim qbqh = qb * qh

        Call rotat1(sig, ln)

        'c     update stress

        Dim i, j As Integer  ' YC 102418

        For i = mft To mlt
            If sig(0, i) <> 0 Then
                Continue For
            End If
            davg(i) = third * (d1(i) + d2(i) + d3(i))
            p(i) = blk * davg(i)
        Next
        'Call Check1D3(sig, p, d1, 1, istep)
        For i = mft To mlt

            ' YC 102418 
            'sig(0, i) = sig(0, i) + p(i) + g * (d1(i) + davg(i))
            'sig(1, i) = sig(1, i) + p(i) + g * (d2(i) + davg(i))
            'sig(2, i) = sig(2, i) + p(i) + g * (d3(i) + davg(i))
            'sig(3, i) = sig(3, i) + gd2 * d4(i)
            'sig(4, i) = sig(4, i) + gd2 * d5(i)
            'sig(5, i) = sig(5, i) + gd2 * d6(i)
            sig(1, i) = sig(1, i) + p(i) + g * (d1(i) + davg(i))
            sig(2, i) = sig(2, i) + p(i) + g * (d2(i) + davg(i))
            sig(3, i) = sig(3, i) + p(i) + g * (d3(i) + davg(i))
            sig(4, i) = sig(4, i) + gd2 * d4(i)
            sig(5, i) = sig(5, i) + gd2 * d5(i)
            sig(6, i) = sig(6, i) + gd2 * d6(i)
            ' YC 102418 END

        Next

        If iphase = 3 Then GoTo 200

        'c     compute tangent constitutive matrix


        Dim q1 As Double = prop(1) * prop(2) / ((1.0 + prop(2)) * (1.0 - 2.0 * prop(2)))
        Dim q2 As Double = prop(1) * 0.5 / (1.0 + prop(2))
        Dim q3 As Double = q1 + 2.0 * q2

        ' YC 102418
        'For i = 0 To 5
        '    For j = 0 To 5
        '        d(i, j) = 0.0
        '    Next
        'Next
        '    d(0, 0) = q3
        '    d(1, 1) = q3
        '    d(2, 2) = q3
        '    d(3, 3) = q2
        '    d(4, 4) = q2
        '    d(5, 5) = q2
        '    d(0, 1) = q1
        '    d(0, 2) = q1
        '    d(1, 0) = q1
        '    d(1, 2) = q1
        '    d(2, 0) = q1
        '    d(2, 1) = q1
        For i = 1 To 6
            For j = 1 To 6
                d(i, j) = 0.0
            Next
        Next
        d(1, 1) = q3
        d(2, 2) = q3
        d(3, 3) = q3
        d(4, 4) = q2
        d(5, 5) = q2
        d(6, 6) = q2
        d(1, 2) = q1
        d(1, 3) = q1
        d(2, 1) = q1
        d(2, 3) = q1
        d(3, 1) = q1
        d(3, 2) = q1
        ' YC 102418

        ' QW 12-12-2018    c11 changed to d11
        d11 = d(1, 1) : d12 = d(1, 2) : d13 = d(1, 3) : d14 = d(1, 4) : d15 = d(1, 5) : d16 = d(1, 6)
        d21 = d(2, 1) : d22 = d(2, 2) : d23 = d(2, 3) : d24 = d(2, 4) : d25 = d(2, 5) : d26 = d(2, 6)
        d31 = d(3, 1) : d32 = d(3, 2) : d33 = d(3, 3) : d34 = d(3, 4) : d35 = d(3, 5) : d36 = d(3, 6)
        d41 = d(4, 1) : d42 = d(4, 2) : d43 = d(4, 3) : d44 = d(4, 4) : d45 = d(4, 5) : d46 = d(4, 6)
        d51 = d(5, 1) : d52 = d(5, 2) : d53 = d(5, 3) : d54 = d(5, 4) : d55 = d(5, 5) : d56 = d(5, 6)
        d61 = d(6, 1) : d62 = d(6, 2) : d63 = d(6, 3) : d64 = d(6, 4) : d65 = d(6, 5) : d66 = d(6, 6)
        '
        For i = mft To mlt
            dsave(1, 1, i) = d11
            dsave(1, 2, i) = d12
            dsave(1, 3, i) = d13
            dsave(1, 4, i) = d14
            dsave(1, 5, i) = d15
            dsave(1, 6, i) = d16
            dsave(2, 1, i) = d21
            dsave(2, 2, i) = d22
            dsave(2, 3, i) = d23
            dsave(2, 4, i) = d24
            dsave(2, 5, i) = d25
            dsave(2, 6, i) = d26
            dsave(3, 1, i) = d31
            dsave(3, 2, i) = d32
            dsave(3, 3, i) = d33
            dsave(3, 4, i) = d34
            dsave(3, 5, i) = d35
            dsave(3, 6, i) = d36
            dsave(4, 1, i) = d41
            dsave(4, 2, i) = d42
            dsave(4, 3, i) = d43
            dsave(4, 4, i) = d44
            dsave(4, 5, i) = d45
            dsave(4, 6, i) = d46
            dsave(5, 1, i) = d51
            dsave(5, 2, i) = d52
            dsave(5, 3, i) = d53
            dsave(5, 4, i) = d54
            dsave(5, 5, i) = d55
            dsave(5, 6, i) = d56
            dsave(6, 1, i) = d61
            dsave(6, 2, i) = d62
            dsave(6, 3, i) = d63
            dsave(6, 4, i) = d64
            dsave(6, 5, i) = d65
            dsave(6, 6, i) = d66
        Next
        '

200:    Call rotat2(sig, ln)

        For i = mft To mlt

            ' YC 102418
            'sig11s(i) = sig(0, i)
            'sig22s(i) = sig(1, i)
            'sig33s(i) = sig(2, i)
            'sig12s(i) = sig(3, i)
            'sig23s(i) = sig(4, i)
            'sig31s(i) = sig(5, i)
            sig11s(i) = sig(1, i)
            sig22s(i) = sig(2, i)
            sig33s(i) = sig(3, i)
            sig12s(i) = sig(4, i)
            sig23s(i) = sig(5, i)
            sig31s(i) = sig(6, i)
            ' YC 102418 END

        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine s1main (prop,sig,ln)
'c
'
'      implicit double precision (a-h,o-z)                             
'c
'c===> module for stress point integration of hypo-elasticity model
'c
'      common/bk10/iphase,nelgp,imass,lhex(9)   
'      common/bk56/
'     1 c11,c21,c31,c41,c51,c61,c12,c22,c32,c42,c52,c62,
'     2 c13,c23,c33,c43,c53,c63,c14,c24,c34,c44,c54,c64,
'     3 c15,c25,c35,c45,c55,c65,c16,c26,c36,c46,c56,c66
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect3/
'     1 sig11s(64),sig22s(64),sig33s(64),sig12s(64),sig23s(64),
'     2 sig31s(64)
'      common/vect8/
'     1 dsave(6,6,64)
'      common/vect14/
'     1 p(64),davg(64),dum14(66,64)
'      common/vect15/
'     1 d1(64),d2(64),d3(64),d4(64),d5(64),d6(64),dum15(384)
'      logical lelstf
'      common/elstfb/lelstf
'      common/nkc_var/dinfin(64)
'c
'      dimension prop(*),sig(ln,*),d(6,6)
'      equivalence (c11,d)
'      data third/-.333333333333333/
'c    
'      qb  =prop(5)
'      qh  =prop(1)*prop(4)/(prop(1)-prop(4))
'      qs  =prop(3)
'      ym  =prop(1)
'      pr  =prop(2)
'      g   =ym/(1.+pr)
'      qa  =1.-qb
'      gd2 =.5*g
'      blk =-ym/(1.-2.*pr)
'      qbqh=qb*qh
'c
'      call rotat1 (sig,ln)
'c
'c     update stress
'c
'      do 10 i=mft,mlt
'          if (sig(1,i).ne.0) then
'          continue
'          end if
'      davg(i)=third*(d1(i)+d2(i)+d3(i))
'   10 p(i)=blk*davg(i)
'      do 20 i=mft,mlt
'      sig(1,i)=sig(1,i)+p(i)+g*(d1(i)+davg(i))
'      sig(2,i)=sig(2,i)+p(i)+g*(d2(i)+davg(i))
'      sig(3,i)=sig(3,i)+p(i)+g*(d3(i)+davg(i))
'      sig(4,i)=sig(4,i)+gd2*d4(i)
'      sig(5,i)=sig(5,i)+gd2*d5(i)
'      sig(6,i)=sig(6,i)+gd2*d6(i)
'   20 continue
'c
'      if (iphase.eq.3) go to 200
'c
'c     compute tangent constitutive matrix
'c
'      q1=prop(1)*prop(2)/((1.0+prop(2))*(1.0-2.0*prop(2)))
'      q2=prop(1)*0.5/(1.0+prop(2))
'      q3=q1+2.0*q2
'      do 150 i=1,6
'      do 150 j=1,6
'  150 d(i,j)=0.0
'      d(1,1)=q3
'      d(2,2)=q3
'      d(3,3)=q3
'      d(4,4)=q2
'      d(5,5)=q2
'      d(6,6)=q2
'      d(1,2)=q1
'      d(1,3)=q1
'      d(2,1)=q1
'      d(2,3)=q1
'      d(3,1)=q1
'      d(3,2)=q1
'c
'      do 160 i=mft,mlt
'      dsave(1,1,i)=c11
'      dsave(1,2,i)=c12
'      dsave(1,3,i)=c13
'      dsave(1,4,i)=c14
'      dsave(1,5,i)=c15
'      dsave(1,6,i)=c16
'      dsave(2,1,i)=c21
'      dsave(2,2,i)=c22
'      dsave(2,3,i)=c23
'      dsave(2,4,i)=c24
'      dsave(2,5,i)=c25
'      dsave(2,6,i)=c26
'      dsave(3,1,i)=c31
'      dsave(3,2,i)=c32
'      dsave(3,3,i)=c33
'      dsave(3,4,i)=c34
'      dsave(3,5,i)=c35
'      dsave(3,6,i)=c36
'      dsave(4,1,i)=c41
'      dsave(4,2,i)=c42
'      dsave(4,3,i)=c43
'      dsave(4,4,i)=c44
'      dsave(4,5,i)=c45
'      dsave(4,6,i)=c46
'      dsave(5,1,i)=c51
'      dsave(5,2,i)=c52
'      dsave(5,3,i)=c53
'      dsave(5,4,i)=c54
'      dsave(5,5,i)=c55
'      dsave(5,6,i)=c56
'      dsave(6,1,i)=c61
'      dsave(6,2,i)=c62
'      dsave(6,3,i)=c63
'      dsave(6,4,i)=c64
'      dsave(6,5,i)=c65
'      dsave(6,6,i)=c66
'  160 continue
'c
'  200 call rotat2 (sig,ln)
'c
'      do 210 i=mft,mlt
'      sig11s(i)=sig(1,i)
'      sig22s(i)=sig(2,i)
'      sig33s(i)=sig(3,i)
'      sig12s(i)=sig(4,i)
'      sig23s(i)=sig(5,i)
'  210 sig31s(i)=sig(6,i)
'c
'      return
'      end

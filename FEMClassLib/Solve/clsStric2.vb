'This file contains all the methods of stric2.f
Partial Public Class clsSolve

    
    'Public gass(,) As Double

    Dim ai11(64), ai12(64), ai13(64),
        ai21(64), ai22(64), ai23(64),
        ai31(64), ai32(64), ai33(64),
      a1111(64), a1113(64), a1115(64), a1117(64),
      a2111(64), a2113(64), a2115(64), a2117(64),
      a3111(64), a3113(64), a3115(64), a3117(64),
      a1221(64), a1222(64), a1225(64), a1226(64),
      a2221(64), a2222(64), a2225(64), a2226(64),
      a3221(64), a3222(64), a3225(64), a3226(64),
      a1331(64), a1332(64), a1333(64), a1334(64),
      a2331(64), a2332(64), a2333(64), a2334(64),
      a3331(64), a3332(64), a3333(64), a3334(64),
      vlinv(64) As Double   ' vect5 by YC? 



    ''' <summary>
    ''' compute gradient matrix for incompatible hex element
    ''' </summary>
    ''' <param name="vlinc"></param>
    Public Sub stric2(ByRef vlinc() As Double, ByVal ipt As Integer, ByVal lst As Integer)        ' QW 12-12-2018

        'Dim p1(7), p2(7), p3(7) As Double     ' YC 102418
        Dim p1(8), p2(8), p3(8) As Double

        Dim i As Integer ' YC 102418

        '.... get coefficients for chain rule at integration point

        For i = lft To llt

            p1(1) = p11 : p2(1) = p21 : p3(1) = p31        ' QW 12-12-2018
            p1(2) = p12 : p2(2) = p22 : p3(2) = p32
            p1(3) = p13 : p2(3) = p23 : p3(3) = p33
            p1(4) = p14 : p2(4) = p24 : p3(4) = p34
            p1(5) = p15 : p2(5) = p25 : p3(5) = p35
            p1(6) = p16 : p2(6) = p26 : p3(6) = p36
            p1(7) = p17 : p2(7) = p27 : p3(7) = p37
            p1(8) = p18 : p2(8) = p28 : p3(8) = p38
            Call basisf(p1, p2, p3, ipt, mtype(i), 1, dro(i), i)
            p11 = p1(1) : p21 = p2(1) : p31 = p3(1)
            p12 = p1(2) : p22 = p2(2) : p32 = p3(2)
            p13 = p1(3) : p23 = p2(3) : p33 = p3(3)
            p14 = p1(4) : p24 = p2(4) : p34 = p3(4)
            p15 = p1(5) : p25 = p2(5) : p35 = p3(5)
            p16 = p1(6) : p26 = p2(6) : p36 = p3(6)
            p17 = p1(7) : p27 = p2(7) : p37 = p3(7)
            p18 = p1(8) : p28 = p2(8) : p38 = p3(8)

            aj11(i) = p11 * x12(i) + p13 * x34(i) + p15 * x56(i) + p17 * x78(i)
            aj12(i) = p11 * y12(i) + p13 * y34(i) + p15 * y56(i) + p17 * y78(i)
            aj13(i) = p11 * z12(i) + p13 * z34(i) + p15 * z56(i) + p17 * z78(i)
            aj21(i) = p21 * x14(i) + p22 * x23(i) + p25 * x58(i) + p26 * x67(i)
            aj22(i) = p21 * y14(i) + p22 * y23(i) + p25 * y58(i) + p26 * y67(i)
            aj23(i) = p21 * z14(i) + p22 * z23(i) + p25 * z58(i) + p26 * z67(i)
            aj31(i) = p31 * x15(i) + p32 * x26(i) + p33 * x37(i) + p34 * x48(i)
            aj32(i) = p31 * y15(i) + p32 * y26(i) + p33 * y37(i) + p34 * y48(i)
            aj33(i) = p31 * z15(i) + p32 * z26(i) + p33 * z37(i) + p34 * z48(i)
        Next

        '.... add incompatible shape functions
        For i = lft To llt
            aj11(i) = aj11(i) - 2.0 * gass(1, ipt) * xic1(i)
            aj12(i) = aj12(i) - 2.0 * gass(1, ipt) * yic1(i)
            aj13(i) = aj13(i) - 2.0 * gass(1, ipt) * zic1(i)
            aj21(i) = aj21(i) - 2.0 * gass(2, ipt) * xic2(i)
            aj22(i) = aj22(i) - 2.0 * gass(2, ipt) * yic2(i)
            aj23(i) = aj23(i) - 2.0 * gass(2, ipt) * zic2(i)
            aj31(i) = aj31(i) - 2.0 * gass(3, ipt) * xic3(i)
            aj32(i) = aj32(i) - 2.0 * gass(3, ipt) * yic3(i)
            aj33(i) = aj33(i) - 2.0 * gass(3, ipt) * zic3(i)
        Next

        '.... compute jacobian and invert
        For i = lft To llt
            ai11(i) = aj22(i) * aj33(i) - aj23(i) * aj32(i)
            ai21(i) = -aj21(i) * aj33(i) + aj23(i) * aj31(i)
            ai31(i) = aj21(i) * aj32(i) - aj22(i) * aj31(i)
            vlinc(i) = aj11(i) * ai11(i) + aj12(i) * ai21(i) + aj13(i) * ai31(i)
            vlinv(i) = 1.0 / vlinc(i)
        Next

        For i = lft To llt
            ai11(i) = vlinv(i) * ai11(i)
            ai21(i) = vlinv(i) * ai21(i)
            ai31(i) = vlinv(i) * ai31(i)
            ai12(i) = vlinv(i) * (-aj12(i) * aj33(i) + aj13(i) * aj32(i))
            ai22(i) = vlinv(i) * (aj11(i) * aj33(i) - aj13(i) * aj31(i))
            ai32(i) = vlinv(i) * (-aj11(i) * aj32(i) + aj12(i) * aj31(i))
            ai13(i) = vlinv(i) * (aj12(i) * aj23(i) - aj13(i) * aj22(i))
            ai23(i) = vlinv(i) * (-aj11(i) * aj23(i) + aj13(i) * aj21(i))
            ai33(i) = vlinv(i) * (aj11(i) * aj22(i) - aj12(i) * aj21(i))
        Next

        For i = lft To llt

            p1(1) = p11 : p2(1) = p21 : p3(1) = p31        ' QW 12-12-2018
            p1(2) = p12 : p2(2) = p22 : p3(2) = p32
            p1(3) = p13 : p2(3) = p23 : p3(3) = p33
            p1(4) = p14 : p2(4) = p24 : p3(4) = p34
            p1(5) = p15 : p2(5) = p25 : p3(5) = p35
            p1(6) = p16 : p2(6) = p26 : p3(6) = p36
            p1(7) = p17 : p2(7) = p27 : p3(7) = p37
            p1(8) = p18 : p2(8) = p28 : p3(8) = p38
            Call basisf(p1, p2, p3, ipt, mtype(i), 2, dro(i), i)
            p11 = p1(1) : p21 = p2(1) : p31 = p3(1)
            p12 = p1(2) : p22 = p2(2) : p32 = p3(2)
            p13 = p1(3) : p23 = p2(3) : p33 = p3(3)
            p14 = p1(4) : p24 = p2(4) : p34 = p3(4)
            p15 = p1(5) : p25 = p2(5) : p35 = p3(5)
            p16 = p1(6) : p26 = p2(6) : p36 = p3(6)
            p17 = p1(7) : p27 = p2(7) : p37 = p3(7)
            p18 = p1(8) : p28 = p2(8) : p38 = p3(8)

            a1111(i) = ai11(i) * p11
            a1113(i) = ai11(i) * p13
            a1115(i) = ai11(i) * p15
            a1117(i) = ai11(i) * p17
            a2111(i) = ai21(i) * p11
            a2113(i) = ai21(i) * p13
            a2115(i) = ai21(i) * p15
            a2117(i) = ai21(i) * p17
            a3111(i) = ai31(i) * p11
            a3113(i) = ai31(i) * p13
            a3115(i) = ai31(i) * p15
            a3117(i) = ai31(i) * p17
            a1221(i) = ai12(i) * p21
            a1222(i) = ai12(i) * p22
            a1225(i) = ai12(i) * p25
            a1226(i) = ai12(i) * p26
            a2221(i) = ai22(i) * p21
            a2222(i) = ai22(i) * p22

            a2225(i) = ai22(i) * p25
            a2226(i) = ai22(i) * p26
            a3221(i) = ai32(i) * p21
            a3222(i) = ai32(i) * p22
            a3225(i) = ai32(i) * p25
            a3226(i) = ai32(i) * p26
            a1331(i) = ai13(i) * p31
            a1332(i) = ai13(i) * p32
            a1333(i) = ai13(i) * p33
            a1334(i) = ai13(i) * p34
            a2331(i) = ai23(i) * p31
            a2332(i) = ai23(i) * p32
            a2333(i) = ai23(i) * p33
            a2334(i) = ai23(i) * p34
            a3331(i) = ai33(i) * p31
            a3332(i) = ai33(i) * p32
            a3333(i) = ai33(i) * p33
            a3334(i) = ai33(i) * p34

            px1(i) = ai11(i) * p11 + ai12(i) * p21 + ai13(i) * p31
            px2(i) = ai11(i) * p12 + ai12(i) * p22 + ai13(i) * p32
            px3(i) = ai11(i) * p13 + ai12(i) * p23 + ai13(i) * p33
            px4(i) = ai11(i) * p14 + ai12(i) * p24 + ai13(i) * p34
            px5(i) = ai11(i) * p15 + ai12(i) * p25 + ai13(i) * p35
            px6(i) = ai11(i) * p16 + ai12(i) * p26 + ai13(i) * p36
            px7(i) = ai11(i) * p17 + ai12(i) * p27 + ai13(i) * p37
            px8(i) = ai11(i) * p18 + ai12(i) * p28 + ai13(i) * p38
            py1(i) = ai21(i) * p11 + ai22(i) * p21 + ai23(i) * p31
            py2(i) = ai21(i) * p12 + ai22(i) * p22 + ai23(i) * p32
            py3(i) = ai21(i) * p13 + ai22(i) * p23 + ai23(i) * p33
            py4(i) = ai21(i) * p14 + ai22(i) * p24 + ai23(i) * p34
            py5(i) = ai21(i) * p15 + ai22(i) * p25 + ai23(i) * p35
            py6(i) = ai21(i) * p16 + ai22(i) * p26 + ai23(i) * p36
            py7(i) = ai21(i) * p17 + ai22(i) * p27 + ai23(i) * p37
            py8(i) = ai21(i) * p18 + ai22(i) * p28 + ai23(i) * p38
            pz1(i) = ai31(i) * p11 + ai32(i) * p21 + ai33(i) * p31
            pz2(i) = ai31(i) * p12 + ai32(i) * p22 + ai33(i) * p32
            pz3(i) = ai31(i) * p13 + ai32(i) * p23 + ai33(i) * p33
            pz4(i) = ai31(i) * p14 + ai32(i) * p24 + ai33(i) * p34
            pz5(i) = ai31(i) * p15 + ai32(i) * p25 + ai33(i) * p35
            pz6(i) = ai31(i) * p16 + ai32(i) * p26 + ai33(i) * p36
            pz7(i) = ai31(i) * p17 + ai32(i) * p27 + ai33(i) * p37
            pz8(i) = ai31(i) * p18 + ai32(i) * p28 + ai33(i) * p38

            'If ng = 1 And (lst = 1 Or lst = 2) And i = 1 Then
            'Call Check3(aj22, aj33, z15, i)

            ' If aj22(i) <> 2 Then
            '      Stop
            'End If
            'End If
        Next


        '.... define gradient for incompatable dof


        For i = lft To llt

            Dim c1 = -2.0 * vlinv(i)
            pxic1(i) = aic11(i) * c1 * gass(1, ipt)
            pxic2(i) = aic12(i) * c1 * gass(2, ipt)
            pxic3(i) = aic13(i) * c1 * gass(3, ipt)
            pyic1(i) = aic21(i) * c1 * gass(1, ipt)
            pyic2(i) = aic22(i) * c1 * gass(2, ipt)
            pyic3(i) = aic23(i) * c1 * gass(3, ipt)
            pzic1(i) = aic31(i) * c1 * gass(1, ipt)
            pzic2(i) = aic32(i) * c1 * gass(2, ipt)
            pzic3(i) = aic33(i) * c1 * gass(3, ipt)
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine stric2 (vlinc)
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c
'c===> module to compute gradient matrix for incompatible hex element
'c
'      common/bk53/gass(3,9)
'      common/bk56/c(6,6),ipt,nel,nelsub
'      common/bk61/p11,p12,p13,p14,p15,p16,p17,p18,
'     1            p21,p22,p23,p24,p25,p26,p27,p28,
'     2            p31,p32,p33,p34,p35,p36,p37,p38
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect1/
'     1 rx1(64),ry1(64),rz1(64),rx2(64),ry2(64),rz2(64),
'     2 rx3(64),ry3(64),rz3(64),rx4(64),ry4(64),rz4(64),
'     3 rx5(64),ry5(64),rz5(64),rx6(64),ry6(64),rz6(64),
'     4 rx7(64),ry7(64),rz7(64),rx8(64),ry8(64),rz8(64),
'     5 mtype(64),matp(64)
'      common/vect4/
'     1 px1(64),px2(64),px3(64),px4(64),
'     2 px5(64),px6(64),px7(64),px8(64),
'     3 py1(64),py2(64),py3(64),py4(64),
'     4 py5(64),py6(64),py7(64),py8(64),
'     5 pz1(64),pz2(64),pz3(64),pz4(64),
'     6 pz5(64),pz6(64),pz7(64),pz8(64)
'      common/vect5/
'     1 aj11(64),aj12(64),aj13(64),aj21(64),aj22(64),aj23(64),
'     2 aj31(64),aj32(64),aj33(64),ai11(64),ai12(64),ai13(64),
'     3 ai21(64),ai22(64),ai23(64),ai31(64),ai32(64),ai33(64),
'     4 a1111(64),a1113(64),a1115(64),a1117(64),a2111(64),
'     5 a2113(64),a2115(64),a2117(64),a3111(64),a3113(64),
'     6 a3115(64),a3117(64),a1221(64),a1222(64),a1225(64),
'     7 a1226(64),a2221(64),a2222(64),a2225(64),a2226(64),
'     8 a3221(64),a3222(64),a3225(64),a3226(64),a1331(64),
'     9 a1332(64),a1333(64),a1334(64),a2331(64),a2332(64),
'     & a2333(64),a2334(64),a3331(64),a3332(64),a3333(64),
'     & a3334(64),vlinv(64),dum5(192)
'      common/vect9/scale(24,64),ym(8,64),ro(64),rov(64),tmecc(64) !db 8-21-02
'      common/vect13/
'     1 x12(64),x34(64),x56(64),x78(64),y12(64),y34(64),
'     2 y56(64),y78(64),z12(64),z34(64),z56(64),z78(64),
'     3 x14(64),x23(64),x58(64),x67(64),y14(64),y23(64),
'     4 y58(64),y67(64),z14(64),z23(64),z58(64),z67(64),
'     6 x15(64),x26(64),x37(64),x48(64),y15(64),y26(64),
'     7 y37(64),y48(64),z15(64),z26(64),z37(64),z48(64),dum13(1536)
'      common/vect51/
'     1 aic11(64),aic12(64),aic13(64),aic21(64),aic22(64),aic23(64),
'     2 aic31(64),aic32(64),aic33(64)
'      common/vect52/
'     1 xic1(64),xic2(64),xic3(64),yic1(64),yic2(64),yic3(64),
'     2 zic1(64),zic2(64),zic3(64),pxic1(64),pxic2(64),pxic3(64),
'     3 pyic1(64),pyic2(64),pyic3(64),pzic1(64),pzic2(64),pzic3(64)
'c
'c	------------- Added by Qiang, 03-09-2010 --------------------------
'	common/inf/dro(64)					
'c	-------------------------------------------------------------------
'c
'      dimension vlinc(*)
'c
'c.... get coefficients for chain rule at integration point
'c
'      do 10 i=lft,llt
'c NKC
'c
'
'      call basisf (p11,p21,p31,ipt,mtype(i),1,dro(i),i) 
'c
'      aj11(i)=p11*x12(i)+p13*x34(i)+p15*x56(i)+p17*x78(i)
'      aj12(i)=p11*y12(i)+p13*y34(i)+p15*y56(i)+p17*y78(i)
'      aj13(i)=p11*z12(i)+p13*z34(i)+p15*z56(i)+p17*z78(i)
'      aj21(i)=p21*x14(i)+p22*x23(i)+p25*x58(i)+p26*x67(i)
'      aj22(i)=p21*y14(i)+p22*y23(i)+p25*y58(i)+p26*y67(i)
'      aj23(i)=p21*z14(i)+p22*z23(i)+p25*z58(i)+p26*z67(i)
'      aj31(i)=p31*x15(i)+p32*x26(i)+p33*x37(i)+p34*x48(i)
'      aj32(i)=p31*y15(i)+p32*y26(i)+p33*y37(i)+p34*y48(i)
'      aj33(i)=p31*z15(i)+p32*z26(i)+p33*z37(i)+p34*z48(i)
'   10 continue
'c
'c.... add incompatible shape functions
'      do 15 i=lft,llt
'      aj11(i)=aj11(i)-2.*gass(1,ipt)*xic1(i)
'      aj12(i)=aj12(i)-2.*gass(1,ipt)*yic1(i)
'      aj13(i)=aj13(i)-2.*gass(1,ipt)*zic1(i)
'      aj21(i)=aj21(i)-2.*gass(2,ipt)*xic2(i)
'      aj22(i)=aj22(i)-2.*gass(2,ipt)*yic2(i)
'      aj23(i)=aj23(i)-2.*gass(2,ipt)*zic2(i)
'      aj31(i)=aj31(i)-2.*gass(3,ipt)*xic3(i)
'      aj32(i)=aj32(i)-2.*gass(3,ipt)*yic3(i)
'      aj33(i)=aj33(i)-2.*gass(3,ipt)*zic3(i)
'   15 continue
'c
'c.... compute jacobian and invert
'      do 20 i=lft,llt
'      ai11(i)= aj22(i)*aj33(i)-aj23(i)*aj32(i)
'      ai21(i)=-aj21(i)*aj33(i)+aj23(i)*aj31(i)
'      ai31(i)= aj21(i)*aj32(i)-aj22(i)*aj31(i)
'      vlinc(i)=aj11(i)*ai11(i)+aj12(i)*ai21(i)+aj13(i)*ai31(i)
'      vlinv(i)=1./vlinc(i)
'   20 continue
'c
'      do 30 i=lft,llt
'      ai11(i)=vlinv(i)*ai11(i)
'      ai21(i)=vlinv(i)*ai21(i)
'      ai31(i)=vlinv(i)*ai31(i)
'      ai12(i)=vlinv(i)*(-aj12(i)*aj33(i)+aj13(i)*aj32(i))
'      ai22(i)=vlinv(i)*( aj11(i)*aj33(i)-aj13(i)*aj31(i))
'      ai32(i)=vlinv(i)*(-aj11(i)*aj32(i)+aj12(i)*aj31(i))
'      ai13(i)=vlinv(i)*( aj12(i)*aj23(i)-aj13(i)*aj22(i))
'      ai23(i)=vlinv(i)*(-aj11(i)*aj23(i)+aj13(i)*aj21(i))
'      ai33(i)=vlinv(i)*( aj11(i)*aj22(i)-aj12(i)*aj21(i))
'   30 continue
'c
'      do 40 i=lft,llt
'c NKC
'c
'      call basisf (p11,p21,p31,ipt,mtype(i),2,dro(i),i)
'c
'      a1111(i)=ai11(i)*p11
'      a1113(i)=ai11(i)*p13
'      a1115(i)=ai11(i)*p15
'      a1117(i)=ai11(i)*p17
'      a2111(i)=ai21(i)*p11
'      a2113(i)=ai21(i)*p13
'      a2115(i)=ai21(i)*p15
'      a2117(i)=ai21(i)*p17
'      a3111(i)=ai31(i)*p11
'      a3113(i)=ai31(i)*p13
'      a3115(i)=ai31(i)*p15
'      a3117(i)=ai31(i)*p17
'      a1221(i)=ai12(i)*p21
'      a1222(i)=ai12(i)*p22
'      a1225(i)=ai12(i)*p25
'      a1226(i)=ai12(i)*p26
'      a2221(i)=ai22(i)*p21
'      a2222(i)=ai22(i)*p22
'c    
'      a2225(i)=ai22(i)*p25
'      a2226(i)=ai22(i)*p26
'      a3221(i)=ai32(i)*p21
'      a3222(i)=ai32(i)*p22
'      a3225(i)=ai32(i)*p25
'      a3226(i)=ai32(i)*p26
'      a1331(i)=ai13(i)*p31
'      a1332(i)=ai13(i)*p32
'      a1333(i)=ai13(i)*p33
'      a1334(i)=ai13(i)*p34
'      a2331(i)=ai23(i)*p31
'      a2332(i)=ai23(i)*p32
'      a2333(i)=ai23(i)*p33
'      a2334(i)=ai23(i)*p34
'      a3331(i)=ai33(i)*p31
'      a3332(i)=ai33(i)*p32
'      a3333(i)=ai33(i)*p33
'      a3334(i)=ai33(i)*p34
'c
'	px1(i)= ai11(i)*p11+ai12(i)*p21+ai13(i)*p31
'      px2(i)= ai11(i)*p12+ai12(i)*p22+ai13(i)*p32
'      px3(i)= ai11(i)*p13+ai12(i)*p23+ai13(i)*p33
'      px4(i)= ai11(i)*p14+ai12(i)*p24+ai13(i)*p34
'      px5(i)= ai11(i)*p15+ai12(i)*p25+ai13(i)*p35
'      px6(i)= ai11(i)*p16+ai12(i)*p26+ai13(i)*p36
'      px7(i)= ai11(i)*p17+ai12(i)*p27+ai13(i)*p37
'      px8(i)= ai11(i)*p18+ai12(i)*p28+ai13(i)*p38
'      py1(i)= ai21(i)*p11+ai22(i)*p21+ai23(i)*p31
'      py2(i)= ai21(i)*p12+ai22(i)*p22+ai23(i)*p32
'      py3(i)= ai21(i)*p13+ai22(i)*p23+ai23(i)*p33
'      py4(i)= ai21(i)*p14+ai22(i)*p24+ai23(i)*p34
'      py5(i)= ai21(i)*p15+ai22(i)*p25+ai23(i)*p35
'      py6(i)= ai21(i)*p16+ai22(i)*p26+ai23(i)*p36
'      py7(i)= ai21(i)*p17+ai22(i)*p27+ai23(i)*p37
'      py8(i)= ai21(i)*p18+ai22(i)*p28+ai23(i)*p38
'      pz1(i)= ai31(i)*p11+ai32(i)*p21+ai33(i)*p31
'      pz2(i)= ai31(i)*p12+ai32(i)*p22+ai33(i)*p32
'      pz3(i)= ai31(i)*p13+ai32(i)*p23+ai33(i)*p33
'      pz4(i)= ai31(i)*p14+ai32(i)*p24+ai33(i)*p34
'      pz5(i)= ai31(i)*p15+ai32(i)*p25+ai33(i)*p35
'      pz6(i)= ai31(i)*p16+ai32(i)*p26+ai33(i)*p36
'      pz7(i)= ai31(i)*p17+ai32(i)*p27+ai33(i)*p37
'      pz8(i)= ai31(i)*p18+ai32(i)*p28+ai33(i)*p38
'   40	continue
'c
'c.... define gradient for incompatable dof
'      do 70 i=lft,llt
'      c1=-2.*vlinv(i)
'      pxic1(i)=aic11(i)*c1*gass(1,ipt)
'      pxic2(i)=aic12(i)*c1*gass(2,ipt)
'      pxic3(i)=aic13(i)*c1*gass(3,ipt)
'      pyic1(i)=aic21(i)*c1*gass(1,ipt)
'      pyic2(i)=aic22(i)*c1*gass(2,ipt)
'      pyic3(i)=aic23(i)*c1*gass(3,ipt)
'      pzic1(i)=aic31(i)*c1*gass(1,ipt)
'      pzic2(i)=aic32(i)*c1*gass(2,ipt)
'      pzic3(i)=aic33(i)*c1*gass(3,ipt)
'   70 continue
'c
'      return
'      end

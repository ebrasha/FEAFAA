'This file contains all the methods of stric1.f
Partial Public Class clsSolve


    

    Private aic11(64), aic12(64), aic13(64), aic21(64), aic22(64), aic23(64),
     aic31(64), aic32(64), aic33(64) As Double

    Private p11, p12, p13, p14, p15, p16, p17, p18,
  p21, p22, p23, p24, p25, p26, p27, p28,
  p31, p32, p33, p34, p35, p36, p37, p38 As Double


    Dim aj11(64), aj12(64), aj13(64),
        aj21(64), aj22(64), aj23(64),
        aj31(64), aj32(64), aj33(64) As Double  ' vect5 by YC?




    ''' <summary>
    ''' compute coefficients for chain rule at center of element
    ''' </summary>
    Public Sub stric1()

        'Dim p1(7), p2(7), p3(7) As Double ' YC 102418
        Dim p1(8), p2(8), p3(8) As Double

        Dim i As Integer ' YC 102418

        For i = lft To llt

            If (incomp(matp(i)) = 0) Then  
                aic11(i) = 0.0
                aic21(i) = 0.0
                aic31(i) = 0.0
                aic12(i) = 0.0
                aic22(i) = 0.0
                aic32(i) = 0.0
                aic13(i) = 0.0
                aic23(i) = 0.0
                aic33(i) = 0.0
            Else
                p1(1) = p11 : p2(1) = p21 : p3(1) = p31        ' QW 12-12-2018
                p1(2) = p12 : p2(2) = p22 : p3(2) = p32
                p1(3) = p13 : p2(3) = p23 : p3(3) = p33
                p1(4) = p14 : p2(4) = p24 : p3(4) = p34
                p1(5) = p15 : p2(5) = p25 : p3(5) = p35
                p1(6) = p16 : p2(6) = p26 : p3(6) = p36
                p1(7) = p17 : p2(7) = p27 : p3(7) = p37
                p1(8) = p18 : p2(8) = p28 : p3(8) = p38
                Call basisf(p1, p2, p3, 9, 1, 1, 1.0, i)
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

                aic11(i) = aj22(i) * aj33(i) - aj23(i) * aj32(i)
                aic21(i) = -aj21(i) * aj33(i) + aj23(i) * aj31(i)
                aic31(i) = aj21(i) * aj32(i) - aj22(i) * aj31(i)
                aic12(i) = -aj12(i) * aj33(i) + aj13(i) * aj32(i)
                aic22(i) = aj11(i) * aj33(i) - aj13(i) * aj31(i)
                aic32(i) = -aj11(i) * aj32(i) + aj12(i) * aj31(i)
                aic13(i) = aj12(i) * aj23(i) - aj13(i) * aj22(i)
                aic23(i) = -aj11(i) * aj23(i) + aj13(i) * aj21(i)
                aic33(i) = aj11(i) * aj22(i) - aj12(i) * aj21(i)
            End If

        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine stric1
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to compute coefficients for chain rule at center of element
'c
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
'c
'c	------------------------ Added by Qiang, 03-09-2010 -----------
'c	incomp=1: indicates incompatible element
'	common/incompatible/incomp(20)						
'c	---------------------------------------------------------------
'c
'      do 10 i=lft,llt
'c	
'
'	if(incomp(matp(i))==0) then
'		aic11(i)=0.
'		aic21(i)=0.
'		aic31(i)=0.
'		aic12(i)=0.
'		aic22(i)=0.
'		aic32(i)=0.
'		aic13(i)=0.
'		aic23(i)=0.
'		aic33(i)=0.
'	else
'		call basisf (p11,p21,p31,9,1,1,1.,i)	
'
'c NKC
'
'      aj11(i)=p11*x12(i)+p13*x34(i)+p15*x56(i)+p17*x78(i)
'      aj12(i)=p11*y12(i)+p13*y34(i)+p15*y56(i)+p17*y78(i)
'      aj13(i)=p11*z12(i)+p13*z34(i)+p15*z56(i)+p17*z78(i)
'      aj21(i)=p21*x14(i)+p22*x23(i)+p25*x58(i)+p26*x67(i)
'      aj22(i)=p21*y14(i)+p22*y23(i)+p25*y58(i)+p26*y67(i)
'      aj23(i)=p21*z14(i)+p22*z23(i)+p25*z58(i)+p26*z67(i)
'      aj31(i)=p31*x15(i)+p32*x26(i)+p33*x37(i)+p34*x48(i)
'      aj32(i)=p31*y15(i)+p32*y26(i)+p33*y37(i)+p34*y48(i)
'      aj33(i)=p31*z15(i)+p32*z26(i)+p33*z37(i)+p34*z48(i)
'
'c
'      aic11(i)= aj22(i)*aj33(i)-aj23(i)*aj32(i)
'      aic21(i)=-aj21(i)*aj33(i)+aj23(i)*aj31(i)
'      aic31(i)= aj21(i)*aj32(i)-aj22(i)*aj31(i)
'      aic12(i)=-aj12(i)*aj33(i)+aj13(i)*aj32(i)
'      aic22(i)= aj11(i)*aj33(i)-aj13(i)*aj31(i)
'      aic32(i)=-aj11(i)*aj32(i)+aj12(i)*aj31(i)
'      aic13(i)= aj12(i)*aj23(i)-aj13(i)*aj22(i)
'      aic23(i)=-aj11(i)*aj23(i)+aj13(i)*aj21(i)
'      aic33(i)= aj11(i)*aj22(i)-aj12(i)*aj21(i)
'	end if										
'
'10	continue									
'c
'      return
'      end

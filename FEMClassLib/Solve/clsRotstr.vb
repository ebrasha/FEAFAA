'This file contains all the methods of rotstr.f
Partial Public Class clsSolve


    
    Private t4(64), t5(64), t6(64) As Double

    Dim a11(64), a12(64), a13(64), a21(64), a22(64), a23(64), a31(64), a32(64), a33(64) As Double  ' vect5 YC?



    ''' <summary>
    ''' rotate strain tensor
    ''' </summary>
    ''' <param name="d1"></param>
    ''' <param name="d2"></param>
    ''' <param name="d3"></param>
    ''' <param name="d4"></param>
    ''' <param name="d5"></param>
    ''' <param name="d6"></param>
    ''' <param name="d7"></param>
    ''' <param name="d8"></param>
    ''' <param name="d9"></param>
    Public Sub rotstr(ByRef d1() As Double, ByRef d2() As Double, ByRef d3() As Double,
                      ByRef d4() As Double, ByRef d5() As Double, ByRef d6() As Double,
                      ByRef d7() As Double, ByRef d8() As Double, ByRef d9() As Double)

        Dim i As Integer

        For i = lft To llt
            t4(i) = 0.5 * (d4(i) + d6(i))
            t5(i) = 0.5 * (d7(i) + d9(i))
            t6(i) = 0.5 * (d5(i) + d8(i))
        Next
        For i = lft To llt
            a11(i) = d1(i) * q11(i) + t4(i) * q21(i) + t6(i) * q31(i)
            a12(i) = d1(i) * q12(i) + t4(i) * q22(i) + t6(i) * q32(i)
            a13(i) = d1(i) * q13(i) + t4(i) * q23(i) + t6(i) * q33(i)
            a21(i) = t4(i) * q11(i) + d2(i) * q21(i) + t5(i) * q31(i)
            a22(i) = t4(i) * q12(i) + d2(i) * q22(i) + t5(i) * q32(i)
            a23(i) = t4(i) * q13(i) + d2(i) * q23(i) + t5(i) * q33(i)
            a31(i) = t6(i) * q11(i) + t5(i) * q21(i) + d3(i) * q31(i)
            a32(i) = t6(i) * q12(i) + t5(i) * q22(i) + d3(i) * q32(i)
            a33(i) = t6(i) * q13(i) + t5(i) * q23(i) + d3(i) * q33(i)
        Next
        For i = lft To llt
            d1(i) = q11(i) * a11(i) + q21(i) * a21(i) + q31(i) * a31(i)
            d2(i) = q12(i) * a12(i) + q22(i) * a22(i) + q32(i) * a32(i)
            d3(i) = q13(i) * a13(i) + q23(i) * a23(i) + q33(i) * a33(i)
            d4(i) = 2.0 * (q11(i) * a12(i) + q21(i) * a22(i) + q31(i) * a32(i))
            d5(i) = 2.0 * (q12(i) * a13(i) + q22(i) * a23(i) + q32(i) * a33(i))
            d6(i) = 2.0 * (q11(i) * a13(i) + q21(i) * a23(i) + q31(i) * a33(i))
        Next


    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine rotstr (d1,d2,d3,d4,d5,d6,d7,d8,d9)
'c
'      implicit double precision (a-h,o-z)                            
'c
'c===> module to rotate strain tensor
'c
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect0/
'     1 q11(64),q12(64),q13(64),q21(64),q22(64),q23(64),
'     2 q31(64),q32(64),q33(64),r11(64),r12(64),r13(64),
'     3 r21(64),r22(64),r23(64),r31(64),r32(64),r33(64),
'     4 s11(64),s12(64),s13(64),s21(64),s22(64),s23(64),
'     5 s31(64),s32(64),s33(64)
'      common/vect5/
'     1 ff(9,64),a11(64),a12(64),a13(64),a21(64),a22(64),a23(64),
'     2 a31(64),a32(64),a33(64),t4(64),t5(64),t6(64),dum5(37,64)
'      dimension d1(*),d2(*),d3(*),d4(*),d5(*),d6(*),d7(*),d8(*),d9(*)
'c
'      do 10 i=lft,llt
'      t4(i)=.50*(d4(i)+d6(i))
'      t5(i)=.50*(d7(i)+d9(i))
'      t6(i)=.50*(d5(i)+d8(i))
'   10 continue
'      do 20 i=lft,llt
'      a11(i)=d1(i)*q11(i)+t4(i)*q21(i)+t6(i)*q31(i)
'      a12(i)=d1(i)*q12(i)+t4(i)*q22(i)+t6(i)*q32(i)
'      a13(i)=d1(i)*q13(i)+t4(i)*q23(i)+t6(i)*q33(i)
'      a21(i)=t4(i)*q11(i)+d2(i)*q21(i)+t5(i)*q31(i)
'      a22(i)=t4(i)*q12(i)+d2(i)*q22(i)+t5(i)*q32(i)
'      a23(i)=t4(i)*q13(i)+d2(i)*q23(i)+t5(i)*q33(i)
'      a31(i)=t6(i)*q11(i)+t5(i)*q21(i)+d3(i)*q31(i)
'      a32(i)=t6(i)*q12(i)+t5(i)*q22(i)+d3(i)*q32(i)
'      a33(i)=t6(i)*q13(i)+t5(i)*q23(i)+d3(i)*q33(i)
'   20 continue
'      do 30 i=lft,llt
'      d1(i)=q11(i)*a11(i)+q21(i)*a21(i)+q31(i)*a31(i)
'      d2(i)=q12(i)*a12(i)+q22(i)*a22(i)+q32(i)*a32(i)
'      d3(i)=q13(i)*a13(i)+q23(i)*a23(i)+q33(i)*a33(i)
'      d4(i)=2.0*(q11(i)*a12(i)+q21(i)*a22(i)+q31(i)*a32(i))
'      d5(i)=2.0*(q12(i)*a13(i)+q22(i)*a23(i)+q32(i)*a33(i))
'      d6(i)=2.0*(q11(i)*a13(i)+q21(i)*a23(i)+q31(i)*a33(i))
'   30 continue
'      return
'      end

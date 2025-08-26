'This file contains all the methods of rotat1.f
Partial Public Class clsSolve

    ''' <summary>
    ''' transform stress to un-rotated configuration
    ''' </summary>
    ''' <param name="sig"></param>
    ''' <param name="ln"></param>
    Public Sub rotat1(ByRef sig(,) As Double, ByRef ln As Integer)

        Dim i As Integer  ' YC 102418

        For i = mft To mlt

            ' YC 102418
            'a11(i) = sig(0, i) * r11(i) + sig(3, i) * r21(i) + sig(5, i) * r31(i)
            'a12(i) = sig(0, i) * r12(i) + sig(3, i) * r22(i) + sig(5, i) * r32(i)
            'a13(i) = sig(0, i) * r13(i) + sig(3, i) * r23(i) + sig(5, i) * r33(i)
            'a21(i) = sig(3, i) * r11(i) + sig(1, i) * r21(i) + sig(4, i) * r31(i)
            'a22(i) = sig(3, i) * r12(i) + sig(1, i) * r22(i) + sig(4, i) * r32(i)
            'a23(i) = sig(3, i) * r13(i) + sig(1, i) * r23(i) + sig(4, i) * r33(i)
            'a31(i) = sig(5, i) * r11(i) + sig(4, i) * r21(i) + sig(2, i) * r31(i)
            'a32(i) = sig(5, i) * r12(i) + sig(4, i) * r22(i) + sig(2, i) * r32(i)
            'a33(i) = sig(5, i) * r13(i) + sig(4, i) * r23(i) + sig(2, i) * r33(i)
            a11(i) = sig(1, i) * r11(i) + sig(4, i) * r21(i) + sig(6, i) * r31(i)
            a12(i) = sig(1, i) * r12(i) + sig(4, i) * r22(i) + sig(6, i) * r32(i)
            a13(i) = sig(1, i) * r13(i) + sig(4, i) * r23(i) + sig(6, i) * r33(i)
            a21(i) = sig(4, i) * r11(i) + sig(2, i) * r21(i) + sig(5, i) * r31(i)
            a22(i) = sig(4, i) * r12(i) + sig(2, i) * r22(i) + sig(5, i) * r32(i)
            a23(i) = sig(4, i) * r13(i) + sig(2, i) * r23(i) + sig(5, i) * r33(i)
            a31(i) = sig(6, i) * r11(i) + sig(5, i) * r21(i) + sig(3, i) * r31(i)
            a32(i) = sig(6, i) * r12(i) + sig(5, i) * r22(i) + sig(3, i) * r32(i)
            a33(i) = sig(6, i) * r13(i) + sig(5, i) * r23(i) + sig(3, i) * r33(i)
            ' YC 102418 END

        Next

        For i = mft To mlt

            ' YC 102418
            'sig(0, i) = r11(i) * a11(i) + r21(i) * a21(i) + r31(i) * a31(i)
            'sig(1, i) = r12(i) * a12(i) + r22(i) * a22(i) + r32(i) * a32(i)
            'sig(2, i) = r13(i) * a13(i) + r23(i) * a23(i) + r33(i) * a33(i)
            'sig(3, i) = r11(i) * a12(i) + r21(i) * a22(i) + r31(i) * a32(i)
            'sig(4, i) = r12(i) * a13(i) + r22(i) * a23(i) + r32(i) * a33(i)
            'sig(5, i) = r11(i) * a13(i) + r21(i) * a23(i) + r31(i) * a33(i)
            sig(1, i) = r11(i) * a11(i) + r21(i) * a21(i) + r31(i) * a31(i)
            sig(2, i) = r12(i) * a12(i) + r22(i) * a22(i) + r32(i) * a32(i)
            sig(3, i) = r13(i) * a13(i) + r23(i) * a23(i) + r33(i) * a33(i)
            sig(4, i) = r11(i) * a12(i) + r21(i) * a22(i) + r31(i) * a32(i)
            sig(5, i) = r12(i) * a13(i) + r22(i) * a23(i) + r32(i) * a33(i)
            sig(6, i) = r11(i) * a13(i) + r21(i) * a23(i) + r31(i) * a33(i)
            ' YC 102418 END

        Next
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine rotat1 (sig,ln)
'c
'
'      implicit double precision (a-h,o-z)                          
'c
'c===> module to transform stress to un-rotated configuration
'c
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect0/
'     1 q11(64),q12(64),q13(64),q21(64),q22(64),q23(64),
'     2 q31(64),q32(64),q33(64),r11(64),r12(64),r13(64),
'     3 r21(64),r22(64),r23(64),r31(64),r32(64),r33(64),
'     4 s11(64),s12(64),s13(64),s21(64),s22(64),s23(64),
'     5 s31(64),s32(64),s33(64)
'      common/vect5/
'     1 a11(64),a12(64),a13(64),a21(64),a22(64),a23(64),
'     2 a31(64),a32(64),a33(64),t4(64),t5(64),t6(64),dum5(2944)
'c
'      dimension sig(ln,*)
'c
'      do 10 i=mft,mlt
'      a11(i)=sig(1,i)*r11(i)+sig(4,i)*r21(i)+sig(6,i)*r31(i)
'      a12(i)=sig(1,i)*r12(i)+sig(4,i)*r22(i)+sig(6,i)*r32(i)
'      a13(i)=sig(1,i)*r13(i)+sig(4,i)*r23(i)+sig(6,i)*r33(i)
'      a21(i)=sig(4,i)*r11(i)+sig(2,i)*r21(i)+sig(5,i)*r31(i)
'      a22(i)=sig(4,i)*r12(i)+sig(2,i)*r22(i)+sig(5,i)*r32(i)
'      a23(i)=sig(4,i)*r13(i)+sig(2,i)*r23(i)+sig(5,i)*r33(i)
'      a31(i)=sig(6,i)*r11(i)+sig(5,i)*r21(i)+sig(3,i)*r31(i)
'      a32(i)=sig(6,i)*r12(i)+sig(5,i)*r22(i)+sig(3,i)*r32(i)
'      a33(i)=sig(6,i)*r13(i)+sig(5,i)*r23(i)+sig(3,i)*r33(i)
'   10 continue
'      do 20 i=mft,mlt
'      sig(1,i)=r11(i)*a11(i)+r21(i)*a21(i)+r31(i)*a31(i)
'      sig(2,i)=r12(i)*a12(i)+r22(i)*a22(i)+r32(i)*a32(i)
'      sig(3,i)=r13(i)*a13(i)+r23(i)*a23(i)+r33(i)*a33(i)
'      sig(4,i)=r11(i)*a12(i)+r21(i)*a22(i)+r31(i)*a32(i)
'      sig(5,i)=r12(i)*a13(i)+r22(i)*a23(i)+r32(i)*a33(i)
'      sig(6,i)=r11(i)*a13(i)+r21(i)*a23(i)+r31(i)*a33(i)
'   20 continue
'c
'      return
'      end

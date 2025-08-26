'This file contains all the methods of rotat2.f
Partial Public Class clsSolve

    ''' <summary>
    ''' transform un-rotated stress to rotated configuration
    ''' </summary>
    ''' <param name="sig"></param>
    ''' <param name="ln"></param>
    Public Sub rotat2(ByRef sig(,) As Double, ByRef ln As Integer)

        Dim i As Integer    'YC 102418

        For i = mft To mlt

            'YC 102418
            'a11(i) = sig(0, i) * s11(i) + sig(3, i) * s12(i) + sig(5, i) * s13(i)
            'a12(i) = sig(0, i) * s21(i) + sig(3, i) * s22(i) + sig(5, i) * s23(i)
            'a13(i) = sig(0, i) * s31(i) + sig(3, i) * s32(i) + sig(5, i) * s33(i)
            'a21(i) = sig(3, i) * s11(i) + sig(1, i) * s12(i) + sig(4, i) * s13(i)
            'a22(i) = sig(3, i) * s21(i) + sig(1, i) * s22(i) + sig(4, i) * s23(i)
            'a23(i) = sig(3, i) * s31(i) + sig(1, i) * s32(i) + sig(4, i) * s33(i)
            'a31(i) = sig(5, i) * s11(i) + sig(4, i) * s12(i) + sig(2, i) * s13(i)
            'a32(i) = sig(5, i) * s21(i) + sig(4, i) * s22(i) + sig(2, i) * s23(i)
            'a33(i) = sig(5, i) * s31(i) + sig(4, i) * s32(i) + sig(2, i) * s33(i)
            a11(i) = sig(1, i) * s11(i) + sig(4, i) * s12(i) + sig(6, i) * s13(i)
            a12(i) = sig(1, i) * s21(i) + sig(4, i) * s22(i) + sig(6, i) * s23(i)
            a13(i) = sig(1, i) * s31(i) + sig(4, i) * s32(i) + sig(6, i) * s33(i)
            a21(i) = sig(4, i) * s11(i) + sig(2, i) * s12(i) + sig(5, i) * s13(i)
            a22(i) = sig(4, i) * s21(i) + sig(2, i) * s22(i) + sig(5, i) * s23(i)
            a23(i) = sig(4, i) * s31(i) + sig(2, i) * s32(i) + sig(5, i) * s33(i)
            a31(i) = sig(6, i) * s11(i) + sig(5, i) * s12(i) + sig(3, i) * s13(i)
            a32(i) = sig(6, i) * s21(i) + sig(5, i) * s22(i) + sig(3, i) * s23(i)
            a33(i) = sig(6, i) * s31(i) + sig(5, i) * s32(i) + sig(3, i) * s33(i)
            'YC 102418 END

        Next

        For i = mft To mlt

            'YC 102418 
            'sig(0, i) = s11(i) * a11(i) + s12(i) * a21(i) + s13(i) * a31(i)
            'sig(1, i) = s21(i) * a12(i) + s22(i) * a22(i) + s23(i) * a32(i)
            'sig(2, i) = s31(i) * a13(i) + s32(i) * a23(i) + s33(i) * a33(i)
            'sig(3, i) = s11(i) * a12(i) + s12(i) * a22(i) + s13(i) * a32(i)
            'sig(4, i) = s21(i) * a13(i) + s22(i) * a23(i) + s23(i) * a33(i)
            'sig(5, i) = s11(i) * a13(i) + s12(i) * a23(i) + s13(i) * a33(i)
            sig(1, i) = s11(i) * a11(i) + s12(i) * a21(i) + s13(i) * a31(i)
            sig(2, i) = s21(i) * a12(i) + s22(i) * a22(i) + s23(i) * a32(i)
            sig(3, i) = s31(i) * a13(i) + s32(i) * a23(i) + s33(i) * a33(i)
            sig(4, i) = s11(i) * a12(i) + s12(i) * a22(i) + s13(i) * a32(i)
            sig(5, i) = s21(i) * a13(i) + s22(i) * a23(i) + s23(i) * a33(i)
            sig(6, i) = s11(i) * a13(i) + s12(i) * a23(i) + s13(i) * a33(i)
            'YC 102418 END

        Next

        For i = mft To mlt

            'YC 102418 
            'sig11(i) = sig(0, i)
            'sig22(i) = sig(1, i)
            'sig33(i) = sig(2, i)
            'sig12(i) = sig(3, i)
            'sig23(i) = sig(4, i)
            'sig31(i) = sig(5, i)
            sig11(i) = sig(1, i)
            sig22(i) = sig(2, i)
            sig33(i) = sig(3, i)
            sig12(i) = sig(4, i)
            sig23(i) = sig(5, i)
            sig31(i) = sig(6, i)
            'YC 102418 END

        Next
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine rotat2 (sig,ln)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'c
'c===> module to transform un-rotated stress to rotated configuration
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
'      common/vect21/
'     1 sig11(64),sig22(64),sig33(64),
'     2 sig12(64),sig23(64),sig31(64)
'c
'      dimension sig(ln,*)
'c
'      do 10 i=mft,mlt
'      a11(i)=sig(1,i)*s11(i)+sig(4,i)*s12(i)+sig(6,i)*s13(i)
'      a12(i)=sig(1,i)*s21(i)+sig(4,i)*s22(i)+sig(6,i)*s23(i)
'      a13(i)=sig(1,i)*s31(i)+sig(4,i)*s32(i)+sig(6,i)*s33(i)
'      a21(i)=sig(4,i)*s11(i)+sig(2,i)*s12(i)+sig(5,i)*s13(i)
'      a22(i)=sig(4,i)*s21(i)+sig(2,i)*s22(i)+sig(5,i)*s23(i)
'      a23(i)=sig(4,i)*s31(i)+sig(2,i)*s32(i)+sig(5,i)*s33(i)
'      a31(i)=sig(6,i)*s11(i)+sig(5,i)*s12(i)+sig(3,i)*s13(i)
'      a32(i)=sig(6,i)*s21(i)+sig(5,i)*s22(i)+sig(3,i)*s23(i)
'      a33(i)=sig(6,i)*s31(i)+sig(5,i)*s32(i)+sig(3,i)*s33(i)
'   10 continue
'      do 20 i=mft,mlt
'      sig(1,i)=s11(i)*a11(i)+s12(i)*a21(i)+s13(i)*a31(i)
'      sig(2,i)=s21(i)*a12(i)+s22(i)*a22(i)+s23(i)*a32(i)
'      sig(3,i)=s31(i)*a13(i)+s32(i)*a23(i)+s33(i)*a33(i)
'      sig(4,i)=s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i)
'      sig(5,i)=s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i)
'      sig(6,i)=s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i)
'   20 continue
'      do 30 i=mft,mlt
'      sig11(i)=sig(1,i)
'      sig22(i)=sig(2,i)
'      sig33(i)=sig(3,i)
'      sig12(i)=sig(4,i)
'      sig23(i)=sig(5,i)
'      sig31(i)=sig(6,i)
'   30 continue
'      return
'      end

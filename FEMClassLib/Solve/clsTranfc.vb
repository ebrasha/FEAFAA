'This file contains all the methods of tranfc.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to compute rank-four transformation of material moduli
    ''' </summary>
    ''' <param name="i"></param>
    Public Sub tranfc(i As Integer)

        'Dim t(5, 5), e(5, 5) As Double ' YC 121219
        Dim t(6, 6), e(6, 6) As Double

        Dim m, n, k As Integer  ' YC 121219
        Dim d_8 = dsave 'common/vect8/


        t(1, 1) = Math.Pow(s11(i), 2)
        t(1, 2) = Math.Pow(s12(i), 2)
        t(1, 3) = Math.Pow(s13(i), 2)
        t(1, 4) = 2.0 * s11(i) * s12(i)
        t(1, 5) = 2.0 * s12(i) * s13(i)
        t(1, 6) = 2.0 * s13(i) * s11(i)
        t(2, 1) = Math.Pow(s21(i), 2)
        t(2, 2) = Math.Pow(s22(i), 2)
        t(2, 3) = Math.Pow(s23(i), 2)
        t(2, 4) = 2.0 * s21(i) * s22(i)
        t(2, 5) = 2.0 * s22(i) * s23(i)
        t(2, 6) = 2.0 * s23(i) * s21(i)
        t(3, 1) = Math.Pow(s31(i), 2)
        t(3, 2) = Math.Pow(s32(i), 2)
        t(3, 3) = Math.Pow(s33(i), 2)
        t(3, 4) = 2.0 * s31(i) * s32(i)
        t(3, 5) = 2.0 * s32(i) * s33(i)
        t(3, 6) = 2.0 * s33(i) * s31(i)
        t(4, 1) = s11(i) * s21(i)
        t(4, 2) = s12(i) * s22(i)
        t(4, 3) = s13(i) * s23(i)
        t(4, 4) = s11(i) * s22(i) + s21(i) * s12(i)
        t(4, 5) = s12(i) * s23(i) + s22(i) * s13(i)
        t(4, 6) = s13(i) * s21(i) + s23(i) * s11(i)
        t(5, 1) = s21(i) * s31(i)
        t(5, 2) = s22(i) * s32(i)
        t(5, 3) = s23(i) * s33(i)
        t(5, 4) = s21(i) * s32(i) + s31(i) * s22(i)
        t(5, 5) = s22(i) * s33(i) + s32(i) * s23(i)
        t(5, 6) = s23(i) * s31(i) + s33(i) * s21(i)
        t(6, 1) = s31(i) * s11(i)
        t(6, 2) = s32(i) * s12(i)
        t(6, 3) = s33(i) * s13(i)
        t(6, 4) = s31(i) * s12(i) + s11(i) * s32(i)
        t(6, 5) = s32(i) * s13(i) + s12(i) * s33(i)
        t(6, 6) = s33(i) * s11(i) + s13(i) * s31(i)
        For m = 1 To 6
            For n = 1 To 6
                e(m, n) = 0.0
                For k = 1 To 6
                    e(m, n) = e(m, n) + t(m, k) * d_8(k, n, i)
                Next
            Next
        Next
        For m = 1 To 6
            For n = m To 6
                d_8(m, n, i) = 0.0
                For k = 1 To 6
                    d_8(m, n, i) = d_8(m, n, i) + e(m, k) * t(n, k)
                Next
                d_8(n, m, i) = d_8(m, n, i)
            Next
        Next

    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine tranfc(i)
'c
'
'      implicit double precision (a-h,o-z)                         
'c
'c===> module to compute rank-four transformation of material moduli
'c
'      common/vect0/
'     1 q11(64),q12(64),q13(64),q21(64),q22(64),q23(64),
'     2 q31(64),q32(64),q33(64),r11(64),r12(64),r13(64),
'     3 r21(64),r22(64),r23(64),r31(64),r32(64),r33(64),
'     4 s11(64),s12(64),s13(64),s21(64),s22(64),s23(64),
'     5 s31(64),s32(64),s33(64)
'      common/vect8/
'     1 d(6,6,64)
'      dimension t(6,6),e(6,6)
'      t(1,1)=s11(i)**2
'      t(1,2)=s12(i)**2
'      t(1,3)=s13(i)**2
'      t(1,4)=2.*s11(i)*s12(i)
'      t(1,5)=2.*s12(i)*s13(i)
'      t(1,6)=2.*s13(i)*s11(i)
'      t(2,1)=s21(i)**2
'      t(2,2)=s22(i)**2
'      t(2,3)=s23(i)**2
'      t(2,4)=2.*s21(i)*s22(i)
'      t(2,5)=2.*s22(i)*s23(i)
'      t(2,6)=2.*s23(i)*s21(i)
'      t(3,1)=s31(i)**2
'      t(3,2)=s32(i)**2
'      t(3,3)=s33(i)**2
'      t(3,4)=2.*s31(i)*s32(i)
'      t(3,5)=2.*s32(i)*s33(i)
'      t(3,6)=2.*s33(i)*s31(i)
'      t(4,1)=s11(i)*s21(i)
'      t(4,2)=s12(i)*s22(i)
'      t(4,3)=s13(i)*s23(i)
'      t(4,4)=s11(i)*s22(i)+s21(i)*s12(i)
'      t(4,5)=s12(i)*s23(i)+s22(i)*s13(i)
'      t(4,6)=s13(i)*s21(i)+s23(i)*s11(i)
'      t(5,1)=s21(i)*s31(i)
'      t(5,2)=s22(i)*s32(i)
'      t(5,3)=s23(i)*s33(i)
'      t(5,4)=s21(i)*s32(i)+s31(i)*s22(i)
'      t(5,5)=s22(i)*s33(i)+s32(i)*s23(i)
'      t(5,6)=s23(i)*s31(i)+s33(i)*s21(i)
'      t(6,1)=s31(i)*s11(i)
'      t(6,2)=s32(i)*s12(i)
'      t(6,3)=s33(i)*s13(i)
'      t(6,4)=s31(i)*s12(i)+s11(i)*s32(i)
'      t(6,5)=s32(i)*s13(i)+s12(i)*s33(i)
'      t(6,6)=s33(i)*s11(i)+s13(i)*s31(i)
'      do 20 m=1,6
'      do 20 n=1,6
'      e(m,n)=0.0
'      do 10 k=1,6
'   10 e(m,n)=e(m,n)+t(m,k)*d(k,n,i)
'   20 continue
'      do 40 m=1,6
'      do 40 n=m,6
'      d(m,n,i)=0.0
'      do 30 k=1,6
'   30 d(m,n,i)=d(m,n,i)+e(m,k)*t(n,k)
'   40 d(n,m,i)=d(m,n,i)
'      
'      return
'      end

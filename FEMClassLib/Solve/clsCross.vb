'This file contains all the methods of cross.f
Partial Public Class clsSolve

    Public Sub cross(ByRef x(,) As Double, ByRef llc As Integer, ByRef lresrt As Integer, ByRef resltl As Double, ByRef result As Double, ByVal i As Integer, ByVal k As Integer, ByVal m As Integer, ByVal n As Integer)   ' QW 5-12-2019
        Dim c11 = x(1, m) - x(1, k)
        Dim c12 = x(2, m) - x(2, k)
        Dim c13 = x(3, m) - x(3, k)
        Dim c21 = x(1, n) - x(1, k)
        Dim c22 = x(2, n) - x(2, k)
        Dim c23 = x(3, n) - x(3, k)
        Dim c31 = c12 * c23 - c13 * c22
        Dim c32 = c13 * c21 - c11 * c23
        Dim c33 = c11 * c22 - c12 * c21
        Dim sl1 = x(1, i) - x(1, k)
        Dim sl2 = x(2, i) - x(2, k)
        Dim sl3 = x(3, i) - x(3, k)
        Dim da = (c31 * sl1 + c32 * sl2 + c33 * sl3) / (c31 * c31 + c32 * c32 + c33 * c33)
        sl1 = sl1 - da * c31
        sl2 = sl2 - da * c32
        sl3 = sl3 - da * c33
        Dim c1dc1 = c11 * c11 + c12 * c12 + c13 * c13
        Dim c2dc2 = c21 * c21 + c22 * c22 + c23 * c23
        Dim sldc2 = sl1 * c21 + sl2 * c22 + sl3 * c23
        Dim c1dc2 = c11 * c21 + c12 * c22 + c13 * c23
        Dim sldc1 = sl1 * c11 + sl2 * c12 + sl3 * c13
        Dim sldsl = sl1 * sl1 + sl2 * sl2 + sl3 * sl3
        Dim dprd1 = sldc2 / Math.Sqrt(c2dc2)
        Dim dprd2 = sldc1 / Math.Sqrt(c1dc1)
        Dim dprdf = Math.Max(dprd1, dprd2)
        If resltl > dprdf Then GoTo 10
        lresrt = llc
        resltl = dprdf
10:     Dim resul1 = c1dc1 * sldc2 - c1dc2 * sldc1
        Dim resul2 = sldc1 * sldc2 - c1dc2 * sldsl
        Dim resul3 = c2dc2 * sldc1 - c1dc2 * sldc2
        result = -1.0
        If Math.Min(resul1, Math.Min(resul2, resul3)) < 0.0 Then Return
        result = 1.0

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine cross(x)
'c
'      implicit double precision (a-h,o-z)                           
'c
'c===> module to
'c
'      common/bk42/llc,lresrt,resltl ,result,i,k,m,n
'      dimension x(3,*)
'
'      c11=x(1,m)-x(1,k)
'      c12=x(2,m)-x(2,k)
'      c13=x(3,m)-x(3,k)
'      c21=x(1,n)-x(1,k)
'      c22=x(2,n)-x(2,k)
'      c23=x(3,n)-x(3,k)
'      c31=c12*c23-c13*c22
'      c32=c13*c21-c11*c23
'      c33=c11*c22-c12*c21
'      sl1=x(1,i)-x(1,k)
'      sl2=x(2,i)-x(2,k)
'      sl3=x(3,i)-x(3,k)
'      da=(c31*sl1+c32*sl2+c33*sl3)/(c31*c31+c32*c32+c33*c33)
'      sl1=sl1-da*c31
'      sl2=sl2-da*c32
'      sl3=sl3-da*c33
'      c1dc1=c11*c11+c12*c12+c13*c13
'      c2dc2=c21*c21+c22*c22+c23*c23
'      sldc2=sl1*c21+sl2*c22+sl3*c23
'      c1dc2=c11*c21+c12*c22+c13*c23
'      sldc1=sl1*c11+sl2*c12+sl3*c13
'      sldsl=sl1*sl1+sl2*sl2+sl3*sl3
'      dprd1=sldc2/sqrt(c2dc2)
'      dprd2=sldc1/sqrt(c1dc1)
'      dprdf=max(dprd1,dprd2)
'      if(resltl .gt.dprdf) go to 10
'      lresrt=llc
'      resltl =dprdf
' 10   resul1=c1dc1*sldc2-c1dc2*sldc1
'      resul2=sldc1*sldc2-c1dc2*sldsl
'      resul3=c2dc2*sldc1-c1dc2*sldc2
'      result=-1.0
'      if(min(resul1,resul2,resul3).lt.0.0) return
'      result= 1.0
'      return
'      end

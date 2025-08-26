'This file contains all the methods of nstiff.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to compute penalty stiffness w.r.t. master facet applicable to frictionless surface
    ''' </summary>
    ''' <param name="xk"></param>
    ''' <param name="n1"></param>
    ''' <param name="n2"></param>
    ''' <param name="n3"></param>
    ''' <param name="e"></param>
    Public Sub nstiff(ByRef xk As Double, ByRef n1 As Double, ByRef n2 As Double,
                      ByRef n3 As Double, ByRef e() As Double)                      ' QW 03-26-2019

        ' YC 101418
        'e(0) = xk * Math.Pow(n1, 2)
        'e(1) = xk * n1 * n2
        'e(2) = xk * Math.Pow(n2, 2)
        'e(3) = xk * n1 * n3
        'e(4) = xk * n2 * n3
        'e(5) = xk * Math.Pow(n3, 2)
        e(1) = xk * Math.Pow(n1, 2)
        e(2) = xk * n1 * n2
        e(3) = xk * Math.Pow(n2, 2)
        e(4) = xk * n1 * n3
        e(5) = xk * n2 * n3
        e(6) = xk * Math.Pow(n3, 2)
        ' YC 101418 END

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine nstiff(xk,n1,n2,n3,e)
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c
'c===> module to compute penalty stiffness w.r.t. master facet
'c     applicable to frictionless surface
'c
'
'      double precision n1,n2,n3
'
'      dimension e(*)
'
'      e(1)=xk*n1**2
'      e(2)=xk*n1*n2
'      e(3)=xk*n2**2
'      e(4)=xk*n1*n3
'      e(5)=xk*n2*n3
'      e(6)=xk*n3**2
'      return
'      end

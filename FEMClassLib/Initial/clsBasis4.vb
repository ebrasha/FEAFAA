'This file contains all the methods of basis4.f
Partial Public Class clsInitial

    ''' <summary>
    ''' evaluate bilinear shape functions and derivatives
    ''' </summary>
    ''' <param name="r"></param>
    ''' <param name="s"></param>
    ''' <param name="h"></param>
    ''' <param name="p"></param>
    Public Sub basis4(ByRef r As Double, ByRef s As Double, ByRef hh() As Double, ByRef p(,) As Double)

        Dim rp = 1.0 + r
        Dim sp = 1.0 + s
        Dim rm = 1.0 - r
        Dim sm = 1.0 - s
        hh(1) = 0.25 * rp * sp
        hh(2) = 0.25 * rm * sp
        hh(3) = 0.25 * rm * sm
        hh(4) = 0.25 * rp * sm
        p(1, 1) = 0.25 * sp
        p(1, 2) = -p(1, 1)
        p(1, 3) = -0.25 * sm
        p(1, 4) = -p(1, 3)
        p(2, 1) = 0.25 * rp
        p(2, 2) = 0.25 * rm
        p(2, 3) = -p(2, 2)
        p(2, 4) = -p(2, 1)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine basis4(r,s,h,p)
'c
'
'      implicit double precision (a-h,o-z)                       
'c
'c===> module to evaluate bilinear shape functions & derivatives
'c
'      dimension h(*),p(2,*)
'
'      rp=1.0+r
'      sp=1.0+s
'      rm=1.0-r
'      sm=1.0-s
'      h(1)=0.25*rp*sp
'      h(2)=0.25*rm*sp
'      h(3)=0.25*rm*sm
'      h(4)=0.25*rp*sm
'      p(1,1)=0.25*sp
'      p(1,2)=-p(1,1)
'      p(1,3)=-0.25*sm
'      p(1,4)=-p(1,3)
'      p(2,1)=0.25*rp
'      p(2,2)=0.25*rm
'      p(2,3)=-p(2,2)
'      p(2,4)=-p(2,1)
'      return
'      end

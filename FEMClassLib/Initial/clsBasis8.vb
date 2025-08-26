'This file contains all the methods of basis8.f
Partial Public Class clsInitial

    ''' <summary>
    ''' evaluate trilinear shape functions and derivatives
    ''' </summary>
    ''' <param name="r"></param>
    ''' <param name="s"></param>
    ''' <param name="t"></param>
    ''' <param name="h"></param>
    ''' <param name="hix"></param>
    ''' <param name="hiy"></param>
    ''' <param name="hiz"></param>
    ''' <param name="pr"></param>
    ''' <param name="ps"></param>
    ''' <param name="pt"></param>
    Public Sub basis8(ByRef r As Double, ByRef s As Double, ByRef t As Double, ByRef h() As Double,
                      ByRef hix() As Double, ByRef hiy() As Double, ByRef hiz() As Double,
                      ByRef pr() As Double, ByRef ps() As Double, ByRef pt() As Double)

        Dim rp = 1.0 + r
        Dim sp = 1.0 + s
        Dim tp = 1.0 + t
        Dim rm = 1.0 - r
        Dim sm = 1.0 - s
        Dim tm = 1.0 - t

        h(1) = 0.125 * rm * sm * tm
        h(2) = 0.125 * rp * sm * tm
        h(3) = 0.125 * rp * sp * tm
        h(4) = 0.125 * rm * sp * tm
        h(5) = 0.125 * rm * sm * tp
        h(6) = 0.125 * rp * sm * tp
        h(7) = 0.125 * rp * sp * tp
        h(8) = 0.125 * rm * sp * tp

        hix(1) = 0.125 * (-2 * r / (1 - r)) * sm * tm
        hix(2) = 0.125 * (1 + 2 * r / (1 - r)) * sm * tm
        hix(3) = 0.125 * (1 + 2 * r / (1 - r)) * sp * tm
        hix(4) = 0.125 * (-2 * r / (1 - r)) * sp * tm
        hix(5) = 0.125 * (-2 * r / (1 - r)) * sm * tp
        hix(6) = 0.125 * (1 + 2 * r / (1 - r)) * sm * tp
        hix(7) = 0.125 * (1 + 2 * r / (1 - r)) * sp * tp
        hix(8) = 0.125 * (-2 * r / (1 - r)) * sp * tp

        hiy(1) = 0.125 * rm * (-2 + s / (1 - s)) * tm
        hiy(2) = 0.125 * rp * (-2 + s / (1 - s)) * tm
        hiy(3) = 0.125 * rp * (1 + 2 * s / (1 - s)) * tm
        hiy(4) = 0.125 * rm * (1 + 2 * s / (1 - s)) * tm
        hiy(5) = 0.125 * rm * (-2 + s / (1 - s)) * tp
        hiy(6) = 0.125 * rp * (-2 + s / (1 - s)) * tp
        hiy(7) = 0.125 * rp * (1 + 2 * s / (1 - s)) * tp
        hiy(8) = 0.125 * rm * (1 + 2 * s / (1 - s)) * tp

        hiz(1) = 0.125 * rm * sm * (-2 + t / (1 - t))
        hiz(2) = 0.125 * rp * sm * (-2 + t / (1 - t))
        hiz(3) = 0.125 * rp * sp * (-2 + t / (1 - t))
        hiz(4) = 0.125 * rm * sp * (-2 + t / (1 - t))
        hiz(5) = 0.125 * rm * sm * (1 + 2 * t / (1 - t))
        hiz(6) = 0.125 * rp * sm * (1 + 2 * t / (1 - t))
        hiz(7) = 0.125 * rp * sp * (1 + 2 * t / (1 - t))
        hiz(8) = 0.125 * rm * sp * (1 + 2 * t / (1 - t))

        pr(1) = -0.125 * sm * tm        ' QW 12-12-2018
        pr(2) = -pr(1)
        pr(3) = 0.125 * sp * tm
        pr(4) = -pr(3)
        pr(5) = -0.125 * sm * tp
        pr(6) = -pr(5)
        pr(7) = 0.125 * sp * tp
        pr(8) = -pr(7)
        ps(1) = -0.125 * rm * tm
        ps(2) = -0.125 * rp * tm
        ps(3) = -ps(2)
        ps(4) = -ps(1)
        ps(5) = -0.125 * rm * tp
        ps(6) = -0.125 * rp * tp
        ps(7) = -ps(6)
        ps(8) = -ps(5)
        pt(1) = -0.125 * rm * sm
        pt(2) = -0.125 * rp * sm
        pt(3) = -0.125 * rp * sp
        pt(4) = -0.125 * rm * sp
        pt(5) = -pt(1)
        pt(6) = -pt(2)
        pt(7) = -pt(3)
        pt(8) = -pt(4)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine basis8 (r,s,t,h,hix,hiy,hiz,pr,ps,pt)
'c
'
'      implicit double precision (a-h,o-z)                    
'c
'c===> module to evaluate trilinear shape functions & derivatives
'c
'      dimension h(*),hix(*),hiy(*),hiz(*),pr(*),ps(*),pt(*)
'
'      rp=1.0+r
'      sp=1.0+s
'      tp=1.0+t
'      rm=1.0-r
'      sm=1.0-s
'      tm=1.0-t
'      h(1)=.125*rm*sm*tm
'      h(2)=.125*rp*sm*tm
'      h(3)=.125*rp*sp*tm
'      h(4)=.125*rm*sp*tm
'      h(5)=.125*rm*sm*tp
'      h(6)=.125*rp*sm*tp
'      h(7)=.125*rp*sp*tp
'      h(8)=.125*rm*sp*tp
'
'      hix(1)=.125*(-2*r/(1-r))*sm*tm
'      hix(2)=.125*(1+2*r/(1-r))*sm*tm
'      hix(3)=.125*(1+2*r/(1-r))*sp*tm
'      hix(4)=.125*(-2*r/(1-r))*sp*tm
'      hix(5)=.125*(-2*r/(1-r))*sm*tp
'      hix(6)=.125*(1+2*r/(1-r))*sm*tp
'      hix(7)=.125*(1+2*r/(1-r))*sp*tp
'      hix(8)=.125*(-2*r/(1-r))*sp*tp
'
'      hiy(1)=.125*rm*(-2+s/(1-s))*tm
'      hiy(2)=.125*rp*(-2+s/(1-s))*tm
'      hiy(3)=.125*rp*(1+2*s/(1-s))*tm
'      hiy(4)=.125*rm*(1+2*s/(1-s))*tm
'      hiy(5)=.125*rm*(-2+s/(1-s))*tp
'      hiy(6)=.125*rp*(-2+s/(1-s))*tp
'      hiy(7)=.125*rp*(1+2*s/(1-s))*tp
'      hiy(8)=.125*rm*(1+2*s/(1-s))*tp
'
'      hiz(1)=.125*rm*sm*(-2+t/(1-t))
'      hiz(2)=.125*rp*sm*(-2+t/(1-t))
'      hiz(3)=.125*rp*sp*(-2+t/(1-t))
'      hiz(4)=.125*rm*sp*(-2+t/(1-t))
'      hiz(5)=.125*rm*sm*(1+2*t/(1-t))
'      hiz(6)=.125*rp*sm*(1+2*t/(1-t))
'      hiz(7)=.125*rp*sp*(1+2*t/(1-t))
'      hiz(8)=.125*rm*sp*(1+2*t/(1-t))
'
'      pr(1)=-.125*sm*tm
'      pr(2)=-pr(1)
'      pr(3)= .125*sp*tm
'      pr(4)=-pr(3)
'      pr(5)=-.125*sm*tp
'      pr(6)=-pr(5)
'      pr(7)= .125*sp*tp
'      pr(8)=-pr(7)
'      ps(1)=-.125*rm*tm
'      ps(2)=-.125*rp*tm
'      ps(3)=-ps(2)
'      ps(4)=-ps(1)
'      ps(5)=-.125*rm*tp
'      ps(6)=-.125*rp*tp
'      ps(7)=-ps(6)
'      ps(8)=-ps(5)
'      pt(1)=-.125*rm*sm
'      pt(2)=-.125*rp*sm
'      pt(3)=-.125*rp*sp
'      pt(4)=-.125*rm*sp
'      pt(5)=-pt(1)
'      pt(6)=-pt(2)
'      pt(7)=-pt(3)
'      pt(8)=-pt(4)
'c
'      return
'      end

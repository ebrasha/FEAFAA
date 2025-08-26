'This file contains all the methods of ptimei.f
Partial Public Class clsInput
    ''' <summary>
    ''' compute volume between slave node and master facet
    ''' used as coarse test for proximity of slave node to master facet
    ''' (initialization phase)
    ''' </summary>
    ''' <param name="k1"></param>
    ''' <param name="k2"></param>
    ''' <param name="k3"></param>
    ''' <param name="det"></param>
    Public Sub ptimei(ByRef k1 As Integer, ByRef k2 As Integer, ByRef k3 As Integer, ByRef det As Double, ByRef xx1() As Double, ByRef xx2() As Double, ByRef xx3() As Double, ByRef xs As Double, ByRef ys As Double, ByRef zs As Double)
        Dim aj1 = xs - xx1(k1)
        Dim aj2 = ys - xx2(k1)
        Dim aj3 = zs - xx3(k1)
        Dim ajn = Math.Sqrt(aj1 * aj1 + aj2 * aj2 + aj3 * aj3)

        Dim thick As Double = 0
        If ajn <> 0 Then
            aj1 = (ajn - thick) * (xs - xx1(k1)) / ajn
            aj2 = (ajn - thick) * (ys - xx2(k1)) / ajn
            aj3 = (ajn - thick) * (zs - xx3(k1)) / ajn
        Else
            aj1 = -thick
            aj2 = -thick
            aj3 = -thick
        End If
        Dim aj4 = xx1(k2) - xx1(k1)
        Dim aj5 = xx2(k2) - xx2(k1)
        Dim aj6 = xx3(k2) - xx3(k1)
        Dim aj7 = xx1(k3) - xx1(k1)
        Dim aj8 = xx2(k3) - xx2(k1)
        Dim aj9 = xx3(k3) - xx3(k1)
        det = aj1 * aj5 * aj9 + aj2 * aj6 * aj7 + aj3 * aj4 * aj8 - aj3 * aj5 * aj7 - aj2 * aj4 * aj9 - aj1 * aj6 * aj8
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'c      subroutine ptimei(k1,k2,k3,ets,etm,det)
'      subroutine ptimei(k1,k2,k3,det)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to compute volume between slave node and master facet
'c     used as coarse test for proximity of slave node to master facet
'c     (initialization phase)
'c
'      common/bk44/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
'      common/bk45/xs,ys,zs,sig(3),epx,mx,ix(10),iy(10)
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'ptimei'
'      ik01 = ik01 + 1
'      end if
'
'      aj1=xs-xx1(k1)
'      aj2=ys-xx2(k1)
'      aj3=zs-xx3(k1)
'      ajn=sqrt(aj1*aj1+aj2*aj2+aj3*aj3)
'c      thick=(ets+etm)/2
'      thick=0.
'      if (ajn.ne.0) then
'       aj1=(ajn - thick) * (xs-xx1(k1))/ajn
'       aj2=(ajn - thick) * (ys-xx2(k1))/ajn
'       aj3=(ajn - thick) * (zs-xx3(k1))/ajn
'      else
'       aj1=-thick
'       aj2=-thick
'       aj3=-thick
'      endif
'      aj4=xx1(k2)-xx1(k1)
'      aj5=xx2(k2)-xx2(k1)
'      aj6=xx3(k2)-xx3(k1)
'      aj7=xx1(k3)-xx1(k1)
'      aj8=xx2(k3)-xx2(k1)
'      aj9=xx3(k3)-xx3(k1)
'      det=aj1*aj5*aj9+aj2*aj6*aj7+aj3*aj4*aj8
'     1   -aj3*aj5*aj7-aj2*aj4*aj9-aj1*aj6*aj8
'      return
'      end
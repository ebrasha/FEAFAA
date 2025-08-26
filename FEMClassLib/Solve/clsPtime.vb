'This file contains all the methods of ptime.f
Partial Public Class clsSolve

    ''' <summary>
    ''' compute volume between slave node and master facet
    ''' used as coarse test for proximity of slave node to master facet
    ''' </summary>
    ''' <param name="k1"></param>
    ''' <param name="k2"></param>
    ''' <param name="k3"></param>
    ''' <param name="det"></param>
    Public Sub ptimeSub(ByRef k1 As Integer, ByRef k2 As Integer, ByRef k3 As Integer, ByRef det As Double)
        'TODO
        'there is a member with ptime in common/bkspf1/. changed sub name to ptimeSub

        Dim aj1 = xs - xx1(k1)
        Dim aj2 = ys - xx2(k1)
        Dim aj3 = zs - xx3(k1)
        Dim aj4 = xx1(k2) - xx1(k1)
        Dim aj5 = xx2(k2) - xx2(k1)
        Dim aj6 = xx3(k2) - xx3(k1)
        Dim aj7 = xx1(k3) - xx1(k1)
        Dim aj8 = xx2(k3) - xx2(k1)
        Dim aj9 = xx3(k3) - xx3(k1)
        det = aj1 * aj5 * aj9 + aj2 * aj6 * aj7 + aj3 * aj4 * aj8 -
            aj3 * aj5 * aj7 - aj2 * aj4 * aj9 - aj1 * aj6 * aj8

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine ptime(k1,k2,k3,det)
'c
'
'      implicit double precision (a-h,o-z)                             
'
'c
'c===> module to compute volume between slave node and master facet
'c     used as coarse test for proximity of slave node to master facet
'c
'      common/bk44/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
'      common/bk45/xs,ys,zs,sig(3),epx,mx,ix(10),iy(10)
'
'      aj1=xs-xx1(k1)
'      aj2=ys-xx2(k1)
'      aj3=zs-xx3(k1)
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

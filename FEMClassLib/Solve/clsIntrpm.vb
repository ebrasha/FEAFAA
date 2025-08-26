'This file contains all the methods of intrpm.f
Partial Public Class clsSolve

    Public Sub intrpm(t() As Double, ym() As Double, temp As Double, prop(,) As Double, thrml() As Double)

        Dim m As Integer
        Dim ratio As Double

        For l = 2 To 8
            m = l - 1
            If ym(l) = 0.0 Then GoTo 20
            If temp > t(l) Then Continue For
            ratio = (temp - t(m)) / (t(l) - t(m))
            GoTo 30
        Next
        m = m + 1
20:     ratio = (temp - t(m - 1)) / (t(m) - t(m - 1))
        m = m - 1
30:     For l = 1 To 5
            thrml(l) = prop(m, l) + ratio * (prop(m + 1, l) - prop(m, l))
        Next
        If ratio < 1.05 And ratio > -0.05 Then Return
        Dim msg = " temperatures exceed range of material properties"

        'Call objNik3d.adios(2) 'YC 121219
        Call objComsub.adios(2)

    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine intrpm(t,ym,temp,prop,thrml)
'c
'
'      implicit double precision (a-h,o-z)                          
'
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      dimension t(*),ym(*),prop(8,*),thrml(*)
'      
'      do 10 l=2,8
'      m=l-1
'      if (ym(l).eq.0.0) go to 20
'      if (temp.gt.t(l)) go to 10
'      ratio=(temp-t(m))/(t(l)-t(m))
'      go to 30
'   10 continue
'      m=m+1
'   20 ratio=(temp-t(m-1))/(t(m)-t(m-1))
'      m=m-1
'   30 do 40 l=1,5
'   40 thrml(l)=prop(m,l)+ratio*(prop(m+1,l)-prop(m,l))
'      if (ratio.lt.1.05.and.ratio.gt.-.05) return
'      write(luo,50)
'      write(lutty,50)
'      call adios (2)
'c
'   50 format(/' temperatures exceed range of material properties')
'      end

'This file contains all the methods of fstiff.f
'Partial Public Class clsOthers  ' YC 102418-041519
Partial Public Class clsSolve

    ''' <summary>
    ''' to compute penalty stiffness w.r.t. master facet applicable when frictional effects are included
    ''' </summary>
    ''' <param name="n1"></param>
    ''' <param name="n2"></param>
    ''' <param name="n3"></param>
    ''' <param name="stfn"></param>
    ''' <param name="fx"></param>
    ''' <param name="fy"></param>
    ''' <param name="fz"></param>
    ''' <param name="fni"></param>
    ''' <param name="fmax"></param>
    ''' <param name="fmag"></param>
    ''' <param name="e"></param>
    ''' <param name="stft"></param>
    'Public Sub fstiff(ByRef n1 As Integer, ByRef n2 As Integer, ByRef n3 As Integer,
    '                  ByRef stfn As Double, ByRef fx As Double, ByRef fy As Double,
    '                  ByRef fz As Double, ByRef fni As Double, ByRef fmax As Double,
    '                  ByRef fmag As Double, ByRef e() As Double, ByRef stft As Double)     ' YC 102418-041519
    Public Sub fstiff(ByRef n1 As Double, ByRef n2 As Double, ByRef n3 As Double,
                  ByRef stfn As Double, ByRef fx As Double, ByRef fy As Double,
                  ByRef fz As Double, ByRef fni As Double, ByRef fmax As Double,
                  ByRef fmag As Double, ByRef e() As Double, ByRef stft As Double)

        'Dim t(2, 2), r(2), s(2, 2), a(2, 2), onorm, fl1, fl2 As Double     ' YC 102418-041519
        Dim t(3, 3), r(3), s(3, 3), a(3, 3), onorm, fl1, fl2 As Double, i, j As Integer


        Dim ip As Integer

        t(3, 1) = n1
        t(3, 2) = n2
        t(3, 3) = n3

        ip = 1
        If Math.Abs(t(3, 2)) < Math.Abs(t(3, 1)) Then ip = 2
        If Math.Abs(t(3, 3)) < Math.Abs(t(3, ip)) Then ip = 3

        r(1) = 0.0
        r(2) = 0.0
        r(3) = 0.0
        r(ip) = 1.0

        t(1, 1) = r(2) * t(3, 3) - r(3) * t(3, 2)
        t(1, 2) = r(3) * t(3, 1) - r(1) * t(3, 3)
        t(1, 3) = r(1) * t(3, 2) - r(2) * t(3, 1)

        onorm = 1.0 / Math.Sqrt(Math.Pow(t(1, 1), 2) + Math.Pow(t(1, 2), 2) + Math.Pow(t(1, 3), 2))
        t(1, 1) = onorm * t(1, 1)
        t(1, 2) = onorm * t(1, 2)
        t(1, 3) = onorm * t(1, 3)

        t(2, 1) = t(3, 2) * t(1, 3) - t(3, 3) * t(1, 2)
        t(2, 2) = t(3, 3) * t(1, 1) - t(3, 1) * t(1, 3)
        t(2, 3) = t(3, 1) * t(1, 2) - t(3, 2) * t(1, 1)

        fl1 = fx * t(1, 1) + fy * t(1, 2) + fz * t(1, 3)
        fl2 = fx * t(2, 1) + fy * t(2, 2) + fz * t(2, 3)

        s(1, 1) = stft
        s(2, 2) = stft
        s(3, 3) = stfn
        s(1, 3) = 0.0
        s(2, 3) = 0.0
        s(3, 1) = 0.0
        s(3, 2) = 0.0

        s(1, 2) = 0.0
        s(2, 1) = 0.0

        For j = 1 To 3
            For i = 1 To 3
                a(i, j) = s(i, 1) * t(1, j) + s(i, 2) * t(2, j) + s(i, 3) * t(3, j)
            Next
        Next

        e(1) = t(1, 1) * a(1, 1) + t(2, 1) * a(2, 1) + t(3, 1) * a(3, 1)
        e(2) = t(1, 1) * a(1, 2) + t(2, 1) * a(2, 2) + t(3, 1) * a(3, 2)
        e(3) = t(1, 2) * a(1, 2) + t(2, 2) * a(2, 2) + t(3, 2) * a(3, 2)
        e(4) = t(1, 1) * a(1, 3) + t(2, 1) * a(2, 3) + t(3, 1) * a(3, 3)
        e(5) = t(1, 2) * a(1, 3) + t(2, 2) * a(2, 3) + t(3, 2) * a(3, 3)
        e(6) = t(1, 3) * a(1, 3) + t(2, 3) * a(2, 3) + t(3, 3) * a(3, 3)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine fstiff(n1,n2,n3,stfn,fx,fy,fz,fni,fmax,fmag,e,stft)
'c
'
'      implicit double precision (a-h,o-z)                              
'c
'c===> module to compute penalty stiffness w.r.t. master facet
'c     applicable when frictional effects are included
'c
'      double precision n1,n2,n3
'      dimension t(3,3),r(3),s(3,3),a(3,3),e(*)
'c
'      t(3,1)=n1
'      t(3,2)=n2
'      t(3,3)=n3
'c
'      ip=1
'      if (abs(t(3,2)).lt.abs(t(3,1))) ip=2
'      if (abs(t(3,3)).lt.abs(t(3,ip))) ip=3
'c
'      r(1)=0.
'      r(2)=0.
'      r(3)=0.
'      r(ip)=1.
'c
'      t(1,1)=r(2)*t(3,3)-r(3)*t(3,2)
'      t(1,2)=r(3)*t(3,1)-r(1)*t(3,3)
'      t(1,3)=r(1)*t(3,2)-r(2)*t(3,1)
'c
'      onorm=1./sqrt(t(1,1)**2+t(1,2)**2+t(1,3)**2)
'      t(1,1)=onorm*t(1,1)
'      t(1,2)=onorm*t(1,2)
'      t(1,3)=onorm*t(1,3)
'c
'      t(2,1)=t(3,2)*t(1,3)-t(3,3)*t(1,2)
'      t(2,2)=t(3,3)*t(1,1)-t(3,1)*t(1,3)
'      t(2,3)=t(3,1)*t(1,2)-t(3,2)*t(1,1)
'c
'      fl1=fx*t(1,1)+fy*t(1,2)+fz*t(1,3)
'      fl2=fx*t(2,1)+fy*t(2,2)+fz*t(2,3)
'c
'      s(1,1)=stft
'      s(2,2)=stft
'      s(3,3)=stfn
'      s(1,3)=0.
'      s(2,3)=0.
'      s(3,1)=0.
'      s(3,2)=0.
'c
'c
'   20 s(1,2)=0.
'      s(2,1)=0.
'c
'   30 do 50 j=1,3
'      do 40 i=1,3
'      a(i,j)=s(i,1)*t(1,j)+s(i,2)*t(2,j)+s(i,3)*t(3,j)
'   40 continue
'   50 continue
'c
'      e(1)=t(1,1)*a(1,1)+t(2,1)*a(2,1)+t(3,1)*a(3,1)
'      e(2)=t(1,1)*a(1,2)+t(2,1)*a(2,2)+t(3,1)*a(3,2)
'      e(3)=t(1,2)*a(1,2)+t(2,2)*a(2,2)+t(3,2)*a(3,2)
'      e(4)=t(1,1)*a(1,3)+t(2,1)*a(2,3)+t(3,1)*a(3,3)
'      e(5)=t(1,2)*a(1,3)+t(2,2)*a(2,3)+t(3,2)*a(3,3)
'      e(6)=t(1,3)*a(1,3)+t(2,3)*a(2,3)+t(3,3)*a(3,3)
'c
'      return
'      end

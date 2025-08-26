'This file contains all the methods of slvage.f
Partial Public Class clsInput
    ''' <summary>
    ''' module to
    ''' </summary>
    ''' <param name="ier"></param>
    ''' <param name="s"></param>
    ''' <param name="t"></param>
    Public Sub slvage(ByRef ier As Integer, ByRef s As Decimal, ByRef t As Decimal)

        Dim ps1, ps2, ps3, pt1, pt2, pt3, sp, sm, tp, tm, xxx, yyy, zzz, xs, ys, zs, xx1(20), xx2(20), xx3(20), h(20) As Double     ' QW 8-2018
        Dim xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p As Double ' QW 8-2018
        Dim ix(10), iy(10) As Integer ' QW 8-2018

        Dim dist1, dist2, xavg, yavg, zavg, dists, distd, anum As Double

        dist1 = Math.Pow((xx1(1) - xx1(3)), 2) + Math.Pow((xx2(1) - xx2(3)), 2) + Math.Pow((xx3(1) - xx3(3)), 2)
        dist2 = Math.Pow((xx1(2) - xx1(4)), 2) + Math.Pow((xx2(2) - xx2(4)), 2) + Math.Pow((xx3(2) - xx3(4)), 2)
        xavg = 0.25 * (xx1(1) + xx1(2) + xx1(3) + xx1(4))
        yavg = 0.25 * (xx2(1) + xx2(2) + xx2(3) + xx2(4))
        zavg = 0.25 * (xx3(1) + xx3(2) + xx3(3) + xx3(4))
        dists = Math.Sqrt(Math.Pow((xs - xavg), 2) + Math.Pow((ys - yavg), 2) + Math.Pow((zs - zavg), 2))
        distd = Math.Sqrt(Math.Max(dist1, dist2))
        If dists < 0.7 * distd Then GoTo 10
        ier = 1
        Return
10:     If Math.Abs(t) > 1.0 Then t = t / Math.Abs(t)
        If ix(2) <> ix(3) Then GoTo 20
        If t > 0.99 Then s = 0
        If Math.Abs(t - 1.0) < 0.0000000001 Then t = 0.99
20:     tp = 0.25 * (1.0 + t)
        tm = 0.25 * (1.0 - t)
        xxx = 0.0
        yyy = 0.0
        zzz = 0.0
        ps1 = tp * xx134 - tm * xx112
        ps2 = tp * xx234 - tm * xx212
        ps3 = tp * xx334 - tm * xx312
        anum = ps1 * (xs - tm * xx112p - tp * xx134p) + ps2 * (ys - tm * xx212p - tp * xx234p) + ps3 * (zs - tm * xx312p - tp * xx334p)
        s = anum / (ps1 * ps1 + ps2 * ps2 + ps3 * ps3)
        If Math.Abs(s) > 1 Then s = s / Math.Abs(s)
        sp = 1.0 + s
        sm = 1.0 - s
        h(1) = tm * sm
        h(2) = tm * sp
        h(3) = tp * sp
        h(4) = tp * sm
        For i = 0 To 3
            xxx = xxx + h(i) * xx1(i)
            yyy = yyy + h(i) * xx2(i)
            zzz = zzz + h(i) * xx3(i)
        Next
        pt1 = -sm * xx114 - sp * xx123
        pt2 = -sm * xx214 - sp * xx223
        pt3 = -sm * xx314 - sp * xx323

    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine slvage (ier,s,t)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to
'c
'      common/bk43/det,h(20),
'     1           p11,p12,p13,p14,p1(16),p21,p22,p23,p24,p2(16),p3(38)
'      common/bk44/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
'      common/bk45/xs,ys,zs,sig(3),epx,mx,ix(10),iy(10)
'      common/bk46/xxx,yyy,zzz,ps1,ps2,ps3,pt1,pt2,pt3,sp,sm,tp,tm
'      common/bk47/xx112,xx134,xx212,xx234,xx312,xx334,xx112p,xx134p,
'     1      xx212p,xx234p,xx312p,xx334p,xx114,xx123,xx214,
'     2      xx223,xx314,xx323,x1423,y1423,z1423,x1423p,y1423p,z1423p
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'slvage'
'      ik01 = ik01 + 1
'      end if
'
'      dist1=(xx1(1)-xx1(3))**2+(xx2(1)-xx2(3))**2+(xx3(1)-xx3(3))**2
'      dist2=(xx1(2)-xx1(4))**2+(xx2(2)-xx2(4))**2+(xx3(2)-xx3(4))**2
'      xavg=.25*(xx1(1)+xx1(2)+xx1(3)+xx1(4))
'      yavg=.25*(xx2(1)+xx2(2)+xx2(3)+xx2(4))
'      zavg=.25*(xx3(1)+xx3(2)+xx3(3)+xx3(4))
'      dists=sqrt((xs-xavg)**2+(ys-yavg)**2+(zs-zavg)**2)
'      distd=sqrt(max(dist1,dist2))
'      if(dists.lt.0.70*distd) go to 10
'      ier=1
'      return
' 10   if(abs(t).gt.1.0) t=t/abs(t)
'      if(ix(3).ne.ix(4)) go to 20
'      if(t.ge.0.990) s=0.
'      if(abs(t-1.0).lt.1.e-10) t=.990
' 20   tp=.25*(1.0+t)
'      tm=.25*(1.0-t)
'      xxx=0.0
'      yyy=0.0
'      zzz=0.0
'      ps1=tp*xx134-tm*xx112
'      ps2=tp*xx234-tm*xx212
'      ps3=tp*xx334-tm*xx312
'      anum=ps1*(xs-tm*xx112p-tp*xx134p)+
'     1     ps2*(ys-tm*xx212p-tp*xx234p)+
'     2     ps3*(zs-tm*xx312p-tp*xx334p)
'      s =anum/(ps1*ps1+ps2*ps2+ps3*ps3)
'      if(abs(s).gt.1.0) s=s/abs(s)
'      sp=1.0+s
'      sm=1.0-s
'      h(1)=tm*sm
'      h(2)=tm*sp
'      h(3)=tp*sp
'      h(4)=tp*sm
'      do 30 i=1,4
'      xxx=xxx+h(i)*xx1(i)
'      yyy=yyy+h(i)*xx2(i)
' 30   zzz=zzz+h(i)*xx3(i)
'      pt1=-sm*xx114-sp*xx123
'      pt2=-sm*xx214-sp*xx223
'      pt3=-sm*xx314-sp*xx323
'      return
'      end
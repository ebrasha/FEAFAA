'This file contains all the methods of st.f
Partial Public Class clsInput

    ''' <summary>
    ''' Calculate curvilinear coordinates s,t on master facet
    ''' </summary>
    ''' <param name="n1"></param>
    ''' <param name="n2"></param>
    ''' <param name="n3"></param>
    ''' <param name="s1"></param>
    ''' <param name="t1"></param>
    ''' <param name="ier"></param>

    Public Sub st(ByRef n1 As Double, ByRef n2 As Double, ByRef n3 As Double, ByRef s1 As Double,
              ByRef t1 As Double, ByRef ier As Integer, ByVal nty As Integer, ByRef xx1() As Double, ByRef xx2() As Double, ByRef xx3() As Double,
              ByRef ix() As Integer, ByRef xs As Double, ByRef ys As Double, ByRef zs As Double, ByRef hh() As Double,
              ByRef xx112 As Double, ByRef xx134 As Double, ByRef xx212 As Double, ByRef xx234 As Double, ByRef xx312 As Double, ByRef xx334 As Double,
              ByRef xx112p As Double, ByRef xx134p As Double, ByRef xx212p As Double, ByRef xx234p As Double, ByRef xx312p As Double, ByRef xx334p As Double,
              ByRef xx114 As Double, ByRef xx123 As Double, ByRef xx214 As Double, ByRef xx223 As Double, ByRef xx314 As Double, xx323 As Double, ByRef x1423 As Double,
              ByRef y1423 As Double, ByRef z1423 As Double, ByRef x1423p As Double, ByRef y1423p As Double, ByRef z1423p As Double, ByRef xxx As Double, ByRef yyy As Double, ByRef zzz As Double)

        'Dim magn As Long
        Dim ft1, ft2, ft3, fs1, fs2, fs3, magn As Double          ' QW 8-2018
        Dim half = 0.5
        Dim one = 1.0
        Dim tol, cb, arg, brg, t2, scl, t1new, t1est, t1old, fprime, sct1, sct2, sct3 As Double
        Dim iter As Integer
        Dim sp, sm, tp, tm As Double

        xx112 = xx1(1) - xx1(2)
        xx134 = xx1(3) - xx1(4)
        xx212 = xx2(1) - xx2(2)
        xx234 = xx2(3) - xx2(4)
        xx312 = xx3(1) - xx3(2)
        xx334 = xx3(3) - xx3(4)
        xx114 = xx1(1) - xx1(4)
        xx123 = xx1(2) - xx1(3) '
        xx214 = xx2(1) - xx2(4)
        xx223 = xx2(2) - xx2(3)
        xx314 = xx3(1) - xx3(4)
        xx323 = xx3(2) - xx3(3)
        xx112p = xx1(1) + xx1(2)
        xx134p = xx1(3) + xx1(4)
        xx212p = xx2(1) + xx2(2)
        xx234p = xx2(3) + xx2(4)
        xx312p = xx3(1) + xx3(2)
        xx334p = xx3(3) + xx3(4)
        x1423 = -xx114 - xx123
        y1423 = -xx214 - xx223
        z1423 = -xx314 - xx323
        x1423p = xx114 - xx123
        y1423p = xx214 - xx223
        z1423p = xx314 - xx323

        'check for triangular segments

        If ix(3) = ix(4) Then GoTo 10
        Dim anum = -xx112 * (2 * xs - xx112p) - xx212 * (2 * ys - xx212p) - xx312 * (2 * zs - xx312p)
        s1 = anum / (xx112 * xx112 + xx212 * xx212 + xx312 * xx312)
        Dim h1 = 0.5 - 0.5 * s1
        Dim h2 = 0.5 + 0.5 * s1
        xxx = h1 * xx1(1) + h2 * xx1(2)
        yyy = h1 * xx2(1) + h2 * xx2(2)
        zzz = h1 * xx3(1) + h2 * xx3(2)
        ft1 = x1423 + s1 * x1423p
        ft2 = y1423 + s1 * y1423p
        ft3 = z1423 + s1 * z1423p
        Dim ftn1 = 0.25 * (ft1 * (xs - xxx) + ft2 * (ys - yyy) + ft3 * (zs - zzz))
        anum = xx134 * (2 * xs - xx134p) + xx234 * (2 * ys - xx234p) + xx334 * (2 * zs - xx334p)
        s1 = anum / (xx134 * xx134 + xx234 * xx234 + xx334 * xx334)
        Dim h3 = 0.5 + 0.5 * s1
        Dim h4 = 0.5 - 0.5 * s1
        xxx = h3 * xx1(3) + h4 * xx1(4)
        yyy = h3 * xx2(3) + h4 * xx2(4)
        zzz = h3 * xx3(3) + h4 * xx3(4)
        ft1 = x1423 + s1 * x1423p
        ft2 = y1423 + s1 * y1423p
        ft3 = z1423 + s1 * z1423p
        Dim ftn3 = 0.25 * (ft1 * (xs - xxx) + ft2 * (ys - yyy) + ft3 * (zs - zzz))
        fs1 = xx134 - xx112
        fs2 = xx234 - xx212
        fs3 = xx334 - xx312
        anum = fs1 * (4 * xs - xx112p - xx134p) + fs2 * (4 * ys - xx212p - xx234p) + fs3 * (4 * zs - xx312p - xx334p)
        s1 = anum / (fs1 * fs1 + fs2 * fs2 + fs3 * fs3)
        sp = 0.25 * (1.0 + s1)
        sm = 0.25 * (1.0 - s1)
        xxx = sm * (xx1(1) + xx1(4)) + sp * (xx1(2) + xx1(3))
        yyy = sm * (xx2(1) + xx2(4)) + sp * (xx2(2) + xx2(3))
        zzz = sm * (xx3(1) + xx3(4)) + sp * (xx3(2) + xx3(3))
        ft1 = x1423 + s1 * x1423p
        ft2 = y1423 + s1 * y1423p
        ft3 = z1423 + s1 * z1423p
        Dim ftn2 = ft1 * (xs - xxx) + ft2 * (ys - yyy) + ft3 * (zs - zzz)
        Dim ca = ftn1 - 0.5 * ftn2 + ftn3
        t1 = -1 - 2 * ftn1 / (ftn3 - ftn1)
        GoTo 20
10:     tol = 1.15
        ftn1 = funct(s1, -tol, xxx, yyy, zzz, fs1, fs2, fs3, ft1, ft2, ft3, sp, sm, tp, tm, xs, ys, zs, hh, xx1, xx2, xx3, xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)
        ftn2 = funct(s1, tol, xxx, yyy, zzz, fs1, fs2, fs3, ft1, ft2, ft3, sp, sm, tp, tm, xs, ys, zs, hh, xx1, xx2, xx3, xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)
        t1 = -1.15 - 2.3 * ftn1 / (ftn2 - ftn1)
        If Math.Abs(t1 - 1.0) < 0.0000000001 Then t1 = 0.99 '1E-10 = 0.0000000001 or we can put CDbl(1E-10)
        GoTo 30
20:     cb = ftn1 - ftn3
        arg = cb * cb - 2 * ca * ftn2
        brg = Math.Min(arg, Math.Abs(ca) - 0.0001 * one)
        t2 = (cb - Math.Sqrt(Math.Abs(arg))) / (2 * ca + 0.00000001)     'vax
        scl = 0.5 + half * Math.Sign(brg)
        t1new = scl * t2 + (1 - scl) * t1
        t1est = t1new
        iter = 1
24:     ftn1 = funct(s1, t1new, xxx, yyy, zzz, fs1, fs2, fs3, ft1, ft2, ft3, sp, sm, tp, tm, xs, ys, zs, hh, xx1, xx2, xx3, xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)
        ftn2 = funct(s1, t1new + 0.00001, xxx, yyy, zzz, fs1, fs2, fs3, ft1, ft2, ft3, sp, sm, tp, tm, xs, ys, zs, hh, xx1, xx2, xx3, xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)

        '   ... check for ftn1=ftn2 ...
        If Math.Abs(ftn1 - ftn2) < 0.00001 * Math.Abs(ftn1 + ftn2) Then
            t1 = t1est
            GoTo 40
        End If

        fprime = 0.0001 + 100000 * (ftn2 - ftn1)                                 'vax
        t1old = t1new
        t1new = t1old - 0.5 * (ftn1 + ftn2) / fprime
        If Math.Abs(t1new - t1old) < 0.0001 Then GoTo 28
        iter = iter + 1
        If iter < 8 Then GoTo 24 'vax
        t1 = t1est
        GoTo 30
28:     t1 = t1new
30:     ftn1 = funct(s1, t1, xxx, yyy, zzz, fs1, fs2, fs3, ft1, ft2, ft3, sp, sm, tp, tm, xs, ys, zs, hh, xx1, xx2, xx3, xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)
        If Math.Abs(s1) > 1.02 Then GoTo 40
        If Math.Abs(t1) > 1.02 Then GoTo 40
        GoTo 50
40:     If nty = 2 Then
            ier = 1
            Return
        End If
        slvage(ier, s1, t1)
50:     sct1 = fs2 * ft3 - fs3 * ft2
        sct2 = fs3 * ft1 - fs1 * ft3
        sct3 = fs1 * ft2 - fs2 * ft1
        magn = Math.Sqrt(sct1 * sct1 + sct2 * sct2 + sct3 * sct3)
        n1 = sct1 / magn
        n2 = sct2 / magn
        n3 = sct3 / magn

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine st(n1,n2,n3,s1,t1,ier)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to calculate curvilinear coordinates s,t on master facet
'c
'      
'      common/bk37/numsv,nrtm,nrts,nmn,nsn,nty,lt,mst,noco,ndum
'      common/bk43/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
'      common/bk44/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
'      common/bk45/xs,ys,zs,sig(3),epx,mx,ix(10),iy(10)
'      common/bk46/xxx,yyy,zzz,fs1,fs2,fs3,ft1,ft2,ft3,sjunk(4)
'      common/bk47/xx112,xx134,xx212,xx234,xx312,xx334,xx112p,xx134p,
'     1      xx212p,xx234p,xx312p,xx334p,xx114,xx123,xx214,
'     2      xx223,xx314,xx323,x1423,y1423,z1423,x1423p,y1423p,z1423p
'
'      double precision n1,n2,n3,magn
'
'
'
'
'c
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'st'
'      ik01 = ik01 + 1
'      end if
'
'      half = 0.5
'      one = 1.0
'c
'      xx112=xx1(1)-xx1(2)
'      xx134=xx1(3)-xx1(4)
'      xx212=xx2(1)-xx2(2)
'      xx234=xx2(3)-xx2(4)
'      xx312=xx3(1)-xx3(2)
'      xx334=xx3(3)-xx3(4)
'      xx114=xx1(1)-xx1(4)
'      xx123=xx1(2)-xx1(3)
'      xx214=xx2(1)-xx2(4)
'      xx223=xx2(2)-xx2(3)
'      xx314=xx3(1)-xx3(4)
'      xx323=xx3(2)-xx3(3)
'      xx112p=xx1(1)+xx1(2)
'      xx134p=xx1(3)+xx1(4)
'      xx212p=xx2(1)+xx2(2)
'      xx234p=xx2(3)+xx2(4)
'      xx312p=xx3(1)+xx3(2)
'      xx334p=xx3(3)+xx3(4)
'      x1423 =-xx114-xx123
'      y1423 =-xx214-xx223
'      z1423 =-xx314-xx323
'      x1423p= xx114-xx123
'      y1423p= xx214-xx223
'      z1423p= xx314-xx323
'c
'c.... check for triangular segments
'c
'      if (ix(3).eq.ix(4)) go to 10
'c
'      anum=-xx112*(2.*xs-xx112p)
'     1     -xx212*(2.*ys-xx212p)
'     2     -xx312*(2.*zs-xx312p)
'      s1=anum/(xx112*xx112+xx212*xx212+xx312*xx312)
'      h1=0.5-.5*s1
'      h2=0.5+.5*s1
'      xxx=h1*xx1(1)+h2*xx1(2)
'      yyy=h1*xx2(1)+h2*xx2(2)
'      zzz=h1*xx3(1)+h2*xx3(2)
'      ft1=x1423+s1*x1423p
'      ft2=y1423+s1*y1423p
'      ft3=z1423+s1*z1423p
'      ftn1=.25*(ft1*(xs-xxx)+ft2*(ys-yyy)+ft3*(zs-zzz))
'      anum=xx134*(2.*xs-xx134p)+
'     1     xx234*(2.*ys-xx234p)+
'     2     xx334*(2.*zs-xx334p)
'      s1=anum/(xx134*xx134+xx234*xx234+xx334*xx334)
'      h3=0.5+.5*s1
'      h4=0.5-.5*s1
'      xxx=h3*xx1(3)+h4*xx1(4)
'      yyy=h3*xx2(3)+h4*xx2(4)
'      zzz=h3*xx3(3)+h4*xx3(4)
'      ft1=x1423+s1*x1423p
'      ft2=y1423+s1*y1423p
'      ft3=z1423+s1*z1423p
'      ftn3=.25*(ft1*(xs-xxx)+ft2*(ys-yyy)+ft3*(zs-zzz))
'      fs1=xx134-xx112
'      fs2=xx234-xx212
'      fs3=xx334-xx312
'      anum=fs1*(4.*xs-xx112p-xx134p)+
'     1     fs2*(4.*ys-xx212p-xx234p)+
'     2     fs3*(4.*zs-xx312p-xx334p)
'      s1=anum/(fs1*fs1+fs2*fs2+fs3*fs3)
'      sp=.25*(1.0+s1)
'      sm=.25*(1.0-s1)
'      xxx=sm*(xx1(1)+xx1(4))+sp*(xx1(2)+xx1(3))
'      yyy=sm*(xx2(1)+xx2(4))+sp*(xx2(2)+xx2(3))
'      zzz=sm*(xx3(1)+xx3(4))+sp*(xx3(2)+xx3(3))
'      ft1=x1423+s1*x1423p
'      ft2=y1423+s1*y1423p
'      ft3=z1423+s1*z1423p
'      ftn2=ft1*(xs-xxx)+ft2*(ys-yyy)+ft3*(zs-zzz)
'      ca=ftn1-.5*ftn2+ftn3
'      t1=-1.-2.*ftn1/(ftn3-ftn1)
'      go to 20
'   10 tol = 1.15
'      ftn1=funct(s1,-tol)
'      ftn2=funct(s1, tol)
'      t1=-1.15-2.30*ftn1/(ftn2-ftn1)
'      if(abs(t1-1.0).lt.1.e-10) t1=.990
'      go to 30
'   20 cb=ftn1-ftn3
'      arg=cb*cb-2.*ca*ftn2
'      brg=min(arg,abs(ca)-.0001*one)
'      t2=(cb-sqrt(abs(arg)))/(2.*ca+1.e-08)                             vax
'      scl=.5e0+sign(half,brg)
'      t1new=scl*t2+(1.-scl)*t1
'      t1est=t1new
'      iter=1
'   24 ftn1=funct(s1,t1new)
'      ftn2=funct(s1,t1new+.00001)
'c
'c   ... check for ftn1=ftn2 ...
'      if (abs(ftn1-ftn2) .lt. 1.e-5*abs(ftn1+ftn2)) then
'c       write(10,973)
'c 973   format(' *',$)
'        t1=t1est
'        goto 40
'      endif
'c
'      fprime=1.e-04+100000.*(ftn2-ftn1)                                 vax
'      t1old=t1new
'      t1new=t1old-.50*(ftn1+ftn2)/fprime
'      if(abs(t1new-t1old).lt.0.0001) go to 28
'      iter=iter+1
'      if(iter.lt. 8) go to 24                                           vax
'      t1= t1est
'      go to 30
'   28 t1=t1new
'   30 ftn1=funct(s1,t1)
'      if (abs(s1).gt.1.020) go to 40
'      if (abs(t1).gt.1.020) go to 40
'      go to 50
'   40 if(nty.eq.2) then
'        ier=1
'        return
'      endif
'      call slvage  (ier,s1,t1)
'   50 sct1=fs2*ft3-fs3*ft2
'      sct2=fs3*ft1-fs1*ft3
'      sct3=fs1*ft2-fs2*ft1
'      magn=sqrt(sct1*sct1+sct2*sct2+sct3*sct3)
'      n1=sct1/magn
'      n2=sct2/magn
'      n3=sct3/magn
'      return
'      end
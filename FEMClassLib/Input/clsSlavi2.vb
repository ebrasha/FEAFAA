'This file contains all the methods of slavi2.f
Partial Public Class clsInput

    ''' <summary>
    ''' determine if slidesurface penalty element is active (initialization phase)
    ''' </summary>
    ''' <param name="x"></param>
    ''' <param name="irect"></param>
    ''' <param name="lmsr"></param>
    ''' <param name="msr"></param>
    ''' <param name="nsv"></param>
    ''' <param name="iloc"></param>
    ''' <param name="irtl"></param>
    ''' <param name="stf"></param>
    ''' <param name="nsn"></param>
    ''' <param name="nmn"></param>
    ''' <param name="nty"></param>
    ''' <param name="cst"></param>
    ''' <param name="nnfnum"></param>
    ''' <param name="pend"></param>
    ''' <param name="istyp"></param>
    ''' <param name="ioff"></param>
    Public Sub slavi2(ByRef x(,) As Double, ByRef irect(,) As Integer, ByRef lmsr() As Integer,
                      ByRef msr() As Integer, ByRef nsv() As Integer, ByRef iloc() As Integer,
                      ByRef irtl() As Integer, ByRef stf() As Double, ByRef nsn As Integer,
                      ByRef nmn As Integer, ByRef nty As Integer, ByRef cst(,) As Double, ByRef nnfnum As Integer,
                      ByRef pend As Double, ByRef istyp As Integer, ByRef ioff As Boolean, ByRef xx1() As Double, ByRef xx2() As Double, ByRef xx3() As Double,
                      ByRef ixx() As Integer, ByRef xs As Double, ByRef ys As Double, ByRef zs As Double, ByRef amx As Double, ByRef amy As Double, ByRef amz As Double, vn() As Double, ByRef hh() As Double,
                      ByRef xx112 As Double, ByRef xx134 As Double, ByRef xx212 As Double, ByRef xx234 As Double, ByRef xx312 As Double, ByRef xx334 As Double, ByRef xx112p As Double, ByRef xx134p As Double, ByRef xx212p As Double, ByRef xx234p As Double, ByRef xx312p As Double, ByRef xx334p As Double, ByRef xx114 As Double, ByRef xx123 As Double, ByRef xx214 As Double, ByRef xx223 As Double, ByRef xx314 As Double, xx323 As Double, ByRef x1423 As Double, ByRef y1423 As Double, ByRef z1423 As Double, ByRef x1423p As Double, ByRef y1423p As Double, ByRef z1423p As Double)

        Dim k1, k2, k3 As Integer     ' QW 8-2018
        Dim n1, n2, n3 As Double          ' QW 8-2018
        n2 = 0 : n3 = 0         ' QW 8-2018
        Dim ioyet = False
        Dim ansmx = 0.0, detv = 0.0, dx = 0.0
        Dim nmx = nsv(1)
        Dim i, j, k, l, nn, ierr As Integer
        'Dim n2 = 0, n3 = 0, ss = 0.0, tt = 0.0, ans = 0.0, gfac = 0.0, ans1 = 0.0, ans11 = 0.0, ans12 = 0.0, ans13 = 0.0
        Dim ss = 0.0, tt = 0.0, ans = 0.0, gfac = 0.0, ans1 = 0.0, ans11 = 0.0, ans12 = 0.0, ans13 = 0.0        ' QW 8-2018
        Dim format301 = "" +
                        Space(5) + "SLIDE SURFACE INITALIZATION: slave side, interface #{0}" + Environment.NewLine +
                        Space(5) + "  node       coordinate change during relocation" + Environment.NewLine +
                        Space(5) + " number        dX             dY             dZ" + Environment.NewLine
        Dim format302 = "" +
                        Space(5) + "SLIDE SURFACE INITALIZATION: master side, interface #{0}" + Environment.NewLine +
                        Space(5) + "  node       coordinate change during relocation" + Environment.NewLine +
                        Space(5) + " number        dX             dY             dZ" + Environment.NewLine
        Dim format310 = Space(5) + "{0}" + Space(2) + Space(3) + "{1}" + Space(3) + "{2}" + Space(3) + "{3}"


        For ii = 1 To nsn
            i = nsv(ii)
            j = iloc(ii)
            k = msr(j)
            l = irtl(ii)
            '      do 10 jj=1,4
            For jj = 1 To 4
                nn = irect(jj, l)
                ixx(jj) = nn
                xx1(jj) = x(1, nn)
                xx2(jj) = x(2, nn)
                xx3(jj) = x(3, nn)
            Next
            xs = x(1, i)
            ys = x(2, i)
            zs = x(3, i)

            'c     check for penetration    ' QW 12-12-2018-
            If k <> ixx(1) Then GoTo 20
            k1 = 1
            k2 = 2
            k3 = 4
            GoTo 50
20:         If k <> ixx(2) Then GoTo 30
            k1 = 2
            k2 = 3
            k3 = 1
            GoTo 50
30:         If k <> ixx(3) Then GoTo 40
            k1 = 3
            k2 = 4
            k3 = 2
            If ixx(3) = ixx(4) Then k2 = 1
            GoTo 50
40:         If k <> ixx(4) Then GoTo 50
            k1 = 4
            k2 = 1
            k3 = 3

50:         If nty < 3 Then GoTo 60

            Call ptimei(k1, k2, k3, detv, xx1, xx2, xx3, xs, ys, zs)  ' this detv has thickness
            Call chardx(dx, xx1, xx2, xx3)
            If detv > 0 Then
                detv = Math.Sqrt(Math.Pow((xs - x(1, k)), 2) +
                                 Math.Pow((ys - x(2, k)), 2) +
                                 Math.Pow((zs - x(3, k)), 2))
                GoTo 90
            End If

            'c     compute contact point and unit normal to master segment
60:         ierr = 0
            Call st(n1, n2, n3, ss, tt, ierr, nty, xx1, xx2, xx3, ixx, xs, ys, zs, hh,
                    xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p,
                    xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423,
                    x1423p, y1423p, z1423p, amx, amy, amz)

            If nty = 1 Then
                If ierr = 1 Then GoTo 100
            ElseIf nty = 2 Then
                If ierr = 1 Then
                    iloc(ii) = -1
                    Dim msg200 = "" +
                    " *******************************************************" + Environment.NewLine +
                    " *                   - WARNING -                       *" + Environment.NewLine +
                    " * Slave node " + i.ToString("#####") + " could not be placed on master    *" + Environment.NewLine +
                    " *  surface of TIED slideline " + nnfnum.ToString("#####") + ", and is now free    *" + Environment.NewLine +
                    " *******************************************************" + Environment.NewLine

                    '          write(lutty,200) msg200

                    ' If (bNIKE3D) Then
                    ' Console.WriteLine(msg200)
                    ' End If

                    '          write(luo,200) msg200
                    GoTo 90
                End If
                cst(0, ii) = ss
                cst(1, ii) = tt
            ElseIf (ierr = 1) Then
                detv = Math.Sqrt(Math.Pow((xs - x(1, k)), 2) + Math.Pow((ys - x(2, k)), 2) + Math.Pow((zs - x(3, k)), 2))
                GoTo 90
            End If

            'c     check for penetration
            ans = n1 * (xs - amx) + n2 * (ys - amy) + n3 * (zs - amz)
            If Math.Abs(ans) > Math.Abs(pend) AndAlso nty = 2 Then
                iloc(ii) = -iloc(ii)           ' ignore tied node
                GoTo 90
            End If

            '  ... handle tied slied with offsets seperately
            If ioff Then        'using type 8 tied slied with offsets
                cst(2, ii) = ans
                '         write(10,*) 'ik02 call normal in sub slavi2'
                Dim msg = "ik02 call normal in sub slavi2"
                'ans11 = amx + cst(2, ii) * vn(1) - x(0, i)
                'ans12 = amy + cst(2, ii) * vn(2) - x(1, i)
                'ans13 = amz + cst(2, ii) * vn(3) - x(2, i)
                ans11 = amx + cst(3, ii) * vn(1) - x(1, i)      ' QW 8-2018
                ans12 = amy + cst(3, ii) * vn(2) - x(2, i)
                ans13 = amz + cst(3, ii) * vn(3) - x(3, i)
                ans1 = Math.Sqrt(ans11 * ans11 + ans12 * ans12 + ans13 * ans13)

                '    ... move node exactly onto normal with offset (st normal is off a little)
                'x(1, i) = amx + cst(2, ii) * vn(1)
                'x(2, i) = amy + cst(2, ii) * vn(2)
                'x(3, i) = amz + cst(2, ii) * vn(3)
                x(1, i) = amx + cst(3, ii) * vn(1)          ' QW 8-2018
                x(2, i) = amy + cst(3, ii) * vn(2)
                x(3, i) = amz + cst(3, ii) * vn(3)
                If ans1 > ansmx Then
                    ansmx = ans1
                    nmx = i
                End If
                If (Not ioyet) Then

                    'if(istyp = 1) write(luo,301) String.Format(format301, nnfnum.ToString("###"))
                    'if(istyp = 2) write(luo,302) String.Format(format302, nnfnum.ToString("###"))
                    ioyet = True
                End If

                'write(luo,310) String.Format(format310, i.ToString("#####"), ans11.ToString("e5"), ans12.ToString("e5"), ans13.ToString("e5"))
                GoTo 90
            End If

            If ans < pend Then GoTo 90
            If nty < 3 Then
                gfac = -1.0
                GoTo 70
            Else
                gfac = -1.1
            End If

            If ans >= 0 Then GoTo 90
70:         ans1 = gfac * ans
            ans11 = ans1 * n1
            ans12 = ans1 * n2
            ans13 = ans1 * n3
            x(1, i) = x(1, i) + ans11
            x(2, i) = x(2, i) + ans12
            x(3, i) = x(3, i) + ans13
            If ans1 > ansmx Then
                ansmx = ans1
                nmx = i
            End If
            If Not ioyet Then
                '         if istyp = 1 then write(luo,301) String.Format(format301, nnfnum.ToString("###"))
                '         if istyp = 2 then write(luo,302) String.Format(format302, nnfnum.ToString("###"))
                ioyet = True
            End If
            ' write(luo,310) String.Format(format310, i.ToString("#####"), ans11.ToString("e5"), ans12.ToString("e5"), ans13.ToString("e5"))
90:     Next        ' QW 12-12-2018-

        Dim msg320 = Space(6) + "maximum relocation distance = " + ansmx.ToString("e5") + "  at node " + nmx.ToString("#####")
        '      if ioyet then write(luo,320) msg320

        If istyp = 1 Then
            Dim msg321 = Space(5) + nnfnum.ToString("#####") + Space(6) + " slave" + Space(9) + ansmx.ToString("e5") +
                Space(7) + nmx.ToString("######")
            '	 write(lutty,321) msg321

            ' If (bNIKE3D) Then
            ' Console.WriteLine(msg321)
            'End If

        End If
        If istyp = 2 Then
            Dim msg322 = Space(16) + "master" + Space(9) + ansmx.ToString("e5") + Space(7) + nmx.ToString("######")
            '	 write(lutty,322) msg322

            'If bNIKE3D Then
            ' Console.WriteLine(msg322)
            'End If
            '
        End If
        Return

100:
        Dim msg110 = " slave node" + i.ToString("#####") + " cannot be placed on master surface of i 1nterface" + nnfnum.ToString("###")

        Dim msg120 = "x-coordinates     slave=" + xs.ToString("e7") + Environment.NewLine +
            "master node coordinates=" + xx1(0).ToString("e7") + "," + xx1(1).ToString("e7") + "," +
            xx1(2).ToString("e7") + "," + xx1(3).ToString("e7")
        Dim msg130 = "y-coordinates     slave=" + ys.ToString("e7") + Environment.NewLine +
            "master node coordinates=" + xx2(0).ToString("e7") + "," + xx2(1).ToString("e7") + "," +
            xx2(2).ToString("e7") + "," + xx2(3).ToString("e7")
        Dim msg140 = "z-coordinates     slave=" + zs.ToString("e7") + Environment.NewLine +
            "master node coordinates=" + xx3(0).ToString("e7") + "" + xx3(1).ToString("e7") + "" +
            xx3(2).ToString("e7") + "" + xx3(3).ToString("e7")

        '      Write(luo, 110) msg110
        '      write(lutty,110) msg110
        '      write(lutty,120) msg120
        '      write(lutty,130) msg130
        '      write(lutty,140) msg140
        '
        'If bNIKE3D Then
        'Console.WriteLine(msg110)
        'Console.WriteLine(msg120)
        'Console.WriteLine(msg130)
        'Console.WriteLine(msg140)
        'End If

        ' Call adios(2)  ' QW 8-2018

    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'c      subroutine slavi2(x,irect,lmsr,msr,nsv,iloc,irtl,stf,
'c     1 nsn,nmn,nty,cst,nnfnum,ngap,pend,ethiks,ethikm,istyp,
'c     2 ioff)
'      subroutine slavi2(x,irect,lmsr,msr,nsv,iloc,irtl,stf,
'     1 nsn,nmn,nty,cst,nnfnum,pend,istyp,
'     2 ioff)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to determine if slidesurface penalty element is active
'c     (initialization phase)
'c
'	common/block02/bNIKE3D !ikawa 04/14/03
'	logical bNIKE3D
'
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'c     common/bk43/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
'      common/bk43/det,h(4),hs(4),ht(4),vn(3),vs(3),vt(3),vnn,
'     &            gx,gy,gz,gn,vx(3,3,4),fv(3),tsk(3,3),ssk(3,3),
'     &            enxn(3,3),dum(6)
'      common/bk44/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
'      common/bk45/xs,ys,zs,sig(3),epx,mx,ix(10),iy(10)
'      common/bk46/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
'      
'      
'c      logical ldyna3d,lgpavg,ioff
'
'c
'      dimension x(3,*),irect(4,*),lmsr(*),msr(*),nsv(*),iloc(*),
'c     1          irtl(1),stf(1),cst(3,1),ethiks(*),ethikm(*)
'     1          irtl(1),stf(1),cst(3,1)
'c
'
'      double precision n1,n2,n3
'
'
'
'
'      logical ioyet
'c
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'slavi2'
'      ik01 = ik01 + 1
'      end if
'
'      ioyet=.false.
'      ansmx=0.0
'      nmx=nsv(1)
'c
'      do 90 ii=1,nsn
'      i=nsv(ii)
'      j=iloc(ii)
'      k=msr(j)
'      l=irtl(ii)
'      do 10 jj=1,4
'      nn=irect(jj,l)
'      ix(jj)=nn
'      xx1(jj)=x(1,nn)
'      xx2(jj)=x(2,nn)
' 10   xx3(jj)=x(3,nn)
'      xs=x(1,i)
'      ys=x(2,i)
'      zs=x(3,i)
'c
'c     check for penetration
'c
'      if(k.ne.ix(1)) go to 20
'      k1=1
'      k2=2
'      k3=4
'      go to 50
' 20   if(k.ne.ix(2)) go to 30
'      k1=2
'      k2=3
'      k3=1
'      go to 50
' 30   if(k.ne.ix(3)) go to 40
'      k1=3
'      k2=4
'      k3=2
'      if(ix(3).eq.ix(4)) k2=1
'      go to 50
' 40   if(k.ne.ix(4)) go to 50
'      k1=4
'      k2=1
'      k3=3
' 50   if(nty.lt.3) go to 60
'c      call ptimei(k1,k2,k3,ethiks(ii),ethikm(j),detv)
'      call ptimei(k1,k2,k3,detv)  ! this detv has thickness
'      call chardx(dx)
'      if(detv.gt.0.) then
'         detv=sqrt((xs-x(1,k))**2+(ys-x(2,k))**2+(zs-x(3,k))**2)
'c         if (lbwgap .and. (detv.gt.ngap*100.*dx)) iloc(ii)=-iloc(ii)
'         goto 90
'      endif
'c
'c     compute contact point and unit normal to master segment
'c
'   60 ierr=0
'      call st(n1,n2,n3,ss,tt,ierr)
'c
'      if(nty.eq.1) then
'        if(ierr.eq.1) goto 100
'      elseif(nty.eq.2) then
'        if(ierr.eq.1) then
'          iloc(ii)=-1
'          write(lutty,200) i,nnfnum
'
'	if (bNIKE3D) then
'        write(*,200) i,nnfnum
'	end if
'
'          write(luo,200) i,nnfnum
'          goto 90
'        endif
'        cst(1,ii)=ss
'        cst(2,ii)=tt
'      elseif(ierr.eq.1) then
'        detv=sqrt((xs-x(1,k))**2+(ys-x(2,k))**2+(zs-x(3,k))**2)
'c        if (lbwgap .and. (detv.gt.ngap*100.*dx)) iloc(ii)=-iloc(ii)
'        go to 90
'      endif
'c
'c     check for penetration
'c
'c      ans=n1*(xs-amx)+n2*(ys-amy)+n3*(zs-amz)-(ethiks(ii)+ethikm(j))/2
'      ans=n1*(xs-amx)+n2*(ys-amy)+n3*(zs-amz)
'      if(abs(ans).gt.abs(pend).and.nty.eq.2) then
'       iloc(ii)=-iloc(ii)           ! ignore tied node
'       go to 90
'      endif
'
'c  ... handle tied slied with offsets seperately
'
'      if (ioff) then         ! using type 8 tied slied with offsets
'        cst(3,ii)=ans
'!        call normal(xx1(1),xx2(1),xx3(1),ss,tt)
'         write(10,*) 'ik02 call normal in sub slavi2'
'
'        ans11 = amx + cst(3,ii)*vn(1) - x(1,i)
'        ans12 = amy + cst(3,ii)*vn(2) - x(2,i)
'        ans13 = amz + cst(3,ii)*vn(3) - x(3,i)
'        ans1 = sqrt(ans11*ans11 + ans12*ans12 + ans13*ans13)
'c    ... move node exactly onto normal with offset (st normal is off a little)
'        x(1,i) = amx + cst(3,ii)*vn(1)
'        x(2,i) = amy + cst(3,ii)*vn(2)
'        x(3,i) = amz + cst(3,ii)*vn(3)
'        if (ans1.gt.ansmx) then
'          ansmx=ans1
'          nmx=i
'        endif
'        if (.not.ioyet) then
'           if(istyp.eq.1) write(luo,301) nnfnum
'           if(istyp.eq.2) write(luo,302) nnfnum
'           ioyet = .true.
'        endif
'        write(luo,310) i,ans11,ans12,ans13
'        goto 90
'      endif
'
'      if (ans.lt.pend) goto 90
'      if(nty.lt.3) then
'        gfac=-1.0
'        goto 70
'      else
'        gfac=-1.10
'      endif
'c      if(ldyna3d) goto 90     ! don't relocate if dyna3d/nike3d total restart
'      if(ans) 70,90,90
'   70 ans1=gfac*ans
'      ans11=ans1*n1
'      ans12=ans1*n2
'      ans13=ans1*n3
'      x(1,i)=x(1,i)+ans11
'      x(2,i)=x(2,i)+ans12
'      x(3,i)=x(3,i)+ans13
'      if (ans1.gt.ansmx) then
'        ansmx=ans1
'        nmx=i
'      endif
'      if (.not.ioyet) then
'         if(istyp.eq.1) write(luo,301) nnfnum
'         if(istyp.eq.2) write(luo,302) nnfnum
'         ioyet = .true.
'      endif
'      write(luo,310) i,ans11,ans12,ans13
' 90   continue
'c
'      if (ioyet) write(luo,320) ansmx, nmx
'      if(istyp.eq.1) then
'	 write(lutty,321) nnfnum, ansmx, nmx
'
'		if (bNIKE3D) then
'         write(*,321) nnfnum, ansmx, nmx
'		end if
'
'	end if
'      if(istyp.eq.2) then
'	 write(lutty,322) ansmx, nmx
'
'	if (bNIKE3D) then
'        write(*,322) ansmx, nmx
'	end if
'
'	end if
'      return
'c
' 100  write(luo,110) i,nnfnum
'      write(lutty,110) i,nnfnum
'      write(lutty,120) xs,(xx1(l),l=1,4)
'      write(lutty,130) ys,(xx2(l),l=1,4)
'      write(lutty,140) zs,(xx3(l),l=1,4)
'
'	if (bNIKE3D) then
'      write(*,110) i,nnfnum
'      write(*,120) xs,(xx1(l),l=1,4)
'      write(*,130) ys,(xx2(l),l=1,4)
'      write(*,140) zs,(xx3(l),l=1,4)
'	end if
'
'      call adios(2)
' 110  format(/' slave node',i5,' cannot be placed on master surface of i
'     1nterface',i3)
' 120  format('x-coordinates     slave=',e14.7,/
'     1       'master node coordinates=',4e14.7///)
' 130  format('y-coordinates     slave=',e14.7,/
'     1       'master node coordinates=',4e14.7///)
' 140  format('z-coordinates     slave=',e14.7,/
'     1       'master node coordinates=',4e14.7///)
'  200 format(/
'     &' *******************************************************',/
'     &' *                   - WARNING -                       *',/
'     &' *   Slave node ',i5,' could not be placed on master    *',/
'     &' *  surface of TIED slideline ',i4,', and is now free    *',/
'     &' *******************************************************'/)
'  301 format(//
'     &5x,'SLIDE SURFACE INITALIZATION: slave side, interface #',i3/
'     &5x,'  node       coordinate change during relocation' /
'     &5x,' number        dX             dY             dZ')
'  302 format(//
'     &5x,'SLIDE SURFACE INITALIZATION: master side, interface #',i3/
'     &5x,'  node       coordinate change during relocation' /
'     &5x,' number        dX             dY             dZ')
'  310 format(5x,i5,2x,3(3x,1pe12.5))
'  320 format(6x,'maximum relocation distance = ',e12.5,'  at node ',i5)
'  321 format(5x,i5,6x,' slave',9x,1pe12.5,7x,i6)
'  322 format(16x,'master',9x,1pe12.5,7x,i6)
'      end
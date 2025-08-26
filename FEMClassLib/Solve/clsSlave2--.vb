'This file contains all the methods of slave2.f
Partial Public Class clsSolve

    ' YC 092018
    Public istold, ilimit As Integer, lprtbd As Boolean
    Public emodl2 As Double, amx, amy, amz As Double


    Private ix(10) As Integer
    Private xx1(20), xx2(20), xx3(20) As Double
    Private xs, ys, zs As Double
    ' YC 092018 END


    ''' <summary>
    ''' to determine if slidesurface penalty element is active
    ''' </summary>
    ''' <param name="x"></param>
    ''' <param name="rhs"></param>
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
    ''' <param name="idp"></param>
    ''' <param name="lmn"></param>
    ''' <param name="s"></param>
    ''' <param name="irects"></param>
    ''' <param name="lnsv"></param>
    ''' <param name="fric"></param>
    ''' <param name="fdat"></param>
    ''' <param name="iseg"></param>
    ''' <param name="diag"></param>
    ''' <param name="sfdata"></param>
    ''' <param name="kfstor"></param>
    ''' <param name="lfstor"></param>
    ''' <param name="iebuf"></param>
    ''' <param name="xls"></param>
    ''' <param name="xgn"></param>
    ''' <param name="xgt"></param>
    ''' <param name="sfact"></param>
    ''' <param name="pend"></param>
    ''' <param name="laugon"></param>
    ''' <param name="numnp"></param>
    ''' <param name="fval"></param>
    ''' <param name="adeath"></param>
    ''' <param name="ifd"></param>
    ''' <param name="r_old"></param>
    ''' <param name="ss_old"></param>
    ''' <param name="l_old"></param>
    'Public Sub slave2(ByRef x(,) As Double, ByRef rhs() As Double, ByRef irect(,) As Integer,
    '                  ByRef lmsr() As Integer, ByRef msr() As Integer, ByRef nsv() As Integer,
    '                  ByRef iloc() As Integer, ByRef irtl() As Integer, ByRef stf() As Double,
    '                  ByRef nsn As Integer, ByRef nmn As Integer, ByRef nty As Integer, ByRef idp(,) As Integer,
    '                  ByRef lmn() As Integer, ByRef s() As Double, ByRef irects(,) As Integer, ByRef lnsv() As Integer,
    '                  ByRef fric() As Double, ByRef fdat(,) As Double, ByRef iseg(,) As Integer,
    '                  ByRef diag() As Double, ByRef sfdata() As Double, ByRef kfstor As Integer,
    '                  ByRef lfstor As Integer, ByRef iebuf As Integer, ByRef xls(,) As Double,
    '                  ByRef xgn As Double, ByRef xgt As Double, ByRef sfact As Double,
    '                  ByRef pend As Double, ByRef laugon As Boolean, ByRef numnp As Integer,
    '                  ByRef fval() As Double, ByRef adeath As Double, ByRef ifd As Integer,
    '                  ByRef r_old(,) As Double, ByRef ss_old(,) As Double, ByRef l_old() As Integer)
    Public Sub slave2(ByRef x(,) As Double, ByRef rhs() As Double, ByRef irect(,) As Integer,
                  ByRef lmsr() As Integer, ByRef msr() As Integer, ByRef nsv() As Integer,
                  ByRef iloc() As Integer, ByRef irtl() As Integer, ByRef stf() As Double,
        ByRef nsn As Integer, ByRef nmn As Integer, ByRef nty As Integer, ByRef idp(,) As Integer,
        ByRef lmn() As Integer, ByRef s() As Double, ByRef irects(,) As Integer, ByRef lnsv() As Integer,
        ByRef fric() As Double, ByRef fdat(,) As Double, ByRef iseg(,) As Integer,
        ByRef diag() As Double, ByRef sfdata() As Double, ByRef kfstor As Integer,
        ByRef lfstor As Integer, ByRef iebuf As Integer, ByRef xls(,) As Double, ByRef xgn As Double)
        ' ByRef xgt As Double)
        'ByRef sfact As Double,
        'ByRef pend As Double, ByRef laugon As Boolean, ByRef numnp As Integer,
        'ByRef fval() As Double, ByRef adeath As Double, ByRef ifd As Integer,
        'ByRef r_old(,) As Double, ByRef ss_old(,) As Double, ByRef l_old() As Integer)

        'Dim id(5) As Integer, e(5) As Double, xrb(2, 4) As Double  ' YC 102418
        Dim id(6) As Integer, e(6) As Double, xrb(3, 5) As Double
        Dim i, j, k, l, nn, j1, j2, j3, k1, k2, k3, ierr, idebug, ierrt, jj, lmmax As Integer
        Dim lsh, lnk, lrbr, lrbk, lifd As Boolean
        Dim ans, fmag, ss, tt, fxi, fyi, fzi, fni, detv, dx, pi, paramm, paraml, dx1, area As Double
        Dim f1, f2, f3, sst, ttt, amxt, amyt, amzt, errn, errnt, effstf, asf, stft As Double
        Dim fdx, fdy, fdz, fmax As Double

        Dim n1, n2, n3 As Double, r(24) As Double    'YC 092018

        'c ... determine interface force death options


    End Sub


End Class

'  ref org fortran code 
'cidp# 1 "slave2.F"
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine slave2(x,rhs,irect,lmsr,msr,nsv,iloc,irtl,stf,
'     & nsn,nmn,nty,idp,lmn,s,irects,lnsv,fric,
'     & fdat,iseg,diag,sfdata,kfstor,lfstor,iebuf,
'     & xls,xgn,xgt,sfact,pend,laugon,numnp,fval,adeath,
'     & ifd,r_old,ss_old,l_old)
'c      
'      implicit double precision (a-h,o-z)                              
'c
'c===> module to determine if slidesurface penalty element is active
'c
'      common/double/iprec
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,
'     &            nipmx,nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk12/dtx0,dt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
'      
'
'      common/bk20/kpri,nstep,ite,ilimit,iref
'
'c      
'      common/bk43/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
'      common/bk44/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
'      common/bk45/xs,ys,zs,sig(3),epx,mx,ix(10),iy(10)
'      common/bk46/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
'      common/bk58/nsgcnt
'      common/bk59/r(24)
'      common/fissl1/melemt,nnns,n2g,llls     
'      common/elcnts/numelf
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye3/neehx,ksizhx,neesf,ksizsf
'      common/bkneq/neql
'            
'      logical lfpass,llpass,lelstf
'      common/ndinit/lfpass,llpass
'      common/elstfb/lelstf
'      common/stold/istold
'	common/partbd/emodl2,lprtbd !added drb 12/02
'      dimension x(3,*),rhs(*),irect(4,*),lmsr(*),msr(*),nsv(*),iloc(*),
'     & irtl(*),stf(*),idp(6,*),lmn(*),s(*),id(6),irects(4,*),
'     & lnsv(*),fric(*),iseg(2,*),fdat(10,*),e(6),fval(*),
'     & diag(*),sfdata(*),xls(3,*),xrb(3,5),
'     & r_old(3,nsn),ss_old(2,nsn),l_old(nsn)
'      logical lsh,lnk,laugon,lrbr,lrbk,lbsibr,lifd
'	logical lprtbd !added drb 12/02
'
'      double precision n1,n2,n3
'
'c ... determine interface force death options
'       lifd=.false.
'      if(ifd.ne.0) then
'       if (adeath.ne.1.0) lifd=.true.     ! begin killing off interface force
'       if(lelstf.and.lfpass) lifd=.false. ! need at least an initial force calc
'       if(.not.lifd .and. lelstf) call azero(r_old,3*nsn)
'      endif
'
'      zero = 0.0
'      fcoeff=fric(1)**2+fric(2)**2+fric(3)**2
'      melemt=0
'      xgn=0.0
'      xgt=0.0
'      do 1000 ii=1,nsn            ! loop over slave node indices
'      lrbr=.false.
'      lrbk=.false.
'      i=nsv(ii)                   ! slave node number for slave index ii
'      j=iloc(ii)                  ! master node index for slave index ii
'      k=msr(j)                    ! master node number for index j
'      l=irtl(ii)                  ! master facet for slave index ii
'      if(lifd) l=l_old(ii)        ! use old master facet
'      ans=1.e+6
'      fmag=0.
'      lsh=(fcoeff.eq.0)
'      lnk=.true.
'
'      do 10 jj=1,4      ! gather master and slave coords into local arrays
'      nn=irect(jj,l)
'      ix(jj)=nn
'      xx1(jj)=x(1,nn)
'      xx2(jj)=x(2,nn)
' 10   xx3(jj)=x(3,nn)
'      xs=x(1,i)
'      ys=x(2,i)
'      zs=x(3,i)
'
'      if(lifd) then            ! skip penetration step
'       ss = ss_old(1,ii)
'       tt = ss_old(2,ii)
'       call shapef(h,ss,tt)    ! get shape function values
'       fxi = r_old(1,ii)*adeath ! kill off last force vector
'       fyi = r_old(2,ii)*adeath
'       fzi = r_old(3,ii)*adeath
'       fni = sqrt(fxi*fxi+fyi*fyi+fzi*fzi)
'       if(laugon) then       ! zero out lagrange mults
'        xls(1,ii)=0
'        xls(2,ii)=0
'        xls(3,ii)=0
'       endif
'       goto  200             ! skip to rhs calcs
'      endif
'c
'c.... check for penetration
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
' 50   if(nty.eq.1) go to 60
'      call ptime(k1,k2,k3,detv)
'      call chardx(dx)
'      if(detv/dx.gt.50) go to 102
'c
'c.... compute contact point and unit normal to master segment
'c
' 60   ierr=0
'
'
'      idebug=0
'
'      if(idebug.ne.1) then
'
'      if(istold.ne.1) call st (n1,n2,n3,ss,tt,ierr)
'      if(istold.eq.1) call stn(n1,n2,n3,ss,tt,ierr)
'
'      endif
'
'      if(idebug.eq.1) then
'      if(istold.ne.1) then
'        call stn(n1,n2,n3,ss,tt,ierr)
'        f1=n1
'        f2=n2
'        f3=n3
'        sst=ss
'        ttt=tt
'        amxt=amx
'        amyt=amy
'        amzt=amz
'        ierrt=ierr
'        ierr=0
'        call st (n1,n2,n3,ss,tt,ierr)
'
'      endif
'
'      if(istold.eq.1) then
'        call st (n1,n2,n3,ss,tt,ierr)
'        f1=n1
'        f2=n2
'        f3=n3
'        sst=ss
'        ttt=tt
'        amxt=amx
'        amyt=amy
'        amzt=amz
'        ierrt=ierr
'        ierr=0
'        call stn(n1,n2,n3,ss,tt,ierr)
'
'       endif
'
'        if(ierr.ne.ierrt) then
'          errn =(xs - amx )*n1 + (ys - amy )*n2 + (zs - amz )*n3
'          errnt=(xs - amxt)*f1 + (ys - amyt)*f2 + (zs - amzt)*f3
'         write(100,79)i,ierr,ierrt,ss,sst,tt,ttt,errn,errnt
'79       format(/,3(i5),6(1pe11.3),/)
'        endif
'
'        if(ierr.eq.ierrt.and.ierr.eq.0) then
'         if(abs(ss-sst).gt.0.0001 .or. abs(tt-ttt).gt.0.0001 .or.
'     &     (f1*n1 + f2*n2 + f3*n3) .lt. 0.99 ) then
'          errn =(xs - amx )*n1 + (ys - amy )*n2 + (zs - amz )*n3
'          errnt=(xs - amxt)*f1 + (ys - amyt)*f2 + (zs - amzt)*f3
'          write(100,69)i,ss,sst,tt,ttt,n1,n2,n3,f1,f2,f3,
'     &                 amx,amy,amz,amxt,amyt,amzt,xs,ys,zs,errn,errnt
'69        format(/,i5,10(1pe11.3),/,5x,11(1pe11.3),/)
'         endif
'        endif
'
'      endif    ! end debug
'
'      if(ierr.eq.1) go to 102
'c
'c.... check for penetration
'
'      ans=n1*(xs-amx)+n2*(ys-amy)+n3*(zs-amz)
'
'      if(ans.lt.pend) goto 140
'c
'      effstf = stf(l)*adeath
'
'      if(nty.eq.1)  go to 70
'c
'      if(ilimit.eq.1) dx=0  ! ilimit=1 => Full Newton Raphson
'      if(laugon) then
'        continue
'      else
'c
'		if(ans.lt.dx.and.ans.gt.0) then						! added by Qiang 
'			asf=10.											! added by Qiang 
'			effstf=effstf/asf								! added by Qiang 
'		end if
'c
'		if(ans.gt.dx) go to 102
'		if(ans.lt.dx.and.ans.gt.0) effstf = (1.0-ans/dx)*effstf ! since stiffness
'		if(ans.gt.0.) ans=0.                                    ! still used
'      endif
'c
'   70 if(laugon) then
'        fni=min(zero,xls(1,ii)+ans*stf(l)*adeath)
'        xls(1,ii)=fni
'        if(fni.lt.0.0) then
'c         ... traction ...
'          xgn=max(xgn,(-1.*ans))
'          lsh=.true.
'          lnk=.false.
'        else
'c         ... no traction ...
'          if(ans.lt.dx) then
'c           ... small gap ...
'            lsh=.true.
'            lnk=.false.
'            effstf = (1.0-ans/dx)*effstf
'          elseif(ans.eq.0) then
'            lnk=.false.
'          else
'c           ... big gap ...
'            goto 102
'          endif
'        endif
'      else
'        fni=ans*stf(l)*adeath
'      endif
'c
'      fxi=n1*fni
'      fyi=n2*fni
'      fzi=n3*fni
'c
'       if (fcoeff.ne.0) then    ! begin friction branch
'         if (laugon) then
'          stft=stf(l)*adeath
'          call fcalca(fni,fxi,fyi,fzi,fric,fdat(1,ii),iseg(1,ii),l,ss,
'     &                tt,n1,n2,n3,irect,dt,stft,x,fdx,fdy,fdz,fmax,fmag,
'     &                sfact,xls(2,ii),xls(3,ii),xgt)
'         else
'		if(lprtbd)then !added drb 12/02
'			call areal(dx1,area)
'			pi=3.1415926537
'			paramm=700.
'			paraml=10**(-(paramm/emodl2))
'			stft=(paraml/(1.-paraml))*area*adeath/(2.*(4./pi))
'		else
'          stft=0.01*stf(l)*adeath
'		endif
'          call fcalc(fni,fxi,fyi,fzi,fric,fdat(1,ii),iseg(1,ii),l,ss,tt,
'     &               n1,n2,n3,amx,amy,amz,irect,dt,stft,x,fdx,fdy,fdz,
'     &               fmax,fmag)
'         endif
'       endif                     ! end friction branch
'c  ....  at beginning of time step store last converged contact force,
'c        isoparm coords and master facet number
'       if(ifd.ne.0 .and. (.not.lifd) .and. lelstf) then
'        r_old(1,ii)=fxi
'        r_old(2,ii)=fyi
'        r_old(3,ii)=fzi
'        ss_old(1,ii)=ss
'        ss_old(2,ii)=tt
'        l_old(ii)=l
'       endif
'c      endif                     ! end .not. udch branch
'
'  200 continue
'
'c      if (.not.udch) then       ! begin .not. udch branch
'c   ... update force (master nodes)
'       do 80 jj=1,4
'       j3=3*jj
'       j2=j3-1
'       j1=j2-1
'       r(3+j1)=fxi*h(jj)
'       r(3+j2)=fyi*h(jj)
' 80    r(3+j3)=fzi*h(jj)
'c      endif                     ! end .not. udch branch
'c
'c   ... comment bsigeteqi calls when including brick bsi nodes
'c       generalized forces using exkcbs.f
'c
'       call unpkid(id,idp(1,i),1)
'       lmn(1)=id(1)
'       lmn(2)=id(2)
'       lmn(3)=id(3)
'
'       do 90 jj=1,4
'       call unpkid(id,idp(1,ix(jj)),1)
'       lmn(3+3*jj-2)=id(1)
'       lmn(3+3*jj-1)=id(2)
'       lmn(3+3*jj  )=id(3)
'
'   90  continue
'c
'c.... update force (slave nodes)
'c
'        r(1)=-fxi
'        r(2)=-fyi
'        r(3)=-fzi
'        lmmax=lmn(1)
'        do 95 kk=2,15
'        lmmax=max(lmmax,lmn(kk))
'   95   continue
'       if (lmmax.gt.neql) then
'         lrbr=.true.
'         xrb(1,1)=xs
'         xrb(2,1)=ys
'         xrb(3,1)=zs
'         xrb(1,2)=xx1(1)
'         xrb(2,2)=xx2(1)
'         xrb(3,2)=xx3(1)
'         xrb(1,3)=xx1(2)
'         xrb(2,3)=xx2(2)
'         xrb(3,3)=xx3(2)
'         xrb(1,4)=xx1(3)
'         xrb(2,4)=xx2(3)
'         xrb(3,4)=xx3(3)
'         xrb(1,5)=xx1(4)
'         xrb(2,5)=xx2(4)
'         xrb(3,5)=xx3(4)
'       else
'
'c        ... update rhs ...
'         do 100 kk=1,15
'         jj=lmn(kk)
'         if (jj.eq.0) go to 100
'         rhs(jj)=rhs(jj)+r(kk)
'  100    continue
'       endif
'c      endif                    ! end .not. udch branch
'
'      if(lifd) goto 140    ! skip stiffness for interface force death
'c
'c   ... zero history? ...
'  102 if (laugon) then
'         if (lsh) goto 105
'      else
'         if (ans.le.0..or.fcoeff.eq.0.) go to 105
'      endif
'c      if (.not.udch) then
'      iseg(2,ii)=0
'      fdat(8,ii)=0.
'      fdat(9,ii)=0.
'      fdat(10,ii)=0.
'c      endif
'c
'c   ... add stiffness? ...
'  105 if (laugon) then
'         if (lnk) goto 140
'      else
'         if (ans.gt.dx) goto 140
'      endif
'c
'c   ... uncomment following lines when including brick bsi nodes
'c       generalized forces using exkcbs.f
'
'      if (iphase-2) 110,110,140
'  110 if (iref) 140,120,140
'  120 if (fcoeff.eq.0.) call nstiff(effstf,n1,n2,n3,e)
'      if (fcoeff.gt.0.) call fstiff(n1,n2,n3,effstf,fxi,fyi,fzi,
'c     1    fni,fmax,fmag,e,alpha*stft)
'     1    fni,fmax,fmag,e,stft)
'      call stfinf(n1,n2,n3,h,e,s)
'      lrbk=.true.                   ! used as lstif (i.e. reform) in exkcrb
'      if (lrbr) goto 140
'c
'
'         lmn(1) = i                                                     
'         lmn(2) = ix(1)                                                 
'         lmn(3) = ix(2)                                                 
'         lmn(4) = ix(3)                                                 
'         lmn(5) = ix(4)                                                 
'         call bdg53(diag,s,lmn)                                         b_n_m
'         call blkcpy(s,sfdata(kfstor),ksizsf)                           
'         call blkcpy(lmn,sfdata(lfstor),neesf)                          
'         kfstor = kfstor + ksizsf                                       
'         lfstor = lfstor + neesf                                        
'         iebuf = iebuf + 1                                              
'      numelf=numelf+1
' 140  continue
'
'c
'c   ... uncomment following lines when including brick bsi nodes
'c       generalized forces using exkcbs.f
'c
'c c    ... bsidofc returns lmmax = 0 if no rigid body nodes and sets bsi  pngv
'c c    ... flag idbsf for exkcbsi also lrbr set = false in exkcbsi        pngv
'
'1000  continue
'2000   format(4(i10,a2))
'      return
'      end

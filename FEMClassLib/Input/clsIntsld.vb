'This file contains all the methods of intsld.f

Partial Public Class clsInput
    ''' <summary>
    ''' enter slide surface initialization
    ''' </summary>
    Public Sub intsld(ByVal x(,) As Double, ByRef lnsv() As Integer, ByRef lmsr() As Integer, ByRef ilocs() As Integer, ByRef ilocm() As Integer,
                      ByRef stfs() As Double, ByRef stfm() As Double, ByRef irtls() As Integer, ByRef irtlm() As Integer,
                      ByRef nsv() As Integer, ByRef msr() As Integer, ByRef nsegs() As Integer,
                      ByRef nsegm() As Integer, ByRef ibh(,) As Integer, ByRef cst(,) As Double, ByVal rdc(,) As Double, ByRef iparm(,) As Integer, ByRef irects(,) As Integer, ByRef irectm(,) As Integer,
                      ByRef xx1() As Double, ByRef xx2() As Double, ByRef xx3() As Double, ByVal det As Double, ByRef emodl2 As Double,
                      ByRef ixx() As Integer, ByRef xs As Double, ByRef ys As Double, ByRef zs As Double, ByRef amx As Double, ByRef amy As Double, ByRef amz As Double,
                      ByRef vn() As Double, ByRef llc As Integer, ByVal lresrt As Integer, ByRef resltl As Double, ByVal result As Double,
                      ByRef nrtm As Integer, ByRef nrts As Integer, ByRef nmn As Integer, ByRef nsn As Integer, ByRef nty As Integer, ByRef nst As Integer, ByRef mst As Integer, ByRef hh() As Double,
                      ByRef xx112 As Double, ByRef xx134 As Double, ByRef xx212 As Double, ByRef xx234 As Double, ByRef xx312 As Double, ByRef xx334 As Double, ByRef xx112p As Double, ByRef xx134p As Double, ByRef xx212p As Double, ByRef xx234p As Double, ByRef xx312p As Double, ByRef xx334p As Double, ByRef xx114 As Double, ByRef xx123 As Double, ByRef xx214 As Double, ByRef xx223 As Double, ByRef xx314 As Double, xx323 As Double, ByRef x1423 As Double, ByRef y1423 As Double, ByRef z1423 As Double, ByRef x1423p As Double, ByRef y1423p As Double, ByRef z1423p As Double)

        Dim maxtl As Integer        ' QW 8-2018

        Call slinit(iparm, irects, irectm, nsv, msr, nsegs, nsegm, lnsv, lmsr, ilocs, ilocm, stfs, stfm, irtls, irtlm, matype, xx1, xx2, xx3,
              x, ibh, cst, rdc, pend, det, emodl2, ixx, xs, ys, zs, amx, amy, amz, vn, llc, lresrt, resltl, result, nrtm, nrts, nmn, nsn, nty, nst, mst, hh,
              xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)

        maxtl = Math.Max(nsntl, nmntl)
        '      
    End Sub
End Class

'  ref org fortran code
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine intsld
'c      subroutine intsld(nstl,nmtl,nrttls,nrttlm)
'c      subroutine intsld(lnsv,lmsr,ilocs,ilocm,stfs,stfm,irtls,irtlm, 
'c     & xo,bh,cst,chrlen,ethik,nstl,nmtl,nrttls,nrttlm,lethik)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to enter slide surface initialization
'c
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09
'
'
'
'c      
'c      common/bk05a/n74
'c      
'      common/bk06/l00,l00a,l00b,l00c,l01,l02,l03,l04,l05,l06,l07,l08,l09
'      common/bk07/l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l00d,l00e
'      common/bk30/cpuio(72),cpuip(72)
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'
'
'      
'c      logical ldyna3d,lgpavg,ioff
'
'      
'      common/slp/nstl,nmtl,nrttls,nrttlm,maxtl                    ! QW 11-11-2015
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh ! QW 11-11-2015
'      common/slar3/nsl,nsntl,nmntl,nslnmx,sltol,slhrd                  ! QW 11-11-2015
'      common/bk37/numsv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n       ! QW 11-11-2015
'      
'      
'c
'c      
'c      
'c      
'      common/iread/idp(54000),x(27000)                                 ! QW 11-11-2015
'      common/readm/matype(25),den(25),prop(48,25),csprop(24,25),
'     & trefm(25),tmecc(25),rdc(4,25),aux(48)                           ! QW 11-11-2015
'c      common/irg/ibcrb(6,25),mgrb(25),irbn(9000),nnrb(25),
'c     & ixrb(9000),ncount(25),ioff(25)                                  ! QW 11-11-2015
'      common/inl/iparm(7,50),fric(3,50),pend(50),iaug(50),
'     & altol(2,50),sfact(50),tdeath(50),tbury(50),ifd(50)
'      common/inn/irects(8000),irectm(8000),nsf(2,50),
'     & nsv(8000),msr(8000),nsegs(8000),nsegm(8000)
'      common/in3/ia(81000),ihv(9001),numelg(2)              ! QW 11-11-2015      
'c      common/sls/lnsv(*),lmsr(*),ilocs(*),ilocm(*),stfs(*),stfm(*),
'c     & irtls(*),irtlm(*),cm(*),xo(*),bh(*),bs(*),
'c     & cst(3,1),chrlen(1),ethik(1),irbacm(*)
'c      dimension lnsv(nstl),lmsr(nmtl),ilocs(nsntl),ilocm(nmntl),       ! QW 11-11-2015
'c     & stfs(nrttls),stfm(nrttlm),irtls(nsntl),irtlm(nmntl),
'c     & xo(3*numnp),bh(9*numelh),cst(3,nsntl),chrlen(numsv),
'c     & ethik(lethik)
'      common/ins/lnsv(8000),lmsr(8000),ilocs(2000),ilocm(2000),       ! QW 11-11-2015
'     & stfs(2000),stfm(2000),irtls(2000),irtlm(2000),
'     & cst(3,2000)
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'intsld'
'      ik01 = ik01 + 1
'      end if
'
'c      n001o=n001
'c      if(ldyna3d) n001o=n001+3*numnp     ! use reference coords for stiff calc
'c
'c      call slinit(
'c     & a(l00),a(l01),a(l02),a(l03),a(l04),a(l05),a(l06),a(l07),a(l08),
'c     & a(l09),a(l10),a(l11),a(l12),a(l13),a(l14),a(nm0),a(nm2),a(ns02),
'c     & a(n001),a(n001o),a(nh01),a(ns01),a(l17),a(n73),a(n74),
'c     & a(nm1b),a(ipbw),a(l00c),a(irbac),a(isrbac),a(iprb2))
'c
'
'
'      call slinit(iparm,irects,irectm,nsv,msr,nsegs,nsegm,lnsv,lmsr,
'     & ilocs,ilocm,stfs,stfm,irtls,irtlm,matype,
'     & x,ia,cst,rdc,pend)
' 
'      maxtl=max0(nsntl,nmntl)
'      
'c      call zoslinit(
'c     & a(l00),a(l01),a(l02),a(l03),a(l04),a(l05),a(l06),a(l07),a(l08),
'c     & a(l09),a(l10),a(l11),a(l12),a(l13),a(l14),
'c     & a(n001),a(l17),
'c     & iparm,irects,irectm,nsv,msr,nsegs,nsegm,lnsv,lmsr,
'c     & ilocs,ilocm,stfs,stfm,irtls,irtlm,
'c     & x,cst,
'c     & nstl,nmtl,numnp,numsv,nrttls,nrttlm,nsntl,nmntl,maxtl)
'      
'c
'c      call testiparm(a(l00))
'c      call testa(a(l01),nrttls)
'c      call testa(a(l02),nrttlm)
'c      call testib(a(nh01),numelh)
'c      call testirtls(a(l13),maxtl)
'c      call testirtlm(a(l14),maxtl)
'c      call testcst(a(l17),nsntl)
'
'      
'c      do 100 i=1, 9*numelh, 9
'c100   write(150,300) ((ia(i+j-1),','),j=1, 9)                          
'c300   format(9(i10,a2))
'      
'      return
'      end
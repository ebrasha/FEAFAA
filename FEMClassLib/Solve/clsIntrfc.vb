'This file contains all the methods of intrfc.f
Partial Public Class clsSolve

    
    'Public negb1 As Integer
    Private irects(,), irectm(,), nsv(), msr(), nsegs(), nsegm(), lnsv(), lmsr(), ilocs(), ilocm(), irtls1(), irtlm1() As Integer
    Public stfs(), stfm(), fric(,), sftemp(,), xgn() As Double       ' QW 12-12-2018-
    Public sfact(), pend(), tdeath(), tbury() As Double, ifd(), ientmp(,) As Integer  ' QW 12-12-2018-




    ''' <summary>
    ''' to enter slidesurface overlay
    ''' </summary>
    Public Sub intrfc()

        Dim ifdn As Integer = ifd(numsv)     ' QW 12-12-2018-
        Dim r_old(3 * (nsntl + nmntl)), ss_old(2 * (nsntl + nmntl)) As Double, l_old((nsntl + nmntl)) As Integer  ' QW 12-12-2018-
        ReDim sftemp(ksizsf, nsnode), ientmp(neesf, nsnode), xgn(4 * numsv)               ' QW 12-12-2018-

        'Dim x(,) As Double, idp_copy(,) As Integer  'YC 092018

        'Call objNik3d.timing(cpuio, cpuip, 13, 2) 'YC 092018
        Call objComsub.timing(cpuio, cpuip, 13, 2)

        Dim m3 = n2
        Dim m5 = n3

        Dim x(3, numnp) As Double       ' QW 12-12-2018-
        Call blkcpy2D(xyz, x, 3, numnp)

        'YC? 092018
        'TODO - ReDim x and idp_copy with proper dimension, can't do now as blkcpy is missing
        'Call blkcpy(x, anegb1, 3 * numnp)

        'Dim x_1D(3 * numnp) As Double
        'Call objComsub.ArrayConvert2Dto1D(x, 3, numnp, x_1D)
        'Call blkcpy(x_1D, anegb1, 3 * numnp)
        'YC 092018 END

        'Dim mpstmp = negb1

        If iphase < 3 Then GoTo 10

        If iphase = 4 Then iphase = 2
        m3 = n6
        m5 = n5
        GoTo 20

10:
        ' YC 102418
        'TODO - Copy data from idp to idp_copy
        'Call sinf(un2, un3, x, fval, idp_copy, 15, 120, iparm, irects, irectm, nsv, msr, nsegs, nsegm, lnsv,
        '          lmsr, ilocs, ilocm, stfs, stfm, irtls, irtlm, fric, iseg, fdat, abdg, sfdata, anegb1, xls,
        '          xlm, xgn, sfact, iaug, pend, tdeath, tbury, ifd, r_old, ss_old, l_old)
        'TODO - Copy data from x to anegb1
        'TODO - Copy data from idp_copy to idp

        Call sinf(un2, un3, x, fval, idp, 15, 120, iparm, irects, irectm,
        nsv, msr, nsegs, nsegm, lnsv, lmsr, ilocs, ilocm, stfs, stfm, irtls1, irtlm1,
        fric, iseg, fdat, abdg, sfdata1, sfdata2, sftemp, ientmp, xls, xlm, xgn,
        sfact, iaug, pend, tdeath, tbury, ifd, r_old, ss_old, l_old)
        ' YC 102418 END


        GoTo 30

20:     ' QW 12-12-2018- sfdata1() as double, sfdata2() As Integer.

        ' YC 102418
        'TODO - Copy data from idp to idp_copy
        'Call sinf(un6, un5, x, fval, idp_copy, 15, 120, iparm, irects, irectm, nsv, msr, nsegs, nsegm, lnsv,
        '          lmsr, ilocs, ilocm, stfs, stfm, irtls, irtlm, fric, iseg, fdat, abdg, sfdata, anegb1, xls,
        '          xlm, xgn, sfact, iaug, pend, tdeath, tbury, ifd, r_old, ss_old, l_old)
        'TODO - Copy data from x to anegb1
        'TODO - Copy data from idp_copy to idp

        Call sinf(un6, un5, x, fval, idp, 15, 120, iparm, irects, irectm, nsv, msr, nsegs, nsegm, lnsv, lmsr,
        ilocs, ilocm, stfs, stfm, irtls1, irtlm1, fric, iseg, fdat, abdg, sfdata1, sfdata2, sftemp, ientmp,
         xls, xlm, xgn, sfact, iaug, pend, tdeath, tbury, ifd, r_old, ss_old, l_old)
        ' YC 102418 END


        '30:     Call objNik3d.timing(cpuio, cpuip, 13, 3) 'YC 092018
30:     Call objComsub.timing(cpuio, cpuip, 13, 3)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine intrfc
'c
'      implicit double precision (a-h,o-z)                             
'c
'c===> module to enter slidesurface overlay
'c
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk01/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n14a
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e,
'     & n20f,n20g
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09
'      common/fupdtc/ifl
'      common/bk06/l00,l00a,l00b,l00c,l01,l02,l03,l04,l05,l06,l07,l08,l09
'      common/bk07/l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l00d,l00e
'      common/bk07b/ifd1,ifd2,ifd3,ifd4
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk11/ndm,ndm2,ndm3,istr,ncon,nprm(2),nms
'      common/bk26/ntlen,ntlenf
'      common/bk30/cpuio(72),cpuip(72)
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/ebye2a/ mpdiag,mpelhx,mpelsf
'      common/ebye3/neehx,ksizhx,neesf,ksizsf
'
'      logical laugon
'      common/aug1/deftal,ip1d1,ip1d2,ip1d3,ip1d4,ip1d5,
'     &                   nalsf,ipsf1,ipsf2,ipsf3,ipsf4,ipsf5,ipsf6,
'     &                   ipsw1,ipsw2,ipsw3,ipsw4,maxaug,laugon
'      
'      common/ebye5/mbsize,nhxblk,ihxblk(750),nsnode,nsfblk,isfblk(750)
'      common/elcnts/numelf,nndt2(7),nnd2t2(7),nlrt2(7),nelcnt
'      common/bk37/numsv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
'      common/slar3/nsl,nsntl,nmntl,nslnmx,sltol,slhrd
'      common/slp/nstl,nmtl,nrttls,nrttlm,maxtl                        ! QW 11-11-2015
'      common/hx/hxdata(10000000),sfdata(1000000),sftemp(10000)        ! QW 11-11-2015
'      common/bk16/nlcur,nptst,nload,nptm
'      common/iread/idp(54000),x(27000)                                ! QW 11-11-2015
'      common/ini/npc(25),p(480),cmde(8,24),ixde(3,500),sclf(500),
'     & dehv(11,500),dehva(11,500),fval(48),mtypde(6)                  ! QW 11-11-2015
'      common/bdg/abdg(189000)
'      common/aa/anegb1(7508928),anegb2(7508928)
'      common/bkneq/neql
'      common/nib/un2(54000),un3(54000),un4(54000),un5(54000),
'     & un6(54000),tvc2(54000)
'      common/inl/iparm(7,50),fric(3,50),pend(50),iaug(50),
'     & altol(2,50),sfact(50),tdeath(50),tbury(50),ifd(50)
'      common/inn/irects(8000),irectm(8000),nsf(2,50),
'     & nsv(8000),msr(8000),nsegs(8000),nsegm(8000)
'      common/ins/lnsv(8000),lmsr(8000),ilocs(2000),ilocm(2000),       ! QW 11-11-2015
'     & stfs(2000),stfm(2000),irtls(2000),irtlm(2000),
'     & cst(3,2000)
'      common/l18/iseg(2,2000),fdat(10,2000)                           ! QW 11-11-2015
'      common/ipsf/xgn(200),xls(12000),xlm(12000)                      ! QW 11-11-2015                     
'c      
'      dimension r_old(12000),ss_old(8000),l_old(4000)               ! QW 11-11-2015
'
'      call timing(cpuio,cpuip,13,2)
'c
'      m3=n2
'      m5=n3
'
'      call blkcpy (x,anegb1,3*numnp)
'
'      mpstmp = negb1                                                 
'                  
'      if (iphase.lt.3) go to 10
'
'      if (iphase.eq.4) iphase=2
'      m3=n6
'      m5=n5
'      goto 20                         ! QW 11-11-2015
'c
'10    call sinf(un2,un3,anegb1,fval,idp,15,
'     & 120,iparm,irects,irectm,nsv,msr,nsegs,nsegm,lnsv,lmsr,
'     & ilocs,ilocm,stfs,stfm,irtls,irtlm,
'     & fric,iseg,fdat,abdg,sfdata,anegb1,
'     & xls,xlm,xgn,sfact,iaug,pend,tdeath,tbury,
'     & ifd,r_old,ss_old,l_old)
'
'      goto 30
'c      
'20    call sinf(un6,un5,anegb1,fval,idp,15,
'     & 120,iparm,irects,irectm,nsv,msr,nsegs,nsegm,lnsv,lmsr,
'     & ilocs,ilocm,stfs,stfm,irtls,irtlm,
'     & fric,iseg,fdat,abdg,sfdata,anegb1,
'     & xls,xlm,xgn,sfact,iaug,pend,tdeath,tbury,
'     & ifd,r_old,ss_old,l_old)
'      
'30    call timing(cpuio,cpuip,13,3)
'c
'      return
'      end

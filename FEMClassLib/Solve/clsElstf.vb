'This file contains all the methods of elstf.f
Partial Public Class clsSolve


    
    Public ptime, tnew As Double

    Public lcspf, numudc, itemp, numsv, nmelde, numnp, nmmat As Integer

    Public idp(,), nod(), idirn(), ncur(), nodep(,), lcp(), kht(), numelg(), ihv() As Integer

    Public fval(), fac(), xyz(,), xn(,), yn(,), zn(,), pmult(,), strt(), rf(), temp(), tmode(), tbase(), dehv(,), dehva(,) As Double



    Private ekp1, rhsn, told As Double

    Private ibkflg, nnns, llls, melemt, nblk As Integer

    Private lhex() As Integer

    Private r(), rfa(), temo() As Double

    Private lelstf As Boolean, neks As Integer




    ''' <summary>
    '''  drive element overlays at beginning of time step residual always computed, stiffness may be computed
    ''' </summary>
    Public Sub elstf()
        ' Call CheckName("elstf")
        Dim mprntl = 0, itcurv = 0
        Dim msg10 = ""
        Dim icadd = 0
        '
        lelstf = True 'by YC 102418
        neks = 0
        numelf = 0
        If time > ptime Then ekp1 = 0.0
        '
        'c  ... calculate rigid body velocities and accelerations
        Dim zero = 0.0

        'Call ldcset(fval, npc, p, time)    ' YC 092018
        Call objInit.ldcset(nlcur, fval, npc, p, time)  ' QW 12-12-2018

        If lcspf <> 0 Then
            If bNike3dMsg Then msg10 = "ik02 call ldmodspf in sub elstf"
        End If

        'c   ... zero rhs ...

        'Call objNik3d.azero(un3, neql)      ' YC 092018
        Call azero(un3, neql)

        'TODO - check 1d-2d array issue in loadcn
        Call loadcn(idp, fval, un3, nod, idirn, ncur, fac, xyz, un2, numnp, nmmat)
        'TODO - tvc1 not defined, check dimension
        'Call Check1D(un3, un3.Length - 1)
        'Dim tvc1(2) As Double  ' QW 12-12-2018

        'TODO - check 1d-2d array issue in loadpr
        Call loadpr(idp, tvc1, r, npc, p, xn, yn, zn, pmult, nodep, strt, lcp, time, xyz, nmmat)

        'c   ... zero rhs dof's with prescribed essential bc's
        If numudc <> 0 Then
            '
            If bNike3dMsg Then msg10 = "ik02 call zrhs 1 in sub elstf"

            'TODO - Missing Sub
            Call blkcpy(rf, rfa, numudc)

            'Call objNik3d.azero(rf, numudc)        ' YC 092018
            Call azero(rf, numudc)

        End If

        If itemp < 0 Then

            If bNike3dMsg Then msg10 = "ik02 call tempr  in sub elstf"
        End If
        If itemp > 0 Then

            'c	------------- Restore temperature feature, Qiang, 03-26-2010 ---------

            'TODO - Missing Sub


            itcurv = Math.Min(nstep + 1, ntime) ' YC 121219-3


            'Call temth(told, temo, tnew, temp, npc, p, itcurv, zero, tmode, tbase) 'QW 11-15-2019-YC 121219
            'Call temth(told, temo, tnew, temp, npc, p, itemp, time, tmode, tbase)  ' YC 121219-3
            Call temth(told, temo, tnew, temp, npc, p, itcurv, time, tmode, tbase)

            'c	----------------------------------------------------------------------

            If bNike3dMsg Then msg10 = "ik02 call temth in sub elstf"
        End If

        'c ----- lhs if reqd -----
        If iref = 0 Then

            'c   ... initialize column height array ...
            'TODO - Missing Sub
            Call blkcpy(kht, un1, neql)
            '
            'c   ... clear nodal block-diagonal array ...                            

            'Call objNik3d.azero(abdg, 21 * numnp)      ' YC 092018
            Call azero(abdg, 21, numnp)

            Call bsolvr(un1, 1, 4)

        End If


        Dim icaddr = 0

        'c     solid hexahedrons

        ibkflg = 0

        'Dim icaddi = 0, nelpg = 0, length = 0   ' YC 102418-052019-3-110419
        Dim icaddi = 0, length = 0

        nnns = 24
        llls = 324

        Call objInit.gethexg(1, numelg, ihv, nelpg, nblk, length, incflg, nh04, nh05)

        'lhex(1) = nelpg
        lhex(2) = nelpg   ' QW 12-12-2018

        'Call Checka2(anegb1, anegb2, length)

        Call blkcpy(anegb2, anegb1, length)

        Call solide()

991:
        Call blkcpy(anegb1, anegb2, length)
        'Call Check1D(un3, un3.Length - 1, 1)
        icaddi = icaddi + length

120:    If numsv > 0 Then
            nnns = 15
            llls = 135
            Call intrfc()
            If ifl <> 0 Then
                'TODO - check 1D-2D array in fupdt
                Dim xls_2D(3, nsntl), xlm_2D(3, nmntl) As Double
                Call objComsub.ArrayConvert1Dto2D(xls, xls_2D, 3, nsntl)
                Call objComsub.ArrayConvert1Dto2D(xlm, xlm_2D, 3, nmntl)
                Call fupdt(iseg, fdat, nsf, iaug, xls_2D, xlm_2D, iparm)
                Call objComsub.ArrayConvert2Dto1D(xls_2D, 3, nsntl, xls)
                Call objComsub.ArrayConvert2Dto1D(xlm_2D, 3, nmntl, xlm)

                'Call fupdt(iseg, fdat, nsf, iaug, xls, xlm, iparm)
            End If
        End If
        'Call Check2D(abdg, 21, numnp)
        melemt = lhex(4)

        'c     discrete elements


        ' YC 052019-4-110419, reversed by ' YC 052019-5-120219
        'Dim i
        'For i = 1 To nmelde
        '    dehva(10, i) = 0
        'Next i
        ' YC 052019-4-110419 END, reversed by ' YC 052019-5-120219 END


        'Call Check1D(un3, un3.Length - 1, 1)


        'YC 052019-5-120219
        'Call dscrte(idp, un2, un2, un3, xyz)
        If Math.Abs(rhsn - rhsc) <= 10 ^ -6 Then Call blkcpy(un40, un4, neql)
        Call dscrte(idp, un2, un4, un3, xyz)
        'YC 052019-5-120219 END


        If nmelde > 0 Then

            'TODO - Missing Sub
            'Call blkcpy(dehv, dehva, 11 * nmelde)      'YC 102418
            Call blkcpy2D(dehv, dehva, 11, nmelde)

        End If

        nnns = 6
        llls = 27

        'c     add aerodynamic drag forces

        nnns = 6
        llls = 27

        rhsn = dotprd(un3, un3)

        lelstf = False

    End Sub


End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine elstf
'c
'      implicit double precision (a-h,o-z)                            
'c
'c===> module to drive element overlays at beginning of time step
'c     residual always computed, stiffness may be computed
'c
'	common/block02/bNIKE3D, bNike3dMsg
'	logical bNIKE3D, bNike3dMsg
'
'	common/block03/StopFEDFAA, FEDFAAStopped	
'      INTEGER(4) StopFEDFAA, FEDFAAStopped	
'      common/double/iprec
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk01/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n14a
'      common/bk02/n10a,n10b,n10c,n10d,n10e
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e,
'     & n20f,n20g
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09     
'      common/bk06/l00,l00a,l00b,l00c,l01,l02,l03,l04,l05,l06,l07,l08,l09
'      common/bk07/l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l00d,l00e
'      common/bk08/itherm,itemp,n20,n21
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/b10a/inpsd,nmmtde,nmelde,nmmass
'      common/b10b/kd01,kd02,kd03,kd04,kd05,kd06  
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh
'      common/bk15/numpc
'      common/bk17/nthpx,nthpy,nthpz,nthsx,nthsy,nthsz,iacflg
'      common/bk20/kpri,nstep,ite,ilimit,iref
'      logical ldns
'      common/bk21/rhsn,rhsvn,cvtl,ectl,tolls,rctl,ldns,men,maxref,iteref
'      common/bk26/ntlen,ntlenf
'      common/bk29/tim(7),iterp1,iterp2,time,timep,lpri,npri
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/bk35/numdc,numudc,nrcc
'      common/bk37/numsv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
'c      
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      common/tin/itread,itrp1,itrp2
'      common/block/nblk
'      common/fissl1/melemt,nnns,n2g,llls
'      common/riksw2/rlnew,alfa0,dsx,iteopt,idctrl,riksf,numspu,mthunl
'      common/elcnts/numelf
'      common/fupdtc/ifl    
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/lunits2/luj,lusl,lumod,lumsr
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye2a/ mpdiag,mpelhx,mpelsf
'      common/bkspf1/etarg,emin,emax,ptime,ptmin,pdmax,lcspf,nmspf,tpelmc
'      common/bkspf2/ekm1,ek,ekp1,pdkm1,pdk,pk,pkp1,pfac,neks
'      common/bkneq/neql     
'      logical laugon
'      common/aug1/deftal,ip1d1,ip1d2,ip1d3,ip1d4,ip1d5,
'     &                   nalsf,ipsf1,ipsf2,ipsf3,ipsf4,ipsf5,ipsf6,
'     &                   ipsw1,ipsw2,ipsw3,ipsw4,maxaug,laugon
'      common/loccon/nhxic
'      logical lelstf
'      common/elstfb/lelstf
'      common/slar3/nsl,nsntl,nmntl,nslnmx,sltol,slhrd  ! slide surf list vars
'      common/histvar/iihv,inhxg
'
'      common/bk16/nlcur,nptst,nload,nptm
'      common/iread/idp(54000),x(27000)                                ! QW 11-11-2015
'      common/ini/npc(25),p(480),cmde(8,24),ixde(3,500),sclf(500),
'     & dehv(11,500),dehva(11,500),fval(48),mtypde(6)                  ! QW 11-11-2015
'      common/ild/rv(24),timv(24),                              ! QW 11-11-2015
'     & nod(300),idirn(300),ncur(300),fac(300)
'      common/tmp/told,tnew,tmode(9000),tbase(9000),         ! QW 11-11-2015
'     & temo(9000),temp(9000)
'      common/ilp/xn(8,1000),yn(8,1000),zn(8,1000),pmult(8,1000),
'     & nodep(8,1000),strt(1000),lcp(1000)
'      common/ibc/noded(1000),idid(1000),lcd(1000),amad(1000),
'     & idflad(1000),rf(1000),rfa(1000)
'      common/bdg/abdg(189000)
'c      
'      common/nia/un1(54000),kht(54000)
'      common/nib/un2(54000),un3(54000),un4(54000),un5(54000),
'     & un6(54000),tvc2(54000)
'      common/aa/anegb1(7508928),anegb2(7508928)
'      common/l18/iseg(2,2000),fdat(10,2000)                           ! QW 11-11-2015
'      common/ipsf/xgn(200),xls(12000),xlm(12000)                      ! QW 11-11-2015
'      common/in3/ia(81000),ihv(9001),numelg(2)              ! QW 11-11-2015
'      common/inl/iparm(7,50),fric(3,50),pend(50),iaug(50),
'     & altol(2,50),sfact(50),tdeath(50),tbury(50),ifd(50)
'      common/inn/irects(8000),irectm(8000),nsf(2,50),
'     & nsv(8000),msr(8000),nsegs(8000),nsegm(8000)
'c      
'      data mprntl/0/
'
'      icadd = 0                                                        
'
'      if(time.gt.ptime) ekp1=0.0
'
'c  ... calculate rigid body velocities and accelerations
'      zero = 0.0
'
'      call ldcset(fval,npc,p,time)
'
'      if(lcspf.ne.0) then
'
'      if (bNike3dMsg) write(10,*) 'ik02 call ldmodspf in sub elstf'
'	end if
'c
'c   ... zero rhs ...
'c      
'      call azero(un3,neql)
'
'      call loadcn(idp,fval,un3,nod,idirn,ncur,fac,
'     &            x,un2,numnp,nmmat)
'     
'      call loadpr(idp,tvc1,r,npc,p,xn,yn,
'     &           zn,pmult,nodep,strt,lcp,time,x,nmmat)
'
'c   ... zero rhs dof's with prescribed essential bc's
'      if(numudc.ne.0) then
'
'      if (bNike3dMsg) write(10,*) 'ik02 call zrhs 1 in sub elstf'
'
'        call blkcpy(rf,rfa,numudc)
'
'        call azero(rf,numudc)
'
'      endif
'
'      if(itemp.lt.0) then
'
'       if (bNike3dMsg) write(10,*) 'ik02 call tempr  in sub elstf'
'	end if
'      if(itemp.gt.0) then
'c
'c	------------- Restore temperature feature, Qiang, 03-26-2010 ---------
'c
'      call temth(told,temo,tnew,temp,npc,p,itcurv,zero,tmode,tbase)
'c	----------------------------------------------------------------------
'c                        
'       if (bNike3dMsg) write(10,*) 'ik02 call temth in sub elstf'
'       end if
'c
'c ----- lhs if reqd -----
'      if(iref.eq.0) then
'c
'c   ... initialize column height array ...
'         call blkcpy (kht,un1,neql)
'
'c   ... clear nodal block-diagonal array ...                            
'
'         call azero(abdg,21*numnp)
'
'         call bsolvr(un1,1,4)
'c
'      endif
'c -----------------------
'
'      icaddr=0          ! initialize out of core storage index
'c
'c     solid hexahedrons
'c
'      ibkflg = 0                                                       
'
'      icaddi = 0
'      nnns=24
'      llls=324
'
'          call gethexg (1,numelg,ihv,nelpg,nblk,length)
'
'          lhex(2) = nelpg
'c
'             call blkcpy(anegb2,anegb1,length)
'
'          call solide(*991)
'
'991       call blkcpy(anegb1,anegb2,length)
'
'            icaddi = icaddi + length
'              
' 120       if (numsv.gt.0) then
'           nnns=15
'           llls=135
'           call intrfc
'           if (ifl.ne.0) then
'
'            call fupdt(iseg,fdat,nsf,iaug,xls,xlm,iparm)
'	     end if
'        endif
'
'      melemt=lhex(4)
'c
'c     discrete elements
'c
'      call dscrte(idp,un2,un2,un3,x)
'
'      if (nmelde.gt.0) then
'
'      call blkcpy(dehv,dehva,11*nmelde)           
'      endif
'
'      nnns=6                                                        
'      llls=27                                                      
'
'c     add aerodynamic drag forces
'c
'      nnns=6
'      llls=27
'
'      rhsn=dotprd (un3,un3)
'
'      lelstf=.false.
'c
'
'      return
'
'      end

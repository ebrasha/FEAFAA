'This file contains all the methods of asmrhs.f
Partial Public Class clsSolve

    Private icadh, icadb, icads As Integer ' YC 092018

    ''' <summary>
    ''' to drive assembly of residual vector, will also return element stiffnesses in some cases
    ''' </summary>
    ''' <param name="ui"></param>
    ''' <param name="usi"></param>
    ''' <param name="tvc2"></param>
    ''' <param name="r"></param>
    ''' <param name="u"></param>
    ''' <param name="tvc1"></param>
    ''' <param name="tt"></param>
    ''' <param name="_step"></param>
    ''' <param name="iopt"></param>
    ''' <param name="g"></param>
    Public Sub asmrhs(ByRef ui() As Double, ByRef usi() As Double, ByRef tvc2() As Double,
                      ByRef r() As Double, ByRef u() As Double, ByRef tvc1() As Double,
                      ByRef tt As Double, ByRef _step As Double, ByRef iopt As Integer, ByRef g As Double)
        'Call CheckName2("asmrhs", iopt)
        'Dim naic, nwic As Integer
        Dim neks = 0
        Dim one = 1.0
        If time > ptime Then ekp1 = 0.0
        'c   ... save iphase since iphase set = 2 if iphase = 4
        'c       iphase = 2 from elstf, iphase = 4 from asmrhs when reforming
        Dim iphsav = iphase
        numrhs = numrhs + 1     'count the number of rhs calcs made by asmrhs

        If iopt = 1 Then GoTo 20

        Dim i As Integer ' YC 102418

        'For i = 0 To neql - 1             ' iopt=0 to do line searchs i.e. loop over   ' YC 102418
        For i = 1 To neql            ' iopt=0 to do line searchs i.e. loop over
            ui(i) = ui(i) + _step * tvc2(i) ' asmrhs with different steps
            usi(i) = ui(i)
            tvc1(i) = u(i) + ui(i)
        Next

        'c   ... zero rhs ...

        '20:     Call objNik3d.azero(r, neql)   ' YC 092018
20:     Call azero(r, neql)

        'c   ... add concentrated nodal loads and pressure loads ...

        Call loadcn(idp, fval, r, nod, idirn, ncur, fac, xyz, tvc1, numnp, nmmat)
        Call loadpr(idp, tvc1, r, npc, p, xn, yn, zn, pmult, nodep, strt, lcp, tt, xyz, nmmat)

        If numudc <> 0 Then

            'Call objNik3d.azero(rf, numudc)        ' YC 092018
            Call azero(rf, numudc)

        End If

        If iphase = 2 Or iphase = 4 Then
            numelf = 0
            'c....... clear nodal block-diagonal array    

            'Call objNik3d.azero(abdg, 21 * numnp)      ' YC 092018
            Call azero(abdg, 21, numnp)

        End If


        If iphsav = 4 Then
            Call bsolvr(un1, 1, 8)
        End If

        'c   ... add rigid body inertia and calc inertial stiffness

        Dim icadd = 0
        Dim icaddr = 0                 ' initialize out of core storage index
        icadh = 0                 ' pointers to access out of core storage
        icadb = 0
        icads = 0

        'c     solid hexahedrons

        Dim icaddi = 0
        stepls = _step
        nnns = 24
        llls = 324
        Dim m = 1
        Dim nelpg = 0
        Dim length = 0
        Call objInit.gethexg(1, numelg, ihv, nelpg, nblk, length, incflg, nh04, nh05)
        lhex(2) = nelpg                  ' QW 12-12-2018-
        If iphase = 2 Then iphase = 4



        If iopt = 1 Then
            'Call Checka2(anegb1, anegb2, length)
        End If

        Call blkcpy(anegb2, anegb1, length)
        Call solide()
        ' If iopt = 1 Then
        'Call Checka2(anegb1, anegb2, length)
        'End If
        'Call Check1D(r, neql, 1)
        If bNike3dMsg Then
            Dim msg1 = " after call solide"
        End If

        'If incflg = 1 Then              ' QW 12-12-2018-
        'naic = nh05 - 1
        'nwic = 333 * nelpg

        ' YC 092018
        'Call blkcpy(anegb1(1 + naic), anegb2(1 + naic), nwic)

        'Dim anegb1_naic_1(nwic), anegb2_naic_1(nwic) As Double
        ' Call objComsub.ArrayExtract1Dfrom1D(anegb1, 1 + naic - 1, anegb1_naic_1, nwic)

        'Call blkcpy(anegb1_naic_1, anegb2_naic_1, nwic)

        ' Call objComsub.ArrayInsert1Dto1D(anegb2_naic_1, nwic, anegb2, 1 + naic - 1)
        ' YC 092018 END


        'End If

        ' icaddi = icaddi + length

        icadh = icaddr   ' save pointer to end of out of core brick memory

        If bNike3dMsg Then
            Dim msg2 = " after solid hexahedrons in asmrhs"
        End If

        'c     sliding interfaces

        If numsv > 0 Then
            nnns = 15
            llls = 135
            iphase = iphsav
            Call intrfc()
            iphase = iphsav
        End If
        'Call Check1D(r, neql, 1)
        melemt = lhex(4)                ' QW 12-12-2018-

        'c     discrete elements

        Call dscrte(idp, tvc1, ui, r, xyz)

        nnns = 6
        llls = 27

        'c     aerodynamic drag segments

        nnns = 6
        llls = 27

        If iopt = 1 Then Return

        'For i = 0 To neql - 1   ' YC 102418
        For i = 1 To neql
            ui(i) = ui(i) - _step * tvc2(i)
            usi(i) = ui(i)
        Next

        g = dotprd(tvc2, r)


    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine asmrhs(ui,usi,tvc2,r,u,tvc1,tt,step,iopt,g,*)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to drive assembly of residual vector
'c     will also return element stiffnesses in some cases
'c
'      common/block02/bNIKE3D, bNike3dMsg
'	logical bNIKE3D, bNike3dMsg
'      
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/double/iprec
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk01/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n14a
'      common/bk02/n10a,n10b,n10c,n10d,n10e
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e,
'     & n20f,n20g
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09
'      common/bk08/itherm,itemp,n20,n21  
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk15/numpc
'      common/bk20/kpri,nstep,ite,ilimit,iref
'      common/bk26/ntlen,ntlenf
'      common/bk29/tim(7),iterp1,iterp2,time,timep,lpri,npri
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/bk35/numdc,numudc,nrcc
'      common/bk37/numsv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
'      common/block/nblk
'      common/fissl1/melemt,nnns,n2g,llls
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      common/outbuf/icadh,icadb,icads
'      common/numrhs/numrhs
'      common/riksw2/rlnew,alfa0,dsx,iteopt,idctrl,riksf,numspu,mthunl
'      common/elcnts/numelf
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye2a/ mpdiag,mpelhx,mpelsf
'      common/bkspf1/etarg,emin,emax,ptime,ptmin,pdmax,lcspf,nmspf,tpelmc
'      common/bkspf2/ekm1,ek,ekp1,pdkm1,pdk,pk,pkp1,pfac,neks
'      common/bkneq/neql    
'      common/loccon/nhxic
'      common/bk16/nlcur,nptst,nload,nptm
'      logical laugon
'      common/aug1/deftal,ip1d1,ip1d2,ip1d3,ip1d4,ip1d5,
'     &                   nalsf,ipsf1,ipsf2,ipsf3,ipsf4,ipsf5,ipsf6,
'     &                   ipsw1,ipsw2,ipsw3,ipsw4,maxaug,laugon
'      dimension ui(*),usi(*),tvc2(*),r(*),u(*),tvc1(*)
'c
'      common/slar3/nsl,nsntl,nmntl,nslnmx,sltol,slhrd
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh
'      common/iread/idp(54000),x(27000)                                 ! QW 11-11-2015
'      common/ini/npc(25),p(480),cmde(8,24),ixde(3,500),sclf(500),
'     & dehv(11,500),dehva(11,500),fval(48),mtypde(6)                 ! QW 11-11-2015
'      common/ild/rv(24),timv(24),
'     & nod(300),idirn(300),ncur(300),fac(300)                         ! QW 11-11-2015
'      common/ilp/xn(8,1000),yn(8,1000),zn(8,1000),pmult(8,1000),
'     & nodep(8,1000),strt(1000),lcp(1000)
'      common/ibc/noded(1000),idid(1000),lcd(1000),amad(1000),
'     & idflad(1000),rf(1000),rfa(1000)
'      common/bdg/abdg(189000)
'      common/nia/un1(54000),kht(54000)
'      common/aa/anegb1(7508928),anegb2(7508928)
'      common/in3/ia(81000),ihv(9001),numelg(2)              ! QW 11-11-2015
'      common/histvar/iihv,inhxg    
'c      
'      equivalence (lhex(7),mxn)
'c
'      neks=0
'      one =1.0
'      if(time.gt.ptime) ekp1=0.0
'c   ... save iphase since iphase set = 2 if iphase = 4
'c       iphase = 2 from elstf, iphase = 4 from asmrhs when reforming
'      iphsav=iphase
'      numrhs=numrhs+1         ! count the number of rhs calcs made by asmrhs
'
'      if(iopt.eq.1) go to 20
'
'      do 10 i=1,neql             ! iopt=0 to do line searchs i.e. loop over
'      ui(i)  =ui(i)+step*tvc2(i) ! asmrhs with different steps
'      usi(i) =ui(i)
'   10 tvc1(i)=u(i)+ui(i)
'c
'c   ... zero rhs ...
'
'20    call azero(r,neql)                                  ! QW 11-11-2015
'c
'c   ... add concentrated nodal loads and pressure loads ...
'c
'      call loadcn(idp,fval,r,nod,idirn,ncur,fac,
'     &            x,tvc1,numnp,nmmat)
'      call loadpr(idp,tvc1,r,npc,p,xn,yn,zn,
'     1            pmult,nodep,strt,lcp,tt,x,nmmat)
'
'      if (numudc.ne.0) then
'        call azero(rf,numudc)
'      endif
'c
'      if (iphase.eq.2.or.iphase.eq.4) then
'          numelf=0
'c....... clear nodal block-diagonal array                               
'c                                                                       
'         call azero(abdg,21*numnp) 
'      endif
'c
'c      if (iphsav.eq.4) call bsolvr(a(n1),a(ntlen),1,8)
'      if (iphsav.eq.4) then
'          call bsolvr(un1,1,8)
'      end if
'c   ... add rigid body inertia and calc inertial stiffness
'
'      icadd = 0                                                         
'      icaddr= 0                 ! initialize out of core storage index
'      icadh = 0                 ! pointers to access out of core storage
'      icadb = 0
'      icads = 0
'c
'c     solid hexahedrons
'c
'c      if(ngphf.gt.ngphl) go to 50
'      icaddi = 0
'      stepls = step                                                     
'      nnns=24
'      llls=324
'      m=1
'      call gethexg (1,numelg,ihv,nelpg,nblk,length)
'      lhex(2) = nelpg
'      if(iphase.eq.2) iphase=4
'      call blkcpy(anegb2,anegb1,length)
'      call solide(*991)
'      if (bNike3dMsg) then 
'      write(10,*) ' after call solide'
'	end if
'c
'          if (incflg.eq.1) then                                         
'            naic = nh05 - 1
'            nwic = 333*nelpg
'            call blkcpy(anegb1(1+naic),anegb2(1+naic),nwic)
'          endif                                                         
'c
'           icaddi = icaddi + length
'
'      icadh = icaddr   ! save pointer to end of out of core brick memory
'      
'      if (bNike3dMsg) then 
'      write(10,*) ' after solid hexahedrons in asmrhs'
'	end if
'c
'c     sliding interfaces
'c
'  140 if(numsv.gt.0) then
'          nnns=15
'          llls=135
'          iphase=iphsav
'          call intrfc
'          iphase=iphsav
'      endif
'c
'       melemt=lhex(4)
'c
'c     discrete elements
'c
'      call dscrte(idp,tvc1,ui,r,x)
'c
'c     stonewalls                                                     
'      nnns=6                                                            
'      llls=27                                                           
'c
'c     aerodynamic drag segments
'c
'      nnns=6
'      llls=27
'c
'      if(iopt.eq.1) return
'c
'      do 150 i=1,neql
'      ui(i)=ui(i)-step*tvc2(i)
'  150 usi(i) =ui(i)
'c
'      g=dotprd (tvc2,r)
'      return
'c
'  991 continue
'898   format(a10,i8,2(a2,e15.8))
'      return 1
'      end

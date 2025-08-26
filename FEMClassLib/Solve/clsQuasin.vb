'This file contains all the methods of quasin.f
Partial Public Class clsSolve


    ' YC 092018
    Public rhsvn As Double
    Public lrattl, lneghex, ldns, laugon As Boolean
    Public men, maxref, itrtot, irhtot As Integer

    Private ite, nbfgs, iteref, numupd, numrhs, itrlas, irhlas As Integer
    Private rkapg, rlnew, dn1 As Double
    Private al20(), rhs(4) As Double
    ' YC 092018 END

    Dim engini As Double ' YC 052019-2-110419
    Dim rhsc As Double ' YC 052019-3-110419

    ''' <summary>
    ''' TO DRIVE QUASI-NEWTON ITERATION METHODS
    ''' </summary>
    ''' <param name="ui"></param>
    ''' <param name="usi"></param>
    ''' <param name="r"></param>
    ''' <param name="tvc2"></param>
    ''' <param name="u"></param>
    ''' <param name="tvc1"></param>
    ''' <param name="tt"></param>
    ''' <param name="ierr"></param>
    Public Sub quasin(ByRef ui() As Double, ByRef usi() As Double, ByRef r() As Double,
                      ByRef tvc2() As Double, ByRef u() As Double, ByRef tvc1() As Double,
                      ByRef tt As Double, ByRef ierr As Integer)
        'c
        'c     bfgs method by matthies and strang ---  published in
        'c     international journal for numerical methods in engineering
        'c     volume 14, pages 1613-1626, 1979 and broyden's method
        'c
        ' Call CheckName("quasin")
        Dim i As Integer    ' YC 102418
        ReDim al20(62 * neql)

        If Not bNIKE3D Then
            If StopFEDFAA = -1 Then Return
        End If
        Dim rhsini, dspini, engreq, engco, dspreq, rhsreq, engblow, rhst As Double
        Dim iengskip, irat As Integer
        Dim lctmp As Boolean

        Dim g = 0.0

        Dim tolfd = 1.0E-32   ' temporary def. of r.o. tol. sqared
        Dim irnegchk = 1
        Dim rhsmx = rhsn
        'Dim rhsc = rhsvn   ' YC 052019-3-110419
        Dim usimx = 0.0
        Dim engmx = 0.0
        rkapg = 0.0
        Dim lratt0 = lrattl
        Dim nstp1 = nstep + 1
        Dim alfa = 0.0
        Dim naug = 1
        ite = 0
        Dim itr = 0
        nbfgs = 0
        iteref = 0
        numupd = 0
        Dim rzero = 0
        numrhs = 1
        iphase = 3
        Dim laugconv = False
        Dim lpconv = False
        Dim engct = 0.0
        If numudc <> 0 Then
            Dim msg1 = "ik02 call zrhs in sub quasin"
        End If
        '
        If mthsol = 5 Then GoTo 10
        Call blkcpy(r, al20, neql)
10:     Dim engc = dotprd(r, ui)

        'Dim engini = engc   ' YC 052019-2-110419
        If nstep = 0 Then engini = engc

        Dim onem = -1

        'For i = 0 To neql - 1      ' YC 102418
        For i = 1 To neql
            tvc2(i) = ui(i)
            ui(i) = 0.0
        Next
        If numudc <> 0 Then
            Dim msg2 = "ik02 call swtche in sub quasin"
        End If

        'c______________________________________________________________________________

        'c  ... BEGIN EQUILIBRIUM ITERATIONS

        'c  ... havent converged yet so start a new iteration

30:     ite = ite + 1           ' count number of bfgs iters between reforms
        itr = itr + 1           ' count number of total iterations
        Dim _step = 1.0            ' line search step size


        '  YC 052019-3-110419
        'If ite = 1 And iteref = 0 Then
        '    PrintLine(lutty, "")
        '    Dim msg170 = String.Format(" ======= beginning time step #{0} =======", nstp1.ToString("####"))

        '    PrintLine(lutty, msg170)    ' YC 102418-012819

        '    If bNIKE3D Then
        '        '        write(*,170)nstp1
        '    End If

        'End If
        'If ite = 1 Then
        '    Dim msg175 = " iterating:"

        '    PrintLine(lutty, msg175)    ' YC 102418-012819

        '    If bNIKE3D Then
        '        '        write(*,175)
        '    End If

        'End If
        '  YC 052019-3-110419 END



        'Dim t1, newtext As String
        'Call com.get_time(t1)
        'newtext = vbCrLf & "iterating = " & t1 & vbCrLf
        'Print(lutty, newtext)

        '        write(lutty,180)ite                                             1
        Dim msg180 = " " + ite.ToString("##")

        Print(lutty, msg180)    ' YC 102418-012819

        If bNIKE3D Then
            '        write(*,180)ite                                                 
        End If

        If Not bNIKE3D Then
            FEDFAAStopped = 0
            Call WINYIELD(StopFEDFAA, FEDFAAStopped)
            If StopFEDFAA = -1 Then Return
        End If

        'c______________________________________________________________________________

        'c  ... DO RIGHT HAND SIDE CALCULATION BASED ON CURRENT DISPLACEMENT

        'c  ... for full newton raphson we dont want to force 2 calls to asmrhs
        'c      so we do assembly on this first call and skip the call below
        'c      before factorization

        Dim lref = 0
        Dim irfact = 0
        If ilimit = 1 Then  ' do reform here instead of below, set flags
            iphase = 4             ' force stiffness assembly in asmrhs and others
            iref = 0               ' flag counting down stiffness forms reset
            nbfgs = 0              ' number of bfgs vectors stored reset
            numupd = 0             ' reset # of bfgs updates so far
            irfact = 1             ' for spf
            iteref = iteref + 1      ' some models need to know this is the first reform
            lref = 1               ' flag set so all routines below know reform done
            Call blkcpy(kht, un1, neql)
        End If

        'c   ... do rhs calc to get error norms. (assembely done too if ilimit=1)

        Call asmrhs(ui, usi, tvc2, r, u, tvc1, tt, _step, 0, g)
        'Call CheckName("iopt, 0")
        'Call Checka2(r, tvc2, neql, 1)
        If bNike3dMsg Then
            Dim msg3 = " after call asmrhs"
        End If

        iphase = 3              ' reset to no reforms
        iref = iref + lref        ' reset iref since reform hasnt happened yet
        iteref = iteref - lref    ' reset iteref since it will be reincremented
        irnegchk = 0            ' i.e. no element inverted
        If lpconv And irfact <> 1 Then ' for spf algorithm
            engct = dotprd(tvc1, r)
            GoTo 40
        End If
        'c______________________________________________________________________________

        'c   ... LINE SEARCH ?

        'c   ... determine if line search needed based on energy norm

        If tolls >= 1.0E+32 Then GoTo 40 ' i.e. line search turned off
        If Math.Abs(g) <= (tolls * Math.Abs(engc)) Or engc * g >= 0.0 Then GoTo 40
        If incflg = 0 Then
            Call lnesch(ui, usi, r, tvc2, u, tvc1, tt, _step, g, engc, rlnew, riksf, alfa)
        End If

        stepsv = _step

        'c    ... handle inverted elements by reforming
992:
        If lneghex Then
            If irnegchk = 0 Then
                irnegchk = 1
                _step = -1             ' unincrement ui since not finished in asmrhs
            Else
                GoTo 991            ' the last reform didnt help
            End If
        End If

        'c    ... update displacement based on step from line search

        '40:     For i = 0 To neql - 1      ' YC 102418
40:     For i = 1 To neql
            ui(i) = ui(i) + _step * tvc2(i)
            usi(i) = ui(i)
            tvc1(i) = u(i) + ui(i)
        Next

        'c    ... update quaternions
        'c______________________________________________________________________________

        'c  ... CALCULATE APPROPRIATE NORMS TO DETERMINE CONVERGENCE

        If irnegchk = 1 Then GoTo 60 ' element inverted go directly to reform

        dn1 = dotprd(tvc1, tvc1)
        Dim dspc = dotprd(tvc2, tvc2)

        'TODO - check usic index
        'usic(0, 0) = dotprd(usi, usi)      ' YC 102418-012819
        Dim usic = dotprd(usi, usi)

        Dim dinorm = Math.Sqrt(dspc) * _step
        rhsc = dotprd(r, r)

        rkapg = Math.Max(rkapg, rhsc)

        If ite = 1 And iteref = 0 Then
            rhsini = rhsmx
            dspini = dspc
        End If

        ''TODO - check usic index
        'usimx = Math.Max(usic(0, 0), usimx)     ' YC 102418-012819
        usimx = Math.Max(usic, usimx)

        engmx = Math.Max(Math.Abs(engc), engmx)

        If ldns Then
            If lpconv Then
                engreq = Math.Min(ectl * Math.Abs(engct), Math.Abs(engco))
                engmx = Math.Min(Math.Abs(engct), Math.Abs(engco) / ectl)
                If irfact = 1 Then lpconv = False ' then we don't need to do extra newton step
            Else                               ' since we just did one
                engreq = ectl * engmx
            End If

            'TODO - check usic index
            'dspreq = usic(0, 0) * cvtl  ' YC 102418-012819
            dspreq = usic * cvtl

            rhsreq = rctl * rhsmx
        Else
            dspreq = dn1 * cvtl
            engreq = ectl * Math.Abs(engini)
            rhsreq = rctl * rhsini
        End If

        'c  ... determine if problem is blowing up

        If itr <= 2 Then            ' sometimes contact occurs in first or second
            engblow = engmx               ' so intial energy norm too small
        End If
        If iengskip <> 1 Then
            If Math.Abs(engc / (engblow + 0.000001)) > 100000.0 And engini <> 0 And itr > 2 Then ' when itr = 1 sometimes contact occurred
                If iauto <> 0 Then      ' and we want to give the time step another
                    ierr = 1                  ' chance
                    Dim msg4 = "iteration = " + itr.ToString()

                    Print(lutty, msg4)    ' YC 102418-012819

                    If bNIKE3D Then
                        '        write(*,*) 'iteration = ',itr
                    End If

                    '        write(lutty,300)
                    Dim msg300 =
                        " *******************************************************" + Environment.NewLine +
                        " *                 - FATAL ERROR -                     *" + Environment.NewLine +
                        " *          Energy norm growing unbounded              *" + Environment.NewLine +
                        " *          during equilibrium iteration               *" + Environment.NewLine +
                        " *******************************************************"

                    PrintLine(lutty, msg300)    ' YC 102418-012819

                    If bNIKE3D Then
                        '        write(*,300)
                    End If

                    '        write(luo,300)
                    Return
                End If
            End If
        End If

        'c  ... check to see if force norm increasing: if so reform
        'c      men = 1 will preclude reforms except when equilib iteration
        'c      limits reached

        If men = 1 Then
            rhst = 2 * rhsc
        Else
            rhst = rhsmx
        End If
        irfact = 0

        'c  ... deterimine if reform necessary, if not check for convergence

        If rhsc <= rhst Or Math.Abs(rhsc) < tolfd Then GoTo 70 ' check for conv at 70
        rhsmx = rhsc
        '      write(luo,160) ite     
        Dim msg160 = " residual load vector larger than incremental loads after iteration " + ite.ToString("#####")

        Dim msg190 = "    iteration process diverging . . . . ."    ' YC 102418-012819


        'PrintLine(lutty, msg190)   ' YC 121219-1
        Print(lutty, msg190)


        If bNIKE3D Then
            '        write(*,190)
            'Dim msg190 = "    iteration process diverging . . . . ."   ' YC 102418-012819
        End If

        'c______________________________________________________________________________
        '
        'c  ... DO NEW STIFFNESS MATRIX ASSEMBLY AND REFORM IF NECESSARY

60:
        iteref = iteref + 1

        'irat = iteref - 4 * ((iteref - 1) / 4) ' YC 102418-012819
        irat = iteref - 4 * Int(((iteref - 1) / 4))

        rhs(irat) = rhsc
        'c ... maxref < 0 means do only maxref equilib iterations and go to next step

        '      !second exit

        If iteref > 14 Then GoTo 110 'ikawa September 20, 2005

        If iteref > Math.Abs(maxref) And maxref > 0 Then

            Dim msg220 = " *** reformation limit reached without converging ***"      ' YC 102418-012819
            PrintLine(lutty, msg220)

            If bNIKE3D Then
                'Dim msg220 = " *** reformation limit reached without converging ***"   ' YC 102418-012819
            End If

            ierr = 1
            Return
        ElseIf maxref < 0 And iteref = Math.Abs(maxref) Then
            GoTo 110
        End If

        '      write(luo,150)
        Dim msg150 = " stiffness matrix will now be reformed "


        Dim msg210 = String.Format("commence reformation # {0}", iteref.ToString("0"))          ' YC 121219-1
        PrintLine(lutty, msg210)


        If lrattl And (Math.Abs(rhsc) < 100 * Math.Abs(rhsreq)) Then
            lrattl = False
            '        write(luo,280)
            Dim msg280 = New String(" ", 45) + "rattle flag ""off"""
        End If
        ite = 0
        iref = 0
        nbfgs = 0
        numupd = 0
        iphase = 4
        irfact = 1
        If lref = 1 And _step = 1 Then GoTo 65
        'Call CheckName("iopt, 1")
        'Call Checka2(r, tvc2, neql, 1)
        Call blkcpy(kht, un1, neql)
        Call asmrhs(ui, usi, r, r, u, tvc1, tt, _step, 1, g)
65:
        'Call CheckName("iopt, 1")
        'Call Checka2(r, tvc2, neql, 1)
        Call blkcpy(r, al20, neql)
        Call blkcpy(r, tvc2, neql)
        Call bsolvr(tvc2, 2, 8)
        engc = dotprd(tvc2, r)
        iref = 1
        iphase = 3
        GoTo 30
        '
        'c______________________________________________________________________________
        '
        'c   ... DETERMINE IF ITERATIONS HAVE CONVERGED

70:     If iteref > 14 Then GoTo 110 'ikawa September 20, 2005

        If Math.Abs(rhsc) < tolfd And dspc < tolfd Then GoTo 110 ' i.e. machine tol.
        If lpconv Then                          ' force a newton calc after
            lpconv = False                           ' a spf increment to get a
            GoTo 80                                  ' delta u
        End If
        If maxref < 0 And itr = Math.Abs(maxref) Then GoTo 110
        If dspc > dspreq Then GoTo 80
        If Math.Abs(engc) > engreq Then GoTo 80
        If Math.Abs(rhsc) > Math.Abs(rhsreq) Then GoTo 80

        GoTo 110                                 ' ITERATION HAS CONVERGED !

80:     If ite < ilimit And _step >= 0.001 Then
            GoTo 90
        ElseIf ite >= ilimit Then
            '        write(luo,140)
            Dim msg140 = " iteration limit reached maximum permitted "


            Dim msg200 = "limit reached without converging. . ."    ' YC 121219-1
            PrintLine(lutty)
            Print(lutty, msg200)


        ElseIf _step < 0.001 Then
            '        write(luo,145)
            Dim msg145 = " line search stepsize zero "


            Dim msg205 = "line search stepsize zero . . . . . "    ' YC 121219-1
            PrintLine(lutty)
            Print(lutty, msg205)


        End If
        GoTo 60                ' do reform

        'c______________________________________________________________________________

        'c   ... DO QUASI-NEWTON ITERATION INSTEAD OF NEW REFORM

90:     If mthsol = 1 Then Call bfgs(tvc2, r, tvc1, _step, g, engc, dinorm)
        If mthsol = 2 Then
            Dim msg5 = "ik02 call broy in sub quasin"
        End If

        If mthsol = 3 Then
            Dim msg6 = "ik02 call dfp in sub quasin"
        End If

        If mthsol = 4 Then
            Dim msg7 = "ik02 call davd in sub quasin"
        End If

        If mthsol = 5 Then
            Dim msg8 = "ik02 call modn in sub quasin"
        End If

        If mthsol = 5 Then GoTo 100

        ' YC 092018
        'Call blkcpy(al20(1 + nbfgs), r, neql)

        Dim al20_nbfgs_1(neql) As Double

        Call objComsub.ArrayExtract1Dfrom1D(al20, 1 + nbfgs - 1, al20_nbfgs_1, neql)

        Call blkcpy(al20_nbfgs_1, r, neql)
        ' YC 092018 END


100:    engc = dotprd(tvc2, r)
        GoTo 30                ' go back up to get rhs and check convergence

        'c______________________________________________________________________________
        '
        'c   ... WE HAVE CONVERGENCE OF EQUILIBRIUM ITERATIONS
        '
        'c  ... if augmented lagrange of spf determine if constraints are converged
        'c      if not redo equilibrium iterations with new rhs updates based on
        'c      constraints

110:    ' we made it to convergence

        If laugon Or lcspf <> 0 Then
            laugconv = False
            If (laugon) Then
                Dim msg9 = "ik02 call augmnt in sub quasin"
            Else
                laugconv = True
            End If

            If lcspf <> 0 And laugconv Then
                laugconv = False
                Dim msg10 = "ik02 call strcalc in sub quasin"

                If lpconv Then GoTo 117
                If ierr <> 0 Then Return
                Dim msg11 = "ik02 call ldmodspf in sub quasin"

                lpconv = True      ' this says the pressure has changed and ensures
                engco = engc        ' that a new tvc2 (du) will be calcd
                usimx = 0
                engmx = 0
                naug = 1
            End If

            If laugconv Then GoTo 117

            'For i = 0 To neql - 1      ' YC 102418
            For i = 1 To neql
                ui(i) = ui(i) - _step * tvc2(i)
                usi(i) = ui(i)
                tvc1(i) = u(i) + ui(i)
            Next

            laugconv = True      ' this says we have just made an augmentation
            If Not lpconv Then engc = engini
            ite = ite - 1
            rhs(1) = 0.0
            rhs(2) = 0.0
            rhs(3) = 0.0
            rhs(4) = 0.0
            lrattl = lratt0
            GoTo 30
        End If

117:

        iphase = 2
        dn2 = Math.Max(dn1, dn2)
        '      write(luo,130) tt,nstp1,itr,numrhs,iteref,rhsini,rhsc,rhsreq,
        '     &               engini,engc,engreq,dspini,dspc,dspreq
        Dim msg130 = String.Format(" Convergence summary: time={0}  step #{1}" + Environment.NewLine +
        "    iterations required            ={2}" + Environment.NewLine +
        "    right hand side evaluations    ={3}" + Environment.NewLine +
        "    stiffness matrix reformations  ={4}" + Environment.NewLine +
        "    convergence norms:   INITIAL       FINAL      REQUIRED" + Environment.NewLine +
        "      residual       {5}  {6}  {7}" + Environment.NewLine +
        "      energy         {8}  {9}  {10}" + Environment.NewLine +
        "      displacement   {11}  {12}  {13}", tt.ToString("e5"), nstp1.ToString("#####"),
            itr.ToString("###"), numrhs.ToString("###"), iteref.ToString("###"), rhsini.ToString("e5"),
            rhsc.ToString("e5"), rhsreq.ToString("e5"), engini.ToString("e5"), engc.ToString("e5"),
            engreq.ToString("e5"), dspini.ToString("e5"), dspc.ToString("e5"), dspreq.ToString("e5"))


        Dim msg230 = String.Format(" ------- converged at time ={0}", tt.ToString(" 0.0000E+00"))           ' YC 121219-1
        PrintLine(lutty)
        PrintLine(lutty, msg230)


        itrtot = itrtot + itr
        irftot = irftot + iteref
        irhtot = irhtot + numrhs
        itrlas = itr
        irflas = irflas + iteref
        irhlas = numrhs
        iteref = 0
        ncstep = ncstep + 1

        If lctmp Then
            'Write(luebe, 270) ncstep,rzero,rhsc   
            Dim msg270 = String.Format(" step {0} completed: initial residual = {1}" +
                        " final residual = {2}", ncstep.ToString("###"), rzero.ToString("e7"), rhsc.ToString("e7"))
        End If
        If ncstep <= 1600 Then
            avlitr(1, ncstep) = itr
            avlitr(2, ncstep) = numlit
        End If

        If rkapg > 0.0 Then
            If rhsini = 0 Then rhsini = 0.0000000001 * rkapg
            rkapg = 1 + rkapg / rhsini
        Else
            rkapg = 1.0
        End If
        Print(lutty, "")
        Return

991:
        ierr = 1
        Print(lutty, "")
        Return

    End Sub


End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine quasin(ui,usi,r,tvc2,u,tvc1,tt,ierr)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'	common/block02/bNIKE3D, bNike3dMsg
'	logical bNIKE3D, bNike3dMsg
'
'      character*4 mess
'	
'	common/block03/StopFEDFAA, FEDFAAStopped	! See WinYield.for
'	INTEGER(4) StopFEDFAA, FEDFAAStopped	! See WinYield.for !ikawa 09-09-03
'c
'C===> MODULE TO DRIVE QUASI-NEWTON ITERATION METHODS
'c
'c     bfgs method by matthies and strang ---  published in
'c     international journal for numerical methods in engineering
'c     volume 14, pages 1613-1626, 1979 and broyden's method
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk01/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n14a
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e,
'     & n20f,n20g
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09
'
'
'
'      common/bk06/l00,l00a,l00b,l00c,l01,l02,l03,l04,l05,l06,l07,l08,l09
'      common/bk07/l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l00d,l00e
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk12/dtx0,dt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
'      common/bk20/kpri,nstep,ite,ilimit,iref
'      logical ldns
'      common/bk21/rhsn,rhsvn,cvtl,ectl,tolls,rctl,ldns,men,maxref,iteref
'      common/bk26/ntlen,ntlenf
'      common/bk30/cpuio(72),cpuip(72)
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/bk35/numdc,numudc,nrcc     
'      common/bk36/mess
'      common/bk40/nbfgs,numupd
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      common/numrhs/numrhs
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye4/ icalls,numlit,maxlit,ncstep,avlitr(2,1600),ncunit  
'      common/bkneq/neql
'      logical laugon,laugconv,lpconv
'      common/aug1/deftal,ip1d1,ip1d2,ip1d3,ip1d4,ip1d5,
'     &                   nalsf,ipsf1,ipsf2,ipsf3,ipsf4,ipsf5,ipsf6,
'     &                   ipsw1,ipsw2,ipsw3,ipsw4,maxaug,laugon
'      common/total/itrlas,irflas,irhlas,itrtot,irftot,irhtot
'      common/bkspf1/etarg,emin,emax,ptime,ptmin,pdmax,lcspf,nmspf,tpelmc
'      logical lrattl,lratt0
'      common/bkratt/rhs(4),lrattl
'      common/autcst/dtold,rkapg,rkapg0,itrold
'      logical lplotm
'      common/automt/dtmin,dtmax,termtm,mxback,iauto,irfwin,lcmust,lplotm
'      logical lneghex
'      common/neghex/lneghex
'      dimension ui(*),usi(*),tvc2(*),r(*),u(*),tvc1(*)
'      logical lconvt,lctmp                                ! changed by QW 12-05-2013
'      common/gttcon/lconvt                                ! changed by QW 12-05-2013
'c      
'      common/nia/un1(54000),kht(54000)
'      common/a20/al20(3348000)
'
'	if (not (bNike3d)) then !ikawa 09/22/03
'      IF( StopFEDFAA .eq. -1 ) return
'	end if
'
'      tolfd = 1.e-32   ! temporary def. of r.o. tol. sqared
'      irnegchk = 1
'      rhsmx = rhsn
'      rhsc  = rhsvn
'      usimx=0.0
'      engmx=0.0
'      rkapg=0.0
'      lratt0=lrattl
'      nstp1=nstep+1
'      alfa=0.0
'      naug=1
'      ite=0
'      itr=0
'      nbfgs=0
'      iteref=0
'      numupd=0
'      rzero=0                                                          
'      numrhs=1
'      iphase=3
'      laugconv=.false.
'      lpconv  =.false.
'      if (numudc.ne.0) then
'       write(10,*) 'ik02 call zrhs in sub quasin'
'      end if
'
'      if (mthsol.eq.5) go to 10
'      call blkcpy(r,al20,neql)
'   10 engc=dotprd (r,ui)
'      engini=engc
'      onem = -1
'      do 20 i=1,neql
'      tvc2(i)=ui(i)
'   20 ui(i)=0.0
'      if (numudc.ne.0) then
'	 write(10,*) 'ik02 call swtche in sub quasin'
'	end if
'
'c______________________________________________________________________________
'
'c  ... BEGIN EQUILIBRIUM ITERATIONS
'
'c  ... havent converged yet so start a new iteration
'
'   30 ite=ite+1           ! count number of bfgs iters between reforms
'      itr=itr+1           ! count number of total iterations
'      step=1.0            ! line search step size
'
'c      if(sw7on)then
'        if(ite.eq.1 .and. iteref.eq.0) then
'	   write(lutty,170)nstp1
'
'	if (bNIKE3D) then
'        write(*,170)nstp1
'	end if
'
'
'	end if
'        if(ite.eq.1) then
'	   write(lutty,175)  
'
'	if (bNIKE3D) then
'        write(*,175)
'	end if
'
'	end if
'        write(lutty,180)ite                                             1
'
'	if (bNIKE3D) then
'        write(*,180)ite                                                 
'	end if
'
'c      endif
'
'	if (not (bNike3d)) then
'      FEDFAAStopped = 0 !ikawa 09-09-03
'      CALL WINYIELD( StopFEDFAA, FEDFAAStopped )
'      IF( StopFEDFAA .eq. -1 ) return
'	end if
'c______________________________________________________________________________
'
'c  ... DO RIGHT HAND SIDE CALCULATION BASED ON CURRENT DISPLACEMENT
'
'c  ... for full newton raphson we dont want to force 2 calls to asmrhs
'c      so we do assembly on this first call and skip the call below
'c      before factorization
'
'      lref=0
'      if(ilimit.eq.1) then  ! do reform here instead of below, set flags
'       iphase=4             ! force stiffness assembly in asmrhs and others
'c      ite=0                ! flag counting # iters between reforms reset
'       iref=0               ! flag counting down stiffness forms reset
'c       iesf=0               ! index for ooc storage of elem stiffness reset
'       nbfgs=0              ! number of bfgs vectors stored reset
'       numupd=0             ! reset # of bfgs updates so far
'       irfact=1             ! for spf
'       iteref=iteref+1      ! some models need to know this is the first reform
'       lref=1               ! flag set so all routines below know reform done
'       call blkcpy (kht,un1,neql)
'      endif
'
'c   ... do rhs calc to get error norms. (assembely done too if ilimit=1)
'
'      call asmrhs (ui,usi,tvc2,r,u,tvc1,tt,step,0,g,*992)
'
'      if (bNike3dMsg) then 
'      write(10,*) ' after call asmrhs'
'	end if
'      
'      iphase=3              ! reset to no reforms
'      iref=iref+lref        ! reset iref since reform hasnt happened yet
'      iteref=iteref-lref    ! reset iteref since it will be reincremented
'      irnegchk=0            ! i.e. no element inverted
'      if(lpconv.and.irfact.ne.1) then ! for spf algorithm
'        engct=dotprd (tvc1,r)
'        goto 40
'      endif
'c______________________________________________________________________________
'
'c   ... LINE SEARCH ?
'
'c   ... determine if line search needed based on energy norm
'
'      if(tolls.ge.1.e32) go to 40                  ! i.e. line search turned off
'      if (abs(g).le.(tolls*abs(engc)).or.engc*g.ge.0.0) go to 40
'       if (incflg.eq.0) then	 
'	  call lnesch(ui,usi,r,tvc2,u,tvc1,tt,step,g,engc,     
'     &                              rlnew,riksf,alfa,*991)   
'       end if
'
'      stepsv = step                                                     
'
'c    ... handle inverted elements by reforming
'  992 continue
'      if(lneghex) then
'       if(irnegchk.eq.0) then
'        irnegchk=1
'        step=-1             ! unincrement ui since not finished in asmrhs
'       else
'        goto 991            ! the last reform didnt help
'       endif
'      endif
'
'c    ... update displacement based on step from line search
'
'   40 do 50 i=1,neql
'      ui(i)=ui(i)+step*tvc2(i)
'      usi(i)=ui(i)
'   50 tvc1(i)=u(i)+ui(i)
'
'c    ... update quaternions
'c______________________________________________________________________________
'
'c  ... CALCULATE APPROPRIATE NORMS TO DETERMINE CONVERGENCE
'
'      if(irnegchk.eq.1) goto 60   ! element inverted go directly to reform
'
'      dn1 =dotprd(tvc1,tvc1)
'      dspc=dotprd(tvc2,tvc2)
'      usic=dotprd(usi,usi)
'      dinorm=sqrt(dspc)*step
'      rhsc=dotprd (r,r)
'c     rhsc = rhsc + rnormi                                              incmode
'      rkapg=max(rkapg,rhsc)
'
'      if(ite.eq.1.and.iteref.eq.0) then
'        rhsini=rhsmx
'        dspini=dspc
'      endif
'
'      usimx=max(usic,usimx)
'      engmx=max(abs(engc),engmx)
'
'      if (ldns) then
'      if(lpconv) then
'        engreq=min(ectl*abs(engct),abs(engco))
'        engmx =min(abs(engct),abs(engco)/ectl)
'        if(irfact.eq.1) lpconv=.false.   ! then we don't need to do extra newton step
'      else                               ! since we just did one
'        engreq=ectl*engmx
'      endif
'        dspreq=usic*cvtl
'        rhsreq=rctl*rhsmx
'      else
'        dspreq=dn1*cvtl
'        engreq=ectl*abs(engini)
'        rhsreq=rctl*rhsini
'      endif
'
'c  ... determine if problem is blowing up
'
'c     iengskip=1
'      if(itr.le.2) then            ! sometimes contact occurs in first or second
'       engblow=engmx               ! so intial energy norm too small
'      endif
'      if(iengskip.ne.1) then
'      if (abs(engc/(engblow+1.e-06)).gt.100000.0.and.engini.ne.0.and.
'     &    itr.gt.2) then        ! when itr = 1 sometimes contact occurred
'       if(iauto.ne.0) then      ! and we want to give the time step another
'        ierr=1                  ! chance
'        write(lutty,*) 'iteration = ',itr
'
'	if (bNIKE3D) then
'        write(*,*) 'iteration = ',itr
'	end if
'
'
'        write(lutty,300)
'
'	if (bNIKE3D) then
'        write(*,300)
'	end if
'
'        write(luo,300)
'        return
'       endif
'      endif
'      endif
'
'c  ... check to see if force norm increasing: if so reform
'c      men = 1 will preclude reforms except when equilib iteration
'c      limits reached
'
'      if (men.eq.1) then
'        rhst = 2*rhsc
'      else
'        rhst = rhsmx
'      endif
'      irfact=0
'
'c  ... deterimine if reform necessary, if not check for convergence
'
'      if (rhsc.le.rhst.or.abs(rhsc).lt.tolfd) go to 70 ! check for conv at 70
'      rhsmx=rhsc
'      write(luo,160) ite     
'
'	if (bNIKE3D) then
'        write(*,190)
'	end if
'
'c	end if
'
'c______________________________________________________________________________
'
'c  ... DO NEW STIFFNESS MATRIX ASSEMBLY AND REFORM IF NECESSARY
'
'   60 continue
'      iteref=iteref+1
'      irat=iteref - 4*( (iteref-1)/4 )
'      rhs(irat)=rhsc
'c ... maxref < 0 means do only maxref equilib iterations and go to next step
'
'
'      !second exit
'
'	if (iteref.gt.14) go to 110 !ikawa September 20, 2005
'
'      if (iteref.gt.abs(maxref).and.maxref.gt.0) then
'	if (bNIKE3D) then
'        write(*,220)
'	end if
'
'c	end if
'       ierr=1
'       return
'      elseif (maxref.lt.0.and.iteref.eq.abs(maxref)) then
'       goto 110
'      endif
'
'      write(luo,150)
'
'      if (lrattl .and. (abs(rhsc).lt.100.*abs(rhsreq))) then
'        lrattl=.false.
'        write(luo,280)
'	endif
'      ite=0
'      iref=0
'      nbfgs=0
'      numupd=0
'      iphase=4
'      irfact=1
'      if(lref.eq.1.and.step.eq.1) goto 65
'      call blkcpy (kht,un1,neql)    
'      call asmrhs (ui,usi,r,r,u,tvc1,tt,step,1,g,*991)
'   65 continue
'      call blkcpy(r,al20,neql)
'      call blkcpy (r,tvc2,neql)
'      call bsolvr (tvc2,2,8)
'      engc=dotprd (tvc2,r)
'      iref=1
'      iphase=3
'      go to 30
'
'c______________________________________________________________________________
'
'c   ... DETERMINE IF ITERATIONS HAVE CONVERGED
'
'     
'   70 if (iteref.gt.14) go to 110 !ikawa September 20, 2005
'
'	if(abs(rhsc).lt.tolfd.and.dspc.lt.tolfd) go to 110 ! i.e. machine tol.
'      if (lpconv) then                          ! force a newton calc after
'       lpconv=.false.                           ! a spf increment to get a
'       goto 80                                  ! delta u
'      endif
'      if (maxref.lt.0.and.itr.eq.abs(maxref)) go to 110
'      if (dspc.gt.dspreq) go to 80
'      if (abs(engc).gt.engreq) go to 80
'      if (abs(rhsc).gt.abs(rhsreq)) goto 80
'
'      go to 110                                 ! ITERATION HAS CONVERGED !
'
'   80 if (ite.lt.ilimit .and. step.ge.0.001) then
'        go to 90
'      elseif (ite.ge.ilimit) then
'        write(luo,140)
'      elseif (step.lt.0.001) then
'        write(luo,145)
'      endif
'      go to 60                ! do reform
'c______________________________________________________________________________
'
'c   ... DO QUASI-NEWTON ITERATION INSTEAD OF NEW REFORM
'
'   90 if (mthsol.eq.1) call bfgs (tvc2,r,tvc1,step,g,engc,dinorm)
'      if (mthsol.eq.2) then
'
'       write(10,*) 'ik02 call broy in sub quasin'
'	end if
'
'      if (mthsol.eq.3) then
'
'       write(10,*) 'ik02 call dfp in sub quasin' 
'	 end if
'
'      if (mthsol.eq.4) then
'
'       write(10,*) 'ik02 call davd in sub quasin'
'
'	 end if
'      if (mthsol.eq.5) then
'
'      write(10,*) 'ik02 call modn in sub quasin'
'      end if
'
'      if (mthsol.eq.5) go to 100
'
'      call blkcpy(al20(1+nbfgs),r,neql)
'  100 engc=dotprd(tvc2,r)
'      go to 30                ! go back up to get rhs and check convergence
'c______________________________________________________________________________
'
'c   ... WE HAVE CONVERGENCE OF EQUILIBRIUM ITERATIONS
'
'c  ... if augmented lagrange of spf determine if constraints are converged
'c      if not redo equilibrium iterations with new rhs updates based on
'c      constraints
'
'  110 continue      ! we made it to convergence
'
'      if (laugon .or.lcspf.ne.0) then
'       laugconv=.false.
'
'       if (laugon) then
'        write(10,*) 'ik02 call augmnt in sub quasin'
'
'       else
'        laugconv=.true.
'       endif
'
'       if ( lcspf.ne.0.and.laugconv) then
'          laugconv=.false.
'           write(10,*) 'ik02 call strcalc in sub quasin'
'
'          if(lpconv) goto 117
'          if(ierr.ne.0) return
'          write(10,*) 'ik02 call ldmodspf in sub quasin'
'
'          lpconv=.true.     ! this says the pressure has changed and ensures
'          engco=engc        ! that a new tvc2 (du) will be calcd
'          usimx=0
'          engmx=0
'          naug=1
'       endif
'
'        if (laugconv) goto 117
'
'        do 115 i=1,neql
'        ui(i)=ui(i)-step*tvc2(i)
'        usi(i)=ui(i)
'        tvc1(i)=u(i)+ui(i)
'  115   continue
'c
'        laugconv=.true.     ! this says we have just made an augmentation
'        if(.not.lpconv) engc=engini
'        ite=ite-1
'        rhs(1)=0.
'        rhs(2)=0.
'        rhs(3)=0.
'        rhs(4)=0.
'        lrattl=lratt0
'        goto 30
'      endif
'c
'  117 continue
'
'      iphase=2
'      dn2=max(dn1,dn2)
'      write(luo,130) tt,nstp1,itr,numrhs,iteref,rhsini,rhsc,rhsreq,
'     &               engini,engc,engreq,dspini,dspc,dspreq
'      itrtot=itrtot+itr
'      irftot=irftot+iteref
'      irhtot=irhtot+numrhs
'      itrlas=itr
'      irflas=irflas+iteref
'      irhlas=numrhs
'      iteref=0
'      ncstep=ncstep+1                                                   r_m_f
'
'       if (lctmp) write(luebe,270) ncstep,rzero,rhsc   ! changed by QW 12-05-2013
'      if (ncstep.le.1600) then
'        avlitr(1,ncstep) = float(itr)                                   r_m_f
'        avlitr(2,ncstep) = float(numlit)                                r_m_f
'      endif
'c
'      if(rkapg.gt.0.0) then
'        if(rhsini.eq.0) rhsini = 1.e-10*rkapg
'         rkapg=1.+rkapg/rhsini
'      else
'        rkapg=1.0
'      endif
'
'      return
'c
'  991 continue
'      ierr=1
'      return
'c
'  120 format(/' Nonlinear solution status: time=',1pe12.4,'  step #',i5,
'     &       '  iteration #',i3
'     &       /'    stiffness updates              =',i3
'     &       /'    right hand side evaluations    =',i3
'     &       /'    stiffness  matrix reformations =',i3
'     &       /'    step size from line search     =',1pe12.4
'     &       /'    convergence norms:   INITIAL      CURRENT     ',
'     &        ' REQUIRED'
'     &       /'      residual       ',1pe12.4,2x,1pe12.4,2x,1pe12.4,
'     &       /'      energy         ',1pe12.4,2x,1pe12.4,2x,1pe12.4,
'     &       /'      displacement   ',1pe12.4,2x,1pe12.4,2x,1pe12.4)
'  121 format( '    residual norm squared                  =',1pe12.4    debug
'     &       /'    incomp contribution to res norm        =',1pe12.4)   debug
'  130 format(/' Convergence summary: time=',1pe12.4,'  step #',i5
'     &       /'    iterations required            =',i3
'     &       /'    right hand side evaluations    =',i3
'     &       /'    stiffness matrix reformations  =',i3
'     &       /'    convergence norms:   INITIAL       FINAL      ',
'     &        ' REQUIRED'
'     &       /'      residual       ',1pe12.4,2x,1pe12.4,2x,1pe12.4,
'     &       /'      energy         ',1pe12.4,2x,1pe12.4,2x,1pe12.4,
'     &       /'      displacement   ',1pe12.4,2x,1pe12.4,2x,1pe12.4)
'  140 format(///' iteration limit reached maximum permitted ')
'  145 format(///' line search stepsize zero ')
'  150 format(///' stiffness matrix will now be reformed ')
'  160 format(///' residual load vector larger than incremental loads'
'     1,' after iteration ',i5)
'  170 format(/' ======= beginning time step #',i4,' =======')
'
'  175 format(' iterating:',$)                                           1
'  180 format(1x,i2,$)                                                   1
'  190 format(/'    iteration process diverging . . . . .',$)            1
'  200 format(/'    limit reached without converging. . .',$)            1
'  205 format(/'    line search stepsize zero . . . . . .',$)            1
'  210 format(' commence reformation #',i2)                              1
'  230 format(/' ------- converged at time=',1pe11.4)                    1
'  220 format(/' *** reformation limit reached without converging ***')
'  270 format(' step ',i3,' completed: initial residual = ',1pe15.6,    
'     &       ' final residual = ',1pe15.6,/,' ')                       
'  280 format(45x,'rattle flag "off"')
'  290 format(/
'     &5x,'*******************************************************',/
'     &5x,'*                   - WARNING -                       *',/
'     &5x,'*    User halted nonlinear equilibrium search.        *',/
'     &5x,'*  Results at time =',1pe11.4,' may be inaccurate     *',/
'     &5x,'*******************************************************')
'  300 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' *          Energy norm growing unbounded              *',/
'     $' *          during equilibrium iteration               *',/
'     &' *******************************************************')
'      end

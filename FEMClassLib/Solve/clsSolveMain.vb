'This file contains all the methods of solve.f
Partial Public Class clsSolve


    ' addd by YC 092018
    Private objInit As New clsInitial
    Private objNik3d As New clsInput
    Private objComsub As New clsCom
    Private objPrintout As New clsPrintOut

    Public lutty As Integer
    Public a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, delt, alfa, ectl, cvtl, rctl, tolls As Double ' QW 12-12-2018
    Public un1(), un2(), un3(), un4(), un5(), un6(), tvc1(), tvc2(), spring_stiff(,) As Double  ' QW 12-12-2018
    Public abdg(,), anegb1(), anegb2(), hxdata1(,), sfdata1(,), gass(,), dehav(,) As Double ' QW 12-12-2018
    Public numelh, neehx, ksizhx, iprec, numelf, nh04, nh05, ilimit, npb, ipnode(,) As Integer  ' QW 12-12-2018
    Public idid(), lcd(), idflad(), hxdata2(,), sfdata2(,), is_nodes(,), is_dofs(,) As Integer    ' QW 12-12-2018
    Public nrttlm, nrttls, mbsize, nsnode, neesf, ksizsf, nrtm, nrts, nmn, nsn, nty, nst, mst As Integer
    Public xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114 As Double
    Public xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p, xs, ys, zs As Double
    Public xx1(20), xx2(20), xx3(20), hh(4) As Double
    Public iblock(), ipoint(), ireord(), idroer() As Integer


    Private irtls(), irtlm() As Integer
    'Public avlitr(,) As Double
    Public amad() As Double
    ' from clsCom
    Public StopFEDFAA As Short
    Public stress1(), stress8() As Double
    Public ntime, istep As Integer

    ' from clsInitial and clsInput
    Public neql, numlit, numspu, mthunl, maxsiz, iadd As Integer

    Public SF1 As Integer

    Public iauto, irfreq, jrfreq, nstep, mthsol, ipri, jpri, kpri, ifl, icnt1, icnt2, incpr As Integer

    Public riksf, time, termtm As Double

    'Private cpuio(71), cpuip(71) As Double 'YC 092018
    Public cpuio(3, 24), cpuip(3, 24) As Double
    Public h8(8, 9), pr(8, 9), ps(8, 9), pt(8, 9), hix(8, 9), hiy(8, 9), hiz(8, 9) As Double
    Public bNike3dMsg, stsion As Boolean
    'Public d1(64), d2(64), d3(64), d4(64), d5(64), d6(64) As Double
    Public p(), fdat(,) As Double
    Public npc(), iaug(), iseg(,), nsf(,), iparm(,), incomp(), ncon() As Integer
    Public irow(9), icol(9), igrad(9) As Integer
    Public id(,), ix(,), ixx(9), iyy(9) As Integer
    Public ym(8, 64), ro(64) As Double
    ' to be used in clsSolve
    Private mess As String
    Private n1, n2, n3, ss, tt As Double
    Private lpri, mpri, npri, krfreq, iress, jrfrec, iref, irflas As Integer



    Private iterp1, iterp2, irftot, ncstep As Integer 'need initiation  by YC?
    Private sltol As Double

    Private lplotm, llpass, lfpass, lresf, lchg As Boolean

    Private timep, dt, dn2 As Double

    Private xls(), xlm(), avlitr(,) As Double
    ' addd by YC 092018 END


    Dim un40() As Double ' YC 052019-5-120219


    ''' <summary>
    ''' drive the solution process
    ''' </summary>
    Public Sub solve(ByVal ini As Object, ByVal istep As Integer, ByRef Stress1() As Double, ByRef Stress8() As Double, ByVal ModelOut As Integer)
        'istep = istep0
        Call ParaTransfer(ini)
        Dim time1 = ""
        Dim msg = ""
        Dim term, ipris, lrron, lauto, lretry, lplot As Boolean
        neehx = 24 : ksizhx = 300 ' QW 12-12-2018
        ReDim un1(neql), un2(neql), un3(neql), un4(neql), un5(neql), un6(neql), tvc1(neql), tvc2(neql)    ' QW 12-12-2018

        ReDim un40(neql)    'YC 052019-5-120219

        ReDim abdg(21, numnp), hxdata1(ksizhx, numelh + 64), hxdata2(neehx, numelh + 64), sfdata1(ksizsf, nsnode), sfdata2(neesf, nsnode)   ' QW 12-12-2018- (151= 121+15*2) (ksizsf+neesf * iprec)
        'ReDim stress1(80), stress8(80)
        ReDim rf(numpc), is_nodes(2, nmelde), is_dofs(6, nmelde), spring_stiff(21, nmelde), avlitr(2, 1600) ' QW 12-12-2018-
        ReDim iseg(2, nsntl + nmntl), fdat(10, nsntl + nmntl)
        ReDim xls(9 * nsntl), xlm(9 * nmntl)
        termtm = dt * ntime             ' QW 12-12-2018-
        StopFEDFAA = SF1
        time = 0.0 : llpass = False                  ' QW 12-12-2018-
        Call objComsub.timing(cpuio, cpuip, 20, 2)
        ReDim irtlm1(irtlm.Length - 1), irtls1(irtls.Length - 1)
        Dim I As Integer
        For I = 1 To irtlm.Length - 1
            irtlm1(I) = irtlm(I)
        Next
        For I = 1 To irtls.Length - 1
            irtls1(I) = irtls(I)
        Next
        lauto = iauto <> 0
        term = False
        ipris = False
        lretry = False
        mess = "    "
        Dim iterm = 0
        lrron = False

        If jrfreq > 0 OrElse lauto Then lrron = True
        'c  ... write new dump file to store info from restart input

        If nstep > 0 Then
            If lrron Then

            End If
            GoTo 130
        End If
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

        ipris = True
        If ipri < 0 Then
            ipri = -ipri
            ipris = False
        End If
        npri = jpri + nstep
        lpri = ipri + nstep

100:    nstep = nstep + 1

        'TODO - Missing Sub
        Call get_time(time1)
        If bNike3dMsg Then
            msg += " nstep=" + nstep.ToString()
        End If

        Dim nbcku = 0

        krfreq = krfreq + 1
        If irfreq <= krfreq Then krfreq = 0
        iress = 1

        If lrron Then
            jrfrec = jrfrec + 1
            If jrfrec >= jrfreq Then
                jrfrec = 0
                msg += "ik02 call rndmp 2 in sub solve"
            End If
        End If

        If mess = "sw1." Then GoTo 110
        If Not lauto AndAlso nstep <> ntime AndAlso krfreq <> 0 AndAlso mess <> "sw4." Then GoTo 120
        If lauto AndAlso time < termtm AndAlso krfreq <> 0 AndAlso mess <> "sw4." Then GoTo 120
        If mthsol > 5 AndAlso iterm = 0 AndAlso krfreq <> 0 AndAlso mess <> "sw4." Then GoTo 120
        iress = 0
        If irfreq < 0 Then GoTo 120
110:    msg += "ik02 call dump in sub solve"

120:
        'If kpri < 0 Then Call objComsub.header(lutty) 'YC? 092018

        'c ...proceed to new time step

125:    If mess <> "sw5." Then GoTo 130
        Dim dtnew As Double
        Dim msg210 = " type desired time step (e10.3)" 'TODO write what in e10.3 format?
        dtnew = Double.Parse(Console.ReadLine()) ' TODO handle input here
        If dtnew = 0.0 OrElse dtnew = dt Then GoTo 130
        dt = dtnew
        Call chgint()
        iterp1 = icnt1 - 1

        'c   ... enforce must points and termination time ...
130:    lplot = lplotm
        lplotm = False

        Call mustpt(p, npc, time, term, lauto)

        If mthsol < 6 OrElse mthsol > 13 Then
            timep = time
            time = time + dt
        ElseIf mthsol >= 6 AndAlso mthsol < 13 Then
            If nstep = 0 Then time = riksf
        End If

        Dim nstp1 = nstep + 1

        mess = "    "

        iterp1 = iterp1 + 1
        iterp2 = iterp2 + 1
        iref = icnt1 - iterp1
        Dim iter = icnt2 - iterp2
        If iref = 0 Then iterp1 = 0
        If nstep <= 0 Then iref = 0
        If iter = 0 Then iterp2 = 0
        irflas = 0
        'c  ... initialize rotation values for time step one
        '
        If ntime = 0 Then llpass = True ' anal check => ntime=0
        If lauto AndAlso termtm = 0 Then llpass = True ' anal check => termtm=0
        If mthsol < 6 OrElse mthsol > 12 Then
            If lauto AndAlso time > termtm Then llpass = True ' anal w/ auto  done
            If Not lauto AndAlso nstep = ntime Then llpass = True ' anal w/ fixed dt done
        End If
        If iterm >= 1 Then llpass = True

        Call get_time(time1)
        If bNike3dMsg Then
            msg += " before call elstf"
        End If
        Dim newtext As String
        'newtext = vbCrLf & "before form element stiffness matrices time = " & time1 & vbCrLf
        'Print(lutty, newtext)
        'c  ...  compute element stiffness matrices and rhs vector
        'Call Check1D(un3, neql, istep)
        Call elstf()


        '  YC 052019-3-110419
        If Not llpass Then
            Dim msg170 = String.Format(" ======= beginning time step #{0} =======", nstp1.ToString("####"))
            PrintLine(lutty)
            PrintLine(lutty, msg170)

            Dim msg175 = " iterating:"
            PrintLine(lutty, msg175)    ' YC 102418-012819

            Print(lutty, "0")

            If Math.Abs(rhsn - rhsc) <= 10 ^ -6 Then

                Call blkcpy(un4, un40, neql) 'YC 052019-5-120219

                Call azero(un3, neql)
                Call azero(un4, neql)

                Call azero(usic, 9, nelpg)
                Call azero(dusic, 9, nelpg)
                Call azero(dic, 9, nelpg)


                ' YC 121219-1
                Dim msg230 = String.Format(" ------- converged at time ={0}", time.ToString(" 0.0000E+00"))
                PrintLine(lutty)
                PrintLine(lutty, msg230)
                ' YC 121219-1 END


                GoTo 135
            End If

        End If
        ' YC 052019-3-110419 END



        Call get_time(time1)
        'newtext = vbCrLf & "after form element stiffness matrices time = " & time1 & vbCrLf
        'Print(lutty, newtext)

        If bNike3dMsg Then
            msg += " after call elstf"
        End If

        If llpass Then GoTo 140
        If ntime = 0 Then GoTo 140

        lfpass = False           ' first pass into elstf made

        Dim one = 1.0
        'Call Check1D(un3, neql, 1)
        'TODO - Missing Sub
        Call blkcpy(un3, un5, neql)
        Call bsolvr(un3, iref + 2, 6)
        Call get_time(time1)
        'newtext = vbCrLf & "after solve element stiffness matrices time = " & time1 & vbCrLf
        'Print(lutty, newtext)

        'Call Check1D(un3, neql, istep)

        'Call blkcpy(un3, un4, neql)
        ' Call Check1D(un3, neql)
        If bNike3dMsg Then
            msg += " after call bsolvr"
        End If

        If iref = 0 Then
            irftot = irftot + 1
            irflas = 1
        End If

        'c   ... skip equilibrium iterations? ...

        If iter > 0 Then
            ncstep = ncstep + 1
            If ncstep < 1600 Then
                avlitr(0, ncstep) = 0.0
                avlitr(1, ncstep) = CDbl(numlit)
            End If

            GoTo 140
        End If

        'c______________________________________________________________________________
        '
        'c   DO EQUILIBRIUM ITERATIONS   (for time n+1)

        Call objComsub.timing(cpuio, cpuip, 20, 3)
        Call objComsub.timing(cpuio, cpuip, 7, 2)
        dn2 = 0.0

        Dim ierr = 0

        'c       ... bfgs or full newton method
        '
        Dim t1, t2 As String
        'Call get_time(t1)
        'newtext = vbCrLf & "before quasin time = " & t1 & vbCrLf
        'Print(lutty, newtext)
        Call quasin(un3, un4, un5, tvc2, un2, un6, time, ierr)            ' QW 11-11-2015
        'Call get_time(t2)
        'newtext = vbCrLf & "after quasin time = " & t2 & vbCrLf
        'Print(lutty, newtext)

        'Call CheckName("after quasin")

        If (bNike3dMsg) Then
            msg += "after call quasin"
        End If


135:    ' YC 052019-3-110419

        lresf = False
        'TODO - Missing Sub
        Call auto(ierr, nbcku, iterm, lretry)     ' auto timestepping ?
        If (lretry) Then lresf = True ' failed time step, retry?

        'c       ... full newton arc lenght methods

        'c
        Call objComsub.timing(cpuio, cpuip, 7, 3)
        Call objComsub.timing(cpuio, cpuip, 20, 2)

        If lretry Then                           ' retry time step bfgs
            term = False
            GoTo 125
        End If

        'c   OUTPUT RESULTS  (for time n)
        '
        'c   ... write high speed printer file ... 
        '
140:
        lpri = lpri + 1

        Call get_time(time1)
        If bNike3dMsg Then
            msg += time1 + " OUTPUT RESULTS" + lpri.ToString()
        End If

        'newtext = vbCrLf & "start output time = " & time1 & vbCrLf
        'Print(lutty, newtext)

        kpri = ipri - lpri
        If kpri > 0 Then GoTo 150
        If ipris Then
            ipris = False
            lpri = 0
            kpri = 1
            GoTo 150
        End If
        Call objComsub.timing(cpuio, cpuip, 20, 3)
        Call objComsub.timing(cpuio, cpuip, 9, 2)

        'Call prtout()  ' YC 092018
        'Call objPrintout.prtout(FilenameOnly, kpri, mpri, npri, iadd, maxsiz, Stress1, Stress8, incflg, lhex, numelg, ihv,
        '                        nh04, nh05, nstep, idroer, ncon, anegb1, anegb2, incpr, dehva, timep, npb, ipnode, un2, ModelOut) ' QW 12-12-2018- ' YC 121219-2
        Call objPrintout.prtout(FilenameOnly, kpri, mpri, npri, iadd, maxsiz, Stress1, Stress8, incflg, lhex, numelg, ihv,
                                nh04, nh05, nstep, idroer, ncon, anegb1, anegb2, incpr, dehva, timep, npb, ipnode, un2, ModelOut, temo)

        Call objComsub.timing(cpuio, cpuip, 9, 3)
        Call objComsub.timing(cpuio, cpuip, 20, 2)
        If nstep = ntime Then GoTo 170
        'c   ... write plot data into binary file ...
150:    npri = npri + 1
        mpri = jpri - npri
        If stsion AndAlso iterm = 1 Then mpri = 0
        If (iterm = 10 OrElse lplot) Then mpri = 0
        If (mpri > 0) Then GoTo 170
        Call objComsub.timing(cpuio, cpuip, 20, 3)
        Call objComsub.timing(cpuio, cpuip, 10, 2)

        'Call prtout()  ' YC 092018
        'Call objPrintout.prtout(FilenameOnly, kpri, mpri, npri, iadd, maxsiz, Stress1, Stress8, incflg, lhex, numelg, ihv,
        '                        nh04, nh05, nstep, idroer, ncon, anegb1, anegb2, incpr, dehva, timep, npb, ipnode, un2, ModelOut) ' QW 12-12-2018-  YC 121219-2
        Call objPrintout.prtout(FilenameOnly, kpri, mpri, npri, iadd, maxsiz, Stress1, Stress8, incflg, lhex, numelg, ihv,
                                nh04, nh05, nstep, idroer, ncon, anegb1, anegb2, incpr, dehva, timep, npb, ipnode, un2, ModelOut, temo)

        Call objComsub.timing(cpuio, cpuip, 10, 3)
        Call objComsub.timing(cpuio, cpuip, 20, 2)
        'c______________________________________________________________________________

        'c UPDATE DISPLACEMENT VARIABLES ETC. TO TIME N+1 VALUES
        '
        Call get_time(time1)
        If bNike3dMsg Then
            msg += time1 + " UPDATE DISPLACEMENT VARIABLES"
        End If

170:    Call update(un2, un3, lauto, neql)

        If lchg Then
            Call chgint()
        End If

        'c______________________________________________________________________________
        '
        'c  DETERMINE IF YOU ARE FINISHED
        'newtext = vbCrLf & "determine if finish time = " & time1 & vbCrLf
        'Print(lutty, newtext)
        If mthsol < 6 OrElse mthsol > 12 Then
            ' If iterm = 2 OrElse iterm = 10 Then Call objComsub.adios(2)
            If iterm = 3 Then
                mess = "sw4."
                iterm = 10
                GoTo 100
            End If
            If llpass Then GoTo 300
            If lauto AndAlso time <= termtm Then GoTo 100
            If Not lauto AndAlso nstep < ntime Then GoTo 100
        Else
            If llpass Then GoTo 300
            If lauto AndAlso time <= termtm Then GoTo 100
            If Not lauto AndAlso nstep < ntime Then GoTo 100
            If numspu > 0 Then
                nstep = 0
                ntime = numspu + 1
                dt = -time / numspu
                dt = dt - 0.0000000001 * dt
                mthsol = mthunl
                sltol = 0.0
                GoTo 100
            End If
            iterm = iterm + 1
            If iterm = 1 Then GoTo 100
        End If
        '
        'c NKC 8/9/99
300:    ' Call objComsub.adios(1) 'congratulations your done
        '
        '
        clsCom.stress1 = Stress1
        clsCom.stress8 = Stress8
        'newtext = vbCrLf & "end of solve time = " & time1 & vbCrLf
        'Print(lutty, newtext)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine solve
'c
'
'      implicit double precision (a-h,o-z)                             
'
'c
'c===> module to drive the solution process
'c
'
'	character*8 time1 !ik00
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      character*4 mess
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
'      common/bk12/dtx0,dt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh
'      common/bk14/ifil,iadd,maxsiz
'      common/bk15/numpc
'      common/bk16/nlcur,nptst,nload,nptm
'      real*8 hed                                                     
'      common/bk19/hed(12)
'      common/bk20/kpri,nstep,ite,ilimit,iref
'      logical ldns
'      common/bk21/rhsn,rhsvn,cvtl,ectl,tolls,rctl,ldns,men,maxref,iteref
'      common/bk22/npb,ipnode(2,8)
'      common/bk24/cnwmk(2),icnt2,ipri,icnt1
'      common/bk26/ntlen,ntlenf
'c      
'      common/bk29/tim(7),iterp1,iterp2,time,timep,lpri,npri
'      common/bk30/cpuio(72),cpuip(72)
'      common/bk33/irfreq,krfreq,iress,jrfreq,jrfrec
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/bk35/numdc,numudc,nrcc
'      logical stsion
'      common/stsi/iaddst,nnelst,lcsts,stsion
'      logical lfpass,llpass
'      common/ndinit/lfpass,llpass
'      common/bk36/mess
'      common/fupdtc/ifl
'      common/riksw2/rlnew,alfa0,dsx,iteopt,idctrl,riksf,numspu,mthunl
'      common/ebye4/ icalls,numlit,maxlit,ncstep,avlitr(2,1600),ncunit  
'      common/bkneq/neql
'
'      logical lplotm
'      common/automt/dtmin,dtmax,termtm,mxback,iauto,irfwin,lcmust,lplotm
'      logical lchg
'      common/autchgnt/lchg
'      logical laugon
'      common/aug1/deftal,ip1d1,ip1d2,ip1d3,ip1d4,ip1d5,
'     &                   nalsf,ipsf1,ipsf2,ipsf3,ipsf4,ipsf5,ipsf6,
'     &                   ipsw1,ipsw2,ipsw3,ipsw4,maxaug,laugon
'      common/total/itrlas,irflas,irhlas,itrtot,irftot,irhtot
'      logical lresf            ! flag that auto time step restart is in effect
'      common/resf/lresf
'      common/bk37/numsv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
'           !     and time step retried with new dt
'   
'c      
'      common/hx/hxdata(10000000),sfdata(1000000),sftemp(10000)         ! QW 11-11-2015
'	common/block02/bNIKE3D, bNike3dMsg
'	logical bNIKE3D, bNike3dMsg
'      
'	common/passFEDFAA/SF1
'	INTEGER(4) SF1
'      common/ini/npc(25),p(480),cmde(8,24),ixde(3,500),sclf(500),
'     & dehv(11,500),dehva(11,500),fval(48),mtypde(6)                  ! QW 11-11-2015
'      common/nib/un2(54000),un3(54000),un4(54000),un5(54000),
'     & un6(54000),tvc2(54000)
'      common/iread/idp(54000),x(27000)                                 ! QW 11-11-2015
'      common/l18/iseg(2,2000),fdat(10,2000)                           ! QW 11-11-2015
'      common/ipsf/xgn(200),xls(12000),xlm(12000)                      ! QW 11-11-2015
'      common/inl/iparm(7,50),fric(3,50),pend(50),iaug(50),
'     & altol(2,50),sfact(50),tdeath(50),tbury(50),ifd(50)
'      common/inn/irects(8000),irectm(8000),nsf(2,50),
'     & nsv(8000),msr(8000),nsegs(8000),nsegm(8000)
'      common/aa/anegb1(7508928),anegb2(7508928)
'	common/block03/StopFEDFAA, FEDFAAStopped	! See WinYield.for
'	INTEGER(4) StopFEDFAA, FEDFAAStopped	! See WinYield.for !ikawa 09-09-03
'
'      logical term,ipris,lrron,lauto,lretry,lplot
'
'c 
'      StopFEDFAA = SF1
'    
'      call timing(cpuio,cpuip,20,2)
'
'      lauto=(iauto.ne.0)
'      term  =.false.
'      ipris=.false.
'      lretry=.false.
'      mess  ='    '
'      iterm =0
'      lrron=.false.
'
'      if (jrfreq.gt.0.or.lauto) lrron=.true.
'c  ... write new dump file to store info from restart input
'      if(nstep.ge.0) then
'       if(lrron) then
'
'	end if
'       go to 130
'      endif
'      if (ifl.ne.0) then
'
'       call fupdt(iseg,fdat,nsf,iaug,xls,xlm,iparm)
'
'	 end if
'c
'      ipris=.true.
'      if(ipri.lt.0) then
'         ipri=-ipri
'         ipris=.false.
'      endif
'      npri=jpri+nstep
'      lpri=ipri+nstep
'c
'  100 nstep=nstep + 1
'
'	call get_time(time1)
'	if (bNike3dMsg) then 
'      write(10,*) time1, ' nstep=',nstep
'	end if
'
'      nbcku=0
'c
'      krfreq=krfreq+1
'      if(irfreq.le.krfreq) krfreq=0
'      iress=1
'c
'      if (lrron) then
'        jrfrec=jrfrec+1
'        if (jrfrec .ge. jrfreq) then
'           jrfrec = 0
'
'        write(10,*) 'ik02 call rndmp 2 in sub solve'
'        endif
'      endif
'c
'      if(mess.eq.'sw1.') go to 110
'      if ( (.not.lauto)  .and. (nstep.ne.ntime) .and.
'     &     (krfreq.ne.0) .and. (mess.ne.'sw4.'))      go to 120
'      if (    lauto      .and. (time.lt.termtm) .and.
'     &     (krfreq.ne.0) .and. (mess.ne.'sw4.'))      go to 120
'      if ( (mthsol.gt.5) .and. (iterm.eq.0)     .and.
'     &     (krfreq.ne.0) .and. (mess.ne.'sw4.'))      go to 120
'      iress=0
'      if(irfreq.lt.0) go to 120
'  110 write(10,*) 'ik02 call dump in sub solve'
'
'c
'  120 continue
'      if(kpri.lt.0) call header
'c
'c ...proceed to new time step
'c
'  125 if(mess.ne.'sw5.') go to 130
'      write(lutty,210)
'
'      read (*,230) dtnew                                                1
'
'      if((dtnew.eq.0.).or.(dtnew.eq.dt)) go to 130
'      dt=dtnew
'      call chgint
'      iterp1=icnt1-1
'c
'c   ... enforce must points and termination time ...
'  130 lplot=lplotm
'      lplotm=.false.
'
'      call mustpt(p,npc,time,term,lauto)
'c
'      if(mthsol.lt.6.or.mthsol.ge.13) then
'       timep=time
'       time =time+dt
'      elseif (mthsol.ge.6.and.mthsol.lt.13) then
'       if(nstep.eq.0) time=riksf
'      endif
'c
'      nstp1=nstep+1
'
'      mess='    '
'c
'      iterp1=iterp1+1
'      iterp2=iterp2+1
'      iref=icnt1-iterp1
'      iter=icnt2-iterp2
'      if (iref.eq.0) iterp1=0
'      if (nstep.le.0) iref=0
'      if (iter.eq.0) iterp2=0
'      irflas=0
'c  ... initialize rotation values for time step one
'
'         if (ntime.eq.0) llpass=.true.                 ! anal check => ntime=0
'         if (lauto .and. termtm.eq.0) llpass=.true.    ! anal check => termtm=0
'      if (mthsol.lt.6.or.mthsol.gt.12) then
'         if (  lauto   .and.time.gt.termtm) llpass=.true. ! anal w/ auto  done
'         if (.not.lauto.and.nstep.eq.ntime) llpass=.true. ! anal w/ fixed dt done
'      endif
'      if (iterm.ge.1) llpass=.true.
'
'	call get_time(time1)
'	if (bNike3dMsg) then 
'      write(10,*) time1, ' before call elstf'
'	end if
'
'c  ...  compute element stiffness matrices and rhs vector
'         
'      call elstf
'      
'	call get_time(time1)
'	if (bNike3dMsg) then 
'      write(10,*) time1, ' after call elstf'
'	end if
'
'      if(llpass) goto 140
'      if (ntime.eq.0) goto 140
'
'      lfpass=.false.           ! first pass into elstf made
'
'      one = 1.0
'      
'      call blkcpy(un3,un5,neql)
'      call bsolvr(un3,iref+2,6)
'      call blkcpy(un3,un4,neql)
'
'      if (bNike3dMsg) then 
'      write(10,*) ' after call bsolvr'
'	end if
'      
'      if (iref.eq.0) then
'        irftot=irftot+1
'        irflas=1
'      endif
'
'c   ... skip equilibrium iterations? ...
'
'      if (iter.gt.0) then
'        ncstep=ncstep+1
'        if(ncstep.lt.1600)then
'          avlitr(1,ncstep)=0.0
'          avlitr(2,ncstep)=float(numlit)
'        endif
'
'        go to 140
'      endif
'
'c______________________________________________________________________________
'
'c   DO EQUILIBRIUM ITERATIONS   (for time n+1)
'
'      call timing(cpuio,cpuip,20,3)
'      call timing(cpuio,cpuip,7,2)
'      dn2=0.
'c
'         ierr=0
'
'c       ... bfgs or full newton method
'
'         call quasin(un3,un4,un5,tvc2,un2,un6,time,ierr)            ! QW 11-11-2015
'          
'     
'      if (bNike3dMsg) then 
'      write(10,*) ' after call quasin'
'      end if
'      
'         lresf=.false.
'         call auto(ierr,nbcku,iterm,lretry)     ! auto timestepping ?
'         if(lretry) lresf=.true.                ! failed time step, retry?
'
'c       ... full newton arc lenght methods
'
'c
'      call timing(cpuio,cpuip,7,3)
'      call timing(cpuio,cpuip,20,2)
'
'      if (lretry) then                           ! retry time step bfgs
'        term=.false.
'        goto 125
'      endif
'
'c   OUTPUT RESULTS  (for time n)
'
'c   ... write high speed printer file ... 
'
'  140 lpri=lpri+1
'      
'	call get_time(time1)
'	if (bNike3dMsg) then 
'      write(10,*) time1, ' OUTPUT RESULTS', lpri
'	end if
'
'      kpri=ipri-lpri
'      if (kpri.gt.0) go to 150
'      if(ipris) then
'         ipris=.false.
'         lpri=0
'         kpri=1
'         goto 150
'      endif
'      call timing(cpuio,cpuip,20,3)
'      call timing(cpuio,cpuip,9,2)
'      call prtout
'      call timing(cpuio,cpuip,9,3)
'      call timing(cpuio,cpuip,20,2)
'c
'c   ... write plot data into binary file ...
'  150 npri=npri+1
'      mpri=jpri-npri
'      if(stsion .and. (iterm.eq.1)) mpri=0
'      if(iterm.eq.10 .or. lplot) mpri=0
'      if (mpri.gt.0) go to 170
'      call timing(cpuio,cpuip,20,3)
'      call timing(cpuio,cpuip,10,2)
'      call prtout
'      call timing(cpuio,cpuip,10,3)
'      call timing(cpuio,cpuip,20,2)
'c______________________________________________________________________________
'
'c UPDATE DISPLACEMENT VARIABLES ETC. TO TIME N+1 VALUES
'
'	call get_time(time1)
'	if (bNike3dMsg) then 
'      write(10,*) time1, ' UPDATE DISPLACEMENT VARIABLES'
'	end if
'
'170   call update (un2,un3,lauto)
'
'      if (lchg) then
'        call chgint
'      endif
'
'c______________________________________________________________________________
'
'c  DETERMINE IF YOU ARE FINISHED
'
'      if (mthsol.lt.6.or.mthsol.gt.12) then
'         if (iterm.eq.2 .or. iterm.eq.10) call adios(2)
'         if (iterm.eq.3) then
'           mess='sw4.'
'           iterm=10
'           goto 100
'         endif
'         if (llpass) goto 300
'         if (   lauto  .and.time.le.termtm) go to 100
'         if (.not.lauto.and.nstep.lt.ntime) go to 100
'      else
'         if (llpass) goto 300
'         if (   lauto  .and.time.le.termtm) go to 100
'         if (.not.lauto.and.nstep.lt.ntime) go to 100
'         if (numspu.gt.0) then
'            nstep=0
'            ntime=numspu+1
'            dt=-time/numspu
'            dt=dt-1.e-10*dt
'            mthsol=mthunl
'            sltol=0.0
'            go to 100
'         endif
'         iterm=iterm+1
'         if (iterm.eq.1) go to 100
'      endif
'
'c NKC 8/9/99
'  300 call adios(1)       ! congratulations your done
'
'c______________________________________________________________________________
'
'c
'  
'  210 format(' type desired time step (e10.3)')
'  230 format(e10.3)
'  
'c
'      end

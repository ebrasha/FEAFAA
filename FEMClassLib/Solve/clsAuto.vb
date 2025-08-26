'This file contains all the methods of auto.f
Partial Public Class clsSolve


    ' YC 092018
    Public ipres, mxback, iteopt, irfwin As Integer
    Public ek, pdk, pk, pkp1, rkapg0, rhs1, rhs2, rhs3, rhs4 As Double

    Private ekm1, pdkm1, dtx0, dtold, dt00 As Double
    Private itrold, irat As Integer
    ' YC 092018 END


    ''' <summary>
    ''' to drive the auto time steppers
    ''' </summary>
    ''' <param name="ierr"></param>
    ''' <param name="nbcku"></param>
    ''' <param name="iterm"></param>
    ''' <param name="lretry"></param>
    Public Sub auto(ByRef ierr As Integer, ByRef nbcku As Integer, ByRef iterm As Integer, ByRef lretry As Boolean)

        Dim tol, one, third, fifth, ekr, ekp1r, pdkr, pkr, pkp1r, neksr, dtx0r As Double
        Dim dt0, ddt, scale, dtn, dtt, rattle, alpha0, newcost, cpnl0, cpnl, dthold, rkaprt, alprat, decsiz As Double
        Dim lchg, lratt0 As Boolean

        Dim msg100, msg200, msg300, msg101, msg150, msg151, msg400 As String
        Dim format100 = Environment.NewLine + " AUTO STEPPER: retry step, dt ={0}"
        Dim format200 = Environment.NewLine + " AUTO STEPPER: increasing time step, dt ={0}"
        Dim format300 = Environment.NewLine + " AUTO STEPPER: decreasing time step, dt ={0}"
        Dim format101 = Environment.NewLine + " AUTO STEPPER: retry step, dt ={0}  rattle flag ""on"""
        Dim format150 = Environment.NewLine + " COST AUTO STEPPER: parameters for current step" + Environment.NewLine +
                        "  old dt  ={0},    old weighted cost={1}" + Environment.NewLine +
                        "  curr. dt={2),  curr. weighted cost={3}" + Environment.NewLine +
                        "  old, new incr. unit cost={4}     {5}" + Environment.NewLine +
                        "  old, new deg. of nonlin.={6}     {7}" + Environment.NewLine +
                        "  old, new cost per nonln.={8}     {9}" + Environment.NewLine +
                        "  kapa , alpha ratios     ={10}     {11}" + Environment.NewLine
        Dim format151 = Environment.NewLine + " NO STEP CONTROL: parameters for current step" + Environment.NewLine +
                        "  old dt={0}  old weighted cost={1}" + Environment.NewLine +
                        "  curr. dt={2}  curr. weighted cost={3}" + Environment.NewLine +
                        "  old, new incr. unit cost={4}     {5}" + Environment.NewLine +
                        "  old, new deg. of nonlin.={6}     {7}" + Environment.NewLine +
                        "  old, new cost per nonln.={8}     {9}" + Environment.NewLine +
                        "  kapa , alpha ratios     ={10}     {11}" + Environment.NewLine
        Dim format400 = Environment.NewLine + " AUTO STEPPER:  unchanged time step, dt ={0}"

        tol = 0.0000000000005
        one = 1.0
        third = 1.0 / 3.0
        fifth = 1.0 / 5.0
        lchg = False

        If ierr = 2 Then
            iterm = 3
            lretry = False
            Return
        End If

        'c  ... "SPF" pressure reevaluation

        If lcspf = 0 Or ipres = 0 Then GoTo 10                            ' instead of press.
        ekr = ek
        ekp1r = ekp1
        pdkr = pdk
        pkr = pk
        pkp1r = pkp1
        neksr = neks
        dtx0r = dt

        Dim msg1 = "ik02 call rsrund 1 in sub auto"

        ekm1 = ekr
        ek = ekp1r
        ekp1 = 0.0
        pdkm1 = pdkr
        pdk = (pkp1r - pkr) / dtx0r
        pkp1 = pkp1r
        pk = pkr
        neks = neksr
        dt = dtx0r
        lretry = True
        Return

10:
        Select Case iauto
            Case 1
                GoTo 1000
            Case 2
                GoTo 2000
            Case 3
                GoTo 3000
            Case 4
                GoTo 4000
            Case 5
                GoTo 5000
        End Select

        If ierr = 1 Then iterm = 2
        lretry = False
        Return

        'c   ... "AUTO" method ...
1000:   If ierr = 1 Then
            If dt <= dtmin Or nbcku > mxback Or mxback = 0 Then
                iterm = 2
                lretry = False
                Return
            End If
            dt0 = dt
            dt = dt0
            If nbcku = 0 Then ddt = (dt - dtmin) / mxback
            nbcku = nbcku + 1
            dt = dt - ddt
            dt = Math.Max(dt, dtmin)

            'Call objNik3d.chgint() ' YC 092018
            Call chgint()

            msg100 = String.Format(format100, dt.ToString("e6"))
            '        write(lutty,100) dt
            '        write(luo,100) dt
            lretry = True
            Return
        End If

        'c   ... successful step ...
        dtx0 = dt
        dt = Math.Max(dt, dtmin)
        scale = Math.Sqrt(CDbl(iteopt) / CDbl(itrlas))
        If scale > 1.0 And dt < dtmax Then
            dtn = dt + (dtmax - dt) * Math.Min(0.2 * one, scale - one)
            dt = Math.Min(dtn, 5.0 * dt)
            '        write(lutty,200) dt
            '        write(luo,200) dt
            msg200 = String.Format(format200, dt.ToString("e6"))
            lchg = True
        ElseIf scale < 1.0 And dt > dtmin Then
            dt = dt - (dt - dtmin) * (1.0 - scale)
            '        write(lutty,300) dt
            '        write(luo,300) dt
            msg300 = String.Format(format300, dt.ToString("e6"))
            lchg = True
        End If
        lretry = False
        Return

5000:   If ierr = 1 Then
            If dt <= dtmin Or nbcku > mxback Or mxback = 0 Then
                iterm = 2
                lretry = False
                Return
            End If
            dt0 = dt
            dt = dt0
            nbcku = nbcku + 1
            irat = 2
            dt = dt / irat

            'Call objNik3d.chgint()     ' YC 092018
            Call chgint()

            msg100 = String.Format(format100, dt.ToString("e6"))
            '        write(lutty,100) dt
            '        write(luo,100) dt
            lretry = True
            Return
        End If
        'c   ... successful step ...
        'c     ... dt may have been decreased below dtmin to hit must point ...
        dtx0 = dt
        dtt = dtt + dt
        If dtt + tol * dt00 > dt00 Then
            dt = dt00
            dtt = 0
        Else
            dt = dt00 - dtt
        End If
        '       write(lutty,200) dt
        '       write(luo,200) dt
        msg200 = String.Format(format200, dt.ToString("e6"))

        lchg = True
        lretry = False
        Return

        'c   ... "RATL" method ...
2000:   If ierr = 1 Then
            'c   ... failed step ...
            If dt <= dtmin And lrattl Then
                iterm = 2
                lretry = False
                Return
            End If
            dt0 = dt
            lratt0 = lrattl
            rattle = Math.Pow((rhs1 - rhs2 + rhs3 - rhs4), 2) / (Math.Pow((rhs1 - rhs3), 2) + Math.Pow((rhs2 - rhs4), 2))
            lrattl = (rattle > 400.0)
            If Not lrattl And dt <= dtmin Then
                iterm = 2
                lretry = False
                Return
            End If
            dt = dt0
            lrattl = (rattle > 400.0)
            If lratt0 Or Not lrattl Then
                dt = Math.Pow(10.0, (Math.Log10(dt0) - third))
                dt = Math.Max(dt, dtmin)

                'Call objNik3d.chgint()     ' YC 092018
                Call chgint()


            End If
            If lrattl Then
                '          write(lutty,101) dt
                '          write(luo,101) dt
                msg101 = String.Format(format101, dt.ToString("e6"))
            Else
                msg100 = String.Format(format100, dt.ToString("e6"))
                '          write(lutty,100) dt
                '          write(luo,100) dt
            End If
            lretry = True
        Else
            'c   ... successful step ...
            'c       ... dt may have been decreased below dtmin to hit must point ...
            dtx0 = dt
            dt = Math.Max(dt, dtmin)
            If irflas > (iteopt + irfwin) And dt > dtmin Then
                dt = Math.Pow(10.0, (Math.Log10(dt) - third))
                dt = Math.Max(dt, dtmin)
                lchg = True
                '          write(lutty,300) dt
                '          write(luo,300) dt
                msg300 = String.Format(format300, dt.ToString("e6"))
            ElseIf irflas < (iteopt - irfwin) And dt < dtmax Then
                dt = Math.Pow(10.0, (Math.Log10(dt) + fifth))
                dt = Math.Min(dt, dtmax)
                lchg = True
                '          write(lutty,200) dt
                '          write(luo,200) dt
                msg200 = String.Format(format200, dt.ToString("e6"))
            End If
            lrattl = False
            rhs1 = 0.0
            rhs2 = 0.0
            rhs3 = 0.0
            rhs4 = 0.0
            lretry = False
        End If
        Return
        'c
        'c   ... "COST" method ...
3000:   If ierr = 1 Then
            'c   ... failed step (same as RATL for now) ...
            If dt <= dtmin And lrattl Then
                iterm = 2
                lretry = False
                Return
            End If
            dt0 = dt
            lratt0 = lrattl
            rattle = Math.Pow((rhs1 - rhs2 + rhs3 - rhs4), 2) / (Math.Pow((rhs1 - rhs3), 2) + Math.Pow((rhs2 - rhs4), 2))
            lrattl = (rattle > 400.0)
            If Not lrattl And dt <= dtmin Then
                iterm = 2
                lretry = False
                Return
            End If
            dt = dt0
            lrattl = (rattle > 400.0)
            If lratt0 Or Not lrattl Then
                dt = Math.Pow(10.0, (Math.Log10(dt0) - third))
                dt = Math.Max(dt, dtmin)

                'Call objNik3d.chgint()     ' YC 092018
                Call chgint()


            End If
            If lrattl Then
                '          write(lutty,101) dt
                '          write(luo,101) dt
                msg101 = String.Format(format101, dt.ToString("e6"))
            Else
                msg100 = String.Format(format100, dt.ToString("e6"))
                '          write(lutty,100) dt
                '          write(luo,100) dt
            End If
            lretry = True
        Else
            'c
            'c   ... successful step ...
            'c   ... check for first converged step ...
            If rkapg0 <= 0.0 Then
                'c         ... fix alphaO = alpha in first step ...
                itrold = itrlas + ilimit * irflas
                dtold = dt
                'c         ... ignore nonlinearity for first step ...
                rkapg0 = rkapg
            End If
            'c
            'c       ... dt may have been decreased below dtmin to hit must point ...
            dtx0 = dt
            dt = Math.Max(dt, dtmin)
            alpha0 = itrold / dtold
            newcost = itrlas + ilimit * irflas
            alpha = newcost / dt
            cpnl0 = alpha0 / rkapg0
            cpnl = alpha / rkapg
            dthold = dt
            rkaprt = rkapg / rkapg0
            alprat = alpha / alpha0
            decsiz = mxback / 10.0
            'c
            'c       ... write cost statistics for debugging purposes
            '        write(luo,150) dtold,itrold,dt,newcost,alpha0,alpha,
            '     &                   rkapg0,rkapg,cpnl0,cpnl,rkaprt,alprat
            'c
            msg150 = String.Format(format150, dtold.ToString("e6"), itrold.ToString("#####"), dt.ToString("e6"),
                               newcost.ToString("#####"), alpha0.ToString("e6"), alpha.ToString("e6"),
                               rkapg0.ToString("e6"), rkapg.ToString("e6"), cpnl0.ToString("e6"),
                               cpnl.ToString("e6"), rkaprt.ToString("e6"), alprat.ToString("e6"))
            If rkapg < 1.0001 And alprat = 1.0 Then
                'c         ... push the strategy if the pb. is easy (nearly linear)
                dt = Math.Pow(10.0, (Math.Log10(dt) + decsiz))
                dt = Math.Min(dt, dtmax)
                lchg = True
                msg200 = String.Format(format200, dt.ToString("e6"))
                '          write(lutty,200) dt
                '          write(luo,200) dt
                '          if (lctmp) write(luebe,200) dt              ! changed by QW 12-05-2013 
            ElseIf alprat < 0.9 * rkaprt And dt >= dtold Then
                'c         ... cost/nonlinearity decreasing, keep increasing
                dt = Math.Pow(10.0, (Math.Log10(dt) + decsiz))
                dt = Math.Min(dt, dtmax)
                lchg = True
                '          write(lutty,200) dt
                '          write(luo,200) dt
                '          if lctmp write(luebe,200) dt
                msg200 = String.Format(format200, dt.ToString("e6"))
            ElseIf alprat > 1.1 * rkaprt And dt >= dtold Then
                'c         ... cost/nonlinearity increasing, decrease time step
                If dt > dtold Then dt = Math.Pow(10.0, (Math.Log10(dt) - decsiz / 2.0))
                If dt = dtold Then dt = Math.Pow(100, (Math.Log10(dt) - decsiz))
                dt = Math.Max(dt, dtmin)
                lchg = True
                '          write(lutty,300) dt
                '          write(luo,300) dt
                '          if (lctmp) write(luebe,300) dt
                msg300 = String.Format(format300, dt.ToString("e6"))
            ElseIf alprat < 0.9 * rkaprt And dt < dtold Then
                'c         ... cost/nonlinearity decreasing, keep decreasing time step
                dt = Math.Pow(10.0, (Math.Log10(dt) - decsiz))
                dt = Math.Max(dt, dtmin)
                lchg = True
                '          write(lutty,300) dt
                '          write(luo,300) dt
                '          if (lctmp) write(luebe,300) dt
                msg300 = String.Format(format300, dt.ToString("e6"))
            ElseIf alprat > 1.1 * rkaprt And dt < dtold Then
                'c         ... cost/nonlinearity increasing, increase time step
                dt = Math.Pow(10.0, (Math.Log10(dt) + decsiz / 2.0))
                dt = Math.Min(dt, dtmax)
                lchg = True
                '          write(lutty,200) dt
                '          write(luo,200) dt
                '          if lctmp write(luebe,200) dt
                msg200 = String.Format(format200, dt.ToString("e6"))
            Else
                'c         ... no change
                '          write(lutty,400) dt
                '          write(luo,400) dt
                '          if (lctmp) write(luebe,400) dt
                msg400 = String.Format(format400, dt.ToString("e6"))
            End If
            'c
            'c   ... update history variables ...
            dtold = dthold
            itrold = newcost
            rkapg0 = rkapg
            lrattl = False
            rhs1 = 0.0
            rhs2 = 0.0
            rhs3 = 0.0
            rhs4 = 0.0
            lretry = False
        End If
        Return
        'c
        'c   ... "PRNT" print out cost statistics, no time step control ...
4000:
        'c
        'c   ... check for first converged step ...
        If rkapg0 <= 0.0 Then
            itrold = iteopt
            dtold = dt
            'c       ... ignore nonlinearity for first step ...
            rkapg0 = rkapg
        End If
        'c
        alpha0 = itrold / dtold
        newcost = itrlas + ilimit * irflas
        alpha = newcost / dt
        cpnl0 = alpha0 / rkapg0
        cpnl = alpha / rkapg
        dthold = dt
        rkaprt = rkapg / rkapg0
        alprat = alpha / alpha0
        'c
        'c  Write cost statistics for debugging purposes
        '      write(lutty,151) dtold,itrold,dt,newcost,alpha0,alpha,
        '     &                   rkapg0,rkapg,cpnl0,cpnl,rkaprt,alprat
        '      write(luo,151) dtold,itrold,dt,newcost,alpha0,alpha,
        '     &                   rkapg0,rkapg,cpnl0,cpnl,rkaprt,alprat
        '      if (lctmp) write(luebe,151) dtold,itrold,dt,newcost,alpha0,alpha,
        '     &                   rkapg0,rkapg,cpnl0,cpnl,rkaprt,alprat
        msg151 = String.Format(format151, dtold.ToString("e6"), itrold.ToString("#####"), dt.ToString("e6"),
                               newcost.ToString("#####"), alpha0.ToString("e6"), alpha.ToString("e6"),
                               rkapg0.ToString("e6"), rkapg.ToString("e6"), cpnl0.ToString("e6"),
                               cpnl.ToString("e6"), rkaprt.ToString("e6"), alprat.ToString("e6"))
        'c   ... update history variables ...
        dtold = dthold
        itrold = newcost
        rkapg0 = rkapg
        Return

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine auto(ierr,nbcku,iterm,lretry)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c===> module to drive the auto time steppers
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk12/dtx0,dt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
'      common/bk20/kpri,nstep,ite,ilimit,iref
'      common/bk29/tim(7),iterp1,iterp2,time,timep,lpri,npri
'      
'      
'      common/riksw2/rlnew,alfa0,dsx,iteopt,idctrl,riksf,numspu,mthunl
'      logical lplotm
'      common/automt/dtmin,dtmax,termtm,mxback,iauto,irfwin,lcmust,lplotm
'      common/autcst/dtold,rkapg,rkapg0,itrold
'      common/autvib/dt00,irat
'      common/total/itrlas,irflas,irhlas,itrtot,irftot,irhtot
'      logical lrattl
'      common/bkratt/rhs1,rhs2,rhs3,rhs4,lrattl
'      common/bkspf1/etarg,emin,emax,ptime,ptmin,pdmax,lcspf,nmspf,tpelmc
'      common/bkspf2/ekm1,ek,ekp1,pdkm1,pdk,pk,pkp1,pfac,neks
'      common/bkspf4/pratio,ipres
'      logical lchg
'      common/autchgnt/lchg
'      logical lconvt,lctmp                    ! changed by QW 12-05-2013 
'      common/gttcon/lconvt                    ! changed by QW 12-05-2013 
'
'      logical lretry,lratt0,sw30,sw60,sw70
'      save ddt,dtt
'      data ddt,dtt /0.,0./
'
'      tol = 5.e-13
'c
'      one=1.0
'      third=1.d0/3.
'      fifth=1.d0/5.
'      lchg = .false.
'c
'      if (ierr.eq.2) then
'        iterm=3
'        lretry=.false.
'        return
'      endif
'c
'c  ... "SPF" pressure reevaluation
'c
'      if(lcspf.eq.0.or.ipres.eq.0) goto 10                            ! instead of press.
'        ekr    = ek
'        ekp1r  = ekp1
'        pdkr   = pdk
'        pkr    = pk
'        pkp1r  = pkp1
'        neksr  = neks
'        dtx0r  = dt
'!       call rsrund
'	  write(10,*) 'ik02 call rsrund 1 in sub auto'
'
'        ekm1   = ekr
'        ek     = ekp1r
'        ekp1   = 0.0
'        pdkm1  = pdkr
'        pdk    = (pkp1r - pkr)/dtx0r
'        pkp1   = pkp1r
'        pk     = pkr
'        neks   = neksr
'        dt     = dtx0r
'       lretry=.true.
'       return
'c
'   10 goto (1000,2000,3000,4000,5000) iauto
'c
'      if (ierr.eq.1) iterm=2
'      lretry=.false.
'      return
'c
'c   ... "AUTO" method ...
' 1000 if (ierr.eq.1) then
'        if (dt.le.dtmin.or.nbcku.gt.mxback.or.mxback.eq.0) then
'          iterm=2
'          lretry=.false.
'          return
'        endif
'        dt0=dt
'        dt=dt0
'        if(nbcku.eq.0) ddt=(dt-dtmin)/mxback
'        nbcku=nbcku+1
'        dt=dt-ddt
'        dt=max(dt,dtmin)
'        call chgint
'        write(lutty,100) dt
'        write(luo,100) dt
'        lretry = .true.
'        return
'      endif
'c   ... successful step ...
'      dtx0=dt
'      dt=max(dt,dtmin)
'      scale=sqrt(float(iteopt)/float(itrlas))
'      if (scale.gt.1.0 .and. dt.lt.dtmax) then
'        dtn=dt+(dtmax-dt)*min(.20*one,scale-one)
'        dt=min(dtn,5.*dt)
'        write(lutty,200) dt
'        write(luo,200) dt
'        lchg = .true.
'      elseif (scale.lt.1.0 .and. dt.gt.dtmin) then
'        dt=dt-(dt-dtmin)*(1.-scale)
'        write(lutty,300) dt
'        write(luo,300) dt
'        lchg = .true.
'      endif
'      lretry = .false.
'      return
'
' 5000 if (ierr.eq.1) then
'        if (dt.le.dtmin.or.nbcku.gt.mxback.or.mxback.eq.0) then
'          iterm=2
'          lretry=.false.
'          return
'        endif
'        dt0=dt
'        dt=dt0
'        nbcku=nbcku+1
'        irat = 2
'        dt = dt/irat
'        call chgint
'        write(lutty,100) dt
'        write(luo,100) dt
'        lretry = .true.
'        return
'      endif
'c   ... successful step ...
'c     ... dt may have been decreased below dtmin to hit must point ...
'      dtx0=dt
'        dtt = dtt + dt
'        if(dtt+tol*dt00.gt.dt00) then
'          dt = dt00
'          dtt = 0
'        else
'          dt = dt00 - dtt
'        endif
'       write(lutty,200) dt
'       write(luo,200) dt
'c      call chgint
'       lchg = .true.
'      lretry = .false.
'      return
'c
'c   ... "RATL" method ...
' 2000 if (ierr.eq.1) then
'c   ... failed step ...
'        if(dt.le.dtmin .and. lrattl) then
'          iterm=2
'          lretry=.false.
'          return
'        endif
'        dt0=dt
'        lratt0=lrattl
'        rattle=(rhs1-rhs2+rhs3-rhs4)**2/((rhs1-rhs3)**2+(rhs2-rhs4)**2)
'        lrattl=(rattle.gt.400.)
'        if(.not.lrattl .and. dt.le.dtmin) then
'          iterm=2
'          lretry=.false.
'          return
'        endif
'        dt=dt0
'        lrattl=(rattle.gt.400.)
'        if(lratt0 .or. .not.lrattl) then
'          dt=10.**(log10(dt0)-third)
'          dt=max(dt,dtmin)
'          call chgint
'        endif
'        if(lrattl)then
'          write(lutty,101) dt
'          write(luo,101) dt
'        else
'          write(lutty,100) dt
'          write(luo,100) dt
'        endif
'        lretry=.true.
'      else
'c   ... successful step ...
'c       ... dt may have been decreased below dtmin to hit must point ...
'        dtx0=dt
'        dt=max(dt,dtmin)
'        if(irflas .gt. (iteopt+irfwin) .and. dt.gt.dtmin) then
'          dt=10.**(log10(dt)-third)
'          dt=max(dt,dtmin)
'          lchg = .true.
'          write(lutty,300) dt
'          write(luo,300) dt
'        elseif(irflas .lt. (iteopt-irfwin) .and. dt.lt.dtmax) then
'          dt=10.**(log10(dt)+fifth)
'          dt=min(dt,dtmax)
'          lchg = .true.
'          write(lutty,200) dt
'          write(luo,200) dt
'        endif
'        lrattl=.false.
'        rhs1=0.0
'        rhs2=0.0
'        rhs3=0.0
'        rhs4=0.0
'        lretry=.false.
'      endif
'      return
'c
'c   ... "COST" method ...
' 3000 if (ierr.eq.1) then
'c   ... failed step (same as RATL for now) ...
'        if(dt.le.dtmin .and. lrattl) then
'          iterm=2
'          lretry=.false.
'          return
'        endif
'        dt0=dt
'        lratt0=lrattl
'        rattle=(rhs1-rhs2+rhs3-rhs4)**2/((rhs1-rhs3)**2+(rhs2-rhs4)**2)
'        lrattl=(rattle.gt.400.)
'        if(.not.lrattl .and. dt.le.dtmin) then
'          iterm=2
'          lretry=.false.
'          return
'        endif
'        dt=dt0
'        lrattl=(rattle.gt.400.)
'        if(lratt0 .or. .not.lrattl) then
'          dt=10.**(log10(dt0)-third)
'          dt=max(dt,dtmin)
'          call chgint
'        endif
'        if(lrattl)then
'          write(lutty,101) dt
'          write(luo,101) dt
'        else
'          write(lutty,100) dt
'          write(luo,100) dt
'        endif
'        lretry=.true.
'      else
'c
'c   ... successful step ...
'c   ... check for first converged step ...
'        if (rkapg0.le.0.0) then
'c         ... fix alphaO = alpha in first step ...
'          itrold=itrlas+ilimit*irflas
'          dtold=dt
'c         ... ignore nonlinearity for first step ...
'	  rkapg0=rkapg
'	endif
'c
'c       ... dt may have been decreased below dtmin to hit must point ...
'	dtx0=dt
'	dt=max(dt,dtmin)
'	alpha0=itrold/dtold
'        newcost=itrlas+ilimit*irflas
'	alpha=newcost/dt
'        cpnl0=alpha0/rkapg0
'        cpnl =alpha /rkapg
'        dthold=dt
'        rkaprt=rkapg/rkapg0
'        alprat=alpha/alpha0
'        decsiz=mxback/10.0
'c
'c       ... write cost statistics for debugging purposes
'        write(luo,150) dtold,itrold,dt,newcost,alpha0,alpha,
'     &                   rkapg0,rkapg,cpnl0,cpnl,rkaprt,alprat
'c
'        if(rkapg .lt. 1.0001 .and. alprat .eq. 1.0) then
'c         ... push the strategy if the pb. is easy (nearly linear)
'          dt=10.**(log10(dt)+decsiz)
'          dt=min(dt,dtmax)
'          lchg = .true.
'          write(lutty,200) dt
'          write(luo,200) dt
'          if (lctmp) write(luebe,200) dt              ! changed by QW 12-05-2013 
'        elseif(alprat .lt. 0.9*rkaprt .and. dt .ge. dtold) then
'c         ... cost/nonlinearity decreasing, keep increasing
'          dt=10.**(log10(dt)+decsiz)
'c          if(dt .eq. dtold)dt=10.**(log10(dt)+decsiz/2.)
'          dt=min(dt,dtmax)
'c         call chgint
'          lchg = .true.
'          write(lutty,200) dt
'          write(luo,200) dt
'          if (lctmp) write(luebe,200) dt
'        elseif(alprat .gt. 1.1*rkaprt .and. dt .ge. dtold) then
'c         ... cost/nonlinearity increasing, decrease time step
'          if(dt .gt. dtold)dt=10.**(log10(dt)-decsiz/2.)
'          if(dt .eq. dtold)dt=10.**(log10(dt)-decsiz)
'          dt=max(dt,dtmin)
'          lchg = .true.
'          write(lutty,300) dt
'          write(luo,300) dt
'          if (lctmp) write(luebe,300) dt
'        elseif(alprat .lt. 0.9*rkaprt .and. dt .lt. dtold) then
'c         ... cost/nonlinearity decreasing, keep decreasing time step
'          dt=10.**(log10(dt)-decsiz)
'          dt=max(dt,dtmin)
'          lchg = .true.
'          write(lutty,300) dt
'          write(luo,300) dt
'          if (lctmp) write(luebe,300) dt
'        elseif(alprat .gt. 1.1*rkaprt .and. dt .lt. dtold) then
'c         ... cost/nonlinearity increasing, increase time step
'          dt=10.**(log10(dt)+decsiz/2.)
'          dt=min(dt,dtmax)
'c         call chgint
'          lchg = .true.
'          write(lutty,200) dt
'          write(luo,200) dt
'          if (lctmp) write(luebe,200) dt
'        else
'c         ... no change
'          write(lutty,400) dt
'          write(luo,400) dt
'          if (lctmp) write(luebe,400) dt
'        endif
'c
'c   ... update history variables ...
'        dtold=dthold
'        itrold=newcost
'        rkapg0=rkapg
'        lrattl=.false.
'        rhs1=0.0
'        rhs2=0.0
'        rhs3=0.0
'        rhs4=0.0
'        lretry=.false.
'      endif
'      return
'c
'c   ... "PRNT" print out cost statistics, no time step control ...
' 4000 continue
'c
'c   ... check for first converged step ...
'      if (rkapg0.le.0.0) then
'        itrold=iteopt
'        dtold=dt
'c       ... ignore nonlinearity for first step ...
'        rkapg0=rkapg
'      endif
'c
'      alpha0=itrold/dtold
'      newcost=itrlas+ilimit*irflas
'      alpha=newcost/dt
'      cpnl0=alpha0/rkapg0
'      cpnl =alpha /rkapg
'      dthold=dt
'      rkaprt=rkapg/rkapg0
'      alprat=alpha/alpha0
'c
'c  Write cost statistics for debugging purposes
'      write(lutty,151) dtold,itrold,dt,newcost,alpha0,alpha,
'     &                   rkapg0,rkapg,cpnl0,cpnl,rkaprt,alprat
'      write(luo,151) dtold,itrold,dt,newcost,alpha0,alpha,
'     &                   rkapg0,rkapg,cpnl0,cpnl,rkaprt,alprat
'      if (lctmp) write(luebe,151) dtold,itrold,dt,newcost,alpha0,alpha,
'     &                   rkapg0,rkapg,cpnl0,cpnl,rkaprt,alprat
'c   ... update history variables ...
'      dtold=dthold
'      itrold=newcost
'      rkapg0=rkapg
'      return
'c
'  100 format (//' AUTO STEPPER: retry step, dt =',1pe12.5)
'  101 format (//' AUTO STEPPER: retry step, dt =',1pe12.5,
'     &          '  rattle flag "on"')
'  150 format (//' COST AUTO STEPPER: parameters for current step',
'     +        /'  old dt=',1pe12.5, '  old weighted cost=',i5,
'     +        /'  curr. dt=',1pe12.5, '  curr. weighted cost=',i5,
'     +        /'  old, new incr. unit cost=',1pe12.5,5x,1pe12.5,
'     +        /'  old, new deg. of nonlin.=',1pe12.5,5x,1pe12.5,
'     +        /'  old, new cost per nonln.=',1pe12.5,5x,1pe12.5,
'     +        /'  kapa , alpha ratios     =',1pe12.5,5x,1pe12.5)
'  151 format (//' NO STEP CONTROL: parameters for current step',
'     +        /'  old dt=',1pe12.5, '  old weighted cost=',i5,
'     +        /'  curr. dt=',1pe12.5, '  curr. weighted cost=',i5,
'     +        /'  old, new incr. unit cost=',1pe12.5,5x,1pe12.5,
'     +        /'  old, new deg. of nonlin.=',1pe12.5,5x,1pe12.5,
'     +        /'  old, new cost per nonln.=',1pe12.5,5x,1pe12.5,
'     +        /'  kapa , alpha ratios     =',1pe12.5,5x,1pe12.5)
'  200 format (//' AUTO STEPPER: increasing time step, dt =',1pe12.5)
'  300 format (//' AUTO STEPPER: decreasing time step, dt =',1pe12.5)
'  400 format (//' AUTO STEPPER:  unchanged time step, dt =',1pe12.5)
'c
'      end

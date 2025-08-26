'This file contains all the methods of precon.f
Partial Public Class clsSolve

    ' YC 092018
    Public icalls, itrprt, itrlmt, maxlit As Integer
    Public tollin As Double
    ' YC 092018 END


    'Public Sub precon(ByRef r() As Double, ByRef us() As Double, ByRef p() As Double,
    '                  ByRef z() As Double, ByRef ap() As Double, ByRef diag() As Double,
    '                ByRef ediag() As Double, ByRef idp() As Integer, ByRef hxdata() As Double, ByRef sfdata() As Double)
    Public Sub precon(ByRef r() As Double, ByRef us() As Double, ByRef p() As Double, ByRef z() As Double, ByRef ap() As Double, ByRef diag(,) As Double,
            ByRef idp(,) As Integer, ByRef hxdata1(,) As Double, ByRef hxdata2(,) As Integer, ByRef sfdata1(,) As Double, ByRef sfdata2(,) As Integer,
            ByVal iopt As Integer)   ' YC 092018   ' QW 12-12-2018-

        Dim rz, rtol, rmin, rzero, pap, rl2, rzlast, beta, pp, apap As Double
        Dim m As Integer
        Dim lctmp As Boolean

        Dim i As Integer     ' YC 102418

        Dim mreq = 1

        mreq = Fix(neql / 200)            ' QW 12-12-2018-

        Dim mmin = 0

        icalls = icalls + 1
        'If itrprt >= 4 Then
        'Dim dl2 = fdot(diag, diag, neql)

        'Dim msg2000 = String.Format(" norm of global diagonal = {0}" + Environment.NewLine +
        '                        " norm of element diagonal = {1}", dl2.ToString("e5"))
        ' End If

        'c.... initialize solution vectors                                       

        'Call objNik3d.azero(us, neql)  ' YC 092018
        Call azero(us, neql)
        'If iopt = 2 Then
        'Call Check1D(r, neql, 1)
        'End If
        Dim msg10 = ""
        If nrcc = 0 Then
            'Call Check1D(r, neql, 1)
            'TODO set dim
            'Dim idp_copy(,) As Integer, diag_copy(,) As Double 'QW 12-12-2018-
            'Dim diag_2D(21, numnp) As Double
            'Call objComsub.ArrayConvert1Dto2D(diag, diag_2D, 21, numnp)
            ''TODO copy from idp, diag to idp_copy, diag_copy
            'Call bdscal(idp, diag_2D, r, numnp, True)
            Call bdscal(idp, diag, r, numnp, True)           'QW 12-12-2018-
            ''TODO copy from idp_copy, diag_copy to idp, diag
            'Call objComsub.ArrayConvert2Dto1D(diag_2D, 21, numnp, diag)
        Else
            msg10 = "ik02 call bdscl3 1 in sub precon"
        End If
        If numdc <> 0 Then
            msg10 = "ik02 call initbc 1 in sub precon"
        End If

        'If iopt = 2 Then
        'Call Check1D(r, neql, 1)
        'End If

        rzero = fdot(r, r, neql)
        rmin = rzero
        rtol = tollin * tollin * rzero
        Call blkcpy(r, z, neql)
        Call blkcpy(z, p, neql)
        rz = fdot(r, z, neql)

        'c....  begin iterations                                                 

        'For m = 0 To itrlmt - 1  ' YC 102418
        For m = 1 To itrlmt
            If m = 279 Then
                'Stop
            End If
            'Call objNik3d.azero(ap, neql)  ' YC 092018
            Call azero(ap, neql)

            Call peval(ap, p, hxdata1, hxdata2, sfdata1, sfdata2)     ' QW 12-12-2018-
            pap = fdot(p, ap, neql)
            alpha = rz / pap
            pp = fdot(p, p, neql)
            apap = fdot(ap, ap, neql)

            'Call Check1(pp, apap)
            'If m = 500 Then
            'Stop
            ' End If
            'For i = 0 To neql - 1  ' YC 102418
            For i = 1 To neql
                us(i) = us(i) + alpha * p(i)
                r(i) = r(i) - alpha * ap(i)
                'Call Check1D(r, neql)
            Next
            If (itrprt >= 5) Then
                Dim mg1030 = "residual vector after " + m.ToString("####") + " p.c.g. iterations"

                'For ind = 0 To neql - 1    ' YC 102418
                For ind = 1 To neql
                    mg1030 = mg1030 + Environment.NewLine + r(ind).ToString("e6")
                Next
            End If

            'c.... convergence check                                                 

            rl2 = fdot(r, r, neql)
            If rl2 < rmin Then
                rmin = rl2
                mmin = m
            End If
            If itrprt >= 2 And lctmp Then
                Dim msg1050 = String.Format("  at itr {0}, rl2 = {1} mmin = {2}",
                                   m.ToString("#####"), rl2.ToString("e5"), mmin.ToString("#####"))
            End If

            If rl2 < rtol And m > mreq Then
                numlit = numlit + m
                If (m > maxlit) Then maxlit = m
                Call blkcpy(us, r, neql)
                If nrcc = 0 Then
                    'TODO set dim
                    ' Dim idp_copy(,) As Integer, diag_copy(,) As Double
                    'TODO copy from idp, diag to idp_copy, diag_copy
                    Call bdscal(idp, diag, r, numnp, False)     ' QW 12-12-2018-
                    'TODO copy from idp_copy, diag_copy to idp, diag
                Else
                    msg10 = "ik02 call bdscl3 2 in sub precon"
                End If
                If lctmp Then
                    Dim msg1040 = String.Format(
                    "  convergence in {0} iterations:" + Environment.NewLine +
                    "  initial norm = {1}  final norm = {2}",
                    m.ToString("####"), rzero.ToString("e7"), rl2.ToString("e7"))
                End If
                If itrprt >= 1 Then
                    Dim msg1000 = String.Format(
                    "          convergence of inner loop after {0} iterations" + Environment.NewLine +
                    "               initial norm of residual = {1}" + Environment.NewLine +
                    "               final norm of residual   = {2}",
                    m.ToString("####"), rzero.ToString("e6"), rl2.ToString("e6"))
                End If

                Return
            End If

            'c.... compute conjugate direction                                       

            If itrprt >= 3 Then
                Dim msg1021 = String.Format("           at iteration {0}:  alpha = {1};  rl2 = {2}",
                    m.ToString("###"), alpha.ToString("e5"), rl2.ToString("e5"))
            End If

            Call blkcpy(r, z, neql)
            rzlast = rz
            rz = fdot(r, z, neql)
            beta = rz / rzlast

            'For i = 0 To neql - 1   ' YC 102418
            For i = 1 To neql
                p(i) = z(i) + beta * p(i)
            Next
        Next

        'c....  announce failure to converge                                     

        Dim msg1010 = String.Format("     convergence of inner loop failed after {0} iterations" + Environment.NewLine +
        "          initial norm of residual  = {1}" + Environment.NewLine +
        "          last norm                 = {2}" + Environment.NewLine +
        "          smallest norm             = {3}" + Environment.NewLine +
        "      (at iteration {4})" + Environment.NewLine +
        "          required norm             = {5}", m.ToString("####"), rzero.ToString("e6"),
                   rl2.ToString("e6"), rmin.ToString("e6"), mmin.ToString("#####"), rtol.ToString("e6"))
        numlit = numlit + m

        'Call objNik3d.adios(2)      ' YC 092018
        ' Call objComsub.adios(2)

    End Sub


End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine precon(r,us,p,z,ap,diag,ediag,idp,
'     &                  hxdata,sfdata)                     ! QW 11-11-2015
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c                                                                       
'c===> module to drive preconditioned conjugate gradients                
'c                                                                       
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,
'     &            nipmx,nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e,  
'     & n20f,n20g
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem           
'      common/bk35/numdc,numudc,nrcc                                     
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye3/neehx,ksizhx,neesf,ksizsf
'      common/ebye4/ icalls,numlit,maxlit,ncstep,avlitr(2,1600),ncunit   
'      common/bkneq/neql
'      logical lconvt,lctmp
'      common/gttcon/lconvt                ! changed by QW 12-05-2013
'                                                
'c      
'c      
'      dimension  r(*),p(*),z(*),us(*),ap(*),idp(*),diag(*)              
'c                                                                       
'c NKC 9/14/99
'c      write(6,*) 'entering precon.F'
'
'      mreq = 1                                                          
'      mreq = neql/200                                                   
'      mmin = 0                                                          
'      icalls = icalls + 1                                               
'      if (itrprt.ge.4) then                                             
'      dl2  = fdot(diag,diag,neql)                                       
'      edl2 = fdot(ediag,ediag,neql)                                     
'      write(luo,2000) dl2,edl2                                          
' 2000 format(' norm of global diagonal = ',e12.4,                       
'     &      /' norm of element diagonal = ',e12.4)                      
'      endif                                                             
'c                                                                       
'c.... initialize solution vectors                                       
'c                                                                       
'      call azero (us,neql)                                              
'      if (nrcc.eq.0) then                                               
'         call bdscal(idp,diag,r,numnp,.true.)                           
'      else                                                              
'	   write(10,*) 'ik02 call bdscl3 1 in sub precon'
'      endif                                                             
'      if (numdc.ne.0) then               
'       write(10,*) 'ik02 call initbc 1 in sub precon' 
'	end if
'
'      rzero = fdot (r,r,neql)                                           
'      rmin = rzero                                                      
'      rtol = tollin*tollin*rzero                                        
'      call blkcpy (r,z,neql)                                
'      call blkcpy (z,p,neql)                                            
'      rz = fdot (r,z,neql)                                              
'c                                                                       
'c....  begin iterations                                                 
'c                                                                       
'      do 300 m=1,itrlmt                                                 
'         call azero(ap,neql)                              
'         call peval (ap,p,hxdata,sfdata)                ! QW 11-11-2015                                                         
'         pap = fdot(p,ap,neql)                                          
'         alpha = rz/pap                                                 
'         do 100 i=1,neql                                                
'            us(i) = us(i) + alpha*p(i)                                  
'            r(i)  = r(i)  - alpha*ap(i)                                 
'  100    continue                                                       
'         if (itrprt.ge.5) write(luo,1030) m,(r(i),i=1,neql)             
'c                                                                       
'c.... convergence check                                                 
'c                                                                       
'         rl2 = fdot(r,r,neql)                                           
'         if (rl2.lt.rmin) then                                          
'            rmin = rl2                                                  
'            mmin = m                                                    
'         endif                                                          
'         if (itrprt.ge.2 .and.lctmp) write (ncunit,1050) m,rl2,mmin    ! changed by QW 12-05-2013
' 1050 format ('  at itr ',i5,', rl2 = ',e14.4,' mmin = ',i5)            
'         if (rl2.lt.rtol .and. m.gt.mreq) then                          
'            numlit = numlit + m                                         
'            if (m.gt.maxlit) maxlit = m                                 
'            call blkcpy (us,r,neql)                                     
'            if (nrcc.eq.0) then                                         
'               call bdscal(idp,diag,r,numnp,.false.)                    
'            else                                                        
'!               call bdscl3(idp,idp(1+6*numnp),diag,r,numnp,nrcc,.false.)vax
'	         write(10,*) 'ik02 call bdscl3 2 in sub precon'
'            endif                                                       
'            if (lctmp) write(ncunit,1040) m,rzero,rl2                 ! changed by QW 12-05-2013
'            if (itrprt.ge.1) write(luo,1000)  m,rzero,rl2               
'            return                                                      
'         endif                                                          
'c                                                                       
'c.... compute conjugate direction                                       
'c                                                                       
'         if (itrprt.ge.3) write(luo,1021) m,alpha,rl2                   
'         if (itrprt.ge.3) write(lutty,1021) m,alpha,rl2                 
'         call blkcpy(r,z,neql)                                                                                                 
'         rzlast = rz                                                    
'         rz = fdot(r,z,neql)                                            
'         beta = rz/rzlast                                               
'         do 200 i=1,neql                                                
'  200    p(i) = z(i) + beta*p(i)                                        
'  300 continue                                                          
'c                                                                       
'c....  announce failure to converge                                     
'c                                                                       
'      write(lutty,1010) m,rzero,rl2,rmin,mmin,rtol                      
'      write(luo,1010) m,rzero,rl2,rmin,mmin,rtol                        
'      numlit = numlit + m                                               
'      call adios(2)                                                     
'c                                                                       
' 1000 format (/                                                         
'     1/5x,'convergence of inner loop after ',i4,' iterations',          
'     2/5x,'     initial norm of residual = ',e12.5,                     
'     3/5x,'     final norm of residual   = ',e12.5)                     
' 1010 format (/                                                         
'     1/5x,'convergence of inner loop failed after ',i4,' iterations',   
'     2/5x,'     initial norm of residual  = ',e12.5,                    
'     3/5x,'     last norm                 = ',e12.5,                    
'     4/5x,'     smallest norm             = ',e12.5,                    
'     5    '  (at iteration ',i5,')',                                    
'     6/5x,'     required norm             = ',e12.5)                    
' 1021 format (                                                          
'     &/5x,' at iteration ',i3,':  alpha = ',e12.4,';  rl2 = ',e12.4)    
' 1030 format (/5x,'residual vector after ',i4,' p.c.g. iterations',     
'     &        10(/5e13.5/5e13.5/5e13.5))                                
' 1040 format('  convergence in ',i4,' iterations:',                     
'     &       '  initial norm = ',e15.6,'  final norm = ',e15.6)         
'c                                                                       
'      return                                                            
'      end                                                               

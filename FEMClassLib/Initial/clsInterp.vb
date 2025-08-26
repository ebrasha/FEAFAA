'This file contains all the methods of interp.f
Partial Public Class clsInitial

    Public Sub interp(ByRef p() As Double, ByRef tau As Double, ByRef numlp As Integer,
                      ByRef f As Double, ByRef xmag As Double, ByRef ierr As Integer)

        Dim p_n(2, numlp) As Double
        ' p starts from 0 element 
        For j = 1 To numlp
            For i = 1 To 2
                Try
                    p_n(i, j) = p(2 * (j - 1) + i - 1)
                Catch ex As Exception
                    p_n(i, j) = 0
                End Try
            Next
        Next
        Call interp(p_n, tau, numlp, f, xmag, ierr)

    End Sub
    ''' <summary>
    ''' interpolate load curve values
    ''' </summary>
    ''' <param name="p"></param>
    ''' <param name="tau"></param>
    ''' <param name="numlp"></param>
    ''' <param name="f"></param>
    ''' <param name="xmag"></param>
    ''' <param name="ierr"></param>
    Public Sub interp(ByRef p(,) As Double, ByRef tau As Double, ByRef numlp As Integer,
                      ByRef f As Double, ByRef xmag As Double, ByRef ierr As Integer)

        Dim n = 0
        Dim dt As Double = 0
        If (mthsol < 6 OrElse mthsol > 12) AndAlso delt >= 0.0 Then
            If tau - p(1, numlp) > 0.0000001 Then GoTo 30
            For m = 2 To numlp
                n = m
                If tau - p(1, m) <= 0.0 Then GoTo 20
                If Math.Abs(tau - p(1, m)) <= 0.0000001 Then GoTo 20
            Next
            GoTo 30
20:         dt = tau - p(1, n - 1)
            Dim d1 = p(1, n) - p(1, n - 1)
            Dim d2 = p(2, n) - p(2, n - 1)
            f = p(2, n - 1) + dt * d2 / d1
            f = xmag * f
            Return
30:         If nstep < ntime Then ierr = 1
            Dim nm1 = numlp - 1
            dt = tau - p(1, nm1)
            d1 = p(1, numlp) - p(1, nm1)
            d2 = p(2, numlp) - p(2, nm1)
            f = p(2, nm1) + dt * d2 / d1
            f = xmag * f
        Else
            riksf = p(1, 2)
            dt = tau - p(1, 1)
            Dim d1 = p(1, 2) - p(1, 1)
            Dim d2 = p(2, 2) - p(2, 1)
            f = p(2, 1) + dt * d2 / d1
            f = xmag * f
        End If

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine interp(p,tau,numlp,f,xmag,ierr)
'c
'      implicit double precision (a-h,o-z)                             
'c
'c===> module to interpolate load curve values
'c
'      common/bk12/dtx0,delt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
'      common/bk20/kpri,nstep,ite,ilimit,iref
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/riksw2/rlnew,alfa0,dsx,iteopt,idctrl,riksf,numspu,mthunl
'      dimension p(2,*)
'c
'      if ((mthsol.lt.6.or.mthsol.gt.12).and.delt.ge.0.0) then
'         if (tau-p(1,numlp).gt.1.0e-07) go to 30
'         do 10 m=2,numlp
'         n=m
'         if (tau-p(1,m).le.0.0) go to 20
'         if (abs(tau-p(1,m)).le.1.0e-07) go to 20
'   10    continue
'         go to 30
'   20    dt=tau-p(1,n-1)
'         d1=p(1,n)-p(1,n-1)
'         d2=p(2,n)-p(2,n-1)
'         f=p(2,n-1)+dt*d2/d1
'         f=xmag*f
'         return
'   30    if(nstep.lt.ntime) ierr=1
'         nm1=numlp-1
'         dt=tau-p(1,nm1)
'         d1=p(1,numlp)-p(1,nm1)
'         d2=p(2,numlp)-p(2,nm1)
'         f=p(2,nm1)+dt*d2/d1
'         f=xmag*f
'      else
'         riksf = p(1,2)
'         dt=tau-p(1,1)
'         d1=p(1,2)-p(1,1)
'         d2=p(2,2)-p(2,1)
'         f=p(2,1)+dt*d2/d1
'         f=xmag*f
'      endif
'      return
'      end

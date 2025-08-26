'This file contains all the methods of bfgs.f
Partial Public Class clsSolve

    'Public l20 As Integer ' YC 092018

    ''' <summary>
    ''' to compute bfgs secant update vectors
    ''' </summary>
    ''' <param name="d"></param>
    ''' <param name="w"></param>
    ''' <param name="v"></param>
    ''' <param name="_step"></param>
    ''' <param name="g"></param>
    ''' <param name="g0"></param>
    ''' <param name="dnorm"></param>
    Public Sub bfgs(ByRef d() As Double, ByRef w() As Double, ByRef v() As Double,
                    ByRef _step As Double, ByRef g As Double, ByRef g0 As Double, ByRef dnorm As Double)
        Dim anegb0(neql) As Double            ' QW 12-12-2018-
        Dim deltag, deltak, fact1, fact2, vw4, vv, ww, estcnd, dw, dv As Double
        Dim nupd As Integer

        If Not bNIKE3D Then
            If StopFEDFAA = -1 Then Return
        End If

        deltag = _step * (g0 - g)
        deltak = _step * _step * g0
        If deltag > 0.0 And deltak > 0.0 Then GoTo 20
        Call blkcpy(w, d, neql)

        '10:     Call blkcpy(d, al20(1 + nbfgs), neql)      ' YC 092018
10:     Call objComsub.ArrayInsert1Dto1D(d, neql, al20, (1 + nbfgs) - 1)

        If Not bNIKE3D Then
            FEDFAAStopped = 0
            Call WINYIELD(StopFEDFAA, FEDFAAStopped)
            If StopFEDFAA = -1 Then Return
        End If

        nupd = 0
        GoTo 60
20:     nupd = 1
        fact1 = 1.0 + _step * Math.Sqrt(deltag / deltak)
        fact2 = -_step / deltag

        'Call blkcpy(al20(1 + nbfgs), v, neql)  ' YC 092018
        Call objComsub.ArrayExtract1Dfrom1D(al20, (1 + nbfgs) - 1, v, neql)

        Call blkcpy(w, anegb0, neql)

        If Not bNIKE3D Then
            FEDFAAStopped = 0
            Call WINYIELD(StopFEDFAA, FEDFAAStopped)
            If StopFEDFAA = -1 Then Return
        End If

        Dim i As Integer ' YC 102418

        'For i = 0 To neql - 1  ' YC 102418
        For i = 1 To neql
            v(i) = fact1 * v(i) - w(i)
        Next

        'For i = 0 To neql - 1 ' YC 102418
        For i = 1 To neql
            w(i) = fact2 * d(i)
        Next
        Call blkcpy(anegb0, d, neql)
        vw4 = 4.0 * fact2 * (fact1 * g0 - g) + 4.0
        vv = dotprd(v, v)
        ww = Math.Pow((dnorm / deltag), 2)
        estcnd = Math.Pow((Math.Sqrt(vv * ww) + Math.Sqrt(Math.Abs(vv * ww + vw4))), 2) / Math.Abs(vw4)
        If estcnd >= 100000.0 Then GoTo 10


        ' YC 092018
        'Call blkcpy(w, al20(1 + nbfgs), neql)
        'Call blkcpy(v, al20(1 + nbfgs + neql), neql)
        'Call blkcpy(d, al20(1 + nbfgs + 2 * neql), neql)
        Call objComsub.ArrayInsert1Dto1D(w, neql, al20, (1 + nbfgs) - 1)
        Call objComsub.ArrayInsert1Dto1D(v, neql, al20, (1 + nbfgs + neql) - 1)
        Call objComsub.ArrayInsert1Dto1D(d, neql, al20, (1 + nbfgs + 2 * neql) - 1)
        ' YC 092018 END


        dw = fact2 * g

        If Not bNIKE3D Then
            FEDFAAStopped = 0
            Call WINYIELD(StopFEDFAA, FEDFAAStopped)
            If StopFEDFAA = -1 Then Return
        End If

        'For i = 0 To neql - 1  ' YC 102418
        For i = 1 To neql
            d(i) = d(i) + dw * v(i)
        Next
60:     If numupd = 0 Then GoTo 80

        'For j = 0 To numupd - 1    ' YC 102418
        For j = 1 To numupd
            'locat = l20 + 2 * neql * (numupd - j)


            ' YC 092018
            'Call blkcpy(al20(1 + 2 * neql * (numupd - j)), w, neql)
            'Call blkcpy(al20(1 + 2 * neql * (numupd - j) + neql), v, neql)
            Call objComsub.ArrayExtract1Dfrom1D(al20, (1 + 2 * neql * (numupd - j)) - 1, w, neql)
            Call objComsub.ArrayExtract1Dfrom1D(al20, (1 + 2 * neql * (numupd - j) + neql) - 1, v, neql)
            ' YC 092018 END


            If Not bNIKE3D Then
                FEDFAAStopped = 0
                Call WINYIELD(StopFEDFAA, FEDFAAStopped)
                If StopFEDFAA = -1 Then Return
            End If

            dw = dotprd(d, w)

            'For i = 0 To neql - 1 ' YC 102418
            For i = 1 To neql
                d(i) = d(i) + dw * v(i)
            Next
        Next
80:     Call bsolvr(d, 3, 8)

        If Not bNIKE3D Then
            FEDFAAStopped = 0
            Call WINYIELD(StopFEDFAA, FEDFAAStopped)
            If StopFEDFAA = -1 Then Return
        End If

        numupd = numupd + nupd
        If numupd = 0 Then Return
        nbfgs = 2 * neql * numupd

        'For j = 0 To numupd - 1  ' YC 102418
        For j = 1 To numupd
            'locat = l20 + 2 * neql * (j - 1)


            ' YC 092018
            'Call blkcpy(al20(1 + 2 * neql * (j - 1)), w, neql)
            'Call blkcpy(al20(1 + 2 * neql * (j - 1) + neql), v, neql)
            Call objComsub.ArrayExtract1Dfrom1D(al20, (1 + 2 * neql * (j - 1)) - 1, w, neql)
            Call objComsub.ArrayExtract1Dfrom1D(al20, (1 + 2 * neql * (j - 1) + neql) - 1, v, neql)
            ' YC 092018 END


            dv = dotprd(d, v)

            'For i = 0 To neql - 1 ' YC 102418
            For i = 1 To neql
                d(i) = d(i) + dv * w(i)
            Next
        Next


    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine bfgs(d,w,v,step,g,g0,dnorm)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'c
'c===> module to compute bfgs secant update vectors
'c
'	common/block02/bNIKE3D, bNike3dMsg
'	logical bNIKE3D, bNike3dMsg
'	common/block03/StopFEDFAA, FEDFAAStopped	! See WinYield.for
'	INTEGER(4) StopFEDFAA, FEDFAAStopped	! See WinYield.for !ikawa 09-09-03
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk07/l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l00d,l00e
'      common/bk26/ntlen,ntlenf
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/bk40/nbfgs,numupd
'      common/bkneq/neql
'      dimension d(*),w(*),v(*)
'c      
'      common/aa/anegb1(7508928),anegb2(7508928)
'      common/a20/al20(3348000)
'c      
'	if (not (bNike3d)) then !ikawa 09/22/03
'      IF( StopFEDFAA .eq. -1 ) return
'	end if
'
'
'      deltag=step*(g0-g)
'      deltak=step*step*g0
'      if (deltag.gt.0.0.and.deltak.gt.0.0) go to 20
'      call blkcpy (w,d,neql)
'10    call blkcpy(d,al20(1+nbfgs),neql)
'
'
'	if (not (bNike3d)) then
'      FEDFAAStopped = 0 !ikawa 09-09-03
'      CALL WINYIELD( StopFEDFAA, FEDFAAStopped )
'      IF( StopFEDFAA .eq. -1 ) return
'	end if
'
'
'      nupd=0
'      go to 60
'   20 nupd=1
'      fact1=1.0+step*sqrt(deltag/deltak)
'      fact2=-step/deltag
'      call blkcpy(al20(1+nbfgs),v,neql)
'      call blkcpy (w,anegb1,neql)
'c
'	if (not (bNike3d)) then
'      FEDFAAStopped = 0 !ikawa 09-09-03
'      CALL WINYIELD( StopFEDFAA, FEDFAAStopped )
'      IF( StopFEDFAA .eq. -1 ) return
'	end if
'c
'      do 30 i=1,neql
'   30 v(i)=fact1*v(i)-w(i)
'      do 40 i=1,neql
'   40 w(i)=fact2*d(i)
'      call blkcpy (anegb1,d,neql)
'      vw4=4.*fact2*(fact1*g0-g)+4.0
'      vv=dotprd (v,v)
'      ww=(dnorm/deltag)**2
'      estcnd =((sqrt(vv*ww)+sqrt(abs(vv*ww+vw4)))**2)/abs(vw4)
'      if (estcnd .ge.1.0e+05) go to 10
'      call blkcpy(w,al20(1+nbfgs),neql)
'      call blkcpy(v,al20(1+nbfgs+neql),neql)
'      call blkcpy(d,al20(1+nbfgs+2*neql),neql)
'      dw=fact2*g
'
'	if (not (bNike3d)) then
'      FEDFAAStopped = 0 !ikawa 09-09-03
'      CALL WINYIELD( StopFEDFAA, FEDFAAStopped )
'      IF( StopFEDFAA .eq. -1 ) return
'	end if
'
'      do 50 i=1,neql
'   50 d(i)=d(i)+dw*v(i)
'   60 if (numupd.eq.0) go to 80
'      do 70 j=1,numupd
'      locat=l20+2*neql*(numupd-j)
'      call blkcpy(al20(1+2*neql*(numupd-j)),w,neql)
'      call blkcpy(al20(1+2*neql*(numupd-j)+neql),v,neql)
'
'	if (not (bNike3d)) then
'      FEDFAAStopped = 0 !ikawa 09-09-03
'      CALL WINYIELD( StopFEDFAA, FEDFAAStopped )
'      IF( StopFEDFAA .eq. -1 ) return
'	end if
'
'      dw=dotprd (d,w)
'      do 70 i=1,neql
'   70 d(i)=d(i)+dw*v(i)
'   80 call bsolvr (d,3,8)
'
'	if (not (bNike3d)) then
'      FEDFAAStopped = 0 !ikawa 09-09-03
'      CALL WINYIELD( StopFEDFAA, FEDFAAStopped )
'      IF( StopFEDFAA .eq. -1 ) return
'	end if
'
'      numupd=numupd+nupd
'      if (numupd.eq.0) go to 100
'      nbfgs=2*neql*numupd
'      do 90 j=1,numupd
'      locat=l20+2*neql*(j-1)
'      call blkcpy(al20(1+2*neql*(j-1)),w,neql)
'      call blkcpy(al20(1+2*neql*(j-1)+neql),v,neql)
'      dv=dotprd (d,v)
'      do 90 i=1,neql
'   90 d(i)=d(i)+dv*w(i)
'  100 return
'      end

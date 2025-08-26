'This file contains all the methods of cklc.f
Partial Public Class clsInitial

    ''' <summary>
    ''' check for applied load at time = 0
    ''' </summary>
    ''' <param name="ncnl"></param>
    ''' <param name="lcnl"></param>
    ''' <param name="npr"></param>
    ''' <param name="lcpr"></param>
    ''' <param name="npd"></param>
    ''' <param name="lcpd"></param>
    ''' <param name="fval"></param>
    ''' <param name="told"></param>
    ''' <param name="time"></param>
    ''' <param name="dt"></param>
    ''' <param name="itemp"></param>
    ''' <param name="nstep"></param>
    Public Sub cklc(ByRef ncnl As Integer, ByRef lcnl() As Integer, ByRef npr As Integer, ByRef lcpr() As Integer,
                    ByRef npd As Integer, ByRef lcpd() As Integer, ByRef fval() As Double, ByRef told As Double,
                    ByRef time As Double, ByRef dt As Double, ByRef itemp As Integer, ByRef nstep As Integer)

        '... concentrated nodal loads ...

        If ncnl <> 0 Then
            For i = 1 To ncnl
                If fval(lcnl(i)) <> 0.0 Then GoTo 100
            Next
        End If

        'c... pressure ...
        If npr <> 0 Then
            For i = 1 To npr
                If fval(lcpr(i)) <> 0.0 Then GoTo 100
            Next
        End If

        '... prescribed displacement ...
        If npd <> 0 Then
            For i = 1 To npd
                If fval(lcpd(i)) <> 0.0 Then GoTo 100
            Next
        End If

        Return

100:
        time = -dt
        nstep = -2
        If itemp <> 0 Then told = -dt

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine cklc(ncnl,lcnl,npr,lcpr,npd,lcpd,
'     &                fval,told,time,dt,itemp,nstep)
'c
'
'      implicit double precision (a-h,o-z)                       
'c
'c===> module to check for applied load at time = 0
'c
'      common/bk17/nthpx,nthpy,nthpz,nthsx,nthsy,nthsz,iacflg
'      dimension lcnl(*),lcpr(*),lcpd(*),fval(*)
'c
'c   ... concentrated nodal loads ...
'
'      if (ncnl.ne.0) then
'        do 10 i=1,ncnl
'        if ( fval(lcnl(i)).ne.0.0 ) goto 100
'   10   continue
'      endif
'c
'c   ... pressure ...
'      if (npr.ne.0) then
'        do 20 i=1,npr
'        if ( fval(lcpr(i)).ne.0.0 ) goto 100
'   20   continue
'      endif
'c
'c   ... prescribed displacement ...
'      if (npd.ne.0) then
'        do 30 i=1,npd
'        if ( fval(lcpd(i)).ne.0.0 ) goto 100
'   30   continue
'      endif
'c
'c   ... base acceleration and angular velocity ...
'      if (nthpx.ne.0) then
'        if (fval(nthpx).ne.0.0) goto 100
'      endif
'      if (nthpy.ne.0) then
'        if (fval(nthpy).ne.0.0) goto 100
'      endif
'      if (nthpz.ne.0) then
'        if (fval(nthpz).ne.0.0) goto 100
'      endif
'      if (nthsx.ne.0) then
'        if (fval(nthsx).ne.0.0) goto 100
'      endif
'      if (nthsy.ne.0) then
'        if (fval(nthsy).ne.0.0) goto 100
'      endif
'      if (nthsz.ne.0) then
'        if (fval(nthsz).ne.0.0) goto 100
'      endif
'c
'      return
'c
'  100 continue
'      time = -dt
'      nstep = -2
'      if (itemp.ne.0) told = -dt
'      return
'      end

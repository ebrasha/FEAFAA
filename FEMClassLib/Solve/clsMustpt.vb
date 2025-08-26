
'This file contains all the methods of mustpt.f
Partial Public Class clsSolve


    
    Public lcmust As Integer

    Public dtmin As Double


    Private dtmax As Double



    ''' <summary>
    ''' evaluate must points for auto time steppers
    ''' </summary>
    ''' <param name="p"></param>
    ''' <param name="npc"></param>
    ''' <param name="time"></param>
    ''' <param name="lterm"></param>
    ''' <param name="lauto"></param>
    Public Sub mustpt(ByRef p() As Double, ByRef npc() As Integer, ByRef time As Double,
                      ByRef lterm As Boolean, ByRef lauto As Boolean)

        Dim lchg As Boolean
        Dim tmust As Double
        'TODO check the inital values
        Dim dtmax0 = 0.0, dtmax1 = 0.0, dtminc = 0.0

        If lterm OrElse (Not lauto) Then Return

        lchg = False

        If lcmust < 0 Then
            tmust = termtm
            GoTo 100
        End If

        '   ... evaluate must point load curve ...
        Dim loc = npc(lcmust)

        'Dim npt = (npc(lcmust + 1) - loc) / 2
        Dim npt As Integer = (npc(lcmust + 1) - loc) / 2  ' YC 102418

        dtmax = Math.Abs(dtmax0)

        '   ... enforce current dtmax ...
        If dt > dtmax Then
            dt = dtmax
            lchg = True
        End If

        If dtmin > dtmax Then dtminc = dtmax

        '   ... enforce must point, avoid tiny step (less that dtminc) next time ...
100:    If time < tmust AndAlso (time + dt) > tmust Then
            dt = tmust - time
            dtmax = Math.Abs(dtmax1)
            lplotm = dtmax0 > 0
            lchg = True
        ElseIf (time + dt) < tmust AndAlso (time + dt + dtminc) > tmust Then
            Dim eps = (time - dt) / 100
            dt = (tmust - time + eps) / 2
            lchg = True
        End If

        '   ... check against termination time ...
        If time < termtm AndAlso (time + dt) >= termtm Then
            dt = termtm - time
            lchg = True
            lterm = True
            lplotm = True
        End If

        '   ... change Newmark parameters if necessary ...
        If (lchg) Then
            'TODO - Missing Sub
            Call chgint()
        End If

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine mustpt(p,npc,time,lterm,lauto)
'c
'      implicit double precision (a-h,o-z)                            
'c
'c===> module to evaluate must points for auto time steppers
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk12/dtx0,dt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
'      logical lplotm
'      common/automt/dtmin,dtmax,termtm,mxback,iauto,irfwin,lcmust,lplotm
'      logical lchg,lterm,lauto
'      dimension p(*),npc(*)
'c
'      if (lterm .or. (.not. lauto)) return
'      lchg=.false.
'      if (lcmust.le.0) then
'        tmust=termtm
'        goto 100
'      endif
'c
'c   ... evaluate must point load curve ...
'      loc=npc(lcmust)
'      npt=(npc(lcmust+1)-loc)/2
'
'      dtmax=abs(dtmax0)
'c
'c   ... enforce current dtmax ...
'      if(dt.gt.dtmax)then
'        dt=dtmax
'        lchg=.true.
'      endif
'
'      if(dtmin.gt.dtmax) dtminc = dtmax
'c
'c   ... enforce must point, avoid tiny step (less that dtminc) next time ...
'  100 if( time.lt.tmust .and. (time+dt).gt.tmust ) then
'        dt=tmust-time
'        dtmax=abs(dtmax1)
'        lplotm=(dtmax0.gt.0)
'        lchg=.true.
'      elseif( (time+dt).lt.tmust .and. (time+dt+dtminc).gt.tmust ) then
'        eps=(time-dt)/100
'        dt=(tmust-time+eps)/2.
'        lchg=.true.
'      endif
'c
'c   ... check against termination time ...
'      if(time.lt.termtm .and. (time+dt).ge.termtm) then
'        dt=termtm-time
'        lchg=.true.
'        lterm=.true.
'        lplotm=.true.
'      endif
'c
'c   ... change Newmark parameters if necessary ...
'      if (lchg) then
'        call chgint
'      endif
'c
'      return
'c
'      end

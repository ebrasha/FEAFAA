'This file contains all the methods of temth.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to set nodal temperatures based on load curve "itemp"
    ''' </summary>
    ''' <param name="told"></param>
    ''' <param name="temo"></param>
    ''' <param name="tnew"></param>
    ''' <param name="temp"></param>
    ''' <param name="npc"></param>
    ''' <param name="p"></param>
    ''' <param name="lcc"></param>
    ''' <param name="tt"></param>
    ''' <param name="tmode"></param>
    ''' <param name="tbase"></param>
    Public Sub temth(told As Double, temo() As Double, tnew As Double, temp() As Double,
                     npc() As Integer, p() As Double, lcc As Integer,
                     tt As Double, tmode() As Double, tbase() As Double)

        Dim ierr = 0
        Dim xmag = 1.0
        Dim loc = npc(lcc)

        'Dim npoint = (npc(lcc + 1) - loc) / 2  ' YC 121219
        Dim npoint As Integer = (npc(lcc + 1) - loc) / 2

        Dim f = 0.0

        ' YC 121219
        'TODO - PASS PRPOER ARRAYS
        'Call objIntial.interp(p(loc), tt, npoint, f, xmag, ierr)  

        Dim p_loc_2D(2, npoint) As Double
        Call objComsub.ArrayConvert1Dto2D(p.Skip(loc - 1).ToArray, p_loc_2D, 2, npoint)
        Call objInit.interp(p_loc_2D, tt, npoint, f, xmag, ierr)
        ' YC 121219 END

        If ierr = 1 Then
            Dim fx = f
            If Math.Abs(xmag) > 0.0000001 Then fx = fx / xmag
            If tt > 1.00001 * extime Then
                extime = tt
                Dim msg100 = String.Format(" **warning** load curve #{0} extended to ({1}, {2})",
                             lcc.ToString("###"), tt.ToString("E6"), fx.ToString("E6"))
            End If
        End If

        told = tnew

        Dim i As Integer ' YC 121219

        For i = 1 To numnp
            temo(i) = temp(i)
        Next

        tnew = tt
        If itread = 0 Then
            For i = 1 To numnp
                temp(i) = f
            Next
        Else
            For i = 1 To numnp
                temp(i) = f * tmode(i) + tbase(i)
            Next
        End If

    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine temth(told,temo,tnew,temp,npc,p,lcc,tt,tmode,tbase)
'c
'
'      implicit double precision (a-h,o-z)                            
'c
'c===> module to set nodal temperatures based on load curve "itemp"
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/tin/itread,itrp1,itrp2
'      common/ldcex/extime
'      dimension temo(*),temp(*),npc(*),p(*),tmode(*),tbase(*)
'c
'	ierr=0
'      xmag=1.0
'      loc=npc(lcc)
'      npoint=(npc(lcc+1)-loc)/2
'c
'      f=0.0
'      call interp(p(loc),tt,npoint,f,xmag,ierr)
'      if(ierr.eq.1) then
'         fx=f
'         if(abs(xmag).gt.1.e-7)fx=fx/xmag
'         if(tt.gt.1.00001*extime)then
'            extime=tt
'            write(lutty,100)lcc,tt,fx
'            write(luo,100)lcc,tt,fx
'         endif
'      endif
'c
'      told = tnew
'      do 10 i=1,numnp
'      temo(i)=temp(i)
'   10 continue
'c
'      tnew = tt
'      if (itread.eq.0) then
'      do 20 i=1,numnp
'   20 temp(i)=f
'      else
'      do 30 i=1,numnp
'   30 temp(i)=f*tmode(i)+tbase(i)
'      endif
'      return
'
'c
'  100 format(' **warning** load curve #',i3,' extended to (',e12.5
'     &      ,' , ',e12.5,')')
'      end

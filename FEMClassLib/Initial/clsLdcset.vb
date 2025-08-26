
'This file contains all the methods of ldcset.f
Partial Public Class clsInitial

    ''' <summary>
    ''' compute value of load curves at time value 'tt'
    ''' </summary>
    ''' <param name="fval"></param>
    ''' <param name="npc"></param>
    ''' <param name="p"></param>
    ''' <param name="tt"></param>
    Public Sub ldcset(ByVal nlcur As Integer, ByRef fval() As Double, ByRef npc() As Integer, ByRef p() As Double, ByRef tt As Double)

        If nlcur = 0 Then Return
        Dim loc As Integer
        For n = 1 To nlcur
            fval(n + nlcur) = fval(n)
            Dim ierr = 0
            Dim xmag = 1.0
            loc = npc(n)
            Dim npoint = (npc(n + 1) - loc) / 2
            Dim fvl As Double = 0
            Call interp(p.Skip(loc).ToArray, tt, npoint, fvl, xmag, ierr)
            fval(n) = fvl
            If ierr = 1 Then
                Dim fx = fvl
                If Math.Abs(xmag) > 0.0000001 Then fx = fx / xmag
                If tt > 1.00001 * extime Then
                    extime = tt
                    Dim msg100 = String.Format(" **warning** load curve #{0} extended to ({1} , {2})",
                                               n.ToString("###"), tt.ToString("e5"), fx.ToString("e5"))
                    '            write(lutty,100)n,tt,fx
                    '            write(luo,100)n,tt,fx
                End If
            End If
        Next
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine ldcset (fval,npc,p,tt)
'c
'      implicit double precision (a-h,o-z)                            
'c
'c===> module to compute value of load curves at time value 'tt'
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk16/nlcur,nptst,nload,nptm
'      common/ldcex/extime
'      dimension fval(*),npc(*),p(*)
'c
'      if(nlcur.eq.0) return
'c
'      do 10 n=1,nlcur
'      fval(n+nlcur)=fval(n)
'      ierr=0
'      xmag=1.0
'      loc=npc(n)
'      npoint=(npc(n+1)-loc)/2
'      call interp(p(loc),tt,npoint,fvl,xmag,ierr)
'      fval(n)=fvl
'      if(ierr.eq.1)then
'         fx=fvl
'         if(abs(xmag).gt.1.e-7) fx=fx/xmag
'         if(tt.gt.1.00001*extime)then
'            extime=tt
'            write(lutty,100)n,tt,fx
'            write(luo,100)n,tt,fx
'         endif
'      endif
' 10   continue
'      return
'c
'  100 format(' **warning** load curve #',i3,' extended to (',e12.5
'     &       ,' , ',e12.5,')')
'      end

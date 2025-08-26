'This file contains all the methods of initde.f
Partial Public Class clsInitial

    ''' <summary>
    ''' initialize discrete elements
    ''' </summary>
    ''' <param name="npc"></param>
    ''' <param name="p"></param>
    ''' <param name="mtypde"></param>
    ''' <param name="cmde"></param>
    ''' <param name="ixde"></param>
    ''' <param name="sclf"></param>
    ''' <param name="dehv"></param>
    ''' <param name="nmelde"></param>
    ''' <param name="x"></param>
    ''' <param name="kht"></param>
    ''' <param name="idp"></param>
    ''' <param name="numnp"></param>
    ''' <param name="nmmat"></param>
    Public Sub initde(npc() As Integer, pld() As Double, mtypde() As Integer,
                      cmde(,) As Double, ixde(,) As Integer, sclf() As Double,
                      dehv(,) As Double, nmelde As Integer, x(,) As Double,
                      kht() As Integer, idp(,) As Integer, numnp As Integer,
                      nmmat As Integer)

        Dim lm(11), loc As Integer

        For i = 1 To nmelde
            Dim matnum = ixde(3, i)
            Dim mtyp = mtypde(matnum)
            If mtyp = 4 Then
                dehv(1, i) = Math.Sqrt(Math.Pow((x(1, ixde(1, i)) - x(1, ixde(2, i))), 2) +
                      Math.Pow((x(2, ixde(1, i)) - x(2, ixde(2, i))), 2) +
                      Math.Pow((x(3, ixde(1, i)) - x(3, ixde(2, i))), 2))
            ElseIf mtyp = 6 Then
                dehv(7, i) = cmde(3, matnum) * sclf(i)
                dehv(8, i) = cmde(4, matnum) * sclf(i)
                Dim ncur As Integer = Math.Round(cmde(0, matnum))
                loc = npc(ncur)
                Dim npoint As Integer = (npc(ncur + 1) - loc) / 2
                Dim zr As Double = 0
                'TODO
                Dim d2d1 As Double
                Call itrpd2(pld(loc), dehv(2, i), npoint, dehv(8, i), sclf(i), 2, zr, d2d1)
                Call itrpd2(pld(loc), dehv(3, i), npoint, dehv(9, i), sclf(i), 2, zr, d2d1)
            End If
            'c
            'c       ... load translational dof's only ...
            '        call unpkid(lm(1),idp(1,ixde(1,i)),1)
            Call com.unpkid(lm.Skip(1).ToArray(), idp, 1, ixde(1, i), 1)
            '        call unpkid(lm(4),idp(1,ixde(2,i)),1)
            Call com.unpkid(lm.Skip(4).ToArray(), idp, 1, ixde(2, i), 1)
            Dim j = 6
            Call colht(kht, j, lm)
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine initde(npc,p,mtypde,cmde,ixde,sclf,dehv,nmelde,x,kht,
'     &                  idp,numnp,nmmat)
'c
'      implicit double precision (a-h,o-z)                           
'c
'c===> module to initialize discrete elements
'c
'      dimension npc(*),p(*),mtypde(*),cmde(8,*),ixde(3,*),sclf(1),
'     &          dehv(11,1),x(3,*),kht(*),lm(12),idp(6,*)               
'      
'      do 20 i=1,nmelde
'      matnum=ixde(3,i)
'      mtyp  =mtypde(matnum)
'      if(mtyp.eq.4) then
'        dehv(1,i)=sqrt( ( x(1,ixde(1,i)) - x(1,ixde(2,i)) )**2 +
'     &                  ( x(2,ixde(1,i)) - x(2,ixde(2,i)) )**2 +
'     &                  ( x(3,ixde(1,i)) - x(3,ixde(2,i)) )**2 )
'      elseif (mtyp.eq.6) then
'       dehv(8,i)=cmde(4,matnum)*sclf(i)
'       dehv(9,i)=cmde(5,matnum)*sclf(i)
'       ncur=nint(cmde(1,matnum))
'       loc=npc(ncur)
'       npoint=(npc(ncur+1)-loc)/2
'       zr = 0.
'       call itrpd2(p(loc),dehv(2,i),npoint,dehv(8,i),sclf(i),2,zr,d2d1)
'       call itrpd2(p(loc),dehv(3,i),npoint,dehv(9,i),sclf(i),2,zr,d2d1)
'      endif
'c
'c       ... load translational dof's only ...
'        call unpkid(lm(1),idp(1,ixde(1,i)),1)
'        call unpkid(lm(4),idp(1,ixde(2,i)),1)
'        j=6
'
'        call colht(kht,j,lm)
'
'   20 continue
'      return
'      end

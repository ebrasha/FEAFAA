'This file contains all the methods of loadpr.f
Partial Public Class clsSolve

    'added by YC 0092018
    Public numpc As Integer

    Public pfac, extime As Double

    Public p1(,), p2(,) As Double




    'overload 1D-2D input
    Public Sub loadpr(ByRef idp() As Integer, ByRef u() As Double, ByRef r() As Double,
                      ByRef npc() As Integer, ByRef p() As Double, ByRef xn(,) As Double,
                      ByRef yn(,) As Double, ByRef zn(,) As Double, ByRef pmult(,) As Double,
                      ByRef nodes(,) As Integer, ByRef strt() As Double, ByRef lc() As Integer,
                      ByRef tt As Double, ByRef x() As Double, ByRef nmmat As Integer)

        Dim idp_ind = idp.Count
        Dim idp_2d(idp_ind - 1, 8) As Integer
        For i = 0 To idp_ind - 1
            idp_2d(i, 0) = idp(i)
        Next
        'x not used - pass dummy
        Dim x_2d(1, 1) As Double

        loadpr(idp_2d, u, r, npc, p, xn, yn, zn, pmult, nodes, strt, lc, tt, x_2d, nmmat)
    End Sub

    ''' <summary>
    ''' add pressure card constributions to residual
    ''' </summary>
    ''' <param name="idp"></param>
    ''' <param name="u"></param>
    ''' <param name="r"></param>
    ''' <param name="npc"></param>
    ''' <param name="p"></param>
    ''' <param name="xn"></param>
    ''' <param name="yn"></param>
    ''' <param name="zn"></param>
    ''' <param name="pmult"></param>
    ''' <param name="nodes"></param>
    ''' <param name="strt"></param>
    ''' <param name="lc"></param>
    ''' <param name="tt"></param>
    ''' <param name="x"></param>
    ''' <param name="nmmat"></param>
    Public Sub loadpr(ByRef idp(,) As Integer, ByRef u() As Double, ByRef r() As Double,
                      ByRef npc() As Integer, ByRef p() As Double, ByRef xn(,) As Double,
                      ByRef yn(,) As Double, ByRef zn(,) As Double, ByRef pmult(,) As Double,
                      ByRef nodes(,) As Integer, ByRef strt() As Double, ByRef lc() As Integer,
                      ByRef tt As Double, ByRef x(,) As Double, ByRef nmmat As Integer)


        Dim xx1(3), xx2(3), xx3(3), pmlt(7), frb(5), fx, f As Double
        Dim ndes(7), id(5) As Integer
        Dim j, k1, k2, k3 As Integer
        Dim h0(4, 4), p1(4, 4), p2(4, 4) As Double   ' QW 12-12-2018- work later
        If numpc = 0 Then Return

        Dim n As Integer ' by YC 092018

        'For n = 0 To numpc     ' YC 102418
        For n = 1 To numpc

            Dim xmag = 1.0
            Dim ierr = 0
            Dim time = tt - strt(n)
            If time < 0.0 Then Continue For
            Dim lcc = lc(n)

            If lcc = lcspf Then xmag = pfac

            Dim loc = npc(lcc)
            Dim npoint = (npc(lcc + 1) - loc) / 2
            '      call interp(p(loc),time,npoint,f,xmag,ierr)
            objInit.interp(p.Skip(loc), time, npoint, f, xmag, ierr)
            If ierr = 1 Then
                fx = f
                If Math.Abs(xmag) > 0.0000001 Then fx = fx / xmag
                If time > 1.00001 * extime Then
                    extime = time
                    'TODO there is no format 200
                    '            write(lutty,200)lcc,time,fx
                    '            write(luo,200)lcc,time,fx
                End If
            End If

            'For i = 0 To 3 ' YC 102418
            For i = 1 To 4
                j = nodes(i, n)
                ndes(i) = j

                'Call clsInput.unpkid(id, idp, 0, j - 1, 1)   'by YC 092018, YC 102418
                Call unpkid(id, idp, 1, j, 1)

                ' YC 102418
                'k1 = id(0)
                'k2 = id(1)
                'k3 = id(2)
                k1 = id(1)
                k2 = id(2)
                k3 = id(3)
                ' YC 102418 END

                xx1(i) = xn(i, n)
                xx2(i) = yn(i, n)
                xx3(i) = zn(i, n)
                If k1 <> 0 Then xx1(i) = xx1(i) + u(k1)
                If k2 <> 0 Then xx2(i) = xx2(i) + u(k2)
                If k3 <> 0 Then xx3(i) = xx3(i) + u(k3)
                pmlt(i) = pmult(i, n)
            Next

            ' YC 102418
            'For i = 0 To 3     
            'Dim fs1 = p1(0, i) * xx1(0) + p1(1, i) * xx1(1) + p1(2, i) * xx1(2) + p1(3, i) * xx1(3)
            'Dim fs2 = p1(0, i) * xx2(0) + p1(1, i) * xx2(1) + p1(2, i) * xx2(2) + p1(3, i) * xx2(3)
            'Dim fs3 = p1(0, i) * xx3(0) + p1(1, i) * xx3(1) + p1(2, i) * xx3(2) + p1(3, i) * xx3(3)
            'Dim ft1 = p2(0, i) * xx1(0) + p2(1, i) * xx1(1) + p2(2, i) * xx1(2) + p2(3, i) * xx1(3)
            'Dim ft2 = p2(0, i) * xx2(0) + p2(1, i) * xx2(1) + p2(2, i) * xx2(2) + p2(3, i) * xx2(3)
            'Dim ft3 = p2(0, i) * xx3(0) + p2(1, i) * xx3(1) + p2(2, i) * xx3(2) + p2(3, i) * xx3(3)
            'Dim prs = h(0, i) * pmlt(0) + h(1, i) * pmlt(1) + h(2, i) * pmlt(2) + h(3, i) * pmlt(3)
            For i = 1 To 4
                Dim fs1 = p1(1, i) * xx1(1) + p1(2, i) * xx1(2) + p1(3, i) * xx1(3) + p1(4, i) * xx1(4)
                Dim fs2 = p1(1, i) * xx2(1) + p1(2, i) * xx2(2) + p1(3, i) * xx2(3) + p1(4, i) * xx2(4)
                Dim fs3 = p1(1, i) * xx3(1) + p1(2, i) * xx3(2) + p1(3, i) * xx3(3) + p1(4, i) * xx3(4)
                Dim ft1 = p2(1, i) * xx1(1) + p2(2, i) * xx1(2) + p2(3, i) * xx1(3) + p2(4, i) * xx1(4)
                Dim ft2 = p2(1, i) * xx2(1) + p2(2, i) * xx2(2) + p2(3, i) * xx2(3) + p2(4, i) * xx2(4)
                Dim ft3 = p2(1, i) * xx3(1) + p2(2, i) * xx3(2) + p2(3, i) * xx3(3) + p2(4, i) * xx3(4)
                Dim prs = h0(1, i) * pmlt(1) + h0(2, i) * pmlt(2) + h0(3, i) * pmlt(3) + h0(4, i) * pmlt(4)
                ' YC 102418 END

                prs = f * prs
                Dim tr1 = (fs2 * ft3 - fs3 * ft2) * prs
                Dim tr2 = (fs3 * ft1 - fs1 * ft3) * prs
                Dim tr3 = (fs1 * ft2 - fs2 * ft1) * prs

                'For j = 0 To 3    ' YC 102418
                For j = 1 To 4
                    Dim kk = ndes(j)

                    'Call clsInput.unpkid(id, idp, 0, kk - 1, 1) 'by YC 092018, YC 102418
                    Call unpkid(id, idp, 1, kk, 1)

                    'YC 102418
                    'k1 = id(0)
                    'k2 = id(1)
                    'k3 = id(2)
                    k1 = id(1)
                    k2 = id(2)
                    k3 = id(3)
                    'YC 102418 END

                    'If k1 < neql Then     'YC 102418
                    If k1 <= neql Then
                        If k1 <> 0 Then r(k1) = r(k1) - h0(j, i) * tr1
                        If k2 <> 0 Then r(k2) = r(k2) - h0(j, i) * tr2
                        If k3 <> 0 Then r(k3) = r(k3) - h0(j, i) * tr3
                    Else

                        'YC 102418
                        'frb(0) = h(j, i) * tr1
                        'frb(1) = h(j, i) * tr2
                        'frb(2) = h(j, i) * tr3
                        'frb(3) = 0.0
                        'frb(4) = 0.0
                        'frb(5) = 0.0
                        frb(1) = h0(j, i) * tr1
                        frb(2) = h0(j, i) * tr2
                        frb(3) = h0(j, i) * tr3
                        frb(4) = 0.0
                        frb(5) = 0.0
                        frb(6) = 0.0
                        'YC 102418 END

                    End If
                Next
            Next
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine loadpr(idp,u,r,npc,p,xn,yn,zn,pmult,nodes,strt,lc,tt,
'     &                  x,nmmat)
'c
'      implicit double precision (a-h,o-z)                           
'c
'c===> module to add pressure card constributions to residual
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk15/numpc
'      common/bk16/nlcur,nptst,nload,nptm
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/bk38/h(4,4),p1(4,4),p2(4,4)
'      common/ldcex/extime
'      common/bkneq/neql
'      common/bkspf1/etarg,emin,emax,ptime,ptmin,pdmax,lcspf,nmspf,tpelmc
'      common/bkspf2/ekm1,ek,ekp1,pdkm1,pdk,pk,pkp1,pfac,neks    
'c
'      dimension idp(6,*),u(*),r(*),yn(8,*),zn(8,*),pmult(8,*),npc(*),  
'     1  xn(8,*),nodes(8,*),strt(*),p(*),xx1(4),xx2(4),xx3(4),
'     4  lc(*),pmlt(8),ndes(8),id(6),frb(6),x(3,numnp)
'c
'      if(numpc.eq.0) return
'c
'   20 do 150 n=1,numpc
'      xmag=1.0
'      ierr=0
'      time=tt-strt(n)
'      if(time.lt.0.0) go to 150
'      lcc=lc(n)
'c
'      if (lcc.eq.lcspf) xmag=pfac
'c
'      loc=npc(lcc)
'      npoint=(npc(lcc+1)-loc)/2
'      call interp(p(loc),time,npoint,f,xmag,ierr)
'      if(ierr.eq.1) then
'         fx=f
'         if(abs(xmag).gt.1.e-7)fx=fx/xmag
'         if(time.gt.1.00001*extime)then
'            extime=time
'            write(lutty,200)lcc,time,fx
'            write(luo,200)lcc,time,fx
'         endif
'      endif
'   30 do 40 i=1,4
'      j=nodes(i,n)
'      ndes(i)=j
'      call unpkid(id,idp(1,j),1)
'      k1=id(1)
'      k2=id(2)
'      k3=id(3)
'      xx1(i)=xn(i,n)
'      xx2(i)=yn(i,n)
'      xx3(i)=zn(i,n)
'      if(k1.ne.0) xx1(i)=xx1(i)+u(k1)
'      if(k2.ne.0) xx2(i)=xx2(i)+u(k2)
'      if(k3.ne.0) xx3(i)=xx3(i)+u(k3)
' 40   pmlt(i)=pmult(i,n)
'      do 140 i=1,4
'      fs1=p1(1,i)*xx1(1)+p1(2,i)*xx1(2)+p1(3,i)*xx1(3)+p1(4,i)*xx1(4)
'      fs2=p1(1,i)*xx2(1)+p1(2,i)*xx2(2)+p1(3,i)*xx2(3)+p1(4,i)*xx2(4)
'      fs3=p1(1,i)*xx3(1)+p1(2,i)*xx3(2)+p1(3,i)*xx3(3)+p1(4,i)*xx3(4)
'      ft1=p2(1,i)*xx1(1)+p2(2,i)*xx1(2)+p2(3,i)*xx1(3)+p2(4,i)*xx1(4)
'      ft2=p2(1,i)*xx2(1)+p2(2,i)*xx2(2)+p2(3,i)*xx2(3)+p2(4,i)*xx2(4)
'      ft3=p2(1,i)*xx3(1)+p2(2,i)*xx3(2)+p2(3,i)*xx3(3)+p2(4,i)*xx3(4)
'      prs=h(1,i)*pmlt(1)+h(2,i)*pmlt(2)+h(3,i)*pmlt(3)+h(4,i)*pmlt(4)
'      prs=f*prs
'      tr1=(fs2*ft3-fs3*ft2)*prs
'      tr2=(fs3*ft1-fs1*ft3)*prs
'      tr3=(fs1*ft2-fs2*ft1)*prs
'      do 130 j=1,4
'      kk=ndes(j)
'      call unpkid(id,idp(1,kk),1)
'      k1=id(1)
'      k2=id(2)
'      k3=id(3)
'      if (k1.le.neql) then
'        if(k1.ne.0)  r(k1)=r(k1)-h(j,i)*tr1
'        if(k2.ne.0)  r(k2)=r(k2)-h(j,i)*tr2
'        if(k3.ne.0)  r(k3)=r(k3)-h(j,i)*tr3
'      else
'        frb(1)=h(j,i)*tr1
'        frb(2)=h(j,i)*tr2
'        frb(3)=h(j,i)*tr3
'        frb(4)=0.0
'        frb(5)=0.0
'        frb(6)=0.0
'
'      endif
' 130  continue
' 140  continue
' 150  continue
'      return
'c
'      end

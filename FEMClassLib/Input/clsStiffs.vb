'This file contains all the methods of stiffs.f
Partial Public Class clsInput

    ''' <summary>
    ''' determine stiffness parameter for slide surface facets
    ''' </summary>
    ''' <param name="x"></param>
    ''' <param name="irect"></param>
    ''' <param name="stf"></param>
    ''' <param name="ibh"></param>
    ''' <param name="matype"></param>
    ''' <param name="numelh"></param>
    ''' <param name="nrt"></param>
    ''' <param name="stffac"></param>
    ''' <param name="dxavg"></param>
    ''' <param name="blkvg"></param>
    ''' <param name="stfvg"></param>
    ''' <param name="nty"></param>
    ''' <param name="nsn"></param>
    ''' <param name="nsv"></param>
    ''' <param name="rdc"></param>
    Public Sub stiffs(ByRef x(,) As Double, ByRef irect(,) As Integer, ByRef stf() As Double, ByRef ibh(,) As Integer,
                      ByRef matype() As Integer, ByRef numelh As Integer, ByRef nrt As Integer, ByRef stffac As Double,
                      ByRef dxavg As Double, ByRef blkvg As Double, ByRef stfvg As Double, ByRef nty As Integer,
                      ByRef nsn As Integer, ByRef rdc(,) As Double, ByVal n As Integer,
                      ByRef x1() As Double, ByRef x2() As Double, ByRef x3() As Double, ByVal vol As Double, ByRef emodl2 As Double)

        Static istyp As Integer
        Dim ii = 0
        Dim m, j, k, j1, myy, iy(9) As Integer
        Dim xn1(8), xc(8), yn2(8), yc(8), zn3(8), zc(8) As Double

        Dim i1 As Integer   'v3.0 003/062920-1 YC

        dxavg = 0
        blkvg = 0
        stfvg = 0

        'llnum = llnum + 1
        If istyp = 2 Then istyp = 0
        istyp = istyp + 1
        'Dim ittyp = (llnum + 1) / 2

        For ind1 = 1 To nrt
            If numelh = 0 Then GoTo 60
            For j = 1 To numelh

                m = 0
                n = 1
                j1 = j
                I1 = irect(n, ind1)
                'Call unpkid(myy, ibh(1, j), 2)
                'Call com.unpkid(myy, ibh, 1, j, 2)
                myy = ibh(1, j)
                For ii = 1 To 8
                    iy(ii) = ibh(ii + 1, j)
                Next
                k = 1
20:             If iy(k) <> I1 Then GoTo 30
                m = m + 1
                n = n + 1
                I1 = irect(n, ind1)
                k = 1
                If m = 3 Then GoTo 70
                GoTo 20
30:             k = k + 1
                If k < 9 Then GoTo 20
            Next

            '60:         If istyp = 1 Then
            ' Write(luo, 280)i, ittyp
            'Write(lutty, 280) i,ittyp
            'ElseIf istyp = 2 Then
            ' Write(luo, 290) i,ittyp
            'Write(lutty, 290) i,ittyp
            'End If
60:         stf(ind1) = 0.0
            Continue For
70:
            For j = 1 To 8
                ii = iy(j)
                xc(j) = x(1, ii)
                yc(j) = x(2, ii)
                zc(j) = x(3, ii)
            Next

            Call volint(vol, j1, myy, xc, yc, zc)

            For j = 1 To 4
                ii = irect(j, ind1)
                If ii = 0 AndAlso j = 33 Then ii = irect(3, ind1)
                xn1(j) = x(1, ii)
                yn2(j) = x(2, ii)
                zn3(j) = x(3, ii)
            Next
            For I = 1 To 8
                x1(I) = xn1(I) : x1(I + 8) = xc(I)
                x2(I) = yn2(I) : x2(I + 8) = yc(I)
                x3(I) = zn3(I) : x3(I + 8) = zc(I)
            Next
            Dim dx, area As Double
            Call areal(dx, area, x1, x2, x3, vol)
            Dim mt = matype(myy)
            Dim bulkmd = rdc(3, myy) / (3 * (1 - 2 * rdc(4, myy)))
            stf(ind1) = 0.05 * stffac * area * bulkmd / dx
            If istyp = 2 Then
                emodl2 = rdc(3, myy)
            End If
            dxavg = dxavg + dx
            blkvg = blkvg + bulkmd
            stfvg = stfvg + stf(ind1)
        Next

        dxavg = dxavg / nrt
        blkvg = blkvg / nrt
        stfvg = stfvg / nrt

        ' QW 12-12-2018-

        For I = 1 To 8
            xx1(I) = xn1(I) : xx1(I + 8) = xc(I) : xx2(I) = yn2(I) : xx2(I + 8) = yc(I) : xx3(I) = zn3(I) : xx3(I + 8) = zc(I)
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'c      subroutine stiffs(x,irect,stf,ibh,ixs,cm,matype,thicks,
'      subroutine stiffs(x, irect, stf, ibh, matype,
'c     1 numelh, numels, nrt, stffac, dxavg, blkvg, stfvg,
'     1 numelh, nrt, stffac, dxavg, blkvg, stfvg,
'c     1 chrlen,ethik,ithick,nty,nsn,nsv,rdc,inoreord)
'     1 nty,nsn,nsv,rdc)
'c

'implicit Double precision (a-h, o - z)                                    dp

'c
'c ==>= module to determine stiffness parameter for slide surface facets
'c
'common/ lunits / lutty, lui, luo, lud, lur, lut, lug, luf, lust,
'     & lus1, lus2, lus5, lus6, lus11, lus12, luebe
'      common/ bk43 / vol, h(20), p1(20), p2(20), p3(20), aj(9), eps(9)
'      common/ bk44 / ux(20), uy(20), uz(20),
'     1 xn1(8),xc(8),xx1(4),
'     2 yn2(8),yc(8),xx2(4),
'     3 zn3(8),zc(8),xx3(4)
'      common/ bk45 / xs1, ys1, zs1, sig(3), epx, mx, ix(9), my, iy(9)  ' to set with other subs
'    common/ partbd / emodl2, lprtbd!added drb 12/02
'c
'c      dimension irect(4,*),stf(*),ibh(9,*),ixs(5,*),x(3,*),matype(*),
'      dimension irect(4,*), stf(*), ibh(9,*), x(3,*), matype(*),
'c     1 cm(1),thicks(4,1),ethik(1),nsv(1),rdc(4,*)
'     1 nsv(1),rdc(4,*)

'      Double precision n1, n2, n3




'c      logical ishell,ithick,inoreord
'	logical lprtbd !added drb 12/02
'      data istyp, llnum/0,0/,iutyp/0/
'c
'dxavg = 0.
'      blkvg = 0.
'      stfvg = 0.
'c     chrlen=-1.
'      llnum = llnum + 1
'If (istyp.eq.2) istyp=0
'      istyp = istyp + 1
'ittyp = (llnum + 1) / 2
'Do 150 i=1,nrt
'c      ishell = .false.
'      if(numelh.eq.0) go To 60 !41
'      Do 40 j=1,numelh
'      m = 0
'n = 1
'j1 = j
'i1 = irect(n, i)
'Call unpkid(my, ibh(1, j), 2)
'k = 1
'20 if(ix(k).ne.i1) go to 30
'      m = m + 1
'n = n + 1
'i1 = irect(n, i)
'k = 1
'If (m.eq.3) go to 70
'      go to 20
'   30 k=k+1
'      If (k.lt.9) go to 20
'   40 continue
'c   41 if(numels.eq.0) go to 60
'c      ishell = .true.
'c      Do 48 j=1,numels
'c      m = 0
'c      n = 1
'c      j1 = j
'c      i1 = irect(n, i)
'c      my = ixs(1, j)
'c      ix(1) = ixs(2, j)
'c      ix(2) = ixs(3, j)
'c      ix(3) = ixs(4, j)
'c      ix(4) = ixs(5, j)
'c      k = 1
'c   44 if(ix(k).ne.i1) go to 46
'c      m = m + 1
'c      n = n + 1
'c      i1 = irect(n, i)
'c      k = 1
'c      If(m.eq.3) go to 85
'c      go To 44
'c   46 k=k+1
'c      If(k.lt.5) go to 44
'c   48 continue
'   60 if(istyp.eq.1) write(luo,280) i,ittyp
'      If (istyp.eq.2) write(luo,290) i,ittyp
'      If (istyp.eq.1) write(lutty,280) i,ittyp
'      If (istyp.eq.2) write(lutty,290) i,ittyp
'      stf(i) = 0.0
'go to 150
'   70 continue
'      Do 80 j=1,8
'      ii = ix(j)
'xc(j) = x(1, ii)
'yc(j) = x(2, ii)
'80 zc(j)=x(3,ii)
'      Call volint(vol, j1, my)
'85 do 90 j=1,4
'      ii = irect(j, i)
'If (ii.eq.0.And.j.eq.4) ii=irect(3,i)
'      xn1(j) = x(1, ii)
'yn2(j) = x(2, ii)
'zn3(j) = x(3, ii)
'90 continue
'      Call areal(dx, area)
'mt = matype(my)
'c      If(ishell)
'c     1Dx=.25*(thicks(1, j1)+thicks(2,j1)+thicks(3,j1)+thicks(4,j1))
'c  94 if(mt.eq.1) bulkmd=cm(48*(my-1)+1)/(3.*(1.-2.*cm(48*(my-1)+2)))
'c     If(mt.eq.2) bulkmd=cm(48*(my-1)+11)/(3.*(1.-2.*cm(48*(my-1)+12)))
'c     If(mt.eq.3) bulkmd=cm(48*(my-1)+1)/(3.*(1.-2.*cm(48*(my-1)+2)))
'c     If(mt.eq.4) bulkmd=cm(48*(my-1)+9)/(3.*(1.-2.*cm(48*(my-1)+17)))
'c     If(mt.eq.5) bulkmd=cm(48*(my-1)+2)
'c     If(mt.eq.6) bulkmd=cm(48*(my-1)+1)
'c     If(mt.eq.7) bulkmd=cm(48*(my-1)+11)/(3.*(1.-2.*cm(48*(my-1)+12)))
'c     If(mt.eq.8) bulkmd=cm(48*(my-1)+17)
'c     If(mt.eq.9) bulkmd=cm(48*(my-1)+1)/(3.*(1.-2.*cm(48*(my-1)+2)))
'c     If(mt.eq.10)bulkmd=cm(48*(my-1)+9)/(3.*(1.-2.*cm(48*(my-1)+17)))
'c     If(mt.eq.11)bulkmd=cm(48*(my-1)+17)/(3.*(1.-2.*cm(48*(my-1)+1)))
'c     If(mt.eq.23)bulkmd=cm(48*(my-1)+21)
'      bulkmd = rdc(3, my) / (3.* (1.- 2.* rdc(4, my)))
'stf(i) = 0.05 * stffac * area * bulkmd / dx
'If (istyp.eq.2)Then
'emodl2 = rdc(3, my) !added drb 12/02
'	End If
'dxavg = dxavg + dx
'blkvg = blkvg + bulkmd
'stfvg = stfvg + stf(i)
'c
'c     If(nty.eq.4) then                  ! type 4: Single surface Is Not used.QW 11-11-2015
'c     alen1 = (xn1(1) - xn1(3)) ** 2 + (yn2(1) - yn2(3)) ** 2 + (zn3(1) - zn3(3)) ** 2
'c     alen2 = (xn1(2) - xn1(4)) ** 2 + (yn2(2) - yn2(4)) ** 2 + (zn3(2) - zn3(4)) ** 2
'c     chrlen = max(chrlen, 0.75 * sqrt(max(alen1, alen2)))
'c     endif
'c
'c      If((.not.ishell).or.(.not.ithick)) go to 98
'c      do 96 jj=1, 4
'c      call fndnn(nsv, nsn, ix(jj), nn)
'c      If(nn.gt.0) ethik(nn)=max(ethik(nn),thicks(jj,j1))
'c   96 continue
'c
'c   98 if(ishell) go to 150
'c      If(inoreord) then      ! skip renumbering when nty = 10
'c       write(luo, 160)ittyp
'c       go To 150
'c      endif

'c
'c     check ordering Of master And slave segment definitions
'c
'c      xs1 = 0.
'c      ys1=0.
'c      zs1 = 0.
'c      Do 100 jj=1,4
'c      nn = irect(jj, i)
'c      iy(jj) = nn
'c      xx1(jj) = x(1, nn)
'c      xx2(jj) = x(2, nn)
'c      xx3(jj) = x(3, nn)
'c      xs1 = xs1 + 0.25 * x(1, nn)
'c      ys1 = ys1 + 0.25 * x(2, nn)
'c  100 zs1=zs1+.25*x(3, nn)
'c      ierr = 0
'c      call st(n1, n2, n3, ss, tt, ierr)
'c      do 130 jj=1, 8
'c      j = ix(jj)
'c      do 110 kk=1, 4
'c      If(irect(kk, i).eq.j) go to 130
'c  110 continue
'c      dds = n1 * (x(1, j) - xs1) + n2 * (x(2, j) - ys1) + n3 * (x(3, j) - zs1)
'c      If(dds.lt.0) go to 150
'c      If(iy(3).eq.iy(4)) go to 125
'c      do 120 kk=1, 4
'c  120 irect(kk, i)=iy(4-kk+1)
'c      go To 128
'c  125 irect(1, i)=iy(2)
'c      irect(2, i) = iy(1)
'c  128 if(iutyp.ne.0) go to 129
'c      call header
'c      write(luo, 200)
'c  129 iutyp=iutyp+1
'c      If(istyp.eq.1) write(luo, 180) i,ittyp
'c      If(istyp.eq.2) write(luo, 190) i,ittyp
'c      go To 150
'c  130 continue
'c
'150 continue
'c
'dxavg = dxavg / nrt
'blkvg = blkvg / nrt
'stfvg = stfvg / nrt
'c
'Return
'160 format(///' renumbering disabled for interface number',i5,/1x)
'  180 format(' node numbering of slave segment',i6,' of interface',i5,
'     1' reversed')
'190 format(' node numbering of master segment',i5,' of interface',i5,
'     1' reversed')
'200 format(///' s l a v e   a n d   m a s t e r   r e n u m b e r'/1x)
'  280 format(' cannot find element of slave segment',i6,' of interface',
'     1I5)
'  290 format(' cannot find element of master segment',i5,' of interface'
'     1,i5)
'      End

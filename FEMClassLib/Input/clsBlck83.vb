'This file contains all the methods of blck83.f
Imports System.Text

Partial Public Class clsInput
    ''' <summary>
    ''' reorder list of 8 node elements (with 3 dof/node) to avoid recursion during vectorized EBE operations
    ''' </summary>
    ''' <param name="idp"></param>
    ''' <param name="id"></param>
    ''' <param name="ixp"></param>
    ''' <param name="ix"></param>
    ''' <param name="iblock"></param>
    ''' <param name="ipoint"></param>
    ''' <param name="ieq"></param>
    ''' <param name="ireord"></param>
    ''' <param name="numnp"></param>
    ''' <param name="neq"></param>
    ''' <param name="numel"></param>
    ''' <param name="mbsize"></param>
    ''' <param name="nelblk"></param>
    ''' <param name="ielblk"></param>
    ''' <param name="idroer"></param>

    Public Sub blck83(ByRef idp(,) As Integer, ByRef id(,) As Integer, ByRef ixp(,) As Integer, ByRef ix(,) As Integer,
                          ByRef iblock() As Integer, ByRef ipoint() As Integer, ByRef ieq() As Integer, ByRef ireord() As Integer,
                          ByRef numnp As Integer, ByRef neq As Integer, ByRef numel As Integer, ByRef mbsize As Integer,
                          ByRef nelblk As Integer, ByRef ielblk() As Integer, ByRef idroer() As Integer)
        Dim lsblck As Boolean

        'Call timing(cpuio, cpuip, 2, 2)

        Dim nzero = 0
        Dim maxblk = nelblk
        Dim ibmin = 8
        Dim ibsize, i3, ixi, isfrst, nvblck, numsel, iel As Integer
        Dim lm(24) As Integer

        '.... unpack element connectivities and global equation numbers

        For ind1 = 1 To numel
            'Call unpkid(ix(0, i), ixp(0, i), 2)
            Call com.unpkid(ix, 1, ind1, ixp, 1, ind1, 2)
        Next

        For ind1 = 1 To numnp
            'Call unpkid(id(0, i), idp(0, i), 1)
            Call com.unpkid(id, 1, ind1, idp, 1, ind1, 1)
        Next

        Dim istart = 0

        nelblk = 0
        lsblck = False

        Call azero(iblock, numel)
500:
        '.... find first element not assigned to a block

        For ii = istart + 1 To numel

            If iblock(ii) = 0 Then

                '....... start a new block

                nelblk = nelblk + 1


                ' v3.0 003/062920-3 YC
                If nelblk > maxblk Then
                    Dim sb = New StringBuilder()
                    sb.AppendLine(" ************************************************************")
                    sb.AppendLine(" *                   - FATAL ERROR -                        *")
                    sb.AppendLine(" *      number of EBE brick element blocks                  *")
                    sb.AppendLine(" *      exceeds maximum number allowed ({0})              *")
                    sb.AppendLine(" *                                                          *")
                    sb.AppendLine(" *     Please reduce number of brick element                *")
                    sb.AppendLine(" ************************************************************")
                    Dim msg = String.Format(sb.ToString(), maxblk.ToString("00000"))

                    PrintLine(lutty, msg)
                    Environment.Exit(0)

                End If
                'v3.0 003/062920-3 YC END


                iblock(ii) = nelblk
                ibsize = 1
                istart = ii

                Call azero(ieq, neq)

                For ind1 = 1 To 8
                    i3 = 3 * (ind1 - 1)
                    ixi = ix(ind1 + 1, ii)
                    lm(i3 + 1) = id(1, ixi)
                    lm(i3 + 2) = id(2, ixi)
                    lm(i3 + 3) = id(3, ixi)
                Next

                For j = 1 To 24
                    ieq(lm(j)) = 1
                Next

                ieq(nzero) = 0

                '....... test unassigned elements for inclusion in current block

                For jj = ii + 1 To numel
                    If iblock(jj) = 0 Then
                        For ind1 = 1 To 8
                            i3 = 3 * (ind1 - 1)
                            ixi = ix(ind1 + 1, jj)
                            lm(i3 + 1) = id(1, ixi)
                            lm(i3 + 2) = id(2, ixi)
                            lm(i3 + 3) = id(3, ixi)
                        Next

                        Dim isum = ieq(lm(1)) + ieq(lm(2)) + ieq(lm(3)) + ieq(lm(4)) +
                            ieq(lm(5)) + ieq(lm(6)) + ieq(lm(7)) + ieq(lm(8)) +
                            ieq(lm(9)) + ieq(lm(10)) + ieq(lm(11)) + ieq(lm(12)) +
                            ieq(lm(13)) + ieq(lm(14)) + ieq(lm(15)) + ieq(lm(16)) +
                            ieq(lm(17)) + ieq(lm(18)) + ieq(lm(19)) + ieq(lm(20)) +
                            ieq(lm(21)) + ieq(lm(22)) + ieq(lm(23)) + ieq(lm(24))

                        If isum = 0 Then

                            '.......... assign current element to current block

                            iblock(jj) = nelblk
                            ibsize = ibsize + 1

                            For ind1 = 1 To 24
                                ieq(lm(ind1)) = 1
                            Next

                            ieq(nzero) = 0

                            If ibsize = mbsize Then

                                '................ block has reached maximum size
                                ielblk(nelblk) = mbsize
                                GoTo 500
                            End If

                        End If
                    End If
                Next

                '....... element list exhausted before block filled

                If ibsize >= ibmin Then
                    ielblk(nelblk) = ibsize
                Else

                    '.......... designate current block as scalar elements

                    If (Not lsblck) Then
                        isfrst = istart
                        lsblck = True
                    End If

                    For ind1 = istart To numel
                        If iblock(ind1) = nelblk Then iblock(ind1) = -1
                    Next

                    nelblk = nelblk - 1
                End If
                GoTo 500

            End If
        Next

        nvblck = nelblk
        numsel = 0
        If lsblck Then

            '....... assign scalar elements to blocks

            ibsize = 0
            For ind1 = isfrst To numel
                If iblock(ind1) = -1 Then
                    ibsize = ibsize + 1
                    If ibsize = 1 Then nelblk = nelblk + 1
                    iblock(ind1) = nelblk
                    If ibsize = 64 Then
                        ielblk(nelblk) = -ibsize
                        numsel = numsel + ibsize
                        ibsize = 0
                    End If
                End If
            Next

            If ibsize <> 0 Then
                ielblk(nelblk) = -ibsize
                numsel = numsel + ibsize
            End If
        End If

        If nelblk > maxblk Then
            Dim sb = New StringBuilder()
            sb.AppendLine(" ************************************************************")
            sb.AppendLine(" *                   - FATAL ERROR -                        *")
            sb.AppendLine(" *      number of EBE brick element blocks ({0})            *")
            sb.AppendLine(" *        exceeds maximum number allowed ({1})              *")
            sb.AppendLine(" ************************************************************")
            Dim msg = String.Format(sb.ToString(), nelblk.ToString("00000"), maxblk.ToString("00000"))

            Stop

        End If

        '.... compute average block size

        Dim itemp = 0
        For ind1 = 1 To nvblck
            itemp = itemp + ielblk(ind1)
        Next

        Dim bmean As Double
        If nvblck = 0 Then                                               'b_n_m
            bmean = 0                                                      'b_n_m
        Else                                                              'b_n_m
            bmean = CDbl(itemp) / nvblck                                    'b_n_m
        End If                                                             'b_n_m
        Dim sb1 = New StringBuilder()
        sb1.AppendLine(New String(" ", 5) + "solid elements:")
        sb1.AppendLine(New String(" ", 5) + "   number of vector element blocks        ={0}")
        sb1.AppendLine(New String(" ", 5) + "   average number of elements per block   ={1}")
        sb1.AppendLine(New String(" ", 5) + "   total number of scalar elements        ={2}")
        Dim msg1 = String.Format(sb1.ToString(), nvblck.ToString("0000000000"), bmean.ToString("0000000000.0"), numsel.ToString("0000000000"))

        '.... reorder elements in packed storage and
        '        write unpacked data to plot file

        'ipoint(0) = 12100          ERROR
        ipoint(1) = 1
        For ind1 = 1 To nelblk - 1
            ipoint(ind1 + 1) = ipoint(ind1) + Math.Abs(ielblk(ind1))
        Next

        For iel = 1 To numel

            Dim iblk = iblock(iel)
            ireord(ipoint(iblk)) = iel
            idroer(iel) = ipoint(iblk)
            ipoint(iblk) = ipoint(iblk) + 1
        Next

        For ind1 = 1 To numel
            iel = ireord(ind1)
            Call com.packid(ix, 1, iel, ixp, 1, ind1, 2)
        Next
        'Call Check2D(ixp, 9, numelh)
        'Call timing(cpuio, cpuip, 2, 3)
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'c      subroutine blck83(idp,id,ixp,ix,iblock,ipoint,ieq,ireord,numnp,
'c     &                  neq,numel,mbsize,nelblk,ielblk,lioofc,mgsize,
'c     &                  nelgrp,ielgrp,idroer)
'      subroutine blck83(idp,id,ixp,ix,iblock,ipoint,ieq,ireord,numnp,
'     &                  neq,numel,mbsize,nelblk,ielblk,
'     &                  idroer)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to reorder list of 8 node elements (with 3 dof/node)
'c     to avoid recursion during vectorized EBE operations
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common /bk30/ cpuio(72),cpuip(72)
'      dimension idp(6,*),id(6,*),ixp(9,*),ix(9,*),                      vax
'     1          iblock(*),ipoint(*),ieq(*),ireord(*),ielblk(*),
'c     2          ielgrp(3,*),lm(25),idroer(*)
'     2          lm(25),idroer(*)
'c      logical lioofc,lsblck
'      logical lsblck
'c
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'blck83'
'      ik01 = ik01 + 1
'      end if
'
'      call timing(cpuio,cpuip,2,2)
'      nzero = 0
'      maxblk = nelblk
'c      maxgrp = nelgrp
'      ibmin = 8
'c
'c      if (mbsize.eq.1) then          ! set mbsize=64 in flccin.f
'c
'c....... define sequentially ordered EBE and exit
'c
'c         nmel = 64
'c         nelblk = (numel-1)/nmel + 1
'c         if (nelblk.gt.maxblk) then
'c            write(lutty,2300) nelblk,maxblk
'c            stop
'c         endif
'c         do 200 i=1,nelblk-1
'c  200    ielblk(i) = -nmel
'c         ielblk(nelblk) = nmel*(nelblk-1) - numel
'c         if (.not.lioofc) return
'c         go to 1800
'c      endif
'c
'c.... unpack element connectivities and global equation numbers
'c
'      do 300 i=1,numel
'      call unpkid(ix(1,i),ixp(1,i),2)
'  300 continue
'      do 400 i=1,numnp
'      call unpkid(id(1,i),idp(1,i),1)
'  400 continue
'      istart = 0
'      nelblk = 0
'      lsblck = .false.
'      call azero(iblock,numel)
'  500 continue
'c
'c.... find first element not assigned to a block
'c
'      do 1200 ii=istart+1,numel
'c
'      if (iblock(ii).eq.0) then
'c
'c....... start a new block
'c
'         nelblk = nelblk + 1
'         iblock(ii) = nelblk
'         ibsize = 1
'         istart = ii
'         call azero(ieq,neq)
'         do 600 i=1,8
'         i3 = 3*(i-1)
'         ixi = ix(i+1,ii)
'         lm(i3+1) = id(1,ixi)
'         lm(i3+2) = id(2,ixi)
'         lm(i3+3) = id(3,ixi)
'  600    continue
'         do 700 j=1,24
'  700    ieq(lm(j)) = 1
'         ieq(nzero) = 0
'c
'c....... test unassigned elements for inclusion in current block
'c
'         do 1000 jj=ii+1,numel
'c
'         if (iblock(jj).eq.0) then
'            do 800 i=1,8
'            i3 = 3*(i-1)
'            ixi = ix(i+1,jj)
'            lm(i3+1) = id(1,ixi)
'            lm(i3+2) = id(2,ixi)
'            lm(i3+3) = id(3,ixi)
'  800       continue
'            isum = ieq(lm( 1)) + ieq(lm( 2)) + ieq(lm( 3)) + ieq(lm( 4))
'     1           + ieq(lm( 5)) + ieq(lm( 6)) + ieq(lm( 7)) + ieq(lm( 8))
'     2           + ieq(lm( 9)) + ieq(lm(10)) + ieq(lm(11)) + ieq(lm(12))
'     3           + ieq(lm(13)) + ieq(lm(14)) + ieq(lm(15)) + ieq(lm(16))
'     4           + ieq(lm(17)) + ieq(lm(18)) + ieq(lm(19)) + ieq(lm(20))
'     5           + ieq(lm(21)) + ieq(lm(22)) + ieq(lm(23)) + ieq(lm(24))
'c
'            if (isum.eq.0) then
'c
'c.......... assign current element to current block
'c
'               iblock(jj) = nelblk
'               ibsize = ibsize + 1
'               do 900 i=1,24
'  900          ieq(lm( i)) = 1
'               ieq(nzero) = 0
'c
'               if (ibsize.eq.mbsize) then
'c
'c................ block has reached maximum size
'c
'                  ielblk(nelblk) = mbsize
'                  go to 500
'               endif
'c
'            endif
'         endif
' 1000    continue
'c
'c....... element list exhausted before block filled
'c
'         if (ibsize.ge.ibmin) then
'            ielblk(nelblk) = ibsize
'         else
'c
'c.......... designate current block as scalar elements
'c
'            if (.not.lsblck) then
'               isfrst = istart
'               lsblck = .true.
'            endif
'            do 1100 i=istart,numel
'            if (iblock(i).eq.nelblk) iblock(i) = -1
' 1100       continue
'            nelblk = nelblk - 1
'         endif
'         go to 500
'c
'      endif
'c
' 1200 continue
'c
'      nvblck = nelblk
'c
'      numsel = 0
'      if (lsblck) then
'c
'c....... assign scalar elements to blocks
'c
'         ibsize = 0
'         do 1300 i=isfrst,numel
'         if (iblock(i).eq.-1) then
'            ibsize = ibsize + 1
'            if (ibsize.eq.1) nelblk = nelblk + 1
'            iblock(i) = nelblk
'            if (ibsize.eq.64) then
'               ielblk(nelblk) = -ibsize
'               numsel = numsel + ibsize
'               ibsize = 0
'            endif
'         endif
' 1300    continue
'         if (ibsize.ne.0) then
'            ielblk(nelblk) = -ibsize
'            numsel = numsel + ibsize
'         endif
'      endif
'c
'      if (nelblk.gt.maxblk) then
'         write(lutty,2300) nelblk,maxblk
'         stop
'      endif
'c
'c.... compute average block size
'c
'      itemp = 0
'      do 1400 i=1,nvblck
'      itemp = itemp + ielblk(i)
' 1400 continue
'      if(nvblck.eq.0)then                                               b_n_m
'         bmean = 0                                                      b_n_m
'      else                                                              b_n_m
'         bmean = float(itemp)/nvblck                                    b_n_m
'      endif                                                             b_n_m
'      write(luo,2000) nvblck,bmean,numsel
'      write(lutty,2000) nvblck,bmean,numsel
'c
'c.... reorder elements in packed storage and
'c        write unpacked data to plot file
'c
'      ipoint(1) = 1
'      do 1500 i=1,nelblk-1
'      ipoint(i+1) = ipoint(i) + iabs(ielblk(i))
' 1500 continue
'c
'      do 1600 iel=1,numel
'      iblk = iblock(iel)
'      ireord(ipoint(iblk)) = iel
'      idroer(iel)=ipoint(iblk)
'      ipoint(iblk) =  ipoint(iblk) + 1
' 1600 continue
'c
'      do 1700 i=1,numel
'      iel = ireord(i)
'      call packid (ix(1,iel),ixp(1,i),2)
'c      call wrtp(ix(1,i),9)
' 1700 continue
' 1800 continue
'c
'c 1800 if (lioofc) then
'c
'c....... determine group sizes for out-of-core el x el
'c
'c         ii = 1
'c         ielgrp(1,1) = 1
'c         ncount = 0
'c         do 1900 i=1,nelblk
'c            ncount = ncount + iabs(ielblk(i))
'c            if (ncount.gt.mgsize) then
'c
'c............. truncate current group and intialize next group
'c
'c               ncount = ncount - iabs(ielblk(i))
'c               ielgrp(3,ii) = ncount
'c               ielgrp(2,ii) = i - 1
'c               ncount = iabs(ielblk(i))
'c               ii = ii + 1
'c               ielgrp(1,ii) = i
'c            endif
'c
'c 1900    continue
'c         ielgrp(3,ii) = ncount
'c         ielgrp(2,ii) = nelblk
'c         nelgrp = ii
'c         if (nelgrp.gt.maxgrp) then
'c            write(lutty,2400) nelgrp,maxgrp
'c            stop
'c         endif
'c         write(luo,2100) nelgrp
'c         write(lutty,2100) nelgrp
'c      endif
'c
'      call timing(cpuio,cpuip,2,3)
'      return
' 2000 format(5x,'solid elements:'
'     &      /5x,'   number of vector element blocks        =',i10
'     &      /5x,'   average number of elements per block   =',f10.1
'     &      /5x,'   total number of scalar elements        =',i10)
' 2100 format(5x,'   number of out-of-core groups           =',i10)
' 2300 format(//
'     &' ************************************************************',/
'     &' *                   - FATAL ERROR -                        *',/
'     &' *      number of EBE brick element blocks (',i5,')          *',/
'     &' *        exceeds maximum number allowed (',i5,')            *',/
'     &' ************************************************************')
' 2400 format(//
'     &' ************************************************************',/
'     &' *                   - FATAL ERROR -                        *',/
'     &' *      number of EBE brick element groups (',i5,')          *',/
'     &' *        exceeds maximum number allowed (',i5,')            *',/
'     &' ************************************************************')
'      end
'This file contains all the methods of testnd.f
Partial Public Class clsInput

    ''' <summary>
    ''' test for nodes not associated with any element
    ''' and constrain out dof left active by the user's input
    ''' </summary>
    ''' <param name="idp"></param>
    ''' <param name="numnp"></param>
    ''' <param name="ixh"></param>
    ''' <param name="numelh"></param>
    ''' <param name="ixde"></param>
    ''' <param name="nmelde"></param>
    'Public Sub testnd(ByRef idp(,) As Integer, ByRef numnp As Integer, ByRef ixh(,) As Integer,
    '                    ByRef numelh As Integer, ByRef ixde(,) As Integer, ByRef nmelde As Integer)
    Public Sub testnd(ByRef idp(,) As Integer, ByRef numnp As Integer, ByVal ixh(,) As Integer,
                 ByRef numelh As Integer, ByRef ixde(,) As Integer, ByRef nmelde As Integer)
        'Dim i, j As Integer     ' QW 8-2018
        'For j = 0 To numelh - 1
        'For i = 0 To 8
        'ixh(i, j) = ia(9 * i + j)
        'Next
        'Next                        ' QW 8-2018

        Dim ien(9), id(6) As Integer
        Dim lwarn = False

        '.... brick elements
        For n = 1 To numelh
            Call com.unpkid(ien, ixh, 1, n, 2)
            For i = 2 To 9
                'Call Check4(n, i, ien(i), 0, 1)
                Call com.unpkid(id, idp, 1, ien(i), 1)
                For j = 1 To 6
                    If id(j) = 0 Then id(j) = 64
                Next
                Call com.packid(id, idp, 1, ien(i), 1)
            Next
        Next

        'c.... discrete elements
        For n = 1 To nmelde
            For i = 1 To 2
                Call com.unpkid(id, idp, 1, ixde(i, n), 1)
                For j = 1 To 6
                    If id(j) = 0 Then id(j) = 64
                Next
                Call com.packid(id, idp, 1, ixde(i, n), 1)
            Next
        Next

        'c.... now loop through nodes to constrain any floating nodes
        lwarn = False
        Dim nsum = 0
        For n = 1 To numnp
            Call com.unpkid(id, idp, 1, n, 1)
            If id(1) = 0 OrElse id(2) = 0 OrElse id(3) = 0 OrElse id(4) = 0 OrElse id(5) = 0 OrElse id(6) = 0 Then
                nsum = nsum + 1
                lwarn = True
                'TODO - WHERE TO WRITE?
                '         write(luo,2000) n
                Dim msg1 = "" +
                " *******************************************************" + Environment.NewLine +
                " *                   - WARNING -                       *" + Environment.NewLine +
                " *   Node " + n.ToString("#####") + " has been constrained to be inactive.   *" + Environment.NewLine +
                " *******************************************************" + Environment.NewLine

                For i = 1 To 6
                    id(i) = 1
                Next
            Else
                For i = 1 To 6
                    If id(i) = 64 Then id(i) = 0
                Next
            End If
            Call com.packid(id, idp, 1, n, 1)
        Next
        'c
        If lwarn Then
            'Write(lutty, 3000) nsum
            'TODO - WHERE TO WRITE?

            Dim msg2 = "" +
            " *******************************************************" + Environment.NewLine +
            " *                   - WARNING -                       *" + Environment.NewLine +
            " *  " + nsum.ToString("#####") + " inactive nodes have been removed from mesh.  *" + Environment.NewLine +
            " *       See printed output file for details.          *" + Environment.NewLine +
            " *******************************************************" + Environment.NewLine
        End If

    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'c      subroutine testnd(idp,numnp,ixh,numelh,ixb,numelb,ixs,numels,
'c     &                  ixt,numelt,ixde,nmelde)
'      subroutine testnd(idp,numnp,ixh,numelh,ixde,nmelde)
'c
'      implicit double precision (a-h,o-z)                                    dp
'c
'c     Remove beam and shell elements test ----- ! QW 11-11-2015 
'c
'c===> module to test for nodes not associated with any element
'c     and constrain out dof left active by the user's input
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'c      dimension idp(6,*),ixh(9,*),ixb(4,*),ixs(5,*),ixt(5,*),ixde(3,*), vax
'c     &          ien(9),id(6)
'      dimension idp(6,*),ixh(9,*),ixde(3,*),ien(9),id(6)
'      logical lwarn
'c
'c.... brick elements
'c
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'testnd'
'      ik01 = ik01 + 1
'      end if
'
'      do 300 n=1,numelh
'      call unpkid(ien,ixh(1,n),2)
'      do 200 i=2,9
'      call unpkid(id,idp(1,ien(i)),1)
'      do 100 j=1,6
'  100 if (id(j).eq.0) id(j) = 64
'      call packid(id,idp(1,ien(i)),1)
'  200 continue
'  300 continue
'c
'c.... beam elements (note: do not view "k" node as active)
'c
'      
'c      do 600 n=1,numelb
'c      do 500 i=2,3
'c      call unpkid(id,idp(1,ixb(i,n)),1)
'c      do 400 j=1,6
'c  400 if (id(j).eq.0) id(j) = 64
'c      call packid(id,idp(1,ixb(i,n)),1)
'c  500 continue
'c  600 continue
'c
'c.... integrated shell elements
'c
'c      do 900 n=1,numels
'c      do 800 i=2,5
'c      call unpkid(id,idp(1,ixs(i,n)),1)
'c      do 700 j=1,6
'c  700 if (id(j).eq.0) id(j) = 64
'c      call packid(id,idp(1,ixs(i,n)),1)
'c  800 continue
'c  900 continue
'c
'c.... thin shell elements
'c
'c      do 1200 n=1,numelt
'c      do 1100 i=2,5
'c      call unpkid(id,idp(1,ixt(i,n)),1)
'c      do 1000 j=1,6
'c 1000 if (id(j).eq.0) id(j) = 64
'c      call packid(id,idp(1,ixt(i,n)),1)
'c 1100 continue
'c 1200 continue
'c
'c.... discrete elements
'c
'      do 1230 n=1,nmelde
'      do 1220 i=1,2
'      call unpkid(id,idp(1,ixde(i,n)),1)
'      do 1210 j=1,6
' 1210 if (id(j).eq.0) id(j) = 64
'      call packid(id,idp(1,ixde(i,n)),1)
' 1220 continue
' 1230 continue
'c
'c.... now loop through nodes to constrain any floating nodes
'c
'      lwarn = .false.
'      nsum = 0
'      do 1500 n=1,numnp
'      call unpkid(id,idp(1,n),1)
'      if (id(1).eq.0 .or. id(2).eq.0 .or. id(3).eq.0 .or.
'     &    id(4).eq.0 .or. id(5).eq.0 .or. id(6).eq.0 ) then
'         nsum = nsum + 1
'         lwarn = .true.
'         write(luo,2000) n
'         do 1300 i=1,6
' 1300    id(i) = 1
'      else
'         do 1400 i=1,6
' 1400    if (id(i).eq.64) id(i) = 0
'      endif
'      call packid(id,idp(1,n),1)
' 1500 continue
'c
'      if (lwarn) write(lutty,3000) nsum
'      return
'c
' 2000 format(/
'     &' *******************************************************',/
'     &' *                   - WARNING -                       *',/
'     &' *   Node ',i5,' has been constrained to be inactive.   *',/
'     &' *******************************************************')
' 3000 format(/
'     &' *******************************************************',/
'     &' *                   - WARNING -                       *',/
'     &' *  ',i5,' inactive nodes have been removed from mesh.  *',/
'     &' *       See printed output file for details.          *',/
'     &' *******************************************************')
'      end


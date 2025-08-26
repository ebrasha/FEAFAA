'This file contains all the methods of eqnum.f

Partial Public Class clsInput

    ''' <summary>
    ''' print nodal data including assigned equation numbers
    ''' </summary>
    ''' <param name="idp"></param>
    ''' <param name="x"></param>
    ''' <param name="numnp"></param>
    Public Sub eqnum(ByRef idp(,) As Integer, ByRef x(,) As Double, ByRef numnp As Integer, ByRef neql As Integer,
                     ByRef i1 As Integer, ByRef i2 As Integer, ByRef i3 As Integer, ByRef i4 As Integer, ByRef i5 As Integer, ByRef i6 As Integer)

        Dim idc(,) As Integer = {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 1, 0}, {0, 1, 1}, {1, 0, 1}, {1, 1, 1}}
        Dim id(6), jd(6) As Integer
        Dim nprint = 0

        '.... perform bandwidth/profile minimization

        Dim neq = 0

        For n = 1 To numnp
            'Call unpkid(i1, idp(1, n), 1)
            i1 = idp(1, n) : i2 = idp(2, n) : i3 = idp(3, n) : i4 = idp(4, n) : i5 = idp(5, n) : i6 = idp(6, n)
            id(1) = i1 : id(2) = i2 : id(3) = i3 : id(4) = i4 : id(5) = i5 : id(6) = i6
            For i = 1 To 6
                If id(i) <= 0 Then
                    neq = neq + 1
                    id(i) = neq
                Else
                    id(i) = 0
                End If
            Next
            i1 = id(1) : i2 = id(2) : i3 = id(3) : i4 = id(4) : i5 = id(5) : i6 = id(6)
            idp(1, n) = i1 : idp(2, n) = i2 : idp(3, n) = i3 : idp(4, n) = i4 : idp(5, n) = i5 : idp(6, n) = i6
            'Call packid(i1, idp(1, n), 1)

        Next

        '
        neql = neq                ' QW 11-11-2015

        For n = 1 To numnp

            'Call unpkid(i1, idp(1, n), 1)
            i1 = idp(1, n) : i2 = idp(2, n) : i3 = idp(3, n) : i4 = idp(4, n) : i5 = idp(5, n) : i6 = idp(6, n)

            Dim bcc As Double = 0
            Dim bcr As Double = 0
            If i1 = 0 AndAlso i2 <> 0 AndAlso i3 <> 0 Then bcc = 1
            If i1 <> 0 AndAlso i2 = 0 AndAlso i3 <> 0 Then bcc = 2
            If i1 <> 0 AndAlso i2 <> 0 AndAlso i3 = 0 Then bcc = 3
            If i1 = 0 AndAlso i2 = 0 AndAlso i3 <> 0 Then bcc = 4
            If i1 <> 0 AndAlso i2 = 0 AndAlso i3 = 0 Then bcc = 5
            If i1 = 0 AndAlso i2 <> 0 AndAlso i3 = 0 Then bcc = 6
            If i1 = 0 AndAlso i2 = 0 AndAlso i3 = 0 Then bcc = 7
            If i4 = 0 AndAlso i5 <> 0 AndAlso i6 <> 0 Then bcr = 1
            If i4 <> 0 AndAlso i5 = 0 AndAlso i6 <> 0 Then bcr = 2
            If i4 <> 0 AndAlso i5 <> 0 AndAlso i6 = 0 Then bcr = 3
            If i4 = 0 AndAlso i5 = 0 AndAlso i6 <> 0 Then bcr = 4
            If i4 <> 0 AndAlso i5 = 0 AndAlso i6 = 0 Then bcr = 5
            If i4 = 0 AndAlso i5 <> 0 AndAlso i6 = 0 Then bcr = 6
            If i4 = 0 AndAlso i5 = 0 AndAlso i6 = 0 Then bcr = 7
            If nprint > 0 Then
                nprint = nprint - 1
                Dim msg1 = Space(1) + n.ToString("00000") + Space(2) + bcc.ToString("000") + Space(1) + bcr.ToString("000") +
                    Space(2) + x(0, 0).ToString("E4") + Space(8) + x(1, 0).ToString("E4") + Space(8) + x(2, 0).ToString("E4") +
                     Space(12) + id(0).ToString("00000")
                For ind = 1 To 5
                    msg1 += Space(4) + id(ind).ToString("00000")
                Next
            Else
                nprint = 50
                'Call header()
                Dim msg2 = " g e n e r a t e d  n o d a l  d a t a " + Environment.NewLine +
                    "  node" + Space(3) + " b.c." + Space(9) + "x" + Space(19) + "y" + Space(19) + "z" +
                    Space(18) + "eq-x" + Space(5) + "eq-y" + Space(5) + "eq-z" + Space(5) + "eq-rx" +
                    Space(4) + "eq-ry" + Space(4) + "eq-rz"
            End If
        Next
    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'c      subroutine eqnum(idp,x,a,numnp,numel,iband,ncn,nrcc)
'      subroutine eqnum(idp,x,numnp)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to print nodal data including assigned equation numbers
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/idpmt/i1,i2,i3,i4,i5,i6,j1,j2,j3,j4,j5,j6
'      common/bkneq/neql
'c      common/bkneqs/neqss,nequs
'      
'
'
'
'      
'c     common/     /b(1)
'c      common/ba/b(50000000)
'c      
'c      pointer (iaa,b(1))
'c      dimension idc(3,8),x(3,*),idp(6,*),id(6),a(*),ncn(4,*)            vax
'      dimension idc(3,8),x(3,*),idp(6,*),id(6) 
'      dimension jd(6)
'      equivalence (i1,id),(j1,jd)                                       vax
'      data idc/0,0,0,1,0,0,0,1,0,0,0,1,1,1,0,0,1,1,1,0,1,1,1,1/
'c
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'eqnum'
'      ik01 = ik01 + 1
'      end if
'      
'c      open(20, FILE = 'idp.csv')
'c      do 501 i=1, numnp
'c501      write(20,200) ((idp(j,i),','), j=1,6)
'c      close (20)
'      nprint=0
'c      if(iband.eq.0) go to 100
'c      ier=0
'c
'c.... perform bandwidth/profile minimization
'c
'c ----- 09-06-2016 QW  (iband = 0 for iterative linear solver)
'c      if (iband.eq.3) then
'c        call reord1(a,idp,numnp,numel,ier,iband,ncn,nrcc,idc)
'!	  write(10,*) 'ik02 call reord1 in sub eqnum'
'c      else
'!        call reordr(a,idp,numnp,numel,ier,iband,ncn,nrcc,idc)
'c       write(10,*) 'ik02 call reordr in sub eqnum'
'c      endif
'c
'c      write(luo,170)
'c      nprint=43
'c      if(ier.eq.0) go to 140
'c -----       
'c
'c  100 if (nnrbt.ne.0.or.nnbst.ne.0) then                                pngv
'!        call eqn2rb(idp,ncn,idc,nrcc,b(nm0),b(iprb2),b(iprb3),
'!     &              b(iprb10),b(iprb11),b(ifr),b(idfr),nrfr,numnp,nmmat)
'c      write(10,*) 'ik02 call eqn2rb in sub eqnum'
'c        goto 140
'c        goto 148
'c      endif
'c
'c      do 700 i=1, numnp
'c700   write(150,710) i,',', ((idp(j,i),','),j=1,6)
'      neq=0
'      do 135 n=1,numnp
'      call unpkid(i1,idp(1,n),1)
'c      if(nrcc.eq.0) go to 105
'c      do 103 j=1,nrcc
'c      if(n.eq.ncn(2,j)) then
'c        if(idc(1,1+ncn(3,j)).eq.1) id(1)=1
'c        if(idc(2,1+ncn(3,j)).eq.1) id(2)=1
'c        if(idc(3,1+ncn(3,j)).eq.1) id(3)=1
'c        if(idc(1,1+ncn(4,j)).eq.1) id(4)=1
'c        if(idc(2,1+ncn(4,j)).eq.1) id(5)=1
'c        if(idc(3,1+ncn(4,j)).eq.1) id(6)=1
'c      endif
'c  103 continue
'  105 do 130 i=1,6
'      if (id(i).le.0) then
'        neq=neq + 1
'        id(i)=neq
'      else
'        id(i)=0
'      endif
'  130 continue
'      call packid(i1,idp(1,n),1)
'  135 continue
'
'c
'c      neqs=neq
'c      neqss=neq               ! make sure symmetric number of eqns saved for restart
'c      nequ=0
'c      neql=neqs+nequ
'      neql=neq                ! QW 11-11-2015
'c      neql=neq
'c
'c  140 if(nrcc.eq.0) go to 148
'c      do 145 j=1,nrcc
'c      n=ncn(1,j)
'c      m=ncn(2,j)
'c      call unpkid(i1,idp(1,n),1)
'c      call unpkid(j1,idp(1,m),1)
'c      if(idc(1,1+ncn(3,j)).eq.1) j1= i1
'c      if(idc(2,1+ncn(3,j)).eq.1) j2= i2
'c      if(idc(3,1+ncn(3,j)).eq.1) j3= i3
'c      if(idc(1,1+ncn(4,j)).eq.1) j4= i4
'c      if(idc(2,1+ncn(4,j)).eq.1) j5= i5
'c      if(idc(3,1+ncn(4,j)).eq.1) j6= i6
'c      call packid(j1,idp(1,m),1)
'c  145 continue
'  148 do 150 n=1,numnp
'      call unpkid(i1,idp(1,n),1)
'      bcc=0.
'      bcr=0.
'      if(i1.eq.0.and.i2.ne.0.and.i3.ne.0) bcc=1.
'      if(i1.ne.0.and.i2.eq.0.and.i3.ne.0) bcc=2.
'      if(i1.ne.0.and.i2.ne.0.and.i3.eq.0) bcc=3.
'      if(i1.eq.0.and.i2.eq.0.and.i3.ne.0) bcc=4.
'      if(i1.ne.0.and.i2.eq.0.and.i3.eq.0) bcc=5.
'      if(i1.eq.0.and.i2.ne.0.and.i3.eq.0) bcc=6.
'      if(i1.eq.0.and.i2.eq.0.and.i3.eq.0) bcc=7.
'      if(i4.eq.0.and.i5.ne.0.and.i6.ne.0) bcr=1.
'      if(i4.ne.0.and.i5.eq.0.and.i6.ne.0) bcr=2.
'      if(i4.ne.0.and.i5.ne.0.and.i6.eq.0) bcr=3.
'      if(i4.eq.0.and.i5.eq.0.and.i6.ne.0) bcr=4.
'      if(i4.ne.0.and.i5.eq.0.and.i6.eq.0) bcr=5.
'      if(i4.eq.0.and.i5.ne.0.and.i6.eq.0) bcr=6.
'      if(i4.eq.0.and.i5.eq.0.and.i6.eq.0) bcr=7.
'      if(nprint.gt.0) go to 149
'      nprint=50
'      call header
'      write(luo,170)
'  149 nprint=nprint-1
'      write(luo,190) n,bcc,bcr,(x(l,n),l=1,3),(id(l),l=1,6)
'  150 continue
'c
'      return
'c
'  170 format(///' g e n e r a t e d  n o d a l  d a t a '/
'     &/'  node',3x,' b.c.',9x,'x',19x,'y',19x,'z'
'     &,18x,'eq-x',5x,'eq-y',5x,'eq-z',5x,'eq-rx',4x,'eq-ry',4x,'eq-rz')
'  190 format(1x,i5,2x,f3.0,1x,f3.0,2x,e12.4,8x,e12.4,8x,e12.4,12x,i5
'     &,5(4x,i5))
'c
'  200 format(6(i10,a))
'710   format(i6,a2,6(i10,a2)) 
'c  
'      end

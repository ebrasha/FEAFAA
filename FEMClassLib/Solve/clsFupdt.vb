
'This file contains all the methods of fupdt.f
Partial Public Class clsSolve


    Public nalsf As Integer         ' addd by YC 092018

    ''' <summary>
    ''' update friction history variables for all slave nodes
    ''' </summary>
    ''' <param name="iseg"></param>
    ''' <param name="fdat"></param>
    ''' <param name="nsf"></param>
    ''' <param name="iaugsf"></param>
    ''' <param name="xlssf"></param>
    ''' <param name="xlmsf"></param>
    ''' <param name="itypsf"></param>
    Public Sub fupdt(ByRef iseg(,) As Integer, ByRef fdat(,) As Double, ByRef nsf(,) As Integer,
                     ByRef iaugsf() As Integer, ByRef xlssf(,) As Double, ByRef xlmsf(,) As Double,
                     ByRef itypsf(,) As Integer)

        Dim laugon As Boolean = False
        Dim i As Integer

        'called from solve.f for initialization and from elstf.f at the
        'begining of each time step

        'c
        'c         ifl = total number of slave nodes with friction
        'c
        'c         iseg( 1,i) = segment in contact at last iteration
        'c
        'c         fdat( 1,i) = ss at time n-1 for s.s. i
        'c         fdat( 2,i) = tt
        'c         fdat( 6,i) = ss at time n
        'c         fdat( 7,i) = tt
        '
        'c         fdat( 3,i) = x frict force at time n-1
        'c         fdat( 4,i) = y ..
        'c         fdat( 5,i) = z ..
        'c         fdat( 8,i) = x frict force at time n-1
        'c         fdat( 9,i) = y ..
        'c         fdat(10,i) = z ..
        'c
        'c

        ' YC 102418
        '        For i = 0 To ifl - 1
        '            iseg(0, i) = iseg(1, i)
        '            fdat(0, i) = fdat(5, i)
        '            fdat(1, i) = fdat(6, i)
        '            fdat(2, i) = fdat(7, i)
        '            fdat(3, i) = fdat(8, i)
        '            fdat(4, i) = fdat(9, i)
        '        Next

        '        Dim iads0 = 0
        '        Dim iadm0 = 0

        '        For ns = 0 To nalsf - 1
        '            Dim nns = nsf(0, ns)
        '            Dim nnm = nsf(1, ns)
        '            Dim iadsn = iads0 + nns - 1
        '            Dim iadmn = iadm0 + nnm - 1
        '            If iaugsf(ns) = 0 Then GoTo 630
        '            Dim ntyp = Math.Abs(itypsf(4, ns))
        '            If ntyp <> 3 AndAlso ntyp <> 8 AndAlso ntyp <> 9 Then GoTo 630

        '            For i = iads0 To iadsn - 1
        '                xlssf(0, i) = 0.0
        '                xlssf(1, i) = 0.0
        '            Next

        '            For i = iadm0 To iadmn - 1
        '                xlmsf(0, i) = 0.0
        '                xlmsf(1, i) = 0.0
        '            Next

        '630:        iads0 = iads0 + 2 * nns
        '            iadm0 = iadm0 + 2 * nnm
        '        Next

        For i = 1 To ifl
            iseg(1, i) = iseg(2, i)
            fdat(1, i) = fdat(6, i)
            fdat(2, i) = fdat(7, i)
            fdat(3, i) = fdat(8, i)
            fdat(4, i) = fdat(9, i)
            fdat(5, i) = fdat(10, i)
        Next

        Dim iads0 = 1
        Dim iadm0 = 1

        For ns = 1 To nalsf
            Dim nns = nsf(1, ns)
            Dim nnm = nsf(2, ns)
            Dim iadsn = iads0 + nns - 1
            Dim iadmn = iadm0 + nnm - 1
            If iaugsf(ns) = 0 Then GoTo 630
            Dim ntyp = Math.Abs(itypsf(5, ns))
            If ntyp <> 3 AndAlso ntyp <> 8 AndAlso ntyp <> 9 Then GoTo 630

            For i = iads0 To iadsn
                xlssf(2, i) = 0.0
                xlssf(3, i) = 0.0
            Next

            For i = iadm0 To iadmn
                xlmsf(2, i) = 0.0
                xlmsf(3, i) = 0.0
            Next

630:        iads0 = iads0 + 2 * nns
            iadm0 = iadm0 + 2 * nnm
        Next
        ' YC 102418

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine fupdt(iseg,fdat,nsf,iaugsf,xlssf,xlmsf,itypsf)
'c
'      implicit double precision (a-h,o-z)                            
'
'      common/fupdtc/ifl
'      logical laugon
'      common/aug1/deftal,ip1d1,ip1d2,ip1d3,ip1d4,ip1d5,
'     &                   nalsf,ipsf1,ipsf2,ipsf3,ipsf4,ipsf5,ipsf6,
'     &                   ipsw1,ipsw2,ipsw3,ipsw4,maxaug,laugon
'      dimension iseg(2,*),fdat(10,*),nsf(2,*),iaugsf(*),xlssf(3,*),
'     &          xlmsf(3,*),itypsf(7,*)
'c
'c => module to update friction history variables for all slave nodes
'c    called from solve.f for initialization and from elstf.f at the
'c    begining of each time step
'c
'c         ifl = total number of slave nodes with friction
'c
'c         iseg( 1,i) = segment in contact at last iteration
'c
'c         fdat( 1,i) = ss at time n-1 for s.s. i
'c         fdat( 2,i) = tt
'c         fdat( 6,i) = ss at time n
'c         fdat( 7,i) = tt
'
'c         fdat( 3,i) = x frict force at time n-1
'c         fdat( 4,i) = y ..
'c         fdat( 5,i) = z ..
'c         fdat( 8,i) = x frict force at time n-1
'c         fdat( 9,i) = y ..
'c         fdat(10,i) = z ..
'c
'c
'      do 10 i=1,ifl
'      iseg(1,i)=iseg(2,i)
'      fdat(1,i)=fdat(6,i)
'      fdat(2,i)=fdat(7,i)
'      fdat(3,i)=fdat(8,i)
'      fdat(4,i)=fdat(9,i)
'      fdat(5,i)=fdat(10,i)
'   10 continue
'c
'      iads0=1
'      iadm0=1
'      do 690 ns=1,nalsf
'      nns=nsf(1,ns)
'      nnm=nsf(2,ns)
'      iadsn=iads0+nns-1
'      iadmn=iadm0+nnm-1
'      if (iaugsf(ns).eq.0) goto 630
'      ntyp = abs(itypsf(5,ns))
'      if (ntyp.ne.3.and.ntyp.ne.8.and.ntyp.ne.9) goto 630
'c
'      do 610 i=iads0,iadsn
'      xlssf(2,i)=0.0
'      xlssf(3,i)=0.0
'  610 continue
'c
'      do 620 i=iadm0,iadmn
'      xlmsf(2,i)=0.0
'      xlmsf(3,i)=0.0
'  620 continue
'c
'  630 iads0=iads0+2*nns
'      iadm0=iadm0+2*nnm
'  690 continue
'c
'      return
'      end

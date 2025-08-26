
'This file contains all the methods of loadcn.f
Partial Public Class clsSolve

    ' added by YC 092018
    Public nload As Integer

    Private xl(6) As Double     ' QW 12-12-2018
    'Private xl(5) As Double
    ' added by YC 092018 END



    'Overload to handle 1D-2D
    Public Sub loadcn(ByRef idp() As Integer, ByRef fval() As Double, ByRef r() As Double,
                      ByRef nod() As Integer, ByRef idirn() As Integer, ByRef ncur() As Integer,
                       ByRef fac() As Double, ByRef x() As Double, ByRef u() As Double,
                       ByRef numnp As Integer, ByRef nmmat As Integer)

        Dim nod_max = nod.Max()
        Dim idp_ind = idp.Count
        Dim x_ind = x.Count
        Dim idp_2d(idp_ind - 1, nod_max - 1) As Integer, x_2d(x_ind - 1, nod_max - 1) As Double

        For i = 0 To idp_ind - 1
            idp_2d(i, 0) = idp(i)
        Next
        For i = 0 To x_ind - 1
            x_2d(i, 0) = x(i)
        Next

        loadcn(idp_2d, fval, r, nod, idirn, ncur, fac, x_2d, u, numnp, nmmat)

    End Sub

    ''' <summary>
    ''' add concentrated loads to residual
    ''' </summary>
    ''' <param name="idp"></param>
    ''' <param name="fval"></param>
    ''' <param name="r"></param>
    ''' <param name="nod"></param>
    ''' <param name="idirn"></param>
    ''' <param name="ncur"></param>
    ''' <param name="fac"></param>
    ''' <param name="x"></param>
    ''' <param name="u"></param>
    ''' <param name="numnp"></param>
    ''' <param name="nmmat"></param>
    Public Sub loadcn(ByRef idp(,) As Integer, ByRef fval() As Double, ByRef r() As Double,
                      ByRef nod() As Integer, ByRef idirn() As Integer, ByRef ncur() As Integer,
                       ByRef fac() As Double, ByRef x(,) As Double, ByRef u() As Double,
                       ByRef numnp As Integer, ByRef nmmat As Integer)

        'Dim id(5) As Integer   ' YC 102418
        Dim id(6) As Integer

        'Dim frb(5), xl(5) As Double ' YC 092018
        Dim frb(6) As Double

        If nload = 0 Then Return

        'YC 102418
        'For i = 0 To 2
        '    xl(2 * i) = x(i, nod(0))
        '    xl(2 * i + 1) = x(i, nod(0))
        'Next
        Dim i As Integer
        For i = 1 To 3
            xl(2 * i - 1) = x(i, nod(1))
            xl(2 * i) = x(i, nod(1))
        Next
        'YC 102418 END

        Dim n As Integer 'by YC 092018
        'Call read(fac, nload)
        'For n = 0 To nload - 1     'YC 102418
        For n = 1 To nload
            Dim lcc = ncur(n)
            Dim nod1 = nod(n)

            '      call unpkid(id,idp(1,nod1),1)
            'Call clsInput.unpkid(id, idp, 0, nod1 - 1, 1) 'by YC 092018 YC 102418
            Call unpkid(id, idp, 1, nod1, 1)

            Dim nod2 = idirn(n)
            Dim idof = id(nod2)
            Dim f = fval(lcc) * fac(n)
            If idof > neql Then

                'YC 102418
                'frb(0) = 0.0
                'frb(1) = 0.0
                'frb(2) = 0.0
                'frb(3) = 0.0
                'frb(4) = 0.0
                'frb(5) = 0.0
                frb(1) = 0.0
                frb(2) = 0.0
                frb(3) = 0.0
                frb(4) = 0.0
                frb(5) = 0.0
                frb(6) = 0.0
                'YC 102418 END

                frb(nod2) = f

            ElseIf idof > 0 Then
                r(idof) = r(idof) + f
            End If

            '	------------- Added by Qiang, July 14, 2010 ---------------
            '	find the coordinate range of load for calculatinf decay factor of infinite element

            'For j = 0 To 2     'YC 102418
            '    If x(j, nod1) < xl(2 * j) Then xl(2 * j) = x(j, nod1)
            '    If x(j, nod1) > xl(2 * j + 1) Then xl(2 * j + 1) = x(j, nod1)
            'Next
            For j = 1 To 3
                If (x(j, nod1) < xl(2 * j - 1)) Then xl(2 * j - 1) = x(j, nod1)
                If (x(j, nod1) > xl(2 * j)) Then xl(2 * j) = x(j, nod1)
            Next

            '	------------- End of add ----------------------------------
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine loadcn(idp,fval,r,nod,idirn,ncur,fac,x,u,numnp,nmmat)
'c
'      implicit double precision (a-h,o-z)                           
'c
'c===> module to add concentrated loads to residual
'c
'      common/bk16/nlcur,nptst,nload,nptm
'      common/bkneq/neql
'	common /rload/xl						
'	dimension xl(6)							
'c
'      dimension idp(6,*),fval(*),r(*),nod(*),idirn(*),ncur(*),fac(*), 
'     &          id(6),frb(6),x(3,numnp),u(*)
'c
'
'      if(nload.eq.0) return
'c
'	do i=1,3
'		xl(2*i-1)=x(i,nod(1))				
'		xl(2*i)=x(i,nod(1))					
'	end do
'c
'      do 10 n=1,nload
'      lcc=ncur(n)
'      nod1=nod(n)
'      call unpkid(id,idp(1,nod1),1)
'      nod2=idirn(n)
'      idof=id(nod2)
'      f=fval(lcc)*fac(n)
'      if(idof.gt.neql) then
'        frb(1)=0.0
'        frb(2)=0.0
'        frb(3)=0.0
'        frb(4)=0.0
'        frb(5)=0.0
'        frb(6)=0.0
'        frb(nod2)=f
'      elseif(idof.gt.0) then
'        r(idof)=r(idof)+f
'      endif
'c
'c	------------- Added by Qiang, July 14, 2010 ---------------
'c	find the coordinate range of load for calculatinf decay factor of infinite element
'c
'	do j=1,3
'		if(x(j,nod1).lt.xl(2*j-1)) xl(2*j-1)=x(j,nod1)
'		if(x(j,nod1).gt.xl(2*j)) xl(2*j)=x(j,nod1)
'	end do
'c
'c	------------- End of add ----------------------------------
'c
'   10 continue
'      return
'      end

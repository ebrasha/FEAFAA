'This file contains all the methods of colht.f
Partial Public Class clsInitial

    ''' <summary>
    ''' update stiffness matrix column heights(excludes diagonal)
    ''' </summary>
    ''' <param name="kht" remarks="current column heights(output updated column heights)"></param>
    ''' <param name="ns" remarks="number of degrees of freedom for this element"></param>
    ''' <param name="ld" remarks="element equation numbers"></param>
    Public Sub colht(ByRef kht() As Integer, ByRef ns As Integer, ByRef ld() As Integer)
        '     the array kht must be initialized to zero before the first
        '     call to this routine. kht is neq words in length.

        Dim l = CInt(Math.Pow(2, 23))

        For i = 1 To ns
            If ld(i) > 0 Then l = Math.Min(l, ld(i))
        Next

        For i = 1 To ns
            ' Dim m = ld(i)
            Dim m = ld(i)
            If m > l Then kht(m) = Math.Max(kht(m), m - l)
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine colht(kht,ns,ld)
'c
'
'      implicit double precision (a-h,o-z)                             
'c
'c===> module to update stiffness matrix column heights
'c===> (excludes diagonal)
'c
'      common/bkneq/neql
'      dimension kht(*),ld(*)
'c
'c     input arguments
'c           kht       current column heights
'c           ns        number of degrees of freedom for this element
'c           ld        element equation numbers
'c
'c     output arguments
'c           kht       updated column heights
'c
'c     the array kht must be initialized to zero before the first
'c     call to this routine. kht is neq words in length.
'c
'
'      l=2**23
'c
'      do 20 i=1,ns
'      if(ld(i).gt.0)                     l=min0(l,ld(i))
'c     
'   20 continue
'      do 40 i=1,ns
'      m=ld(i)
'      if(m.gt.l)                 kht(m)=max0(kht(m),m-l)
'c     
'   40 continue
'      return
'      end

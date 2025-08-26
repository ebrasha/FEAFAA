'This file contains all the methods of fdot.f
Partial Public Class clsSolve

    Public Function fdot(ByRef a() As Double, ByRef b() As Double, ByRef n As Integer) As Double

        fdot = 0.0

        Dim i As Integer    ' YC 102418

        'For i = 0 To n - 1 ' YC 102418
        For i = 1 To n
            fdot = fdot + a(i) * b(i)
        Next

    End Function
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      function fdot(a,b,n)                                            
'c
'      implicit double precision (a-h,o-z)                             
'
'      dimension a(*),b(*)                                             
'c
'      fdot=0.                                                         
'      do 10 i=1,n                                                     
' 10   fdot=fdot + a(i)*b(i)                                           
'      return                                                          
'      end                                                             

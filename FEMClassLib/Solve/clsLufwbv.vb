'This file contains all the methods of lufwbv.f
Partial Public Class clsSolve

    ''' <summary>
    ''' perform forward-backward substitution in vector blocks with b ordered properly
    ''' </summary>
    ''' <param name="a"></param>
    ''' <param name="n1"></param>
    ''' <param name="n2"></param>
    ''' <param name="n"></param>
    ''' <param name="b"></param>
    ''' <param name="n3"></param>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    Public Sub lufwbv(ByRef a(,,) As Double, ByRef n1 As Integer, ByRef n2 As Integer, ByRef n As Integer,
                      ByRef b(,) As Double, ByRef n3 As Integer, ByRef lft As Integer, ByRef llt As Integer)

        'Dim sum(63) As Double      ' YC 102418
        Dim sum(64) As Double

        '   ... forward substitution ...

        'For i = 0 To n - 1   ' YC 102418
        For i = 1 To n
            For l = lft To llt
                sum(l) = b(l, i)
            Next

            'For j = 0 To i     'YC 102418
            For j = 1 To i - 1

                For l = lft To llt
                    sum(l) = sum(l) - a(i, j, l) * b(l, j)
                Next
            Next

            For l = lft To llt
                b(l, i) = sum(l)
            Next
        Next

        '   ... backward substitution ...
        'For i = n - 1 To 0 Step -1   'YC 102418
        For i = n To 1 Step -1
            For l = lft To llt
                sum(l) = b(l, i)
            Next

            If i < n Then
                'For j = i To n - 1    'YC 102418
                For j = i + 1 To n
                    For l = lft To llt
                        sum(l) = sum(l) - a(i, j, l) * b(l, j)
                    Next
                Next
            End If

            For l = lft To llt
                If a(i, i, l) = 0 Then a(i, i, l) = 1.0E-20
                b(l, i) = sum(l) / a(i, i, l)
            Next
        Next


    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine lufwbv(a,n1,n2,n,b,n3,lft,llt)
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c
'c ===> module to perform forward-backward substitution in vector blocks
'c ===> with b ordered properly
'c
'      dimension a(n1,n2,*),b(n3,*),sum(64)
'c
'c   ... forward substitution ...
'
'      do 200 i=1,n
'c
'      do 50 l=lft,llt
'      sum(l)=b(l,i)
'   50 continue
'c
'      do 100 j=1,i-1
'      do 60 l=lft,llt
'      sum(l)=sum(l)-a(i,j,l)*b(l,j)
'   60 continue
'  100 continue
'c
'      do 150 l=lft,llt
'      b(l,i)=sum(l)
'  150 continue
'c
'  200 continue
'c
'c   ... backward substitution ...
'      do 400 i=n,1,-1
'c
'      do 250 l=lft,llt
'      sum(l)=b(l,i)
'  250 continue
'c
'      if(i.lt.n) then
'      do 300 j=i+1,n
'      do 260 l=lft,llt
'      sum(l)=sum(l)-a(i,j,l)*b(l,j)
'  260 continue
'  300 continue
'      endif
'c
'      do 350 l=lft,llt
'c
'c	----------------- Added by Qiang, 03-09-2010 -------------	
'	if(a(i,i,l).eq.0.) a(i,i,l)=1.e-20		
'c	----------------------------------------------------------
'c
'      b(l,i)=sum(l)/a(i,i,l)
'  350 continue
'  400 continue
'c
'      return
'      end

'This file contains all the methods of ludcmp.f
Partial Public Class clsSolve

    ''' <summary>
    ''' performs LU (crout) decomposition of NxN matrix A without pivoting
    ''' </summary>
    ''' <param name="a"></param>
    ''' <param name="n1"></param>
    ''' <param name="n2"></param>
    ''' <param name="n"></param>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    Public Sub ludcmp(ByRef a(,,) As Double, ByRef n1 As Integer, ByRef n2 As Integer,
                      ByRef n As Integer, ByRef lft As Integer, ByRef llt As Integer)

        'Dim sum(63), dum(63) As Double ' YC 102418
        Dim sum(64), dum(64) As Double
        'Call Check3D(a, 9, 9, 9)
        Dim i, k As Integer ' YC 102418

        '   ... loop over columns ...


        'For j = 0 To n - 1  ' YC 102418
        For j = 1 To n
            '   ... process U ...
            'For i = 0 To j         ' YC 102418
            For i = 1 To j - 1
                For l = lft To llt
                    sum(l) = a(i, j, l)
                Next

                'For k = 0 To i     ' YC 102418
                For k = 1 To i - 1
                    For l = lft To llt
                        sum(l) = sum(l) - a(i, k, l) * a(k, j, l)
                    Next
                Next

                For l = lft To llt
                    a(i, j, l) = sum(l)
                Next
            Next

            '   ... process diagonal and L ...
            'For i = j To n - 1   ' YC 102418
            For i = j To n
                For l = lft To llt
                    sum(l) = a(i, j, l)
                Next

                'For k = 0 To j         ' YC 102418
                For k = 1 To j - 1
                    For l = lft To llt
                        sum(l) = sum(l) - a(i, k, l) * a(k, j, l)
                    Next
                Next

                For l = lft To llt
                    a(i, j, l) = sum(l)
                Next
            Next

            '   ... divide by pivot ...
            'If j <> n Then         ' YC 102418
            If j <> n Then
                For l = lft To llt
                    If a(j, j, l) = 0 Then a(j, j, l) = 1.0E-20
                    dum(l) = 1.0 / a(j, j, l)
                Next

                'For i = j To n - 1     ' YC 102418
                For i = j + 1 To n
                    For l = lft To llt
                        a(i, j, l) = a(i, j, l) * dum(l)
                    Next
                Next
            End If
        Next


    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine ludcmp(a,n1,n2,n,lft,llt)
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c
'c ===> this routine performs LU (crout) decomposition of NxN matrix A
'c ===> without pivoting
'c
'      dimension a(n1,n2,*),sum(64),dum(64)
'c
'c   ... loop over columns ...
'
'      do 1000 j=1,n
'c
'c   ... process U ...
'      do 200 i=1,j-1
'        do 50 l=lft,llt
'        sum(l)=a(i,j,l)
'   50   continue
'      do 100 k=1,i-1
'        do 60 l=lft,llt
'        sum(l)=sum(l)-a(i,k,l)*a(k,j,l)
'   60   continue
'  100 continue
'        do 150 l=lft,llt
'        a(i,j,l)=sum(l)
'  150   continue
'  200 continue
'c
'c   ... process diagonal and L ...
'      do 400 i=j,n
'        do 250 l=lft,llt
'        sum(l)=a(i,j,l)
'  250   continue
'      do 300 k=1,j-1
'        do 260 l=lft,llt
'        sum(l)=sum(l)-a(i,k,l)*a(k,j,l)
'  260   continue
'  300 continue
'        do 350 l=lft,llt
'        a(i,j,l)=sum(l)
'  350   continue
'  400 continue
'c
'c   ... divide by pivot ...
'      if(j.ne.n)then
'        do 450 l=lft,llt
'c
'c	----------------- Added by Qiang, 03-09-2010 -------------	
'	  if(a(j,j,l).eq.0.) a(j,j,l)=1.e-20		
'c	----------------------------------------------------------
'c
'        dum(l)=1./a(j,j,l)
'  450   continue
'      do 500 i=j+1,n
'        do 460 l=lft,llt
'        a(i,j,l)=a(i,j,l)*dum(l)
'  460   continue
'  500 continue
'      endif
' 1000 continue
'c
'      return
'      end

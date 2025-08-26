'This file contains all the methods of chsky06.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to perform cholesky factorization of 6 dof element matrix 
    ''' </summary>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    ''' <param name="s"></param>
    Public Sub chsky06(ByRef lft As Integer, ByRef llt As Integer, ByRef s(,) As Double)

        Dim temp(128) As Double ' common /vect2/  by YC 102418 

        Dim i, k As Integer ' YC 102418


        ' YC 102418 
        'For l = lft - 1 To llt - 1     
        's(0, l) = 1.0 / Math.Sqrt(s(0, l))
        'Next
        For l = lft To llt
            s(1, l) = 1.0 / Math.Sqrt(s(1, l))
        Next
        ' YC 102418 END

        '.... loop over columns                                                 

        'Dim jj = 0     ' YC 102418
        Dim jj = 1

        'For j = 1 To 5     'j is not a physical index by YC 102418
        For j = 2 To 6

            Dim jjlast = jj
            jj = jj + j

            'c.... scale first entry in column                                       

            ' YC 102418
            'For l = lft - 1 To llt - 1
            '    s(jjlast + 1, l) = s(jjlast + 1, l) * s(0, l)
            'Next
            'For l = lft - 1 To llt - 1
            '    s(jj, l) = s(jj, l) - s(jjlast + 1, l) * s(jjlast + 1, l)
            'Next
            For l = lft To llt
                s(jjlast + 1, l) = s(jjlast + 1, l) * s(1, l)
            Next
            For l = lft To llt
                s(jj, l) = s(jj, l) - s(jjlast + 1, l) * s(jjlast + 1, l)
            Next
            ' YC 102418 END

            'c.... reduce remainder of column                                        

            'Dim iilast = 0     ' YC 102418
            'For i = 0 To j - 1
            Dim iilast = 1
            For i = 2 To j - 1  'i is not a physical index by YC 102418

                Dim ii = iilast + i

                'For l = lft - 1 To llt - 1   ' YC 102418
                For l = lft To llt
                    temp(l) = 0.0
                Next

                'Dim npass= (i - 1) / 2        ' YC 102418
                'Dim next_pass= (i - 1) - 2 * npass
                Dim npass As Integer = (i - 1) / 2
                Dim next_pass As Integer = (i - 1) - 2 * npass

                ' YC 102418
                'For k = 0 To 2 * npass - 1 Step 2      
                '    For l = lft - 1 To llt - 1
                For k = 1 To 2 * npass - 1 Step 2
                    For l = lft To llt
                        temp(l) = ((temp(l) + s(iilast + k, l) * s(jjlast + k, l)) +
                                   s(iilast + k + 1, l) * s(jjlast + k + 1, l))
                    Next
                Next
                ' YC 102418 END


                If next_pass = 0 Then

                    'For l = lft - 1 To llt - 1  ' YC 102418
                    For l = lft To llt
                        s(jjlast + i, l) = (s(jjlast + i, l) - temp(l)) * s(ii, l)
                    Next
                Else

                    'For l = lft - 1 To llt - 1     ' YC 102418
                    For l = lft To llt
                        s(jjlast + i, l) = ((s(jjlast + i, l) - temp(l)) -
                                            s(iilast + i - 1, l) * s(jjlast + i - 1, l)) * s(ii, l)
                    Next
                End If

                'c.... reduce diagonal                                                   

                'For l = lft - 1 To llt - 1     ' YC 102418
                For l = lft To llt
                    s(jj, l) = s(jj, l) - s(jjlast + i, l) * s(jjlast + i, l)
                Next

                iilast = ii
            Next

            'c.... scale diagonal                                                    

            'For l = lft - 1 To llt - 1    ' YC 102418
            For l = lft To llt
                s(jj, l) = 1.0 / Math.Sqrt(s(jj, l))
            Next

        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine chsky06(lft,llt,s)                                     
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c                                                                       
'c===> module to perform cholesky factorization of 6 dof element matrix  
'c                                                                       
'      dimension s(21,*)                                                 
'      common /vect2/ temp(128)                                          
'c
'      do 100 l=lft,llt                                                  
'      s(  1,l) = 1.0/sqrt(s(  1,l))                                     
'  100 continue                                                          
'c                                                                       
'c.... loop over columns                                                 
'c                                                                       
'      jj = 1                                                            
'      do 1100 j=2,6                                                     
'      jjlast = jj                                                       
'      jj     = jj + j                                                   
'c                                                                       
'c.... scale first entry in column                                       
'c                                                                       
'      do 200 l=lft,llt                                                  
'      s(jjlast+1,l) = s(jjlast+1,l)*s(  1,l)                            
'  200 continue                                                          
'      do 300 l=lft,llt                                                  
'      s(jj,l) = s(jj,l) - s(jjlast+1,l)*s(jjlast+1,l)                   
'  300 continue                                                          
'c                                                                       
'c.... reduce remainder of column                                        
'c                                                                       
'      iilast = 1                                                        
'      do 900 i=2,j-1                                                    
'      ii = iilast + i                                                   
'c                                                                       
'      do 400 l=lft,llt                                                  
'      temp(l) = 0.0                                                     
'  400 continue                                                          
'c                                                                       
'      npass = (i-1)/2                                                   
'      next = (i-1) - 2*npass                                            
'c                                                                       
'      do 600 k=1,2*npass-1,2                                            
'c                                                                       
'      do 500 l=lft,llt                                                  
'  500 temp(l) = ((temp(l) + s(iilast+k  ,l)*s(jjlast+k  ,l) )           
'     &                    + s(iilast+k+1,l)*s(jjlast+k+1,l) )           
'c                                                                       
'  600 continue                                                          
'c                                                                       
'      if (next.eq.0) then                                               
'         do 700 l=lft,llt                                               
'  700    s(jjlast+i,l) = (s(jjlast+i,l) - temp(l))*s(ii,l)              
'c                                                                       
'      else                                                              
'c                                                                       
'         do 750 l=lft,llt                                               
'  750    s(jjlast+i,l) = ((s(jjlast+i,l) - temp(l) )                    
'     &                   - s(iilast+i-1,l)*s(jjlast+i-1,l) )*s(ii,l)    
'      endif                                                             
'c                                                                       
'c.... reduce diagonal                                                   
'c                                                                       
'      do 800 l=lft,llt                                                  
'      s(jj,l) = s(jj,l) - s(jjlast+i,l)*s(jjlast+i,l)                   
'  800 continue                                                          
'c                                                                       
'      iilast = ii                                                       
'  900 continue                                                          
'c                                                                       
'c.... scale diagonal                                                    
'c                                                                       
'      do 1000 l=lft,llt                                                 
'      s(jj,l) = 1.0/sqrt(s(jj,l))                                       
' 1000 continue                                                          
'c                                                                       
' 1100 continue                                                          
'c                                                                       
'      return                                                            
'      end                                                               

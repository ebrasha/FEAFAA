'This file contains all the methods of bdg83.f
Partial Public Class clsSolve


    Private idodiag As Integer     


    ''' <summary>
    ''' to assemble 8 node, 24 dof element into block diagonal
    ''' </summary>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    ''' <param name="diag"></param>
    ''' <param name="s"></param>
    ''' <param name="lm"></param>
    ''' <param name="ien"></param>
    ''' <param name="ldgen2"></param>
    Public Sub bdg83(ByRef lft As Integer, ByRef llt As Integer, ByRef diag(,) As Double, ByRef s(,) As Double,
                     ByRef lm(,) As Integer, ByRef ien(,) As Integer, ByRef ldgen2 As Boolean)

        Dim ii, jj, jl, il As Integer

        Dim k, i As Integer ' YC 102418

        If ldgen2 Then

            '....... test for and contract redundant degrees of freedom           

            For k = lft To llt

                'For j = 0 To 20    ' YC 102418
                For j = 1 To 21
                    If lm(j, k) <> 0 Then
                        For i = j + 3 To 24 Step 3
                            If lm(j, k) = lm(i, k) Then
                                jj = (j - 1) * j / 2
                                ii = (i - 1) * i / 2

                                'For l = 0 To j  ' YC 102418
                                For l = 1 To j - 1
                                    s(ii + l, k) = s(ii + l, k) + s(jj + l, k)
                                    s(jj + l, k) = 0
                                Next
                                jl = jj + j
                                s(ii + j, k) = 2 * s(ii + j, k) + s(jl, k)
                                s(jl, k) = 0
                                For l = j + 1 To i
                                    jl = jl + l - 1
                                    s(ii + l, k) = s(ii + l, k) + s(jl, k)
                                    s(jl, k) = 0
                                Next
                                il = ii + i
                                For l = i + 1 To 24
                                    jl = jl + l - 1
                                    il = il + l - 1
                                    s(il, k) = s(il, k) + s(jl, k)
                                    s(jl, k) = 0
                                Next
                                lm(j, k) = 0
                                GoTo 500
                            End If
                        Next
                    End If
500:            Next
            Next
        End If

        For i = lft To llt
            '

            'c	Set idodiag to true, this tells the next call to the spring
            'c	stiffness function to compute spring contributions to the stiffness diagonal

            idodiag = 1

            'For jj = 1 To 6
            'If ien(i, jj) = 1655 Then
            'Call Check4(i, jj, diag(1, ien(i, jj)), s(jj, i), 1)
            'End If
            'Next

            ' YC 102418
            'diag(0, ien(i, 0)) = diag(0, ien(i, 0)) + s(0, i)
            'diag(1, ien(i, 0)) = diag(1, ien(i, 0)) + s(1, i)
            'diag(2, ien(i, 0)) = diag(2, ien(i, 0)) + s(2, i)
            'diag(3, ien(i, 0)) = diag(3, ien(i, 0)) + s(3, i)
            'diag(4, ien(i, 0)) = diag(4, ien(i, 0)) + s(4, i)
            'diag(5, ien(i, 0)) = diag(5, ien(i, 0)) + s(5, i)

            'diag(0, ien(i, 1)) = diag(0, ien(i, 1)) + s(9, i)
            'diag(1, ien(i, 1)) = diag(1, ien(i, 1)) + s(13, i)
            'diag(2, ien(i, 1)) = diag(2, ien(i, 1)) + s(14, i)
            'diag(3, ien(i, 1)) = diag(3, ien(i, 1)) + s(18, i)
            'diag(4, ien(i, 1)) = diag(4, ien(i, 1)) + s(19, i)
            'diag(5, ien(i, 1)) = diag(5, ien(i, 1)) + s(20, i)

            'diag(0, ien(i, 2)) = diag(0, ien(i, 2)) + s(27, i)
            'diag(1, ien(i, 2)) = diag(1, ien(i, 2)) + s(34, i)
            'diag(2, ien(i, 2)) = diag(2, ien(i, 2)) + s(35, i)
            'diag(3, ien(i, 2)) = diag(3, ien(i, 2)) + s(42, i)
            'diag(4, ien(i, 2)) = diag(4, ien(i, 2)) + s(43, i)
            'diag(5, ien(i, 2)) = diag(5, ien(i, 2)) + s(44, i)

            'diag(0, ien(i, 3)) = diag(0, ien(i, 3)) + s(54, i)
            'diag(1, ien(i, 3)) = diag(1, ien(i, 3)) + s(64, i)
            'diag(2, ien(i, 3)) = diag(2, ien(i, 3)) + s(65, i)
            'diag(3, ien(i, 3)) = diag(3, ien(i, 3)) + s(75, i)
            'diag(4, ien(i, 3)) = diag(4, ien(i, 3)) + s(76, i)
            'diag(5, ien(i, 3)) = diag(5, ien(i, 3)) + s(77, i)

            'diag(0, ien(i, 4)) = diag(0, ien(i, 4)) + s(90, i)
            'diag(1, ien(i, 4)) = diag(1, ien(i, 4)) + s(103, i)
            'diag(2, ien(i, 4)) = diag(2, ien(i, 4)) + s(104, i)
            'diag(3, ien(i, 4)) = diag(3, ien(i, 4)) + s(117, i)
            'diag(4, ien(i, 4)) = diag(4, ien(i, 4)) + s(118, i)
            'diag(5, ien(i, 4)) = diag(5, ien(i, 4)) + s(119, i)

            'diag(0, ien(i, 5)) = diag(0, ien(i, 5)) + s(135, i)
            'diag(1, ien(i, 5)) = diag(1, ien(i, 5)) + s(151, i)
            'diag(2, ien(i, 5)) = diag(2, ien(i, 5)) + s(152, i)
            'diag(3, ien(i, 5)) = diag(3, ien(i, 5)) + s(168, i)
            'diag(4, ien(i, 5)) = diag(4, ien(i, 5)) + s(169, i)
            'diag(5, ien(i, 5)) = diag(5, ien(i, 5)) + s(170, i)

            'diag(0, ien(i, 6)) = diag(0, ien(i, 6)) + s(189, i)
            'diag(1, ien(i, 6)) = diag(1, ien(i, 6)) + s(208, i)
            'diag(2, ien(i, 6)) = diag(2, ien(i, 6)) + s(209, i)
            'diag(3, ien(i, 6)) = diag(3, ien(i, 6)) + s(228, i)
            'diag(4, ien(i, 6)) = diag(4, ien(i, 6)) + s(229, i)
            'diag(5, ien(i, 6)) = diag(5, ien(i, 6)) + s(230, i)

            'diag(0, ien(i, 7)) = diag(0, ien(i, 7)) + s(252, i)
            'diag(1, ien(i, 7)) = diag(1, ien(i, 7)) + s(274, i)
            'diag(2, ien(i, 7)) = diag(2, ien(i, 7)) + s(275, i)
            'diag(3, ien(i, 7)) = diag(3, ien(i, 7)) + s(297, i)
            'diag(4, ien(i, 7)) = diag(4, ien(i, 7)) + s(298, i)
            'diag(5, ien(i, 7)) = diag(5, ien(i, 7)) + s(299, i)
            diag(1, ien(i, 1)) = diag(1, ien(i, 1)) + s(1, i)
            diag(2, ien(i, 1)) = diag(2, ien(i, 1)) + s(2, i)
            diag(3, ien(i, 1)) = diag(3, ien(i, 1)) + s(3, i)
            diag(4, ien(i, 1)) = diag(4, ien(i, 1)) + s(4, i)
            diag(5, ien(i, 1)) = diag(5, ien(i, 1)) + s(5, i)
            diag(6, ien(i, 1)) = diag(6, ien(i, 1)) + s(6, i)

            diag(1, ien(i, 2)) = diag(1, ien(i, 2)) + s(10, i)
            diag(2, ien(i, 2)) = diag(2, ien(i, 2)) + s(14, i)
            diag(3, ien(i, 2)) = diag(3, ien(i, 2)) + s(15, i)
            diag(4, ien(i, 2)) = diag(4, ien(i, 2)) + s(19, i)
            diag(5, ien(i, 2)) = diag(5, ien(i, 2)) + s(20, i)
            diag(6, ien(i, 2)) = diag(6, ien(i, 2)) + s(21, i)

            diag(1, ien(i, 3)) = diag(1, ien(i, 3)) + s(28, i)
            diag(2, ien(i, 3)) = diag(2, ien(i, 3)) + s(35, i)
            diag(3, ien(i, 3)) = diag(3, ien(i, 3)) + s(36, i)
            diag(4, ien(i, 3)) = diag(4, ien(i, 3)) + s(43, i)
            diag(5, ien(i, 3)) = diag(5, ien(i, 3)) + s(44, i)
            diag(6, ien(i, 3)) = diag(6, ien(i, 3)) + s(45, i)

            diag(1, ien(i, 4)) = diag(1, ien(i, 4)) + s(55, i)
            diag(2, ien(i, 4)) = diag(2, ien(i, 4)) + s(65, i)
            diag(3, ien(i, 4)) = diag(3, ien(i, 4)) + s(66, i)
            diag(4, ien(i, 4)) = diag(4, ien(i, 4)) + s(76, i)
            diag(5, ien(i, 4)) = diag(5, ien(i, 4)) + s(77, i)
            diag(6, ien(i, 4)) = diag(6, ien(i, 4)) + s(78, i)

            diag(1, ien(i, 5)) = diag(1, ien(i, 5)) + s(91, i)
            diag(2, ien(i, 5)) = diag(2, ien(i, 5)) + s(104, i)
            diag(3, ien(i, 5)) = diag(3, ien(i, 5)) + s(105, i)
            diag(4, ien(i, 5)) = diag(4, ien(i, 5)) + s(118, i)
            diag(5, ien(i, 5)) = diag(5, ien(i, 5)) + s(119, i)
            diag(6, ien(i, 5)) = diag(6, ien(i, 5)) + s(120, i)

            diag(1, ien(i, 6)) = diag(1, ien(i, 6)) + s(136, i)
            diag(2, ien(i, 6)) = diag(2, ien(i, 6)) + s(152, i)
            diag(3, ien(i, 6)) = diag(3, ien(i, 6)) + s(153, i)
            diag(4, ien(i, 6)) = diag(4, ien(i, 6)) + s(169, i)
            diag(5, ien(i, 6)) = diag(5, ien(i, 6)) + s(170, i)
            diag(6, ien(i, 6)) = diag(6, ien(i, 6)) + s(171, i)

            diag(1, ien(i, 7)) = diag(1, ien(i, 7)) + s(190, i)
            diag(2, ien(i, 7)) = diag(2, ien(i, 7)) + s(209, i)
            diag(3, ien(i, 7)) = diag(3, ien(i, 7)) + s(210, i)
            diag(4, ien(i, 7)) = diag(4, ien(i, 7)) + s(229, i)
            diag(5, ien(i, 7)) = diag(5, ien(i, 7)) + s(230, i)
            diag(6, ien(i, 7)) = diag(6, ien(i, 7)) + s(231, i)

            diag(1, ien(i, 8)) = diag(1, ien(i, 8)) + s(253, i)
            diag(2, ien(i, 8)) = diag(2, ien(i, 8)) + s(275, i)
            diag(3, ien(i, 8)) = diag(3, ien(i, 8)) + s(276, i)
            diag(4, ien(i, 8)) = diag(4, ien(i, 8)) + s(298, i)
            diag(5, ien(i, 8)) = diag(5, ien(i, 8)) + s(299, i)
            diag(6, ien(i, 8)) = diag(6, ien(i, 8)) + s(300, i)
            ' YC 102418 END

        Next
        'Call Check2D(diag, 6, numnp, istep)
        For i = lft To llt

            ' YC 102418
            'lm(0, i) = ien(i, 0)
            'lm(1, i) = ien(i, 1)
            'lm(2, i) = ien(i, 2)
            'lm(3, i) = ien(i, 3)
            'lm(4, i) = ien(i, 4)
            'lm(5, i) = ien(i, 5)
            'lm(6, i) = ien(i, 6)
            'lm(7, i) = ien(i, 7)
            lm(1, i) = ien(i, 1)
            lm(2, i) = ien(i, 2)
            lm(3, i) = ien(i, 3)
            lm(4, i) = ien(i, 4)
            lm(5, i) = ien(i, 5)
            lm(6, i) = ien(i, 6)
            lm(7, i) = ien(i, 7)
            lm(8, i) = ien(i, 8)
            ' YC 102418 END

        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine bdg83(lft,llt,diag,s,lm,ien,ldgen2)                  
'c
'
'      implicit double precision (a-h,o-z)                           
'c                                                                     
'c===> module to assemble 8 node, 24 dof element into block diagonal   
'c                                                                     
'      logical ldgen2                                                  
'      common/double/iprec
'c NKC 9/14/99
'      common/nkcgar/idodiag
'
'      dimension diag(21,*),s(300,*),lm(24*iprec,*),ien(64,*)          
'
'c NKC 9/14/99
'c      write(6,*) 'entering bdg83.F'
'c                                                                     
'      if (ldgen2) then                                                
'c                                                                     
'c....... test for and contract redundant degrees of freedom           
'c                                                                     
'         do 600 k=lft,llt                                             
'c                                                                     
'         do 500 j=1,21                                                
'         if (lm(j,k).ne.0) then                                       
'            do 400 i=j+3,24,3                                         
'            if (lm(j,k).eq.lm(i,k)) then                              
'               jj = (j-1)*j/2                                         
'               ii = (i-1)*i/2                                         
'               do 100 l=1,j-1                                         
'               s(ii+l,k) = s(ii+l,k) + s(jj+l,k)                      
'  100          s(jj+l,k) = 0.                                         
'               jl = jj + j                                            
'               s(ii+j,k) = 2.*s(ii+j,k) + s(jl,k)                     
'               s(jl,k) = 0.                                           
'               do 200 l=j+1,i                                         
'               jl = jl + l - 1                                        
'               s(ii+l,k) = s(ii+l,k) + s(jl,k)                        
'  200          s(jl,k) = 0.                                           
'               il = ii + i                                            
'               do 300 l=i+1,24                                        
'               jl = jl + l - 1                                        
'               il = il + l - 1                                        
'               s(il,k) = s(il,k) + s(jl,k)                            
'  300          s(jl,k) = 0.                                           
'               lm(j,k) = 0                                            
'               go to 500                                              
'            endif                                                     
'  400       continue                                                  
'         endif                                                        
'  500    continue                                                     
'c                                                                     
'  600    continue                                                     
'c                                                                     
'      endif                                                           
'c                                                                     
'
'c      write(6,*) 'adding hex terms to stiffness diag'
'      do 700 i=lft,llt                                                
'
'c     NKC 9/13/99
'c	Set idodiag to true, this tells the next call to the spring
'c	stiffness function to compute spring contributions to the stiffness
'c	diagonal
'         idodiag=1
'
'         diag(1,ien(i,1)) = diag(1,ien(i,1)) + s(  1,i)               
'         diag(2,ien(i,1)) = diag(2,ien(i,1)) + s(  2,i)               
'         diag(3,ien(i,1)) = diag(3,ien(i,1)) + s(  3,i)               
'         diag(4,ien(i,1)) = diag(4,ien(i,1)) + s(  4,i)               
'         diag(5,ien(i,1)) = diag(5,ien(i,1)) + s(  5,i)               
'         diag(6,ien(i,1)) = diag(6,ien(i,1)) + s(  6,i)               
'c                                                                     
'         diag(1,ien(i,2)) = diag(1,ien(i,2)) + s( 10,i)               
'         diag(2,ien(i,2)) = diag(2,ien(i,2)) + s( 14,i)               
'         diag(3,ien(i,2)) = diag(3,ien(i,2)) + s( 15,i)               
'         diag(4,ien(i,2)) = diag(4,ien(i,2)) + s( 19,i)               
'         diag(5,ien(i,2)) = diag(5,ien(i,2)) + s( 20,i)               
'         diag(6,ien(i,2)) = diag(6,ien(i,2)) + s( 21,i)               
'c                                                                     
'         diag(1,ien(i,3)) = diag(1,ien(i,3)) + s( 28,i)               
'         diag(2,ien(i,3)) = diag(2,ien(i,3)) + s( 35,i)               
'         diag(3,ien(i,3)) = diag(3,ien(i,3)) + s( 36,i)               
'         diag(4,ien(i,3)) = diag(4,ien(i,3)) + s( 43,i)               
'         diag(5,ien(i,3)) = diag(5,ien(i,3)) + s( 44,i)               
'         diag(6,ien(i,3)) = diag(6,ien(i,3)) + s( 45,i)               
'c                                                                     
'         diag(1,ien(i,4)) = diag(1,ien(i,4)) + s( 55,i)               
'         diag(2,ien(i,4)) = diag(2,ien(i,4)) + s( 65,i)               
'         diag(3,ien(i,4)) = diag(3,ien(i,4)) + s( 66,i)               
'         diag(4,ien(i,4)) = diag(4,ien(i,4)) + s( 76,i)               
'         diag(5,ien(i,4)) = diag(5,ien(i,4)) + s( 77,i)               
'         diag(6,ien(i,4)) = diag(6,ien(i,4)) + s( 78,i)               
'c                                                                     
'         diag(1,ien(i,5)) = diag(1,ien(i,5)) + s( 91,i)               
'         diag(2,ien(i,5)) = diag(2,ien(i,5)) + s(104,i)               
'         diag(3,ien(i,5)) = diag(3,ien(i,5)) + s(105,i)               
'         diag(4,ien(i,5)) = diag(4,ien(i,5)) + s(118,i)               
'         diag(5,ien(i,5)) = diag(5,ien(i,5)) + s(119,i)               
'         diag(6,ien(i,5)) = diag(6,ien(i,5)) + s(120,i)               
'c                                                                     
'         diag(1,ien(i,6)) = diag(1,ien(i,6)) + s(136,i)               
'         diag(2,ien(i,6)) = diag(2,ien(i,6)) + s(152,i)               
'         diag(3,ien(i,6)) = diag(3,ien(i,6)) + s(153,i)               
'         diag(4,ien(i,6)) = diag(4,ien(i,6)) + s(169,i)               
'         diag(5,ien(i,6)) = diag(5,ien(i,6)) + s(170,i)               
'         diag(6,ien(i,6)) = diag(6,ien(i,6)) + s(171,i)               
'c                                                                     
'         diag(1,ien(i,7)) = diag(1,ien(i,7)) + s(190,i)               
'         diag(2,ien(i,7)) = diag(2,ien(i,7)) + s(209,i)               
'         diag(3,ien(i,7)) = diag(3,ien(i,7)) + s(210,i)               
'         diag(4,ien(i,7)) = diag(4,ien(i,7)) + s(229,i)               
'         diag(5,ien(i,7)) = diag(5,ien(i,7)) + s(230,i)               
'         diag(6,ien(i,7)) = diag(6,ien(i,7)) + s(231,i)               
'c                                                                     
'         diag(1,ien(i,8)) = diag(1,ien(i,8)) + s(253,i)               
'         diag(2,ien(i,8)) = diag(2,ien(i,8)) + s(275,i)               
'         diag(3,ien(i,8)) = diag(3,ien(i,8)) + s(276,i)               
'         diag(4,ien(i,8)) = diag(4,ien(i,8)) + s(298,i)               
'         diag(5,ien(i,8)) = diag(5,ien(i,8)) + s(299,i)               
'         diag(6,ien(i,8)) = diag(6,ien(i,8)) + s(300,i)               
'  700 continue                                                        
'c                                                                     
'      do 800 i=lft,llt                                                
'         lm(1,i) = ien(i,1)                                           
'         lm(2,i) = ien(i,2)                                           
'         lm(3,i) = ien(i,3)                                           
'         lm(4,i) = ien(i,4)                                           
'         lm(5,i) = ien(i,5)                                           
'         lm(6,i) = ien(i,6)                                           
'         lm(7,i) = ien(i,7)                                           
'         lm(8,i) = ien(i,8)                                           
'  800 continue                                                        
'
'      return                                                          
'      end                                                             

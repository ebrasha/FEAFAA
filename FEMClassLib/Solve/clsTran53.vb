'This file contains all the methods of tran53.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to perform block-diagonal transformation of 5 node 15 dof element matrix 
    ''' assumes ien array temporarily in first 5 words of lm array  
    ''' </summary>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    ''' <param name="idp"></param>
    ''' <param name="lm"></param>
    ''' <param name="s"></param>
    ''' <param name="diag"></param>
    ''' <param name="ldgen"></param>
    ''' <param name="dcheck"></param>
    Public Sub tran53(ByRef lft As Integer, ByRef llt As Integer, idp(,) As Integer,
                      ByRef lm(,) As Integer, ByRef s(,) As Double, ByRef diag(,) As Double,
                      ByRef ldgen As Boolean, ByRef dcheck As Double)

        Dim istart, ij, i As Integer

        Dim dg_i(65, 6), dg_j(65, 6) As Double  ' YC 092018  ' QW 12-12-2018-

        'For l = lft - 1 To llt - 1           ' YC 102418
        For l = lft To llt

            ' YC 102418
            'dg(l, 0, 0) = diag(0, lm(0, l))
            'dg(l, 1, 0) = diag(1, lm(0, l))
            'dg(l, 2, 0) = diag(2, lm(0, l))
            'dg(l, 3, 0) = diag(3, lm(0, l))
            'dg(l, 4, 0) = diag(4, lm(0, l))
            'dg(l, 5, 0) = diag(5, lm(0, l))
            'dg(l, 0, 1) = diag(0, lm(1, l))
            'dg(l, 1, 1) = diag(1, lm(1, l))
            'dg(l, 2, 1) = diag(2, lm(1, l))
            'dg(l, 3, 1) = diag(3, lm(1, l))
            'dg(l, 4, 1) = diag(4, lm(1, l))
            'dg(l, 5, 1) = diag(5, lm(1, l))
            'dg(l, 0, 2) = diag(0, lm(2, l))
            'dg(l, 1, 2) = diag(1, lm(2, l))
            'dg(l, 2, 2) = diag(2, lm(2, l))
            'dg(l, 3, 2) = diag(3, lm(2, l))
            'dg(l, 4, 2) = diag(4, lm(2, l))
            'dg(l, 5, 2) = diag(5, lm(2, l))
            'dg(l, 0, 3) = diag(0, lm(3, l))
            'dg(l, 1, 3) = diag(1, lm(3, l))
            'dg(l, 2, 3) = diag(2, lm(3, l))
            'dg(l, 3, 3) = diag(3, lm(3, l))
            'dg(l, 4, 3) = diag(4, lm(3, l))
            'dg(l, 5, 3) = diag(5, lm(3, l))
            'dg(l, 0, 4) = diag(0, lm(4, l))
            'dg(l, 1, 4) = diag(1, lm(4, l))
            'dg(l, 2, 4) = diag(2, lm(4, l))
            'dg(l, 3, 4) = diag(3, lm(4, l))
            'dg(l, 4, 4) = diag(4, lm(4, l))
            'dg(l, 5, 4) = diag(5, lm(4, l))
            dg(l, 1, 1) = diag(1, lm(1, l))
            dg(l, 2, 1) = diag(2, lm(1, l))
            dg(l, 3, 1) = diag(3, lm(1, l))
            dg(l, 4, 1) = diag(4, lm(1, l))
            dg(l, 5, 1) = diag(5, lm(1, l))
            dg(l, 6, 1) = diag(6, lm(1, l))
            dg(l, 1, 2) = diag(1, lm(2, l))
            dg(l, 2, 2) = diag(2, lm(2, l))
            dg(l, 3, 2) = diag(3, lm(2, l))
            dg(l, 4, 2) = diag(4, lm(2, l))
            dg(l, 5, 2) = diag(5, lm(2, l))
            dg(l, 6, 2) = diag(6, lm(2, l))
            dg(l, 1, 3) = diag(1, lm(3, l))
            dg(l, 2, 3) = diag(2, lm(3, l))
            dg(l, 3, 3) = diag(3, lm(3, l))
            dg(l, 4, 3) = diag(4, lm(3, l))
            dg(l, 5, 3) = diag(5, lm(3, l))
            dg(l, 6, 3) = diag(6, lm(3, l))
            dg(l, 1, 4) = diag(1, lm(4, l))
            dg(l, 2, 4) = diag(2, lm(4, l))
            dg(l, 3, 4) = diag(3, lm(4, l))
            dg(l, 4, 4) = diag(4, lm(4, l))
            dg(l, 5, 4) = diag(5, lm(4, l))
            dg(l, 6, 4) = diag(6, lm(4, l))
            dg(l, 1, 5) = diag(1, lm(5, l))
            dg(l, 2, 5) = diag(2, lm(5, l))
            dg(l, 3, 5) = diag(3, lm(5, l))
            dg(l, 4, 5) = diag(4, lm(5, l))
            dg(l, 5, 5) = diag(5, lm(5, l))
            dg(l, 6, 5) = diag(6, lm(5, l))
            ' YC 102418 END

        Next

        For j = 1 To 5
            istart = 3 * (j - 1) * (3 * j - 2) / 2
            For i = 1 To j - 1

                'c.... nonsymmetric blocks                                               

                ' YC 102418
                'ij = istart + 3 * i - 3
                'For l = lft - 1 To llt - 1
                '    stemp(l, 1) = s(ij, l)
                '    stemp(l, 1) = s(ij + 1, l)
                '    stemp(l, 2) = s(ij + 2, l)
                'Next

                'ij = ij + 3 * j - 3
                'For l = lft - 1 To llt - 1
                '    stemp(l, 3) = s(ij, l)
                '    stemp(l, 4) = s(ij + 1, l)
                '    stemp(l, 5) = s(ij + 2, l)
                'Next

                'ij = ij + 3 * j - 2    
                'For l = lft - 1 To llt - 1
                '    stemp(l, 6) = s(ij, l)
                '    stemp(l, 7) = s(ij + 1, l)
                '    stemp(l, 8) = s(ij + 2, l)
                'Next
                ij = istart + 3 * i - 2
                For l = lft To llt
                    stemp(l, 1) = s(ij, l)
                    stemp(l, 2) = s(ij + 1, l)
                    stemp(l, 3) = s(ij + 2, l)
                Next

                ij = ij + 3 * j - 2
                For l = lft To llt
                    stemp(l, 4) = s(ij, l)
                    stemp(l, 5) = s(ij + 1, l)
                    stemp(l, 6) = s(ij + 2, l)
                Next

                ij = ij + 3 * j - 1
                For l = lft To llt
                    stemp(l, 7) = s(ij, l)
                    stemp(l, 8) = s(ij + 1, l)
                    stemp(l, 9) = s(ij + 2, l)
                Next
                ' YC 102418 END


                ' YC 092018
                ''TODO - set proper dim
                'Dim dg_i(,), dg_j(,) As Double
                ''TODO - copy dg to dg_i and dg to dg_j
                'Call full33(lft - 1, llt - 1, stemp, dg_j, dg_i)
                ''TODO - copy dg_i to dg and dg_j to dg

                Call objComsub.ArrayExtract2Dfrom3D(dg, 0, 0, i, dg_i, 65, 6) ' QW 12-12-2018-
                Call objComsub.ArrayExtract2Dfrom3D(dg, 0, 0, j, dg_j, 65, 6)
                Call full33(lft, llt, stemp, dg_j, dg_i)
                Call objComsub.ArrayInsert2Dto3D(dg_i, 65, 6, dg, 0, 0, i)
                Call objComsub.ArrayInsert2Dto3D(dg_j, 65, 6, dg, 0, 0, j)
                ' YC 092018 END


                ' YC 102418
                'ij = istart + 3 * i - 3
                'For l = lft - 1 To llt - 1
                '    s(ij, l) = stemp(l, 0)
                '    s(ij + 1, l) = stemp(l, 1)
                '    s(ij + 2, l) = stemp(l, 2)
                'Next

                'ij = ij + 3 * j - 3
                'For l = lft - 1 To llt - 1
                '    s(ij, l) = stemp(l, 3)
                '    s(ij + 1, l) = stemp(l, 4)
                '    s(ij + 2, l) = stemp(l, 5)
                'Next

                'ij = ij + 3 * j - 2
                'For l = lft - 1 To llt - 1
                '    s(ij, l) = stemp(l, 6)
                '    s(ij + 1, l) = stemp(l, 7)
                '    s(ij + 2, l) = stemp(l, 8)
                'Next
                ij = istart + 3 * i - 2
                For l = lft To llt
                    s(ij, l) = stemp(l, 1)
                    s(ij + 1, l) = stemp(l, 2)
                    s(ij + 2, l) = stemp(l, 3)
                Next

                ij = ij + 3 * j - 2
                For l = lft To llt
                    s(ij, l) = stemp(l, 4)
                    s(ij + 1, l) = stemp(l, 5)
                    s(ij + 2, l) = stemp(l, 6)
                Next

                ij = ij + 3 * j - 1
                For l = lft To llt
                    s(ij, l) = stemp(l, 7)
                    s(ij + 1, l) = stemp(l, 8)
                    s(ij + 2, l) = stemp(l, 9)
                Next
                ' YC 102418 END

            Next

            'c.... symmetric block                                                   

            ' YC 102418
            'ij = istart + 3 * i - 3
            'For l = lft - 1 To llt - 1
            '    stemp(l, 0) = s(ij, l)
            'Next

            'ij = ij + 3 * j - 3
            'For l = lft - 1 To llt - 1
            '    stemp(l, 1) = s(ij, l)
            '    stemp(l, 2) = s(ij + 1, l)
            'Next

            'ij = ij + 3 * j - 2
            'For l = lft - 1 To llt - 1
            '    stemp(l, 3) = s(ij, l)
            '    stemp(l, 4) = s(ij + 1, l)
            '    stemp(l, 5) = s(ij + 2, l)
            'Next
            ij = istart + 3 * i - 2
            For l = lft To llt
                stemp(l, 1) = s(ij, l)
            Next

            ij = ij + 3 * j - 2
            For l = lft To llt
                stemp(l, 2) = s(ij, l)
                stemp(l, 3) = s(ij + 1, l)
            Next

            ij = ij + 3 * j - 1
            For l = lft To llt
                stemp(l, 4) = s(ij, l)
                stemp(l, 5) = s(ij + 1, l)
                stemp(l, 6) = s(ij + 2, l)
            Next
            ' YC 102418


            ' YC 092018
            ''TODO - set proper dim
            'Dim dg_copy(,) As Double
            ''TODO - copy dg to dg_copy
            'Call symm33(lft - 1, llt - 1, stemp, dg_copy)
            ''TODO - copy dg_copy to dg

            Call objComsub.ArrayExtract2Dfrom3D(dg, 0, 0, j, dg_j, 65, 6)       ' QW 12-12-2018-
            Call symm33(lft, llt, stemp, dg_j)
            Call objComsub.ArrayInsert2Dto3D(dg_j, 65, 6, dg, 0, 0, j)
            ' YC 092018 END


            ' YC 102418
            'ij = istart + 3 * i - 3
            'For l = lft - 1 To llt - 1
            '    s(ij, l) = stemp(l, 0)
            'Next

            'ij = ij + 3 * j - 3
            'For l = lft - 1 To llt - 1
            '    s(ij, l) = stemp(l, 1)
            '    s(ij + 1, l) = stemp(l, 2)
            'Next

            'ij = ij + 3 * j - 2
            'For l = lft - 1 To llt - 1
            '    s(ij, l) = stemp(l, 3)
            '    s(ij + 1, l) = stemp(l, 4)
            '    s(ij + 2, l) = stemp(l, 5)
            'Next
            ij = istart + 3 * i - 2
            For l = lft To llt
                s(ij, l) = stemp(l, 1)
            Next

            ij = ij + 3 * j - 2
            For l = lft To llt
                s(ij, l) = stemp(l, 2)
                s(ij + 1, l) = stemp(l, 3)
            Next

            ij = ij + 3 * j - 1
            For l = lft To llt
                s(ij, l) = stemp(l, 4)
                s(ij + 1, l) = stemp(l, 5)
                s(ij + 2, l) = stemp(l, 6)
            Next
            ' YC 102418 END

        Next

        'c.... recover first 5 words of lm array                                 

        ' YC 102418
        'For l = lft - 1 To llt - 1
        '    lm(4, l) = idp(1, lm(1, l))
        '    lm(3, l) = idp(0, lm(1, l))
        '    lm(2, l) = idp(2, lm(0, l))
        '    lm(1, l) = idp(1, lm(0, l))
        '    lm(0, l) = idp(0, lm(0, l))
        'Next
        For l = lft To llt
            lm(5, l) = idp(2, lm(2, l))
            lm(4, l) = idp(1, lm(2, l))
            lm(3, l) = idp(3, lm(1, l))
            lm(2, l) = idp(2, lm(1, l))
            lm(1, l) = idp(1, lm(1, l))
        Next
        ' YC 102418 END

        If ldgen Then

            'c....... test for and contract redundant degrees of freedom             

            Dim k As Integer    ' YC 102418

            'For k = lft - 1 To llt - 1     ' YC 102418
            '    For j = 0 To 4
            For k = lft To llt
                For j = 1 To 5
                    If lm(j, k) <> 0 Then
                        For i = j + 3 To 24 Step 3
                            If lm(j, k) = lm(i, k) Then
                                lm(j, k) = 0
                                Exit For
                            End If
                        Next
                    End If
                Next

            Next

        End If

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine tran53(lft,llt,idp,lm,s,diag,ldgen,dcheck)             
'c
'
'      implicit double precision (a-h,o-z)                         
'
'c                                                                       
'c===> module to perform block-diagonal transformation of                
'c        5 node 15 dof element matrix                                   
'c        assumes ien array temporarily in first 5 words of lm array     
'c                                                                       
'      common/double/iprec
'      common/bk301/ stemp(65,9),dg(65,6,8)                            
'      dimension idp(6,*),lm(15*iprec,*),s(121,*),diag(21,*)             
'      logical ldgen                                                     
'
'      do 100 l=lft,llt                                                  
'         dg(l,1,1) = diag(1,lm(1,l))                                    
'         dg(l,2,1) = diag(2,lm(1,l))                                    
'         dg(l,3,1) = diag(3,lm(1,l))                                    
'         dg(l,4,1) = diag(4,lm(1,l))                                    
'         dg(l,5,1) = diag(5,lm(1,l))                                    
'         dg(l,6,1) = diag(6,lm(1,l))                                    
'         dg(l,1,2) = diag(1,lm(2,l))                                    
'         dg(l,2,2) = diag(2,lm(2,l))                                    
'         dg(l,3,2) = diag(3,lm(2,l))                                    
'         dg(l,4,2) = diag(4,lm(2,l))                                    
'         dg(l,5,2) = diag(5,lm(2,l))                                    
'         dg(l,6,2) = diag(6,lm(2,l))                                    
'         dg(l,1,3) = diag(1,lm(3,l))                                    
'         dg(l,2,3) = diag(2,lm(3,l))                                    
'         dg(l,3,3) = diag(3,lm(3,l))                                    
'         dg(l,4,3) = diag(4,lm(3,l))                                    
'         dg(l,5,3) = diag(5,lm(3,l))                                    
'         dg(l,6,3) = diag(6,lm(3,l))                                    
'         dg(l,1,4) = diag(1,lm(4,l))                                    
'         dg(l,2,4) = diag(2,lm(4,l))                                    
'         dg(l,3,4) = diag(3,lm(4,l))                                    
'         dg(l,4,4) = diag(4,lm(4,l))                                    
'         dg(l,5,4) = diag(5,lm(4,l))                                    
'         dg(l,6,4) = diag(6,lm(4,l))                                    
'         dg(l,1,5) = diag(1,lm(5,l))                                    
'         dg(l,2,5) = diag(2,lm(5,l))                                    
'         dg(l,3,5) = diag(3,lm(5,l))                                    
'         dg(l,4,5) = diag(4,lm(5,l))                                    
'         dg(l,5,5) = diag(5,lm(5,l))                                    
'         dg(l,6,5) = diag(6,lm(5,l))                                    
'  100 continue                                                          
'c                                                                       
'      do 1600 j=1,5                                                     
'      istart = 3*(j-1)*(3*j-2)/2                                        
'      do 900 i=1,j-1                                                    
'c                                                                       
'c.... nonsymmetric blocks                                               
'c                                                                       
'      ij = istart + 3*i - 2                                             
'      do 300 l=lft,llt                                                  
'         stemp(l,1) = s(ij  ,l)                                         
'         stemp(l,2) = s(ij+1,l)                                         
'         stemp(l,3) = s(ij+2,l)                                         
'  300 continue                                                          
'c                                                                       
'      ij = ij + 3*j - 2                                                 
'      do 400 l=lft,llt                                                  
'         stemp(l,4) = s(ij  ,l)                                         
'         stemp(l,5) = s(ij+1,l)                                         
'         stemp(l,6) = s(ij+2,l)                                         
'  400 continue                                                          
'c                                                                       
'      ij = ij + 3*j - 1                                                 
'      do 500 l=lft,llt                                                  
'         stemp(l,7) = s(ij  ,l)                                         
'         stemp(l,8) = s(ij+1,l)                                         
'         stemp(l,9) = s(ij+2,l)                                         
'  500 continue                                                          
'c                                                                       
'      call full33(lft,llt,stemp,dg(1,1,j),dg(1,1,i))                    
'c                                                                       
'      ij = istart + 3*i - 2                                             
'      do 600 l=lft,llt                                                  
'         s(ij  ,l) = stemp(l,1)                                         
'         s(ij+1,l) = stemp(l,2)                                         
'         s(ij+2,l) = stemp(l,3)                                         
'  600 continue                                                          
'c                                                                       
'      ij = ij + 3*j - 2                                                 
'      do 700 l=lft,llt                                                  
'         s(ij  ,l) = stemp(l,4)                                         
'         s(ij+1,l) = stemp(l,5)                                         
'         s(ij+2,l) = stemp(l,6)                                         
'  700 continue                                                          
'c                                                                       
'      ij = ij + 3*j - 1                                                 
'      do 800 l=lft,llt                                                  
'         s(ij  ,l) = stemp(l,7)                                         
'         s(ij+1,l) = stemp(l,8)                                         
'         s(ij+2,l) = stemp(l,9)                                         
'  800 continue                                                          
'c                                                                       
'  900 continue                                                          
'c                                                                       
'c.... symmetric block                                                   
'c                                                                       
'      ij = istart + 3*i - 2                                             
'      do 1000 l=lft,llt                                                 
'         stemp(l,1) = s(ij  ,l)                                         
' 1000 continue                                                          
'c                                                                       
'      ij = ij + 3*j - 2                                                 
'      do 1100 l=lft,llt                                                 
'         stemp(l,2) = s(ij  ,l)                                         
'         stemp(l,3) = s(ij+1,l)                                         
' 1100 continue                                                          
'c                                                                       
'      ij = ij + 3*j - 1                                                 
'      do 1200 l=lft,llt                                                 
'         stemp(l,4) = s(ij  ,l)                                         
'         stemp(l,5) = s(ij+1,l)                                         
'         stemp(l,6) = s(ij+2,l)                                         
' 1200 continue                                                          
'c                                                                       
'      call symm33(lft,llt,stemp,dg(1,1,j))                              
'c                                                                       
'      ij = istart + 3*i - 2                                             
'      do 1300 l=lft,llt                                                 
'         s(ij  ,l) = stemp(l,1)                                         
' 1300 continue                                                          
'c                                                                       
'      ij = ij + 3*j - 2                                                 
'      do 1400 l=lft,llt                                                 
'         s(ij  ,l) = stemp(l,2)                                         
'         s(ij+1,l) = stemp(l,3)                                         
' 1400 continue                                                          
'c                                                                       
'      ij = ij + 3*j - 1                                                 
'      do 1500 l=lft,llt                                                 
'         s(ij  ,l) = stemp(l,4)                                         
'         s(ij+1,l) = stemp(l,5)                                         
'         s(ij+2,l) = stemp(l,6)                                         
' 1500 continue                                                          
'c                                                                       
' 1600 continue                                                          
'c                                                                       
'c.... recover first 5 words of lm array                                 
'c                                                                       
'      do 1700 l=lft,llt                                                 
'         lm(5,l) = idp(2,lm(2,l))                                       
'         lm(4,l) = idp(1,lm(2,l))                                       
'         lm(3,l) = idp(3,lm(1,l))                                       
'         lm(2,l) = idp(2,lm(1,l))                                       
'         lm(1,l) = idp(1,lm(1,l))                                       
' 1700 continue                                                          
'c                                                                                                                                           
'c                                                                       
'      if (ldgen) then                                                   
'c                                                                       
'c....... test for and contract redundant degrees of freedom             
'c                                                                       
'         do 2200 k=lft,llt                                              
'c                                                                       
'         do 2100 j=1,5                                                  
'         if (lm(j,k).ne.0) then                                         
'            do 2000 i=j+3,24,3                                          
'            if (lm(j,k).eq.lm(i,k)) then                                
'               lm(j,k) = 0                                              
'               go to 2100                                               
'            endif                                                       
' 2000       continue                                                    
'         endif                                                          
' 2100    continue                                                       
'c                                                                       
' 2200    continue                                                       
'c                                                                       
'      endif                                                             
'c                                                                       
'      return                                                            
'      end                                                               

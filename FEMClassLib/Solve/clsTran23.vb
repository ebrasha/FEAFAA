'This file contains all the methods of tran23.f
Partial Public Class clsSolve

    Public Sub tran23(ByRef nmelde As Integer, ByRef diag(,) As Double)

        Dim istart, ij, i As Integer

        Dim dg_i(65, 6), dg_j(65, 6) As Double  ' YC 092018


        'For ielem = 0 To nmelde - 1            ' YC 102418
        For ielem = 1 To nmelde

            ' YC 102418
            'dg(0, 0, 0) = diag(0, is_nodes(0, ielem))
            'dg(0, 1, 0) = diag(1, is_nodes(0, ielem))
            'dg(0, 2, 0) = diag(2, is_nodes(0, ielem))
            'dg(0, 3, 0) = diag(3, is_nodes(0, ielem))
            'dg(0, 4, 0) = diag(4, is_nodes(0, ielem))
            'dg(0, 5, 0) = diag(5, is_nodes(0, ielem))

            'dg(0, 0, 1) = diag(0, is_nodes(1, ielem))
            'dg(0, 1, 1) = diag(1, is_nodes(1, ielem))
            'dg(0, 2, 1) = diag(2, is_nodes(1, ielem))
            'dg(0, 3, 1) = diag(3, is_nodes(1, ielem))
            'dg(0, 4, 1) = diag(4, is_nodes(1, ielem))
            'dg(0, 5, 1) = diag(5, is_nodes(1, ielem))
            dg(1, 1, 1) = diag(1, is_nodes(1, ielem))
            dg(1, 2, 1) = diag(2, is_nodes(1, ielem))
            dg(1, 3, 1) = diag(3, is_nodes(1, ielem))
            dg(1, 4, 1) = diag(4, is_nodes(1, ielem))
            dg(1, 5, 1) = diag(5, is_nodes(1, ielem))
            dg(1, 6, 1) = diag(6, is_nodes(1, ielem))

            dg(1, 1, 2) = diag(1, is_nodes(2, ielem))
            dg(1, 2, 2) = diag(2, is_nodes(2, ielem))
            dg(1, 3, 2) = diag(3, is_nodes(2, ielem))
            dg(1, 4, 2) = diag(4, is_nodes(2, ielem))
            dg(1, 5, 2) = diag(5, is_nodes(2, ielem))
            dg(1, 6, 2) = diag(6, is_nodes(2, ielem))
            ' YC 102418 END

            For j = 1 To 2

                'istart = 3 * (j - 1) * (3 * j - 2) / 1     ' YC 102418
                istart = 3 * (j - 1) * (3 * j - 2) / 2

                For i = 1 To j - 1

                    'c.... nonsymmetric blocks                                               

                    ' YC 102418
                    'ij = istart + 3 * i - 2
                    'stemp(0, 0) = spring_stiff(ij, ielem) 
                    'stemp(0, 1) = spring_stiff(ij + 0, ielem)     
                    'stemp(0, 2) = spring_stiff(ij + 1, ielem)

                    'ij = ij + 3 * j - 2
                    'stemp(0, 3) = spring_stiff(ij, ielem)  
                    'stemp(0, 4) = spring_stiff(ij + 0, ielem)      
                    'stemp(0, 5) = spring_stiff(ij + 1, ielem)

                    'ij = ij + 3 * j - 2' YC 102418
                    'stemp(0, 6) = spring_stiff(ij, ielem) 
                    'stemp(0, 7) = spring_stiff(ij + 0, ielem) 
                    'stemp(0, 8) = spring_stiff(ij + 1, ielem)
                    ij = istart + 3 * i - 2
                    stemp(1, 1) = spring_stiff(ij, ielem)
                    stemp(1, 2) = spring_stiff(ij + 1, ielem)
                    stemp(1, 3) = spring_stiff(ij + 2, ielem)

                    ij = ij + 3 * j - 2
                    stemp(1, 4) = spring_stiff(ij, ielem)
                    stemp(1, 5) = spring_stiff(ij + 1, ielem)
                    stemp(1, 6) = spring_stiff(ij + 2, ielem)

                    ij = ij + 3 * j - 1
                    stemp(1, 7) = spring_stiff(ij, ielem)
                    stemp(1, 8) = spring_stiff(ij + 1, ielem)
                    stemp(1, 9) = spring_stiff(ij + 2, ielem)
                    ' YC 102418 END


                    ' YC 092018 
                    ''TODO - set proper dim
                    'Dim dg_i(,), dg_j(,) As Double
                    ''TODO - copy dg to dg_i and dg to dg_j
                    'Call full33(0, 0, stemp, dg_j, dg_i)
                    ''TODO - copy dg_i to dg and dg_j to dg

                    Call objComsub.ArrayExtract2Dfrom3D(dg, 0, 0, i, dg_i, 65, 6)               ' QW 12-12-2018-
                    Call objComsub.ArrayExtract2Dfrom3D(dg, 0, 0, j, dg_j, 65, 6)
                    Call full33(1, 1, stemp, dg_j, dg_i)
                    Call objComsub.ArrayInsert2Dto3D(dg_i, 65, 6, dg, 0, 0, i)
                    Call objComsub.ArrayInsert2Dto3D(dg_j, 65, 6, dg, 0, 0, j)
                    ' YC 092018 

                    ' YC 102418
                    'ij = istart + 3 * i - 3         
                    'spring_stiff(ij, ielem) = stemp(0, 0)
                    'spring_stiff(ij + 0, ielem) = stemp(0, 1)      
                    'spring_stiff(ij + 1, ielem) = stemp(0, 2)

                    'ij = ij + 3 * j - 3   
                    'spring_stiff(ij, ielem) = stemp(0, 3)
                    'spring_stiff(ij + 0, ielem) = stemp(0, 4)  
                    'spring_stiff(ij + 1, ielem) = stemp(0, 5)

                    'ij = ij + 3 * j - 2  
                    'spring_stiff(ij, ielem) = stemp(0, 6)
                    'spring_stiff(ij + 0, ielem) = stemp(0, 7)   
                    'spring_stiff(ij + 1, ielem) = stemp(0, 8)
                    ij = istart + 3 * i - 2
                    spring_stiff(ij, ielem) = stemp(1, 1)
                    spring_stiff(ij + 1, ielem) = stemp(1, 2)
                    spring_stiff(ij + 2, ielem) = stemp(1, 3)

                    ij = ij + 3 * j - 2
                    spring_stiff(ij, ielem) = stemp(1, 4)
                    spring_stiff(ij + 1, ielem) = stemp(1, 5)
                    spring_stiff(ij + 2, ielem) = stemp(1, 6)

                    ij = ij + 3 * j - 1
                    spring_stiff(ij, ielem) = stemp(1, 7)
                    spring_stiff(ij + 1, ielem) = stemp(1, 8)
                    spring_stiff(ij + 2, ielem) = stemp(1, 9)
                    ' YC 102418 END
                Next

                'c.... symmetric block                                                   

                ' YC 102418
                'ij = istart + 3 * i - 3
                'stemp(0, 0) = spring_stiff(ij, ielem)

                'ij = ij + 3 * j - 3 
                'stemp(0, 1) = spring_stiff(ij, ielem)
                'stemp(0, 2) = spring_stiff(ij + 0, ielem)

                'ij = ij + 3 * j - 2 
                'stemp(0, 3) = spring_stiff(ij, ielem)
                'stemp(0, 4) = spring_stiff(ij + 0, ielem)
                'stemp(0, 5) = spring_stiff(ij + 1, ielem)
                ij = istart + 3 * i - 2
                stemp(1, 1) = spring_stiff(ij, ielem)

                ij = ij + 3 * j - 2
                stemp(1, 2) = spring_stiff(ij, ielem)
                stemp(1, 3) = spring_stiff(ij + 1, ielem)

                ij = ij + 3 * j - 1
                stemp(1, 4) = spring_stiff(ij, ielem)
                stemp(1, 5) = spring_stiff(ij + 1, ielem)
                stemp(1, 6) = spring_stiff(ij + 2, ielem)
                ' YC 102418 END

                ' YC 092018
                ''TODO - set proper dim
                'Dim dg_copy(,) As Double
                ''TODO - copy dg to dg_copy
                'Call symm33(0, 0, stemp, dg_copy)
                ''TODO - copy dg_copy to dg

                Call objComsub.ArrayExtract2Dfrom3D(dg, 0, 0, j, dg_j, 65, 6)       ' QW 12-12-2018-
                Call symm33(1, 1, stemp, dg_j)
                Call objComsub.ArrayInsert2Dto3D(dg_j, 65, 6, dg, 0, 0, j)
                ' YC 092018 END


                ' YC 102418
                'ij = istart + 3 * i - 3
                'spring_stiff(ij, ielem) = stemp(0, 0)

                'ij = ij + 3 * j - 3
                'spring_stiff(ij, ielem) = stemp(0, 1)
                'spring_stiff(ij + 0, ielem) = stemp(0, 2)

                'ij = ij + 3 * j - 2 
                'spring_stiff(ij, ielem) = stemp(0, 3)
                'spring_stiff(ij + 0, ielem) = stemp(0, 4)
                'spring_stiff(ij + 1, ielem) = stemp(0, 5)
                ij = istart + 3 * i - 2
                spring_stiff(ij, ielem) = stemp(1, 1)

                ij = ij + 3 * j - 2
                spring_stiff(ij, ielem) = stemp(1, 2)
                spring_stiff(ij + 1, ielem) = stemp(1, 3)

                ij = ij + 3 * j - 1
                spring_stiff(ij, ielem) = stemp(1, 4)
                spring_stiff(ij + 1, ielem) = stemp(1, 5)
                spring_stiff(ij + 2, ielem) = stemp(1, 6)
                ' YC 102418 END

            Next

        Next

    End Sub
End Class

'  ref org fortran code 
'
'      subroutine tran23(nmelde,diag)
'
'
'      implicit double precision (a-h,o-z)
'
'c	NKC
'c===> use diagonal to transform spring elements
'c
'      common spring_stiff(21,10000),is_dofs(6,10000),is_nodes(2,10000)
'      dimension diag(21,*)
'      common/double/iprec
'      common/bk301/ stemp(65,9),dg(65,6,8)
'
'      do ielem=1, nmelde
'        dg(1,1,1)=diag(1,is_nodes(1,ielem))
'        dg(1,2,1)=diag(2,is_nodes(1,ielem))
'        dg(1,3,1)=diag(3,is_nodes(1,ielem))
'        dg(1,4,1)=diag(4,is_nodes(1,ielem))
'        dg(1,5,1)=diag(5,is_nodes(1,ielem))
'        dg(1,6,1)=diag(6,is_nodes(1,ielem))
'
'        dg(1,1,2)=diag(1,is_nodes(2,ielem))
'        dg(1,2,2)=diag(2,is_nodes(2,ielem))
'        dg(1,3,2)=diag(3,is_nodes(2,ielem))
'        dg(1,4,2)=diag(4,is_nodes(2,ielem))
'        dg(1,5,2)=diag(5,is_nodes(2,ielem))
'        dg(1,6,2)=diag(6,is_nodes(2,ielem))
'
'      do 1600 j=1,2                                                     
'      istart = 3*(j-1)*(3*j-2)/2                                        
'      do 900 i=1,j-1                                                    
'c                                                                       
'c.... nonsymmetric blocks                                               
'c                                                                       
'      ij = istart + 3*i - 2                                             
'         stemp(1,1) = spring_stiff(ij,ielem)
'         stemp(1,2) = spring_stiff(ij+1,ielem)
'         stemp(1,3) = spring_stiff(ij+2,ielem)
'c                                                                       
'      ij = ij + 3*j - 2                                                 
'         stemp(1,4) = spring_stiff(ij,ielem)
'         stemp(1,5) = spring_stiff(ij+1,ielem)
'         stemp(1,6) = spring_stiff(ij+2,ielem)
'c                                                                       
'      ij = ij + 3*j - 1                                                 
'         stemp(1,7) = spring_stiff(ij  ,ielem)
'         stemp(1,8) = spring_stiff(ij+1,ielem)
'         stemp(1,9) = spring_stiff(ij+2,ielem)
'c
'      call full33(1,1,stemp,dg(1,1,j),dg(1,1,i))
'c                                                                       
'      ij = istart + 3*i - 2                                             
'         spring_stiff(ij  ,ielem) = stemp(1,1)
'         spring_stiff(ij+1,ielem) = stemp(1,2)
'         spring_stiff(ij+2,ielem) = stemp(1,3)
'c                                                                       
'      ij = ij + 3*j - 2                                                 
'         spring_stiff(ij  ,ielem) = stemp(1,4)
'         spring_stiff(ij+1,ielem) = stemp(1,5)
'         spring_stiff(ij+2,ielem) = stemp(1,6)
'c                                                                       
'      ij = ij + 3*j - 1                                                 
'         spring_stiff(ij  ,ielem) = stemp(1,7)
'         spring_stiff(ij+1,ielem) = stemp(1,8)
'         spring_stiff(ij+2,ielem) = stemp(1,9)
'c                                                                       
'  900 continue                                                          
'c                                                                       
'c.... symmetric block                                                   
'c                                                                       
'      ij = istart + 3*i - 2                                             
'         stemp(1,1) = spring_stiff(ij  ,ielem)
'c                                                                       
'      ij = ij + 3*j - 2                                                 
'         stemp(1,2) = spring_stiff(ij  ,ielem)
'         stemp(1,3) = spring_stiff(ij+1,ielem)
'c                                                                       
'      ij = ij + 3*j - 1                                                 
'         stemp(1,4) = spring_stiff(ij  ,ielem)
'         stemp(1,5) = spring_stiff(ij+1,ielem)
'         stemp(1,6) = spring_stiff(ij+2,ielem)
'c                                                                       
'      call symm33(1,1,stemp,dg(1,1,j))
'c                                                                       
'      ij = istart + 3*i - 2                                             
'         spring_stiff(ij  ,ielem) = stemp(1,1)
'c                                                                       
'      ij = ij + 3*j - 2                                                 
'         spring_stiff(ij  ,ielem) = stemp(1,2)
'         spring_stiff(ij+1,ielem) = stemp(1,3)
'c                                                                       
'      ij = ij + 3*j - 1                                                 
'         spring_stiff(ij  ,ielem) = stemp(1,4)
'         spring_stiff(ij+1,ielem) = stemp(1,5)
'         spring_stiff(ij+2,ielem) = stemp(1,6)
'
'
' 1600 continue
'
'      enddo
'
'      return
'      end

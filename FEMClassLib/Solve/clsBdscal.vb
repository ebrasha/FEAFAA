'This file contains all the methods of bdscal.f
Partial Public Class clsSolve


    ' YC 092018
    Private idof(6, 64) As Integer
    Private r01(128), r02(128), r03(128), r04(128), r05(128), r06(128),
             dr01(128), dr02(128), dr03(128), dr04(128), dr05(128), dr06(128) As Double
    ' YC 092018 END


    ''' <summary>
    ''' to scale rhs or lhs vector with block-diagonal array 
    ''' </summary>
    ''' <param name="idp"></param>
    ''' <param name="diag"></param>
    ''' <param name="r"></param>
    ''' <param name="numnp"></param>
    ''' <param name="ltrans"></param>
    Public Sub bdscal(ByRef idp(,) As Integer, ByRef diag(,) As Double, ByRef r() As Double,
                     ByRef numnp As Integer, ByRef ltrans As Boolean)
        Dim nlt, ngrp, nnode As Integer

        Dim ng, i As Integer  ' YC 102418
        'Call Check2D(idp, 6, numnp)
        nlt = 0
        'ngrp = (numnp - 1) / 64 + 1
        ngrp = Fix((numnp - 1) / 64) + 1    ' QW 11-01-2019
        nnode = numnp - (ngrp - 1) * 64

        'For ng = 0 To ngrp - 1  ' YC 102418
        For ng = 1 To ngrp
            ' Call Check4(ng, nlt, ngrp, nnode)
            ' YC 102418
            'For i = 0 To nnode - 1   
            '    idof(5, i) = idp(5, nlt + i)
            '    idof(4, i) = idp(4, nlt + i)
            '    idof(3, i) = idp(3, nlt + i)
            '    idof(2, i) = idp(2, nlt + i)
            '    idof(1, i) = idp(1, nlt + i)
            '    idof(0, i) = idp(0, nlt + i)
            'Next
            For i = 1 To nnode
                idof(6, i) = idp(6, nlt + i)
                idof(5, i) = idp(5, nlt + i)
                idof(4, i) = idp(4, nlt + i)
                idof(3, i) = idp(3, nlt + i)
                idof(2, i) = idp(2, nlt + i)
                idof(1, i) = idp(1, nlt + i)
            Next
            ' YC 102418 END

            'Call objNik3d.azero(r01, 6 * 128)      ' YC 092018
            Call azero(r01, 128)
            Call azero(r02, 128)
            Call azero(r03, 128)
            Call azero(r04, 128)
            Call azero(r05, 128)
            Call azero(r06, 128)

            ' YC 102418
            'For i = 0 To nnode - 1
            'If idof(0, i) > 0 Then r01(i) = r(idof(0, i))
            'If idof(1, i) > 0 Then r02(i) = r(idof(1, i))
            'If idof(2, i) > 0 Then r03(i) = r(idof(2, i))
            'If idof(3, i) > 0 Then r04(i) = r(idof(3, i))
            'If idof(4, i) > 0 Then r05(i) = r(idof(4, i))
            'If idof(5, i) > 0 Then r06(i) = r(idof(5, i))
            'Next
            For i = 1 To nnode
                If idof(1, i) > 0 Then r01(i) = r(idof(1, i))
                If idof(2, i) > 0 Then r02(i) = r(idof(2, i))
                If idof(3, i) > 0 Then r03(i) = r(idof(3, i))
                If idof(4, i) > 0 Then r04(i) = r(idof(4, i))
                If idof(5, i) > 0 Then r05(i) = r(idof(5, i))
                If idof(6, i) > 0 Then r06(i) = r(idof(6, i))
            Next
            ' YC 102418 END

            If ltrans Then

                ' YC 102418
                'For i = 0 To nnode - 1
                '    dr01(i) = diag(0, nlt + i) * r01(i)
                '    dr02(i) = diag(2, nlt + i) * (r02(i) - dr01(i) * diag(1, nlt + i))
                '    dr03(i) = diag(5, nlt + i) * (r03(i) - dr01(i) * diag(3, nlt + i) - dr02(i) * diag(4, nlt + i))
                '    dr04(i) = diag(9, nlt + i) * (r04(i) - dr01(i) * diag(6, nlt + i) -
                '    dr02(i) * diag(7, nlt + i) - dr03(i) * diag(8, nlt + i))
                '    dr05(i) = diag(14, nlt + i) * (r05(i) - dr01(i) * diag(10, nlt + i) -
                '    dr02(i) * diag(11, nlt + i) - dr03(i) * diag(12, nlt + i) - dr04(i) * diag(13, nlt + i))
                '    dr06(i) = diag(20, nlt + i) * (r06(i) - dr01(i) * diag(15, nlt + i) -
                '    dr02(i) * diag(16, nlt + i) - dr03(i) * diag(17, nlt + i) -
                '    dr04(i) * diag(18, nlt + i) - dr05(i) * diag(19, nlt + i))
                'Next
                For i = 1 To nnode
                    dr01(i) = diag(1, nlt + i) * r01(i)
                    dr02(i) = diag(3, nlt + i) * (r02(i) - dr01(i) * diag(2, nlt + i))
                    dr03(i) = diag(6, nlt + i) * (r03(i) - dr01(i) * diag(4, nlt + i) -
                              dr02(i) * diag(5, nlt + i))
                    dr04(i) = diag(10, nlt + i) * (r04(i) - dr01(i) * diag(7, nlt + i) -
                              dr02(i) * diag(8, nlt + i) - dr03(i) * diag(9, nlt + i))
                    dr05(i) = diag(15, nlt + i) * (r05(i) - dr01(i) * diag(11, nlt + i) -
                              dr02(i) * diag(12, nlt + i) - dr03(i) * diag(13, nlt + i) -
                              dr04(i) * diag(14, nlt + i))
                    dr06(i) = diag(21, nlt + i) * (r06(i) - dr01(i) * diag(16, nlt + i) -
                              dr02(i) * diag(17, nlt + i) - dr03(i) * diag(18, nlt + i) -
                              dr04(i) * diag(19, nlt + i) - dr05(i) * diag(20, nlt + i))
                Next
                ' YC 102418 END

            Else

                ' YC 102418
                'For i = 0 To nnode - 1
                '    dr06(i) = diag(20, nlt + i) * r06(i)
                '    dr05(i) = diag(14, nlt + i) * (r05(i) - diag(19, nlt + i) * dr06(i))
                '    dr04(i) = diag(9, nlt + i) * (r04(i) - diag(18, nlt + i) * dr06(i) - diag(13, nlt + i) * dr05(i))
                '    dr03(i) = diag(5, nlt + i) * (r03(i) - diag(17, nlt + i) * dr06(i) -
                '    diag(12, nlt + i) * dr05(i) - diag(8, nlt + i) * dr04(i))
                '    dr02(i) = diag(2, nlt + i) * (r02(i) - diag(16, nlt + i) * dr06(i) -
                '    diag(11, nlt + i) * dr05(i) - diag(7, nlt + i) * dr04(i) - diag(4, nlt + i) * dr03(i))
                '    dr01(i) = diag(0, nlt + i) * (r01(i) - diag(15, nlt + i) * dr06(i) -
                '    diag(10, nlt + i) * dr05(i) - diag(6, nlt + i) * dr04(i) -
                '    diag(3, nlt + i) * dr03(i) - diag(1, nlt + i) * dr02(i))
                'Next
                For i = 1 To nnode
                    dr06(i) = diag(21, nlt + i) * r06(i)
                    dr05(i) = diag(15, nlt + i) * (r05(i) - diag(20, nlt + i) * dr06(i))
                    dr04(i) = diag(10, nlt + i) * (r04(i) - diag(19, nlt + i) * dr06(i) -
                              diag(14, nlt + i) * dr05(i))
                    dr03(i) = diag(6, nlt + i) * (r03(i) - diag(18, nlt + i) * dr06(i) -
                              diag(13, nlt + i) * dr05(i) - diag(9, nlt + i) * dr04(i))
                    dr02(i) = diag(3, nlt + i) * (r02(i) - diag(17, nlt + i) * dr06(i) -
                              diag(12, nlt + i) * dr05(i) - diag(8, nlt + i) * dr04(i) -
                              diag(5, nlt + i) * dr03(i))
                    dr01(i) = diag(1, nlt + i) * (r01(i) - diag(16, nlt + i) * dr06(i) -
                              diag(11, nlt + i) * dr05(i) - diag(7, nlt + i) * dr04(i) -
                              diag(4, nlt + i) * dr03(i) - diag(2, nlt + i) * dr02(i))
                Next
                ' YC 102418 END

            End If

            For i = 1 To nnode           ' QW 12-12-2018-
                For ii = 1 To 6
                    If (idof(ii, i) = 10736) Then
                        ii = ii
                    End If
                Next ii

                ' YC 102418
                'If idof(0, i) > 0 Then r(idof(0, i)) = dr01(i)
                'If idof(1, i) > 0 Then r(idof(1, i)) = dr02(i)
                'If idof(2, i) > 0 Then r(idof(2, i)) = dr03(i)
                'If idof(3, i) > 0 Then r(idof(3, i)) = dr04(i)
                'If idof(4, i) > 0 Then r(idof(4, i)) = dr05(i)
                'If idof(5, i) > 0 Then r(idof(5, i)) = dr06(i)
                If idof(1, i) > 0 Then r(idof(1, i)) = dr01(i)
                If idof(2, i) > 0 Then r(idof(2, i)) = dr02(i)
                If idof(3, i) > 0 Then r(idof(3, i)) = dr03(i)
                If idof(4, i) > 0 Then r(idof(4, i)) = dr04(i)
                If idof(5, i) > 0 Then r(idof(5, i)) = dr05(i)
                If idof(6, i) > 0 Then r(idof(6, i)) = dr06(i)
                ' YC 102418 END

            Next

            nlt = nlt + nnode
            nnode = 64
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine bdscal(idp,diag,r,numnp,ltrans)                       
'c
'      implicit double precision (a-h,o-z)                           
'c                                                                      
'c===> module to scale rhs or lhs vector with block-diagonal array      
'c                                                                      
'      common /bk31/ idof(6,64) ,                                       
'     1             r01(128),r02(128),r03(128),r04(128),                
'     2             r05(128),r06(128),                                  
'     7             dr01(128),dr02(128),dr03(128),dr04(128),            
'     8             dr05(128),dr06(128),                                
'     3             dummy(1)                                            
'      dimension idp(6,*),diag(21,*),r(*)                                
'      logical ltrans                                                   
'c                                                                      
'      nlt   = 0                                                        
'      ngrp  = (numnp-1)/64 + 1                                         
'      nnode = numnp - (ngrp-1)*64                                      
'      do 800 ng=1,ngrp                                                 
'c
'      do 100 i=1,nnode                                                 
'      idof(6,i) = idp(6,nlt+i)                                          
'      idof(5,i) = idp(5,nlt+i)                                          
'      idof(4,i) = idp(4,nlt+i)                                          
'      idof(3,i) = idp(3,nlt+i)                                          
'      idof(2,i) = idp(2,nlt+i)                                          
'      idof(1,i) = idp(1,nlt+i)                                          
'c
'  100 continue
'c                                                                      
'      call azero(r01,6*128)                                            
'      do 400 i=1,nnode                                                 
'      if (idof(1,i).gt.0) r01(i) = r(idof(1,i))                        
'      if (idof(2,i).gt.0) r02(i) = r(idof(2,i))                        
'      if (idof(3,i).gt.0) r03(i) = r(idof(3,i))                        
'      if (idof(4,i).gt.0) r04(i) = r(idof(4,i))                        
'      if (idof(5,i).gt.0) r05(i) = r(idof(5,i))                        
'  400 if (idof(6,i).gt.0) r06(i) = r(idof(6,i))                        
'c                                                                      
'      if (ltrans) then                                                 
'         do 500 i=1,nnode                                              
'         dr01(i) = diag( 1,nlt+i)*r01(i)                               
'         dr02(i) = diag( 3,nlt+i)*(r02(i) - dr01(i)*diag( 2,nlt+i))    
'         dr03(i) = diag( 6,nlt+i)*(r03(i) - dr01(i)*diag( 4,nlt+i)     
'     &           - dr02(i)*diag( 5,nlt+i))                             
'         dr04(i) = diag(10,nlt+i)*(r04(i) - dr01(i)*diag( 7,nlt+i)     
'     &           - dr02(i)*diag( 8,nlt+i) - dr03(i)*diag( 9,nlt+i))    
'         dr05(i) = diag(15,nlt+i)*(r05(i) - dr01(i)*diag(11,nlt+i)     
'     &           - dr02(i)*diag(12,nlt+i) - dr03(i)*diag(13,nlt+i)     
'     &           - dr04(i)*diag(14,nlt+i))                             
'         dr06(i) = diag(21,nlt+i)*(r06(i) - dr01(i)*diag(16,nlt+i)     
'     &           - dr02(i)*diag(17,nlt+i) - dr03(i)*diag(18,nlt+i)     
'     &           - dr04(i)*diag(19,nlt+i) - dr05(i)*diag(20,nlt+i))    
'  500    continue                                                      
'      else                                                             
'         do 600 i=1,nnode                                              
'         dr06(i) = diag(21,nlt+i)*r06(i)                               
'         dr05(i) = diag(15,nlt+i)*(r05(i) - diag(20,nlt+i)*dr06(i))    
'         dr04(i) = diag(10,nlt+i)*(r04(i) - diag(19,nlt+i)*dr06(i)     
'     &           - diag(14,nlt+i)*dr05(i))                             
'         dr03(i) = diag( 6,nlt+i)*(r03(i) - diag(18,nlt+i)*dr06(i)     
'     &           - diag(13,nlt+i)*dr05(i) - diag( 9,nlt+i)*dr04(i))    
'         dr02(i) = diag( 3,nlt+i)*(r02(i) - diag(17,nlt+i)*dr06(i)     
'     &           - diag(12,nlt+i)*dr05(i) - diag( 8,nlt+i)*dr04(i)     
'     &           - diag( 5,nlt+i)*dr03(i))                             
'         dr01(i) = diag( 1,nlt+i)*(r01(i) - diag(16,nlt+i)*dr06(i)     
'     &           - diag(11,nlt+i)*dr05(i) - diag( 7,nlt+i)*dr04(i)     
'     &           - diag( 4,nlt+i)*dr03(i) - diag( 2,nlt+i)*dr02(i))    
'  600    continue                                                      
'      endif                                                            
'c                                                                      
'      do 700 i=1,nnode                                                 
'      if (idof(1,i).gt.0) r(idof(1,i)) = dr01(i)                       
'      if (idof(2,i).gt.0) r(idof(2,i)) = dr02(i)                       
'      if (idof(3,i).gt.0) r(idof(3,i)) = dr03(i)                       
'      if (idof(4,i).gt.0) r(idof(4,i)) = dr04(i)                       
'      if (idof(5,i).gt.0) r(idof(5,i)) = dr05(i)                       
'  700 if (idof(6,i).gt.0) r(idof(6,i)) = dr06(i)                       
'c                                                                      
'      nlt = nlt + nnode                                                
'      nnode = 64                                                       
'  800 continue                                                         
'c                                                                      
'      return                                                           
'      end                                                              

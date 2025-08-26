'This file contains all the methods of cskydg.f
Partial Public Class clsSolve

    ''' <summary>
    ''' module to condense inactive dof and compute cholesky factor 
    ''' of the 6 dof/node block-diagonal transformation array
    ''' </summary>
    ''' <param name="idp"></param>
    ''' <param name="diag"></param>
    ''' <param name="numnp"></param>
    Public Sub cskydg(ByRef idp(,) As Integer, ByRef diag(,) As Double, ByRef numnp As Integer)

        Dim id(6, 64), ng, nblk0 As Integer    ' QW 12-12-2018-

        Dim i, n As Integer ' YC 092018

        'nblk0 = (numnp - 1) / 64 + 1
        nblk0 = Fix((numnp - 1) / 64) + 1         ' QW 11-01-2019
        Dim iblk = numnp - (nblk0 - 1) * 64

        n = 1       ' QW 12-12-2018-

        Static ik01 As Integer
        ik01 = ik01 + 1
        'If ik01 = 2 Then
        'Call Check2D(diag, 6, numnp, 1)
        'End If

        'Dim nfirst = 0             ' YC 102418
        'For ng = 0 To nblk - 1
        '    For i = 0 To iblk - 1
        Dim nfirst = 1
        For ng = 1 To nblk0
            For i = 1 To iblk

                'Call clsInput.unpkid(id, 0, i, idp, 0, n, 1)      ' YC 092018     YC 102418
                Call unpkid(id, 1, i, idp, 1, n, 1)

                ' YC 102418
                'If id(0, i) = 0 Or diag(0, n) = 0 Then
                '    diag(0, n) = 1.0
                '    diag(1, n) = 0.0
                '    diag(3, n) = 0.0
                '    diag(6, n) = 0.0
                '    diag(10, n) = 0.0
                '    diag(15, n) = 0.0
                'End If
                'If id(1, i) = 0 Or diag(2, n) = 0 Then
                '    diag(1, n) = 0.0
                '    diag(2, n) = 1.0
                '    diag(4, n) = 0.0
                '    diag(7, n) = 0.0
                '    diag(11, n) = 0.0
                '    diag(16, n) = 0.0
                'End If
                'If id(2, i) = 0 Or diag(5, n) = 0 Then
                '    diag(3, n) = 0.0
                '    diag(4, n) = 0.0
                '    diag(5, n) = 1.0
                '    diag(8, n) = 0.0
                '    diag(12, n) = 0.0
                '    diag(17, n) = 0.0
                'End If
                'If id(3, i) = 0 Or diag(9, n) = 0 Then
                '    diag(6, n) = 0.0
                '    diag(7, n) = 0.0
                '    diag(8, n) = 0.0
                '    diag(9, n) = 1.0
                '    diag(13, n) = 0.0
                '    diag(18, n) = 0.0
                'End If
                'If id(4, i) = 0 Or diag(14, n) = 0 Then
                '    diag(10, n) = 0.0
                '    diag(11, n) = 0.0
                '    diag(12, n) = 0.0
                '    diag(13, n) = 0.0
                '    diag(14, n) = 1.0
                '    diag(19, n) = 0.0
                'End If
                'If id(5, i) = 0 Or diag(20, n) = 0 Then
                '    diag(15, n) = 0.0
                '    diag(16, n) = 0.0
                '    diag(17, n) = 0.0
                '    diag(18, n) = 0.0
                '    diag(19, n) = 0.0
                '    diag(20, n) = 1.0
                'End If
                If id(1, i) = 0 Or diag(1, n) = 0 Then
                    diag(1, n) = 1.0
                    diag(2, n) = 0.0
                    diag(4, n) = 0.0
                    diag(7, n) = 0.0
                    diag(11, n) = 0.0
                    diag(16, n) = 0.0
                End If
                If id(2, i) = 0 Or diag(3, n) = 0 Then
                    diag(2, n) = 0.0
                    diag(3, n) = 1.0
                    diag(5, n) = 0.0
                    diag(8, n) = 0.0
                    diag(12, n) = 0.0
                    diag(17, n) = 0.0
                End If
                If id(3, i) = 0 Or diag(6, n) = 0 Then
                    diag(4, n) = 0.0
                    diag(5, n) = 0.0
                    diag(6, n) = 1.0
                    diag(9, n) = 0.0
                    diag(13, n) = 0.0
                    diag(18, n) = 0.0
                End If
                If id(4, i) = 0 Or diag(10, n) = 0 Then
                    diag(7, n) = 0.0
                    diag(8, n) = 0.0
                    diag(9, n) = 0.0
                    diag(10, n) = 1.0
                    diag(14, n) = 0.0
                    diag(19, n) = 0.0
                End If
                If id(5, i) = 0 Or diag(15, n) = 0 Then
                    diag(11, n) = 0.0
                    diag(12, n) = 0.0
                    diag(13, n) = 0.0
                    diag(14, n) = 0.0
                    diag(15, n) = 1.0
                    diag(20, n) = 0.0
                End If
                If id(6, i) = 0 Or diag(21, n) = 0 Then
                    diag(16, n) = 0.0
                    diag(17, n) = 0.0
                    diag(18, n) = 0.0
                    diag(19, n) = 0.0
                    diag(20, n) = 0.0
                    diag(21, n) = 1.0
                End If
                ' YC 102418 END

                n = n + 1
            Next

            ' YC 092018
            ''TODO - set proper dimension
            'Dim diag_copy(,) As Double
            ''TODO - copy diag to diag_copy
            'Call chsky06(1, iblk, diag_copy)
            ''TODO - copy diag_copy to diag

            'Call Check2D(diag, 21, numnp)
            'Call chsky06(1, iblk, diag(1, nfirst))
            Dim diag_nfirst(21, iblk) As Double
            Call objComsub.ArrayExtract2Dfrom2D(diag, 1 - 1, nfirst - 1, diag_nfirst, 21, iblk)
            Call chsky06(1, iblk, diag_nfirst)
            Call objComsub.ArrayInsert2Dto2D(diag_nfirst, 21, iblk, diag, 1 - 1, nfirst - 1)
            'Call Check2D(diag, 21, numnp)
            ' YC 092018 END


            n = nfirst

            ' YC 102418
            'For i = 0 To iblk - 1
            'If id(0, i) = 0 Then diag(0, n) = 0.0
            'If id(1, i) = 0 Then diag(2, n) = 0.0
            'If id(2, i) = 0 Then diag(5, n) = 0.0
            'If id(3, i) = 0 Then diag(9, n) = 0.0
            'If id(4, i) = 0 Then diag(14, n) = 0.0
            'If id(5, i) = 0 Then diag(20, n) = 0.0
            For i = 1 To iblk
                If id(1, i) = 0 Then diag(1, n) = 0.0
                If id(2, i) = 0 Then diag(3, n) = 0.0
                If id(3, i) = 0 Then diag(6, n) = 0.0
                If id(4, i) = 0 Then diag(10, n) = 0.0
                If id(5, i) = 0 Then diag(15, n) = 0.0
                If id(6, i) = 0 Then diag(21, n) = 0.0
                ' YC 102418 END

                n = n + 1
            Next

            nfirst = nfirst + iblk
            iblk = 64
        Next
        'If ik01 = 1 Then
        'Call Check2D(diag, 6, numnp, 1)
        'End If


    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine cskydg(idp,diag,numnp)                                 
'c
'
'      implicit double precision (a-h,o-z)                         
'
'c                                                                       
'c===> module to condense inactive dof and compute cholesky factor       
'c      of the 6 dof/node block-diagonal transformation array            
'c                                                                       
'      dimension idp(6,*),diag(21,*),id(6,64)                          
'
'c NKC 9/14/99
'c      write(6,*) 'entering cskydg.F'
'
'c                                                                       
'      nblk = (numnp-1)/64 + 1                                           
'      iblk = numnp - (nblk-1)*64                                        
'      n = 1                                                             
'      nfirst = 1                                                        
'      do 300 ng=1,nblk                                                  
'c                                                                       
'      do 100 i=1,iblk                                                   
'      call unpkid(id(1,i),idp(1,n),1)                                   
'      if (id(1,i).eq.0 .or. diag( 1,n).eq.0.) then                      
'         diag( 1,n) = 1.0                                               
'         diag( 2,n) = 0.0                                               
'         diag( 4,n) = 0.0                                               
'         diag( 7,n) = 0.0                                               
'         diag(11,n) = 0.0                                               
'         diag(16,n) = 0.0                                               
'      endif                                                             
'      if (id(2,i).eq.0 .or. diag( 3,n).eq.0.) then                      
'         diag( 2,n) = 0.0                                               
'         diag( 3,n) = 1.0                                               
'         diag( 5,n) = 0.0                                               
'         diag( 8,n) = 0.0                                               
'         diag(12,n) = 0.0                                               
'         diag(17,n) = 0.0                                               
'      endif                                                             
'      if (id(3,i).eq.0 .or. diag( 6,n).eq.0.) then                      
'         diag( 4,n) = 0.0                                               
'         diag( 5,n) = 0.0                                               
'         diag( 6,n) = 1.0                                               
'         diag( 9,n) = 0.0                                               
'         diag(13,n) = 0.0                                               
'         diag(18,n) = 0.0                                               
'      endif                                                             
'      if (id(4,i).eq.0 .or. diag(10,n).eq.0.) then                      
'         diag( 7,n) = 0.0                                               
'         diag( 8,n) = 0.0                                               
'         diag( 9,n) = 0.0                                               
'         diag(10,n) = 1.0                                               
'         diag(14,n) = 0.0                                               
'         diag(19,n) = 0.0                                               
'      endif                                                             
'      if (id(5,i).eq.0 .or. diag(15,n).eq.0.) then                      
'         diag(11,n) = 0.0                                               
'         diag(12,n) = 0.0                                               
'         diag(13,n) = 0.0                                               
'         diag(14,n) = 0.0                                               
'         diag(15,n) = 1.0                                               
'         diag(20,n) = 0.0                                               
'      endif                                                             
'      if (id(6,i).eq.0 .or. diag(21,n).eq.0.) then                      
'         diag(16,n) = 0.0                                               
'         diag(17,n) = 0.0                                               
'         diag(18,n) = 0.0                                               
'         diag(19,n) = 0.0                                               
'         diag(20,n) = 0.0                                               
'         diag(21,n) = 1.0                                               
'      endif                                                             
'      n = n + 1                                                         
'  100 continue                                                          
'c
'      call chsky06(1,iblk,diag(1,nfirst))                               
'
'c                                                                       
'      n = nfirst                                                        
'      do 200 i=1,iblk                                                   
'      if (id(1,i).eq.0) diag( 1,n) = 0.0                                
'      if (id(2,i).eq.0) diag( 3,n) = 0.0                                
'      if (id(3,i).eq.0) diag( 6,n) = 0.0                                
'      if (id(4,i).eq.0) diag(10,n) = 0.0                                
'      if (id(5,i).eq.0) diag(15,n) = 0.0                                
'      if (id(6,i).eq.0) diag(21,n) = 0.0                                
'      n = n + 1                                                         
'  200 continue                                                          
'c                                                                       
'      nfirst = nfirst + iblk                                            
'      iblk = 64                                                         
'  300 continue                                                          
'c                                                                       
'      return                                                            
'      end                                                               

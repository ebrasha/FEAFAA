'This file contains all the methods of ssblck.f
Imports System.Text

Partial Public Class clsSolve


    ' Public mbsize, neesf As Integer

    'Private nsfblk, isfblk(750 - 1) As Integer ' v3.0 003/062920-3 YC
    Private nsfblk, isfblk(750) As Integer



    ''' <summary>
    ''' to reorder list of 5 node elements (with 3 dof/node) to avoid recursion during vectorized operations
    ''' </summary>
    ''' <param name="s"></param>
    ''' <param name="ien"></param>
    ''' <param name="stemp"></param>
    ''' <param name="ientmp"></param>
    ''' <param name="numel"></param>
    Public Sub ssblck(ByRef s2(,) As Double, ByRef ien(,) As Integer, ByRef stemp(,) As Double,
                      ByRef ientmp(,) As Integer, ByRef numel As Integer)

        Dim sb = New StringBuilder()

        'Dim inode(64 * 5 - 1), nmel, nblock As Integer     ' YC 102418
        Dim inode(64 * 5), nmel, nblock As Integer

        Dim lsblck = False

        Dim maxblk = 500
        Dim maxgrp = 150
        Dim ibmin = 8

        nsfblk = 0
        Dim numtot = 0
        Dim istart, ilast, ibsize, jlast, nsfbm1, nvblck, numsel As Integer

        Dim ind, i, j As Integer ' YC 102418

        If mbsize = 1 Or numel < 64 Then

            '....... sequentially ordered                                          

            nmel = 64
            nblock = (numel - 1) / nmel + 1
            If nsfblk + nblock > maxblk Then
                sb.Clear()
                sb.AppendLine(" ************************************************************")
                sb.AppendLine(" *                   - FATAL ERROR -                        *")
                sb.AppendLine(" *  number of slide surface element blocks ({0})      *")
                sb.AppendLine(" *        exceeds maximum number allowed ({1})            *")
                sb.AppendLine(" ************************************************************")
                Dim msg2300 = String.Format(sb.ToString(), nsfblk.ToString("00000"), maxblk.ToString("00000"))

                PrintLine(lutty, msg2300)      ' YC 102418

                Environment.Exit(0)
            End If

            'For i = 0 To nblock - 2      ' YC 102418
            For i = 1 To nblock - 1
                isfblk(nsfblk + i) = -nmel
            Next
            nsfblk = nsfblk + nblock
            isfblk(nsfblk) = nmel * (nblock - 1) - numel
            Return

        End If

        '.... blocked el x el                                                  

        istart = 0
        ilast = 0
        lsblck = False
300:
        '.... find first element not assigned to a block                       

        'For ii = istart To numel - 1       ' YC 102418
        For ii = istart + 1 To numel

            'If ien(0, ii) <> -1 Then     ' YC 102418
            If ien(1, ii) <> -1 Then

                '....... start a new block                                             

                nsfblk = nsfblk + 1


                ' v3.0 003/062920-3 YC
                If nsfblk > 750 Then
                    sb.Clear()
                    sb.AppendLine(" ************************************************************")
                    sb.AppendLine(" *                   - FATAL ERROR -                        *")
                    sb.AppendLine(" *        number of slide surface node blocks               *")
                    sb.AppendLine(" *        exceeds maximum number allowed ({0})             *")
                    sb.AppendLine(" *                                                          *")
                    sb.AppendLine(" *     Please reduce number of slide surface node           *")
                    sb.AppendLine(" ************************************************************")
                    Dim msg = String.Format(sb.ToString(), 750.ToString("00000"))

                    PrintLine(lutty, msg)
                    Environment.Exit(0)
                End If
                ' v3.0 003/062920-3 YC END


                ibsize = 1
                istart = ii

                'For ind = 0 To 5 * mbsize - 1    ' YC 102418
                For ind = 1 To 5 * mbsize
                    inode(ind) = 0.0
                Next

                'For j = 0 To 4   ' YC 102418
                For j = 1 To 5
                    inode(j) = ien(j, ii)
                Next

                jlast = 1
                ilast = ilast + 1

                'For j = 0 To neesf - 1  ' YC 102418
                For j = 1 To neesf
                    ientmp(j, ilast) = ien(j, ii)
                Next

                'ien(0, ii) = -1        ' YC 102418
                ien(1, ii) = -1

                'For j = 0 To ksizsf - 1      ' YC 102418
                For j = 1 To ksizsf
                    'stemp(j, ilast) = s(j, ii)  's->s2 by YC? 092018
                    stemp(j, ilast) = s2(j, ii)
                Next

                '....... test unassigned elements for inclusion in current block       

                'For jj = ii To numel - 1   ' YC 102418
                For jj = ii + 1 To numel

                    'If ien(0, jj) <> -1 Then  ' YC 102418
                    If ien(1, jj) <> -1 Then

                        'For j = 0 To 4       ' YC 102418
                        '    For i = 0 To 5 * ibsize - 1
                        For j = 1 To 5
                            For i = 1 To 5 * ibsize
                                If ien(j, jj) = inode(i) Then GoTo 1200 ' QW 12-12-2018-
                                'Continue For ' which For YC? 
                            Next i
                        Next j

                        ibsize = ibsize + 1

                        'For j = 0 To 4    ' YC 102418
                        For j = 1 To 5
                            inode(5 * jlast + j) = ien(j, jj)
                        Next j

                        jlast = jlast + 1
                        ilast = ilast + 1

                        'For j = 0 To neesf - 1   ' YC 102418
                        For j = 1 To neesf
                            ientmp(j, ilast) = ien(j, jj)
                        Next j

                        'ien(0, jj) = -1  ' YC 102418
                        ien(1, jj) = -1

                        'For i = 0 To ksizsf - 1         ' YC 102418
                        For i = 1 To ksizsf

                            'stemp(i, ilast) = s(i, jj) 's->s2 by YC? 102418
                            stemp(i, ilast) = s2(i, jj)

                        Next

                        If ibsize = mbsize Then

                            '............. block has reached maximum size                          

                            isfblk(nsfblk) = mbsize
                            GoTo 300
                        End If

                    End If
1200:           Next

                '....... element list exhausted before block filled                    

                If ibsize >= ibmin Then
                    isfblk(nsfblk) = ibsize
                Else
                    nsfbm1 = nsfblk - 1

                    'If isfblk(nsfbm1) < 0 Then     ' v3.0 003/062920-2 YC
                    If isfblk(nsfbm1) < 0 And isfblk(nsfbm1) > -mbsize + ibmin + 1 Then

                        nsfblk = nsfbm1
                        isfblk(nsfblk) = isfblk(nsfblk) - ibsize
                    Else
                        isfblk(nsfblk) = -ibsize
                    End If
                End If
                GoTo 300

            End If

        Next

        'Call blkcpy(stemp, s, ksizsf * numel)  's->s2 by YC? 092018
        Call blkcpy2D(stemp, s2, ksizsf, numel)

        'For inn = 0 To neesf - 1       ' YC 102418
        '    For ine = 0 To numel - 1
        For inn = 1 To neesf
            For ine = 1 To numel
                ien(inn, ine) = ientmp(inn, ine)
            Next
        Next

        '.... compute average block size                                       

        Dim itemp0 As Integer = 0   ' QW 03-26-2019
        nvblck = 0
        numsel = 0

        'For i = 0 To nsfblk - 1    ' YC 102418
        For i = 1 To nsfblk
            If isfblk(i) > 0 Then
                itemp0 = itemp0 + isfblk(i)
                nvblck = nvblck + 1
            Else
                numsel = numsel - isfblk(i)
            End If
        Next

        Dim bmean = 0.0
        If nvblck > 0 Then bmean = CDbl(itemp0) / nvblck
        numtot = numtot + numel

        sb.Clear()
        sb.AppendLine("     slide surface elements:")
        sb.AppendLine("        total number of elements               ={0}")
        sb.AppendLine("        number of vector element blocks        ={1}")
        sb.AppendLine("        average number of elements per block   ={2}")
        sb.AppendLine("        total number of scalar elements        ={3}")

        Dim msg2000 = String.Format(sb.ToString(), numtot.ToString("##########"), nvblck.ToString("##########"),
                                    bmean.ToString("##########.0"), numsel.ToString("##########"))

        'Call Check2DT(s2, ksizsf, 10)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c               
'      subroutine ssblck(s,ien,stemp,ientmp,numel)
'c
'
'      implicit double precision (a-h,o-z)
'
'c                                                                      
'c===> module to reorder list of 5 node elements (with 3 dof/node)      
'c     to avoid recursion during vectorized operations              
'c                                                                      
'      common/double/iprec
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/ebye3/neehx,ksizhx,neesf,ksizsf
'      common/ebye5/mbsize,nhxblk,ihxblk(750),nsnode,nsfblk,isfblk(750)
'      
'      dimension s(ksizsf,1),ien(neesf*iprec,1),stemp(ksizsf,1),        
'     &          ientmp(neesf*iprec,1),inode(64*5)                      
'      logical lsblck                                                   
'c                                                                      
'
'      maxblk = 500                                                     
'      maxgrp = 150                                                     
'      ibmin = 8                                                        
'c                                                                                      
'         nsfblk = 0                                                    
'                                              
'         numtot = 0
'c                                                                      
'      if (mbsize.eq.1 .or. numel.lt.64 ) then                          
'c                                                                      
'c....... sequentially ordered                                          
'c                                                                      
'         nmel = 64                                                     
'         nblock = (numel-1)/nmel + 1                                   
'         if (nsfblk+nblock.gt.maxblk) then                             
'            write (lutty,2300) nsfblk,maxblk                           
'            stop                                                       
'         endif                                                         
'         do 200 i=1,nblock-1                                           
'  200    isfblk(nsfblk+i) = -nmel                                      
'         nsfblk = nsfblk + nblock                                      
'         isfblk(nsfblk  ) = nmel*(nblock-1) - numel                    
'                                   
'         return                                                        
'      endif                                                            
'c                                                                      
'c.... blocked el x el                                                  
'c                                                                      
'      istart = 0                                                       
'      ilast  = 0                                                       
'      lsblck = .false.                                                 
'  300 continue                                                         
'c                                                                      
'c.... find first element not assigned to a block                       
'c                                                                      
'      do 1300 ii=istart+1,numel                                        
'c                                                                      
'      if (ien(1,ii).ne.-1) then                                        
'c                                                                      
'c....... start a new block                                             
'c                                                                      
'         nsfblk = nsfblk + 1                                           
'         ibsize = 1                                                    
'         istart = ii                                                   
'         do 350 in=1,5*mbsize
'  350    inode(in)=0.0
'         do 400 j=1,5                                                  
'  400    inode(j) = ien(j,ii)                                          
'         jlast = 1                                                     
'         ilast = ilast + 1                                             
'         do 500 j=1,neesf                                              
'  500    ientmp(j,ilast) = ien(j,ii)                                   
'         ien(1,ii) = -1                                                
'         do 600 j=1,ksizsf                                             
'  600    stemp(j,ilast) = s(j,ii)                                      
'c                                                                      
'c....... test unassigned elements for inclusion in current block       
'c                                                                      
'         do 1200 jj=ii+1,numel                                         
'c                                                                      
'         if (ien(1,jj).ne.-1) then                                     
'c                                                                      
'            do 800 j=1,5                                               
'            do 700 i=1,5*ibsize                                        
'  700       if (ien(j,jj).eq.inode(i)) go to 1200                      
'  800       continue                                                   
'c                                                                      
'            ibsize = ibsize + 1                                        
'            do 900 j=1,5                                               
'  900       inode(5*jlast+j) = ien(j,jj)                               
'            jlast = jlast + 1                                          
'            ilast = ilast + 1                                          
'            do 1000 j=1,neesf                                          
' 1000       ientmp(j,ilast) = ien(j,jj)                                
'            ien(1,jj) = -1                                             
'            do 1100 i=1,ksizsf                                         
' 1100       stemp(i,ilast) = s(i,jj)                                   
'c                                                                      
'            if (ibsize.eq.mbsize) then                                 
'c                                                                      
'c............. block has reached maximum size                          
'c                                                                      
'               isfblk(nsfblk) = mbsize                                 
'               go to 300                                               
'            endif                                                      
'c                                                                      
'         endif                                                         
' 1200    continue                                                      
'c                                                                      
'c....... element list exhausted before block filled                    
'c                                                                      
'         if (ibsize.ge.ibmin) then                                     
'            isfblk(nsfblk) = ibsize                                    
'         else                                                          
'            nsfbm1 = nsfblk - 1                                        
'c                
'            if (isfblk(nsfbm1).lt.0 ) then                     
'               nsfblk = nsfbm1                                         
'               isfblk(nsfblk) = isfblk(nsfblk) - ibsize                
'            else                                                       
'               isfblk(nsfblk) = -ibsize                                
'            endif                                                      
'         endif                                                         
'         go to 300                                                     
'c                                                                      
'      endif                                                            
'c                                                                      
' 1300 continue                                                         
'c                                                                      
'      call blkcpy(stemp,s,ksizsf*numel)                                
'      do 1350 inn=1,neesf
'       do 1350 ine=1,numel
' 1350   ien(inn,ine)=ientmp(inn,ine)                                      
'c                                                                      
'c.... compute average block size                                       
'c                                                                      
'      itemp  = 0                                                       
'      nvblck = 0                                                       
'      numsel = 0                                                       
'      do 1400 i=1,nsfblk                                               
'      if (isfblk(i).gt.0) then                                         
'         itemp  = itemp + isfblk(i)                                    
'         nvblck = nvblck + 1                                           
'      else                                                             
'         numsel = numsel - isfblk(i)                                   
'      endif                                                            
' 1400 continue                                                         
'      bmean = 0                                                        
'      if (nvblck.gt.0) bmean = float(itemp)/nvblck                     
'      numtot = numtot + numel                                          
'      write (luo,2000) numtot,nvblck,bmean,numsel                      
'      write (lutty,2000) numtot,nvblck,bmean,numsel                    
'c                                                                      
'      return                                                           
' 2000 format(5x,'slide surface elements:'
'     &      /5x,'   total number of elements               =',i10
'     &      /5x,'   number of vector element blocks        =',i10
'     &      /5x,'   average number of elements per block   =',f10.1
'     &      /5x,'   total number of scalar elements        =',i10)
' 2300 format(//
'     &' ************************************************************',/
'     &' *                   - FATAL ERROR -                        *',/
'     &' *  number of slide surface element blocks (',i5,')      *',/
'     &' *        exceeds maximum number allowed (',i5,')            *',/
'     &' ************************************************************')
'      end                                                              

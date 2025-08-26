'This file contains all the methods of invert.f
Partial Public Class clsSolve


    Public nrcc As Integer          


    ''' <summary>
    ''' to drive transformation and factorization of el matrices
    ''' </summary>
    ''' <param name="diag"></param>
    ''' <param name="hxdata"></param>
    ''' <param name="sfdata"></param>
    Public Sub invert(ByRef diag(,) As Double, ByRef hxdata1(,) As Double, ByRef hxdata2(,) As Integer, ByRef sfdata1(,) As Double, ByRef sfdata2(,) As Integer)

        Dim i, nelg, kstore, nword, ng As Integer   ' QW 12-12-2018- remove kfact
        Dim dcheck As Double

        If nrcc > 0 Then
            Dim msg1 = "ik02 call connod in sub invert"
            Dim msg2 = "ik02 call sumnod 1 in sub invert"
        End If

        'Call Check2D(diag, 6, numnp, istep)


        ' YC 092018
        'Call cskydg(idp, diag, numnp)

        ' Dim diag_2D(21, numnp) As Double    ' diag=abdg YC?
        'Call objComsub.ArrayConvert1Dto2D(diag, diag_2D, 21, numnp)

        'Call cskydg(idp, diag_2D, numnp)
        Call cskydg(idp, diag, numnp)
        ' Call objComsub.ArrayConvert2Dto1D(diag_2D, 21, numnp, diag)
        ' YC 092018 END



        Dim nmel = 64

        '.... solid hexahedron elements                                        

        If numelh <> 0 Then

            nelg = Fix((numelh - 1) / nmel) + 1
            kstore = 1
            'lstore = kfact + numelh * ksizhx
            'lstore = 1
            ' YC 102418
            'TODO changed lft and llt to 0 based index. Important to check
            'lft = 0
            'llt = numelh - nmel * (nelg - 1) - 1
            lft = 1
            llt = numelh - nmel * (nelg - 1)
            ' YC 102418


            'For ng = 0 To nelg - 1     ' YC 102418
            For ng = 1 To nelg

                ' YC 092018
                'Call tran83(lft, llt, idp, hxdata.Skip(lstore).ToArray(), hxdata.Skip(kstore).ToArray(), diag, ldgen, dcheck)  

                'Call objComsub.ArrayConvert1Dto2D(diag, diag_2D, 21, numnp)

                ' Dim hxdata_kstore(ksizhx, llt), hxdata_kstore_2D(ksizhx, llt) As Double                ' QW 12-12-2018-
                'Dim hxdata_kstore(ksizhx, llt) As Double
                'Array.Copy(hxdata1, kstore, hxdata_kstore, 1, ksizhx * llt)
                'Call objComsub.ArrayExtract1Dfrom1D(hxdata1, kstore - 1, hxdata_kstore, ksizhx * llt)
                'Call objComsub.ArrayConvert1Dto2D(hxdata_kstore, hxdata_kstore_2D, ksizhx, llt)

                ' Dim hxdata_lstore(neehx * llt), hxdata_lstore_2D(neehx, llt) As Integer
                'Dim hxdata_lstore(neehx, llt) As Integer
                'Array.Copy(hxdata2, lstore, hxdata_lstore, 1, neehx * llt)
                'Call objComsub.ArrayExtract1Dfrom1D(hxdata2, lstore - 1, hxdata_lstore, neehx * llt)
                'Call objComsub.ArrayConvert1Dto2D(hxdata_lstore, hxdata_lstore_2D, neehx, llt)
                'Call tran83(lft, llt, idp, hxdata_lstore_2D, hxdata_kstore_2D, diag_2D, ldgen, dcheck)

                Dim s(ksizhx, llt) As Double, lm(neehx, llt) As Integer
                For i = lft To llt
                    For j = 1 To ksizhx
                        s(j, i) = hxdata1(j, kstore + i - 1)
                    Next
                    For j = 1 To neehx
                        lm(j, i) = hxdata2(j, kstore + i - 1)
                    Next
                Next


                'Call tran83(lft, llt, idp, hxdata_lstore, hxdata_kstore, diag_2D, ldgen, dcheck)  ' QW 12-12-2018-
                'Call tran83(ng, lft, llt, idp, lm, s, diag_2D, ldgen, dcheck) 
                Call tran83(ng, lft, llt, idp, lm, s, diag, ldgen, dcheck)  ' QW 12-12-2018-

                'Call objComsub.ArrayConvert2Dto1D(diag_2D, 21, numnp, diag)
                'Call objComsub.ArrayConvert2Dto1D(hxdata_lstore_2D, neehx, llt, hxdata_lstore)
                'Call objComsub.ArrayInsert1Dto1D(hxdata_lstore, neehx, hxdata2, lstore - 1)
                'Call objComsub.ArrayConvert2Dto1D(hxdata_kstore_2D, ksizhx, llt, hxdata_kstore)
                'Call objComsub.ArrayInsert1Dto1D(hxdata_kstore, ksizhx, hxdata1, kstore - 1)
                'Array.Copy(hxdata_kstore, 1, hxdata1, kstore, ksizhx * llt)
                'Array.Copy(hxdata_lstore, 1, hxdata2, lstore, neehx * llt)

                For i = lft To llt
                    For j = 1 To ksizhx
                        hxdata1(j, kstore + i - 1) = s(j, i)
                    Next
                    For j = 1 To neehx
                        hxdata2(j, kstore + i - 1) = lm(j, i)
                    Next
                Next
                ' YC 092018 END
                kstore = kstore + llt
                ' kstore = kstore + llt * ksizhx
                'lstore = lstore + llt * neehx

                'TODO changed llt to 0 based index. Important to check
                'llt = nmel - 1     ' YC 102418
                llt = nmel

            Next

            '.... integrated beam elements                                         
            '...  Spring elements

            ' YC 092018
            'Call tran23(nmelde, diag)

            'Call objComsub.ArrayConvert1Dto2D(diag, diag_2D, 21, numnp)
            Call tran23(nmelde, diag)
            ' Call tran23(nmelde, diag_2D)

            'Call objComsub.ArrayConvert2Dto1D(diag_2D, 21, numnp, diag)
            ' YC 092018 END

        End If
        ' QW 12-12-2018-

        '.... integrated shell elements                                        
        '.... slidesurface interface elememts                                  

        If numelf <> 0 Then

            nelg = Fix((numelf - 1) / nmel) + 1
            nword = neesf + ksizsf
            kstore = 1
            'lstore = kstore + nsnode * ksizsf
            'lstore = 1

            ' YC 102418
            ''TODO changed lft and llt to 0 based index. Important to check
            'lft = 0
            'llt = numelf - nmel * (nelg - 1) - 1

            lft = 1
            llt = numelf - nmel * (nelg - 1)
            ' YC 102418 END
            'Call Check4(nmel, numelf, nelg, llt)
            'For ng = 0 To nelg - 1       ' YC 102418
            For ng = 1 To nelg
                'Call Check4(ng, numelf, llt, nelg)
                ' YC 092018
                'Call tran53(lft, llt, idp, sfdata.Skip(lstore).ToArray(), sfdata.Skip(kstore).ToArray(), diag, ldgen, dcheck)      

                'Call objComsub.ArrayConvert1Dto2D(diag, diag_2D, 21, numnp)

                'Dim sfdata_lstore(neesf * llt), sfdata_lstore_2D(neesf, llt) As Integer                 ' QW 12-12-2018-
                'Call objComsub.ArrayExtract1Dfrom1D(sfdata2, lstore - 1, sfdata_lstore, neesf * llt)
                'Call objComsub.ArrayConvert1Dto2D(sfdata_lstore, sfdata_lstore_2D, neesf, llt)

                'Dim sfdata_kstore(ksizsf * llt), sfdata_kstore_2D(ksizsf, llt) As Double
                'Call objComsub.ArrayExtract1Dfrom1D(sfdata1, kstore - 1, sfdata_kstore, ksizsf * llt)
                'Call objComsub.ArrayConvert1Dto2D(sfdata_kstore, sfdata_kstore_2D, ksizsf, llt)
                'Call tran53(lft, llt, idp, sfdata_lstore_2D, sfdata_kstore_2D, diag_2D, ldgen, dcheck)

                Dim s(ksizsf, llt) As Double, lm(neesf, llt) As Integer
                For i = lft To llt
                    For j = 1 To ksizsf
                        s(j, i) = sfdata1(j, kstore + i - 1)
                    Next
                    For j = 1 To neesf
                        lm(j, i) = sfdata2(j, kstore + i - 1)
                    Next
                Next
                ' Call tran53(lft, llt, idp, sfdata_lstore_2D, sfdata_kstore_2D, diag, ldgen, dcheck)
                Call tran53(lft, llt, idp, lm, s, diag, ldgen, dcheck)

                'Call objComsub.ArrayConvert2Dto1D(diag_2D, 21, numnp, diag)
                'Call objComsub.ArrayConvert2Dto1D(sfdata_lstore_2D, neesf, llt, sfdata_lstore)
                'Call objComsub.ArrayInsert1Dto1D(sfdata_lstore, neesf, sfdata2, lstore - 1)
                'Call objComsub.ArrayConvert2Dto1D(sfdata_kstore_2D, ksizsf, llt, sfdata_kstore)
                'Call objComsub.ArrayInsert1Dto1D(sfdata_kstore, ksizsf, sfdata1, kstore - 1)

                For i = lft To llt
                    For j = 1 To ksizsf
                        sfdata1(j, kstore + i - 1) = s(j, i)
                    Next
                    For j = 1 To neesf
                        sfdata2(j, kstore + i - 1) = lm(j, i)
                    Next
                Next
                kstore = kstore + llt

                'kstore = kstore + llt * ksizsf
                'lstore = lstore + llt * neesf

                'TODO changed llt to 0 based index. Important to check
                'llt = nmel - 1     ' YC 102418
                llt = nmel

            Next
            'c.... rockwall interface elements                                      

        End If
    End Sub


End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine invert(diag,hxdata,sfdata) 
'      implicit double precision (a-h,o-z)                              
'
'c                                                                      
'c===> module to drive transformation & factorization of el matrices     
'c                                                                       
'      common/double/iprec
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e, 
'     & n20f,n20g
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09       
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem           
'      common/bk35/numdc,numudc,nrcc                                 
'c NKC 9/15/99
'      common/b10a/inpsd,nmmtde,nmelde,nmmass
'
'      common/block/nblk                                              
'      common/elcnts/numelf
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye3/neehx,ksizhx,neesf,ksizsf
'
'      common/ebye5/mbsize,nhxblk,ihxblk(750),nsnode,nsfblk,isfblk(750)
'
'      common/bkneq/neql
'      
'      common/iread/idp(54000),x(27000) 
'c      
'c NKC 9/15/99
'c      
'      dimension diag(*),hxdata(*),sfdata(*)
'
'      if (nrcc.gt.0) then                                            
'                
'      write(10,*) 'ik02 call connod in sub invert'                 
'      write(10,*) 'ik02 call sumnod 1 in sub invert'
'
'      endif                                                            
'c NKC 9/14/99
'
'      call cskydg(idp,diag,numnp)
'c                                                                                                                                                                                        
'      nmel=64                                                           
'c                                                                      
'c.... solid hexahedron elements                                        
'c                                                                      
'      if (numelh.eq.0) go to 1300 !800                                       
'c
'         nelg=(numelh-1)/nmel + 1                                      
'         kstore = 1                                                    
'         kfact  = 1                                                    
'c         if (lcrout.or.lchsky) kfact = 1 + numelh*ksizhx               
'         lstore = kfact + numelh*ksizhx                                
'         lft = 1                                                       
'         llt = numelh - nmel*(nelg-1)                                  
'         do 700 ng=1,nelg                                              
'        
'            call tran83(lft,llt,idp,hxdata(lstore),                 
'     &                  hxdata(kstore),diag,ldgen,dcheck)              
'                          
'            kstore = kstore + llt*ksizhx                               
'            kfact  = kfact  + llt*ksizhx                               
'            lstore = lstore + llt*neehx                                
'            llt = nmel                                                 
'  700    continue                                                      
'c                                                                      
'c.... integrated beam elements                                         
'c                                                                      
'c NKC 9/14/99
'c
'c...  Spring elements
'c
'      call tran23(nmelde,diag)
'c                                                                      
'c.... integrated shell elements                                        
'c                                                                                                    
'c.... slidesurface interface elememts                                  
'c                                                                      
' 1300 if (numelf.eq.0) go to 1800                                      
'c
'         nelg=(numelf-1)/nmel + 1                                      
'         nword = neesf + ksizsf                                        
'         kstore = 1                                                    
'         lstore = kstore + nsnode*ksizsf                               
'         kfact  = lstore + nsnode*neesf                                
'         lft = 1                                                       
'         llt = numelf - nmel*(nelg-1)                                  
'         do 1700 ng=1,nelg                                             
'
'            call tran53(lft,llt,idp,sfdata(lstore),            
'     &                  sfdata(kstore),diag,ldgen,dcheck)     
'                                                    
'         kstore = kstore + llt*ksizsf                                  
'         kfact  = kfact  + llt*ksizsf                                  
'         lstore = lstore + llt*neesf                                   
'         llt = nmel                                                    
' 1700    continue                                                      
'c                                                                      
'c.... rockwall interface elements                                      
'c
' 1800 continue
'c
'      return                                                           
'      end                                                              

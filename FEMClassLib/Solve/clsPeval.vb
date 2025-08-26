'This file contains all the methods of peval.f
Partial Public Class clsSolve


    Public ihxblk() As Integer     ' YC 092018


    ''' <summary>
    ''' to drive product evaluation for conjugate gradients
    ''' </summary>
    ''' <param name="ap"></param>
    ''' <param name="p"></param>
    ''' <param name="hxdata"></param>
    ''' <param name="sfdata"></param>
    Public Sub peval(ByRef ap() As Double, ByRef p() As Double, ByRef hxdata1(,) As Double, ByRef hxdata2(,) As Integer, ByRef sfdata1(,) As Double, ByRef sfdata2(,) As Integer)

        Dim i, nmel, kstore, ng As Integer     ' QW 12-12-2018-
        Dim p01, p02, p03, p04, p05, p06 As Double
        Dim ap01, ap02, ap03, ap04, ap05, ap06 As Double

        nmel = 64      ' QW 12-12-2018-

        'c.... solid element contribution                                        

        If numelh = 0 Then GoTo 600

        kstore = 1
        'lstore = numelh * ksizhx + 1
        'lstore = 1  ' QW 12-12-2018-
        lft = 1

        'For ng = 0 To nhxblk - 1   ' YC 102418
        For ng = 1 To nhxblk
            llt = ihxblk(ng)
            If llt < 0 Then     ' QW 12-12-2018-
                llt = -llt
            End If
            'Dim hxdata_kstore(ksizhx * llt), hxdata_kstore_2D(ksizhx, llt) As Double
            'Call objComsub.ArrayExtract1Dfrom1D(hxdata1, kstore - 1, hxdata_kstore, ksizhx * llt)
            'Call objComsub.ArrayConvert1Dto2D(hxdata_kstore, hxdata_kstore_2D, ksizhx, llt)
            'Dim hxdata_kstore(ksizhx, llt) As Double
            'Array.Copy(hxdata1, kstore, hxdata_kstore, 1, ksizhx * llt)
            'Dim s(ksizhx, llt) As Double, lm(neehx, llt) As Integer        ' QW 08-14-2019
            'For i = lft To llt
            'For j = 1 To ksizhx
            's(j, i) = hxdata1(j, kstore + i - 1)
            'Next
            'For j = 1 To neehx
            'lm(j, i) = hxdata2(j, kstore + i - 1)
            'Next
            'Next

            'Dim hxdata_lstore(neehx * llt), hxdata_lstore_2D(neehx, llt) As Integer        ' QW 12-12-2018-
            'Call objComsub.ArrayExtract1Dfrom1D(hxdata2, lstore - 1, hxdata_lstore, neehx * llt)
            'Call objComsub.ArrayConvert1Dto2D(hxdata_lstore, hxdata_lstore_2D, neehx, llt)
            'Dim hxdata_lstore(neehx, llt) As Integer        ' QW 12-12-2018-
            'Array.Copy(hxdata2, lstore, hxdata_lstore, 1, neehx * llt)
            'Call matv24(lft, llt, hxdata(kstore), hxdata(lstore), ap, p)        ' YC 092018
            ' If ng = 13 Then
            'Call Check2DT(s, 300, 64)
            'End If

            'Call matv24(lft, llt, s, lm, ap, p)
            Call matv24(lft, llt, hxdata1, hxdata2, ap, p, kstore)      ' QW 08-14-2019
            kstore = kstore + llt                                       ' QW 08-14-2019
            'lstore = lstore + llt * neehx
            'kstore = kstore + llt * ksizhx
        Next
        ' Call Check1D(ap, neql)
        'c.... NKC Spring element contribution

        'For i = 0 To nmelde - 1     ' YC 102418
        For i = 1 To nmelde
            If is_dofs(2, i) = 3148 Then
                i = i
            End If
            ' YC 102418
            'p01 = p(is_dofs(0, i))
            'p02 = p(is_dofs(1, i))
            'p03 = p(is_dofs(2, i))
            'p04 = p(is_dofs(3, i))
            'p05 = p(is_dofs(4, i))
            'p06 = p(is_dofs(5, i))
            p01 = p(is_dofs(1, i))
            p02 = p(is_dofs(2, i))
            p03 = p(is_dofs(3, i))
            p04 = p(is_dofs(4, i))
            p05 = p(is_dofs(5, i))
            p06 = p(is_dofs(6, i))

            'Dim ptemp(6 - 1) As Double
            'For j = 1 - 1 To 6 - 1
            '    If is_dofs(j, i) > 0 Then ptemp(j) = p(is_dofs(j, i) - 1) 'is_dofs(j, i)=0 YC? 
            'Next j

            'p01 = ptemp(0)
            'p02 = ptemp(1)
            'p03 = ptemp(2)
            'p04 = ptemp(3)
            'p05 = ptemp(4)
            'p06 = ptemp(5)
            ' YC 102418 END


            ' YC 102418
            'ap01 = spring_stiff(0, i) * p01 + spring_stiff(1, i) * p02 +
            '      spring_stiff(3, i) * p03 + spring_stiff(6, i) * p04 +
            '      spring_stiff(10, i) * p05 + spring_stiff(15, i) * p06
            'ap02 = spring_stiff(1, i) * p01 + spring_stiff(2, i) * p02 +
            '      spring_stiff(4, i) * p03 + spring_stiff(7, i) * p04 +
            '      spring_stiff(11, i) * p05 + spring_stiff(16, i) * p06
            'ap03 = spring_stiff(3, i) * p01 + spring_stiff(4, i) * p02 +
            '      spring_stiff(5, i) * p03 + spring_stiff(8, i) * p04 +
            '      spring_stiff(12, i) * p05 + spring_stiff(17, i) * p06
            'ap04 = spring_stiff(6, i) * p01 + spring_stiff(7, i) * p02 +
            '      spring_stiff(8, i) * p03 + spring_stiff(9, i) * p04 +
            '      spring_stiff(13, i) * p05 + spring_stiff(18, i) * p06
            'ap05 = spring_stiff(10, i) * p01 + spring_stiff(12, i) * p02 +
            '      spring_stiff(12, i) * p03 + spring_stiff(13, i) * p04 +
            '      spring_stiff(14, i) * p05 + spring_stiff(19, i) * p06
            'ap06 = spring_stiff(15, i) * p01 + spring_stiff(16, i) * p02 +
            '      spring_stiff(17, i) * p03 + spring_stiff(18, i) * p04 +
            '      spring_stiff(19, i) * p05 + spring_stiff(20, i) * p06
            ap01 = spring_stiff(1, i) * p01 + spring_stiff(2, i) * p02 +
                spring_stiff(4, i) * p03 + spring_stiff(7, i) * p04 +
                spring_stiff(11, i) * p05 + spring_stiff(16, i) * p06
            ap02 = spring_stiff(2, i) * p01 + spring_stiff(3, i) * p02 +
                spring_stiff(5, i) * p03 + spring_stiff(8, i) * p04 +
                spring_stiff(12, i) * p05 + spring_stiff(17, i) * p06
            ap03 = spring_stiff(4, i) * p01 + spring_stiff(5, i) * p02 +
                spring_stiff(6, i) * p03 + spring_stiff(9, i) * p04 +
                spring_stiff(13, i) * p05 + spring_stiff(18, i) * p06
            ap04 = spring_stiff(7, i) * p01 + spring_stiff(8, i) * p02 +
               spring_stiff(9, i) * p03 + spring_stiff(10, i) * p04 +
                spring_stiff(14, i) * p05 + spring_stiff(19, i) * p06
            ap05 = spring_stiff(11, i) * p01 + spring_stiff(12, i) * p02 +
                spring_stiff(13, i) * p03 + spring_stiff(14, i) * p04 +
               spring_stiff(15, i) * p05 + spring_stiff(20, i) * p06
            ap06 = spring_stiff(16, i) * p01 + spring_stiff(17, i) * p02 +
                spring_stiff(18, i) * p03 + spring_stiff(19, i) * p04 +
                spring_stiff(20, i) * p05 + spring_stiff(21, i) * p06
            ' YC 102418 END


            ' YC 102418
            'ap(is_dofs(0, i)) = ap(is_dofs(0, i)) + ap01
            'ap(is_dofs(1, i)) = ap(is_dofs(1, i)) + ap02
            'ap(is_dofs(2, i)) = ap(is_dofs(2, i)) + ap03
            'ap(is_dofs(3, i)) = ap(is_dofs(3, i)) + ap04
            'ap(is_dofs(4, i)) = ap(is_dofs(4, i)) + ap05
            'ap(is_dofs(5, i)) = ap(is_dofs(5, i)) + ap06
            ap(is_dofs(1, i)) = ap(is_dofs(1, i)) + ap01
            ap(is_dofs(2, i)) = ap(is_dofs(2, i)) + ap02
            ap(is_dofs(3, i)) = ap(is_dofs(3, i)) + ap03
            ap(is_dofs(4, i)) = ap(is_dofs(4, i)) + ap04
            ap(is_dofs(5, i)) = ap(is_dofs(5, i)) + ap05
            ap(is_dofs(6, i)) = ap(is_dofs(6, i)) + ap06

            'Dim aptemp(6 - 1) As Double
            'For j = 1 - 1 To 6 - 1
            '    If is_dofs(j, i) > 0 Then aptemp(j) = ap(is_dofs(j, i) - 1) 'is_dofs(j, i)=0 YC? 
            'Next j

            'aptemp(0) = aptemp(0) + ap01
            'aptemp(1) = aptemp(1) + ap02
            'aptemp(2) = aptemp(2) + ap03
            'aptemp(3) = aptemp(3) + ap04
            'aptemp(4) = aptemp(4) + ap05
            'aptemp(5) = aptemp(5) + ap06

            'For j = 1 - 1 To 6 - 1
            '    If is_dofs(j, i) > 0 Then ap(is_dofs(j, i) - 1) = aptemp(j) 'is_dofs(j, i)=0 YC? 
            'Next j
            ' YC 102418 END

        Next
        'Call Check1D(ap, neql)
        'c.... integrated shell element contribution                             

600:    If numelf = 0 Then Return

        kstore = 1
        'lstore = 1 + nsnode * ksizsf
        'lstore = 1  ' QW 12-12-2018-
        lft = 1

        'For ng = 0 To nsfblk - 1   ' YC 102418
        For ng = 1 To nsfblk
            llt = isfblk(ng)
            If llt < 0 Then     ' QW 12-12-2018-
                llt = -llt
            End If

            ' YC 092018
            'Call matv15(lft, llt, sfdata(kstore), sfdata(lstore), ap, p)    
            'Dim s(ksizsf, llt) As Double, lm(neesf, llt) As Integer    ' QW 08-14-2019
            'For i = lft To llt
            'For j = 1 To ksizsf
            's(j, i) = sfdata1(j, kstore + i - 1)
            'Next
            'For j = 1 To neesf
            'lm(j, i) = sfdata2(j, kstore + i - 1)
            'Next
            'Next
            'Call matv15(lft, llt, s, lm, ap, p)
            Call matv15(lft, llt, sfdata1, sfdata2, ap, p, kstore)      ' QW 08-14-2019
            kstore = kstore + llt
            'Dim sfdata_kstore(ksizsf * llt), sfdata_kstore_2D(ksizsf, llt) As Double
            'Call objComsub.ArrayExtract1Dfrom1D(sfdata1, kstore - 1, sfdata_kstore, ksizsf * llt)
            'Call objComsub.ArrayConvert1Dto2D(sfdata_kstore, sfdata_kstore_2D, ksizsf, llt)

            'Dim sfdata_lstore(neesf * llt), sfdata_lstore_2D(neesf, llt) As Integer        ' QW 12-12-2018-
            'Call objComsub.ArrayExtract1Dfrom1D(sfdata2, lstore - 1, sfdata_lstore, neesf * llt)
            '-Call objComsub.ArrayConvert1Dto2D(sfdata_lstore, sfdata_lstore_2D, neesf, llt)
            'Call matv15(lft, llt, sfdata_kstore_2D, sfdata_lstore_2D, ap, p)
            ' YC 092018 END

            'kstore = kstore + llt * ksizsf
            'lstore = lstore + llt * neesf
            'Call Check1D(ap, neql)
        Next
        'Call Check1D(ap, neql)
    End Sub


End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine peval (ap,p,hxdata,sfdata)            ! QW 11-11-2015
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c                                                                       
'c===> module to drive product evaluation for conjugate gradients        
'c                                                                       
'      common/double/iprec
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh
'      common/elcnts/numelf
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye3/neehx,ksizhx,neesf,ksizsf
'      common/ebye5/mbsize,nhxblk,ihxblk(750),nsnode,nsfblk,isfblk(750)
'      common/b10a/inpsd,nmmtde,nmelde,nmmass
'      common spring_stiff(21,10000),is_dofs(6,10000)
' 
'      dimension ap(*),p(*),hxdata(*),sfdata(*)
'c                                                                       
'                                  
'c  NKC 9/14/99
'c      write(6,*) 'entering peval.F'
'
'      nmel = 128                                                                                                        
'c                                                                       
'c.... solid element contribution                                        
'c                                                                       
'      if (numelh.eq.0) go to  600 !300                                        
'c                                                                       
'         kstore = 1                                                     
'         lstore = numelh*ksizhx + 1                                     
'c         if (lcrout.or.lchsky) lstore = 2*numelh*ksizhx + 1             
'         lft = 1                                                        
'         do 200 ng=1,nhxblk                                             
'            llt = ihxblk(ng)                                            
'            call matv24 (lft,llt,hxdata(kstore),hxdata(lstore),ap,p)    
'            lstore = lstore + llt*neehx                                 
'            kstore = kstore + llt*ksizhx                                
'  200    continue                                                       
'c                                                                       
'c.... NKC Spring element contribution
'c
'      do i=1, nmelde
'        p01=p(is_dofs(1,i))
'        p02=p(is_dofs(2,i))
'        p03=p(is_dofs(3,i))
'        p04=p(is_dofs(4,i))
'        p05=p(is_dofs(5,i))
'        p06=p(is_dofs(6,i))
'
'c        write(6,*) 'p',p01,p02,p03,p04,p05,p06
'
'        ap01=spring_stiff(1,i)*p01+spring_stiff(2,i)*p02+
'     &       spring_stiff(4,i)*p03+spring_stiff(7,i)*p04+
'     &       spring_stiff(11,i)*p05+spring_stiff(16,i)*p06
'        ap02=spring_stiff(2,i)*p01+spring_stiff(3,i)*p02+
'     &       spring_stiff(5,i)*p03+spring_stiff(8,i)*p04+
'     &       spring_stiff(12,i)*p05+spring_stiff(17,i)*p06
'        ap03=spring_stiff(4,i)*p01+spring_stiff(5,i)*p02+
'     &       spring_stiff(6,i)*p03+spring_stiff(9,i)*p04+
'     &       spring_stiff(13,i)*p05+spring_stiff(18,i)*p06
'        ap04=spring_stiff(7,i)*p01+spring_stiff(8,i)*p02+
'     &       spring_stiff(9,i)*p03+spring_stiff(10,i)*p04+
'     &       spring_stiff(14,i)*p05+spring_stiff(19,i)*p06
'        ap05=spring_stiff(11,i)*p01+spring_stiff(12,i)*p02+
'     &       spring_stiff(13,i)*p03+spring_stiff(14,i)*p04+
'     &       spring_stiff(15,i)*p05+spring_stiff(20,i)*p06
'        ap06=spring_stiff(16,i)*p01+spring_stiff(17,i)*p02+
'     &       spring_stiff(18,i)*p03+spring_stiff(19,i)*p04+
'     &       spring_stiff(20,i)*p05+spring_stiff(21,i)*p06
'
'        ap(is_dofs(1,i))=ap(is_dofs(1,i))+ap01
'        ap(is_dofs(2,i))=ap(is_dofs(2,i))+ap02
'        ap(is_dofs(3,i))=ap(is_dofs(3,i))+ap03
'        ap(is_dofs(4,i))=ap(is_dofs(4,i))+ap04
'        ap(is_dofs(5,i))=ap(is_dofs(5,i))+ap05
'        ap(is_dofs(6,i))=ap(is_dofs(6,i))+ap06
'
'      enddo
'c                                                                       
'c.... integrated shell element contribution                             
'c                                                                       
'600   if (numelf.eq.0) go to 1200   
'
'c                                                                       
'         kstore = 1                                                     
'         lstore = 1 + nsnode*ksizsf                                     
'         lft = 1                                                        
'         do 800 ng=1,nsfblk                                             
'               llt = isfblk(ng)                                         
'                                  
'               call matv15 (lft,llt,sfdata(kstore),sfdata(lstore),ap,p)
'                                                  
'            kstore = kstore + llt*ksizsf                                
'            lstore = lstore + llt*neesf                                 
'  800    continue                                                       
'c                                                                       
' 1200 return                                                            
'      end                                                               

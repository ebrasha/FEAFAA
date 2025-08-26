'This file contains all the methods of sslcs.f
Imports System.Text

Partial Public Class clsSolve


    Public im, mm, icmpr(64) As Integer, prop(,) As Double       


    ''' <summary>
    ''' drive entry into continuum material model
    ''' </summary>
    ''' <param name="ihv"></param>
    Public Sub sslcs(ByRef ihv() As Integer, ByVal d1() As Double, ByVal d2() As Double, ByVal d3() As Double,
                      ByVal d4() As Double, ByVal d5() As Double, ByVal d6() As Double)


        Dim msg1 = "", msg1000 = ""

        Dim npt = 8

        'c ... ihv(i) is the index which references the first storage location in
        'c     memory for the history variables of the ith elem
        'c
        'c ... this routine only handles a common material/element upon each call
        'c      ln  = the stride in storage for a given material
        'c      nne = the relative offset in the element history variable storage buffer
        'c            nne locates the first storage location for the element number
        'c            nblk+nel-1+mftl (where mft is the same as mftl when no rigid bodys)
        'c      nn  = offset adusted for gauss point and location of first element in
        'c            block. The material models expect hist vars pointer (i.e. a(nn))
        'c            to be the reference to the first element in block (of 64 or less).
        'c            nne is the reference to memory in the middle of block so that nn
        'c            is found by shifting down from nne by (mft-1)*ln. This could
        'c            produce negative addressing but should be ok since neg addresses
        'c            wont be accessed.
        'c      nblk  = the sum of all elements in previous element blocks so far
        'c              (upper level blocking)
        'c      nel-1 = the sum of all elements in previous element blocks of 64 so far
        'c              (lower level blocking)
        'c      mftl  = the first element number in current block referencing common
        'c              material and element type. mft=mftl when no rigid bodies.
        'c              with rigid bodies, element data is compressed in compix.f
        'c              such that lft to llt only considers non rigid elements.
        'c              because of compression, real element number is mapped over
        'c              via icmpr array
        'c      nblk+nel-1+mft = the first element number in this current block of
        'c                       64 (or less)
        'c      nnet  = locates first storage location in buffer of element and does
        'c              (mft-1)*ln shift so that element data is passed appropriately
        'c              to constit model routines. nnet used mainly for temp dependent
        'c              material models since first 8 words of element constitutive
        'c              data for these models stores node numbers for nodal temps.
        'c      kk    = starting location for the mft element of history variable for
        'c              temperature dependent materials. The history variabls are
        'c              located after the first 8 node numbers for temps. The
        'c              stress per gauss point are seperated by ncon(model)-1.
        'c
        'c

        If matp(mft) = 1 Then
            'Continue
        End If
        '
        Call objComsub.timing(cpuio, cpuip, 15, 2)


        '' added for model=0 by YC 102418
        'If model = 0 Then
        '    Call objComsub.timing(cpuio, cpuip, 15, 3)
        '    Return
        'End If
        '' added for model=0 by YC 102418 END

        mm = anegb1.Length          ' QW 12-12-2018- 
        'Dim nm = nm2 + (matp(mft) - 1) * 48
        Dim ln = npt * ncon(model)          ' stride in hist var storage  
        Dim mftl = mft
        If lcmpr Then mftl = icmpr(mft) ' w/ r.b. map mft to real elm #  
        Dim nne = ihv(nblk + nel - 1 + mftl) - ihv(nblk + 1) + nh04 ' rel offset for mem in buffer
        Dim nnet = nne - (mft - 1) * ln

        'YC? 102418
        'Dim nn = nne + (ipt - 1) * ncon(model) - (mft - 1) * ln        ' offset adjust for gp & shift
        'Dim kk = nne + 8 + (ipt - 1) * (ncon(model) - 1) - (mft - 1) * ln
        Dim nn = nne - (mft - 1) * ln        ' offset adjust for gp & shift
        Dim kk = nne - (mft - 1) * ln

        'kk = Math.Max(kk, 1) + 8
        'nn = Math.Max(nn, 1)
        'nnet = Math.Max(nnet, 1)
        nn = Math.Max(nn, nne) + (ipt - 1) * ncon(model)
        kk = Math.Max(kk, nne) + 8 + (ipt - 1) * (ncon(model) - 1)
        nnet = Math.Max(nnet, nne)

        'If mft > 1 Then
        '    Dim model0 = mtype((mft - 1)) 'model # for element before mft
        '    Dim ln0 = npt * ncon(model0) 'ln for element before mft
        '    nn = nne + (ipt - 1) * ncon(model) - (mft - 1) * ln0
        '    nnet = nne - (mft - 1) * ln0
        '    kk = nne + 8 + (ipt - 1) * (ncon(model) - 1) - (mft - 1) * ln0
        'End If
        ' YC? 102418 END


        'model & lhex(8)=lhex(9) see bk10, YC 102418 END
        'Dim lnt = lhex(8) * npt
        'Dim iit = (nel - 1) * npt * lhex(8)
        'Dim nnt = nh04 + iit + (ipt - 1) * lhex(8)
        'Dim kkt = nh04 + iit + 8 + (ipt - 1) * (lhex(8) - 1)
        Dim lnt = lhex(9) * npt
        Dim iit = (nel - 1) * npt * lhex(9)
        Dim nnt = nh04 + iit + (ipt - 1) * lhex(9)
        Dim kkt = nh04 + iit + 8 + (ipt - 1) * (lhex(9) - 1)
        'YC 102418 END


        Dim nnh = nh04 + iit


        If model = 0 Then
            Call objComsub.timing(cpuio, cpuip, 15, 2)
            Return
        ElseIf model = 1 OrElse model = 56 OrElse model = 57 OrElse model = 58 Then
            Dim number_of_rows As Integer = prop.GetLength(0)
            Dim anegb1_2d(ln, mlt), prop_1d(number_of_rows - 1) As Double   ' QW 12-12-2018-
            Dim j, k As Integer
            'For j = 1 To ln
            'For k = mft To mlt
            'im = nn + (j - 1) * (mlt - mft + 1) + k - mft
            'If im < mm Then
            'anegb1_2d(j, k) = anegb1(nn + (k - mft) * ln + j - 1)
            'anegb1_2d(j, k) = anegb1(nn + (j - 1) * (mlt - mft + 1) + k - mft)
            'Else
            '   anegb1_2d(j, k) = 0.0
            'End If
            '   Next k
            '  Next j
            For k = mft To mlt
                For j = 1 To ln
                    im = nn + (k - mft) * ln + j - 1
                    If im < mm Then
                        anegb1_2d(j, k) = anegb1(nn + (k - mft) * ln + j - 1)
                    End If
                Next j
            Next k
            For j = 0 To number_of_rows - 1
                prop_1d(j) = prop(j, matp(mft))
            Next
            Call s1main(prop_1d, anegb1_2d, ln, d1, d2, d3, d4, d5, d6)
            For k = mft To mlt
                For j = 1 To ln
                    im = nn + (k - mft) * ln + j - 1
                    If im < mm Then
                        anegb1(nn + (k - mft) * ln + j - 1) = anegb1_2d(j, k)
                    End If
                Next j
            Next k




            'TODO - Check as s1main call below params are different
            'Call s1main(prop(1, matp(mft)), anegb1(nn), ln)

            ' YC 102418
            'Dim anegb1_2d(nn - 1, mlt - 1), prop_1d(4) As Double
            'For j = 0 To nn - 1
            '    anegb1_2d(j, 0) = anegb1(j)
            'Next
            'For j = 0 To 4
            '    prop_1d(j) = prop(j, matp(mft))
            'Next
            'Call s1main(prop_1d, anegb1_2d, ln)

            'Dim prop_mft_1D(48) As Double
            'Call objComsub.ArrayExtract1Dfrom2D(prop, 1 - 1, matp(mft) - 1, prop_mft_1D, 48)

            'Dim sig(ln, llt) As Double, sig_mft_1D(ln * (mlt - mft + 1)) As Double
            'Call objComsub.ArrayExtract1Dfrom1D(anegb1, nn - 1, sig_mft_1D, ln * (mlt - mft + 1))

            'Dim sig_mft(ln, (mlt - mft + 1)) As Double
            'Call objComsub.ArrayConvert1Dto2D(sig_mft_1D, sig_mft, ln, (mlt - mft + 1))

            'Call objComsub.ArrayInsert2Dto2D(sig_mft, ln, (mlt - mft + 1), sig, 1 - 1, mft - 1)


            'Call s1main(prop_mft_1D, sig, ln)


            'Call objComsub.ArrayExtract2Dfrom2D(sig, 1 - 1, mft - 1, sig_mft, ln, (mlt - mft + 1))

            'Call objComsub.ArrayConvert2Dto1D(sig_mft, ln, (mlt - mft + 1), sig_mft_1D)
            'Call objComsub.ArrayInsert1Dto1D(sig_mft_1D, ln * (mlt - mft + 1), anegb1, nn - 1)


            'Dim iele As Integer
            'For iele = mft To mlt
            '    Call objComsub.ArrayExtract1Dfrom2D(sig, 1 - 1, iele - 1, sig_iele_1D, 6)
            '    Call objComsub.ArrayInsert1Dto1D(sig_iele_1D, 6, anegb1, nn - 1 + (iele - 1) * ln)
            'Next iele


            'Dim anegb1_nn(ln * (mlt - mft + 1)), anegb1_nn_2D(ln, (mlt - mft + 1)) As Double 'size llt-lft+1 vs mlt-mft+1 YC? 102418
            'Call objComsub.ArrayExtract1Dfrom1D(anegb1, nn - 1, anegb1_nn, ln * (mlt - mft + 1))
            'objComsub.ArrayConvert1Dto2D(anegb1_nn, anegb1_nn_2D, ln, (mlt - mft + 1))

            'Call s1main(prop_mft_1D, anegb1_nn_2D, ln)

            'objComsub.ArrayConvert2Dto1D(anegb1_nn_2D, ln, llt, anegb1_nn)
            'objComsub.ArrayInsert1Dto1D(anegb1_nn, ln * llt, anegb1, nn - 1)





            ' YC 102418 END



        ElseIf model = 4 Then

            'c	------------- Restore temperature feature, Qiang, 03-26-2010 ---------

            'TODO - Missing Sub

            'YC 121219
            'Call s4main(prop(1, matp(mft)), anegb1(nnet), anegb1(kk), anegb1(kk + 6), anegb1(kk + 7), temo, temp, anegb1(kk + 8), ln)
            'Dim prop_mft_1D(48) As Double
            'Call objComsub.ArrayExtract1Dfrom2D(prop, 1 - 1, matp(mft) - 1, prop_mft_1D, 48)
            Dim prop_mft(8, 6) As Double

            ' YC 121219-1
            For j = 1 To 6
                For k = 1 To 8
                    prop_mft(k, j) = prop(k + (j - 1) * 8, matp(mft))
                Next k
            Next j
            ' YC 121219-1 END

            Dim nodes(8, mlt) As Integer 'YC? anegb1_nnet_2d(ln, mlt)

            'Dim anegb1_kk_2d(ln, mlt), anegb1_kk_6_2D(ln, mlt), anegb1_kk_7_2D(ln, mlt), anegb1_kk_8_2D(ln, mlt) As Double  ' YC 121219-3
            Dim anegb1_kk_2d(6, mlt), anegb1_kk_6_2D(1, mlt), anegb1_kk_7_2D(1, mlt), anegb1_kk_8_2D(1, mlt) As Double


            For k = mft To mlt
                For j = 1 To 8

                    'nodes(j, k) = ixh(j + 1, k) 'v3.0 001/YC 052620-1
                    nodes(j, k) = ixh(j + 1, k + nftm1)

                Next j

                'For j = 1 To ln    ' YC 121219-3
                For j = 1 To 6

                    im = nnet + (k - mft) * ln + j - 1
                    If im < mm Then
                        'anegb1_nnet_2d(j, k) = anegb1(nnet + (k - mft) * ln + j - 1)
                        anegb1_kk_2d(j, k) = anegb1(kk + (k - mft) * ln + j - 1)

                        ' YC 121219-3
                        'anegb1_kk_6_2D(j, k) = anegb1(kk + 6 + (k - mft) * ln + j - 1)
                        'anegb1_kk_7_2D(j, k) = anegb1(kk + 7 + nn + (k - mft) * ln + j - 1)
                        'anegb1_kk_8_2D(j, k) = anegb1(kk + 8 + nn + (k - mft) * ln + j - 1)
                        ' YC 121219-3 END

                    End If
                Next j

                ' YC 121219-3
                anegb1_kk_6_2D(1, k) = anegb1(kk + 6 + (k - mft) * ln)

                'anegb1_kk_7_2D(1, k) = anegb1(kk + 7 + nn + (k - mft) * ln)    ' YC 121219-4
                'anegb1_kk_8_2D(1, k) = anegb1(kk + 8 + nn + (k - mft) * ln)
                anegb1_kk_7_2D(1, k) = anegb1(kk + 7 + (k - mft) * ln)
                anegb1_kk_8_2D(1, k) = anegb1(kk + 8 + (k - mft) * ln)
                ' YC 121219-3 END

            Next k

            Call s4main(prop_mft, nodes, anegb1_kk_2d, anegb1_kk_6_2D, anegb1_kk_7_2D, temo, temp, anegb1_kk_8_2D, ln)

            For k = mft To mlt

                'For j = 1 To ln ' YC 121219-3
                For j = 1 To 6

                    im = nn + (k - mft) * ln + j - 1
                    If im < mm Then
                        'anegb1(nnet + (k - mft) * ln + j - 1) = anegb1_nnet_2d(j, k)
                        anegb1(kk + (k - mft) * ln + j - 1) = anegb1_kk_2d(j, k)

                        ' YC 121219-3
                        'anegb1(kk + 6 + (k - mft) * ln + j - 1) = anegb1_kk_6_2D(j, k)
                        'anegb1(kk + 7 + nn + (k - mft) * ln + j - 1) = anegb1_kk_7_2D(j, k)
                        'anegb1(kk + 8 + nn + (k - mft) * ln + j - 1) = anegb1_kk_8_2D(j, k)
                        ' YC 121219-3 END

                    End If
                Next j

                ' YC 121219-3
                anegb1(kk + 6 + (k - mft) * ln) = anegb1_kk_6_2D(1, k)

                'anegb1(kk + 7 + nn + (k - mft) * ln) = anegb1_kk_7_2D(1, k)    ' YC 121219-4
                'anegb1(kk + 8 + nn + (k - mft) * ln) = anegb1_kk_8_2D(1, k)
                anegb1(kk + 7 + (k - mft) * ln) = anegb1_kk_7_2D(1, k)
                anegb1(kk + 8 + (k - mft) * ln) = anegb1_kk_8_2D(1, k)
                ' YC 121219-3 END

            Next k
            'YC 121219 END
            'c	----------------------------------------------------------------------
            If bNike3dMsg Then msg1 = "ik02 call s4main in sub sslcs"

        ElseIf model = 6 Then
            '
            Call s6main(prop(1, matp(mft)), anegb1(nn), anegb1(nn + 6), ln)
            '        
            If bNike3dMsg Then msg1 = "ik02 call s6main in sub sslcs"
        Else
            Dim sb As New StringBuilder()
            sb.AppendLine(" *******************************************************")
            sb.AppendLine(" *                 - FATAL ERROR -                     *")
            sb.AppendLine(" *   Material model type {0} not implemented (sslcs)   *")
            sb.AppendLine(" *******************************************************")
            msg1000 = String.Format(sb.ToString(), model.ToString("###"))

            'Call objComsub.adios(2)  ' YC 092018
            Call objComsub.adios(2)

        End If

        Call objComsub.timing(cpuio, cpuip, 15, 3)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine sslcs(ihv,*)
'c
'
'      implicit double precision (a-h,o-z)                            
'
'c
'c===> module to drive entry into continuum material model
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk01/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n14a
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09
'
'      common/bk08/itherm,itemp,n20,n21
'      common/bk10/iphase,nelgp,imass,model,lhex(8)
'     
'      common/bk30/cpuio(72),cpuip(72)
'      common/bk56/d(6,6),ipt,nel,nelsub
'      common/range/mft,mlt,lft,llt,nftm1
'      common/block/nblk
'      common/vect1/
'     1 rx1(64),ry1(64),rz1(64),rx2(64),ry2(64),rz2(64),
'     2 rx3(64),ry3(64),rz3(64),rx4(64),ry4(64),rz4(64),
'     3 rx5(64),ry5(64),rz5(64),rx6(64),ry6(64),rz6(64),
'     4 rx7(64),ry7(64),rz7(64),rx8(64),ry8(64),rz8(64),
'     5 mtype(64),matp(64)
'      
'      logical lfpass,llpass
'      common/ndinit/lfpass,llpass
'c      logical ldyna3d,lgpavg
'
'      logical lelstf
'      logical stsion
'      common/stsi/iaddst,nnelst,lcsts,stsion
'      common/elstfb/lelstf
'      logical lcmpr
'      common/bkrb3/icmpr(64),lcmpr
'      common/ncond/ncon(60)
'      dimension ihv(*)
'      
'      common/block02/bNIKE3D, bNike3dMsg
'	logical bNIKE3D, bNike3dMsg
'      common/tmp/told,tnew,tmode(9000),tbase(9000),
'     & temo(9000),temp(9000)
'      common/readm/matype(25),den(25),prop(48,25),csprop(24,25),
'     & trefm(25),tmecc(25),rdc(4,25),aux(48)                           ! QW 11-11-2015
'                                                
'      common/aa/anegb1(7508928),anegb2(7508928)                       ! QW 11-11-2015
'c      
'c      
'      data npt/8/
'c
'c ... ihv(i) is the index which references the first storage location in
'c     memory for the history variables of the ith elem
'c
'c ... this routine only handles a common material/element upon each call
'c      ln  = the stride in storage for a given material
'c      nne = the relative offset in the element history variable storage buffer
'c            nne locates the first storage location for the element number
'c            nblk+nel-1+mftl (where mft is the same as mftl when no rigid bodys)
'c      nn  = offset adusted for gauss point and location of first element in
'c            block. The material models expect hist vars pointer (i.e. a(nn))
'c            to be the reference to the first element in block (of 64 or less).
'c            nne is the reference to memory in the middle of block so that nn
'c            is found by shifting down from nne by (mft-1)*ln. This could
'c            produce negative addressing but should be ok since neg addresses
'c            wont be accessed.
'c      nblk  = the sum of all elements in previous element blocks so far
'c              (upper level blocking)
'c      nel-1 = the sum of all elements in previous element blocks of 64 so far
'c              (lower level blocking)
'c      mftl  = the first element number in current block referencing common
'c              material and element type. mft=mftl when no rigid bodies.
'c              with rigid bodies, element data is compressed in compix.f
'c              such that lft to llt only considers non rigid elements.
'c              because of compression, real element number is mapped over
'c              via icmpr array
'c      nblk+nel-1+mft = the first element number in this current block of
'c                       64 (or less)
'c      nnet  = locates first storage location in buffer of element and does
'c              (mft-1)*ln shift so that element data is passed appropriately
'c              to constit model routines. nnet used mainly for temp dependent
'c              material models since first 8 words of element constitutive
'c              data for these models stores node numbers for nodal temps.
'c      kk    = starting location for the mft element of history variable for
'c              temperature dependent materials. The history variabls are
'c              located after the first 8 node numbers for temps. The
'c              stress per gauss point are seperated by ncon(model)-1.
'c
'c
'
'      if (matp(mft).eq.1) then
'      continue
'      end if
'
'      call timing(cpuio,cpuip,15,2)
'      nm=nm2+(matp(mft)-1)*48
'      ln=npt*ncon(model)                            ! stride in hist var storage
'      mftl= mft
'      if(lcmpr) mftl = icmpr(mft)                   ! w/ r.b. map mft to real elm #
'      nne=ihv(nblk+nel-1+mftl) - ihv(nblk+1) + nh04 ! rel offset for mem in buffer
'      nn =nne+(ipt-1)*ncon(model)-(mft-1)*ln        ! offset adjust for gp & shift
'      nnet=nne-(mft-1)*ln
'      kk  =nne+8+(ipt-1)*(ncon(model)-1)-(mft-1)*ln
'
'      lnt=lhex(8)*npt
'      iit=(nel-1)*npt*lhex(8)
'      nnt=nh04+iit+(ipt-1)*lhex(8)
'      kkt=nh04+iit+8+(ipt-1)*(lhex(8)-1)
'      nnh=nh04+iit
'
'      if (model.eq.0) then
'        call timing(cpuio,cpuip,15,3)
'        return
'c
'c NKC
'      elseif (model.eq.1 .or. model.eq.56 .or.
'     &        model.eq.57 .or. model.eq.58) then
'c
'
'        call s1main (prop(1,matp(mft)),anegb1(nn),ln)          ! QW 11-11-2015
'c     
'        if (nn.eq.546701) then
'            continue
'        end if
'c
'      elseif (model.eq.4) then
'c
'c	------------- Restore temperature feature, Qiang, 03-26-2010 ---------
'c
'        call s4main (prop(1,matp(mft)),anegb1(nnet),anegb1(kk),
'     &   anegb1(kk+6),anegb1(kk+7),temo,temp,anegb1(kk+8),ln)          ! QW 11-11-2015
'c
'c	----------------------------------------------------------------------
'c
'       if (bNike3dMsg) write(10,*) 'ik02 call s4main in sub sslcs'    
'c
'c
'      elseif (model.eq.6) then
'
'        call s6main (prop(1,matp(mft)),anegb1(nn),anegb1(nn+6),ln)     ! QW 11-11-2015
'        
'        if (bNike3dMsg) write(10,*) 'ik02 call s6main in sub sslcs'
'
'c
'      else
'        write(lutty,1000) model
'        write(luo,1000) model
'        call adios(2)
'      endif
'c
'      call timing(cpuio,cpuip,15,3)
'      return
'c
'  991 call timing(cpuio,cpuip,15,3)
'      return1
'c
'  888 format(a10,5(a2,i10))
'898   format(a10,i8,2(a2,e15.8))
' 1000 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' *   Material model type ',i3,' not implemented (sslcs)   *',/
'     &' *******************************************************')
'      end

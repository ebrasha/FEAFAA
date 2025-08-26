'This file contains all the methods of in3dhhv.f
Partial Public Class clsInitial
   
    ''' <summary>
    ''' set up element constitutive storage index list ihv
    ''' </summary>
    ''' <param name="ia"></param>
    ''' <param name="ihv"></param>
    ''' <param name="matype"></param>
    ''' <param name="numelg"></param>
    ''' <param name="nhexg"></param>
    ''' <param name="ihvct"></param>
    Public Sub in3dhhv(ByRef ixh(,) As Integer, ByRef ihv() As Integer, ByRef matype() As Integer,
                       ByRef numelg() As Integer, ByRef nhexg As Integer, ByRef ihvct As Integer)
        Dim N, mx, loc, ncon(60) As Integer
        Dim ihvc = 1                          ' initialize hex hist var counter
        nhexg = 1                       ' # of groups
        numelg(nhexg) = 1               ' partial sum of elems in nhexg grp
        ihvct = 0                       ' zero based total storage
        Dim ihvctl = 0                      ' storage to the end of prev grp
        loc = 1
        Dim i333 = incflg * 333               ' add extra stiffness storage

        Call com.ancon(ncon)
        For N = 1 To numelh
            '        call unpkid(mx,ia(loc),2)
            'Call clsInput.unpkid(mx, ia, loc - 1, 2)

            'Dim jj As Integer
            'For jj = 1 To 8
            ' ix(jj) = ia(loc + jj)
            'Next jj
            'mx = ia(loc)
            'loc = loc + 9
            mx = ixh(1, N)
            lhex(1) = matype(mx)

            '    ... set up indices to element history variable storage ihv(i)

            Dim npt = 8                           ' npt = # of gauss points
            ihv(N) = ihvc                   ' first storage loc for nth elem
            ihvc = ihvc + npt * ncon(lhex(1)) ' first storage loc for n+1th elem
            ihvct = ihvc - 1 + N * i333         ' increment total storage

            '     ... numelg(nhexg) is the first element # in the nhexg group

            If ihvct - ihvctl > nwebuf Then           ' start new group
                Dim nelpg = N - numelg(nhexg)
                If nelpg > 64 Then nelpg = 64 * (nelpg / 64) ' want blocks of multiple of 64
                Dim nel = numelg(nhexg) - 1 + nelpg           ' last elem # in the nhexg grp
                ihvctl = ihv(nel + 1) - 1 + nel * i333 ' total memory for nel elements
                nhexg = nhexg + 1               ' increment # of element groups
                numelg(nhexg) = nel + 1               ' new group begins at nel+1
            End If

        Next
        'Call Check1D(ihv, numelh)
        ihv(numelh + 1) = ihvc
        numelg(nhexg + 1) = numelh + 1
        loc = 1

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine in3dhhv(ia,ihv,matype,numelg,nhexg,ihvct)
'c
'      implicit double precision (a-h,o-z)                              
'c
'c===> module to set up element constitutive storage index list ihv
'c
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/bk55/mx,ix(8),loc
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      common/ncond/ncon(60)
'c
'      dimension ia(*),ihv(*),matype(*),numelg(*)
'c
'      ihvc=1                          ! initialize hex hist var counter
'      nhexg = 1                       ! # of groups
'      numelg(nhexg) = 1               ! partial sum of elems in nhexg grp
'      ihvct = 0                       ! zero based total storage
'      ihvctl = 0                      ! storage to the end of prev grp
'      loc = 1
'      i333 = incflg*333               ! add extra stiffness storage
'
'      do 160 n=1,numelh
'        call unpkid(mx,ia(loc),2)
'        loc=loc+9
'        lhex(1)=matype(mx)
'
'c    ... set up indices to element history variable storage ihv(i)
'
'        npt=8                           ! npt = # of gauss points
'        ihv(n) = ihvc                   ! first storage loc for nth elem
'        ihvc = ihvc + npt*ncon(lhex(1)) ! first storage loc for n+1th elem
'        ihvct = ihvc-1 + n*i333         ! increment total storage
'
'c     ... numelg(nhexg) is the first element # in the nhexg group
'
'        if(ihvct-ihvctl.gt.nwebuf) then           ! start new group
'         nelpg = n - numelg(nhexg)
'         if(nelpg.gt.64) nelpg = 64*(nelpg/64)   ! want blocks of multiple of 64
'         nel = numelg(nhexg)-1 + nelpg           ! last elem # in the nhexg grp
'         ihvctl        = ihv(nel+1)-1 + nel*i333 ! total memory for nel elements
'         nhexg         = nhexg + 1               ! increment # of element groups
'         numelg(nhexg) = nel   + 1               ! new group begins at nel+1
'        endif
'
'  160 continue
'
'      ihv(numelh+1)=ihvc
'      numelg(nhexg+1) = numelh + 1
'      loc=1
'
'      return
'      end

'This file contains all the methods of dispbc.f
Partial Public Class clsSolve

    
    Public nlcur As Integer
    Public alpha As Double


    ''' <summary>
    ''' to impose displacement bc's on hexahedral element
    ''' </summary>
    ''' <param name="icnt2"></param>
    ''' <param name="fval"></param>
    ''' <param name="r"></param>
    ''' <param name="rm"></param>
    ''' <param name="s"></param>
    ''' <param name="lm"></param>
    ''' <param name="idir"></param>
    ''' <param name="lc"></param>
    ''' <param name="xmag"></param>
    ''' <param name="idflag"></param>
    ''' <param name="rf"></param>
    ''' <param name="iprec"></param>
    Public Sub dispbc(ByRef icnt2 As Integer, ByRef fval() As Double, ByRef r(,) As Double,
                      ByRef rm(,) As Double, ByRef s(,) As Double, ByRef lm(,) As Integer,
                      ByRef idir() As Integer, ByRef lc() As Integer, ByRef xmag() As Double,
                      ByRef idflag() As Integer, ByRef rf() As Double, ByRef iprec As Integer)

        If numdc = 0 Then Return

        Dim lcv, l, i1, j1, k1, ns, ldf, lprec As Integer
        Dim dsb As Double

        Dim n As Integer

        Dim j, i, k, m As Integer   ' YC 102418

        For j = lft To llt

            'For i = 0 To numudc - 1    ' YC 102418
            For i = 1 To numudc
                If idflag(i) < 0 Then Continue For
                lcv = lc(i)
                dsb = 0.0
                If icnt2 <> 1 Then dsb = (fval(lcv) - fval(lcv + nlcur)) * xmag(i)

                'For k = 0 To 23        ' YC 102418
                For k = 1 To 24
                    lprec = j
                    'lprec = iprec * j - (iprec - 1)
                    If lm(k, lprec) = idir(i) Then
                        l = k
                        i1 = (k * l) / 2
                        j1 = i1 + k - 1
                        n = 0

                        If alpha <> 1 Then        ' store applied disp in inertia force vector

                            For m = i1 To j1
                                rm(j, n) = rm(j, n) + s(m, j) * dsb
                                s(m, j) = 0.0
                                n = n + 1
                            Next
                            rf(i) = rf(i) + r(j, n) + rm(j, n)
                            n = n + 1
                            k1 = k

                            'For m = k1 To 23          ' YC 102418
                            For m = k1 To 24
                                ns = ((m - 1) * m) / 2 + k
                                rm(j, n) = rm(j, n) + s(ns, j) * dsb
                                s(ns, j) = 0.0
                                n = n + 1
                            Next
                            ldf = (k * (k + 1)) / 2
                            s(ldf, j) = 1.0
                            rm(j, k) = -dsb
                            r(j, k) = 0

                        Else

                            For m = i1 To j1
                                r(j, n) = r(j, n) + s(m, j) * dsb
                                s(m, j) = 0.0
                                n = n + 1
                            Next
                            rf(i) = rf(i) + r(j, n)
                            n = n + 1
                            k1 = k

                            'For m = k1 To 23      ' YC 102418
                            For m = k1 To 24
                                ns = ((m - 1) * m) / 2 + k
                                r(j, n) = r(j, n) + s(ns, j) * dsb
                                s(ns, j) = 0.0
                                n = n + 1
                            Next
                            ldf = (k * (k + 1)) / 2
                            s(ldf, j) = 1.0
                            r(j, k) = -dsb

                        End If
                    End If
                Next
            Next
        Next
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine dispbc(icnt2,fval,r,rm,s,lm,idir,lc,xmag,idflag,rf,
'     &                  iprec)
'c
'      implicit double precision (a-h,o-z)                             
'
'c
'c===> module to impose displacement bc's on hexahedral element
'c
'      common/bk12alpha/alpha
'      common/bk16/nlcur,nptst,nload,nptm
'      common/bk35/numdc,numudc,nrcc
'      common/range/mft,mlt,lft,llt,nftm1
'c
'      dimension fval(*),r(64,*),s(324,*),lm(324*iprec,*),idir(*),lc(*),
'     1 xmag(*),idflag(*),rf(*),rm(64,*)
'c
'      if(numdc.eq.0) return
'c
'      do 100 j=lft,llt
'      do 90 i=1,numudc
'      if (idflag(i).lt.0) go to 90
'      lcv=lc(i)
'      dsb=0.
'      if(icnt2.eq.1) go to 10
'      dsb=(fval(lcv)-fval(lcv+nlcur))*xmag(i)
'   10 do 40 k=1,24
'      lprec = j
'c     lprec = iprec*j - (iprec - 1)
'      if(lm(k,lprec).eq.idir(i)) then
'      l=k-1
'      i1=(k*l)/2+1
'      j1=i1+k-2
'      n=1
'
'      if(alpha.ne.1) then        ! store applied disp in inertia force vector
'
'       do 20 m=i1,j1
'       rm(j,n)=rm(j,n)+s(m,j)*dsb
'       s(m,j)=0.0
'   20  n=n+1
'       rf(i)=rf(i)+r(j,n)+rm(j,n)
'       n=n+1
'       k1=k+1
'       do 30 m=k1,24
'       ns=((m-1)*m)/2+k
'       rm(j,n)=rm(j,n)+s(ns,j)*dsb
'       s(ns,j)=0.0
'   30  n=n+1
'       ldf=(k*(k+1))/2
'       s(ldf,j)=1.0
'       rm(j,k)  =-dsb
'       r(j,k) =0
'
'      else
'
'       do 25 m=i1,j1
'       r(j,n)=r(j,n)+s(m,j)*dsb
'       s(m,j)=0.0
'   25  n=n+1
'       rf(i)=rf(i)+r(j,n)
'       n=n+1
'       k1=k+1
'       do 35 m=k1,24
'       ns=((m-1)*m)/2+k
'       r(j,n)=r(j,n)+s(ns,j)*dsb
'       s(ns,j)=0.0
'   35  n=n+1
'       ldf=(k*(k+1))/2
'       s(ldf,j)=1.0
'       r(j,k)   =-dsb
'
'      endif
'
'      endif
'   40 continue
'   90 continue
'  100 continue
'      return
'      end

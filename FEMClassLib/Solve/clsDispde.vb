'This file contains all the methods of dispde.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to impose displacement bc's on discrete elements
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
    ''' <param name="rf"></param>
    Public Sub dispde(ByVal icnt2 As Integer, ByVal fval() As Double, ByVal r() As Double,
                      ByVal rm() As Double, ByVal s() As Double, ByVal lm() As Integer, ByVal idir() As Integer,
                      ByVal lc() As Integer, ByVal xmag() As Double, ByVal rf() As Double)

        Dim lcv, l, i1, j1, k1, ns, ldf As Integer
        Dim dsb As Double

        If numdc = 0 Then Return

        Dim n, i, k, m As Integer   ' YC 092018

        'For i = 0 To numudc - 1    ' YC 102418
        For i = 1 To numudc
            lcv = lc(i)
            dsb = 0.0
            If icnt2 <> 1 Then dsb = (fval(lcv) - fval(lcv + nlcur)) * xmag(i)

            'For k = 0 To 5     ' YC 102418
            For k = 1 To 6
                If lm(k) = idir(i) Then
                    l = k
                    i1 = (k * l) / 2
                    j1 = i1 + k - 1
                    n = 0

                    For m = i1 To j1
                        r(n) = r(n) + s(m) * dsb
                        s(m) = 0.0
                        n = n + 1
                    Next
                    rf(i) = rf(i) + r(n)
                    n = n + 1
                    k1 = k

                    'For m = k1 To 5  ' YC 102418
                    For m = k1 To 6
                        ns = ((m - 1) * m) / 2 + k
                        r(n) = r(n) + s(ns) * dsb
                        s(ns) = 0.0
                        n = n + 1
                    Next
                    ldf = (k * (k + 1)) / 2
                    s(ldf) = 1.0
                    r(k) = -dsb
                End If
            Next
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine dispde(icnt2,fval,r,rm,s,lm,idir,lc,xmag,rf)
'c
'
'      implicit double precision (a-h,o-z)                             
'
'c
'c===> module to impose displacement bc's on discrete elements
'c
'      common/bk16/nlcur,nptst,nload,nptm
'      common/bk35/numdc,numudc,nrcc
'c
'      dimension fval(*),r(6),s(21),lm(6),idir(*),lc(*),xmag(*),rf(*),
'     &          rm(*)
'c
'c NKC 9/14/99
'c      write(6,*) 'entering dispde.F'
'
'      if(numdc.eq.0) return
'c
'      do 90 i=1,numudc
'      lcv=lc(i)
'      dsb=0.
'      if(icnt2.eq.1) go to 10
'      dsb=(fval(lcv)-fval(lcv+nlcur))*xmag(i)
'   10 do 40 k=1,6
'      if(lm(k).eq.idir(i)) then
'      l=k-1
'      i1=(k*l)/2+1
'      j1=i1+k-2
'      n=1
'
'       do 20 m=i1,j1
'       r(n)=r(n)+s(m)*dsb
'       s(m)=0.0
'   20  n=n+1
'       rf(i)=rf(i)+r(n)
'       n=n+1
'       k1=k+1
'       do 30 m=k1,6
'       ns=((m-1)*m)/2+k
'       r(n)=r(n)+s(ns)*dsb
'       s(ns)=0.0
'   30  n=n+1
'       ldf=(k*(k+1))/2
'       s(ldf)=1.0
'       r(k)  =-dsb
'
'      endif
'   40 continue
'   90 continue
'      return
'      end

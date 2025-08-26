'This file contains all the methods of slave1.f
Partial Public Class clsSolve


    ' YC 092018
    Public lresrt As Integer
    Private i, k, m, n, llc As Integer
    Private resltl, result As Double
    ' YC 092018 END


    ''' <summary>
    ''' to perform nearest-neighbor search for interfaces
    ''' </summary>
    ''' <param name="x"></param>
    ''' <param name="irect"></param>
    ''' <param name="lmsr"></param>
    ''' <param name="msr"></param>
    ''' <param name="nsv"></param>
    ''' <param name="iloc"></param>
    ''' <param name="irtl"></param>
    ''' <param name="nseg"></param>
    ''' <param name="nsn"></param>
    ''' <param name="nmn"></param>
    Public Sub slave1(ByRef x(,) As Double, ByRef irect(,) As Integer, ByRef lmsr() As Integer,
                      ByRef msr() As Integer, ByRef nsv() As Integer, ByRef iloc() As Integer,
                      ByRef irtl() As Integer, ByRef nseg() As Integer, ByRef nsn As Integer, ByRef nmn As Integer)

        Dim knew, j, num, npt, ll, l, j1, j2, k1, k2 As Integer
        Dim cms, dms, ems, fms As Double



        'For ii = 0 To nsn - 1      ' YC 102418
        For ii = 1 To nsn

            'c.... check for change in master nodes

5:          knew = 0
            i = nsv(ii)
            j = iloc(ii)
            k = msr(j)

            'cms = Math.Pow((x(0, i) - x(0, k)), 2) + Math.Pow((x(1, i) - x(1, k)), 2) + Math.Pow((x(2, i) - x(2, k)), 2)    ' YC 102418
            cms = Math.Pow((x(1, i) - x(1, k)), 2) + Math.Pow((x(2, i) - x(2, k)), 2) + Math.Pow((x(3, i) - x(3, k)), 2)

            num = nseg(j + 1) - nseg(j)
            npt = nseg(j) - 1

            'For jj = 0 To num - 1    ' YC 102418
            For jj = 1 To num
                ll = lmsr(npt + jj)

                ' YC 102418
                '                If k <> irect(0, ll) Then GoTo 10
                '                l = irect(1, ll)
                '                m = irect(2, ll)
                '                n = irect(3, ll)
                '                GoTo 40
                '10:             If k <> irect(1, ll) Then GoTo 20
                '                l = irect(0, ll)
                '                m = irect(2, ll)
                '                n = irect(3, ll)
                '                GoTo 40
                '20:             If k <> irect(2, ll) Then GoTo 30
                '                l = irect(0, ll)
                '                m = irect(1, ll)
                '                n = irect(3, ll)
                '                GoTo 40
                '30:             If k <> irect(3, ll) Then GoTo 200
                '                l = irect(0, ll)
                '                m = irect(1, ll)
                '                n = irect(2, ll)
                '40:             dms = Math.Pow((x(0, i) - x(0, l)), 2) + Math.Pow((x(1, i) - x(1, l)), 2) + Math.Pow((x(2, i) - x(2, l)), 2)
                '                ems = Math.Pow((x(0, i) - x(0, m)), 2) + Math.Pow((x(1, i) - x(1, m)), 2) + Math.Pow((x(2, i) - x(2, m)), 2)
                '                fms = Math.Pow((x(0, i) - x(0, n)), 2) + Math.Pow((x(1, i) - x(1, n)), 2) + Math.Pow((x(2, i) - x(2, n)), 2)
                If k <> irect(1, ll) Then GoTo 10
                l = irect(2, ll)
                m = irect(3, ll)
                n = irect(4, ll)
                GoTo 40
10:             If k <> irect(2, ll) Then GoTo 20
                l = irect(1, ll)
                m = irect(3, ll)
                n = irect(4, ll)
                GoTo 40
20:             If k <> irect(3, ll) Then GoTo 30
                l = irect(1, ll)
                m = irect(2, ll)
                n = irect(4, ll)
                GoTo 40
30:             If k <> irect(4, ll) Then GoTo 200
                l = irect(1, ll)
                m = irect(2, ll)
                n = irect(3, ll)
40:             dms = Math.Pow((x(1, i) - x(1, l)), 2) + Math.Pow((x(2, i) - x(2, l)), 2) + Math.Pow((x(3, i) - x(3, l)), 2)
                ems = Math.Pow((x(1, i) - x(1, m)), 2) + Math.Pow((x(2, i) - x(2, m)), 2) + Math.Pow((x(3, i) - x(3, m)), 2)
                fms = Math.Pow((x(1, i) - x(1, n)), 2) + Math.Pow((x(2, i) - x(2, n)), 2) + Math.Pow((x(3, i) - x(3, n)), 2)
                ' YC 102418 END

                If dms >= cms Then GoTo 45
                cms = dms
                knew = l
45:             If ems >= cms Then GoTo 50
                cms = ems
                knew = m
50:             If fms >= cms Then GoTo 60
                cms = fms
                knew = n
60:         Next
            If knew <> 0 Then k = knew
            If k = msr(j) Then GoTo 80

            'For jj = 0 To nmn - 1   ' YC 102418
            For jj = 1 To nmn
                If msr(jj) <> k Then GoTo 70
                iloc(ii) = jj
                GoTo 5
70:         Next

            'c.... check for possible update to irtl

80:         j = iloc(ii)
            l = irtl(ii)
            'TODO - check resltl value
            resltl = -1.0E+37       ' QW 12-12-2018-
            num = nseg(j + 1) - nseg(j)
            npt = nseg(j) - 1
            If l = 0 Then GoTo 105
            llc = l

            'For jj = 0 To 3      ' YC 102418
            For jj = 1 To 4
                If irect(jj, l) <> k Then GoTo 90
                j1 = jj + 1
                j2 = jj - 1
                If jj <> 3 Then GoTo 100

                'If irect(2, l) = irect(3, l) Then j1 = 1      ' YC 102418
                If irect(3, l) = irect(4, l) Then j1 = 1

                GoTo 100
90:         Next

            'c   ... new nmn not in old seg ...
            GoTo 105

100:        If j1 = 5 Then j1 = 1
            If j2 = 0 Then j2 = 4
            m = irect(j1, l)
            n = irect(j2, l)
            cross(x, llc, lresrt, resltl, result, i, k, m, n)           ' QW 5-12-2019
            If result > 0.0 Then GoTo 190
105:
            'For jj = 0 To num - 1   ' YC 102418
            For jj = 1 To num
                ll = lmsr(jj + npt)
                llc = ll
                If l = ll Then GoTo 140

                'For kk = 0 To 3    ' YC 102418
                For kk = 1 To 4
                    If irect(kk, ll) <> k Then GoTo 110 ' QW 12-12-2018-
                    k1 = kk + 1
                    k2 = kk - 1
                    If kk <> 3 Then GoTo 120
                    If irect(3, ll) = irect(4, ll) Then k1 = 1 ' QW 12-12-2018-
                    GoTo 120
110:            Next
120:            If k1 = 5 Then k1 = 1
                If k2 = 0 Then k2 = 4
                m = irect(k1, ll)
                n = irect(k2, ll)
                cross(x, llc, lresrt, resltl, result, i, k, m, n)       ' QW 5-12-2019
                If result <= 0 Then         ' QW 12-12-2018-
                    GoTo 140
                Else
                    GoTo 130
                End If
130:            irtl(ii) = ll
                GoTo 190
140:        Next
            irtl(ii) = lresrt
190:    Next
        Return
200:
        Dim msg210 = " fatal error in subroutine slave1"

        If bNIKE3D Then
            '        write(*,210)
        End If

        '      write(luo,210)
        'Call objNik3d.adios(2) ' YC 092018
        ' Call objComsub.adios(2)

    End Sub


End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine slave1(x,irect,lmsr,msr,nsv,iloc,irtl,nseg,nsn,nmn)
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c
'c===> module to perform nearest-neighbor search for interfaces
'c
'	common/block02/bNIKE3D !ikawa 04/14/03
'	logical bNIKE3D
'
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk42/llc,lresrt,resltl ,result,i,k,m,n
'      dimension x(3,*),irect(4,*),lmsr(*),msr(*),nsv(*),
'     1          iloc(*),irtl(*),nseg(*)
'c
'      do 190 ii=1,nsn
'c
'c.... check for change in master nodes
'c
'   5  knew=0
'      i=nsv(ii)
'      j=iloc(ii)
'      k=msr(j)
'      cms=(x(1,i)-x(1,k))**2+(x(2,i)-x(2,k))**2+(x(3,i)-x(3,k))**2
'      num=nseg(j+1)-nseg(j)
'      npt=nseg(j)-1
'      do 60 jj=1,num
'      ll=lmsr(npt+jj)
'      if(k.ne.irect(1,ll)) go to 10
'      l=irect(2,ll)
'      m=irect(3,ll)
'      n=irect(4,ll)
'      go to 40
' 10   if(k.ne.irect(2,ll)) go to 20
'      l=irect(1,ll)
'      m=irect(3,ll)
'      n=irect(4,ll)
'      go to 40
' 20   if(k.ne.irect(3,ll)) go to 30
'      l=irect(1,ll)
'      m=irect(2,ll)
'      n=irect(4,ll)
'      go to 40
' 30   if(k.ne.irect(4,ll)) go to 200
'      l=irect(1,ll)
'      m=irect(2,ll)
'      n=irect(3,ll)
' 40   dms=(x(1,i)-x(1,l))**2+(x(2,i)-x(2,l))**2+(x(3,i)-x(3,l))**2
'      ems=(x(1,i)-x(1,m))**2+(x(2,i)-x(2,m))**2+(x(3,i)-x(3,m))**2
'      fms=(x(1,i)-x(1,n))**2+(x(2,i)-x(2,n))**2+(x(3,i)-x(3,n))**2
'      if(dms.ge.cms) go to 45
'      cms=dms
'      knew=l
' 45   if(ems.ge.cms) go to 50
'      cms=ems
'      knew=m
' 50   if(fms.ge.cms) go to 60
'      cms=fms
'      knew=n
' 60   continue
'      if(knew.ne.0) k=knew
'      if(k.eq.msr(j)) go to 80
'      do 70 jj=1,nmn
'      if(msr(jj).ne.k) go to 70
'      iloc(ii)=jj
'      go to 5
' 70   continue
'c
'c.... check for possible update to irtl
'c
' 80   j=iloc(ii)
'      l=irtl(ii)
'      resltl =-10.d+36
'      num=nseg(j+1)-nseg(j)
'      npt=nseg(j)-1
'      if(l.eq.0) go to 105
'      llc=l
'      do 90 jj=1,4
'      if(irect(jj,l).ne.k) go to 90
'      j1=jj+1
'      j2=jj-1
'      if(jj.ne.3) go to 100
'      if(irect(3,l).eq.irect(4,l)) j1=1
'      go to 100
' 90   continue
'c
'c   ... new nmn not in old seg ...
'      goto 105
'c
' 100  if(j1.eq.5) j1=1
'      if(j2.eq.0) j2=4
'      m=irect(j1,l)
'      n=irect(j2,l)
'      call cross(x)
'      if(result.gt.0.0) go to 190
' 105  do 140 jj=1,num
'      ll=lmsr(jj+npt)
'      llc=ll
'      if(l.eq.ll) go to 140
'      do 110 kk=1,4
'      if(irect(kk,ll).ne.k) go to 110
'      k1=kk+1
'      k2=kk-1
'      if(kk.ne.3) go to 120
'      if(irect(3,ll).eq.irect(4,ll)) k1=1
'      go to 120
' 110  continue
' 120  if(k1.eq.5) k1=1
'      if(k2.eq.0) k2=4
'      m=irect(k1,ll)
'      n=irect(k2,ll)
'      call cross(x)
'      if(result) 140,140,130
' 130  irtl(ii)=ll
'      go to 190
' 140  continue
'      irtl(ii)=lresrt
' 190  continue
'      return
' 200  write(lutty,210)
'
'	if (bNIKE3D) then
'        write(*,210)
'	end if
'
'      write(luo,210)
'      call adios(2)
' 210  format(' fatal error in subroutine slave1')
'      end

'This file contains all the methods of slavi0.f
Partial Public Class clsInput

    ''' <summary>
    ''' initialize slide surface logic
    ''' </summary>
    ''' <param name="x"></param>
    ''' <param name="irect"></param>
    ''' <param name="nseg"></param>
    ''' <param name="lc"></param>
    ''' <param name="nsv"></param>
    ''' <param name="msr"></param>
    ''' <param name="iloc"></param>
    ''' <param name="nmn"></param>
    ''' <param name="nsn"></param>
    ''' <param name="nrt"></param>
    Public Sub slavi0(ByRef x(,) As Double, ByRef irect(,) As Integer, ByRef nseg() As Integer, ByRef lc() As Integer, ByRef nsv() As Integer,
                      ByRef msr() As Integer, ByRef iloc() As Integer, ByRef nmn As Integer, ByRef nsn As Integer, ByRef nrt As Integer, ByVal n As Integer)

        If nrt = 0 Then Return
        For ii = 1 To nsn
            Dim iInd = nsv(ii)
            Dim cms = 1.0E+37                                                         'vax
            For jj = 1 To nmn
                Dim jInd = msr(jj)
                Dim dms = Math.Pow((x(1, iInd) - x(1, jInd)), 2) + Math.Pow((x(2, iInd) - x(2, jInd)), 2) +
                    Math.Pow((x(3, iInd) - x(3, jInd)), 2)
                If dms > cms Then Continue For
                cms = dms
                iloc(ii) = jj
            Next
        Next

        '
        '     initialize lc (connection) array
        '
        Dim il = 0
        For iInd = 1 To nrt
            For jInd = 1 To 4
                If irect(jInd, iInd) = 0 Then Continue For
                For kInd = 1 To nsn
                    il = kInd
                    If nsv(kInd) = irect(jInd, iInd) Then Exit For
                Next
                Dim ii = 0
                Dim locn = nseg(il)
                Dim nsgt = nseg(il + 1) - locn
                For lInd = 1 To nsgt
                    If lc(locn + lInd - 1) <> 0 Then ii = ii + 1
                Next
                lc(locn + ii) = iInd
            Next
        Next

    End Sub
End Class


'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine slavi0(x,irect,nseg,lc,nsv,msr,iloc,nmn,nsn,nrt)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to initialize slide surface logic
'c
'      dimension x(3,*),irect(4,*),lc(*),msr(*),nsv(*),iloc(*),
'     1          nseg(1)
'      common/block02/bNIKE3D, bNike3dMsg
'	logical bNIKE3D, bNike3dMsg
'c
'c     initialize slide and void logic
'c
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'slavi0'
'      ik01 = ik01 + 1
'      end if
'
'      if(nrt.eq.0) return
'      do 20 ii=1,nsn
'      i=nsv(ii)
'      cms=1.e37                                                         vax
'      do 10 jj=1,nmn
'      j=msr(jj)
'      dms=(x(1,i)-x(1,j))**2+(x(2,i)-x(2,j))**2+(x(3,i)-x(3,j))**2
'      if(dms.gt.cms) go to 10
'      cms=dms
'      iloc(ii)= jj
' 10   continue
' 20   continue
'c
'c     initialize lc (connection) array
'c
'      do 70 i=1,nrt
'      do 60 j=1,4
'      if(irect(j,i).eq.0) go to 60
'      do 30 k=1,nsn
'      il=k
' 30   if(nsv(k).eq.irect(j,i)) go to 40
' 40   ii=0
'      locn=nseg(il)
'      nsgt=nseg(il+1)-locn
'      do 50 l=1,nsgt
' 50   if(lc(locn+l-1).ne.0) ii=ii+1
'      lc(locn+ii)=i
'
' 60   continue
' 70   continue
'c
'      
'      return
'      end

'This file contains all the methods of in3dh.f
Partial Public Class clsInitial

    ''' <summary>
    ''' drive consitutive history initialization for hexahedron elements
    ''' </summary>
    ''' <param name="idp"></param>
    ''' <param name="x"></param>
    ''' <param name="matype"></param>
    ''' <param name="den"></param>
    ''' <param name="prop"></param>
    ''' <param name="ia"></param>
    ''' <param name="kht"></param>
    ''' <param name="ihv"></param>
    ''' <param name="numnp"></param>
    ''' <param name="nmmat"></param>
    Public Sub in3dh(ByRef idp(,) As Integer, ByRef x(,) As Double, ByRef matype() As Integer,
                     ByRef den() As Double, ByRef prop(,) As Double, ByRef ixh(,) As Integer,
                     ByRef kht() As Integer, ByRef ihv() As Integer, ByRef numnp As Integer,
                     ByRef nmmat As Integer)
        Dim mx As Integer
        Dim lm(48), ln(48), ix(8) As Integer
        Dim numel = lhex(2)

        Dim ibs = 0
        For n = 1 To numel
            '      call unpkid(mx,ia(loc),2)
            'Call clsInput.unpkid(mx, ia, loc - 1, 2)
            Dim jj As Integer
            mx = ixh(1, n)
            For jj = 1 To 8
                ix(jj) = ixh(jj + 1, n)
            Next jj

            lhex(1) = matype(mx)
            Dim j = 0
            For i = 1 To 8
                Dim i2 = 3 * i
                If ibs = 0 Then               ' node is not a bsi node
                    '       call unpkid(lm(j+1),idp(1,ix(i)),1)
                    ' Call com.unpkid(ln, idp, 0, ix(i) - 1, 1)
                    For k = 1 To 6
                        lm(j + k) = idp(k, ix(i))
                    Next
                    j = j + 3
                End If
            Next

            Call colht(kht, j, lm)

            'Call strint(n, nblk, ihv, ncon(lhex(1)))           ' Temporarily stop running due to long time consuming. QW 11-28-2018
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'      subroutine in3dh(idp,x,matype,den,prop,ia,kht,ihv,
'     &                 numnp,nmmat)
'c
'      implicit double precision (a-h,o-z)                        
'c
'c===> module to drive consitutive history initialization
'c     for hexahedron elements
'c
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk55/mx,ix(8),loc  
'      common/ncond/ncon(60)
'
'      dimension idp(6,*),x(3,*),kht(*),xyz(24),ihv(*),    
'     1          lm(48),den(*),prop(48,*),ia(*),matype(*)
'      
'      common/block/nblk
'      equivalence (lhex(2),numel)
'c
'      ibs=0
'
'      do 160 n=1,numel
'      call unpkid(mx,ia(loc),2)
'      loc=loc+9                                                      
'
'      lhex(1)=matype(mx)
'c
'      j=0
'      do 110 i=1,8
'      i2=3*i
'      if(ibs.eq.0) then               ! node is not a bsi node
'       call unpkid(lm(j+1),idp(1,ix(i)),1)
'       j=j+3
'      endif
'      xyz(i2-2)=x(1,ix(i))
'      xyz(i2-1)=x(2,ix(i))
'  110 xyz(i2)  =x(3,ix(i))
'
'      call colht(kht,j,lm)
'
'      call strint(n,nblk,ihv,ncon(lhex(1)))
'  160 continue
'      return
'c
'  200 continue
'      end

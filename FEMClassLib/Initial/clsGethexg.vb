'This file contains all the methods of gethexg.f
Partial Public Class clsInitial

    ''' <summary>
    ''' return group storage length and number of elements in block
    ''' and set up indices for stiffness storage for incomp modes
    ''' </summary>
    ''' <param name="n"></param>
    ''' <param name="numelg"></param>
    ''' <param name="ihv"></param>
    ''' <param name="nelpg"></param>
    ''' <param name="nblk"></param>
    ''' <param name="length"></param>
    Public Sub gethexg(ByRef n As Integer, ByRef numelg() As Integer, ByRef ihv() As Integer,
                       ByRef nelpg As Integer, ByRef nblk As Integer, ByRef length As Integer, ByVal incflg As Integer, ByVal nh04 As Integer, ByRef nh05 As Integer)

        Dim nelg = numelg(n)       ' first elem # in group n
        Dim nelgp1 = numelg(n + 1)     ' first elem # in group n+1
        Dim ihvg = ihv(nelg)       ' index for storage for elem nelg
        Dim ihvgp1 = ihv(nelgp1)     ' index for storage for elem nelgp1

        nelpg = nelgp1 - nelg                     ' # of elems in group n
        nblk = nelg - 1                       ' elem # in last block
        'length = ihvgp1 - ihvg + nelpg * incflg * 333 ' storage for group n
        length = ihvgp1 - ihvg              ' QW 12-12-2018-
        If incflg = 1 Then             ' QW
            nh05 = nh04 + ihvgp1 - ihvg
            'nh06 = nh05 + 297 * nelpg
            'nh07 = nh06 + 9 * nelpg
            'nh08 = nh07 + 9 * nelpg
            'nh09 = nh08 + 9 * nelpg
        End If

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine gethexg (n,numelg,ihv,nelpg,nblk,length)
'
'c ===> routine to return group storage length and number of elements in block
'c      and set up indices for stiffness storage for incomp modes
'
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09
'      common/incomp/incflg,ibkflg,stepls,stepsv
'
'      dimension numelg(*),ihv(*)
'
'      nelg   = numelg(n)       ! first elem # in group n
'      nelgp1 = numelg(n+1)     ! first elem # in group n+1
'      ihvg   = ihv(nelg)       ! index for storage for elem nelg
'      ihvgp1 = ihv(nelgp1)     ! index for storage for elem nelgp1
'
'      nelpg = nelgp1 - nelg                     ! # of elems in group n
'      nblk     = nelg - 1                       ! elem # in last block
'      length = ihvgp1 - ihvg + nelpg*incflg*333 ! storage for group n
'
'      if(incflg.eq.1) then
'       nh05 = nh04 + ihvgp1 - ihvg
'       nh06 = nh05 + 297*nelpg
'       nh07 = nh06 +   9*nelpg
'       nh08 = nh07 +   9*nelpg
'       nh09 = nh08 +   9*nelpg
'      endif
'
'      return
'      end

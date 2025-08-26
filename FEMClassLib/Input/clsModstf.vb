'This file contains all the methods of modstf.f

Partial Public Class clsInput

    ''' <summary>
    ''' balance stiffness parameters for slide surface facets
    ''' </summary>
    ''' <param name="stf"></param>
    ''' <param name="n"></param>
    ''' <param name="stfo"></param>
    ''' <param name="avgs"></param>
    ''' <param name="iside"></param>
    ''' <param name="nm"></param>
    Public Sub modstf(ByRef stf() As Double, ByRef n As Integer, ByRef stfo As Double,
                      ByRef avgs As Double, ByRef iside As Integer, ByRef nm As Integer)

        If n = 0 Then Return
        Dim scal = avgs / stfo

        For I = 1 To n
            stf(I) = scal * stf(I)
        Next
        Dim msg = ""
        If iside = 1 Then
            msg = "slave stiffness of sliding interface" + nm.ToString("0000") +
                " is scaled by" + scal.ToString("e5")
        ElseIf iside = 2 Then
            msg = "master stiffness of sliding interface" + nm.ToString("0000") +
                " is scaled by" + scal.ToString("e5")
        End If

    End Sub
End Class

'  ref org fortran code

'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine modstf(stf,n,stfo,avgs,iside,nm)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to balance stiffness parameters for slide surface facets
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      dimension stf(*)
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'modstf'
'      ik01 = ik01 + 1
'      end if
'
'      if (n.eq.0) return
'      scal=avgs/stfo
'      do 10 i=1,n
'   10 stf(i)=scal*stf(i)
'      if(iside.eq.1) write(luo,20) nm,scal
'      if(iside.eq.2) write(luo,30) nm,scal
'      return
'   20 format('slave stiffness of sliding interface',i4,' is scaled by',
'     1 e14.5)
'   30 format('master stiffness of sliding interface',i4,' is scaled by',
'     1 e14.5)
'      end

'This file contains all the methods of chgint.f
Partial Public Class clsInput

    ''' <summary>
    ''' compute Newmark integration coefficients
    ''' </summary>
    Public Sub chgint(ByRef a0 As Double, ByRef a1 As Double, ByRef a2 As Double, ByRef a3 As Double, ByRef a4 As Double, ByRef a5 As Double, ByRef a6 As Double, ByRef a7 As Double, ByRef a8 As Double, ByRef a9 As Double, ByRef a10 As Double, ByVal delt As Double, ByVal alfa As Double, ByVal dt As Double)
        a0 = 1 / (alfa * dt * dt)
        a1 = (delt / alfa) / dt
        a2 = 1 / (alfa * dt)
        a3 = 0.5 / alfa - 1
        a4 = (delt / alfa) - 1
        a5 = dt * (0.5 * (delt / alfa) - 1)
        a6 = a0
        a7 = -a2
        a8 = -a3
        a9 = dt * (1 - delt)
        a10 = delt * dt
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine chgint
'c

'implicit Double precision (a-h, o - z)                                    dp

'c
'c ==>= module to compute Newmark integration coefficients
'c
'common/ bk12 / dtx0, dt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
'      common/ bk24 / delt, alfa, icnt2, ipri, icnt1

'!ik099 1111111111
'      Integer ik01
'      If (ik01.eq.0) Then
'write(88,*) 'chgint'
'ik01 = ik01 + 1
'End If

'a0 = 1./ (alfa * dt * dt)
'a1 = (delt / alfa) / dt
'a2 = 1./ (alfa * dt)
'a3 = 0.5 / alfa - 1.
'      a4 = (delt / alfa) - 1.
'      a5 = dt * (0.5 * (delt / alfa) - 1.)
'a6 = a0
'a7 = -a2
'a8 = -a3
'a9 = dt * (1.- delt)
'a10 = delt * dt
'Return
'End


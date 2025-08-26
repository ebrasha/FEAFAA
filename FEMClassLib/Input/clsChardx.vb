'This file contains all the methods of chardx.f
Partial Public Class clsInput
    ''' <summary>
    ''' compute characteristic length for master facet
    ''' </summary>
    ''' <param name="dx"></param>
    Public Sub chardx(ByRef dx As Double, ByVal x() As Double, ByVal y() As Double, ByVal z() As Double)
        Dim fs1 = -x(1) + x(2) + x(3) - x(4)
        Dim fs2 = -y(1) + y(2) + y(3) - y(4)
        Dim fs3 = -z(1) + z(2) + z(3) - z(4)
        Dim ft1 = -x(1) - x(2) + x(3) + x(4)
        Dim ft2 = -y(1) - y(2) + y(3) + y(4)
        Dim ft3 = -z(1) - z(2) + z(3) + z(4)
        Dim e = fs1 * fs1 + fs2 * fs2 + fs3 * fs3
        Dim f = fs1 * ft1 + fs2 * ft2 + fs3 * ft3
        Dim g = ft1 * ft1 + ft2 * ft2 + ft3 * ft3
        Dim area = Math.Sqrt((e * g - f * f) / 16)
        dx = 0.01 * Math.Sqrt(area)
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine chardx(dx)
'c

'implicit Double precision (a-h, o - z)                                    dp

'c
'c ==>= module to compute characteristic length for master facet
'c
'common/ bk44 / ux(20), uy(20), uz(20), x(20), y(20), z(20)

'!ik099 1111111111
'      Integer ik01
'      If (ik01.eq.0) Then
'write(88,*) 'chardx'
'ik01 = ik01 + 1
'End If

'fs1 = -x(1) + x(2) + x(3) - x(4)
'fs2 = -y(1) + y(2) + y(3) - y(4)
'fs3 = -z(1) + z(2) + z(3) - z(4)
'ft1 = -x(1) - x(2) + x(3) + x(4)
'ft2 = -y(1) - y(2) + y(3) + y(4)
'ft3 = -z(1) - z(2) + z(3) + z(4)
'e = fs1 * fs1 + fs2 * fs2 + fs3 * fs3
'f = fs1 * ft1 + fs2 * ft2 + fs3 * ft3
'g = ft1 * ft1 + ft2 * ft2 + ft3 * ft3
'area = sqrt((e * g - f * f) / 16.)
'dx = 0.01 * sqrt(area)
'Return
'End


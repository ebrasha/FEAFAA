'This file contains all the methods of volint.f
Partial Public Class clsInput

    ''' <summary>
    ''' compute volume of element bounded by a slidesurface facet
    ''' </summary>
    ''' <param name="volo"></param>
    ''' <param name="nel"></param>
    ''' <param name="mx"></param
    ''' 

    Public Sub volint(ByRef volo As Double, ByRef nel As Integer, ByRef mx As Integer, ByRef xc() As Double, ByRef yc() As Double, ByRef zc() As Double)
        '     jacobian matrix
        '
        Dim aj(9) As Double
        Dim x1, x2, x3, x4, x5, x6, x7, x8, y1, y2, y3, y4, y5, y6, y7, y8, z1, z2, z3, z4, z5, z6, z7, z8 As Double

        x1 = xc(1) : x2 = xc(2) : x3 = xc(3) : x4 = xc(4) : x5 = xc(5) : x6 = xc(6) : x7 = xc(7) : x8 = xc(8)
        y1 = yc(1) : y2 = yc(2) : y3 = yc(3) : y4 = yc(4) : y5 = yc(5) : y6 = yc(6) : y7 = yc(7) : y8 = yc(8)
        z1 = zc(1) : z2 = zc(2) : z3 = zc(3) : z4 = zc(4) : z5 = zc(5) : z6 = zc(6) : z7 = zc(7) : z8 = zc(8)
        aj(1) = -x1 - x2 + x3 + x4 - x5 - x6 + x7 + x8
        aj(4) = -x1 - x2 - x3 - x4 + x5 + x6 + x7 + x8
        aj(7) = -x1 + x2 + x3 - x4 - x5 + x6 + x7 - x8
        aj(2) = -y1 - y2 + y3 + y4 - y5 - y6 + y7 + y8
        aj(5) = -y1 - y2 - y3 - y4 + y5 + y6 + y7 + y8
        aj(8) = -y1 + y2 + y3 - y4 - y5 + y6 + y7 - y8
        aj(3) = -z1 - z2 + z3 + z4 - z5 - z6 + z7 + z8
        aj(6) = -z1 - z2 - z3 - z4 + z5 + z6 + z7 + z8
        aj(9) = -z1 + z2 + z3 - z4 - z5 + z6 + z7 - z8

        volo = aj(1) * aj(5) * aj(9) + aj(2) * aj(6) * aj(7) + aj(3) * aj(4) * aj(8) _
                - aj(3) * aj(5) * aj(7) - aj(2) * aj(4) * aj(9) - aj(1) * aj(6) * aj(8)
        volo = 0.015625 * volo

        If volo > 0 Then Return

        Dim format As String = " error-element {0} (material {1}) has zero volume"
        Dim msg = String.Format(format, nel.ToString("######"), mx.ToString("###"))

        'Call adios(2)          ' QW 8-2018
    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine volint(volo, nel, mx)
'c

'implicit Double precision (a-h, o - z)                                    dp

'c
'c ==>= module to compute volume of element bounded by a
'c     slidesurface facet
'c
'common/ lunits / lutty, lui, luo, lud, lur, lut, lug, luf, lust,
'     & lus1, lus2, lus5, lus6, lus11, lus12, luebe
'      common/ bk43 / det, h(20), p1(20), p2(20), p3(20), aj(9), eps(9)
'      common/ dk10 / px(20), py(20), pz(20), vx(20), vy(20), vz(20)
'      common/ bk44 / ux(20), uy(20), uz(20),
'     1 xn1(8),x1,x2,x3,x4,x5,x6,x7,x8,xx1(4),
'     2 yn2(8),y1,y2,y3,y4,y5,y6,y7,y8,xx2(4),
'     3 zn3(8),z1,z2,z3,z4,z5,z6,z7,z8,xx3(4)
'c
'c     jacobian matrix
'c

'!ik099 1111111111
'      Integer ik01
'      If (ik01.eq.0) Then
'write(88,*) 'volint'
'ik01 = ik01 + 1
'End If

'aj(1) = -x1 - x2 + x3 + x4 - x5 - x6 + x7 + x8
'aj(4) = -x1 - x2 - x3 - x4 + x5 + x6 + x7 + x8
'aj(7) = -x1 + x2 + x3 - x4 - x5 + x6 + x7 - x8
'aj(2) = -y1 - y2 + y3 + y4 - y5 - y6 + y7 + y8
'aj(5) = -y1 - y2 - y3 - y4 + y5 + y6 + y7 + y8
'aj(8) = -y1 + y2 + y3 - y4 - y5 + y6 + y7 - y8
'aj(3) = -z1 - z2 + z3 + z4 - z5 - z6 + z7 + z8
'aj(6) = -z1 - z2 - z3 - z4 + z5 + z6 + z7 + z8
'aj(9) = -z1 + z2 + z3 - z4 - z5 + z6 + z7 - z8
'c
'volo = aj(1) * aj(5) * aj(9) + aj(2) * aj(6) * aj(7) + aj(3) * aj(4) * aj(8)
'1    -aj(3)*aj(5)*aj(7)-aj(2)*aj(4)*aj(9)-aj(1)*aj(6)*aj(8)
'      volo = 0.015625 * volo
'c
'If (volo.gt.0.) return
'c
'write(luo, 10) nel,mx
'      write(lutty, 10) nel,mx
'      Call adios(2)
'10 format(' error-element',i6,' (material',i3,') has zero volume')
'      End


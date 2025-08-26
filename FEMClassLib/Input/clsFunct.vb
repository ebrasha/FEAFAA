'This file contains all the methods of funct.f
Partial Public Class clsInput
    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="s"></param>
    ''' <param name="t"></param>
    ''' <returns></returns>
    Public Function funct(ByRef s As Double, ByRef t As Double, ByRef xxx As Double, ByRef yyy As Double, ByRef zzz As Double, ByRef ps1 As Double, ByRef ps2 As Double, ByRef ps3 As Double, ByRef pt1 As Double, ByRef pt2 As Double, ByRef pt3 As Double, ByRef sp As Double, ByRef sm As Double, ByRef tp As Double, ByRef tm As Double, ByRef xs As Double, ByRef ys As Double, ByRef zs As Double, ByRef hh() As Double, ByRef xx1() As Double, ByRef xx2() As Double, ByRef xx3() As Double,
                          ByRef xx112 As Double, ByRef xx134 As Double, ByRef xx212 As Double, ByRef xx234 As Double, ByRef xx312 As Double, ByRef xx334 As Double, ByRef xx112p As Double, ByRef xx134p As Double, ByRef xx212p As Double, ByRef xx234p As Double, ByRef xx312p As Double, ByRef xx334p As Double, ByRef xx114 As Double, ByRef xx123 As Double, ByRef xx214 As Double, ByRef xx223 As Double, ByRef xx314 As Double, xx323 As Double, ByRef x1423 As Double, ByRef y1423 As Double, ByRef z1423 As Double, ByRef x1423p As Double, ByRef y1423p As Double, ByRef z1423p As Double) As Double

        tp = 0.25 * (1.0 + t)
        tm = 0.25 * (1.0 - t)
        ps1 = tp * xx134 - tm * xx112
        ps2 = tp * xx234 - tm * xx212
        ps3 = tp * xx334 - tm * xx312
        s = (ps1 * (xs - tm * xx112p - tp * xx134p) + ps2 * (ys - tm * xx212p - tp * xx234p) +
            ps3 * (zs - tm * xx312p - tp * xx334p)) / (ps1 * ps1 + ps2 * ps2 + ps3 * ps3)
        sp = 1.0 + s
        sm = 1.0 - s
        hh(1) = tm * sm
        hh(2) = tm * sp
        hh(3) = tp * sp
        hh(4) = tp * sm
        xxx = hh(1) * xx1(1) + hh(2) * xx1(2) + hh(3) * xx1(3) + hh(4) * xx1(4)
        yyy = hh(1) * xx2(1) + hh(2) * xx2(2) + hh(3) * xx2(3) + hh(4) * xx2(4)
        zzz = hh(1) * xx3(1) + hh(2) * xx3(2) + hh(3) * xx3(3) + hh(4) * xx3(4)
        pt1 = x1423 + s * x1423p
        pt2 = y1423 + s * y1423p
        pt3 = z1423 + s * z1423p
        funct = pt1 * (xs - xxx) + pt2 * (ys - yyy) + pt3 * (zs - zzz)
    End Function
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'Function funct(s, t)
'    c

'    implicit Double precision (a-h, o - z)                                    dp

'c
'    c ==>= module to
'c
'    Common/ bk43 / det, h(20),
'     1           p11, p12, p13, p14, p1(16), p21, p22, p23, p24, p2(16), p3(38)
'      Common/ bk44 / ux(20), uy(20), uz(20), xx1(20), xx2(20), xx3(20)
'      Common/ bk45 / xs, ys, zs, sig(3), epx, mx, ix(10), iy(10)
'      Common/ bk46 / xxx, yyy, zzz, ps1, ps2, ps3, pt1, pt2, pt3, sp, sm, tp, tm
'      Common/ bk47 / xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p,
'     1      xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214,
'     2      xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p

'!ik099 1111111111
'      Integer ik01
'      If (ik01.eq.0) Then
'        Write(88,*) 'funct'
'        ik01 = ik01 + 1
'    End If

'    tp = 0.25 * (1.0 + t)
'    tm = 0.25 * (1.0 - t)
'    ps1 = tp * xx134 - tm * xx112
'    ps2 = tp * xx234 - tm * xx212
'    ps3 = tp * xx334 - tm * xx312
'    s = (ps1 * (xs - tm * xx112p - tp * xx134p) +
'     1   ps2*(ys-tm*xx212p-tp*xx234p)+
'     2   ps3*(zs-tm*xx312p-tp*xx334p))/(ps1*ps1+ps2*ps2+ps3*ps3)
'      sp = 1.0 + s
'    sm = 1.0 - s
'    h(1) = tm * sm
'    h(2) = tm * sp
'    h(3) = tp * sp
'    h(4) = tp * sm
'    xxx = h(1) * xx1(1) + h(2) * xx1(2) + h(3) * xx1(3) + h(4) * xx1(4)
'    yyy = h(1) * xx2(1) + h(2) * xx2(2) + h(3) * xx2(3) + h(4) * xx2(4)
'    zzz = h(1) * xx3(1) + h(2) * xx3(2) + h(3) * xx3(3) + h(4) * xx3(4)
'    pt1 = x1423 + s * x1423p
'    pt2 = y1423 + s * y1423p
'    pt3 = z1423 + s * z1423p
'    funct = pt1 * (xs - xxx) + pt2 * (ys - yyy) + pt3 * (zs - zzz)
'    Return
'    c
'    End


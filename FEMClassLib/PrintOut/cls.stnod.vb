Partial Public Class clsPrintOut  
    Sub stnod(ByRef nnd As Integer, ByRef ie As Integer, ByRef nne As Integer, ByRef ixp(,) As Integer, ByRef ast(,) As Double, ByRef ss(,) As Double, ByRef mn() As Integer)

        'Dim c As Double = 0.8660254037845  'v3.0 004/YC 092820
        Dim c As Double = Math.Sqrt(3) / 2


        Dim j, k, l As Integer
        Dim tt(6, 8) As Double


        Dim aa, bb, cc, dd As Double    'Formula by G. P. Nikishkov  v3.0 004/YC 092820
        'aa = (5 + Math.Sqrt(3)) / 4 : bb = -(Math.Sqrt(3) + 1) / 4 : cc = (Math.Sqrt(3) - 1) / 4 : dd = (5 - Math.Sqrt(3)) / 4   'Parameter by G. P. Nikishkov, AA and DD corrected by v3.0 004/YC 092820-1
        aa = (5 + 3 * Math.Sqrt(3)) / 4 : bb = -(Math.Sqrt(3) + 1) / 4 : cc = (Math.Sqrt(3) - 1) / 4 : dd = (5 - 3 * Math.Sqrt(3)) / 4
        Dim m11 = aa, m12 = bb, m13 = cc, m14 = bb, m15 = bb, m16 = cc, m17 = dd, m18 = cc,
         m21 = m12, m22 = aa, m23 = bb, m24 = cc, m25 = cc, m26 = bb, m27 = cc, m28 = dd,
         m31 = m13, m32 = m23, m33 = aa, m34 = bb, m35 = dd, m36 = cc, m37 = bb, m38 = cc,
         m41 = m14, m42 = m24, m43 = m34, m44 = aa, m45 = cc, m46 = dd, m47 = cc, m48 = bb,
         m51 = m15, m52 = m25, m53 = m35, m54 = m45, m55 = aa, m56 = bb, m57 = cc, m58 = bb,
         m61 = m16, m62 = m26, m63 = m36, m64 = m46, m65 = m56, m66 = aa, m67 = bb, m68 = cc,
         m71 = m17, m72 = m27, m73 = m37, m74 = m47, m75 = m57, m76 = m67, m77 = aa, m78 = bb,
         m81 = m18, m82 = m28, m83 = m38, m84 = m48, m85 = m58, m86 = m68, m87 = m78, m88 = aa


        For j = 1 To 6
            k = j + 1


            ' v3.0 004/YC 092820
            'tt(j, 1) = (ast(1, k) - ast(7, k)) * c + 0.5 * (ast(1, k) + ast(7, k))
            'tt(j, 2) = (ast(2, k) - ast(8, k)) * c + 0.5 * (ast(2, k) + ast(8, k))
            'tt(j, 3) = (ast(3, k) - ast(5, k)) * c + 0.5 * (ast(3, k) + ast(5, k))
            'tt(j, 4) = (ast(4, k) - ast(6, k)) * c + 0.5 * (ast(4, k) + ast(6, k))
            'tt(j, 5) = (ast(5, k) - ast(3, k)) * c + 0.5 * (ast(5, k) + ast(3, k))
            'tt(j, 6) = (ast(6, k) - ast(4, k)) * c + 0.5 * (ast(6, k) + ast(4, k))
            'tt(j, 7) = (ast(7, k) - ast(1, k)) * c + 0.5 * (ast(7, k) + ast(1, k))
            'tt(j, 8) = (ast(8, k) - ast(2, k)) * c + 0.5 * (ast(8, k) + ast(2, k))
            tt(j, 1) = m11 * ast(1, k) + m12 * ast(2, k) + m13 * ast(3, k) + m14 * ast(4, k) + m15 * ast(5, k) + m16 * ast(6, k) + m17 * ast(7, k) + m18 * ast(8, k)
            tt(j, 2) = m21 * ast(1, k) + m22 * ast(2, k) + m23 * ast(3, k) + m24 * ast(4, k) + m25 * ast(5, k) + m26 * ast(6, k) + m27 * ast(7, k) + m28 * ast(8, k)
            tt(j, 3) = m31 * ast(1, k) + m32 * ast(2, k) + m33 * ast(3, k) + m34 * ast(4, k) + m35 * ast(5, k) + m36 * ast(6, k) + m37 * ast(7, k) + m38 * ast(8, k)
            tt(j, 4) = m41 * ast(1, k) + m42 * ast(2, k) + m43 * ast(3, k) + m44 * ast(4, k) + m45 * ast(5, k) + m46 * ast(6, k) + m47 * ast(7, k) + m48 * ast(8, k)
            tt(j, 5) = m51 * ast(1, k) + m52 * ast(2, k) + m53 * ast(3, k) + m54 * ast(4, k) + m55 * ast(5, k) + m56 * ast(6, k) + m57 * ast(7, k) + m58 * ast(8, k)
            tt(j, 6) = m61 * ast(1, k) + m62 * ast(2, k) + m63 * ast(3, k) + m64 * ast(4, k) + m65 * ast(5, k) + m66 * ast(6, k) + m67 * ast(7, k) + m68 * ast(8, k)
            tt(j, 7) = m71 * ast(1, k) + m72 * ast(2, k) + m73 * ast(3, k) + m74 * ast(4, k) + m75 * ast(5, k) + m76 * ast(6, k) + m77 * ast(7, k) + m78 * ast(8, k)
            tt(j, 8) = m81 * ast(1, k) + m82 * ast(2, k) + m83 * ast(3, k) + m84 * ast(4, k) + m85 * ast(5, k) + m86 * ast(6, k) + m87 * ast(7, k) + m88 * ast(8, k)
            ' YC v3.0 004/092820 END


        Next j

        For k = 1 To 8
            For l = 1 To 6
                ss(l, ixp(k + 1, ie)) = ss(l, ixp(k + 1, ie)) + tt(l, k)
            Next l

            ss(7, ixp(k + 1, ie)) = ss(7, ixp(k + 1, ie)) + 1
            mn(ixp(k + 1, ie)) = ixp(1, ie)
        Next k

    End Sub
End Class




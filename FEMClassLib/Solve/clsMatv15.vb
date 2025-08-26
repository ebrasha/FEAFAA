'This file contains all the methods of matv15.f
Partial Public Class clsSolve

    ''' <summary>
    ''' for matrix-vector product for element with 15 dof
    ''' </summary>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    ''' <param name="s"></param>
    ''' <param name="lm"></param>
    ''' <param name="ap"></param>
    ''' <param name="p"></param>
    ''' 
    Public Sub matv15(lft As Integer, llt As Integer, s(,) As Double,
                      lm(,) As Integer, ap() As Double, p() As Double, kstore As Integer) ' QW 08-14-2019

        Dim scalar As Boolean, ii As Integer

        scalar = False
        If llt < 0 Then
            llt = -llt
            scalar = True
        End If

        'c.... gather operations                                                

        Dim j As Integer    ' YC 102418

        Dim i = 0
        p(i) = 0.0
        For i = lft To llt      ' QW 12-12-2018-
            ii = kstore + i - 1
            ' For i = lft - 1 To llt - 1
            ' YC 102418
            'p01(i) = p(lm(0, i))
            'p02(i) = p(lm(1, i))
            'p03(i) = p(lm(2, i))
            'p04(i) = p(lm(3, i))
            'p05(i) = p(lm(4, i))
            'p06(i) = p(lm(5, i))
            'p07(i) = p(lm(6, i))
            'p08(i) = p(lm(7, i))
            'p09(i) = p(lm(8, i))
            'p10(i) = p(lm(9, i))
            'p11_31(i) = p(lm(10, i))
            'p12_31(i) = p(lm(11, i))
            'p13_31(i) = p(lm(12, i))
            'p14_31(i) = p(lm(13, i))
            'p15_31(i) = p(lm(14, i))
            p01(i) = p(lm(1, ii))
            p02(i) = p(lm(2, ii))
            p03(i) = p(lm(3, ii))
            p04(i) = p(lm(4, ii))
            p05(i) = p(lm(5, ii))
            p06(i) = p(lm(6, ii))
            p07(i) = p(lm(7, ii))
            p08(i) = p(lm(8, ii))
            p09(i) = p(lm(9, ii))
            p10(i) = p(lm(10, ii))
            p11_31(i) = p(lm(11, ii))
            p12_31(i) = p(lm(12, ii))
            p13_31(i) = p(lm(13, ii))
            p14_31(i) = p(lm(14, ii))
            p15_31(i) = p(lm(15, ii))

            'Dim ptemp(15 - 1) As Double
            'For j = 1 - 1 To 15 - 1
            '    If lm(j, ii) > 0 Then ptemp(j) = p(lm(j, ii) - 1) 'lm(,)=0 YC? 
            'Next j

            'p01(i) = ptemp(0)
            'p02(i) = ptemp(1)
            'p03(i) = ptemp(2)
            'p04(i) = ptemp(3)
            'p05(i) = ptemp(4)
            'p06(i) = ptemp(5)
            'p07(i) = ptemp(6)
            'p08(i) = ptemp(7)
            'p09(i) = ptemp(8)
            'p10(i) = ptemp(9)
            'p11_31(i) = ptemp(10)
            'p12_31(i) = ptemp(11)
            'p13_31(i) = ptemp(12)
            'p14_31(i) = ptemp(13)
            'p15_31(i) = ptemp(14)
            ' YC 102418 END

        Next
        'c                                                                      
        'c.... do-loops broken to allow optimization                            
        'c   

        ' YC 102418
        'For i = lft - 1 To llt - 1
        '    ap01(i) = s(0, i) * p01(i) + s(1, i) * p02(i) + s(3, i) * p03(i) +
        '      s(6, i) * p04(i) + s(10, i) * p05(i) + s(15, i) * p06(i) +
        '      s(21, i) * p07(i) + s(28, i) * p08(i) + s(36, i) * p09(i) +
        '      s(45, i) * p10(i) + s(55, i) * p11_31(i) + s(66, i) * p12_31(i) +
        '      s(78, i) * p13_31(i) + s(91, i) * p14_31(i) + s(105, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap02(i) = s(1, i) * p01(i) + s(2, i) * p02(i) + s(4, i) * p03(i) +
        '      s(7, i) * p04(i) + s(11, i) * p05(i) + s(16, i) * p06(i) +
        '      s(22, i) * p07(i) + s(29, i) * p08(i) + s(37, i) * p09(i) +
        '      s(46, i) * p10(i) + s(56, i) * p11_31(i) + s(67, i) * p12_31(i) +
        '      s(79, i) * p13_31(i) + s(92, i) * p14_31(i) + s(106, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap03(i) = s(3, i) * p01(i) + s(4, i) * p02(i) + s(5, i) * p03(i) +
        '      s(8, i) * p04(i) + s(12, i) * p05(i) + s(17, i) * p06(i) +
        '      s(23, i) * p07(i) + s(30, i) * p08(i) + s(38, i) * p09(i) +
        '      s(47, i) * p10(i) + s(57, i) * p11_31(i) + s(68, i) * p12_31(i) +
        '      s(80, i) * p13_31(i) + s(93, i) * p14_31(i) + s(107, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap04(i) = s(6, i) * p01(i) + s(7, i) * p02(i) + s(8, i) * p03(i) +
        '      s(9, i) * p04(i) + s(13, i) * p05(i) + s(18, i) * p06(i) +
        '      s(24, i) * p07(i) + s(31, i) * p08(i) + s(39, i) * p09(i) +
        '      s(48, i) * p10(i) + s(58, i) * p11_31(i) + s(69, i) * p12_31(i) +
        '      s(81, i) * p13_31(i) + s(94, i) * p14_31(i) + s(108, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap05(i) = s(10, i) * p01(i) + s(11, i) * p02(i) + s(12, i) * p03(i) +
        '      s(13, i) * p04(i) + s(14, i) * p05(i) + s(19, i) * p06(i) +
        '      s(25, i) * p07(i) + s(32, i) * p08(i) + s(40, i) * p09(i) +
        '      s(49, i) * p10(i) + s(59, i) * p11_31(i) + s(70, i) * p12_31(i) +
        '      s(82, i) * p13_31(i) + s(95, i) * p14_31(i) + s(109, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap06(i) = s(15, i) * p01(i) + s(16, i) * p02(i) + s(17, i) * p03(i) +
        '      s(18, i) * p04(i) + s(19, i) * p05(i) + s(20, i) * p06(i) +
        '      s(26, i) * p07(i) + s(33, i) * p08(i) + s(41, i) * p09(i) +
        '      s(50, i) * p10(i) + s(60, i) * p11_31(i) + s(71, i) * p12_31(i) +
        '      s(83, i) * p13_31(i) + s(96, i) * p14_31(i) + s(110, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap07(i) = s(21, i) * p01(i) + s(22, i) * p02(i) + s(23, i) * p03(i) +
        '      s(24, i) * p04(i) + s(25, i) * p05(i) + s(26, i) * p06(i) +
        '      s(27, i) * p07(i) + s(34, i) * p08(i) + s(42, i) * p09(i) +
        '      s(51, i) * p10(i) + s(61, i) * p11_31(i) + s(72, i) * p12_31(i) +
        '      s(84, i) * p13_31(i) + s(97, i) * p14_31(i) + s(111, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap08(i) = s(28, i) * p01(i) + s(29, i) * p02(i) + s(30, i) * p03(i) +
        '      s(31, i) * p04(i) + s(32, i) * p05(i) + s(33, i) * p06(i) +
        '      s(34, i) * p07(i) + s(35, i) * p08(i) + s(43, i) * p09(i) +
        '      s(52, i) * p10(i) + s(62, i) * p11_31(i) + s(73, i) * p12_31(i) +
        '      s(85, i) * p13_31(i) + s(98, i) * p14_31(i) + s(112, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap09(i) = s(36, i) * p01(i) + s(37, i) * p02(i) + s(38, i) * p03(i) +
        '      s(39, i) * p04(i) + s(40, i) * p05(i) + s(41, i) * p06(i) +
        '      s(42, i) * p07(i) + s(43, i) * p08(i) + s(44, i) * p09(i) +
        '      s(53, i) * p10(i) + s(63, i) * p11_31(i) + s(74, i) * p12_31(i) +
        '      s(86, i) * p13_31(i) + s(99, i) * p14_31(i) + s(113, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap10(i) = s(45, i) * p01(i) + s(46, i) * p02(i) + s(47, i) * p03(i) +
        '      s(48, i) * p04(i) + s(49, i) * p05(i) + s(50, i) * p06(i) +
        '      s(51, i) * p07(i) + s(52, i) * p08(i) + s(53, i) * p09(i) +
        '      s(54, i) * p10(i) + s(64, i) * p11_31(i) + s(75, i) * p12_31(i) +
        '      s(87, i) * p13_31(i) + s(100, i) * p14_31(i) + s(114, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap11(i) = s(55, i) * p01(i) + s(56, i) * p02(i) + s(57, i) * p03(i) +
        '      s(58, i) * p04(i) + s(59, i) * p05(i) + s(60, i) * p06(i) +
        '      s(61, i) * p07(i) + s(62, i) * p08(i) + s(63, i) * p09(i) +
        '      s(64, i) * p10(i) + s(65, i) * p11_31(i) + s(76, i) * p12_31(i) +
        '      s(88, i) * p13_31(i) + s(101, i) * p14_31(i) + s(115, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap12(i) = s(66, i) * p01(i) + s(67, i) * p02(i) + s(68, i) * p03(i) +
        '      s(69, i) * p04(i) + s(70, i) * p05(i) + s(71, i) * p06(i) +
        '      s(72, i) * p07(i) + s(73, i) * p08(i) + s(74, i) * p09(i) +
        '      s(75, i) * p10(i) + s(76, i) * p11_31(i) + s(77, i) * p12_31(i) +
        '      s(89, i) * p13_31(i) + s(102, i) * p14_31(i) + s(116, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap13(i) = s(78, i) * p01(i) + s(79, i) * p02(i) + s(80, i) * p03(i) +
        '      s(81, i) * p04(i) + s(82, i) * p05(i) + s(83, i) * p06(i) +
        '      s(84, i) * p07(i) + s(85, i) * p08(i) + s(86, i) * p09(i) +
        '      s(87, i) * p10(i) + s(88, i) * p11_31(i) + s(89, i) * p12_31(i) +
        '      s(90, i) * p13_31(i) + s(103, i) * p14_31(i) + s(117, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap14(i) = s(91, i) * p01(i) + s(92, i) * p02(i) + s(93, i) * p03(i) +
        '      s(94, i) * p04(i) + s(95, i) * p05(i) + s(96, i) * p06(i) +
        '      s(97, i) * p07(i) + s(98, i) * p08(i) + s(99, i) * p09(i) +
        '      s(100, i) * p10(i) + s(101, i) * p11_31(i) + s(102, i) * p12_31(i) +
        '      s(103, i) * p13_31(i) + s(104, i) * p14_31(i) + s(118, i) * p15_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap15(i) = s(105, i) * p01(i) + s(106, i) * p02(i) + s(107, i) * p03(i) +
        '      s(108, i) * p04(i) + s(109, i) * p05(i) + s(110, i) * p06(i) +
        '      s(111, i) * p07(i) + s(112, i) * p08(i) + s(113, i) * p09(i) +
        '      s(114, i) * p10(i) + s(115, i) * p11_31(i) + s(116, i) * p12_31(i) +
        '      s(117, i) * p13_31(i) + s(118, i) * p14_31(i) + s(119, i) * p15_31(i)
        'Next
        For i = lft To llt
            ii = kstore + i - 1
            ap01(i) = s(1, ii) * p01(i) + s(2, ii) * p02(i) + s(4, ii) * p03(i) +
                s(7, ii) * p04(i) + s(11, ii) * p05(i) + s(16, ii) * p06(i) +
                s(22, ii) * p07(i) + s(29, ii) * p08(i) + s(37, ii) * p09(i) +
                s(46, ii) * p10(i) + s(56, ii) * p11_31(i) + s(67, ii) * p12_31(i) +
                s(79, ii) * p13_31(i) + s(92, ii) * p14_31(i) + s(106, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap02(i) = s(2, ii) * p01(i) + s(3, ii) * p02(i) + s(5, ii) * p03(i) +
                s(8, ii) * p04(i) + s(12, ii) * p05(i) + s(17, ii) * p06(i) +
                s(23, ii) * p07(i) + s(30, ii) * p08(i) + s(38, ii) * p09(i) +
                s(47, ii) * p10(i) + s(57, ii) * p11_31(i) + s(68, ii) * p12_31(i) +
                s(80, ii) * p13_31(i) + s(93, ii) * p14_31(i) + s(107, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap03(i) = s(4, ii) * p01(i) + s(5, ii) * p02(i) + s(6, ii) * p03(i) +
                s(9, ii) * p04(i) + s(13, ii) * p05(i) + s(18, ii) * p06(i) +
                s(24, ii) * p07(i) + s(31, ii) * p08(i) + s(39, ii) * p09(i) +
                s(48, ii) * p10(i) + s(58, ii) * p11_31(i) + s(69, ii) * p12_31(i) +
                s(81, ii) * p13_31(i) + s(94, ii) * p14_31(i) + s(108, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap04(i) = s(7, ii) * p01(i) + s(8, ii) * p02(i) + s(9, ii) * p03(i) +
                s(10, ii) * p04(i) + s(14, ii) * p05(i) + s(19, ii) * p06(i) +
                s(25, ii) * p07(i) + s(32, ii) * p08(i) + s(40, ii) * p09(i) +
                s(49, ii) * p10(i) + s(59, ii) * p11_31(i) + s(70, ii) * p12_31(i) +
                s(82, ii) * p13_31(i) + s(95, ii) * p14_31(i) + s(109, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap05(i) = s(11, ii) * p01(i) + s(12, ii) * p02(i) + s(13, ii) * p03(i) +
                s(14, ii) * p04(i) + s(15, ii) * p05(i) + s(20, ii) * p06(i) +
                s(26, ii) * p07(i) + s(33, ii) * p08(i) + s(41, ii) * p09(i) +
                s(50, ii) * p10(i) + s(60, ii) * p11_31(i) + s(71, ii) * p12_31(i) +
                s(83, ii) * p13_31(i) + s(96, ii) * p14_31(i) + s(110, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap06(i) = s(16, ii) * p01(i) + s(17, ii) * p02(i) + s(18, ii) * p03(i) +
                s(19, ii) * p04(i) + s(20, ii) * p05(i) + s(21, ii) * p06(i) +
                s(27, ii) * p07(i) + s(34, ii) * p08(i) + s(42, ii) * p09(i) +
                s(51, ii) * p10(i) + s(61, ii) * p11_31(i) + s(72, ii) * p12_31(i) +
                s(84, ii) * p13_31(i) + s(97, ii) * p14_31(i) + s(111, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap07(i) = s(22, ii) * p01(i) + s(23, ii) * p02(i) + s(24, ii) * p03(i) +
                s(25, ii) * p04(i) + s(26, ii) * p05(i) + s(27, ii) * p06(i) +
                s(28, ii) * p07(i) + s(35, ii) * p08(i) + s(43, ii) * p09(i) +
                s(52, ii) * p10(i) + s(62, ii) * p11_31(i) + s(73, ii) * p12_31(i) +
                s(85, ii) * p13_31(i) + s(98, ii) * p14_31(i) + s(112, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap08(i) = s(29, ii) * p01(i) + s(30, ii) * p02(i) + s(31, ii) * p03(i) +
                s(32, ii) * p04(i) + s(33, ii) * p05(i) + s(34, ii) * p06(i) +
                s(35, ii) * p07(i) + s(36, ii) * p08(i) + s(44, ii) * p09(i) +
                s(53, ii) * p10(i) + s(63, ii) * p11_31(i) + s(74, ii) * p12_31(i) +
                s(86, ii) * p13_31(i) + s(99, ii) * p14_31(i) + s(113, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap09(i) = s(37, ii) * p01(i) + s(38, ii) * p02(i) + s(39, ii) * p03(i) +
                s(40, ii) * p04(i) + s(41, ii) * p05(i) + s(42, ii) * p06(i) +
                s(43, ii) * p07(i) + s(44, ii) * p08(i) + s(45, ii) * p09(i) +
                s(54, ii) * p10(i) + s(64, ii) * p11_31(i) + s(75, ii) * p12_31(i) +
                s(87, ii) * p13_31(i) + s(100, ii) * p14_31(i) + s(114, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap10(i) = s(46, ii) * p01(i) + s(47, ii) * p02(i) + s(48, ii) * p03(i) +
                s(49, ii) * p04(i) + s(50, ii) * p05(i) + s(51, ii) * p06(i) +
                s(52, ii) * p07(i) + s(53, ii) * p08(i) + s(54, ii) * p09(i) +
                s(55, ii) * p10(i) + s(65, ii) * p11_31(i) + s(76, ii) * p12_31(i) +
                s(88, ii) * p13_31(i) + s(101, ii) * p14_31(i) + s(115, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap11(i) = s(56, ii) * p01(i) + s(57, ii) * p02(i) + s(58, ii) * p03(i) +
                s(59, ii) * p04(i) + s(60, ii) * p05(i) + s(61, ii) * p06(i) +
                s(62, ii) * p07(i) + s(63, ii) * p08(i) + s(64, ii) * p09(i) +
                s(65, ii) * p10(i) + s(66, ii) * p11_31(i) + s(77, ii) * p12_31(i) +
                s(89, ii) * p13_31(i) + s(102, ii) * p14_31(i) + s(116, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap12(i) = s(67, ii) * p01(i) + s(68, ii) * p02(i) + s(69, ii) * p03(i) +
                s(70, ii) * p04(i) + s(71, ii) * p05(i) + s(72, ii) * p06(i) +
                s(73, ii) * p07(i) + s(74, ii) * p08(i) + s(75, ii) * p09(i) +
                s(76, ii) * p10(i) + s(77, ii) * p11_31(i) + s(78, ii) * p12_31(i) +
                s(90, ii) * p13_31(i) + s(103, ii) * p14_31(i) + s(117, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap13(i) = s(79, ii) * p01(i) + s(80, ii) * p02(i) + s(81, ii) * p03(i) +
                s(82, ii) * p04(i) + s(83, ii) * p05(i) + s(84, ii) * p06(i) +
                s(85, ii) * p07(i) + s(86, ii) * p08(i) + s(87, ii) * p09(i) +
                s(88, ii) * p10(i) + s(89, ii) * p11_31(i) + s(90, ii) * p12_31(i) +
                s(91, ii) * p13_31(i) + s(104, ii) * p14_31(i) + s(118, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap14(i) = s(92, ii) * p01(i) + s(93, ii) * p02(i) + s(94, ii) * p03(i) +
                s(95, ii) * p04(i) + s(96, ii) * p05(i) + s(97, ii) * p06(i) +
                s(98, ii) * p07(i) + s(99, ii) * p08(i) + s(100, ii) * p09(i) +
                s(101, ii) * p10(i) + s(102, ii) * p11_31(i) + s(103, ii) * p12_31(i) +
                s(104, ii) * p13_31(i) + s(105, ii) * p14_31(i) + s(119, ii) * p15_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap15(i) = s(106, ii) * p01(i) + s(107, ii) * p02(i) + s(108, ii) * p03(i) +
                s(109, ii) * p04(i) + s(110, ii) * p05(i) + s(111, ii) * p06(i) +
                s(112, ii) * p07(i) + s(113, ii) * p08(i) + s(114, ii) * p09(i) +
                s(115, ii) * p10(i) + s(116, ii) * p11_31(i) + s(117, ii) * p12_31(i) +
                s(118, ii) * p13_31(i) + s(119, ii) * p14_31(i) + s(120, ii) * p15_31(i)
        Next
        ' YC 102418 END


        If scalar Then

            ' YC 102418
            'For i = lft - 1 To llt - 1
            'ap(lm(0, i)) = ap(lm(0, i)) + ap01(i)
            'ap(lm(1, i)) = ap(lm(1, i)) + ap02(i)
            'ap(lm(2, i)) = ap(lm(2, i)) + ap03(i)
            'ap(lm(3, i)) = ap(lm(3, i)) + ap04(i)
            'ap(lm(4, i)) = ap(lm(4, i)) + ap05(i)
            'ap(lm(5, i)) = ap(lm(5, i)) + ap06(i)
            'ap(lm(6, i)) = ap(lm(6, i)) + ap07(i)
            'ap(lm(7, i)) = ap(lm(7, i)) + ap08(i)
            'ap(lm(8, i)) = ap(lm(8, i)) + ap09(i)
            'ap(lm(9, i)) = ap(lm(9, i)) + ap10(i)
            'ap(lm(10, i)) = ap(lm(10, i)) + ap11(i)
            'ap(lm(11, i)) = ap(lm(11, i)) + ap12(i)
            'ap(lm(12, i)) = ap(lm(12, i)) + ap13(i)
            'ap(lm(13, i)) = ap(lm(13, i)) + ap14(i)
            'ap(lm(14, i)) = ap(lm(14, i)) + ap15(i)
            For i = lft To llt
                ii = kstore + i - 1
                ap(lm(1, ii)) = ap(lm(1, ii)) + ap01(i)
                ap(lm(2, ii)) = ap(lm(2, ii)) + ap02(i)
                ap(lm(3, ii)) = ap(lm(3, ii)) + ap03(i)
                ap(lm(4, ii)) = ap(lm(4, ii)) + ap04(i)
                ap(lm(5, ii)) = ap(lm(5, ii)) + ap05(i)
                ap(lm(6, ii)) = ap(lm(6, ii)) + ap06(i)
                ap(lm(7, ii)) = ap(lm(7, ii)) + ap07(i)
                ap(lm(8, ii)) = ap(lm(8, ii)) + ap08(i)
                ap(lm(9, ii)) = ap(lm(9, ii)) + ap09(i)
                ap(lm(10, ii)) = ap(lm(10, ii)) + ap10(i)
                ap(lm(11, ii)) = ap(lm(11, ii)) + ap11(i)
                ap(lm(12, ii)) = ap(lm(12, ii)) + ap12(i)
                ap(lm(13, ii)) = ap(lm(13, ii)) + ap13(i)
                ap(lm(14, ii)) = ap(lm(14, ii)) + ap14(i)
                ap(lm(15, ii)) = ap(lm(15, ii)) + ap15(i)
                ' YC 102418 END

            Next
        Else

            'dir$ ivdep                                                            

            ' YC 102418
            'For i = lft - 1 To llt - 1
            'ap(lm(0, i)) = ap(lm(0, i)) + ap01(i)
            'ap(lm(1, i)) = ap(lm(1, i)) + ap02(i)
            'ap(lm(2, i)) = ap(lm(2, i)) + ap03(i)
            'ap(lm(3, i)) = ap(lm(3, i)) + ap04(i)
            'ap(lm(4, i)) = ap(lm(4, i)) + ap05(i)
            'ap(lm(5, i)) = ap(lm(5, i)) + ap06(i)
            'ap(lm(6, i)) = ap(lm(6, i)) + ap07(i)
            'ap(lm(7, i)) = ap(lm(7, i)) + ap08(i)
            'ap(lm(8, i)) = ap(lm(8, i)) + ap09(i)
            'ap(lm(9, i)) = ap(lm(9, i)) + ap10(i)
            'ap(lm(10, i)) = ap(lm(10, i)) + ap11(i)
            'ap(lm(11, i)) = ap(lm(11, i)) + ap12(i)
            'ap(lm(12, i)) = ap(lm(12, i)) + ap13(i)
            'ap(lm(13, i)) = ap(lm(13, i)) + ap14(i)
            'ap(lm(14, i)) = ap(lm(14, i)) + ap15(i)
            For i = lft To llt
                ii = kstore + i - 1
                ap(lm(1, ii)) = ap(lm(1, ii)) + ap01(i)
                ap(lm(2, ii)) = ap(lm(2, ii)) + ap02(i)
                ap(lm(3, ii)) = ap(lm(3, ii)) + ap03(i)
                ap(lm(4, ii)) = ap(lm(4, ii)) + ap04(i)
                ap(lm(5, ii)) = ap(lm(5, ii)) + ap05(i)
                ap(lm(6, ii)) = ap(lm(6, ii)) + ap06(i)
                ap(lm(7, ii)) = ap(lm(7, ii)) + ap07(i)
                ap(lm(8, ii)) = ap(lm(8, ii)) + ap08(i)
                ap(lm(9, ii)) = ap(lm(9, ii)) + ap09(i)
                ap(lm(10, ii)) = ap(lm(10, ii)) + ap10(i)
                ap(lm(11, ii)) = ap(lm(11, ii)) + ap11(i)
                ap(lm(12, ii)) = ap(lm(12, ii)) + ap12(i)
                ap(lm(13, ii)) = ap(lm(13, ii)) + ap13(i)
                ap(lm(14, ii)) = ap(lm(14, ii)) + ap14(i)
                ap(lm(15, ii)) = ap(lm(15, ii)) + ap15(i)
                ' YC 102418 END

            Next
        End If

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine matv15 (lft,llt,s,lm,ap,p)                            
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c                                                                      
'c===> module for matrix-vector product for element with 15 dof         
'c                                                                      
'      common/double/iprec
'      logical scalar                                                   
'      common /bk31/                                                    
'     1             p01(128),p02(128),p03(128),p04(128),                
'     2             p05(128),p06(128),p07(128),p08(128),                
'     3             p09(128),p10(128),p11(128),p12(128),                
'     4             p13(128),p14(128),p15(128),p16(128),                
'     5             p17(128),p18(128),p19(128),p20(128),                
'     6             p21(128),p22(128),p23(128),p24(128),                
'     7             ap01(128),ap02(128),ap03(128),ap04(128),            
'     8             ap05(128),ap06(128),ap07(128),ap08(128),            
'     9             ap09(128),ap10(128),ap11(128),ap12(128),            
'     *             ap13(128),ap14(128),ap15(128),ap16(128),            
'     1             ap17(128),ap18(128),ap19(128),ap20(128),            
'     2             ap21(128),ap22(128),ap23(128),ap24(128),            
'     3             dummy(1)                                            
'      dimension s(121,*),lm(15*iprec,*),ap(*),p(*)                     
'c                                                                      
'      scalar = .false.                                                 
'      if (llt.lt.0) then                                               
'         llt = - llt                                                   
'         scalar = .true.                                               
'      endif                                                            
'c                                                                      
'c.... gather operations                                                
'c                                                                      
'      i = 0
'      p(i) = 0.0                                                       
'      do 100 i=lft,llt                                                 
'         p01(i) = p(lm( 1,i))                                          
'         p02(i) = p(lm( 2,i))                                          
'         p03(i) = p(lm( 3,i))                                          
'         p04(i) = p(lm( 4,i))                                          
'         p05(i) = p(lm( 5,i))                                          
'         p06(i) = p(lm( 6,i))                                          
'         p07(i) = p(lm( 7,i))                                          
'         p08(i) = p(lm( 8,i))                                          
'         p09(i) = p(lm( 9,i))                                          
'         p10(i) = p(lm(10,i))                                          
'         p11(i) = p(lm(11,i))                                          
'         p12(i) = p(lm(12,i))                                          
'         p13(i) = p(lm(13,i))                                          
'         p14(i) = p(lm(14,i))                                          
'         p15(i) = p(lm(15,i))                                          
'  100 continue                                                         
'c                                                                      
'c.... do-loops broken to allow optimization                            
'c                                                                      
'      do 200 i=lft,llt                                                 
'        ap01(i) = s(  1,i)*p01(i) + s(  2,i)*p02(i) + s(  4,i)*p03(i)  
'     1          + s(  7,i)*p04(i) + s( 11,i)*p05(i) + s( 16,i)*p06(i)  
'     2          + s( 22,i)*p07(i) + s( 29,i)*p08(i) + s( 37,i)*p09(i)  
'     3          + s( 46,i)*p10(i) + s( 56,i)*p11(i) + s( 67,i)*p12(i)  
'     4          + s( 79,i)*p13(i) + s( 92,i)*p14(i) + s(106,i)*p15(i)  
'  200 continue                                                         
'c                                                                      
'      do 201 i=lft,llt                                                 
'        ap02(i) = s(  2,i)*p01(i) + s(  3,i)*p02(i) + s(  5,i)*p03(i)  
'     1          + s(  8,i)*p04(i) + s( 12,i)*p05(i) + s( 17,i)*p06(i)  
'     2          + s( 23,i)*p07(i) + s( 30,i)*p08(i) + s( 38,i)*p09(i)  
'     3          + s( 47,i)*p10(i) + s( 57,i)*p11(i) + s( 68,i)*p12(i)  
'     4          + s( 80,i)*p13(i) + s( 93,i)*p14(i) + s(107,i)*p15(i)  
'  201 continue                                                         
'c                                                                      
'      do 202 i=lft,llt                                                 
'        ap03(i) = s(  4,i)*p01(i) + s(  5,i)*p02(i) + s(  6,i)*p03(i)  
'     1          + s(  9,i)*p04(i) + s( 13,i)*p05(i) + s( 18,i)*p06(i)  
'     2          + s( 24,i)*p07(i) + s( 31,i)*p08(i) + s( 39,i)*p09(i)  
'     3          + s( 48,i)*p10(i) + s( 58,i)*p11(i) + s( 69,i)*p12(i)  
'     4          + s( 81,i)*p13(i) + s( 94,i)*p14(i) + s(108,i)*p15(i)  
'  202 continue                                                         
'c                                                                      
'      do 203 i=lft,llt                                                 
'        ap04(i) = s(  7,i)*p01(i) + s(  8,i)*p02(i) + s(  9,i)*p03(i)  
'     1          + s( 10,i)*p04(i) + s( 14,i)*p05(i) + s( 19,i)*p06(i)  
'     2          + s( 25,i)*p07(i) + s( 32,i)*p08(i) + s( 40,i)*p09(i)  
'     3          + s( 49,i)*p10(i) + s( 59,i)*p11(i) + s( 70,i)*p12(i)  
'     4          + s( 82,i)*p13(i) + s( 95,i)*p14(i) + s(109,i)*p15(i)  
'  203 continue                                                         
'c                                                                      
'      do 204 i=lft,llt                                                 
'        ap05(i) = s( 11,i)*p01(i) + s( 12,i)*p02(i) + s( 13,i)*p03(i)  
'     1          + s( 14,i)*p04(i) + s( 15,i)*p05(i) + s( 20,i)*p06(i)  
'     2          + s( 26,i)*p07(i) + s( 33,i)*p08(i) + s( 41,i)*p09(i)  
'     3          + s( 50,i)*p10(i) + s( 60,i)*p11(i) + s( 71,i)*p12(i)  
'     4          + s( 83,i)*p13(i) + s( 96,i)*p14(i) + s(110,i)*p15(i)  
'  204 continue                                                         
'c                                                                      
'      do 205 i=lft,llt                                                 
'        ap06(i) = s( 16,i)*p01(i) + s( 17,i)*p02(i) + s( 18,i)*p03(i)  
'     1          + s( 19,i)*p04(i) + s( 20,i)*p05(i) + s( 21,i)*p06(i)  
'     2          + s( 27,i)*p07(i) + s( 34,i)*p08(i) + s( 42,i)*p09(i)  
'     3          + s( 51,i)*p10(i) + s( 61,i)*p11(i) + s( 72,i)*p12(i)  
'     4          + s( 84,i)*p13(i) + s( 97,i)*p14(i) + s(111,i)*p15(i)  
'  205 continue                                                         
'c                                                                      
'      do 206 i=lft,llt                                                 
'        ap07(i) = s( 22,i)*p01(i) + s( 23,i)*p02(i) + s( 24,i)*p03(i)  
'     1          + s( 25,i)*p04(i) + s( 26,i)*p05(i) + s( 27,i)*p06(i)  
'     2          + s( 28,i)*p07(i) + s( 35,i)*p08(i) + s( 43,i)*p09(i)  
'     3          + s( 52,i)*p10(i) + s( 62,i)*p11(i) + s( 73,i)*p12(i)  
'     4          + s( 85,i)*p13(i) + s( 98,i)*p14(i) + s(112,i)*p15(i)  
'  206 continue                                                         
'c                                                                      
'      do 207 i=lft,llt                                                 
'        ap08(i) = s( 29,i)*p01(i) + s( 30,i)*p02(i) + s( 31,i)*p03(i)  
'     1          + s( 32,i)*p04(i) + s( 33,i)*p05(i) + s( 34,i)*p06(i)  
'     2          + s( 35,i)*p07(i) + s( 36,i)*p08(i) + s( 44,i)*p09(i)  
'     3          + s( 53,i)*p10(i) + s( 63,i)*p11(i) + s( 74,i)*p12(i)  
'     4          + s( 86,i)*p13(i) + s( 99,i)*p14(i) + s(113,i)*p15(i)  
'  207 continue                                                         
'c                                                                      
'      do 208 i=lft,llt                                                 
'        ap09(i) = s( 37,i)*p01(i) + s( 38,i)*p02(i) + s( 39,i)*p03(i)  
'     1          + s( 40,i)*p04(i) + s( 41,i)*p05(i) + s( 42,i)*p06(i)  
'     2          + s( 43,i)*p07(i) + s( 44,i)*p08(i) + s( 45,i)*p09(i)  
'     3          + s( 54,i)*p10(i) + s( 64,i)*p11(i) + s( 75,i)*p12(i)  
'     4          + s( 87,i)*p13(i) + s(100,i)*p14(i) + s(114,i)*p15(i)  
'  208 continue                                                         
'c                                                                      
'      do 209 i=lft,llt                                                 
'        ap10(i) = s( 46,i)*p01(i) + s( 47,i)*p02(i) + s( 48,i)*p03(i)  
'     1          + s( 49,i)*p04(i) + s( 50,i)*p05(i) + s( 51,i)*p06(i)  
'     2          + s( 52,i)*p07(i) + s( 53,i)*p08(i) + s( 54,i)*p09(i)  
'     3          + s( 55,i)*p10(i) + s( 65,i)*p11(i) + s( 76,i)*p12(i)  
'     4          + s( 88,i)*p13(i) + s(101,i)*p14(i) + s(115,i)*p15(i)  
'  209 continue                                                         
'c                                                                      
'      do 210 i=lft,llt                                                 
'        ap11(i) = s( 56,i)*p01(i) + s( 57,i)*p02(i) + s( 58,i)*p03(i)  
'     1          + s( 59,i)*p04(i) + s( 60,i)*p05(i) + s( 61,i)*p06(i)  
'     2          + s( 62,i)*p07(i) + s( 63,i)*p08(i) + s( 64,i)*p09(i)  
'     3          + s( 65,i)*p10(i) + s( 66,i)*p11(i) + s( 77,i)*p12(i)  
'     4          + s( 89,i)*p13(i) + s(102,i)*p14(i) + s(116,i)*p15(i)  
'  210 continue                                                         
'c                                                                      
'      do 211 i=lft,llt                                                 
'        ap12(i) = s( 67,i)*p01(i) + s( 68,i)*p02(i) + s( 69,i)*p03(i)  
'     1          + s( 70,i)*p04(i) + s( 71,i)*p05(i) + s( 72,i)*p06(i)  
'     2          + s( 73,i)*p07(i) + s( 74,i)*p08(i) + s( 75,i)*p09(i)  
'     3          + s( 76,i)*p10(i) + s( 77,i)*p11(i) + s( 78,i)*p12(i)  
'     4          + s( 90,i)*p13(i) + s(103,i)*p14(i) + s(117,i)*p15(i)  
'  211 continue                                                         
'c                                                                      
'      do 212 i=lft,llt                                                 
'        ap13(i) = s( 79,i)*p01(i) + s( 80,i)*p02(i) + s( 81,i)*p03(i)  
'     1          + s( 82,i)*p04(i) + s( 83,i)*p05(i) + s( 84,i)*p06(i)  
'     2          + s( 85,i)*p07(i) + s( 86,i)*p08(i) + s( 87,i)*p09(i)  
'     3          + s( 88,i)*p10(i) + s( 89,i)*p11(i) + s( 90,i)*p12(i)  
'     4          + s( 91,i)*p13(i) + s(104,i)*p14(i) + s(118,i)*p15(i)  
'  212 continue                                                         
'c                                                                      
'      do 213 i=lft,llt                                                 
'        ap14(i) = s( 92,i)*p01(i) + s( 93,i)*p02(i) + s( 94,i)*p03(i)  
'     1          + s( 95,i)*p04(i) + s( 96,i)*p05(i) + s( 97,i)*p06(i)  
'     2          + s( 98,i)*p07(i) + s( 99,i)*p08(i) + s(100,i)*p09(i)  
'     3          + s(101,i)*p10(i) + s(102,i)*p11(i) + s(103,i)*p12(i)  
'     4          + s(104,i)*p13(i) + s(105,i)*p14(i) + s(119,i)*p15(i)  
'  213 continue                                                         
'c                                                                      
'      do 214 i=lft,llt                                                 
'        ap15(i) = s(106,i)*p01(i) + s(107,i)*p02(i) + s(108,i)*p03(i)  
'     1          + s(109,i)*p04(i) + s(110,i)*p05(i) + s(111,i)*p06(i)  
'     2          + s(112,i)*p07(i) + s(113,i)*p08(i) + s(114,i)*p09(i)  
'     3          + s(115,i)*p10(i) + s(116,i)*p11(i) + s(117,i)*p12(i)  
'     4          + s(118,i)*p13(i) + s(119,i)*p14(i) + s(120,i)*p15(i)  
'  214 continue                                                         
'c                                                                      
'      if (scalar) then                                                 
'      do 300 i=lft,llt                                                 
'         ap(lm( 1,i)) = ap(lm( 1,i)) + ap01(i)                         
'         ap(lm( 2,i)) = ap(lm( 2,i)) + ap02(i)                         
'         ap(lm( 3,i)) = ap(lm( 3,i)) + ap03(i)                         
'         ap(lm( 4,i)) = ap(lm( 4,i)) + ap04(i)                         
'         ap(lm( 5,i)) = ap(lm( 5,i)) + ap05(i)                         
'         ap(lm( 6,i)) = ap(lm( 6,i)) + ap06(i)                         
'         ap(lm( 7,i)) = ap(lm( 7,i)) + ap07(i)                         
'         ap(lm( 8,i)) = ap(lm( 8,i)) + ap08(i)                         
'         ap(lm( 9,i)) = ap(lm( 9,i)) + ap09(i)                         
'         ap(lm(10,i)) = ap(lm(10,i)) + ap10(i)                         
'         ap(lm(11,i)) = ap(lm(11,i)) + ap11(i)                         
'         ap(lm(12,i)) = ap(lm(12,i)) + ap12(i)                         
'         ap(lm(13,i)) = ap(lm(13,i)) + ap13(i)                         
'         ap(lm(14,i)) = ap(lm(14,i)) + ap14(i)                         
'         ap(lm(15,i)) = ap(lm(15,i)) + ap15(i)                         
'  300 continue                                                         
'      else                                                             
'c                                                                      
'cdir$ ivdep                                                            
'      do 400 i=lft,llt                                                 
'         ap(lm( 1,i)) = ap(lm( 1,i)) + ap01(i)                         
'         ap(lm( 2,i)) = ap(lm( 2,i)) + ap02(i)                         
'         ap(lm( 3,i)) = ap(lm( 3,i)) + ap03(i)                         
'         ap(lm( 4,i)) = ap(lm( 4,i)) + ap04(i)                         
'         ap(lm( 5,i)) = ap(lm( 5,i)) + ap05(i)                         
'         ap(lm( 6,i)) = ap(lm( 6,i)) + ap06(i)                         
'         ap(lm( 7,i)) = ap(lm( 7,i)) + ap07(i)                         
'         ap(lm( 8,i)) = ap(lm( 8,i)) + ap08(i)                         
'         ap(lm( 9,i)) = ap(lm( 9,i)) + ap09(i)                         
'         ap(lm(10,i)) = ap(lm(10,i)) + ap10(i)                         
'         ap(lm(11,i)) = ap(lm(11,i)) + ap11(i)                         
'         ap(lm(12,i)) = ap(lm(12,i)) + ap12(i)                         
'         ap(lm(13,i)) = ap(lm(13,i)) + ap13(i)                         
'         ap(lm(14,i)) = ap(lm(14,i)) + ap14(i)                         
'         ap(lm(15,i)) = ap(lm(15,i)) + ap15(i)                         
'  400 continue                                                         
'      endif                                                            
'c                                                                      
'      return                                                           
'      end                                                              

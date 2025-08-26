'This file contains all the methods of matv24.f
Partial Public Class clsSolve


    ' YC 092018
    Private p01(128), p02(128), p03(128), p04(128), p05(128), p06(128), p07(128), p08(128), p09(128), p10(128),
      p11_31(128), p12_31(128), p13_31(128), p14_31(128), p15_31(128), p16_31(128), p17_31(128), p18_31(128),
 p19(128), p20(128), p21_31(128), p22_31(128), p23_31(128), p24_31(128) As Double

    Private ap01(128), ap02(128), ap03(128), ap04(128), ap05(128), ap06(128), ap07(128), ap08(128),
           ap09(128), ap10(128), ap11(128), ap12(128), ap13(128), ap14(128), ap15(128), ap16(128),
             ap17(128), ap18(128), ap19(128), ap20(128), ap21(128), ap22(128), ap23(128), ap24(128) As Double
    ' YC 092018 END



    ''' <summary>
    ''' for matrix-vector product for element with 24 dof
    ''' </summary>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    ''' <param name="s"></param>
    ''' <param name="lm"></param>
    ''' <param name="ap"></param>
    ''' <param name="p"></param>
    Public Sub matv24(lft As Integer, llt As Integer, s(,) As Double,
                      lm(,) As Integer, ap() As Double, p() As Double, kstore As Integer) ' QW 08-14-2019

        Dim scalar As Boolean, ii As Integer

        scalar = False
        If llt < 0 Then
            llt = -llt
            scalar = True
        End If
        'Call Check2DT(s, 300, 64)
        'c.... gather operations                                                 

        Dim j As Integer    ' YC 102418

        Dim i = 0
        p(i) = 0.0
        For i = lft To llt ' QW 12-12-2018-
            ii = kstore + i - 1
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
            'p16_31(i) = p(lm(15, i))
            'p17_31(i) = p(lm(16, i))
            'p18_31(i) = p(lm(17, i))
            'p19(i) = p(lm(18, i))
            'p20(i) = p(lm(19, i))
            'p21_31(i) = p(lm(20, i))
            'p22_31(i) = p(lm(21, i))
            'p23_31(i) = p(lm(22, i))
            'p24_31(i) = p(lm(23, i))
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
            p16_31(i) = p(lm(16, ii))
            p17_31(i) = p(lm(17, ii))
            p18_31(i) = p(lm(18, ii))
            p19(i) = p(lm(19, ii))
            p20(i) = p(lm(20, ii))
            p21_31(i) = p(lm(21, ii))
            p22_31(i) = p(lm(22, ii))
            p23_31(i) = p(lm(23, ii))
            p24_31(i) = p(lm(24, ii))
            ' YC 102418 END
        Next

        'c.... do-loops broken to allow optimization                             

        ' YC 102418
        'For i = lft - 1 To llt - 1
        '    ap01(i) = s(0, i) * p01(i) + s(1, i) * p02(i) + s(3, i) * p03(i) +
        '        s(6, i) * p04(i) + s(10, i) * p05(i) + s(15, i) * p06(i) +
        '        s(21, i) * p07(i) + s(28, i) * p08(i) + s(36, i) * p09(i) +
        '        s(45, i) * p10(i) + s(55, i) * p11_31(i) + s(66, i) * p12_31(i) +
        '        s(78, i) * p13_31(i) + s(91, i) * p14_31(i) + s(105, i) * p15_31(i) +
        '        s(120, i) * p16_31(i) + s(136, i) * p17_31(i) + s(153, i) * p18_31(i) +
        '        s(171, i) * p19(i) + s(190, i) * p20(i) + s(210, i) * p21_31(i) +
        '        s(231, i) * p22_31(i) + s(253, i) * p23_31(i) + s(276, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap02(i) = s(1, i) * p01(i) + s(2, i) * p02(i) + s(4, i) * p03(i) +
        '        s(7, i) * p04(i) + s(11, i) * p05(i) + s(16, i) * p06(i) +
        '        s(22, i) * p07(i) + s(29, i) * p08(i) + s(37, i) * p09(i) +
        '        s(46, i) * p10(i) + s(56, i) * p11_31(i) + s(67, i) * p12_31(i) +
        '        s(79, i) * p13_31(i) + s(92, i) * p14_31(i) + s(106, i) * p15_31(i) +
        '        s(121, i) * p16_31(i) + s(137, i) * p17_31(i) + s(154, i) * p18_31(i) +
        '        s(172, i) * p19(i) + s(191, i) * p20(i) + s(211, i) * p21_31(i) +
        '        s(232, i) * p22_31(i) + s(254, i) * p23_31(i) + s(277, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap03(i) = s(3, i) * p01(i) + s(4, i) * p02(i) + s(5, i) * p03(i) +
        '        s(8, i) * p04(i) + s(12, i) * p05(i) + s(17, i) * p06(i) +
        '        s(23, i) * p07(i) + s(30, i) * p08(i) + s(38, i) * p09(i) +
        '        s(47, i) * p10(i) + s(57, i) * p11_31(i) + s(68, i) * p12_31(i) +
        '        s(80, i) * p13_31(i) + s(93, i) * p14_31(i) + s(107, i) * p15_31(i) +
        '        s(122, i) * p16_31(i) + s(138, i) * p17_31(i) + s(155, i) * p18_31(i) +
        '        s(173, i) * p19(i) + s(192, i) * p20(i) + s(212, i) * p21_31(i) +
        '        s(233, i) * p22_31(i) + s(255, i) * p23_31(i) + s(278, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap04(i) = s(6, i) * p01(i) + s(7, i) * p02(i) + s(8, i) * p03(i) +
        '        s(9, i) * p04(i) + s(13, i) * p05(i) + s(18, i) * p06(i) +
        '        s(24, i) * p07(i) + s(31, i) * p08(i) + s(39, i) * p09(i) +
        '        s(48, i) * p10(i) + s(58, i) * p11_31(i) + s(69, i) * p12_31(i) +
        '        s(81, i) * p13_31(i) + s(94, i) * p14_31(i) + s(108, i) * p15_31(i) +
        '        s(123, i) * p16_31(i) + s(139, i) * p17_31(i) + s(156, i) * p18_31(i) +
        '        s(174, i) * p19(i) + s(193, i) * p20(i) + s(213, i) * p21_31(i) +
        '        s(234, i) * p22_31(i) + s(256, i) * p23_31(i) + s(279, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap05(i) = s(10, i) * p01(i) + s(11, i) * p02(i) + s(12, i) * p03(i) +
        '        s(13, i) * p04(i) + s(14, i) * p05(i) + s(19, i) * p06(i) +
        '        s(25, i) * p07(i) + s(32, i) * p08(i) + s(40, i) * p09(i) +
        '        s(49, i) * p10(i) + s(59, i) * p11_31(i) + s(70, i) * p12_31(i) +
        '        s(82, i) * p13_31(i) + s(95, i) * p14_31(i) + s(109, i) * p15_31(i) +
        '        s(124, i) * p16_31(i) + s(140, i) * p17_31(i) + s(157, i) * p18_31(i) +
        '        s(175, i) * p19(i) + s(194, i) * p20(i) + s(214, i) * p21_31(i) +
        '        s(235, i) * p22_31(i) + s(257, i) * p23_31(i) + s(280, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap06(i) = s(15, i) * p01(i) + s(16, i) * p02(i) + s(17, i) * p03(i) +
        '      s(18, i) * p04(i) + s(19, i) * p05(i) + s(20, i) * p06(i) +
        '      s(26, i) * p07(i) + s(33, i) * p08(i) + s(41, i) * p09(i) +
        '      s(50, i) * p10(i) + s(60, i) * p11_31(i) + s(71, i) * p12_31(i) +
        '      s(83, i) * p13_31(i) + s(96, i) * p14_31(i) + s(110, i) * p15_31(i) +
        '      s(125, i) * p16_31(i) + s(141, i) * p17_31(i) + s(158, i) * p18_31(i) +
        '      s(176, i) * p19(i) + s(195, i) * p20(i) + s(215, i) * p21_31(i) +
        '      s(236, i) * p22_31(i) + s(258, i) * p23_31(i) + s(281, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap07(i) = s(21, i) * p01(i) + s(22, i) * p02(i) + s(23, i) * p03(i) +
        '      s(24, i) * p04(i) + s(25, i) * p05(i) + s(26, i) * p06(i) +
        '      s(27, i) * p07(i) + s(34, i) * p08(i) + s(42, i) * p09(i) +
        '      s(51, i) * p10(i) + s(61, i) * p11_31(i) + s(72, i) * p12_31(i) +
        '      s(84, i) * p13_31(i) + s(97, i) * p14_31(i) + s(111, i) * p15_31(i) +
        '      s(126, i) * p16_31(i) + s(142, i) * p17_31(i) + s(159, i) * p18_31(i) +
        '      s(177, i) * p19(i) + s(196, i) * p20(i) + s(216, i) * p21_31(i) +
        '      s(237, i) * p22_31(i) + s(259, i) * p23_31(i) + s(282, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap08(i) = s(28, i) * p01(i) + s(29, i) * p02(i) + s(30, i) * p03(i) +
        '      s(31, i) * p04(i) + s(32, i) * p05(i) + s(33, i) * p06(i) +
        '      s(34, i) * p07(i) + s(35, i) * p08(i) + s(43, i) * p09(i) +
        '      s(52, i) * p10(i) + s(62, i) * p11_31(i) + s(73, i) * p12_31(i) +
        '      s(85, i) * p13_31(i) + s(98, i) * p14_31(i) + s(112, i) * p15_31(i) +
        '      s(127, i) * p16_31(i) + s(143, i) * p17_31(i) + s(160, i) * p18_31(i) +
        '      s(178, i) * p19(i) + s(197, i) * p20(i) + s(217, i) * p21_31(i) +
        '      s(238, i) * p22_31(i) + s(260, i) * p23_31(i) + s(283, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap09(i) = s(36, i) * p01(i) + s(37, i) * p02(i) + s(38, i) * p03(i) +
        '      s(39, i) * p04(i) + s(40, i) * p05(i) + s(41, i) * p06(i) +
        '      s(42, i) * p07(i) + s(43, i) * p08(i) + s(44, i) * p09(i) +
        '      s(53, i) * p10(i) + s(63, i) * p11_31(i) + s(74, i) * p12_31(i) +
        '      s(86, i) * p13_31(i) + s(99, i) * p14_31(i) + s(113, i) * p15_31(i) +
        '      s(128, i) * p16_31(i) + s(144, i) * p17_31(i) + s(161, i) * p18_31(i) +
        '      s(179, i) * p19(i) + s(198, i) * p20(i) + s(218, i) * p21_31(i) +
        '      s(239, i) * p22_31(i) + s(261, i) * p23_31(i) + s(284, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap10(i) = s(45, i) * p01(i) + s(46, i) * p02(i) + s(47, i) * p03(i) +
        '      s(48, i) * p04(i) + s(49, i) * p05(i) + s(50, i) * p06(i) +
        '      s(51, i) * p07(i) + s(52, i) * p08(i) + s(53, i) * p09(i) +
        '      s(54, i) * p10(i) + s(64, i) * p11_31(i) + s(75, i) * p12_31(i) +
        '      s(87, i) * p13_31(i) + s(100, i) * p14_31(i) + s(114, i) * p15_31(i) +
        '      s(129, i) * p16_31(i) + s(145, i) * p17_31(i) + s(162, i) * p18_31(i) +
        '      s(180, i) * p19(i) + s(199, i) * p20(i) + s(219, i) * p21_31(i) +
        '      s(240, i) * p22_31(i) + s(262, i) * p23_31(i) + s(285, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap11(i) = s(55, i) * p01(i) + s(56, i) * p02(i) + s(57, i) * p03(i) +
        '      s(58, i) * p04(i) + s(59, i) * p05(i) + s(60, i) * p06(i) +
        '      s(61, i) * p07(i) + s(62, i) * p08(i) + s(63, i) * p09(i) +
        '      s(64, i) * p10(i) + s(65, i) * p11_31(i) + s(76, i) * p12_31(i) +
        '      s(88, i) * p13_31(i) + s(101, i) * p14_31(i) + s(115, i) * p15_31(i) +
        '      s(130, i) * p16_31(i) + s(146, i) * p17_31(i) + s(163, i) * p18_31(i) +
        '      s(181, i) * p19(i) + s(200, i) * p20(i) + s(220, i) * p21_31(i) +
        '      s(241, i) * p22_31(i) + s(263, i) * p23_31(i) + s(286, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap12(i) = s(66, i) * p01(i) + s(67, i) * p02(i) + s(68, i) * p03(i) +
        '      s(69, i) * p04(i) + s(70, i) * p05(i) + s(71, i) * p06(i) +
        '      s(72, i) * p07(i) + s(73, i) * p08(i) + s(74, i) * p09(i) +
        '      s(75, i) * p10(i) + s(76, i) * p11_31(i) + s(77, i) * p12_31(i) +
        '      s(89, i) * p13_31(i) + s(102, i) * p14_31(i) + s(116, i) * p15_31(i) +
        '      s(131, i) * p16_31(i) + s(147, i) * p17_31(i) + s(164, i) * p18_31(i) +
        '      s(182, i) * p19(i) + s(201, i) * p20(i) + s(221, i) * p21_31(i) +
        '      s(242, i) * p22_31(i) + s(264, i) * p23_31(i) + s(287, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap13(i) = s(78, i) * p01(i) + s(79, i) * p02(i) + s(80, i) * p03(i) +
        '      s(81, i) * p04(i) + s(82, i) * p05(i) + s(83, i) * p06(i) +
        '      s(84, i) * p07(i) + s(85, i) * p08(i) + s(86, i) * p09(i) +
        '      s(87, i) * p10(i) + s(88, i) * p11_31(i) + s(89, i) * p12_31(i) +
        '      s(90, i) * p13_31(i) + s(103, i) * p14_31(i) + s(117, i) * p15_31(i) +
        '      s(132, i) * p16_31(i) + s(148, i) * p17_31(i) + s(165, i) * p18_31(i) +
        '      s(183, i) * p19(i) + s(202, i) * p20(i) + s(222, i) * p21_31(i) +
        '      s(243, i) * p22_31(i) + s(265, i) * p23_31(i) + s(288, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap14(i) = s(91, i) * p01(i) + s(92, i) * p02(i) + s(93, i) * p03(i) +
        '      s(94, i) * p04(i) + s(95, i) * p05(i) + s(96, i) * p06(i) +
        '      s(97, i) * p07(i) + s(98, i) * p08(i) + s(99, i) * p09(i) +
        '      s(100, i) * p10(i) + s(101, i) * p11_31(i) + s(102, i) * p12_31(i) +
        '      s(103, i) * p13_31(i) + s(104, i) * p14_31(i) + s(118, i) * p15_31(i) +
        '      s(133, i) * p16_31(i) + s(149, i) * p17_31(i) + s(166, i) * p18_31(i) +
        '      s(184, i) * p19(i) + s(203, i) * p20(i) + s(223, i) * p21_31(i) +
        '      s(244, i) * p22_31(i) + s(266, i) * p23_31(i) + s(289, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap15(i) = s(105, i) * p01(i) + s(106, i) * p02(i) + s(107, i) * p03(i) +
        '      s(108, i) * p04(i) + s(109, i) * p05(i) + s(110, i) * p06(i) +
        '      s(111, i) * p07(i) + s(112, i) * p08(i) + s(113, i) * p09(i) +
        '      s(114, i) * p10(i) + s(115, i) * p11_31(i) + s(116, i) * p12_31(i) +
        '      s(117, i) * p13_31(i) + s(118, i) * p14_31(i) + s(119, i) * p15_31(i) +
        '      s(134, i) * p16_31(i) + s(150, i) * p17_31(i) + s(167, i) * p18_31(i) +
        '      s(185, i) * p19(i) + s(204, i) * p20(i) + s(224, i) * p21_31(i) +
        '      s(245, i) * p22_31(i) + s(267, i) * p23_31(i) + s(290, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap16(i) = s(120, i) * p01(i) + s(121, i) * p02(i) + s(122, i) * p03(i) +
        '      s(123, i) * p04(i) + s(124, i) * p05(i) + s(125, i) * p06(i) +
        '      s(126, i) * p07(i) + s(127, i) * p08(i) + s(128, i) * p09(i) +
        '      s(129, i) * p10(i) + s(130, i) * p11_31(i) + s(131, i) * p12_31(i) +
        '      s(132, i) * p13_31(i) + s(133, i) * p14_31(i) + s(134, i) * p15_31(i) +
        '      s(135, i) * p16_31(i) + s(151, i) * p17_31(i) + s(168, i) * p18_31(i) +
        '      s(186, i) * p19(i) + s(205, i) * p20(i) + s(225, i) * p21_31(i) +
        '      s(246, i) * p22_31(i) + s(268, i) * p23_31(i) + s(291, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap17(i) = s(136, i) * p01(i) + s(137, i) * p02(i) + s(138, i) * p03(i) +
        '      s(139, i) * p04(i) + s(140, i) * p05(i) + s(141, i) * p06(i) +
        '      s(142, i) * p07(i) + s(143, i) * p08(i) + s(144, i) * p09(i) +
        '      s(145, i) * p10(i) + s(146, i) * p11_31(i) + s(147, i) * p12_31(i) +
        '      s(148, i) * p13_31(i) + s(149, i) * p14_31(i) + s(150, i) * p15_31(i) +
        '      s(151, i) * p16_31(i) + s(152, i) * p17_31(i) + s(169, i) * p18_31(i) +
        '      s(187, i) * p19(i) + s(206, i) * p20(i) + s(226, i) * p21_31(i) +
        '      s(247, i) * p22_31(i) + s(269, i) * p23_31(i) + s(292, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap18(i) = s(153, i) * p01(i) + s(154, i) * p02(i) + s(155, i) * p03(i) +
        '      s(156, i) * p04(i) + s(157, i) * p05(i) + s(158, i) * p06(i) +
        '      s(159, i) * p07(i) + s(160, i) * p08(i) + s(161, i) * p09(i) +
        '      s(162, i) * p10(i) + s(163, i) * p11_31(i) + s(164, i) * p12_31(i) +
        '      s(165, i) * p13_31(i) + s(166, i) * p14_31(i) + s(167, i) * p15_31(i) +
        '      s(168, i) * p16_31(i) + s(169, i) * p17_31(i) + s(170, i) * p18_31(i) +
        '      s(188, i) * p19(i) + s(207, i) * p20(i) + s(227, i) * p21_31(i) +
        '      s(248, i) * p22_31(i) + s(270, i) * p23_31(i) + s(293, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap19(i) = s(171, i) * p01(i) + s(172, i) * p02(i) + s(173, i) * p03(i) +
        '      s(174, i) * p04(i) + s(175, i) * p05(i) + s(176, i) * p06(i) +
        '      s(177, i) * p07(i) + s(178, i) * p08(i) + s(179, i) * p09(i) +
        '      s(180, i) * p10(i) + s(181, i) * p11_31(i) + s(182, i) * p12_31(i) +
        '      s(183, i) * p13_31(i) + s(184, i) * p14_31(i) + s(185, i) * p15_31(i) +
        '      s(186, i) * p16_31(i) + s(187, i) * p17_31(i) + s(188, i) * p18_31(i) +
        '      s(189, i) * p19(i) + s(208, i) * p20(i) + s(228, i) * p21_31(i) +
        '      s(249, i) * p22_31(i) + s(271, i) * p23_31(i) + s(294, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap20(i) = s(190, i) * p01(i) + s(191, i) * p02(i) + s(192, i) * p03(i) +
        '      s(193, i) * p04(i) + s(194, i) * p05(i) + s(195, i) * p06(i) +
        '      s(196, i) * p07(i) + s(197, i) * p08(i) + s(198, i) * p09(i) +
        '      s(199, i) * p10(i) + s(200, i) * p11_31(i) + s(201, i) * p12_31(i) +
        '      s(202, i) * p13_31(i) + s(203, i) * p14_31(i) + s(204, i) * p15_31(i) +
        '      s(205, i) * p16_31(i) + s(206, i) * p17_31(i) + s(207, i) * p18_31(i) +
        '      s(208, i) * p19(i) + s(209, i) * p20(i) + s(229, i) * p21_31(i) +
        '      s(250, i) * p22_31(i) + s(272, i) * p23_31(i) + s(295, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap21(i) = s(210, i) * p01(i) + s(211, i) * p02(i) + s(212, i) * p03(i) +
        '      s(213, i) * p04(i) + s(214, i) * p05(i) + s(215, i) * p06(i) +
        '      s(216, i) * p07(i) + s(217, i) * p08(i) + s(218, i) * p09(i) +
        '      s(219, i) * p10(i) + s(220, i) * p11_31(i) + s(221, i) * p12_31(i) +
        '      s(222, i) * p13_31(i) + s(223, i) * p14_31(i) + s(224, i) * p15_31(i) +
        '      s(225, i) * p16_31(i) + s(226, i) * p17_31(i) + s(227, i) * p18_31(i) +
        '      s(228, i) * p19(i) + s(229, i) * p20(i) + s(230, i) * p21_31(i) +
        '      s(251, i) * p22_31(i) + s(273, i) * p23_31(i) + s(296, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap22(i) = s(231, i) * p01(i) + s(232, i) * p02(i) + s(233, i) * p03(i) +
        '      s(234, i) * p04(i) + s(235, i) * p05(i) + s(236, i) * p06(i) +
        '      s(237, i) * p07(i) + s(238, i) * p08(i) + s(239, i) * p09(i) +
        '      s(240, i) * p10(i) + s(241, i) * p11_31(i) + s(242, i) * p12_31(i) +
        '      s(243, i) * p13_31(i) + s(244, i) * p14_31(i) + s(245, i) * p15_31(i) +
        '      s(246, i) * p16_31(i) + s(247, i) * p17_31(i) + s(248, i) * p18_31(i) +
        '      s(249, i) * p19(i) + s(250, i) * p20(i) + s(251, i) * p21_31(i) +
        '      s(252, i) * p22_31(i) + s(274, i) * p23_31(i) + s(297, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap23(i) = s(253, i) * p01(i) + s(254, i) * p02(i) + s(255, i) * p03(i) +
        '      s(256, i) * p04(i) + s(257, i) * p05(i) + s(258, i) * p06(i) +
        '      s(259, i) * p07(i) + s(260, i) * p08(i) + s(261, i) * p09(i) +
        '      s(262, i) * p10(i) + s(263, i) * p11_31(i) + s(264, i) * p12_31(i) +
        '      s(265, i) * p13_31(i) + s(266, i) * p14_31(i) + s(267, i) * p15_31(i) +
        '      s(268, i) * p16_31(i) + s(269, i) * p17_31(i) + s(270, i) * p18_31(i) +
        '      s(271, i) * p19(i) + s(272, i) * p20(i) + s(273, i) * p21_31(i) +
        '      s(274, i) * p22_31(i) + s(275, i) * p23_31(i) + s(298, i) * p24_31(i)
        'Next

        'For i = lft - 1 To llt - 1
        '    ap24(i) = s(276, i) * p01(i) + s(277, i) * p02(i) + s(278, i) * p03(i) +
        '      s(279, i) * p04(i) + s(280, i) * p05(i) + s(281, i) * p06(i) +
        '      s(282, i) * p07(i) + s(283, i) * p08(i) + s(284, i) * p09(i) +
        '      s(285, i) * p10(i) + s(286, i) * p11_31(i) + s(287, i) * p12_31(i) +
        '      s(288, i) * p13_31(i) + s(289, i) * p14_31(i) + s(290, i) * p15_31(i) +
        '      s(291, i) * p16_31(i) + s(292, i) * p17_31(i) + s(293, i) * p18_31(i) +
        '      s(294, i) * p19(i) + s(295, i) * p20(i) + s(296, i) * p21_31(i) +
        '      s(297, i) * p22_31(i) + s(298, i) * p23_31(i) + s(299, i) * p24_31(i)
        'Next

        For i = lft To llt
            ii = kstore + i - 1
            ap01(i) = s(1, ii) * p01(i) + s(2, ii) * p02(i) + s(4, ii) * p03(i) +
                s(7, ii) * p04(i) + s(11, ii) * p05(i) + s(16, ii) * p06(i) +
                s(22, ii) * p07(i) + s(29, ii) * p08(i) + s(37, ii) * p09(i) +
                s(46, ii) * p10(i) + s(56, ii) * p11_31(i) + s(67, ii) * p12_31(i) +
                s(79, ii) * p13_31(i) + s(92, ii) * p14_31(i) + s(106, ii) * p15_31(i) +
                s(121, ii) * p16_31(i) + s(137, ii) * p17_31(i) + s(154, ii) * p18_31(i) +
                s(172, ii) * p19(i) + s(191, ii) * p20(i) + s(211, ii) * p21_31(i) +
                s(232, ii) * p22_31(i) + s(254, ii) * p23_31(i) + s(277, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap02(i) = s(2, ii) * p01(i) + s(3, ii) * p02(i) + s(5, ii) * p03(i) +
                s(8, ii) * p04(i) + s(12, ii) * p05(i) + s(17, ii) * p06(i) +
                s(23, ii) * p07(i) + s(30, ii) * p08(i) + s(38, ii) * p09(i) +
                s(47, ii) * p10(i) + s(57, ii) * p11_31(i) + s(68, ii) * p12_31(i) +
                s(80, ii) * p13_31(i) + s(93, ii) * p14_31(i) + s(107, ii) * p15_31(i) +
                s(122, ii) * p16_31(i) + s(138, ii) * p17_31(i) + s(155, ii) * p18_31(i) +
                s(173, ii) * p19(i) + s(192, ii) * p20(i) + s(212, ii) * p21_31(i) +
                s(233, ii) * p22_31(i) + s(255, ii) * p23_31(i) + s(278, ii) * p24_31(i)
        Next

        For i = lft To llt
                ii = kstore + i - 1
                ap03(i) = s(4, ii) * p01(i) + s(5, ii) * p02(i) + s(6, ii) * p03(i) +
                s(9, ii) * p04(i) + s(13, ii) * p05(i) + s(18, ii) * p06(i) +
                s(24, ii) * p07(i) + s(31, ii) * p08(i) + s(39, ii) * p09(i) +
                s(48, ii) * p10(i) + s(58, ii) * p11_31(i) + s(69, ii) * p12_31(i) +
                s(81, ii) * p13_31(i) + s(94, ii) * p14_31(i) + s(108, ii) * p15_31(i) +
                s(123, ii) * p16_31(i) + s(139, ii) * p17_31(i) + s(156, ii) * p18_31(i) +
                s(174, ii) * p19(i) + s(193, ii) * p20(i) + s(213, ii) * p21_31(i) +
                s(234, ii) * p22_31(i) + s(256, ii) * p23_31(i) + s(279, ii) * p24_31(i)
            Next

        For i = lft To llt
            ii = kstore + i - 1
            ap04(i) = s(7, ii) * p01(i) + s(8, ii) * p02(i) + s(9, ii) * p03(i) +
                s(10, ii) * p04(i) + s(14, ii) * p05(i) + s(19, ii) * p06(i) +
                s(25, ii) * p07(i) + s(32, ii) * p08(i) + s(40, ii) * p09(i) +
                s(49, ii) * p10(i) + s(59, ii) * p11_31(i) + s(70, ii) * p12_31(i) +
                s(82, ii) * p13_31(i) + s(95, ii) * p14_31(i) + s(109, ii) * p15_31(i) +
                s(124, ii) * p16_31(i) + s(140, ii) * p17_31(i) + s(157, ii) * p18_31(i) +
                s(175, ii) * p19(i) + s(194, ii) * p20(i) + s(214, ii) * p21_31(i) +
                s(235, ii) * p22_31(i) + s(257, ii) * p23_31(i) + s(280, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap05(i) = s(11, ii) * p01(i) + s(12, ii) * p02(i) + s(13, ii) * p03(i) +
                s(14, ii) * p04(i) + s(15, ii) * p05(i) + s(20, ii) * p06(i) +
                s(26, ii) * p07(i) + s(33, ii) * p08(i) + s(41, ii) * p09(i) +
                s(50, ii) * p10(i) + s(60, ii) * p11_31(i) + s(71, ii) * p12_31(i) +
                s(83, ii) * p13_31(i) + s(96, ii) * p14_31(i) + s(110, ii) * p15_31(i) +
                s(125, ii) * p16_31(i) + s(141, ii) * p17_31(i) + s(158, ii) * p18_31(i) +
                s(176, ii) * p19(i) + s(195, ii) * p20(i) + s(215, ii) * p21_31(i) +
                s(236, ii) * p22_31(i) + s(258, ii) * p23_31(i) + s(281, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap06(i) = s(16, ii) * p01(i) + s(17, ii) * p02(i) + s(18, ii) * p03(i) +
                s(19, ii) * p04(i) + s(20, ii) * p05(i) + s(21, ii) * p06(i) +
                s(27, ii) * p07(i) + s(34, ii) * p08(i) + s(42, ii) * p09(i) +
                s(51, ii) * p10(i) + s(61, ii) * p11_31(i) + s(72, ii) * p12_31(i) +
                s(84, ii) * p13_31(i) + s(97, ii) * p14_31(i) + s(111, ii) * p15_31(i) +
                s(126, ii) * p16_31(i) + s(142, ii) * p17_31(i) + s(159, ii) * p18_31(i) +
                s(177, ii) * p19(i) + s(196, ii) * p20(i) + s(216, ii) * p21_31(i) +
                s(237, ii) * p22_31(i) + s(259, ii) * p23_31(i) + s(282, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap07(i) = s(22, ii) * p01(i) + s(23, ii) * p02(i) + s(24, ii) * p03(i) +
                s(25, ii) * p04(i) + s(26, ii) * p05(i) + s(27, ii) * p06(i) +
                s(28, ii) * p07(i) + s(35, ii) * p08(i) + s(43, ii) * p09(i) +
                s(52, ii) * p10(i) + s(62, ii) * p11_31(i) + s(73, ii) * p12_31(i) +
                s(85, ii) * p13_31(i) + s(98, ii) * p14_31(i) + s(112, ii) * p15_31(i) +
                s(127, ii) * p16_31(i) + s(143, ii) * p17_31(i) + s(160, ii) * p18_31(i) +
                s(178, ii) * p19(i) + s(197, ii) * p20(i) + s(217, ii) * p21_31(i) +
                s(238, ii) * p22_31(i) + s(260, ii) * p23_31(i) + s(283, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap08(i) = s(29, ii) * p01(i) + s(30, ii) * p02(i) + s(31, ii) * p03(i) +
                s(32, ii) * p04(i) + s(33, ii) * p05(i) + s(34, ii) * p06(i) +
                s(35, ii) * p07(i) + s(36, ii) * p08(i) + s(44, ii) * p09(i) +
                s(53, ii) * p10(i) + s(63, ii) * p11_31(i) + s(74, ii) * p12_31(i) +
                s(86, ii) * p13_31(i) + s(99, ii) * p14_31(i) + s(113, ii) * p15_31(i) +
                s(128, ii) * p16_31(i) + s(144, ii) * p17_31(i) + s(161, ii) * p18_31(i) +
                s(179, ii) * p19(i) + s(198, ii) * p20(i) + s(218, ii) * p21_31(i) +
                s(239, ii) * p22_31(i) + s(261, ii) * p23_31(i) + s(284, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap09(i) = s(37, ii) * p01(i) + s(38, ii) * p02(i) + s(39, ii) * p03(i) +
                s(40, ii) * p04(i) + s(41, ii) * p05(i) + s(42, ii) * p06(i) +
                s(43, ii) * p07(i) + s(44, ii) * p08(i) + s(45, ii) * p09(i) +
                s(54, ii) * p10(i) + s(64, ii) * p11_31(i) + s(75, ii) * p12_31(i) +
                s(87, ii) * p13_31(i) + s(100, ii) * p14_31(i) + s(114, ii) * p15_31(i) +
                s(129, ii) * p16_31(i) + s(145, ii) * p17_31(i) + s(162, ii) * p18_31(i) +
                s(180, ii) * p19(i) + s(199, ii) * p20(i) + s(219, ii) * p21_31(i) +
                s(240, ii) * p22_31(i) + s(262, ii) * p23_31(i) + s(285, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap10(i) = s(46, ii) * p01(i) + s(47, ii) * p02(i) + s(48, ii) * p03(i) +
                s(49, ii) * p04(i) + s(50, ii) * p05(i) + s(51, ii) * p06(i) +
                s(52, ii) * p07(i) + s(53, ii) * p08(i) + s(54, ii) * p09(i) +
                s(55, ii) * p10(i) + s(65, ii) * p11_31(i) + s(76, ii) * p12_31(i) +
                s(88, ii) * p13_31(i) + s(101, ii) * p14_31(i) + s(115, ii) * p15_31(i) +
                s(130, ii) * p16_31(i) + s(146, ii) * p17_31(i) + s(163, ii) * p18_31(i) +
                s(181, ii) * p19(i) + s(200, ii) * p20(i) + s(220, ii) * p21_31(i) +
                s(241, ii) * p22_31(i) + s(263, ii) * p23_31(i) + s(286, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap11(i) = s(56, ii) * p01(i) + s(57, ii) * p02(i) + s(58, ii) * p03(i) +
                s(59, ii) * p04(i) + s(60, ii) * p05(i) + s(61, ii) * p06(i) +
                s(62, ii) * p07(i) + s(63, ii) * p08(i) + s(64, ii) * p09(i) +
                s(65, ii) * p10(i) + s(66, ii) * p11_31(i) + s(77, ii) * p12_31(i) +
                s(89, ii) * p13_31(i) + s(102, ii) * p14_31(i) + s(116, ii) * p15_31(i) +
                s(131, ii) * p16_31(i) + s(147, ii) * p17_31(i) + s(164, ii) * p18_31(i) +
                s(182, ii) * p19(i) + s(201, ii) * p20(i) + s(221, ii) * p21_31(i) +
                s(242, ii) * p22_31(i) + s(264, ii) * p23_31(i) + s(287, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap12(i) = s(67, ii) * p01(i) + s(68, ii) * p02(i) + s(69, ii) * p03(i) +
                s(70, ii) * p04(i) + s(71, ii) * p05(i) + s(72, ii) * p06(i) +
                s(73, ii) * p07(i) + s(74, ii) * p08(i) + s(75, ii) * p09(i) +
                s(76, ii) * p10(i) + s(77, ii) * p11_31(i) + s(78, ii) * p12_31(i) +
                s(90, ii) * p13_31(i) + s(103, ii) * p14_31(i) + s(117, ii) * p15_31(i) +
                s(132, ii) * p16_31(i) + s(148, ii) * p17_31(i) + s(165, ii) * p18_31(i) +
                s(183, ii) * p19(i) + s(202, ii) * p20(i) + s(222, ii) * p21_31(i) +
                s(243, ii) * p22_31(i) + s(265, ii) * p23_31(i) + s(288, ii) * p24_31(i)
        Next
        For i = lft To llt
            ii = kstore + i - 1
            ap13(i) = s(79, ii) * p01(i) + s(80, ii) * p02(i) + s(81, ii) * p03(i) +
                s(82, ii) * p04(i) + s(83, ii) * p05(i) + s(84, ii) * p06(i) +
                s(85, ii) * p07(i) + s(86, ii) * p08(i) + s(87, ii) * p09(i) +
                s(88, ii) * p10(i) + s(89, ii) * p11_31(i) + s(90, ii) * p12_31(i) +
                s(91, ii) * p13_31(i) + s(104, ii) * p14_31(i) + s(118, ii) * p15_31(i) +
                s(133, ii) * p16_31(i) + s(149, ii) * p17_31(i) + s(166, ii) * p18_31(i) +
                s(184, ii) * p19(i) + s(203, ii) * p20(i) + s(223, ii) * p21_31(i) +
                s(244, ii) * p22_31(i) + s(266, ii) * p23_31(i) + s(289, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap14(i) = s(92, ii) * p01(i) + s(93, ii) * p02(i) + s(94, ii) * p03(i) +
                s(95, ii) * p04(i) + s(96, ii) * p05(i) + s(97, ii) * p06(i) +
                s(98, ii) * p07(i) + s(99, ii) * p08(i) + s(100, ii) * p09(i) +
                s(101, ii) * p10(i) + s(102, ii) * p11_31(i) + s(103, ii) * p12_31(i) +
                s(104, ii) * p13_31(i) + s(105, ii) * p14_31(i) + s(119, ii) * p15_31(i) +
                s(134, ii) * p16_31(i) + s(150, ii) * p17_31(i) + s(167, ii) * p18_31(i) +
                s(185, ii) * p19(i) + s(204, ii) * p20(i) + s(224, ii) * p21_31(i) +
                s(245, ii) * p22_31(i) + s(267, ii) * p23_31(i) + s(290, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap15(i) = s(106, ii) * p01(i) + s(107, ii) * p02(i) + s(108, ii) * p03(i) +
                s(109, ii) * p04(i) + s(110, ii) * p05(i) + s(111, ii) * p06(i) +
                s(112, ii) * p07(i) + s(113, ii) * p08(i) + s(114, ii) * p09(i) +
                s(115, ii) * p10(i) + s(116, ii) * p11_31(i) + s(117, ii) * p12_31(i) +
                s(118, ii) * p13_31(i) + s(119, ii) * p14_31(i) + s(120, ii) * p15_31(i) +
                s(135, ii) * p16_31(i) + s(151, ii) * p17_31(i) + s(168, ii) * p18_31(i) +
                s(186, ii) * p19(i) + s(205, ii) * p20(i) + s(225, ii) * p21_31(i) +
                s(246, ii) * p22_31(i) + s(268, ii) * p23_31(i) + s(291, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap16(i) = s(121, ii) * p01(i) + s(122, ii) * p02(i) + s(123, ii) * p03(i) +
                s(124, ii) * p04(i) + s(125, ii) * p05(i) + s(126, ii) * p06(i) +
                s(127, ii) * p07(i) + s(128, ii) * p08(i) + s(129, ii) * p09(i) +
                s(130, ii) * p10(i) + s(131, ii) * p11_31(i) + s(132, ii) * p12_31(i) +
                s(133, ii) * p13_31(i) + s(134, ii) * p14_31(i) + s(135, ii) * p15_31(i) +
                s(136, ii) * p16_31(i) + s(152, ii) * p17_31(i) + s(169, ii) * p18_31(i) +
                s(187, ii) * p19(i) + s(206, ii) * p20(i) + s(226, ii) * p21_31(i) +
                s(247, ii) * p22_31(i) + s(269, ii) * p23_31(i) + s(292, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap17(i) = s(137, ii) * p01(i) + s(138, ii) * p02(i) + s(139, ii) * p03(i) +
                s(140, ii) * p04(i) + s(141, ii) * p05(i) + s(142, ii) * p06(i) +
                s(143, ii) * p07(i) + s(144, ii) * p08(i) + s(145, ii) * p09(i) +
                s(146, ii) * p10(i) + s(147, ii) * p11_31(i) + s(148, ii) * p12_31(i) +
                s(149, ii) * p13_31(i) + s(150, ii) * p14_31(i) + s(151, ii) * p15_31(i) +
                s(152, ii) * p16_31(i) + s(153, ii) * p17_31(i) + s(170, ii) * p18_31(i) +
                s(188, ii) * p19(i) + s(207, ii) * p20(i) + s(227, ii) * p21_31(i) +
                s(248, ii) * p22_31(i) + s(270, ii) * p23_31(i) + s(293, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap18(i) = s(154, ii) * p01(i) + s(155, ii) * p02(i) + s(156, ii) * p03(i) +
                s(157, ii) * p04(i) + s(158, ii) * p05(i) + s(159, ii) * p06(i) +
                s(160, ii) * p07(i) + s(161, ii) * p08(i) + s(162, ii) * p09(i) +
                s(163, ii) * p10(i) + s(164, ii) * p11_31(i) + s(165, ii) * p12_31(i) +
                s(166, ii) * p13_31(i) + s(167, ii) * p14_31(i) + s(168, ii) * p15_31(i) +
                s(169, ii) * p16_31(i) + s(170, ii) * p17_31(i) + s(171, ii) * p18_31(i) +
                s(189, ii) * p19(i) + s(208, ii) * p20(i) + s(228, ii) * p21_31(i) +
                s(249, ii) * p22_31(i) + s(271, ii) * p23_31(i) + s(294, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap19(i) = s(172, ii) * p01(i) + s(173, ii) * p02(i) + s(174, ii) * p03(i) +
                s(175, ii) * p04(i) + s(176, ii) * p05(i) + s(177, ii) * p06(i) +
                s(178, ii) * p07(i) + s(179, ii) * p08(i) + s(180, ii) * p09(i) +
                s(181, ii) * p10(i) + s(182, ii) * p11_31(i) + s(183, ii) * p12_31(i) +
                s(184, ii) * p13_31(i) + s(185, ii) * p14_31(i) + s(186, ii) * p15_31(i) +
                s(187, ii) * p16_31(i) + s(188, ii) * p17_31(i) + s(189, ii) * p18_31(i) +
                s(190, ii) * p19(i) + s(209, ii) * p20(i) + s(229, ii) * p21_31(i) +
                s(250, ii) * p22_31(i) + s(272, ii) * p23_31(i) + s(295, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap20(i) = s(191, ii) * p01(i) + s(192, ii) * p02(i) + s(193, ii) * p03(i) +
                s(194, ii) * p04(i) + s(195, ii) * p05(i) + s(196, ii) * p06(i) +
                s(197, ii) * p07(i) + s(198, ii) * p08(i) + s(199, ii) * p09(i) +
                s(200, ii) * p10(i) + s(201, ii) * p11_31(i) + s(202, ii) * p12_31(i) +
                s(203, ii) * p13_31(i) + s(204, ii) * p14_31(i) + s(205, ii) * p15_31(i) +
                s(206, ii) * p16_31(i) + s(207, ii) * p17_31(i) + s(208, ii) * p18_31(i) +
                s(209, ii) * p19(i) + s(210, ii) * p20(i) + s(230, ii) * p21_31(i) +
                s(251, ii) * p22_31(i) + s(273, ii) * p23_31(i) + s(296, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap21(i) = s(211, ii) * p01(i) + s(212, ii) * p02(i) + s(213, ii) * p03(i) +
                s(214, ii) * p04(i) + s(215, ii) * p05(i) + s(216, ii) * p06(i) +
                s(217, ii) * p07(i) + s(218, ii) * p08(i) + s(219, ii) * p09(i) +
                s(220, ii) * p10(i) + s(221, ii) * p11_31(i) + s(222, ii) * p12_31(i) +
                s(223, ii) * p13_31(i) + s(224, ii) * p14_31(i) + s(225, ii) * p15_31(i) +
                s(226, ii) * p16_31(i) + s(227, ii) * p17_31(i) + s(228, ii) * p18_31(i) +
                s(229, ii) * p19(i) + s(230, ii) * p20(i) + s(231, ii) * p21_31(i) +
                s(252, ii) * p22_31(i) + s(274, ii) * p23_31(i) + s(297, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap22(i) = s(232, ii) * p01(i) + s(233, ii) * p02(i) + s(234, ii) * p03(i) +
                s(235, ii) * p04(i) + s(236, ii) * p05(i) + s(237, ii) * p06(i) +
                s(238, ii) * p07(i) + s(239, ii) * p08(i) + s(240, ii) * p09(i) +
                s(241, ii) * p10(i) + s(242, ii) * p11_31(i) + s(243, ii) * p12_31(i) +
                s(244, ii) * p13_31(i) + s(245, ii) * p14_31(i) + s(246, ii) * p15_31(i) +
                s(247, ii) * p16_31(i) + s(248, ii) * p17_31(i) + s(249, ii) * p18_31(i) +
                s(250, ii) * p19(i) + s(251, ii) * p20(i) + s(252, ii) * p21_31(i) +
                s(253, ii) * p22_31(i) + s(275, ii) * p23_31(i) + s(298, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap23(i) = s(254, ii) * p01(i) + s(255, ii) * p02(i) + s(256, ii) * p03(i) +
                s(257, ii) * p04(i) + s(258, ii) * p05(i) + s(259, ii) * p06(i) +
                s(260, ii) * p07(i) + s(261, ii) * p08(i) + s(262, ii) * p09(i) +
                s(263, ii) * p10(i) + s(264, ii) * p11_31(i) + s(265, ii) * p12_31(i) +
                s(266, ii) * p13_31(i) + s(267, ii) * p14_31(i) + s(268, ii) * p15_31(i) +
                s(269, ii) * p16_31(i) + s(270, ii) * p17_31(i) + s(271, ii) * p18_31(i) +
                s(272, ii) * p19(i) + s(273, ii) * p20(i) + s(274, ii) * p21_31(i) +
                s(275, ii) * p22_31(i) + s(276, ii) * p23_31(i) + s(299, ii) * p24_31(i)
        Next

        For i = lft To llt
            ii = kstore + i - 1
            ap24(i) = s(277, ii) * p01(i) + s(278, ii) * p02(i) + s(279, ii) * p03(i) +
                s(280, ii) * p04(i) + s(281, ii) * p05(i) + s(282, ii) * p06(i) +
                s(283, ii) * p07(i) + s(284, ii) * p08(i) + s(285, ii) * p09(i) +
                s(286, ii) * p10(i) + s(287, ii) * p11_31(i) + s(288, ii) * p12_31(i) +
                s(289, ii) * p13_31(i) + s(290, ii) * p14_31(i) + s(291, ii) * p15_31(i) +
                s(292, ii) * p16_31(i) + s(293, ii) * p17_31(i) + s(294, ii) * p18_31(i) +
                s(295, ii) * p19(i) + s(296, ii) * p20(i) + s(297, ii) * p21_31(i) +
                s(298, ii) * p22_31(i) + s(299, ii) * p23_31(i) + s(300, ii) * p24_31(i)
        Next
        ' YC 102418 END

        'c.... scatter operations                                                

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
            'ap(lm(15, i)) = ap(lm(15, i)) + ap16(i)
            'ap(lm(16, i)) = ap(lm(16, i)) + ap17(i)
            'ap(lm(17, i)) = ap(lm(17, i)) + ap18(i)
            'ap(lm(18, i)) = ap(lm(18, i)) + ap19(i)
            'ap(lm(19, i)) = ap(lm(19, i)) + ap20(i)
            'ap(lm(20, i)) = ap(lm(20, i)) + ap21(i)
            'ap(lm(21, i)) = ap(lm(21, i)) + ap22(i)
            'ap(lm(22, i)) = ap(lm(22, i)) + ap23(i)
            'ap(lm(23, i)) = ap(lm(23, i)) + ap24(i)
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
                ap(lm(16, ii)) = ap(lm(16, ii)) + ap16(i)
                ap(lm(17, ii)) = ap(lm(17, ii)) + ap17(i)
                ap(lm(18, ii)) = ap(lm(18, ii)) + ap18(i)
                ap(lm(19, ii)) = ap(lm(19, ii)) + ap19(i)
                ap(lm(20, ii)) = ap(lm(20, ii)) + ap20(i)
                ap(lm(21, ii)) = ap(lm(21, ii)) + ap21(i)
                ap(lm(22, ii)) = ap(lm(22, ii)) + ap22(i)
                ap(lm(23, ii)) = ap(lm(23, ii)) + ap23(i)
                ap(lm(24, ii)) = ap(lm(24, ii)) + ap24(i)
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
            'ap(lm(15, i)) = ap(lm(15, i)) + ap16(i)
            'ap(lm(16, i)) = ap(lm(16, i)) + ap17(i)
            'ap(lm(17, i)) = ap(lm(17, i)) + ap18(i)
            'ap(lm(18, i)) = ap(lm(18, i)) + ap19(i)
            'ap(lm(19, i)) = ap(lm(19, i)) + ap20(i)
            'ap(lm(20, i)) = ap(lm(20, i)) + ap21(i)
            'ap(lm(21, i)) = ap(lm(21, i)) + ap22(i)
            'ap(lm(22, i)) = ap(lm(22, i)) + ap23(i)
            'ap(lm(23, i)) = ap(lm(23, i)) + ap24(i)
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
                ap(lm(16, ii)) = ap(lm(16, ii)) + ap16(i)
                ap(lm(17, ii)) = ap(lm(17, ii)) + ap17(i)
                ap(lm(18, ii)) = ap(lm(18, ii)) + ap18(i)
                ap(lm(19, ii)) = ap(lm(19, ii)) + ap19(i)
                ap(lm(20, ii)) = ap(lm(20, ii)) + ap20(i)
                ap(lm(21, ii)) = ap(lm(21, ii)) + ap21(i)
                ap(lm(22, ii)) = ap(lm(22, ii)) + ap22(i)
                ap(lm(23, ii)) = ap(lm(23, ii)) + ap23(i)
                ap(lm(24, ii)) = ap(lm(24, ii)) + ap24(i)
                ' YC 102418 END

            Next
        End If
        'Call Check1D(ap, neql)
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine matv24 (lft,llt,s,lm,ap,p)                             
'c
'
'      implicit double precision (a-h,o-z)                          
'
'c                                                                       
'c===> module for matrix-vector product for element with 24 dof          
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
'      dimension s(300,*),lm(24*iprec,*),ap(*),p(*)                      
'c NKC 9/14/99
'c      write(6,*) 'entering matv24.F'
'
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
'         p16(i) = p(lm(16,i))                                           
'         p17(i) = p(lm(17,i))                                           
'         p18(i) = p(lm(18,i))                                           
'         p19(i) = p(lm(19,i))                                           
'         p20(i) = p(lm(20,i))                                           
'         p21(i) = p(lm(21,i))                                           
'         p22(i) = p(lm(22,i))                                           
'         p23(i) = p(lm(23,i))                                           
'         p24(i) = p(lm(24,i))                                           
'  100 continue                                                          
'c                                                                       
'c.... do-loops broken to allow optimization                             
'c                                                                       
'      do 200 i=lft,llt                                                  
'
'        ap01(i) = s(  1,i)*p01(i) + s(  2,i)*p02(i) + s(  4,i)*p03(i)   
'     1          + s(  7,i)*p04(i) + s( 11,i)*p05(i) + s( 16,i)*p06(i)   
'     2          + s( 22,i)*p07(i) + s( 29,i)*p08(i) + s( 37,i)*p09(i)   
'     3          + s( 46,i)*p10(i) + s( 56,i)*p11(i) + s( 67,i)*p12(i)   
'     4          + s( 79,i)*p13(i) + s( 92,i)*p14(i) + s(106,i)*p15(i)   
'     5          + s(121,i)*p16(i) + s(137,i)*p17(i) + s(154,i)*p18(i)   
'     6          + s(172,i)*p19(i) + s(191,i)*p20(i) + s(211,i)*p21(i)   
'     7          + s(232,i)*p22(i) + s(254,i)*p23(i) + s(277,i)*p24(i)   
'  200 continue                                                          
'c                                                                       
'      do 201 i=lft,llt                                                  
'        ap02(i) = s(  2,i)*p01(i) + s(  3,i)*p02(i) + s(  5,i)*p03(i)   
'     1          + s(  8,i)*p04(i) + s( 12,i)*p05(i) + s( 17,i)*p06(i)   
'     2          + s( 23,i)*p07(i) + s( 30,i)*p08(i) + s( 38,i)*p09(i)   
'     3          + s( 47,i)*p10(i) + s( 57,i)*p11(i) + s( 68,i)*p12(i)   
'     4          + s( 80,i)*p13(i) + s( 93,i)*p14(i) + s(107,i)*p15(i)   
'     5          + s(122,i)*p16(i) + s(138,i)*p17(i) + s(155,i)*p18(i)   
'     6          + s(173,i)*p19(i) + s(192,i)*p20(i) + s(212,i)*p21(i)   
'     7          + s(233,i)*p22(i) + s(255,i)*p23(i) + s(278,i)*p24(i)   
'  201 continue                                                          
'c                                                                       
'      do 202 i=lft,llt                                                  
'        ap03(i) = s(  4,i)*p01(i) + s(  5,i)*p02(i) + s(  6,i)*p03(i)   
'     1          + s(  9,i)*p04(i) + s( 13,i)*p05(i) + s( 18,i)*p06(i)   
'     2          + s( 24,i)*p07(i) + s( 31,i)*p08(i) + s( 39,i)*p09(i)   
'     3          + s( 48,i)*p10(i) + s( 58,i)*p11(i) + s( 69,i)*p12(i)   
'     4          + s( 81,i)*p13(i) + s( 94,i)*p14(i) + s(108,i)*p15(i)   
'     5          + s(123,i)*p16(i) + s(139,i)*p17(i) + s(156,i)*p18(i)   
'     6          + s(174,i)*p19(i) + s(193,i)*p20(i) + s(213,i)*p21(i)   
'     7          + s(234,i)*p22(i) + s(256,i)*p23(i) + s(279,i)*p24(i)   
'  202 continue                                                          
'c                                                                       
'      do 203 i=lft,llt                                                  
'        ap04(i) = s(  7,i)*p01(i) + s(  8,i)*p02(i) + s(  9,i)*p03(i)   
'     1          + s( 10,i)*p04(i) + s( 14,i)*p05(i) + s( 19,i)*p06(i)   
'     2          + s( 25,i)*p07(i) + s( 32,i)*p08(i) + s( 40,i)*p09(i)   
'     3          + s( 49,i)*p10(i) + s( 59,i)*p11(i) + s( 70,i)*p12(i)   
'     4          + s( 82,i)*p13(i) + s( 95,i)*p14(i) + s(109,i)*p15(i)   
'     5          + s(124,i)*p16(i) + s(140,i)*p17(i) + s(157,i)*p18(i)   
'     6          + s(175,i)*p19(i) + s(194,i)*p20(i) + s(214,i)*p21(i)   
'     7          + s(235,i)*p22(i) + s(257,i)*p23(i) + s(280,i)*p24(i)   
'  203 continue                                                          
'c                                                                       
'      do 204 i=lft,llt                                                  
'        ap05(i) = s( 11,i)*p01(i) + s( 12,i)*p02(i) + s( 13,i)*p03(i)   
'     1          + s( 14,i)*p04(i) + s( 15,i)*p05(i) + s( 20,i)*p06(i)   
'     2          + s( 26,i)*p07(i) + s( 33,i)*p08(i) + s( 41,i)*p09(i)   
'     3          + s( 50,i)*p10(i) + s( 60,i)*p11(i) + s( 71,i)*p12(i)   
'     4          + s( 83,i)*p13(i) + s( 96,i)*p14(i) + s(110,i)*p15(i)   
'     5          + s(125,i)*p16(i) + s(141,i)*p17(i) + s(158,i)*p18(i)   
'     6          + s(176,i)*p19(i) + s(195,i)*p20(i) + s(215,i)*p21(i)   
'     7          + s(236,i)*p22(i) + s(258,i)*p23(i) + s(281,i)*p24(i)   
'  204 continue                                                          
'c                                                                       
'      do 205 i=lft,llt                                                  
'        ap06(i) = s( 16,i)*p01(i) + s( 17,i)*p02(i) + s( 18,i)*p03(i)   
'     1          + s( 19,i)*p04(i) + s( 20,i)*p05(i) + s( 21,i)*p06(i)   
'     2          + s( 27,i)*p07(i) + s( 34,i)*p08(i) + s( 42,i)*p09(i)   
'     3          + s( 51,i)*p10(i) + s( 61,i)*p11(i) + s( 72,i)*p12(i)   
'     4          + s( 84,i)*p13(i) + s( 97,i)*p14(i) + s(111,i)*p15(i)   
'     5          + s(126,i)*p16(i) + s(142,i)*p17(i) + s(159,i)*p18(i)   
'     6          + s(177,i)*p19(i) + s(196,i)*p20(i) + s(216,i)*p21(i)   
'     7          + s(237,i)*p22(i) + s(259,i)*p23(i) + s(282,i)*p24(i)   
'  205 continue                                                          
'c                                                                       
'      do 206 i=lft,llt                                                  
'        ap07(i) = s( 22,i)*p01(i) + s( 23,i)*p02(i) + s( 24,i)*p03(i)   
'     1          + s( 25,i)*p04(i) + s( 26,i)*p05(i) + s( 27,i)*p06(i)   
'     2          + s( 28,i)*p07(i) + s( 35,i)*p08(i) + s( 43,i)*p09(i)   
'     3          + s( 52,i)*p10(i) + s( 62,i)*p11(i) + s( 73,i)*p12(i)   
'     4          + s( 85,i)*p13(i) + s( 98,i)*p14(i) + s(112,i)*p15(i)   
'     5          + s(127,i)*p16(i) + s(143,i)*p17(i) + s(160,i)*p18(i)   
'     6          + s(178,i)*p19(i) + s(197,i)*p20(i) + s(217,i)*p21(i)   
'     7          + s(238,i)*p22(i) + s(260,i)*p23(i) + s(283,i)*p24(i)   
'  206 continue                                                          
'c                                                                       
'      do 207 i=lft,llt                                                  
'        ap08(i) = s( 29,i)*p01(i) + s( 30,i)*p02(i) + s( 31,i)*p03(i)   
'     1          + s( 32,i)*p04(i) + s( 33,i)*p05(i) + s( 34,i)*p06(i)   
'     2          + s( 35,i)*p07(i) + s( 36,i)*p08(i) + s( 44,i)*p09(i)   
'     3          + s( 53,i)*p10(i) + s( 63,i)*p11(i) + s( 74,i)*p12(i)   
'     4          + s( 86,i)*p13(i) + s( 99,i)*p14(i) + s(113,i)*p15(i)   
'     5          + s(128,i)*p16(i) + s(144,i)*p17(i) + s(161,i)*p18(i)   
'     6          + s(179,i)*p19(i) + s(198,i)*p20(i) + s(218,i)*p21(i)   
'     7          + s(239,i)*p22(i) + s(261,i)*p23(i) + s(284,i)*p24(i)   
'  207 continue                                                          
'c                                                                       
'      do 208 i=lft,llt                                                  
'        ap09(i) = s( 37,i)*p01(i) + s( 38,i)*p02(i) + s( 39,i)*p03(i)   
'     1          + s( 40,i)*p04(i) + s( 41,i)*p05(i) + s( 42,i)*p06(i)   
'     2          + s( 43,i)*p07(i) + s( 44,i)*p08(i) + s( 45,i)*p09(i)   
'     3          + s( 54,i)*p10(i) + s( 64,i)*p11(i) + s( 75,i)*p12(i)   
'     4          + s( 87,i)*p13(i) + s(100,i)*p14(i) + s(114,i)*p15(i)   
'     5          + s(129,i)*p16(i) + s(145,i)*p17(i) + s(162,i)*p18(i)   
'     6          + s(180,i)*p19(i) + s(199,i)*p20(i) + s(219,i)*p21(i)   
'     7          + s(240,i)*p22(i) + s(262,i)*p23(i) + s(285,i)*p24(i)   
'  208 continue                                                          
'c                                                                       
'      do 209 i=lft,llt                                                  
'        ap10(i) = s( 46,i)*p01(i) + s( 47,i)*p02(i) + s( 48,i)*p03(i)   
'     1          + s( 49,i)*p04(i) + s( 50,i)*p05(i) + s( 51,i)*p06(i)   
'     2          + s( 52,i)*p07(i) + s( 53,i)*p08(i) + s( 54,i)*p09(i)   
'     3          + s( 55,i)*p10(i) + s( 65,i)*p11(i) + s( 76,i)*p12(i)   
'     4          + s( 88,i)*p13(i) + s(101,i)*p14(i) + s(115,i)*p15(i)   
'     5          + s(130,i)*p16(i) + s(146,i)*p17(i) + s(163,i)*p18(i)   
'     6          + s(181,i)*p19(i) + s(200,i)*p20(i) + s(220,i)*p21(i)   
'     7          + s(241,i)*p22(i) + s(263,i)*p23(i) + s(286,i)*p24(i)   
'  209 continue                                                          
'c                                                                       
'      do 210 i=lft,llt                                                  
'        ap11(i) = s( 56,i)*p01(i) + s( 57,i)*p02(i) + s( 58,i)*p03(i)   
'     1          + s( 59,i)*p04(i) + s( 60,i)*p05(i) + s( 61,i)*p06(i)   
'     2          + s( 62,i)*p07(i) + s( 63,i)*p08(i) + s( 64,i)*p09(i)   
'     3          + s( 65,i)*p10(i) + s( 66,i)*p11(i) + s( 77,i)*p12(i)   
'     4          + s( 89,i)*p13(i) + s(102,i)*p14(i) + s(116,i)*p15(i)   
'     5          + s(131,i)*p16(i) + s(147,i)*p17(i) + s(164,i)*p18(i)   
'     6          + s(182,i)*p19(i) + s(201,i)*p20(i) + s(221,i)*p21(i)   
'     7          + s(242,i)*p22(i) + s(264,i)*p23(i) + s(287,i)*p24(i)   
'  210 continue                                                          
'c                                                                       
'      do 211 i=lft,llt                                                  
'        ap12(i) = s( 67,i)*p01(i) + s( 68,i)*p02(i) + s( 69,i)*p03(i)   
'     1          + s( 70,i)*p04(i) + s( 71,i)*p05(i) + s( 72,i)*p06(i)   
'     2          + s( 73,i)*p07(i) + s( 74,i)*p08(i) + s( 75,i)*p09(i)   
'     3          + s( 76,i)*p10(i) + s( 77,i)*p11(i) + s( 78,i)*p12(i)   
'     4          + s( 90,i)*p13(i) + s(103,i)*p14(i) + s(117,i)*p15(i)   
'     5          + s(132,i)*p16(i) + s(148,i)*p17(i) + s(165,i)*p18(i)   
'     6          + s(183,i)*p19(i) + s(202,i)*p20(i) + s(222,i)*p21(i)   
'     7          + s(243,i)*p22(i) + s(265,i)*p23(i) + s(288,i)*p24(i)   
'  211 continue                                                          
'c                                                                       
'      do 212 i=lft,llt                                                  
'        ap13(i) = s( 79,i)*p01(i) + s( 80,i)*p02(i) + s( 81,i)*p03(i)   
'     1          + s( 82,i)*p04(i) + s( 83,i)*p05(i) + s( 84,i)*p06(i)   
'     2          + s( 85,i)*p07(i) + s( 86,i)*p08(i) + s( 87,i)*p09(i)   
'     3          + s( 88,i)*p10(i) + s( 89,i)*p11(i) + s( 90,i)*p12(i)   
'     4          + s( 91,i)*p13(i) + s(104,i)*p14(i) + s(118,i)*p15(i)   
'     5          + s(133,i)*p16(i) + s(149,i)*p17(i) + s(166,i)*p18(i)   
'     6          + s(184,i)*p19(i) + s(203,i)*p20(i) + s(223,i)*p21(i)   
'     7          + s(244,i)*p22(i) + s(266,i)*p23(i) + s(289,i)*p24(i)   
'  212 continue                                                          
'c                                                                       
'      do 213 i=lft,llt                                                  
'        ap14(i) = s( 92,i)*p01(i) + s( 93,i)*p02(i) + s( 94,i)*p03(i)   
'     1          + s( 95,i)*p04(i) + s( 96,i)*p05(i) + s( 97,i)*p06(i)   
'     2          + s( 98,i)*p07(i) + s( 99,i)*p08(i) + s(100,i)*p09(i)   
'     3          + s(101,i)*p10(i) + s(102,i)*p11(i) + s(103,i)*p12(i)   
'     4          + s(104,i)*p13(i) + s(105,i)*p14(i) + s(119,i)*p15(i)   
'     5          + s(134,i)*p16(i) + s(150,i)*p17(i) + s(167,i)*p18(i)   
'     6          + s(185,i)*p19(i) + s(204,i)*p20(i) + s(224,i)*p21(i)   
'     7          + s(245,i)*p22(i) + s(267,i)*p23(i) + s(290,i)*p24(i)   
'  213 continue                                                          
'c                                                                       
'      do 214 i=lft,llt                                                  
'        ap15(i) = s(106,i)*p01(i) + s(107,i)*p02(i) + s(108,i)*p03(i)   
'     1          + s(109,i)*p04(i) + s(110,i)*p05(i) + s(111,i)*p06(i)   
'     2          + s(112,i)*p07(i) + s(113,i)*p08(i) + s(114,i)*p09(i)   
'     3          + s(115,i)*p10(i) + s(116,i)*p11(i) + s(117,i)*p12(i)   
'     4          + s(118,i)*p13(i) + s(119,i)*p14(i) + s(120,i)*p15(i)   
'     5          + s(135,i)*p16(i) + s(151,i)*p17(i) + s(168,i)*p18(i)   
'     6          + s(186,i)*p19(i) + s(205,i)*p20(i) + s(225,i)*p21(i)   
'     7          + s(246,i)*p22(i) + s(268,i)*p23(i) + s(291,i)*p24(i)   
'  214 continue                                                          
'c                                                                       
'      do 215 i=lft,llt                                                  
'        ap16(i) = s(121,i)*p01(i) + s(122,i)*p02(i) + s(123,i)*p03(i)   
'     1          + s(124,i)*p04(i) + s(125,i)*p05(i) + s(126,i)*p06(i)   
'     2          + s(127,i)*p07(i) + s(128,i)*p08(i) + s(129,i)*p09(i)   
'     3          + s(130,i)*p10(i) + s(131,i)*p11(i) + s(132,i)*p12(i)   
'     4          + s(133,i)*p13(i) + s(134,i)*p14(i) + s(135,i)*p15(i)   
'     5          + s(136,i)*p16(i) + s(152,i)*p17(i) + s(169,i)*p18(i)   
'     6          + s(187,i)*p19(i) + s(206,i)*p20(i) + s(226,i)*p21(i)   
'     7          + s(247,i)*p22(i) + s(269,i)*p23(i) + s(292,i)*p24(i)   
'  215 continue                                                          
'c                                                                       
'      do 216 i=lft,llt                                                  
'        ap17(i) = s(137,i)*p01(i) + s(138,i)*p02(i) + s(139,i)*p03(i)   
'     1          + s(140,i)*p04(i) + s(141,i)*p05(i) + s(142,i)*p06(i)   
'     2          + s(143,i)*p07(i) + s(144,i)*p08(i) + s(145,i)*p09(i)   
'     3          + s(146,i)*p10(i) + s(147,i)*p11(i) + s(148,i)*p12(i)   
'     4          + s(149,i)*p13(i) + s(150,i)*p14(i) + s(151,i)*p15(i)   
'     5          + s(152,i)*p16(i) + s(153,i)*p17(i) + s(170,i)*p18(i)   
'     6          + s(188,i)*p19(i) + s(207,i)*p20(i) + s(227,i)*p21(i)   
'     7          + s(248,i)*p22(i) + s(270,i)*p23(i) + s(293,i)*p24(i)   
'  216 continue                                                          
'c                                                                       
'      do 217 i=lft,llt                                                  
'        ap18(i) = s(154,i)*p01(i) + s(155,i)*p02(i) + s(156,i)*p03(i)   
'     1          + s(157,i)*p04(i) + s(158,i)*p05(i) + s(159,i)*p06(i)   
'     2          + s(160,i)*p07(i) + s(161,i)*p08(i) + s(162,i)*p09(i)   
'     3          + s(163,i)*p10(i) + s(164,i)*p11(i) + s(165,i)*p12(i)   
'     4          + s(166,i)*p13(i) + s(167,i)*p14(i) + s(168,i)*p15(i)   
'     5          + s(169,i)*p16(i) + s(170,i)*p17(i) + s(171,i)*p18(i)   
'     6          + s(189,i)*p19(i) + s(208,i)*p20(i) + s(228,i)*p21(i)   
'     7          + s(249,i)*p22(i) + s(271,i)*p23(i) + s(294,i)*p24(i)   
'  217 continue                                                          
'c                                                                       
'      do 218 i=lft,llt                                                  
'        ap19(i) = s(172,i)*p01(i) + s(173,i)*p02(i) + s(174,i)*p03(i)   
'     1          + s(175,i)*p04(i) + s(176,i)*p05(i) + s(177,i)*p06(i)   
'     2          + s(178,i)*p07(i) + s(179,i)*p08(i) + s(180,i)*p09(i)   
'     3          + s(181,i)*p10(i) + s(182,i)*p11(i) + s(183,i)*p12(i)   
'     4          + s(184,i)*p13(i) + s(185,i)*p14(i) + s(186,i)*p15(i)   
'     5          + s(187,i)*p16(i) + s(188,i)*p17(i) + s(189,i)*p18(i)   
'     6          + s(190,i)*p19(i) + s(209,i)*p20(i) + s(229,i)*p21(i)   
'     7          + s(250,i)*p22(i) + s(272,i)*p23(i) + s(295,i)*p24(i)   
'  218 continue                                                          
'c                                                                       
'      do 219 i=lft,llt                                                  
'        ap20(i) = s(191,i)*p01(i) + s(192,i)*p02(i) + s(193,i)*p03(i)   
'     1          + s(194,i)*p04(i) + s(195,i)*p05(i) + s(196,i)*p06(i)   
'     2          + s(197,i)*p07(i) + s(198,i)*p08(i) + s(199,i)*p09(i)   
'     3          + s(200,i)*p10(i) + s(201,i)*p11(i) + s(202,i)*p12(i)   
'     4          + s(203,i)*p13(i) + s(204,i)*p14(i) + s(205,i)*p15(i)   
'     5          + s(206,i)*p16(i) + s(207,i)*p17(i) + s(208,i)*p18(i)   
'     6          + s(209,i)*p19(i) + s(210,i)*p20(i) + s(230,i)*p21(i)   
'     7          + s(251,i)*p22(i) + s(273,i)*p23(i) + s(296,i)*p24(i)   
'  219 continue                                                          
'c                                                                       
'      do 220 i=lft,llt                                                  
'        ap21(i) = s(211,i)*p01(i) + s(212,i)*p02(i) + s(213,i)*p03(i)   
'     1          + s(214,i)*p04(i) + s(215,i)*p05(i) + s(216,i)*p06(i)   
'     2          + s(217,i)*p07(i) + s(218,i)*p08(i) + s(219,i)*p09(i)   
'     3          + s(220,i)*p10(i) + s(221,i)*p11(i) + s(222,i)*p12(i)   
'     4          + s(223,i)*p13(i) + s(224,i)*p14(i) + s(225,i)*p15(i)   
'     5          + s(226,i)*p16(i) + s(227,i)*p17(i) + s(228,i)*p18(i)   
'     6          + s(229,i)*p19(i) + s(230,i)*p20(i) + s(231,i)*p21(i)   
'     7          + s(252,i)*p22(i) + s(274,i)*p23(i) + s(297,i)*p24(i)   
'  220 continue                                                          
'c                                                                       
'      do 221 i=lft,llt                                                  
'        ap22(i) = s(232,i)*p01(i) + s(233,i)*p02(i) + s(234,i)*p03(i)   
'     1          + s(235,i)*p04(i) + s(236,i)*p05(i) + s(237,i)*p06(i)   
'     2          + s(238,i)*p07(i) + s(239,i)*p08(i) + s(240,i)*p09(i)   
'     3          + s(241,i)*p10(i) + s(242,i)*p11(i) + s(243,i)*p12(i)   
'     4          + s(244,i)*p13(i) + s(245,i)*p14(i) + s(246,i)*p15(i)   
'     5          + s(247,i)*p16(i) + s(248,i)*p17(i) + s(249,i)*p18(i)   
'     6          + s(250,i)*p19(i) + s(251,i)*p20(i) + s(252,i)*p21(i)   
'     7          + s(253,i)*p22(i) + s(275,i)*p23(i) + s(298,i)*p24(i)   
'  221 continue                                                          
'c                                                                       
'      do 222 i=lft,llt                                                  
'        ap23(i) = s(254,i)*p01(i) + s(255,i)*p02(i) + s(256,i)*p03(i)   
'     1          + s(257,i)*p04(i) + s(258,i)*p05(i) + s(259,i)*p06(i)   
'     2          + s(260,i)*p07(i) + s(261,i)*p08(i) + s(262,i)*p09(i)   
'     3          + s(263,i)*p10(i) + s(264,i)*p11(i) + s(265,i)*p12(i)   
'     4          + s(266,i)*p13(i) + s(267,i)*p14(i) + s(268,i)*p15(i)   
'     5          + s(269,i)*p16(i) + s(270,i)*p17(i) + s(271,i)*p18(i)   
'     6          + s(272,i)*p19(i) + s(273,i)*p20(i) + s(274,i)*p21(i)   
'     7          + s(275,i)*p22(i) + s(276,i)*p23(i) + s(299,i)*p24(i)   
'  222 continue                                                          
'c                                                                       
'      do 223 i=lft,llt                                                  
'        ap24(i) = s(277,i)*p01(i) + s(278,i)*p02(i) + s(279,i)*p03(i)   
'     1          + s(280,i)*p04(i) + s(281,i)*p05(i) + s(282,i)*p06(i)   
'     2          + s(283,i)*p07(i) + s(284,i)*p08(i) + s(285,i)*p09(i)   
'     3          + s(286,i)*p10(i) + s(287,i)*p11(i) + s(288,i)*p12(i)   
'     4          + s(289,i)*p13(i) + s(290,i)*p14(i) + s(291,i)*p15(i)   
'     5          + s(292,i)*p16(i) + s(293,i)*p17(i) + s(294,i)*p18(i)   
'     6          + s(295,i)*p19(i) + s(296,i)*p20(i) + s(297,i)*p21(i)   
'     7          + s(298,i)*p22(i) + s(299,i)*p23(i) + s(300,i)*p24(i)   
'  223 continue                                                          
'c                                                                       
'c.... scatter operations                                                
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
'         ap(lm(16,i)) = ap(lm(16,i)) + ap16(i)                          
'         ap(lm(17,i)) = ap(lm(17,i)) + ap17(i)                          
'         ap(lm(18,i)) = ap(lm(18,i)) + ap18(i)                          
'         ap(lm(19,i)) = ap(lm(19,i)) + ap19(i)                          
'         ap(lm(20,i)) = ap(lm(20,i)) + ap20(i)                          
'         ap(lm(21,i)) = ap(lm(21,i)) + ap21(i)                          
'         ap(lm(22,i)) = ap(lm(22,i)) + ap22(i)                          
'         ap(lm(23,i)) = ap(lm(23,i)) + ap23(i)                          
'         ap(lm(24,i)) = ap(lm(24,i)) + ap24(i)                          
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
'         ap(lm(16,i)) = ap(lm(16,i)) + ap16(i)                          
'         ap(lm(17,i)) = ap(lm(17,i)) + ap17(i)                          
'         ap(lm(18,i)) = ap(lm(18,i)) + ap18(i)                          
'         ap(lm(19,i)) = ap(lm(19,i)) + ap19(i)                          
'         ap(lm(20,i)) = ap(lm(20,i)) + ap20(i)                          
'         ap(lm(21,i)) = ap(lm(21,i)) + ap21(i)                          
'         ap(lm(22,i)) = ap(lm(22,i)) + ap22(i)                          
'         ap(lm(23,i)) = ap(lm(23,i)) + ap23(i)                          
'         ap(lm(24,i)) = ap(lm(24,i)) + ap24(i)                          
'  400 continue                                                          
'      endif                                                             
'c                                                                       
'      return                                                            
'      end                                                               

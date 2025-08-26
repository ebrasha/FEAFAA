'This file contains all the methods of fstif1.f
Partial Public Class clsSolve

    ''' <summary>
    ''' compute b *d*b product for second 150 coefficients in column profile of hexahedral element stiffness
    ''' </summary>
    ''' <param name="s"></param>
    Public Sub fstif1(ByRef s(,) As Double)

        Dim i As Integer ' YC 102418

        For i = lft To llt

            ' YC 102418
            's(150, i) = s(150, i) + py6(i) * c215(i) + px6(i) * c415(i) + pz6(i) * c515(i)
            's(151, i) = s(151, i) + py6(i) * c216(i) + px6(i) * c416(i) + pz6(i) * c516(i)
            's(152, i) = s(152, i) + py6(i) * c217(i) + px6(i) * c417(i) + pz6(i) * c517(i)
            's(153, i) = s(153, i) + pz6(i) * c301(i) + py6(i) * c501(i) + px6(i) * c601(i)
            's(154, i) = s(154, i) + pz6(i) * c302(i) + py6(i) * c502(i) + px6(i) * c602(i)
            's(155, i) = s(155, i) + pz6(i) * c303(i) + py6(i) * c503(i) + px6(i) * c603(i)
            's(156, i) = s(156, i) + pz6(i) * c304(i) + py6(i) * c504(i) + px6(i) * c604(i)
            's(157, i) = s(157, i) + pz6(i) * c305(i) + py6(i) * c505(i) + px6(i) * c605(i)
            's(158, i) = s(158, i) + pz6(i) * c306(i) + py6(i) * c506(i) + px6(i) * c606(i)
            's(159, i) = s(159, i) + pz6(i) * c307(i) + py6(i) * c507(i) + px6(i) * c607(i)
            s(151, i) = s(151, i) + py6(i) * c215(i) + px6(i) * c415(i) + pz6(i) * c515(i)
            s(152, i) = s(152, i) + py6(i) * c216(i) + px6(i) * c416(i) + pz6(i) * c516(i)
            s(153, i) = s(153, i) + py6(i) * c217(i) + px6(i) * c417(i) + pz6(i) * c517(i)
            s(154, i) = s(154, i) + pz6(i) * c301(i) + py6(i) * c501(i) + px6(i) * c601(i)
            s(155, i) = s(155, i) + pz6(i) * c302(i) + py6(i) * c502(i) + px6(i) * c602(i)
            s(156, i) = s(156, i) + pz6(i) * c303(i) + py6(i) * c503(i) + px6(i) * c603(i)
            s(157, i) = s(157, i) + pz6(i) * c304(i) + py6(i) * c504(i) + px6(i) * c604(i)
            s(158, i) = s(158, i) + pz6(i) * c305(i) + py6(i) * c505(i) + px6(i) * c605(i)
            s(159, i) = s(159, i) + pz6(i) * c306(i) + py6(i) * c506(i) + px6(i) * c606(i)
            s(160, i) = s(160, i) + pz6(i) * c307(i) + py6(i) * c507(i) + px6(i) * c607(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(160, i) = s(160, i) + pz6(i) * c308(i) + py6(i) * c508(i) + px6(i) * c608(i)
            's(161, i) = s(161, i) + pz6(i) * c309(i) + py6(i) * c509(i) + px6(i) * c609(i)
            's(162, i) = s(162, i) + pz6(i) * c310(i) + py6(i) * c510(i) + px6(i) * c610(i)
            's(163, i) = s(163, i) + pz6(i) * c311(i) + py6(i) * c511(i) + px6(i) * c611(i)
            's(164, i) = s(164, i) + pz6(i) * c312(i) + py6(i) * c512(i) + px6(i) * c612(i)
            's(165, i) = s(165, i) + pz6(i) * c313(i) + py6(i) * c513(i) + px6(i) * c613(i)
            's(166, i) = s(166, i) + pz6(i) * c314(i) + py6(i) * c514(i) + px6(i) * c614(i)
            's(167, i) = s(167, i) + pz6(i) * c315(i) + py6(i) * c515(i) + px6(i) * c615(i)
            's(168, i) = s(168, i) + pz6(i) * c316(i) + py6(i) * c516(i) + px6(i) * c616(i)
            's(169, i) = s(169, i) + pz6(i) * c317(i) + py6(i) * c517(i) + px6(i) * c617(i)
            s(161, i) = s(161, i) + pz6(i) * c308(i) + py6(i) * c508(i) + px6(i) * c608(i)
            s(162, i) = s(162, i) + pz6(i) * c309(i) + py6(i) * c509(i) + px6(i) * c609(i)
            s(163, i) = s(163, i) + pz6(i) * c310(i) + py6(i) * c510(i) + px6(i) * c610(i)
            s(164, i) = s(164, i) + pz6(i) * c311(i) + py6(i) * c511(i) + px6(i) * c611(i)
            s(165, i) = s(165, i) + pz6(i) * c312(i) + py6(i) * c512(i) + px6(i) * c612(i)
            s(166, i) = s(166, i) + pz6(i) * c313(i) + py6(i) * c513(i) + px6(i) * c613(i)
            s(167, i) = s(167, i) + pz6(i) * c314(i) + py6(i) * c514(i) + px6(i) * c614(i)
            s(168, i) = s(168, i) + pz6(i) * c315(i) + py6(i) * c515(i) + px6(i) * c615(i)
            s(169, i) = s(169, i) + pz6(i) * c316(i) + py6(i) * c516(i) + px6(i) * c616(i)
            s(170, i) = s(170, i) + pz6(i) * c317(i) + py6(i) * c517(i) + px6(i) * c617(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(170, i) = s(170, i) + pz6(i) * c318(i) + py6(i) * c518(i) + px6(i) * c618(i)
            's(171, i) = s(171, i) + px7(i) * c101(i) + py7(i) * c401(i) + pz7(i) * c601(i)
            's(172, i) = s(172, i) + px7(i) * c102(i) + py7(i) * c402(i) + pz7(i) * c602(i)
            's(173, i) = s(173, i) + px7(i) * c103(i) + py7(i) * c403(i) + pz7(i) * c603(i)
            's(174, i) = s(174, i) + px7(i) * c104(i) + py7(i) * c404(i) + pz7(i) * c604(i)
            's(175, i) = s(175, i) + px7(i) * c105(i) + py7(i) * c405(i) + pz7(i) * c605(i)
            's(176, i) = s(176, i) + px7(i) * c106(i) + py7(i) * c406(i) + pz7(i) * c606(i)
            's(177, i) = s(177, i) + px7(i) * c107(i) + py7(i) * c407(i) + pz7(i) * c607(i)
            's(178, i) = s(178, i) + px7(i) * c108(i) + py7(i) * c408(i) + pz7(i) * c608(i)
            's(179, i) = s(179, i) + px7(i) * c109(i) + py7(i) * c409(i) + pz7(i) * c609(i)
            s(171, i) = s(171, i) + pz6(i) * c318(i) + py6(i) * c518(i) + px6(i) * c618(i)
            s(172, i) = s(172, i) + px7(i) * c101(i) + py7(i) * c401(i) + pz7(i) * c601(i)
            s(173, i) = s(173, i) + px7(i) * c102(i) + py7(i) * c402(i) + pz7(i) * c602(i)
            s(174, i) = s(174, i) + px7(i) * c103(i) + py7(i) * c403(i) + pz7(i) * c603(i)
            s(175, i) = s(175, i) + px7(i) * c104(i) + py7(i) * c404(i) + pz7(i) * c604(i)
            s(176, i) = s(176, i) + px7(i) * c105(i) + py7(i) * c405(i) + pz7(i) * c605(i)
            s(177, i) = s(177, i) + px7(i) * c106(i) + py7(i) * c406(i) + pz7(i) * c606(i)
            s(178, i) = s(178, i) + px7(i) * c107(i) + py7(i) * c407(i) + pz7(i) * c607(i)
            s(179, i) = s(179, i) + px7(i) * c108(i) + py7(i) * c408(i) + pz7(i) * c608(i)
            s(180, i) = s(180, i) + px7(i) * c109(i) + py7(i) * c409(i) + pz7(i) * c609(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(180, i) = s(180, i) + px7(i) * c110(i) + py7(i) * c410(i) + pz7(i) * c610(i)
            's(181, i) = s(181, i) + px7(i) * c111(i) + py7(i) * c411(i) + pz7(i) * c611(i)
            's(182, i) = s(182, i) + px7(i) * c112(i) + py7(i) * c412(i) + pz7(i) * c612(i)
            's(183, i) = s(183, i) + px7(i) * c113(i) + py7(i) * c413(i) + pz7(i) * c613(i)
            's(184, i) = s(184, i) + px7(i) * c114(i) + py7(i) * c414(i) + pz7(i) * c614(i)
            's(185, i) = s(185, i) + px7(i) * c115(i) + py7(i) * c415(i) + pz7(i) * c615(i)
            's(186, i) = s(186, i) + px7(i) * c116(i) + py7(i) * c416(i) + pz7(i) * c616(i)
            's(187, i) = s(187, i) + px7(i) * c117(i) + py7(i) * c417(i) + pz7(i) * c617(i)
            's(188, i) = s(188, i) + px7(i) * c118(i) + py7(i) * c418(i) + pz7(i) * c618(i)
            's(189, i) = s(189, i) + px7(i) * c119(i) + py7(i) * c419(i) + pz7(i) * c619(i)
            s(181, i) = s(181, i) + px7(i) * c110(i) + py7(i) * c410(i) + pz7(i) * c610(i)
            s(182, i) = s(182, i) + px7(i) * c111(i) + py7(i) * c411(i) + pz7(i) * c611(i)
            s(183, i) = s(183, i) + px7(i) * c112(i) + py7(i) * c412(i) + pz7(i) * c612(i)
            s(184, i) = s(184, i) + px7(i) * c113(i) + py7(i) * c413(i) + pz7(i) * c613(i)
            s(185, i) = s(185, i) + px7(i) * c114(i) + py7(i) * c414(i) + pz7(i) * c614(i)
            s(186, i) = s(186, i) + px7(i) * c115(i) + py7(i) * c415(i) + pz7(i) * c615(i)
            s(187, i) = s(187, i) + px7(i) * c116(i) + py7(i) * c416(i) + pz7(i) * c616(i)
            s(188, i) = s(188, i) + px7(i) * c117(i) + py7(i) * c417(i) + pz7(i) * c617(i)
            s(189, i) = s(189, i) + px7(i) * c118(i) + py7(i) * c418(i) + pz7(i) * c618(i)
            s(190, i) = s(190, i) + px7(i) * c119(i) + py7(i) * c419(i) + pz7(i) * c619(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(190, i) = s(190, i) + py7(i) * c201(i) + px7(i) * c401(i) + pz7(i) * c501(i)
            's(191, i) = s(191, i) + py7(i) * c202(i) + px7(i) * c402(i) + pz7(i) * c502(i)
            's(192, i) = s(192, i) + py7(i) * c203(i) + px7(i) * c403(i) + pz7(i) * c503(i)
            's(193, i) = s(193, i) + py7(i) * c204(i) + px7(i) * c404(i) + pz7(i) * c504(i)
            's(194, i) = s(194, i) + py7(i) * c205(i) + px7(i) * c405(i) + pz7(i) * c505(i)
            's(195, i) = s(195, i) + py7(i) * c206(i) + px7(i) * c406(i) + pz7(i) * c506(i)
            's(196, i) = s(196, i) + py7(i) * c207(i) + px7(i) * c407(i) + pz7(i) * c507(i)
            's(197, i) = s(197, i) + py7(i) * c208(i) + px7(i) * c408(i) + pz7(i) * c508(i)
            's(198, i) = s(198, i) + py7(i) * c209(i) + px7(i) * c409(i) + pz7(i) * c509(i)
            's(199, i) = s(199, i) + py7(i) * c210(i) + px7(i) * c410(i) + pz7(i) * c510(i)
            s(191, i) = s(191, i) + py7(i) * c201(i) + px7(i) * c401(i) + pz7(i) * c501(i)
            s(192, i) = s(192, i) + py7(i) * c202(i) + px7(i) * c402(i) + pz7(i) * c502(i)
            s(193, i) = s(193, i) + py7(i) * c203(i) + px7(i) * c403(i) + pz7(i) * c503(i)
            s(194, i) = s(194, i) + py7(i) * c204(i) + px7(i) * c404(i) + pz7(i) * c504(i)
            s(195, i) = s(195, i) + py7(i) * c205(i) + px7(i) * c405(i) + pz7(i) * c505(i)
            s(196, i) = s(196, i) + py7(i) * c206(i) + px7(i) * c406(i) + pz7(i) * c506(i)
            s(197, i) = s(197, i) + py7(i) * c207(i) + px7(i) * c407(i) + pz7(i) * c507(i)
            s(198, i) = s(198, i) + py7(i) * c208(i) + px7(i) * c408(i) + pz7(i) * c508(i)
            s(199, i) = s(199, i) + py7(i) * c209(i) + px7(i) * c409(i) + pz7(i) * c509(i)
            s(200, i) = s(200, i) + py7(i) * c210(i) + px7(i) * c410(i) + pz7(i) * c510(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(200, i) = s(200, i) + py7(i) * c211(i) + px7(i) * c411(i) + pz7(i) * c511(i)
            's(201, i) = s(201, i) + py7(i) * c212(i) + px7(i) * c412(i) + pz7(i) * c512(i)
            's(202, i) = s(202, i) + py7(i) * c213(i) + px7(i) * c413(i) + pz7(i) * c513(i)
            's(203, i) = s(203, i) + py7(i) * c214(i) + px7(i) * c414(i) + pz7(i) * c514(i)
            's(204, i) = s(204, i) + py7(i) * c215(i) + px7(i) * c415(i) + pz7(i) * c515(i)
            's(205, i) = s(205, i) + py7(i) * c216(i) + px7(i) * c416(i) + pz7(i) * c516(i)
            's(206, i) = s(206, i) + py7(i) * c217(i) + px7(i) * c417(i) + pz7(i) * c517(i)
            's(207, i) = s(207, i) + py7(i) * c218(i) + px7(i) * c418(i) + pz7(i) * c518(i)
            's(208, i) = s(208, i) + py7(i) * c219(i) + px7(i) * c419(i) + pz7(i) * c519(i)
            's(209, i) = s(209, i) + py7(i) * c220(i) + px7(i) * c420(i) + pz7(i) * c520(i)
            s(201, i) = s(201, i) + py7(i) * c211(i) + px7(i) * c411(i) + pz7(i) * c511(i)
            s(202, i) = s(202, i) + py7(i) * c212(i) + px7(i) * c412(i) + pz7(i) * c512(i)
            s(203, i) = s(203, i) + py7(i) * c213(i) + px7(i) * c413(i) + pz7(i) * c513(i)
            s(204, i) = s(204, i) + py7(i) * c214(i) + px7(i) * c414(i) + pz7(i) * c514(i)
            s(205, i) = s(205, i) + py7(i) * c215(i) + px7(i) * c415(i) + pz7(i) * c515(i)
            s(206, i) = s(206, i) + py7(i) * c216(i) + px7(i) * c416(i) + pz7(i) * c516(i)
            s(207, i) = s(207, i) + py7(i) * c217(i) + px7(i) * c417(i) + pz7(i) * c517(i)
            s(208, i) = s(208, i) + py7(i) * c218(i) + px7(i) * c418(i) + pz7(i) * c518(i)
            s(209, i) = s(209, i) + py7(i) * c219(i) + px7(i) * c419(i) + pz7(i) * c519(i)
            s(210, i) = s(210, i) + py7(i) * c220(i) + px7(i) * c420(i) + pz7(i) * c520(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(210, i) = s(210, i) + pz7(i) * c301(i) + py7(i) * c501(i) + px7(i) * c601(i)
            's(211, i) = s(211, i) + pz7(i) * c302(i) + py7(i) * c502(i) + px7(i) * c602(i)
            's(212, i) = s(212, i) + pz7(i) * c303(i) + py7(i) * c503(i) + px7(i) * c603(i)
            's(213, i) = s(213, i) + pz7(i) * c304(i) + py7(i) * c504(i) + px7(i) * c604(i)
            's(214, i) = s(214, i) + pz7(i) * c305(i) + py7(i) * c505(i) + px7(i) * c605(i)
            's(215, i) = s(215, i) + pz7(i) * c306(i) + py7(i) * c506(i) + px7(i) * c606(i)
            's(216, i) = s(216, i) + pz7(i) * c307(i) + py7(i) * c507(i) + px7(i) * c607(i)
            's(217, i) = s(217, i) + pz7(i) * c308(i) + py7(i) * c508(i) + px7(i) * c608(i)
            's(218, i) = s(218, i) + pz7(i) * c309(i) + py7(i) * c509(i) + px7(i) * c609(i)
            's(219, i) = s(219, i) + pz7(i) * c310(i) + py7(i) * c510(i) + px7(i) * c610(i)
            s(211, i) = s(211, i) + pz7(i) * c301(i) + py7(i) * c501(i) + px7(i) * c601(i)
            s(212, i) = s(212, i) + pz7(i) * c302(i) + py7(i) * c502(i) + px7(i) * c602(i)
            s(213, i) = s(213, i) + pz7(i) * c303(i) + py7(i) * c503(i) + px7(i) * c603(i)
            s(214, i) = s(214, i) + pz7(i) * c304(i) + py7(i) * c504(i) + px7(i) * c604(i)
            s(215, i) = s(215, i) + pz7(i) * c305(i) + py7(i) * c505(i) + px7(i) * c605(i)
            s(216, i) = s(216, i) + pz7(i) * c306(i) + py7(i) * c506(i) + px7(i) * c606(i)
            s(217, i) = s(217, i) + pz7(i) * c307(i) + py7(i) * c507(i) + px7(i) * c607(i)
            s(218, i) = s(218, i) + pz7(i) * c308(i) + py7(i) * c508(i) + px7(i) * c608(i)
            s(219, i) = s(219, i) + pz7(i) * c309(i) + py7(i) * c509(i) + px7(i) * c609(i)
            s(220, i) = s(220, i) + pz7(i) * c310(i) + py7(i) * c510(i) + px7(i) * c610(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(220, i) = s(220, i) + pz7(i) * c311(i) + py7(i) * c511(i) + px7(i) * c611(i)
            's(221, i) = s(221, i) + pz7(i) * c312(i) + py7(i) * c512(i) + px7(i) * c612(i)
            's(222, i) = s(222, i) + pz7(i) * c313(i) + py7(i) * c513(i) + px7(i) * c613(i)
            's(223, i) = s(223, i) + pz7(i) * c314(i) + py7(i) * c514(i) + px7(i) * c614(i)
            's(224, i) = s(224, i) + pz7(i) * c315(i) + py7(i) * c515(i) + px7(i) * c615(i)
            's(225, i) = s(225, i) + pz7(i) * c316(i) + py7(i) * c516(i) + px7(i) * c616(i)
            's(226, i) = s(226, i) + pz7(i) * c317(i) + py7(i) * c517(i) + px7(i) * c617(i)
            's(227, i) = s(227, i) + pz7(i) * c318(i) + py7(i) * c518(i) + px7(i) * c618(i)
            's(228, i) = s(228, i) + pz7(i) * c319(i) + py7(i) * c519(i) + px7(i) * c619(i)
            's(229, i) = s(229, i) + pz7(i) * c320(i) + py7(i) * c520(i) + px7(i) * c620(i)
            s(221, i) = s(221, i) + pz7(i) * c311(i) + py7(i) * c511(i) + px7(i) * c611(i)
            s(222, i) = s(222, i) + pz7(i) * c312(i) + py7(i) * c512(i) + px7(i) * c612(i)
            s(223, i) = s(223, i) + pz7(i) * c313(i) + py7(i) * c513(i) + px7(i) * c613(i)
            s(224, i) = s(224, i) + pz7(i) * c314(i) + py7(i) * c514(i) + px7(i) * c614(i)
            s(225, i) = s(225, i) + pz7(i) * c315(i) + py7(i) * c515(i) + px7(i) * c615(i)
            s(226, i) = s(226, i) + pz7(i) * c316(i) + py7(i) * c516(i) + px7(i) * c616(i)
            s(227, i) = s(227, i) + pz7(i) * c317(i) + py7(i) * c517(i) + px7(i) * c617(i)
            s(228, i) = s(228, i) + pz7(i) * c318(i) + py7(i) * c518(i) + px7(i) * c618(i)
            s(229, i) = s(229, i) + pz7(i) * c319(i) + py7(i) * c519(i) + px7(i) * c619(i)
            s(230, i) = s(230, i) + pz7(i) * c320(i) + py7(i) * c520(i) + px7(i) * c620(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(230, i) = s(230, i) + pz7(i) * c321(i) + py7(i) * c521(i) + px7(i) * c621(i)
            's(231, i) = s(231, i) + px8(i) * c101(i) + py8(i) * c401(i) + pz8(i) * c601(i)
            's(232, i) = s(232, i) + px8(i) * c102(i) + py8(i) * c402(i) + pz8(i) * c602(i)
            's(233, i) = s(233, i) + px8(i) * c103(i) + py8(i) * c403(i) + pz8(i) * c603(i)
            's(234, i) = s(234, i) + px8(i) * c104(i) + py8(i) * c404(i) + pz8(i) * c604(i)
            's(235, i) = s(235, i) + px8(i) * c105(i) + py8(i) * c405(i) + pz8(i) * c605(i)
            's(236, i) = s(236, i) + px8(i) * c106(i) + py8(i) * c406(i) + pz8(i) * c606(i)
            's(237, i) = s(237, i) + px8(i) * c107(i) + py8(i) * c407(i) + pz8(i) * c607(i)
            's(238, i) = s(238, i) + px8(i) * c108(i) + py8(i) * c408(i) + pz8(i) * c608(i)
            's(239, i) = s(239, i) + px8(i) * c109(i) + py8(i) * c409(i) + pz8(i) * c609(i)
            s(231, i) = s(231, i) + pz7(i) * c321(i) + py7(i) * c521(i) + px7(i) * c621(i)
            s(232, i) = s(232, i) + px8(i) * c101(i) + py8(i) * c401(i) + pz8(i) * c601(i)
            s(233, i) = s(233, i) + px8(i) * c102(i) + py8(i) * c402(i) + pz8(i) * c602(i)
            s(234, i) = s(234, i) + px8(i) * c103(i) + py8(i) * c403(i) + pz8(i) * c603(i)
            s(235, i) = s(235, i) + px8(i) * c104(i) + py8(i) * c404(i) + pz8(i) * c604(i)
            s(236, i) = s(236, i) + px8(i) * c105(i) + py8(i) * c405(i) + pz8(i) * c605(i)
            s(237, i) = s(237, i) + px8(i) * c106(i) + py8(i) * c406(i) + pz8(i) * c606(i)
            s(238, i) = s(238, i) + px8(i) * c107(i) + py8(i) * c407(i) + pz8(i) * c607(i)
            s(239, i) = s(239, i) + px8(i) * c108(i) + py8(i) * c408(i) + pz8(i) * c608(i)
            s(240, i) = s(240, i) + px8(i) * c109(i) + py8(i) * c409(i) + pz8(i) * c609(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(240, i) = s(240, i) + px8(i) * c110(i) + py8(i) * c410(i) + pz8(i) * c610(i)
            's(241, i) = s(241, i) + px8(i) * c111(i) + py8(i) * c411(i) + pz8(i) * c611(i)
            's(242, i) = s(242, i) + px8(i) * c112(i) + py8(i) * c412(i) + pz8(i) * c612(i)
            's(243, i) = s(243, i) + px8(i) * c113(i) + py8(i) * c413(i) + pz8(i) * c613(i)
            's(244, i) = s(244, i) + px8(i) * c114(i) + py8(i) * c414(i) + pz8(i) * c614(i)
            's(245, i) = s(245, i) + px8(i) * c115(i) + py8(i) * c415(i) + pz8(i) * c615(i)
            's(246, i) = s(246, i) + px8(i) * c116(i) + py8(i) * c416(i) + pz8(i) * c616(i)
            's(247, i) = s(247, i) + px8(i) * c117(i) + py8(i) * c417(i) + pz8(i) * c617(i)
            's(248, i) = s(248, i) + px8(i) * c118(i) + py8(i) * c418(i) + pz8(i) * c618(i)
            's(249, i) = s(249, i) + px8(i) * c119(i) + py8(i) * c419(i) + pz8(i) * c619(i)
            s(241, i) = s(241, i) + px8(i) * c110(i) + py8(i) * c410(i) + pz8(i) * c610(i)
            s(242, i) = s(242, i) + px8(i) * c111(i) + py8(i) * c411(i) + pz8(i) * c611(i)
            s(243, i) = s(243, i) + px8(i) * c112(i) + py8(i) * c412(i) + pz8(i) * c612(i)
            s(244, i) = s(244, i) + px8(i) * c113(i) + py8(i) * c413(i) + pz8(i) * c613(i)
            s(245, i) = s(245, i) + px8(i) * c114(i) + py8(i) * c414(i) + pz8(i) * c614(i)
            s(246, i) = s(246, i) + px8(i) * c115(i) + py8(i) * c415(i) + pz8(i) * c615(i)
            s(247, i) = s(247, i) + px8(i) * c116(i) + py8(i) * c416(i) + pz8(i) * c616(i)
            s(248, i) = s(248, i) + px8(i) * c117(i) + py8(i) * c417(i) + pz8(i) * c617(i)
            s(249, i) = s(249, i) + px8(i) * c118(i) + py8(i) * c418(i) + pz8(i) * c618(i)
            s(250, i) = s(250, i) + px8(i) * c119(i) + py8(i) * c419(i) + pz8(i) * c619(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 
            's(250, i) = s(250, i) + px8(i) * c120(i) + py8(i) * c420(i) + pz8(i) * c620(i)
            's(251, i) = s(251, i) + px8(i) * c121(i) + py8(i) * c421(i) + pz8(i) * c621(i)
            's(252, i) = s(252, i) + px8(i) * c122(i) + py8(i) * c422(i) + pz8(i) * c622(i)
            's(253, i) = s(253, i) + py8(i) * c201(i) + px8(i) * c401(i) + pz8(i) * c501(i)
            's(254, i) = s(254, i) + py8(i) * c202(i) + px8(i) * c402(i) + pz8(i) * c502(i)
            's(255, i) = s(255, i) + py8(i) * c203(i) + px8(i) * c403(i) + pz8(i) * c503(i)
            's(256, i) = s(256, i) + py8(i) * c204(i) + px8(i) * c404(i) + pz8(i) * c504(i)
            's(257, i) = s(257, i) + py8(i) * c205(i) + px8(i) * c405(i) + pz8(i) * c505(i)
            's(258, i) = s(258, i) + py8(i) * c206(i) + px8(i) * c406(i) + pz8(i) * c506(i)
            's(259, i) = s(259, i) + py8(i) * c207(i) + px8(i) * c407(i) + pz8(i) * c507(i)
            s(251, i) = s(251, i) + px8(i) * c120(i) + py8(i) * c420(i) + pz8(i) * c620(i)
            s(252, i) = s(252, i) + px8(i) * c121(i) + py8(i) * c421(i) + pz8(i) * c621(i)
            s(253, i) = s(253, i) + px8(i) * c122(i) + py8(i) * c422(i) + pz8(i) * c622(i)
            s(254, i) = s(254, i) + py8(i) * c201(i) + px8(i) * c401(i) + pz8(i) * c501(i)
            s(255, i) = s(255, i) + py8(i) * c202(i) + px8(i) * c402(i) + pz8(i) * c502(i)
            s(256, i) = s(256, i) + py8(i) * c203(i) + px8(i) * c403(i) + pz8(i) * c503(i)
            s(257, i) = s(257, i) + py8(i) * c204(i) + px8(i) * c404(i) + pz8(i) * c504(i)
            s(258, i) = s(258, i) + py8(i) * c205(i) + px8(i) * c405(i) + pz8(i) * c505(i)
            s(259, i) = s(259, i) + py8(i) * c206(i) + px8(i) * c406(i) + pz8(i) * c506(i)
            s(260, i) = s(260, i) + py8(i) * c207(i) + px8(i) * c407(i) + pz8(i) * c507(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(260, i) = s(260, i) + py8(i) * c208(i) + px8(i) * c408(i) + pz8(i) * c508(i)
            's(261, i) = s(261, i) + py8(i) * c209(i) + px8(i) * c409(i) + pz8(i) * c509(i)
            's(262, i) = s(262, i) + py8(i) * c210(i) + px8(i) * c410(i) + pz8(i) * c510(i)
            's(263, i) = s(263, i) + py8(i) * c211(i) + px8(i) * c411(i) + pz8(i) * c511(i)
            's(264, i) = s(264, i) + py8(i) * c212(i) + px8(i) * c412(i) + pz8(i) * c512(i)
            's(265, i) = s(265, i) + py8(i) * c213(i) + px8(i) * c413(i) + pz8(i) * c513(i)
            's(266, i) = s(266, i) + py8(i) * c214(i) + px8(i) * c414(i) + pz8(i) * c514(i)
            's(267, i) = s(267, i) + py8(i) * c215(i) + px8(i) * c415(i) + pz8(i) * c515(i)
            's(268, i) = s(268, i) + py8(i) * c216(i) + px8(i) * c416(i) + pz8(i) * c516(i)
            's(269, i) = s(269, i) + py8(i) * c217(i) + px8(i) * c417(i) + pz8(i) * c517(i)
            s(261, i) = s(261, i) + py8(i) * c208(i) + px8(i) * c408(i) + pz8(i) * c508(i)
            s(262, i) = s(262, i) + py8(i) * c209(i) + px8(i) * c409(i) + pz8(i) * c509(i)
            s(263, i) = s(263, i) + py8(i) * c210(i) + px8(i) * c410(i) + pz8(i) * c510(i)
            s(264, i) = s(264, i) + py8(i) * c211(i) + px8(i) * c411(i) + pz8(i) * c511(i)
            s(265, i) = s(265, i) + py8(i) * c212(i) + px8(i) * c412(i) + pz8(i) * c512(i)
            s(266, i) = s(266, i) + py8(i) * c213(i) + px8(i) * c413(i) + pz8(i) * c513(i)
            s(267, i) = s(267, i) + py8(i) * c214(i) + px8(i) * c414(i) + pz8(i) * c514(i)
            s(268, i) = s(268, i) + py8(i) * c215(i) + px8(i) * c415(i) + pz8(i) * c515(i)
            s(269, i) = s(269, i) + py8(i) * c216(i) + px8(i) * c416(i) + pz8(i) * c516(i)
            s(270, i) = s(270, i) + py8(i) * c217(i) + px8(i) * c417(i) + pz8(i) * c517(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(270, i) = s(270, i) + py8(i) * c218(i) + px8(i) * c418(i) + pz8(i) * c518(i)
            's(271, i) = s(271, i) + py8(i) * c219(i) + px8(i) * c419(i) + pz8(i) * c519(i)
            's(272, i) = s(272, i) + py8(i) * c220(i) + px8(i) * c420(i) + pz8(i) * c520(i)
            's(273, i) = s(273, i) + py8(i) * c221(i) + px8(i) * c421(i) + pz8(i) * c521(i)
            's(274, i) = s(274, i) + py8(i) * c222(i) + px8(i) * c422(i) + pz8(i) * c522(i)
            's(275, i) = s(275, i) + py8(i) * c223(i) + px8(i) * c423(i) + pz8(i) * c523(i)
            's(276, i) = s(276, i) + pz8(i) * c301(i) + py8(i) * c501(i) + px8(i) * c601(i)
            's(277, i) = s(277, i) + pz8(i) * c302(i) + py8(i) * c502(i) + px8(i) * c602(i)
            's(278, i) = s(278, i) + pz8(i) * c303(i) + py8(i) * c503(i) + px8(i) * c603(i)
            's(279, i) = s(279, i) + pz8(i) * c304(i) + py8(i) * c504(i) + px8(i) * c604(i)
            s(271, i) = s(271, i) + py8(i) * c218(i) + px8(i) * c418(i) + pz8(i) * c518(i)
            s(272, i) = s(272, i) + py8(i) * c219(i) + px8(i) * c419(i) + pz8(i) * c519(i)
            s(273, i) = s(273, i) + py8(i) * c220(i) + px8(i) * c420(i) + pz8(i) * c520(i)
            s(274, i) = s(274, i) + py8(i) * c221(i) + px8(i) * c421(i) + pz8(i) * c521(i)
            s(275, i) = s(275, i) + py8(i) * c222(i) + px8(i) * c422(i) + pz8(i) * c522(i)
            s(276, i) = s(276, i) + py8(i) * c223(i) + px8(i) * c423(i) + pz8(i) * c523(i)
            s(277, i) = s(277, i) + pz8(i) * c301(i) + py8(i) * c501(i) + px8(i) * c601(i)
            s(278, i) = s(278, i) + pz8(i) * c302(i) + py8(i) * c502(i) + px8(i) * c602(i)
            s(279, i) = s(279, i) + pz8(i) * c303(i) + py8(i) * c503(i) + px8(i) * c603(i)
            s(280, i) = s(280, i) + pz8(i) * c304(i) + py8(i) * c504(i) + px8(i) * c604(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(280, i) = s(280, i) + pz8(i) * c305(i) + py8(i) * c505(i) + px8(i) * c605(i)
            's(281, i) = s(281, i) + pz8(i) * c306(i) + py8(i) * c506(i) + px8(i) * c606(i)
            's(282, i) = s(282, i) + pz8(i) * c307(i) + py8(i) * c507(i) + px8(i) * c607(i)
            's(283, i) = s(283, i) + pz8(i) * c308(i) + py8(i) * c508(i) + px8(i) * c608(i)
            's(284, i) = s(284, i) + pz8(i) * c309(i) + py8(i) * c509(i) + px8(i) * c609(i)
            's(285, i) = s(285, i) + pz8(i) * c310(i) + py8(i) * c510(i) + px8(i) * c610(i)
            's(286, i) = s(286, i) + pz8(i) * c311(i) + py8(i) * c511(i) + px8(i) * c611(i)
            's(287, i) = s(287, i) + pz8(i) * c312(i) + py8(i) * c512(i) + px8(i) * c612(i)
            's(288, i) = s(288, i) + pz8(i) * c313(i) + py8(i) * c513(i) + px8(i) * c613(i)
            's(289, i) = s(289, i) + pz8(i) * c314(i) + py8(i) * c514(i) + px8(i) * c614(i)
            s(281, i) = s(281, i) + pz8(i) * c305(i) + py8(i) * c505(i) + px8(i) * c605(i)
            s(282, i) = s(282, i) + pz8(i) * c306(i) + py8(i) * c506(i) + px8(i) * c606(i)
            s(283, i) = s(283, i) + pz8(i) * c307(i) + py8(i) * c507(i) + px8(i) * c607(i)
            s(284, i) = s(284, i) + pz8(i) * c308(i) + py8(i) * c508(i) + px8(i) * c608(i)
            s(285, i) = s(285, i) + pz8(i) * c309(i) + py8(i) * c509(i) + px8(i) * c609(i)
            s(286, i) = s(286, i) + pz8(i) * c310(i) + py8(i) * c510(i) + px8(i) * c610(i)
            s(287, i) = s(287, i) + pz8(i) * c311(i) + py8(i) * c511(i) + px8(i) * c611(i)
            s(288, i) = s(288, i) + pz8(i) * c312(i) + py8(i) * c512(i) + px8(i) * c612(i)
            s(289, i) = s(289, i) + pz8(i) * c313(i) + py8(i) * c513(i) + px8(i) * c613(i)
            s(290, i) = s(290, i) + pz8(i) * c314(i) + py8(i) * c514(i) + px8(i) * c614(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(290, i) = s(290, i) + pz8(i) * c315(i) + py8(i) * c515(i) + px8(i) * c615(i)
            's(291, i) = s(291, i) + pz8(i) * c316(i) + py8(i) * c516(i) + px8(i) * c616(i)
            's(292, i) = s(292, i) + pz8(i) * c317(i) + py8(i) * c517(i) + px8(i) * c617(i)
            's(293, i) = s(293, i) + pz8(i) * c318(i) + py8(i) * c518(i) + px8(i) * c618(i)
            's(294, i) = s(294, i) + pz8(i) * c319(i) + py8(i) * c519(i) + px8(i) * c619(i)
            's(295, i) = s(295, i) + pz8(i) * c320(i) + py8(i) * c520(i) + px8(i) * c620(i)
            's(296, i) = s(296, i) + pz8(i) * c321(i) + py8(i) * c521(i) + px8(i) * c621(i)
            's(297, i) = s(297, i) + pz8(i) * c322(i) + py8(i) * c522(i) + px8(i) * c622(i)
            's(298, i) = s(298, i) + pz8(i) * c323(i) + py8(i) * c523(i) + px8(i) * c623(i)
            's(299, i) = s(299, i) + pz8(i) * c324(i) + py8(i) * c524(i) + px8(i) * c624(i)
            s(291, i) = s(291, i) + pz8(i) * c315(i) + py8(i) * c515(i) + px8(i) * c615(i)
            s(292, i) = s(292, i) + pz8(i) * c316(i) + py8(i) * c516(i) + px8(i) * c616(i)
            s(293, i) = s(293, i) + pz8(i) * c317(i) + py8(i) * c517(i) + px8(i) * c617(i)
            s(294, i) = s(294, i) + pz8(i) * c318(i) + py8(i) * c518(i) + px8(i) * c618(i)
            s(295, i) = s(295, i) + pz8(i) * c319(i) + py8(i) * c519(i) + px8(i) * c619(i)
            s(296, i) = s(296, i) + pz8(i) * c320(i) + py8(i) * c520(i) + px8(i) * c620(i)
            s(297, i) = s(297, i) + pz8(i) * c321(i) + py8(i) * c521(i) + px8(i) * c621(i)
            s(298, i) = s(298, i) + pz8(i) * c322(i) + py8(i) * c522(i) + px8(i) * c622(i)
            s(299, i) = s(299, i) + pz8(i) * c323(i) + py8(i) * c523(i) + px8(i) * c623(i)
            s(300, i) = s(300, i) + pz8(i) * c324(i) + py8(i) * c524(i) + px8(i) * c624(i)
            ' YC 102418 END

        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine fstif1(s)
'c
'
'      implicit double precision (a-h,o-z)                             
'
'c
'c                        t
'c===> module to compute b *d*b product for second 150 coefficients
'c     in column profile of hexahedral element stiffness
'c
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect4/
'     1 px1(64),px2(64),px3(64),px4(64),
'     2 px5(64),px6(64),px7(64),px8(64),
'     3 py1(64),py2(64),py3(64),py4(64),
'     4 py5(64),py6(64),py7(64),py8(64),
'     5 pz1(64),pz2(64),pz3(64),pz4(64),
'     6 pz5(64),pz6(64),pz7(64),pz8(64)
'      common/vect5/
'     1c101(64),c201(64),c301(64),c401(64),c501(64),c601(64),
'     2c102(64),c202(64),c302(64),c402(64),c502(64),c602(64),
'     3c103(64),c203(64),c303(64),c403(64),c503(64),c603(64),
'     4c104(64),c204(64),c304(64),c404(64),c504(64),c604(64),
'     5c105(64),c205(64),c305(64),c405(64),c505(64),c605(64),
'     6c106(64),c206(64),c306(64),c406(64),c506(64),c606(64),
'     7c107(64),c207(64),c307(64),c407(64),c507(64),c607(64),
'     8c108(64),c208(64),c308(64),c408(64),c508(64),c608(64),
'     9c109(64),c209(64),c309(64),c409(64),c509(64),c609(64),dum5(256)
'      common/vect5a/
'     1c110(64),c210(64),c310(64),c410(64),c510(64),c610(64),
'     1c111(64),c211(64),c311(64),c411(64),c511(64),c611(64),
'     2c112(64),c212(64),c312(64),c412(64),c512(64),c612(64),
'     3c113(64),c213(64),c313(64),c413(64),c513(64),c613(64),
'     4c114(64),c214(64),c314(64),c414(64),c514(64),c614(64),
'     5c115(64),c215(64),c315(64),c415(64),c515(64),c615(64),
'     6c116(64),c216(64),c316(64),c416(64),c516(64),c616(64),
'     7c117(64),c217(64),c317(64),c417(64),c517(64),c617(64),
'     8c118(64),c218(64),c318(64),c418(64),c518(64),c618(64)
'      common/vect5b/
'     9c119(64),c219(64),c319(64),c419(64),c519(64),c619(64),
'     1c120(64),c220(64),c320(64),c420(64),c520(64),c620(64),
'     1c121(64),c221(64),c321(64),c421(64),c521(64),c621(64),
'     2c122(64),c222(64),c322(64),c422(64),c522(64),c622(64),
'     3c123(64),c223(64),c323(64),c423(64),c523(64),c623(64),
'     4c124(64),c224(64),c324(64),c424(64),c524(64),c624(64)
'c
'      dimension s(324,*)
'c
'      do 10 i=lft,llt
'      s(151,i)=s(151,i)+py6(i)*c215(i)+px6(i)*c415(i)+pz6(i)*c515(i)
'      s(152,i)=s(152,i)+py6(i)*c216(i)+px6(i)*c416(i)+pz6(i)*c516(i)
'      s(153,i)=s(153,i)+py6(i)*c217(i)+px6(i)*c417(i)+pz6(i)*c517(i)
'      s(154,i)=s(154,i)+pz6(i)*c301(i)+py6(i)*c501(i)+px6(i)*c601(i)
'      s(155,i)=s(155,i)+pz6(i)*c302(i)+py6(i)*c502(i)+px6(i)*c602(i)
'      s(156,i)=s(156,i)+pz6(i)*c303(i)+py6(i)*c503(i)+px6(i)*c603(i)
'      s(157,i)=s(157,i)+pz6(i)*c304(i)+py6(i)*c504(i)+px6(i)*c604(i)
'      s(158,i)=s(158,i)+pz6(i)*c305(i)+py6(i)*c505(i)+px6(i)*c605(i)
'      s(159,i)=s(159,i)+pz6(i)*c306(i)+py6(i)*c506(i)+px6(i)*c606(i)
'      s(160,i)=s(160,i)+pz6(i)*c307(i)+py6(i)*c507(i)+px6(i)*c607(i)
'   10 continue
'      do 20 i=lft,llt
'      s(161,i)=s(161,i)+pz6(i)*c308(i)+py6(i)*c508(i)+px6(i)*c608(i)
'      s(162,i)=s(162,i)+pz6(i)*c309(i)+py6(i)*c509(i)+px6(i)*c609(i)
'      s(163,i)=s(163,i)+pz6(i)*c310(i)+py6(i)*c510(i)+px6(i)*c610(i)
'      s(164,i)=s(164,i)+pz6(i)*c311(i)+py6(i)*c511(i)+px6(i)*c611(i)
'      s(165,i)=s(165,i)+pz6(i)*c312(i)+py6(i)*c512(i)+px6(i)*c612(i)
'      s(166,i)=s(166,i)+pz6(i)*c313(i)+py6(i)*c513(i)+px6(i)*c613(i)
'      s(167,i)=s(167,i)+pz6(i)*c314(i)+py6(i)*c514(i)+px6(i)*c614(i)
'      s(168,i)=s(168,i)+pz6(i)*c315(i)+py6(i)*c515(i)+px6(i)*c615(i)
'      s(169,i)=s(169,i)+pz6(i)*c316(i)+py6(i)*c516(i)+px6(i)*c616(i)
'      s(170,i)=s(170,i)+pz6(i)*c317(i)+py6(i)*c517(i)+px6(i)*c617(i)
'   20 continue
'      do 30 i=lft,llt
'      s(171,i)=s(171,i)+pz6(i)*c318(i)+py6(i)*c518(i)+px6(i)*c618(i)
'      s(172,i)=s(172,i)+px7(i)*c101(i)+py7(i)*c401(i)+pz7(i)*c601(i)
'      s(173,i)=s(173,i)+px7(i)*c102(i)+py7(i)*c402(i)+pz7(i)*c602(i)
'      s(174,i)=s(174,i)+px7(i)*c103(i)+py7(i)*c403(i)+pz7(i)*c603(i)
'      s(175,i)=s(175,i)+px7(i)*c104(i)+py7(i)*c404(i)+pz7(i)*c604(i)
'      s(176,i)=s(176,i)+px7(i)*c105(i)+py7(i)*c405(i)+pz7(i)*c605(i)
'      s(177,i)=s(177,i)+px7(i)*c106(i)+py7(i)*c406(i)+pz7(i)*c606(i)
'      s(178,i)=s(178,i)+px7(i)*c107(i)+py7(i)*c407(i)+pz7(i)*c607(i)
'      s(179,i)=s(179,i)+px7(i)*c108(i)+py7(i)*c408(i)+pz7(i)*c608(i)
'      s(180,i)=s(180,i)+px7(i)*c109(i)+py7(i)*c409(i)+pz7(i)*c609(i)
'   30 continue
'      do 40 i=lft,llt
'      s(181,i)=s(181,i)+px7(i)*c110(i)+py7(i)*c410(i)+pz7(i)*c610(i)
'      s(182,i)=s(182,i)+px7(i)*c111(i)+py7(i)*c411(i)+pz7(i)*c611(i)
'      s(183,i)=s(183,i)+px7(i)*c112(i)+py7(i)*c412(i)+pz7(i)*c612(i)
'      s(184,i)=s(184,i)+px7(i)*c113(i)+py7(i)*c413(i)+pz7(i)*c613(i)
'      s(185,i)=s(185,i)+px7(i)*c114(i)+py7(i)*c414(i)+pz7(i)*c614(i)
'      s(186,i)=s(186,i)+px7(i)*c115(i)+py7(i)*c415(i)+pz7(i)*c615(i)
'      s(187,i)=s(187,i)+px7(i)*c116(i)+py7(i)*c416(i)+pz7(i)*c616(i)
'      s(188,i)=s(188,i)+px7(i)*c117(i)+py7(i)*c417(i)+pz7(i)*c617(i)
'      s(189,i)=s(189,i)+px7(i)*c118(i)+py7(i)*c418(i)+pz7(i)*c618(i)
'      s(190,i)=s(190,i)+px7(i)*c119(i)+py7(i)*c419(i)+pz7(i)*c619(i)
'   40 continue
'      do 50 i=lft,llt
'      s(191,i)=s(191,i)+py7(i)*c201(i)+px7(i)*c401(i)+pz7(i)*c501(i)
'      s(192,i)=s(192,i)+py7(i)*c202(i)+px7(i)*c402(i)+pz7(i)*c502(i)
'      s(193,i)=s(193,i)+py7(i)*c203(i)+px7(i)*c403(i)+pz7(i)*c503(i)
'      s(194,i)=s(194,i)+py7(i)*c204(i)+px7(i)*c404(i)+pz7(i)*c504(i)
'      s(195,i)=s(195,i)+py7(i)*c205(i)+px7(i)*c405(i)+pz7(i)*c505(i)
'      s(196,i)=s(196,i)+py7(i)*c206(i)+px7(i)*c406(i)+pz7(i)*c506(i)
'      s(197,i)=s(197,i)+py7(i)*c207(i)+px7(i)*c407(i)+pz7(i)*c507(i)
'      s(198,i)=s(198,i)+py7(i)*c208(i)+px7(i)*c408(i)+pz7(i)*c508(i)
'      s(199,i)=s(199,i)+py7(i)*c209(i)+px7(i)*c409(i)+pz7(i)*c509(i)
'      s(200,i)=s(200,i)+py7(i)*c210(i)+px7(i)*c410(i)+pz7(i)*c510(i)
'   50 continue
'      do 60 i=lft,llt
'      s(201,i)=s(201,i)+py7(i)*c211(i)+px7(i)*c411(i)+pz7(i)*c511(i)
'      s(202,i)=s(202,i)+py7(i)*c212(i)+px7(i)*c412(i)+pz7(i)*c512(i)
'      s(203,i)=s(203,i)+py7(i)*c213(i)+px7(i)*c413(i)+pz7(i)*c513(i)
'      s(204,i)=s(204,i)+py7(i)*c214(i)+px7(i)*c414(i)+pz7(i)*c514(i)
'      s(205,i)=s(205,i)+py7(i)*c215(i)+px7(i)*c415(i)+pz7(i)*c515(i)
'      s(206,i)=s(206,i)+py7(i)*c216(i)+px7(i)*c416(i)+pz7(i)*c516(i)
'      s(207,i)=s(207,i)+py7(i)*c217(i)+px7(i)*c417(i)+pz7(i)*c517(i)
'      s(208,i)=s(208,i)+py7(i)*c218(i)+px7(i)*c418(i)+pz7(i)*c518(i)
'      s(209,i)=s(209,i)+py7(i)*c219(i)+px7(i)*c419(i)+pz7(i)*c519(i)
'      s(210,i)=s(210,i)+py7(i)*c220(i)+px7(i)*c420(i)+pz7(i)*c520(i)
'   60 continue
'      do 70 i=lft,llt
'      s(211,i)=s(211,i)+pz7(i)*c301(i)+py7(i)*c501(i)+px7(i)*c601(i)
'      s(212,i)=s(212,i)+pz7(i)*c302(i)+py7(i)*c502(i)+px7(i)*c602(i)
'      s(213,i)=s(213,i)+pz7(i)*c303(i)+py7(i)*c503(i)+px7(i)*c603(i)
'      s(214,i)=s(214,i)+pz7(i)*c304(i)+py7(i)*c504(i)+px7(i)*c604(i)
'      s(215,i)=s(215,i)+pz7(i)*c305(i)+py7(i)*c505(i)+px7(i)*c605(i)
'      s(216,i)=s(216,i)+pz7(i)*c306(i)+py7(i)*c506(i)+px7(i)*c606(i)
'      s(217,i)=s(217,i)+pz7(i)*c307(i)+py7(i)*c507(i)+px7(i)*c607(i)
'      s(218,i)=s(218,i)+pz7(i)*c308(i)+py7(i)*c508(i)+px7(i)*c608(i)
'      s(219,i)=s(219,i)+pz7(i)*c309(i)+py7(i)*c509(i)+px7(i)*c609(i)
'      s(220,i)=s(220,i)+pz7(i)*c310(i)+py7(i)*c510(i)+px7(i)*c610(i)
'   70 continue
'      do 80 i=lft,llt
'      s(221,i)=s(221,i)+pz7(i)*c311(i)+py7(i)*c511(i)+px7(i)*c611(i)
'      s(222,i)=s(222,i)+pz7(i)*c312(i)+py7(i)*c512(i)+px7(i)*c612(i)
'      s(223,i)=s(223,i)+pz7(i)*c313(i)+py7(i)*c513(i)+px7(i)*c613(i)
'      s(224,i)=s(224,i)+pz7(i)*c314(i)+py7(i)*c514(i)+px7(i)*c614(i)
'      s(225,i)=s(225,i)+pz7(i)*c315(i)+py7(i)*c515(i)+px7(i)*c615(i)
'      s(226,i)=s(226,i)+pz7(i)*c316(i)+py7(i)*c516(i)+px7(i)*c616(i)
'      s(227,i)=s(227,i)+pz7(i)*c317(i)+py7(i)*c517(i)+px7(i)*c617(i)
'      s(228,i)=s(228,i)+pz7(i)*c318(i)+py7(i)*c518(i)+px7(i)*c618(i)
'      s(229,i)=s(229,i)+pz7(i)*c319(i)+py7(i)*c519(i)+px7(i)*c619(i)
'      s(230,i)=s(230,i)+pz7(i)*c320(i)+py7(i)*c520(i)+px7(i)*c620(i)
'   80 continue
'      do 90 i=lft,llt
'      s(231,i)=s(231,i)+pz7(i)*c321(i)+py7(i)*c521(i)+px7(i)*c621(i)
'      s(232,i)=s(232,i)+px8(i)*c101(i)+py8(i)*c401(i)+pz8(i)*c601(i)
'      s(233,i)=s(233,i)+px8(i)*c102(i)+py8(i)*c402(i)+pz8(i)*c602(i)
'      s(234,i)=s(234,i)+px8(i)*c103(i)+py8(i)*c403(i)+pz8(i)*c603(i)
'      s(235,i)=s(235,i)+px8(i)*c104(i)+py8(i)*c404(i)+pz8(i)*c604(i)
'      s(236,i)=s(236,i)+px8(i)*c105(i)+py8(i)*c405(i)+pz8(i)*c605(i)
'      s(237,i)=s(237,i)+px8(i)*c106(i)+py8(i)*c406(i)+pz8(i)*c606(i)
'      s(238,i)=s(238,i)+px8(i)*c107(i)+py8(i)*c407(i)+pz8(i)*c607(i)
'      s(239,i)=s(239,i)+px8(i)*c108(i)+py8(i)*c408(i)+pz8(i)*c608(i)
'      s(240,i)=s(240,i)+px8(i)*c109(i)+py8(i)*c409(i)+pz8(i)*c609(i)
'   90 continue
'      do 100 i=lft,llt
'      s(241,i)=s(241,i)+px8(i)*c110(i)+py8(i)*c410(i)+pz8(i)*c610(i)
'      s(242,i)=s(242,i)+px8(i)*c111(i)+py8(i)*c411(i)+pz8(i)*c611(i)
'      s(243,i)=s(243,i)+px8(i)*c112(i)+py8(i)*c412(i)+pz8(i)*c612(i)
'      s(244,i)=s(244,i)+px8(i)*c113(i)+py8(i)*c413(i)+pz8(i)*c613(i)
'      s(245,i)=s(245,i)+px8(i)*c114(i)+py8(i)*c414(i)+pz8(i)*c614(i)
'      s(246,i)=s(246,i)+px8(i)*c115(i)+py8(i)*c415(i)+pz8(i)*c615(i)
'      s(247,i)=s(247,i)+px8(i)*c116(i)+py8(i)*c416(i)+pz8(i)*c616(i)
'      s(248,i)=s(248,i)+px8(i)*c117(i)+py8(i)*c417(i)+pz8(i)*c617(i)
'      s(249,i)=s(249,i)+px8(i)*c118(i)+py8(i)*c418(i)+pz8(i)*c618(i)
'      s(250,i)=s(250,i)+px8(i)*c119(i)+py8(i)*c419(i)+pz8(i)*c619(i)
'  100 continue
'      do 110 i=lft,llt
'      s(251,i)=s(251,i)+px8(i)*c120(i)+py8(i)*c420(i)+pz8(i)*c620(i)
'      s(252,i)=s(252,i)+px8(i)*c121(i)+py8(i)*c421(i)+pz8(i)*c621(i)
'      s(253,i)=s(253,i)+px8(i)*c122(i)+py8(i)*c422(i)+pz8(i)*c622(i)
'      s(254,i)=s(254,i)+py8(i)*c201(i)+px8(i)*c401(i)+pz8(i)*c501(i)
'      s(255,i)=s(255,i)+py8(i)*c202(i)+px8(i)*c402(i)+pz8(i)*c502(i)
'      s(256,i)=s(256,i)+py8(i)*c203(i)+px8(i)*c403(i)+pz8(i)*c503(i)
'      s(257,i)=s(257,i)+py8(i)*c204(i)+px8(i)*c404(i)+pz8(i)*c504(i)
'      s(258,i)=s(258,i)+py8(i)*c205(i)+px8(i)*c405(i)+pz8(i)*c505(i)
'      s(259,i)=s(259,i)+py8(i)*c206(i)+px8(i)*c406(i)+pz8(i)*c506(i)
'      s(260,i)=s(260,i)+py8(i)*c207(i)+px8(i)*c407(i)+pz8(i)*c507(i)
'  110 continue
'      do 120 i=lft,llt
'      s(261,i)=s(261,i)+py8(i)*c208(i)+px8(i)*c408(i)+pz8(i)*c508(i)
'      s(262,i)=s(262,i)+py8(i)*c209(i)+px8(i)*c409(i)+pz8(i)*c509(i)
'      s(263,i)=s(263,i)+py8(i)*c210(i)+px8(i)*c410(i)+pz8(i)*c510(i)
'      s(264,i)=s(264,i)+py8(i)*c211(i)+px8(i)*c411(i)+pz8(i)*c511(i)
'      s(265,i)=s(265,i)+py8(i)*c212(i)+px8(i)*c412(i)+pz8(i)*c512(i)
'      s(266,i)=s(266,i)+py8(i)*c213(i)+px8(i)*c413(i)+pz8(i)*c513(i)
'      s(267,i)=s(267,i)+py8(i)*c214(i)+px8(i)*c414(i)+pz8(i)*c514(i)
'      s(268,i)=s(268,i)+py8(i)*c215(i)+px8(i)*c415(i)+pz8(i)*c515(i)
'      s(269,i)=s(269,i)+py8(i)*c216(i)+px8(i)*c416(i)+pz8(i)*c516(i)
'      s(270,i)=s(270,i)+py8(i)*c217(i)+px8(i)*c417(i)+pz8(i)*c517(i)
'  120 continue
'      do 130 i=lft,llt
'      s(271,i)=s(271,i)+py8(i)*c218(i)+px8(i)*c418(i)+pz8(i)*c518(i)
'      s(272,i)=s(272,i)+py8(i)*c219(i)+px8(i)*c419(i)+pz8(i)*c519(i)
'      s(273,i)=s(273,i)+py8(i)*c220(i)+px8(i)*c420(i)+pz8(i)*c520(i)
'      s(274,i)=s(274,i)+py8(i)*c221(i)+px8(i)*c421(i)+pz8(i)*c521(i)
'      s(275,i)=s(275,i)+py8(i)*c222(i)+px8(i)*c422(i)+pz8(i)*c522(i)
'      s(276,i)=s(276,i)+py8(i)*c223(i)+px8(i)*c423(i)+pz8(i)*c523(i)
'      s(277,i)=s(277,i)+pz8(i)*c301(i)+py8(i)*c501(i)+px8(i)*c601(i)
'      s(278,i)=s(278,i)+pz8(i)*c302(i)+py8(i)*c502(i)+px8(i)*c602(i)
'      s(279,i)=s(279,i)+pz8(i)*c303(i)+py8(i)*c503(i)+px8(i)*c603(i)
'      s(280,i)=s(280,i)+pz8(i)*c304(i)+py8(i)*c504(i)+px8(i)*c604(i)
'  130 continue
'      do 140 i=lft,llt
'      s(281,i)=s(281,i)+pz8(i)*c305(i)+py8(i)*c505(i)+px8(i)*c605(i)
'      s(282,i)=s(282,i)+pz8(i)*c306(i)+py8(i)*c506(i)+px8(i)*c606(i)
'      s(283,i)=s(283,i)+pz8(i)*c307(i)+py8(i)*c507(i)+px8(i)*c607(i)
'      s(284,i)=s(284,i)+pz8(i)*c308(i)+py8(i)*c508(i)+px8(i)*c608(i)
'      s(285,i)=s(285,i)+pz8(i)*c309(i)+py8(i)*c509(i)+px8(i)*c609(i)
'      s(286,i)=s(286,i)+pz8(i)*c310(i)+py8(i)*c510(i)+px8(i)*c610(i)
'      s(287,i)=s(287,i)+pz8(i)*c311(i)+py8(i)*c511(i)+px8(i)*c611(i)
'      s(288,i)=s(288,i)+pz8(i)*c312(i)+py8(i)*c512(i)+px8(i)*c612(i)
'      s(289,i)=s(289,i)+pz8(i)*c313(i)+py8(i)*c513(i)+px8(i)*c613(i)
'      s(290,i)=s(290,i)+pz8(i)*c314(i)+py8(i)*c514(i)+px8(i)*c614(i)
'  140 continue
'      do 150 i=lft,llt
'      s(291,i)=s(291,i)+pz8(i)*c315(i)+py8(i)*c515(i)+px8(i)*c615(i)
'      s(292,i)=s(292,i)+pz8(i)*c316(i)+py8(i)*c516(i)+px8(i)*c616(i)
'      s(293,i)=s(293,i)+pz8(i)*c317(i)+py8(i)*c517(i)+px8(i)*c617(i)
'      s(294,i)=s(294,i)+pz8(i)*c318(i)+py8(i)*c518(i)+px8(i)*c618(i)
'      s(295,i)=s(295,i)+pz8(i)*c319(i)+py8(i)*c519(i)+px8(i)*c619(i)
'      s(296,i)=s(296,i)+pz8(i)*c320(i)+py8(i)*c520(i)+px8(i)*c620(i)
'      s(297,i)=s(297,i)+pz8(i)*c321(i)+py8(i)*c521(i)+px8(i)*c621(i)
'      s(298,i)=s(298,i)+pz8(i)*c322(i)+py8(i)*c522(i)+px8(i)*c622(i)
'      s(299,i)=s(299,i)+pz8(i)*c323(i)+py8(i)*c523(i)+px8(i)*c623(i)
'      s(300,i)=s(300,i)+pz8(i)*c324(i)+py8(i)*c524(i)+px8(i)*c624(i)
'  150 continue
'      return
'      end

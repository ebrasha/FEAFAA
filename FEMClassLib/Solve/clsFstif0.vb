'This file contains all the methods of fstif0.f
Partial Public Class clsSolve

    ''' <summary>
    ''' compute b *d*b product for first 150 coefficients in column profile of hexahedral element stiffness
    ''' </summary>
    ''' <param name="s"></param>
    Public Sub fstif0(ByRef s(,) As Double)

        Dim i As Integer ' YC 102418

        For i = lft To llt

            ' YC 102418
            's(0, i) = s(0, i) + px1(i) * c101(i) + py1(i) * c401(i) + pz1(i) * c601(i)
            's(1, i) = s(1, i) + py1(i) * c201(i) + px1(i) * c401(i) + pz1(i) * c501(i)
            's(2, i) = s(2, i) + py1(i) * c202(i) + px1(i) * c402(i) + pz1(i) * c502(i)
            's(3, i) = s(3, i) + pz1(i) * c301(i) + py1(i) * c501(i) + px1(i) * c601(i)
            's(4, i) = s(4, i) + pz1(i) * c302(i) + py1(i) * c502(i) + px1(i) * c602(i)
            's(5, i) = s(5, i) + pz1(i) * c303(i) + py1(i) * c503(i) + px1(i) * c603(i)
            's(6, i) = s(6, i) + px2(i) * c101(i) + py2(i) * c401(i) + pz2(i) * c601(i)
            's(7, i) = s(7, i) + px2(i) * c102(i) + py2(i) * c402(i) + pz2(i) * c602(i)
            's(8, i) = s(8, i) + px2(i) * c103(i) + py2(i) * c403(i) + pz2(i) * c603(i)
            's(9, i) = s(9, i) + px2(i) * c104(i) + py2(i) * c404(i) + pz2(i) * c604(i)
            s(1, i) = s(1, i) + px1(i) * c101(i) + py1(i) * c401(i) + pz1(i) * c601(i)
            s(2, i) = s(2, i) + py1(i) * c201(i) + px1(i) * c401(i) + pz1(i) * c501(i)
            s(3, i) = s(3, i) + py1(i) * c202(i) + px1(i) * c402(i) + pz1(i) * c502(i)
            s(4, i) = s(4, i) + pz1(i) * c301(i) + py1(i) * c501(i) + px1(i) * c601(i)
            s(5, i) = s(5, i) + pz1(i) * c302(i) + py1(i) * c502(i) + px1(i) * c602(i)
            s(6, i) = s(6, i) + pz1(i) * c303(i) + py1(i) * c503(i) + px1(i) * c603(i)
            s(7, i) = s(7, i) + px2(i) * c101(i) + py2(i) * c401(i) + pz2(i) * c601(i)
            s(8, i) = s(8, i) + px2(i) * c102(i) + py2(i) * c402(i) + pz2(i) * c602(i)
            s(9, i) = s(9, i) + px2(i) * c103(i) + py2(i) * c403(i) + pz2(i) * c603(i)
            s(10, i) = s(10, i) + px2(i) * c104(i) + py2(i) * c404(i) + pz2(i) * c604(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(10, i) = s(10, i) + py2(i) * c201(i) + px2(i) * c401(i) + pz2(i) * c501(i)
            's(11, i) = s(11, i) + py2(i) * c202(i) + px2(i) * c402(i) + pz2(i) * c502(i)
            's(12, i) = s(12, i) + py2(i) * c203(i) + px2(i) * c403(i) + pz2(i) * c503(i)
            's(13, i) = s(13, i) + py2(i) * c204(i) + px2(i) * c404(i) + pz2(i) * c504(i)
            's(14, i) = s(14, i) + py2(i) * c205(i) + px2(i) * c405(i) + pz2(i) * c505(i)
            's(15, i) = s(15, i) + pz2(i) * c301(i) + py2(i) * c501(i) + px2(i) * c601(i)
            's(16, i) = s(16, i) + pz2(i) * c302(i) + py2(i) * c502(i) + px2(i) * c602(i)
            's(17, i) = s(17, i) + pz2(i) * c303(i) + py2(i) * c503(i) + px2(i) * c603(i)
            's(18, i) = s(18, i) + pz2(i) * c304(i) + py2(i) * c504(i) + px2(i) * c604(i)
            's(19, i) = s(19, i) + pz2(i) * c305(i) + py2(i) * c505(i) + px2(i) * c605(i)
            s(11, i) = s(11, i) + py2(i) * c201(i) + px2(i) * c401(i) + pz2(i) * c501(i)
            s(12, i) = s(12, i) + py2(i) * c202(i) + px2(i) * c402(i) + pz2(i) * c502(i)
            s(13, i) = s(13, i) + py2(i) * c203(i) + px2(i) * c403(i) + pz2(i) * c503(i)
            s(14, i) = s(14, i) + py2(i) * c204(i) + px2(i) * c404(i) + pz2(i) * c504(i)
            s(15, i) = s(15, i) + py2(i) * c205(i) + px2(i) * c405(i) + pz2(i) * c505(i)
            s(16, i) = s(16, i) + pz2(i) * c301(i) + py2(i) * c501(i) + px2(i) * c601(i)
            s(17, i) = s(17, i) + pz2(i) * c302(i) + py2(i) * c502(i) + px2(i) * c602(i)
            s(18, i) = s(18, i) + pz2(i) * c303(i) + py2(i) * c503(i) + px2(i) * c603(i)
            s(19, i) = s(19, i) + pz2(i) * c304(i) + py2(i) * c504(i) + px2(i) * c604(i)
            s(20, i) = s(20, i) + pz2(i) * c305(i) + py2(i) * c505(i) + px2(i) * c605(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 
            's(20, i) = s(20, i) + pz2(i) * c306(i) + py2(i) * c506(i) + px2(i) * c606(i)
            's(21, i) = s(21, i) + px3(i) * c101(i) + py3(i) * c401(i) + pz3(i) * c601(i)
            's(22, i) = s(22, i) + px3(i) * c102(i) + py3(i) * c402(i) + pz3(i) * c602(i)
            's(23, i) = s(23, i) + px3(i) * c103(i) + py3(i) * c403(i) + pz3(i) * c603(i)
            's(24, i) = s(24, i) + px3(i) * c104(i) + py3(i) * c404(i) + pz3(i) * c604(i)
            's(25, i) = s(25, i) + px3(i) * c105(i) + py3(i) * c405(i) + pz3(i) * c605(i)
            's(26, i) = s(26, i) + px3(i) * c106(i) + py3(i) * c406(i) + pz3(i) * c606(i)
            's(27, i) = s(27, i) + px3(i) * c107(i) + py3(i) * c407(i) + pz3(i) * c607(i)
            's(28, i) = s(28, i) + py3(i) * c201(i) + px3(i) * c401(i) + pz3(i) * c501(i)
            's(29, i) = s(29, i) + py3(i) * c202(i) + px3(i) * c402(i) + pz3(i) * c502(i)
            s(21, i) = s(21, i) + pz2(i) * c306(i) + py2(i) * c506(i) + px2(i) * c606(i)
            s(22, i) = s(22, i) + px3(i) * c101(i) + py3(i) * c401(i) + pz3(i) * c601(i)
            s(23, i) = s(23, i) + px3(i) * c102(i) + py3(i) * c402(i) + pz3(i) * c602(i)
            s(24, i) = s(24, i) + px3(i) * c103(i) + py3(i) * c403(i) + pz3(i) * c603(i)
            s(25, i) = s(25, i) + px3(i) * c104(i) + py3(i) * c404(i) + pz3(i) * c604(i)
            s(26, i) = s(26, i) + px3(i) * c105(i) + py3(i) * c405(i) + pz3(i) * c605(i)
            s(27, i) = s(27, i) + px3(i) * c106(i) + py3(i) * c406(i) + pz3(i) * c606(i)
            s(28, i) = s(28, i) + px3(i) * c107(i) + py3(i) * c407(i) + pz3(i) * c607(i)
            s(29, i) = s(29, i) + py3(i) * c201(i) + px3(i) * c401(i) + pz3(i) * c501(i)
            s(30, i) = s(30, i) + py3(i) * c202(i) + px3(i) * c402(i) + pz3(i) * c502(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(30, i) = s(30, i) + py3(i) * c203(i) + px3(i) * c403(i) + pz3(i) * c503(i)
            's(31, i) = s(31, i) + py3(i) * c204(i) + px3(i) * c404(i) + pz3(i) * c504(i)
            's(32, i) = s(32, i) + py3(i) * c205(i) + px3(i) * c405(i) + pz3(i) * c505(i)
            's(33, i) = s(33, i) + py3(i) * c206(i) + px3(i) * c406(i) + pz3(i) * c506(i)
            's(34, i) = s(34, i) + py3(i) * c207(i) + px3(i) * c407(i) + pz3(i) * c507(i)
            's(35, i) = s(35, i) + py3(i) * c208(i) + px3(i) * c408(i) + pz3(i) * c508(i)
            's(36, i) = s(36, i) + pz3(i) * c301(i) + py3(i) * c501(i) + px3(i) * c601(i)
            's(37, i) = s(37, i) + pz3(i) * c302(i) + py3(i) * c502(i) + px3(i) * c602(i)
            's(38, i) = s(38, i) + pz3(i) * c303(i) + py3(i) * c503(i) + px3(i) * c603(i)
            's(39, i) = s(39, i) + pz3(i) * c304(i) + py3(i) * c504(i) + px3(i) * c604(i)
            s(31, i) = s(31, i) + py3(i) * c203(i) + px3(i) * c403(i) + pz3(i) * c503(i)
            s(32, i) = s(32, i) + py3(i) * c204(i) + px3(i) * c404(i) + pz3(i) * c504(i)
            s(33, i) = s(33, i) + py3(i) * c205(i) + px3(i) * c405(i) + pz3(i) * c505(i)
            s(34, i) = s(34, i) + py3(i) * c206(i) + px3(i) * c406(i) + pz3(i) * c506(i)
            s(35, i) = s(35, i) + py3(i) * c207(i) + px3(i) * c407(i) + pz3(i) * c507(i)
            s(36, i) = s(36, i) + py3(i) * c208(i) + px3(i) * c408(i) + pz3(i) * c508(i)
            s(37, i) = s(37, i) + pz3(i) * c301(i) + py3(i) * c501(i) + px3(i) * c601(i)
            s(38, i) = s(38, i) + pz3(i) * c302(i) + py3(i) * c502(i) + px3(i) * c602(i)
            s(39, i) = s(39, i) + pz3(i) * c303(i) + py3(i) * c503(i) + px3(i) * c603(i)
            s(40, i) = s(40, i) + pz3(i) * c304(i) + py3(i) * c504(i) + px3(i) * c604(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(40, i) = s(40, i) + pz3(i) * c305(i) + py3(i) * c505(i) + px3(i) * c605(i)
            's(41, i) = s(41, i) + pz3(i) * c306(i) + py3(i) * c506(i) + px3(i) * c606(i)
            's(42, i) = s(42, i) + pz3(i) * c307(i) + py3(i) * c507(i) + px3(i) * c607(i)
            's(43, i) = s(43, i) + pz3(i) * c308(i) + py3(i) * c508(i) + px3(i) * c608(i)
            's(44, i) = s(44, i) + pz3(i) * c309(i) + py3(i) * c509(i) + px3(i) * c609(i)
            's(45, i) = s(45, i) + px4(i) * c101(i) + py4(i) * c401(i) + pz4(i) * c601(i)
            's(46, i) = s(46, i) + px4(i) * c102(i) + py4(i) * c402(i) + pz4(i) * c602(i)
            's(47, i) = s(47, i) + px4(i) * c103(i) + py4(i) * c403(i) + pz4(i) * c603(i)
            's(48, i) = s(48, i) + px4(i) * c104(i) + py4(i) * c404(i) + pz4(i) * c604(i)
            's(49, i) = s(49, i) + px4(i) * c105(i) + py4(i) * c405(i) + pz4(i) * c605(i)
            s(41, i) = s(41, i) + pz3(i) * c305(i) + py3(i) * c505(i) + px3(i) * c605(i)
            s(42, i) = s(42, i) + pz3(i) * c306(i) + py3(i) * c506(i) + px3(i) * c606(i)
            s(43, i) = s(43, i) + pz3(i) * c307(i) + py3(i) * c507(i) + px3(i) * c607(i)
            s(44, i) = s(44, i) + pz3(i) * c308(i) + py3(i) * c508(i) + px3(i) * c608(i)
            s(45, i) = s(45, i) + pz3(i) * c309(i) + py3(i) * c509(i) + px3(i) * c609(i)
            s(46, i) = s(46, i) + px4(i) * c101(i) + py4(i) * c401(i) + pz4(i) * c601(i)
            s(47, i) = s(47, i) + px4(i) * c102(i) + py4(i) * c402(i) + pz4(i) * c602(i)
            s(48, i) = s(48, i) + px4(i) * c103(i) + py4(i) * c403(i) + pz4(i) * c603(i)
            s(49, i) = s(49, i) + px4(i) * c104(i) + py4(i) * c404(i) + pz4(i) * c604(i)
            s(50, i) = s(50, i) + px4(i) * c105(i) + py4(i) * c405(i) + pz4(i) * c605(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 
            's(50, i) = s(50, i) + px4(i) * c106(i) + py4(i) * c406(i) + pz4(i) * c606(i)
            's(51, i) = s(51, i) + px4(i) * c107(i) + py4(i) * c407(i) + pz4(i) * c607(i)
            's(52, i) = s(52, i) + px4(i) * c108(i) + py4(i) * c408(i) + pz4(i) * c608(i)
            's(53, i) = s(53, i) + px4(i) * c109(i) + py4(i) * c409(i) + pz4(i) * c609(i)
            's(54, i) = s(54, i) + px4(i) * c110(i) + py4(i) * c410(i) + pz4(i) * c610(i)
            's(55, i) = s(55, i) + py4(i) * c201(i) + px4(i) * c401(i) + pz4(i) * c501(i)
            's(56, i) = s(56, i) + py4(i) * c202(i) + px4(i) * c402(i) + pz4(i) * c502(i)
            's(57, i) = s(57, i) + py4(i) * c203(i) + px4(i) * c403(i) + pz4(i) * c503(i)
            's(58, i) = s(58, i) + py4(i) * c204(i) + px4(i) * c404(i) + pz4(i) * c504(i)
            's(59, i) = s(59, i) + py4(i) * c205(i) + px4(i) * c405(i) + pz4(i) * c505(i)
            s(51, i) = s(51, i) + px4(i) * c106(i) + py4(i) * c406(i) + pz4(i) * c606(i)
            s(52, i) = s(52, i) + px4(i) * c107(i) + py4(i) * c407(i) + pz4(i) * c607(i)
            s(53, i) = s(53, i) + px4(i) * c108(i) + py4(i) * c408(i) + pz4(i) * c608(i)
            s(54, i) = s(54, i) + px4(i) * c109(i) + py4(i) * c409(i) + pz4(i) * c609(i)
            s(55, i) = s(55, i) + px4(i) * c110(i) + py4(i) * c410(i) + pz4(i) * c610(i)
            s(56, i) = s(56, i) + py4(i) * c201(i) + px4(i) * c401(i) + pz4(i) * c501(i)
            s(57, i) = s(57, i) + py4(i) * c202(i) + px4(i) * c402(i) + pz4(i) * c502(i)
            s(58, i) = s(58, i) + py4(i) * c203(i) + px4(i) * c403(i) + pz4(i) * c503(i)
            s(59, i) = s(59, i) + py4(i) * c204(i) + px4(i) * c404(i) + pz4(i) * c504(i)
            s(60, i) = s(60, i) + py4(i) * c205(i) + px4(i) * c405(i) + pz4(i) * c505(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 
            's(60, i) = s(60, i) + py4(i) * c206(i) + px4(i) * c406(i) + pz4(i) * c506(i)
            's(61, i) = s(61, i) + py4(i) * c207(i) + px4(i) * c407(i) + pz4(i) * c507(i)
            's(62, i) = s(62, i) + py4(i) * c208(i) + px4(i) * c408(i) + pz4(i) * c508(i)
            's(63, i) = s(63, i) + py4(i) * c209(i) + px4(i) * c409(i) + pz4(i) * c509(i)
            's(64, i) = s(64, i) + py4(i) * c210(i) + px4(i) * c410(i) + pz4(i) * c510(i)
            's(65, i) = s(65, i) + py4(i) * c211(i) + px4(i) * c411(i) + pz4(i) * c511(i)
            's(66, i) = s(66, i) + pz4(i) * c301(i) + py4(i) * c501(i) + px4(i) * c601(i)
            's(67, i) = s(67, i) + pz4(i) * c302(i) + py4(i) * c502(i) + px4(i) * c602(i)
            's(68, i) = s(68, i) + pz4(i) * c303(i) + py4(i) * c503(i) + px4(i) * c603(i)
            's(69, i) = s(69, i) + pz4(i) * c304(i) + py4(i) * c504(i) + px4(i) * c604(i)
            s(61, i) = s(61, i) + py4(i) * c206(i) + px4(i) * c406(i) + pz4(i) * c506(i)
            s(62, i) = s(62, i) + py4(i) * c207(i) + px4(i) * c407(i) + pz4(i) * c507(i)
            s(63, i) = s(63, i) + py4(i) * c208(i) + px4(i) * c408(i) + pz4(i) * c508(i)
            s(64, i) = s(64, i) + py4(i) * c209(i) + px4(i) * c409(i) + pz4(i) * c509(i)
            s(65, i) = s(65, i) + py4(i) * c210(i) + px4(i) * c410(i) + pz4(i) * c510(i)
            s(66, i) = s(66, i) + py4(i) * c211(i) + px4(i) * c411(i) + pz4(i) * c511(i)
            s(67, i) = s(67, i) + pz4(i) * c301(i) + py4(i) * c501(i) + px4(i) * c601(i)
            s(68, i) = s(68, i) + pz4(i) * c302(i) + py4(i) * c502(i) + px4(i) * c602(i)
            s(69, i) = s(69, i) + pz4(i) * c303(i) + py4(i) * c503(i) + px4(i) * c603(i)
            s(70, i) = s(70, i) + pz4(i) * c304(i) + py4(i) * c504(i) + px4(i) * c604(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 
            's(70, i) = s(70, i) + pz4(i) * c305(i) + py4(i) * c505(i) + px4(i) * c605(i)
            's(71, i) = s(71, i) + pz4(i) * c306(i) + py4(i) * c506(i) + px4(i) * c606(i)
            's(72, i) = s(72, i) + pz4(i) * c307(i) + py4(i) * c507(i) + px4(i) * c607(i)
            's(73, i) = s(73, i) + pz4(i) * c308(i) + py4(i) * c508(i) + px4(i) * c608(i)
            's(74, i) = s(74, i) + pz4(i) * c309(i) + py4(i) * c509(i) + px4(i) * c609(i)
            's(75, i) = s(75, i) + pz4(i) * c310(i) + py4(i) * c510(i) + px4(i) * c610(i)
            's(76, i) = s(76, i) + pz4(i) * c311(i) + py4(i) * c511(i) + px4(i) * c611(i)
            's(77, i) = s(77, i) + pz4(i) * c312(i) + py4(i) * c512(i) + px4(i) * c612(i)
            's(78, i) = s(78, i) + px5(i) * c101(i) + py5(i) * c401(i) + pz5(i) * c601(i)
            's(79, i) = s(79, i) + px5(i) * c102(i) + py5(i) * c402(i) + pz5(i) * c602(i)
            s(71, i) = s(71, i) + pz4(i) * c305(i) + py4(i) * c505(i) + px4(i) * c605(i)
            s(72, i) = s(72, i) + pz4(i) * c306(i) + py4(i) * c506(i) + px4(i) * c606(i)
            s(73, i) = s(73, i) + pz4(i) * c307(i) + py4(i) * c507(i) + px4(i) * c607(i)
            s(74, i) = s(74, i) + pz4(i) * c308(i) + py4(i) * c508(i) + px4(i) * c608(i)
            s(75, i) = s(75, i) + pz4(i) * c309(i) + py4(i) * c509(i) + px4(i) * c609(i)
            s(76, i) = s(76, i) + pz4(i) * c310(i) + py4(i) * c510(i) + px4(i) * c610(i)
            s(77, i) = s(77, i) + pz4(i) * c311(i) + py4(i) * c511(i) + px4(i) * c611(i)
            s(78, i) = s(78, i) + pz4(i) * c312(i) + py4(i) * c512(i) + px4(i) * c612(i)
            s(79, i) = s(79, i) + px5(i) * c101(i) + py5(i) * c401(i) + pz5(i) * c601(i)
            s(80, i) = s(80, i) + px5(i) * c102(i) + py5(i) * c402(i) + pz5(i) * c602(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(80, i) = s(80, i) + px5(i) * c103(i) + py5(i) * c403(i) + pz5(i) * c603(i)
            's(81, i) = s(81, i) + px5(i) * c104(i) + py5(i) * c404(i) + pz5(i) * c604(i)
            's(82, i) = s(82, i) + px5(i) * c105(i) + py5(i) * c405(i) + pz5(i) * c605(i)
            's(83, i) = s(83, i) + px5(i) * c106(i) + py5(i) * c406(i) + pz5(i) * c606(i)
            's(84, i) = s(84, i) + px5(i) * c107(i) + py5(i) * c407(i) + pz5(i) * c607(i)
            's(85, i) = s(85, i) + px5(i) * c108(i) + py5(i) * c408(i) + pz5(i) * c608(i)
            's(86, i) = s(86, i) + px5(i) * c109(i) + py5(i) * c409(i) + pz5(i) * c609(i)
            's(87, i) = s(87, i) + px5(i) * c110(i) + py5(i) * c410(i) + pz5(i) * c610(i)
            's(88, i) = s(88, i) + px5(i) * c111(i) + py5(i) * c411(i) + pz5(i) * c611(i)
            's(89, i) = s(89, i) + px5(i) * c112(i) + py5(i) * c412(i) + pz5(i) * c612(i)
            s(81, i) = s(81, i) + px5(i) * c103(i) + py5(i) * c403(i) + pz5(i) * c603(i)
            s(82, i) = s(82, i) + px5(i) * c104(i) + py5(i) * c404(i) + pz5(i) * c604(i)
            s(83, i) = s(83, i) + px5(i) * c105(i) + py5(i) * c405(i) + pz5(i) * c605(i)
            s(84, i) = s(84, i) + px5(i) * c106(i) + py5(i) * c406(i) + pz5(i) * c606(i)
            s(85, i) = s(85, i) + px5(i) * c107(i) + py5(i) * c407(i) + pz5(i) * c607(i)
            s(86, i) = s(86, i) + px5(i) * c108(i) + py5(i) * c408(i) + pz5(i) * c608(i)
            s(87, i) = s(87, i) + px5(i) * c109(i) + py5(i) * c409(i) + pz5(i) * c609(i)
            s(88, i) = s(88, i) + px5(i) * c110(i) + py5(i) * c410(i) + pz5(i) * c610(i)
            s(89, i) = s(89, i) + px5(i) * c111(i) + py5(i) * c411(i) + pz5(i) * c611(i)
            s(90, i) = s(90, i) + px5(i) * c112(i) + py5(i) * c412(i) + pz5(i) * c612(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(90, i) = s(90, i) + px5(i) * c113(i) + py5(i) * c413(i) + pz5(i) * c613(i)
            's(91, i) = s(91, i) + py5(i) * c201(i) + px5(i) * c401(i) + pz5(i) * c501(i)
            's(92, i) = s(92, i) + py5(i) * c202(i) + px5(i) * c402(i) + pz5(i) * c502(i)
            's(93, i) = s(93, i) + py5(i) * c203(i) + px5(i) * c403(i) + pz5(i) * c503(i)
            's(94, i) = s(94, i) + py5(i) * c204(i) + px5(i) * c404(i) + pz5(i) * c504(i)
            's(95, i) = s(95, i) + py5(i) * c205(i) + px5(i) * c405(i) + pz5(i) * c505(i)
            's(96, i) = s(96, i) + py5(i) * c206(i) + px5(i) * c406(i) + pz5(i) * c506(i)
            's(97, i) = s(97, i) + py5(i) * c207(i) + px5(i) * c407(i) + pz5(i) * c507(i)
            's(98, i) = s(98, i) + py5(i) * c208(i) + px5(i) * c408(i) + pz5(i) * c508(i)
            's(99, i) = s(99, i) + py5(i) * c209(i) + px5(i) * c409(i) + pz5(i) * c509(i)
            s(91, i) = s(91, i) + px5(i) * c113(i) + py5(i) * c413(i) + pz5(i) * c613(i)
            s(92, i) = s(92, i) + py5(i) * c201(i) + px5(i) * c401(i) + pz5(i) * c501(i)
            s(93, i) = s(93, i) + py5(i) * c202(i) + px5(i) * c402(i) + pz5(i) * c502(i)
            s(94, i) = s(94, i) + py5(i) * c203(i) + px5(i) * c403(i) + pz5(i) * c503(i)
            s(95, i) = s(95, i) + py5(i) * c204(i) + px5(i) * c404(i) + pz5(i) * c504(i)
            s(96, i) = s(96, i) + py5(i) * c205(i) + px5(i) * c405(i) + pz5(i) * c505(i)
            s(97, i) = s(97, i) + py5(i) * c206(i) + px5(i) * c406(i) + pz5(i) * c506(i)
            s(98, i) = s(98, i) + py5(i) * c207(i) + px5(i) * c407(i) + pz5(i) * c507(i)
            s(99, i) = s(99, i) + py5(i) * c208(i) + px5(i) * c408(i) + pz5(i) * c508(i)
            s(100, i) = s(100, i) + py5(i) * c209(i) + px5(i) * c409(i) + pz5(i) * c509(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(100, i) = s(100, i) + py5(i) * c210(i) + px5(i) * c410(i) + pz5(i) * c510(i)
            's(101, i) = s(101, i) + py5(i) * c211(i) + px5(i) * c411(i) + pz5(i) * c511(i)
            's(102, i) = s(102, i) + py5(i) * c212(i) + px5(i) * c412(i) + pz5(i) * c512(i)
            's(103, i) = s(103, i) + py5(i) * c213(i) + px5(i) * c413(i) + pz5(i) * c513(i)
            's(104, i) = s(104, i) + py5(i) * c214(i) + px5(i) * c414(i) + pz5(i) * c514(i)
            's(105, i) = s(105, i) + pz5(i) * c301(i) + py5(i) * c501(i) + px5(i) * c601(i)
            's(106, i) = s(106, i) + pz5(i) * c302(i) + py5(i) * c502(i) + px5(i) * c602(i)
            's(107, i) = s(107, i) + pz5(i) * c303(i) + py5(i) * c503(i) + px5(i) * c603(i)
            's(108, i) = s(108, i) + pz5(i) * c304(i) + py5(i) * c504(i) + px5(i) * c604(i)
            's(109, i) = s(109, i) + pz5(i) * c305(i) + py5(i) * c505(i) + px5(i) * c605(i)
            s(101, i) = s(101, i) + py5(i) * c210(i) + px5(i) * c410(i) + pz5(i) * c510(i)
            s(102, i) = s(102, i) + py5(i) * c211(i) + px5(i) * c411(i) + pz5(i) * c511(i)
            s(103, i) = s(103, i) + py5(i) * c212(i) + px5(i) * c412(i) + pz5(i) * c512(i)
            s(104, i) = s(104, i) + py5(i) * c213(i) + px5(i) * c413(i) + pz5(i) * c513(i)
            s(105, i) = s(105, i) + py5(i) * c214(i) + px5(i) * c414(i) + pz5(i) * c514(i)
            s(106, i) = s(106, i) + pz5(i) * c301(i) + py5(i) * c501(i) + px5(i) * c601(i)
            s(107, i) = s(107, i) + pz5(i) * c302(i) + py5(i) * c502(i) + px5(i) * c602(i)
            s(108, i) = s(108, i) + pz5(i) * c303(i) + py5(i) * c503(i) + px5(i) * c603(i)
            s(109, i) = s(109, i) + pz5(i) * c304(i) + py5(i) * c504(i) + px5(i) * c604(i)
            s(110, i) = s(110, i) + pz5(i) * c305(i) + py5(i) * c505(i) + px5(i) * c605(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(110, i) = s(110, i) + pz5(i) * c306(i) + py5(i) * c506(i) + px5(i) * c606(i)
            's(111, i) = s(111, i) + pz5(i) * c307(i) + py5(i) * c507(i) + px5(i) * c607(i)
            's(112, i) = s(112, i) + pz5(i) * c308(i) + py5(i) * c508(i) + px5(i) * c608(i)
            's(113, i) = s(113, i) + pz5(i) * c309(i) + py5(i) * c509(i) + px5(i) * c609(i)
            's(114, i) = s(114, i) + pz5(i) * c310(i) + py5(i) * c510(i) + px5(i) * c610(i)
            's(115, i) = s(115, i) + pz5(i) * c311(i) + py5(i) * c511(i) + px5(i) * c611(i)
            's(116, i) = s(116, i) + pz5(i) * c312(i) + py5(i) * c512(i) + px5(i) * c612(i)
            's(117, i) = s(117, i) + pz5(i) * c313(i) + py5(i) * c513(i) + px5(i) * c613(i)
            's(118, i) = s(118, i) + pz5(i) * c314(i) + py5(i) * c514(i) + px5(i) * c614(i)
            's(119, i) = s(119, i) + pz5(i) * c315(i) + py5(i) * c515(i) + px5(i) * c615(i)
            s(111, i) = s(111, i) + pz5(i) * c306(i) + py5(i) * c506(i) + px5(i) * c606(i)
            s(112, i) = s(112, i) + pz5(i) * c307(i) + py5(i) * c507(i) + px5(i) * c607(i)
            s(113, i) = s(113, i) + pz5(i) * c308(i) + py5(i) * c508(i) + px5(i) * c608(i)
            s(114, i) = s(114, i) + pz5(i) * c309(i) + py5(i) * c509(i) + px5(i) * c609(i)
            s(115, i) = s(115, i) + pz5(i) * c310(i) + py5(i) * c510(i) + px5(i) * c610(i)
            s(116, i) = s(116, i) + pz5(i) * c311(i) + py5(i) * c511(i) + px5(i) * c611(i)
            s(117, i) = s(117, i) + pz5(i) * c312(i) + py5(i) * c512(i) + px5(i) * c612(i)
            s(118, i) = s(118, i) + pz5(i) * c313(i) + py5(i) * c513(i) + px5(i) * c613(i)
            s(119, i) = s(119, i) + pz5(i) * c314(i) + py5(i) * c514(i) + px5(i) * c614(i)
            s(120, i) = s(120, i) + pz5(i) * c315(i) + py5(i) * c515(i) + px5(i) * c615(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 
            's(120, i) = s(120, i) + px6(i) * c101(i) + py6(i) * c401(i) + pz6(i) * c601(i)
            's(121, i) = s(121, i) + px6(i) * c102(i) + py6(i) * c402(i) + pz6(i) * c602(i)
            's(122, i) = s(122, i) + px6(i) * c103(i) + py6(i) * c403(i) + pz6(i) * c603(i)
            's(123, i) = s(123, i) + px6(i) * c104(i) + py6(i) * c404(i) + pz6(i) * c604(i)
            's(124, i) = s(124, i) + px6(i) * c105(i) + py6(i) * c405(i) + pz6(i) * c605(i)
            's(125, i) = s(125, i) + px6(i) * c106(i) + py6(i) * c406(i) + pz6(i) * c606(i)
            's(126, i) = s(126, i) + px6(i) * c107(i) + py6(i) * c407(i) + pz6(i) * c607(i)
            's(127, i) = s(127, i) + px6(i) * c108(i) + py6(i) * c408(i) + pz6(i) * c608(i)
            's(128, i) = s(128, i) + px6(i) * c109(i) + py6(i) * c409(i) + pz6(i) * c609(i)
            's(129, i) = s(129, i) + px6(i) * c110(i) + py6(i) * c410(i) + pz6(i) * c610(i)
            s(121, i) = s(121, i) + px6(i) * c101(i) + py6(i) * c401(i) + pz6(i) * c601(i)
            s(122, i) = s(122, i) + px6(i) * c102(i) + py6(i) * c402(i) + pz6(i) * c602(i)
            s(123, i) = s(123, i) + px6(i) * c103(i) + py6(i) * c403(i) + pz6(i) * c603(i)
            s(124, i) = s(124, i) + px6(i) * c104(i) + py6(i) * c404(i) + pz6(i) * c604(i)
            s(125, i) = s(125, i) + px6(i) * c105(i) + py6(i) * c405(i) + pz6(i) * c605(i)
            s(126, i) = s(126, i) + px6(i) * c106(i) + py6(i) * c406(i) + pz6(i) * c606(i)
            s(127, i) = s(127, i) + px6(i) * c107(i) + py6(i) * c407(i) + pz6(i) * c607(i)
            s(128, i) = s(128, i) + px6(i) * c108(i) + py6(i) * c408(i) + pz6(i) * c608(i)
            s(129, i) = s(129, i) + px6(i) * c109(i) + py6(i) * c409(i) + pz6(i) * c609(i)
            s(130, i) = s(130, i) + px6(i) * c110(i) + py6(i) * c410(i) + pz6(i) * c610(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 
            's(130, i) = s(130, i) + px6(i) * c111(i) + py6(i) * c411(i) + pz6(i) * c611(i)
            's(131, i) = s(131, i) + px6(i) * c112(i) + py6(i) * c412(i) + pz6(i) * c612(i)
            's(132, i) = s(132, i) + px6(i) * c113(i) + py6(i) * c413(i) + pz6(i) * c613(i)
            's(133, i) = s(133, i) + px6(i) * c114(i) + py6(i) * c414(i) + pz6(i) * c614(i)
            's(134, i) = s(134, i) + px6(i) * c115(i) + py6(i) * c415(i) + pz6(i) * c615(i)
            's(135, i) = s(135, i) + px6(i) * c116(i) + py6(i) * c416(i) + pz6(i) * c616(i)
            's(136, i) = s(136, i) + py6(i) * c201(i) + px6(i) * c401(i) + pz6(i) * c501(i)
            's(137, i) = s(137, i) + py6(i) * c202(i) + px6(i) * c402(i) + pz6(i) * c502(i)
            's(138, i) = s(138, i) + py6(i) * c203(i) + px6(i) * c403(i) + pz6(i) * c503(i)
            's(139, i) = s(139, i) + py6(i) * c204(i) + px6(i) * c404(i) + pz6(i) * c504(i)
            s(131, i) = s(131, i) + px6(i) * c111(i) + py6(i) * c411(i) + pz6(i) * c611(i)
            s(132, i) = s(132, i) + px6(i) * c112(i) + py6(i) * c412(i) + pz6(i) * c612(i)
            s(133, i) = s(133, i) + px6(i) * c113(i) + py6(i) * c413(i) + pz6(i) * c613(i)
            s(134, i) = s(134, i) + px6(i) * c114(i) + py6(i) * c414(i) + pz6(i) * c614(i)
            s(135, i) = s(135, i) + px6(i) * c115(i) + py6(i) * c415(i) + pz6(i) * c615(i)
            s(136, i) = s(136, i) + px6(i) * c116(i) + py6(i) * c416(i) + pz6(i) * c616(i)
            s(137, i) = s(137, i) + py6(i) * c201(i) + px6(i) * c401(i) + pz6(i) * c501(i)
            s(138, i) = s(138, i) + py6(i) * c202(i) + px6(i) * c402(i) + pz6(i) * c502(i)
            s(139, i) = s(139, i) + py6(i) * c203(i) + px6(i) * c403(i) + pz6(i) * c503(i)
            s(140, i) = s(140, i) + py6(i) * c204(i) + px6(i) * c404(i) + pz6(i) * c504(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            's(140, i) = s(140, i) + py6(i) * c205(i) + px6(i) * c405(i) + pz6(i) * c505(i)
            's(141, i) = s(141, i) + py6(i) * c206(i) + px6(i) * c406(i) + pz6(i) * c506(i)
            's(142, i) = s(142, i) + py6(i) * c207(i) + px6(i) * c407(i) + pz6(i) * c507(i)
            's(143, i) = s(143, i) + py6(i) * c208(i) + px6(i) * c408(i) + pz6(i) * c508(i)
            's(144, i) = s(144, i) + py6(i) * c209(i) + px6(i) * c409(i) + pz6(i) * c509(i)
            's(145, i) = s(145, i) + py6(i) * c210(i) + px6(i) * c410(i) + pz6(i) * c510(i)
            's(146, i) = s(146, i) + py6(i) * c211(i) + px6(i) * c411(i) + pz6(i) * c511(i)
            's(147, i) = s(147, i) + py6(i) * c212(i) + px6(i) * c412(i) + pz6(i) * c512(i)
            's(148, i) = s(148, i) + py6(i) * c213(i) + px6(i) * c413(i) + pz6(i) * c513(i)
            's(149, i) = s(149, i) + py6(i) * c214(i) + px6(i) * c414(i) + pz6(i) * c514(i)
            s(141, i) = s(141, i) + py6(i) * c205(i) + px6(i) * c405(i) + pz6(i) * c505(i)
            s(142, i) = s(142, i) + py6(i) * c206(i) + px6(i) * c406(i) + pz6(i) * c506(i)
            s(143, i) = s(143, i) + py6(i) * c207(i) + px6(i) * c407(i) + pz6(i) * c507(i)
            s(144, i) = s(144, i) + py6(i) * c208(i) + px6(i) * c408(i) + pz6(i) * c508(i)
            s(145, i) = s(145, i) + py6(i) * c209(i) + px6(i) * c409(i) + pz6(i) * c509(i)
            s(146, i) = s(146, i) + py6(i) * c210(i) + px6(i) * c410(i) + pz6(i) * c510(i)
            s(147, i) = s(147, i) + py6(i) * c211(i) + px6(i) * c411(i) + pz6(i) * c511(i)
            s(148, i) = s(148, i) + py6(i) * c212(i) + px6(i) * c412(i) + pz6(i) * c512(i)
            s(149, i) = s(149, i) + py6(i) * c213(i) + px6(i) * c413(i) + pz6(i) * c513(i)
            s(150, i) = s(150, i) + py6(i) * c214(i) + px6(i) * c414(i) + pz6(i) * c514(i)
            ' YC 102418 END

        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine fstif0(s)
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c
'c                        t
'c===> module to compute b *d*b product for first 150 coefficients
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
'      s(1,i)  =s(1,i)  +px1(i)*c101(i)+py1(i)*c401(i)+pz1(i)*c601(i)
'      s(2,i)  =s(2,i)  +py1(i)*c201(i)+px1(i)*c401(i)+pz1(i)*c501(i)
'      s(3,i)  =s(3,i)  +py1(i)*c202(i)+px1(i)*c402(i)+pz1(i)*c502(i)
'      s(4,i)  =s(4,i)  +pz1(i)*c301(i)+py1(i)*c501(i)+px1(i)*c601(i)
'      s(5,i)  =s(5,i)  +pz1(i)*c302(i)+py1(i)*c502(i)+px1(i)*c602(i)
'      s(6,i)  =s(6,i)  +pz1(i)*c303(i)+py1(i)*c503(i)+px1(i)*c603(i)
'      s(7,i)  =s(7,i)  +px2(i)*c101(i)+py2(i)*c401(i)+pz2(i)*c601(i)
'      s(8,i)  =s(8,i)  +px2(i)*c102(i)+py2(i)*c402(i)+pz2(i)*c602(i)
'      s(9,i)  =s(9,i)  +px2(i)*c103(i)+py2(i)*c403(i)+pz2(i)*c603(i)
'      s(10,i) =s(10,i) +px2(i)*c104(i)+py2(i)*c404(i)+pz2(i)*c604(i)
'   10 continue
'      do 20 i=lft,llt
'      s(11,i) =s(11,i) +py2(i)*c201(i)+px2(i)*c401(i)+pz2(i)*c501(i)
'      s(12,i) =s(12,i) +py2(i)*c202(i)+px2(i)*c402(i)+pz2(i)*c502(i)
'      s(13,i) =s(13,i) +py2(i)*c203(i)+px2(i)*c403(i)+pz2(i)*c503(i)
'      s(14,i) =s(14,i) +py2(i)*c204(i)+px2(i)*c404(i)+pz2(i)*c504(i)
'      s(15,i) =s(15,i) +py2(i)*c205(i)+px2(i)*c405(i)+pz2(i)*c505(i)
'      s(16,i) =s(16,i) +pz2(i)*c301(i)+py2(i)*c501(i)+px2(i)*c601(i)
'      s(17,i) =s(17,i) +pz2(i)*c302(i)+py2(i)*c502(i)+px2(i)*c602(i)
'      s(18,i) =s(18,i) +pz2(i)*c303(i)+py2(i)*c503(i)+px2(i)*c603(i)
'      s(19,i) =s(19,i) +pz2(i)*c304(i)+py2(i)*c504(i)+px2(i)*c604(i)
'      s(20,i) =s(20,i) +pz2(i)*c305(i)+py2(i)*c505(i)+px2(i)*c605(i)
'   20 continue
'      do 30 i=lft,llt
'      s(21,i) =s(21,i) +pz2(i)*c306(i)+py2(i)*c506(i)+px2(i)*c606(i)
'      s(22,i) =s(22,i) +px3(i)*c101(i)+py3(i)*c401(i)+pz3(i)*c601(i)
'      s(23,i) =s(23,i) +px3(i)*c102(i)+py3(i)*c402(i)+pz3(i)*c602(i)
'      s(24,i) =s(24,i) +px3(i)*c103(i)+py3(i)*c403(i)+pz3(i)*c603(i)
'      s(25,i) =s(25,i) +px3(i)*c104(i)+py3(i)*c404(i)+pz3(i)*c604(i)
'      s(26,i) =s(26,i) +px3(i)*c105(i)+py3(i)*c405(i)+pz3(i)*c605(i)
'      s(27,i) =s(27,i) +px3(i)*c106(i)+py3(i)*c406(i)+pz3(i)*c606(i)
'      s(28,i) =s(28,i) +px3(i)*c107(i)+py3(i)*c407(i)+pz3(i)*c607(i)
'      s(29,i) =s(29,i) +py3(i)*c201(i)+px3(i)*c401(i)+pz3(i)*c501(i)
'      s(30,i) =s(30,i) +py3(i)*c202(i)+px3(i)*c402(i)+pz3(i)*c502(i)
'   30 continue
'      do 40 i=lft,llt
'      s(31,i) =s(31,i) +py3(i)*c203(i)+px3(i)*c403(i)+pz3(i)*c503(i)
'      s(32,i) =s(32,i) +py3(i)*c204(i)+px3(i)*c404(i)+pz3(i)*c504(i)
'      s(33,i) =s(33,i) +py3(i)*c205(i)+px3(i)*c405(i)+pz3(i)*c505(i)
'      s(34,i) =s(34,i) +py3(i)*c206(i)+px3(i)*c406(i)+pz3(i)*c506(i)
'      s(35,i) =s(35,i) +py3(i)*c207(i)+px3(i)*c407(i)+pz3(i)*c507(i)
'      s(36,i) =s(36,i) +py3(i)*c208(i)+px3(i)*c408(i)+pz3(i)*c508(i)
'      s(37,i) =s(37,i) +pz3(i)*c301(i)+py3(i)*c501(i)+px3(i)*c601(i)
'      s(38,i) =s(38,i) +pz3(i)*c302(i)+py3(i)*c502(i)+px3(i)*c602(i)
'      s(39,i) =s(39,i) +pz3(i)*c303(i)+py3(i)*c503(i)+px3(i)*c603(i)
'      s(40,i) =s(40,i) +pz3(i)*c304(i)+py3(i)*c504(i)+px3(i)*c604(i)
'   40 continue
'      do 50 i=lft,llt
'      s(41,i) =s(41,i) +pz3(i)*c305(i)+py3(i)*c505(i)+px3(i)*c605(i)
'      s(42,i) =s(42,i) +pz3(i)*c306(i)+py3(i)*c506(i)+px3(i)*c606(i)
'      s(43,i) =s(43,i) +pz3(i)*c307(i)+py3(i)*c507(i)+px3(i)*c607(i)
'      s(44,i) =s(44,i) +pz3(i)*c308(i)+py3(i)*c508(i)+px3(i)*c608(i)
'      s(45,i) =s(45,i) +pz3(i)*c309(i)+py3(i)*c509(i)+px3(i)*c609(i)
'      s(46,i) =s(46,i) +px4(i)*c101(i)+py4(i)*c401(i)+pz4(i)*c601(i)
'      s(47,i) =s(47,i) +px4(i)*c102(i)+py4(i)*c402(i)+pz4(i)*c602(i)
'      s(48,i) =s(48,i) +px4(i)*c103(i)+py4(i)*c403(i)+pz4(i)*c603(i)
'      s(49,i) =s(49,i) +px4(i)*c104(i)+py4(i)*c404(i)+pz4(i)*c604(i)
'      s(50,i) =s(50,i) +px4(i)*c105(i)+py4(i)*c405(i)+pz4(i)*c605(i)
'   50 continue
'      do 60 i=lft,llt
'      s(51,i) =s(51,i) +px4(i)*c106(i)+py4(i)*c406(i)+pz4(i)*c606(i)
'      s(52,i) =s(52,i) +px4(i)*c107(i)+py4(i)*c407(i)+pz4(i)*c607(i)
'      s(53,i) =s(53,i) +px4(i)*c108(i)+py4(i)*c408(i)+pz4(i)*c608(i)
'      s(54,i) =s(54,i) +px4(i)*c109(i)+py4(i)*c409(i)+pz4(i)*c609(i)
'      s(55,i) =s(55,i) +px4(i)*c110(i)+py4(i)*c410(i)+pz4(i)*c610(i)
'      s(56,i) =s(56,i) +py4(i)*c201(i)+px4(i)*c401(i)+pz4(i)*c501(i)
'      s(57,i) =s(57,i) +py4(i)*c202(i)+px4(i)*c402(i)+pz4(i)*c502(i)
'      s(58,i) =s(58,i) +py4(i)*c203(i)+px4(i)*c403(i)+pz4(i)*c503(i)
'      s(59,i) =s(59,i) +py4(i)*c204(i)+px4(i)*c404(i)+pz4(i)*c504(i)
'      s(60,i) =s(60,i) +py4(i)*c205(i)+px4(i)*c405(i)+pz4(i)*c505(i)
'   60 continue
'      do 70 i=lft,llt
'      s(61,i) =s(61,i) +py4(i)*c206(i)+px4(i)*c406(i)+pz4(i)*c506(i)
'      s(62,i) =s(62,i) +py4(i)*c207(i)+px4(i)*c407(i)+pz4(i)*c507(i)
'      s(63,i) =s(63,i) +py4(i)*c208(i)+px4(i)*c408(i)+pz4(i)*c508(i)
'      s(64,i) =s(64,i) +py4(i)*c209(i)+px4(i)*c409(i)+pz4(i)*c509(i)
'      s(65,i) =s(65,i) +py4(i)*c210(i)+px4(i)*c410(i)+pz4(i)*c510(i)
'      s(66,i) =s(66,i) +py4(i)*c211(i)+px4(i)*c411(i)+pz4(i)*c511(i)
'      s(67,i) =s(67,i) +pz4(i)*c301(i)+py4(i)*c501(i)+px4(i)*c601(i)
'      s(68,i) =s(68,i) +pz4(i)*c302(i)+py4(i)*c502(i)+px4(i)*c602(i)
'      s(69,i) =s(69,i) +pz4(i)*c303(i)+py4(i)*c503(i)+px4(i)*c603(i)
'      s(70,i) =s(70,i) +pz4(i)*c304(i)+py4(i)*c504(i)+px4(i)*c604(i)
'   70 continue
'      do 80 i=lft,llt
'      s(71,i) =s(71,i) +pz4(i)*c305(i)+py4(i)*c505(i)+px4(i)*c605(i)
'      s(72,i) =s(72,i) +pz4(i)*c306(i)+py4(i)*c506(i)+px4(i)*c606(i)
'      s(73,i) =s(73,i) +pz4(i)*c307(i)+py4(i)*c507(i)+px4(i)*c607(i)
'      s(74,i) =s(74,i) +pz4(i)*c308(i)+py4(i)*c508(i)+px4(i)*c608(i)
'      s(75,i) =s(75,i) +pz4(i)*c309(i)+py4(i)*c509(i)+px4(i)*c609(i)
'      s(76,i) =s(76,i) +pz4(i)*c310(i)+py4(i)*c510(i)+px4(i)*c610(i)
'      s(77,i) =s(77,i) +pz4(i)*c311(i)+py4(i)*c511(i)+px4(i)*c611(i)
'      s(78,i) =s(78,i) +pz4(i)*c312(i)+py4(i)*c512(i)+px4(i)*c612(i)
'      s(79,i) =s(79,i) +px5(i)*c101(i)+py5(i)*c401(i)+pz5(i)*c601(i)
'      s(80,i) =s(80,i) +px5(i)*c102(i)+py5(i)*c402(i)+pz5(i)*c602(i)
'   80 continue
'      do 90 i=lft,llt
'      s(81,i) =s(81,i) +px5(i)*c103(i)+py5(i)*c403(i)+pz5(i)*c603(i)
'      s(82,i) =s(82,i) +px5(i)*c104(i)+py5(i)*c404(i)+pz5(i)*c604(i)
'      s(83,i) =s(83,i) +px5(i)*c105(i)+py5(i)*c405(i)+pz5(i)*c605(i)
'      s(84,i) =s(84,i) +px5(i)*c106(i)+py5(i)*c406(i)+pz5(i)*c606(i)
'      s(85,i) =s(85,i) +px5(i)*c107(i)+py5(i)*c407(i)+pz5(i)*c607(i)
'      s(86,i) =s(86,i) +px5(i)*c108(i)+py5(i)*c408(i)+pz5(i)*c608(i)
'      s(87,i) =s(87,i) +px5(i)*c109(i)+py5(i)*c409(i)+pz5(i)*c609(i)
'      s(88,i) =s(88,i) +px5(i)*c110(i)+py5(i)*c410(i)+pz5(i)*c610(i)
'      s(89,i) =s(89,i) +px5(i)*c111(i)+py5(i)*c411(i)+pz5(i)*c611(i)
'      s(90,i) =s(90,i) +px5(i)*c112(i)+py5(i)*c412(i)+pz5(i)*c612(i)
'   90 continue
'      do 100 i=lft,llt
'      s(91,i) =s(91,i) +px5(i)*c113(i)+py5(i)*c413(i)+pz5(i)*c613(i)
'      s(92,i) =s(92,i) +py5(i)*c201(i)+px5(i)*c401(i)+pz5(i)*c501(i)
'      s(93,i) =s(93,i) +py5(i)*c202(i)+px5(i)*c402(i)+pz5(i)*c502(i)
'      s(94,i) =s(94,i) +py5(i)*c203(i)+px5(i)*c403(i)+pz5(i)*c503(i)
'      s(95,i) =s(95,i) +py5(i)*c204(i)+px5(i)*c404(i)+pz5(i)*c504(i)
'      s(96,i) =s(96,i) +py5(i)*c205(i)+px5(i)*c405(i)+pz5(i)*c505(i)
'      s(97,i) =s(97,i) +py5(i)*c206(i)+px5(i)*c406(i)+pz5(i)*c506(i)
'      s(98,i) =s(98,i) +py5(i)*c207(i)+px5(i)*c407(i)+pz5(i)*c507(i)
'      s(99,i) =s(99,i) +py5(i)*c208(i)+px5(i)*c408(i)+pz5(i)*c508(i)
'      s(100,i)=s(100,i)+py5(i)*c209(i)+px5(i)*c409(i)+pz5(i)*c509(i)
'  100 continue
'      do 110 i=lft,llt
'      s(101,i)=s(101,i)+py5(i)*c210(i)+px5(i)*c410(i)+pz5(i)*c510(i)
'      s(102,i)=s(102,i)+py5(i)*c211(i)+px5(i)*c411(i)+pz5(i)*c511(i)
'      s(103,i)=s(103,i)+py5(i)*c212(i)+px5(i)*c412(i)+pz5(i)*c512(i)
'      s(104,i)=s(104,i)+py5(i)*c213(i)+px5(i)*c413(i)+pz5(i)*c513(i)
'      s(105,i)=s(105,i)+py5(i)*c214(i)+px5(i)*c414(i)+pz5(i)*c514(i)
'      s(106,i)=s(106,i)+pz5(i)*c301(i)+py5(i)*c501(i)+px5(i)*c601(i)
'      s(107,i)=s(107,i)+pz5(i)*c302(i)+py5(i)*c502(i)+px5(i)*c602(i)
'      s(108,i)=s(108,i)+pz5(i)*c303(i)+py5(i)*c503(i)+px5(i)*c603(i)
'      s(109,i)=s(109,i)+pz5(i)*c304(i)+py5(i)*c504(i)+px5(i)*c604(i)
'      s(110,i)=s(110,i)+pz5(i)*c305(i)+py5(i)*c505(i)+px5(i)*c605(i)
'  110 continue
'      do 120 i=lft,llt
'      s(111,i)=s(111,i)+pz5(i)*c306(i)+py5(i)*c506(i)+px5(i)*c606(i)
'      s(112,i)=s(112,i)+pz5(i)*c307(i)+py5(i)*c507(i)+px5(i)*c607(i)
'      s(113,i)=s(113,i)+pz5(i)*c308(i)+py5(i)*c508(i)+px5(i)*c608(i)
'      s(114,i)=s(114,i)+pz5(i)*c309(i)+py5(i)*c509(i)+px5(i)*c609(i)
'      s(115,i)=s(115,i)+pz5(i)*c310(i)+py5(i)*c510(i)+px5(i)*c610(i)
'      s(116,i)=s(116,i)+pz5(i)*c311(i)+py5(i)*c511(i)+px5(i)*c611(i)
'      s(117,i)=s(117,i)+pz5(i)*c312(i)+py5(i)*c512(i)+px5(i)*c612(i)
'      s(118,i)=s(118,i)+pz5(i)*c313(i)+py5(i)*c513(i)+px5(i)*c613(i)
'      s(119,i)=s(119,i)+pz5(i)*c314(i)+py5(i)*c514(i)+px5(i)*c614(i)
'      s(120,i)=s(120,i)+pz5(i)*c315(i)+py5(i)*c515(i)+px5(i)*c615(i)
'  120 continue
'      do 130 i=lft,llt
'      s(121,i)=s(121,i)+px6(i)*c101(i)+py6(i)*c401(i)+pz6(i)*c601(i)
'      s(122,i)=s(122,i)+px6(i)*c102(i)+py6(i)*c402(i)+pz6(i)*c602(i)
'      s(123,i)=s(123,i)+px6(i)*c103(i)+py6(i)*c403(i)+pz6(i)*c603(i)
'      s(124,i)=s(124,i)+px6(i)*c104(i)+py6(i)*c404(i)+pz6(i)*c604(i)
'      s(125,i)=s(125,i)+px6(i)*c105(i)+py6(i)*c405(i)+pz6(i)*c605(i)
'      s(126,i)=s(126,i)+px6(i)*c106(i)+py6(i)*c406(i)+pz6(i)*c606(i)
'      s(127,i)=s(127,i)+px6(i)*c107(i)+py6(i)*c407(i)+pz6(i)*c607(i)
'      s(128,i)=s(128,i)+px6(i)*c108(i)+py6(i)*c408(i)+pz6(i)*c608(i)
'      s(129,i)=s(129,i)+px6(i)*c109(i)+py6(i)*c409(i)+pz6(i)*c609(i)
'      s(130,i)=s(130,i)+px6(i)*c110(i)+py6(i)*c410(i)+pz6(i)*c610(i)
'  130 continue
'      do 140 i=lft,llt
'      s(131,i)=s(131,i)+px6(i)*c111(i)+py6(i)*c411(i)+pz6(i)*c611(i)
'      s(132,i)=s(132,i)+px6(i)*c112(i)+py6(i)*c412(i)+pz6(i)*c612(i)
'      s(133,i)=s(133,i)+px6(i)*c113(i)+py6(i)*c413(i)+pz6(i)*c613(i)
'      s(134,i)=s(134,i)+px6(i)*c114(i)+py6(i)*c414(i)+pz6(i)*c614(i)
'      s(135,i)=s(135,i)+px6(i)*c115(i)+py6(i)*c415(i)+pz6(i)*c615(i)
'      s(136,i)=s(136,i)+px6(i)*c116(i)+py6(i)*c416(i)+pz6(i)*c616(i)
'      s(137,i)=s(137,i)+py6(i)*c201(i)+px6(i)*c401(i)+pz6(i)*c501(i)
'      s(138,i)=s(138,i)+py6(i)*c202(i)+px6(i)*c402(i)+pz6(i)*c502(i)
'      s(139,i)=s(139,i)+py6(i)*c203(i)+px6(i)*c403(i)+pz6(i)*c503(i)
'      s(140,i)=s(140,i)+py6(i)*c204(i)+px6(i)*c404(i)+pz6(i)*c504(i)
'  140 continue
'      do 150 i=lft,llt
'      s(141,i)=s(141,i)+py6(i)*c205(i)+px6(i)*c405(i)+pz6(i)*c505(i)
'      s(142,i)=s(142,i)+py6(i)*c206(i)+px6(i)*c406(i)+pz6(i)*c506(i)
'      s(143,i)=s(143,i)+py6(i)*c207(i)+px6(i)*c407(i)+pz6(i)*c507(i)
'      s(144,i)=s(144,i)+py6(i)*c208(i)+px6(i)*c408(i)+pz6(i)*c508(i)
'      s(145,i)=s(145,i)+py6(i)*c209(i)+px6(i)*c409(i)+pz6(i)*c509(i)
'      s(146,i)=s(146,i)+py6(i)*c210(i)+px6(i)*c410(i)+pz6(i)*c510(i)
'      s(147,i)=s(147,i)+py6(i)*c211(i)+px6(i)*c411(i)+pz6(i)*c511(i)
'      s(148,i)=s(148,i)+py6(i)*c212(i)+px6(i)*c412(i)+pz6(i)*c512(i)
'      s(149,i)=s(149,i)+py6(i)*c213(i)+px6(i)*c413(i)+pz6(i)*c513(i)
'      s(150,i)=s(150,i)+py6(i)*c214(i)+px6(i)*c414(i)+pz6(i)*c514(i)
'  150 continue
'      return
'      end

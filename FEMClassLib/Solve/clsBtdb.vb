'This file contains all the methods of btdb.f
Partial Public Class clsSolve

    
    Private row1(64), row2(64), row3(64), row4(64), row5(64), row6(64) As Double

    Private c101(64), c201(64), c301(64), c401(64), c501(64), c601(64),
     c102(64), c202(64), c302(64), c402(64), c502(64), c602(64),
     c103(64), c203(64), c303(64), c403(64), c503(64), c603(64),
     c104(64), c204(64), c304(64), c404(64), c504(64), c604(64),
     c105(64), c205(64), c305(64), c405(64), c505(64), c605(64),
     c106(64), c206(64), c306(64), c406(64), c506(64), c606(64),
     c107(64), c207(64), c307(64), c407(64), c507(64), c607(64),
     c108(64), c208(64), c308(64), c408(64), c508(64), c608(64),
     c109(64), c209(64), c309(64), c409(64), c509(64), c609(64),
     c110(64), c210(64), c310(64), c410(64), c510(64), c610(64),
     c111(64), c211(64), c311(64), c411(64), c511(64), c611(64),
     c112(64), c212(64), c312(64), c412(64), c512(64), c612(64),
     c113(64), c213(64), c313(64), c413(64), c513(64), c613(64),
     c114(64), c214(64), c314(64), c414(64), c514(64), c614(64),
     c115(64), c215(64), c315(64), c415(64), c515(64), c615(64),
     c116(64), c216(64), c316(64), c416(64), c516(64), c616(64),
     c117(64), c217(64), c317(64), c417(64), c517(64), c617(64),
     c118(64), c218(64), c318(64), c418(64), c518(64), c618(64),
     c119(64), c219(64), c319(64), c419(64), c519(64), c619(64),
     c120(64), c220(64), c320(64), c420(64), c520(64), c620(64),
     c121(64), c221(64), c321(64), c421(64), c521(64), c621(64),
     c122(64), c222(64), c322(64), c422(64), c522(64), c622(64),
     c123(64), c223(64), c323(64), c423(64), c523(64), c623(64),
     c124(64), c224(64), c324(64), c424(64), c524(64), c624(64) As Double



    ''' <summary>
    ''' drive b *d*b product for hexahedral element this routine computes the d*b product
    ''' </summary>
    ''' <param name="s"></param>
    ''' <param name="d"></param>
    Public Sub btdb(ByRef s(,) As Double, ByRef d(,,) As Double, ByVal ng As Integer)

        Dim i As Integer ' YC 102418
        If ng = 35 Then
            ng = ng
        End If
        For i = lft To llt

            ' YC 102418 
            'row1(i) = d(0, 0, i) + d(0, 1, i) + d(0, 2, i)  '322653270 vs 322653228  YC? 102418 
            'row2(i) = d(1, 0, i) + d(1, 1, i) + d(1, 2, i)
            'row3(i) = d(2, 0, i) + d(2, 1, i) + d(2, 2, i)
            'row4(i) = d(3, 0, i) + d(3, 1, i) + d(3, 2, i)
            'row5(i) = d(4, 0, i) + d(4, 1, i) + d(4, 2, i)
            'row6(i) = d(5, 0, i) + d(5, 1, i) + d(5, 2, i)
            row1(i) = d(1, 1, i) + d(1, 2, i) + d(1, 3, i)
            row2(i) = d(2, 1, i) + d(2, 2, i) + d(2, 3, i)
            row3(i) = d(3, 1, i) + d(3, 2, i) + d(3, 3, i)
            row4(i) = d(4, 1, i) + d(4, 2, i) + d(4, 3, i)
            row5(i) = d(5, 1, i) + d(5, 2, i) + d(5, 3, i)
            row6(i) = d(6, 1, i) + d(6, 2, i) + d(6, 3, i)
            ' YC 102418 

        Next

        For i = lft To llt

            ' YC 102418 
            'c101(i) = px1(i) * d(0, 0, i) + py1(i) * d(0, 3, i) + pz1(i) * d(0, 5, i) + row1(i) * pxm1(i)
            'c201(i) = px1(i) * d(1, 0, i) + py1(i) * d(1, 3, i) + pz1(i) * d(1, 5, i) + row2(i) * pxm1(i)
            'c301(i) = px1(i) * d(2, 0, i) + py1(i) * d(2, 3, i) + pz1(i) * d(2, 5, i) + row3(i) * pxm1(i)
            'c401(i) = px1(i) * d(3, 0, i) + py1(i) * d(3, 3, i) + pz1(i) * d(3, 5, i) + row4(i) * pxm1(i)
            'c501(i) = px1(i) * d(4, 0, i) + py1(i) * d(4, 3, i) + pz1(i) * d(4, 5, i) + row5(i) * pxm1(i)
            'c601(i) = px1(i) * d(5, 0, i) + py1(i) * d(5, 3, i) + pz1(i) * d(5, 5, i) + row6(i) * pxm1(i)
            'c102(i) = py1(i) * d(0, 1, i) + px1(i) * d(0, 3, i) + pz1(i) * d(0, 4, i) + row1(i) * pym1(i)
            'c202(i) = py1(i) * d(1, 1, i) + px1(i) * d(1, 3, i) + pz1(i) * d(1, 4, i) + row2(i) * pym1(i)
            'c302(i) = py1(i) * d(2, 1, i) + px1(i) * d(2, 3, i) + pz1(i) * d(2, 4, i) + row3(i) * pym1(i)
            'c402(i) = py1(i) * d(3, 1, i) + px1(i) * d(3, 3, i) + pz1(i) * d(3, 4, i) + row4(i) * pym1(i)
            'c502(i) = py1(i) * d(4, 1, i) + px1(i) * d(4, 3, i) + pz1(i) * d(4, 4, i) + row5(i) * pym1(i)
            'c602(i) = py1(i) * d(5, 1, i) + px1(i) * d(5, 3, i) + pz1(i) * d(5, 4, i) + row6(i) * pym1(i)
            c101(i) = px1(i) * d(1, 1, i) + py1(i) * d(1, 4, i) + pz1(i) * d(1, 6, i) + row1(i) * pxm1(i)
            c201(i) = px1(i) * d(2, 1, i) + py1(i) * d(2, 4, i) + pz1(i) * d(2, 6, i) + row2(i) * pxm1(i)
            c301(i) = px1(i) * d(3, 1, i) + py1(i) * d(3, 4, i) + pz1(i) * d(3, 6, i) + row3(i) * pxm1(i)
            c401(i) = px1(i) * d(4, 1, i) + py1(i) * d(4, 4, i) + pz1(i) * d(4, 6, i) + row4(i) * pxm1(i)
            c501(i) = px1(i) * d(5, 1, i) + py1(i) * d(5, 4, i) + pz1(i) * d(5, 6, i) + row5(i) * pxm1(i)
            c601(i) = px1(i) * d(6, 1, i) + py1(i) * d(6, 4, i) + pz1(i) * d(6, 6, i) + row6(i) * pxm1(i)
            c102(i) = py1(i) * d(1, 2, i) + px1(i) * d(1, 4, i) + pz1(i) * d(1, 5, i) + row1(i) * pym1(i)
            c202(i) = py1(i) * d(2, 2, i) + px1(i) * d(2, 4, i) + pz1(i) * d(2, 5, i) + row2(i) * pym1(i)
            c302(i) = py1(i) * d(3, 2, i) + px1(i) * d(3, 4, i) + pz1(i) * d(3, 5, i) + row3(i) * pym1(i)
            c402(i) = py1(i) * d(4, 2, i) + px1(i) * d(4, 4, i) + pz1(i) * d(4, 5, i) + row4(i) * pym1(i)
            c502(i) = py1(i) * d(5, 2, i) + px1(i) * d(5, 4, i) + pz1(i) * d(5, 5, i) + row5(i) * pym1(i)
            c602(i) = py1(i) * d(6, 2, i) + px1(i) * d(6, 4, i) + pz1(i) * d(6, 5, i) + row6(i) * pym1(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 
            'c103(i) = pz1(i) * d(0, 2, i) + py1(i) * d(0, 4, i) + px1(i) * d(0, 5, i) + row1(i) * pzm1(i)
            'c203(i) = pz1(i) * d(1, 2, i) + py1(i) * d(1, 4, i) + px1(i) * d(1, 5, i) + row2(i) * pzm1(i)
            'c303(i) = pz1(i) * d(2, 2, i) + py1(i) * d(2, 4, i) + px1(i) * d(2, 5, i) + row3(i) * pzm1(i)
            'c403(i) = pz1(i) * d(3, 2, i) + py1(i) * d(3, 4, i) + px1(i) * d(3, 5, i) + row4(i) * pzm1(i)
            'c503(i) = pz1(i) * d(4, 2, i) + py1(i) * d(4, 4, i) + px1(i) * d(4, 5, i) + row5(i) * pzm1(i)
            'c603(i) = pz1(i) * d(5, 2, i) + py1(i) * d(5, 4, i) + px1(i) * d(5, 5, i) + row6(i) * pzm1(i)
            'c104(i) = px2(i) * d(0, 0, i) + py2(i) * d(0, 3, i) + pz2(i) * d(0, 5, i) + row1(i) * pxm2(i)
            'c204(i) = px2(i) * d(1, 0, i) + py2(i) * d(1, 3, i) + pz2(i) * d(1, 5, i) + row2(i) * pxm2(i)
            'c304(i) = px2(i) * d(2, 0, i) + py2(i) * d(2, 3, i) + pz2(i) * d(2, 5, i) + row3(i) * pxm2(i)
            'c404(i) = px2(i) * d(3, 0, i) + py2(i) * d(3, 3, i) + pz2(i) * d(3, 5, i) + row4(i) * pxm2(i)
            'c504(i) = px2(i) * d(4, 0, i) + py2(i) * d(4, 3, i) + pz2(i) * d(4, 5, i) + row5(i) * pxm2(i)
            'c604(i) = px2(i) * d(5, 0, i) + py2(i) * d(5, 3, i) + pz2(i) * d(5, 5, i) + row6(i) * pxm2(i)
            c103(i) = pz1(i) * d(1, 3, i) + py1(i) * d(1, 5, i) + px1(i) * d(1, 6, i) + row1(i) * pzm1(i)
            c203(i) = pz1(i) * d(2, 3, i) + py1(i) * d(2, 5, i) + px1(i) * d(2, 6, i) + row2(i) * pzm1(i)
            c303(i) = pz1(i) * d(3, 3, i) + py1(i) * d(3, 5, i) + px1(i) * d(3, 6, i) + row3(i) * pzm1(i)
            c403(i) = pz1(i) * d(4, 3, i) + py1(i) * d(4, 5, i) + px1(i) * d(4, 6, i) + row4(i) * pzm1(i)
            c503(i) = pz1(i) * d(5, 3, i) + py1(i) * d(5, 5, i) + px1(i) * d(5, 6, i) + row5(i) * pzm1(i)
            c603(i) = pz1(i) * d(6, 3, i) + py1(i) * d(6, 5, i) + px1(i) * d(6, 6, i) + row6(i) * pzm1(i)
            c104(i) = px2(i) * d(1, 1, i) + py2(i) * d(1, 4, i) + pz2(i) * d(1, 6, i) + row1(i) * pxm2(i)
            c204(i) = px2(i) * d(2, 1, i) + py2(i) * d(2, 4, i) + pz2(i) * d(2, 6, i) + row2(i) * pxm2(i)
            c304(i) = px2(i) * d(3, 1, i) + py2(i) * d(3, 4, i) + pz2(i) * d(3, 6, i) + row3(i) * pxm2(i)
            c404(i) = px2(i) * d(4, 1, i) + py2(i) * d(4, 4, i) + pz2(i) * d(4, 6, i) + row4(i) * pxm2(i)
            c504(i) = px2(i) * d(5, 1, i) + py2(i) * d(5, 4, i) + pz2(i) * d(5, 6, i) + row5(i) * pxm2(i)
            c604(i) = px2(i) * d(6, 1, i) + py2(i) * d(6, 4, i) + pz2(i) * d(6, 6, i) + row6(i) * pxm2(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            'c105(i) = py2(i) * d(0, 1, i) + px2(i) * d(0, 3, i) + pz2(i) * d(0, 4, i) + row1(i) * pym2(i)
            'c205(i) = py2(i) * d(1, 1, i) + px2(i) * d(1, 3, i) + pz2(i) * d(1, 4, i) + row2(i) * pym2(i)
            'c305(i) = py2(i) * d(2, 1, i) + px2(i) * d(2, 3, i) + pz2(i) * d(2, 4, i) + row3(i) * pym2(i)
            'c405(i) = py2(i) * d(3, 1, i) + px2(i) * d(3, 3, i) + pz2(i) * d(3, 4, i) + row4(i) * pym2(i)
            'c505(i) = py2(i) * d(4, 1, i) + px2(i) * d(4, 3, i) + pz2(i) * d(4, 4, i) + row5(i) * pym2(i)
            'c605(i) = py2(i) * d(5, 1, i) + px2(i) * d(5, 3, i) + pz2(i) * d(5, 4, i) + row6(i) * pym2(i)
            'c106(i) = pz2(i) * d(0, 2, i) + py2(i) * d(0, 4, i) + px2(i) * d(0, 5, i) + row1(i) * pzm2(i)
            'c206(i) = pz2(i) * d(1, 2, i) + py2(i) * d(1, 4, i) + px2(i) * d(1, 5, i) + row2(i) * pzm2(i)
            'c306(i) = pz2(i) * d(2, 2, i) + py2(i) * d(2, 4, i) + px2(i) * d(2, 5, i) + row3(i) * pzm2(i)
            'c406(i) = pz2(i) * d(3, 2, i) + py2(i) * d(3, 4, i) + px2(i) * d(3, 5, i) + row4(i) * pzm2(i)
            'c506(i) = pz2(i) * d(4, 2, i) + py2(i) * d(4, 4, i) + px2(i) * d(4, 5, i) + row5(i) * pzm2(i)
            'c606(i) = pz2(i) * d(5, 2, i) + py2(i) * d(5, 4, i) + px2(i) * d(5, 5, i) + row6(i) * pzm2(i)
            c105(i) = py2(i) * d(1, 2, i) + px2(i) * d(1, 4, i) + pz2(i) * d(1, 5, i) + row1(i) * pym2(i)
            c205(i) = py2(i) * d(2, 2, i) + px2(i) * d(2, 4, i) + pz2(i) * d(2, 5, i) + row2(i) * pym2(i)
            c305(i) = py2(i) * d(3, 2, i) + px2(i) * d(3, 4, i) + pz2(i) * d(3, 5, i) + row3(i) * pym2(i)
            c405(i) = py2(i) * d(4, 2, i) + px2(i) * d(4, 4, i) + pz2(i) * d(4, 5, i) + row4(i) * pym2(i)
            c505(i) = py2(i) * d(5, 2, i) + px2(i) * d(5, 4, i) + pz2(i) * d(5, 5, i) + row5(i) * pym2(i)
            c605(i) = py2(i) * d(6, 2, i) + px2(i) * d(6, 4, i) + pz2(i) * d(6, 5, i) + row6(i) * pym2(i)
            c106(i) = pz2(i) * d(1, 3, i) + py2(i) * d(1, 5, i) + px2(i) * d(1, 6, i) + row1(i) * pzm2(i)
            c206(i) = pz2(i) * d(2, 3, i) + py2(i) * d(2, 5, i) + px2(i) * d(2, 6, i) + row2(i) * pzm2(i)
            c306(i) = pz2(i) * d(3, 3, i) + py2(i) * d(3, 5, i) + px2(i) * d(3, 6, i) + row3(i) * pzm2(i)
            c406(i) = pz2(i) * d(4, 3, i) + py2(i) * d(4, 5, i) + px2(i) * d(4, 6, i) + row4(i) * pzm2(i)
            c506(i) = pz2(i) * d(5, 3, i) + py2(i) * d(5, 5, i) + px2(i) * d(5, 6, i) + row5(i) * pzm2(i)
            c606(i) = pz2(i) * d(6, 3, i) + py2(i) * d(6, 5, i) + px2(i) * d(6, 6, i) + row6(i) * pzm2(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            'c107(i) = px3(i) * d(0, 0, i) + py3(i) * d(0, 3, i) + pz3(i) * d(0, 5, i) + row1(i) * pxm3(i)
            'c207(i) = px3(i) * d(1, 0, i) + py3(i) * d(1, 3, i) + pz3(i) * d(1, 5, i) + row2(i) * pxm3(i)
            'c307(i) = px3(i) * d(2, 0, i) + py3(i) * d(2, 3, i) + pz3(i) * d(2, 5, i) + row3(i) * pxm3(i)
            'c407(i) = px3(i) * d(3, 0, i) + py3(i) * d(3, 3, i) + pz3(i) * d(3, 5, i) + row4(i) * pxm3(i)
            'c507(i) = px3(i) * d(4, 0, i) + py3(i) * d(4, 3, i) + pz3(i) * d(4, 5, i) + row5(i) * pxm3(i)
            'c607(i) = px3(i) * d(5, 0, i) + py3(i) * d(5, 3, i) + pz3(i) * d(5, 5, i) + row6(i) * pxm3(i)
            'c108(i) = py3(i) * d(0, 1, i) + px3(i) * d(0, 3, i) + pz3(i) * d(0, 4, i) + row1(i) * pym3(i)
            'c208(i) = py3(i) * d(1, 1, i) + px3(i) * d(1, 3, i) + pz3(i) * d(1, 4, i) + row2(i) * pym3(i)
            'c308(i) = py3(i) * d(2, 1, i) + px3(i) * d(2, 3, i) + pz3(i) * d(2, 4, i) + row3(i) * pym3(i)
            'c408(i) = py3(i) * d(3, 1, i) + px3(i) * d(3, 3, i) + pz3(i) * d(3, 4, i) + row4(i) * pym3(i)
            'c508(i) = py3(i) * d(4, 1, i) + px3(i) * d(4, 3, i) + pz3(i) * d(4, 4, i) + row5(i) * pym3(i)
            'c608(i) = py3(i) * d(5, 1, i) + px3(i) * d(5, 3, i) + pz3(i) * d(5, 4, i) + row6(i) * pym3(i)
            c107(i) = px3(i) * d(1, 1, i) + py3(i) * d(1, 4, i) + pz3(i) * d(1, 6, i) + row1(i) * pxm3(i)
            c207(i) = px3(i) * d(2, 1, i) + py3(i) * d(2, 4, i) + pz3(i) * d(2, 6, i) + row2(i) * pxm3(i)
            c307(i) = px3(i) * d(3, 1, i) + py3(i) * d(3, 4, i) + pz3(i) * d(3, 6, i) + row3(i) * pxm3(i)
            c407(i) = px3(i) * d(4, 1, i) + py3(i) * d(4, 4, i) + pz3(i) * d(4, 6, i) + row4(i) * pxm3(i)
            c507(i) = px3(i) * d(5, 1, i) + py3(i) * d(5, 4, i) + pz3(i) * d(5, 6, i) + row5(i) * pxm3(i)
            c607(i) = px3(i) * d(6, 1, i) + py3(i) * d(6, 4, i) + pz3(i) * d(6, 6, i) + row6(i) * pxm3(i)
            c108(i) = py3(i) * d(1, 2, i) + px3(i) * d(1, 4, i) + pz3(i) * d(1, 5, i) + row1(i) * pym3(i)
            c208(i) = py3(i) * d(2, 2, i) + px3(i) * d(2, 4, i) + pz3(i) * d(2, 5, i) + row2(i) * pym3(i)
            c308(i) = py3(i) * d(3, 2, i) + px3(i) * d(3, 4, i) + pz3(i) * d(3, 5, i) + row3(i) * pym3(i)
            c408(i) = py3(i) * d(4, 2, i) + px3(i) * d(4, 4, i) + pz3(i) * d(4, 5, i) + row4(i) * pym3(i)
            c508(i) = py3(i) * d(5, 2, i) + px3(i) * d(5, 4, i) + pz3(i) * d(5, 5, i) + row5(i) * pym3(i)
            c608(i) = py3(i) * d(6, 2, i) + px3(i) * d(6, 4, i) + pz3(i) * d(6, 5, i) + row6(i) * pym3(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 END
            'c109(i) = pz3(i) * d(0, 2, i) + py3(i) * d(0, 4, i) + px3(i) * d(0, 5, i) + row1(i) * pzm3(i)
            'c209(i) = pz3(i) * d(1, 2, i) + py3(i) * d(1, 4, i) + px3(i) * d(1, 5, i) + row2(i) * pzm3(i)
            'c309(i) = pz3(i) * d(2, 2, i) + py3(i) * d(2, 4, i) + px3(i) * d(2, 5, i) + row3(i) * pzm3(i)
            'c409(i) = pz3(i) * d(3, 2, i) + py3(i) * d(3, 4, i) + px3(i) * d(3, 5, i) + row4(i) * pzm3(i)
            'c509(i) = pz3(i) * d(4, 2, i) + py3(i) * d(4, 4, i) + px3(i) * d(4, 5, i) + row5(i) * pzm3(i)
            'c609(i) = pz3(i) * d(5, 2, i) + py3(i) * d(5, 4, i) + px3(i) * d(5, 5, i) + row6(i) * pzm3(i)
            'c110(i) = px4(i) * d(0, 0, i) + py4(i) * d(0, 3, i) + pz4(i) * d(0, 5, i) + row1(i) * pxm4(i)
            'c210(i) = px4(i) * d(1, 0, i) + py4(i) * d(1, 3, i) + pz4(i) * d(1, 5, i) + row2(i) * pxm4(i)
            'c310(i) = px4(i) * d(2, 0, i) + py4(i) * d(2, 3, i) + pz4(i) * d(2, 5, i) + row3(i) * pxm4(i)
            'c410(i) = px4(i) * d(3, 0, i) + py4(i) * d(3, 3, i) + pz4(i) * d(3, 5, i) + row4(i) * pxm4(i)
            'c510(i) = px4(i) * d(4, 0, i) + py4(i) * d(4, 3, i) + pz4(i) * d(4, 5, i) + row5(i) * pxm4(i)
            'c610(i) = px4(i) * d(5, 0, i) + py4(i) * d(5, 3, i) + pz4(i) * d(5, 5, i) + row6(i) * pxm4(i)
            c109(i) = pz3(i) * d(1, 3, i) + py3(i) * d(1, 5, i) + px3(i) * d(1, 6, i) + row1(i) * pzm3(i)
            c209(i) = pz3(i) * d(2, 3, i) + py3(i) * d(2, 5, i) + px3(i) * d(2, 6, i) + row2(i) * pzm3(i)
            c309(i) = pz3(i) * d(3, 3, i) + py3(i) * d(3, 5, i) + px3(i) * d(3, 6, i) + row3(i) * pzm3(i)
            c409(i) = pz3(i) * d(4, 3, i) + py3(i) * d(4, 5, i) + px3(i) * d(4, 6, i) + row4(i) * pzm3(i)
            c509(i) = pz3(i) * d(5, 3, i) + py3(i) * d(5, 5, i) + px3(i) * d(5, 6, i) + row5(i) * pzm3(i)
            c609(i) = pz3(i) * d(6, 3, i) + py3(i) * d(6, 5, i) + px3(i) * d(6, 6, i) + row6(i) * pzm3(i)
            c110(i) = px4(i) * d(1, 1, i) + py4(i) * d(1, 4, i) + pz4(i) * d(1, 6, i) + row1(i) * pxm4(i)
            c210(i) = px4(i) * d(2, 1, i) + py4(i) * d(2, 4, i) + pz4(i) * d(2, 6, i) + row2(i) * pxm4(i)
            c310(i) = px4(i) * d(3, 1, i) + py4(i) * d(3, 4, i) + pz4(i) * d(3, 6, i) + row3(i) * pxm4(i)
            c410(i) = px4(i) * d(4, 1, i) + py4(i) * d(4, 4, i) + pz4(i) * d(4, 6, i) + row4(i) * pxm4(i)
            c510(i) = px4(i) * d(5, 1, i) + py4(i) * d(5, 4, i) + pz4(i) * d(5, 6, i) + row5(i) * pxm4(i)
            c610(i) = px4(i) * d(6, 1, i) + py4(i) * d(6, 4, i) + pz4(i) * d(6, 6, i) + row6(i) * pxm4(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            'c111(i) = py4(i) * d(0, 1, i) + px4(i) * d(0, 3, i) + pz4(i) * d(0, 4, i) + row1(i) * pym4(i)
            'c211(i) = py4(i) * d(1, 1, i) + px4(i) * d(1, 3, i) + pz4(i) * d(1, 4, i) + row2(i) * pym4(i)
            'c311(i) = py4(i) * d(2, 1, i) + px4(i) * d(2, 3, i) + pz4(i) * d(2, 4, i) + row3(i) * pym4(i)
            'c411(i) = py4(i) * d(3, 1, i) + px4(i) * d(3, 3, i) + pz4(i) * d(3, 4, i) + row4(i) * pym4(i)
            'c511(i) = py4(i) * d(4, 1, i) + px4(i) * d(4, 3, i) + pz4(i) * d(4, 4, i) + row5(i) * pym4(i)
            'c611(i) = py4(i) * d(5, 1, i) + px4(i) * d(5, 3, i) + pz4(i) * d(5, 4, i) + row6(i) * pym4(i)
            'c112(i) = pz4(i) * d(0, 2, i) + py4(i) * d(0, 4, i) + px4(i) * d(0, 5, i) + row1(i) * pzm4(i)
            'c212(i) = pz4(i) * d(1, 2, i) + py4(i) * d(1, 4, i) + px4(i) * d(1, 5, i) + row2(i) * pzm4(i)
            'c312(i) = pz4(i) * d(2, 2, i) + py4(i) * d(2, 4, i) + px4(i) * d(2, 5, i) + row3(i) * pzm4(i)
            'c412(i) = pz4(i) * d(3, 2, i) + py4(i) * d(3, 4, i) + px4(i) * d(3, 5, i) + row4(i) * pzm4(i)
            'c512(i) = pz4(i) * d(4, 2, i) + py4(i) * d(4, 4, i) + px4(i) * d(4, 5, i) + row5(i) * pzm4(i)
            'c612(i) = pz4(i) * d(5, 2, i) + py4(i) * d(5, 4, i) + px4(i) * d(5, 5, i) + row6(i) * pzm4(i)
            c111(i) = py4(i) * d(1, 2, i) + px4(i) * d(1, 4, i) + pz4(i) * d(1, 5, i) + row1(i) * pym4(i)
            c211(i) = py4(i) * d(2, 2, i) + px4(i) * d(2, 4, i) + pz4(i) * d(2, 5, i) + row2(i) * pym4(i)
            c311(i) = py4(i) * d(3, 2, i) + px4(i) * d(3, 4, i) + pz4(i) * d(3, 5, i) + row3(i) * pym4(i)
            c411(i) = py4(i) * d(4, 2, i) + px4(i) * d(4, 4, i) + pz4(i) * d(4, 5, i) + row4(i) * pym4(i)
            c511(i) = py4(i) * d(5, 2, i) + px4(i) * d(5, 4, i) + pz4(i) * d(5, 5, i) + row5(i) * pym4(i)
            c611(i) = py4(i) * d(6, 2, i) + px4(i) * d(6, 4, i) + pz4(i) * d(6, 5, i) + row6(i) * pym4(i)
            c112(i) = pz4(i) * d(1, 3, i) + py4(i) * d(1, 5, i) + px4(i) * d(1, 6, i) + row1(i) * pzm4(i)
            c212(i) = pz4(i) * d(2, 3, i) + py4(i) * d(2, 5, i) + px4(i) * d(2, 6, i) + row2(i) * pzm4(i)
            c312(i) = pz4(i) * d(3, 3, i) + py4(i) * d(3, 5, i) + px4(i) * d(3, 6, i) + row3(i) * pzm4(i)
            c412(i) = pz4(i) * d(4, 3, i) + py4(i) * d(4, 5, i) + px4(i) * d(4, 6, i) + row4(i) * pzm4(i)
            c512(i) = pz4(i) * d(5, 3, i) + py4(i) * d(5, 5, i) + px4(i) * d(5, 6, i) + row5(i) * pzm4(i)
            c612(i) = pz4(i) * d(6, 3, i) + py4(i) * d(6, 5, i) + px4(i) * d(6, 6, i) + row6(i) * pzm4(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            'c113(i) = px5(i) * d(0, 0, i) + py5(i) * d(0, 3, i) + pz5(i) * d(0, 5, i) + row1(i) * pxm5(i)
            'c213(i) = px5(i) * d(1, 0, i) + py5(i) * d(1, 3, i) + pz5(i) * d(1, 5, i) + row2(i) * pxm5(i)
            'c313(i) = px5(i) * d(2, 0, i) + py5(i) * d(2, 3, i) + pz5(i) * d(2, 5, i) + row3(i) * pxm5(i)
            'c413(i) = px5(i) * d(3, 0, i) + py5(i) * d(3, 3, i) + pz5(i) * d(3, 5, i) + row4(i) * pxm5(i)
            'c513(i) = px5(i) * d(4, 0, i) + py5(i) * d(4, 3, i) + pz5(i) * d(4, 5, i) + row5(i) * pxm5(i)
            'c613(i) = px5(i) * d(5, 0, i) + py5(i) * d(5, 3, i) + pz5(i) * d(5, 5, i) + row6(i) * pxm5(i)
            'c114(i) = py5(i) * d(0, 1, i) + px5(i) * d(0, 3, i) + pz5(i) * d(0, 4, i) + row1(i) * pym5(i)
            'c214(i) = py5(i) * d(1, 1, i) + px5(i) * d(1, 3, i) + pz5(i) * d(1, 4, i) + row2(i) * pym5(i)
            'c314(i) = py5(i) * d(2, 1, i) + px5(i) * d(2, 3, i) + pz5(i) * d(2, 4, i) + row3(i) * pym5(i)
            'c414(i) = py5(i) * d(3, 1, i) + px5(i) * d(3, 3, i) + pz5(i) * d(3, 4, i) + row4(i) * pym5(i)
            'c514(i) = py5(i) * d(4, 1, i) + px5(i) * d(4, 3, i) + pz5(i) * d(4, 4, i) + row5(i) * pym5(i)
            'c614(i) = py5(i) * d(5, 1, i) + px5(i) * d(5, 3, i) + pz5(i) * d(5, 4, i) + row6(i) * pym5(i)
            c113(i) = px5(i) * d(1, 1, i) + py5(i) * d(1, 4, i) + pz5(i) * d(1, 6, i) + row1(i) * pxm5(i)
            c213(i) = px5(i) * d(2, 1, i) + py5(i) * d(2, 4, i) + pz5(i) * d(2, 6, i) + row2(i) * pxm5(i)
            c313(i) = px5(i) * d(3, 1, i) + py5(i) * d(3, 4, i) + pz5(i) * d(3, 6, i) + row3(i) * pxm5(i)
            c413(i) = px5(i) * d(4, 1, i) + py5(i) * d(4, 4, i) + pz5(i) * d(4, 6, i) + row4(i) * pxm5(i)
            c513(i) = px5(i) * d(5, 1, i) + py5(i) * d(5, 4, i) + pz5(i) * d(5, 6, i) + row5(i) * pxm5(i)
            c613(i) = px5(i) * d(6, 1, i) + py5(i) * d(6, 4, i) + pz5(i) * d(6, 6, i) + row6(i) * pxm5(i)
            c114(i) = py5(i) * d(1, 2, i) + px5(i) * d(1, 4, i) + pz5(i) * d(1, 5, i) + row1(i) * pym5(i)
            c214(i) = py5(i) * d(2, 2, i) + px5(i) * d(2, 4, i) + pz5(i) * d(2, 5, i) + row2(i) * pym5(i)
            c314(i) = py5(i) * d(3, 2, i) + px5(i) * d(3, 4, i) + pz5(i) * d(3, 5, i) + row3(i) * pym5(i)
            c414(i) = py5(i) * d(4, 2, i) + px5(i) * d(4, 4, i) + pz5(i) * d(4, 5, i) + row4(i) * pym5(i)
            c514(i) = py5(i) * d(5, 2, i) + px5(i) * d(5, 4, i) + pz5(i) * d(5, 5, i) + row5(i) * pym5(i)
            c614(i) = py5(i) * d(6, 2, i) + px5(i) * d(6, 4, i) + pz5(i) * d(6, 5, i) + row6(i) * pym5(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            'c115(i) = pz5(i) * d(0, 2, i) + py5(i) * d(0, 4, i) + px5(i) * d(0, 5, i) + row1(i) * pzm5(i)
            'c215(i) = pz5(i) * d(1, 2, i) + py5(i) * d(1, 4, i) + px5(i) * d(1, 5, i) + row2(i) * pzm5(i)
            'c315(i) = pz5(i) * d(2, 2, i) + py5(i) * d(2, 4, i) + px5(i) * d(2, 5, i) + row3(i) * pzm5(i)
            'c415(i) = pz5(i) * d(3, 2, i) + py5(i) * d(3, 4, i) + px5(i) * d(3, 5, i) + row4(i) * pzm5(i)
            'c515(i) = pz5(i) * d(4, 2, i) + py5(i) * d(4, 4, i) + px5(i) * d(4, 5, i) + row5(i) * pzm5(i)
            'c615(i) = pz5(i) * d(5, 2, i) + py5(i) * d(5, 4, i) + px5(i) * d(5, 5, i) + row6(i) * pzm5(i)
            'c116(i) = px6(i) * d(0, 0, i) + py6(i) * d(0, 3, i) + pz6(i) * d(0, 5, i) + row1(i) * pxm6(i)
            'c216(i) = px6(i) * d(1, 0, i) + py6(i) * d(1, 3, i) + pz6(i) * d(1, 5, i) + row2(i) * pxm6(i)
            'c316(i) = px6(i) * d(2, 0, i) + py6(i) * d(2, 3, i) + pz6(i) * d(2, 5, i) + row3(i) * pxm6(i)
            'c416(i) = px6(i) * d(3, 0, i) + py6(i) * d(3, 3, i) + pz6(i) * d(3, 5, i) + row4(i) * pxm6(i)
            'c516(i) = px6(i) * d(4, 0, i) + py6(i) * d(4, 3, i) + pz6(i) * d(4, 5, i) + row5(i) * pxm6(i)
            'c616(i) = px6(i) * d(5, 0, i) + py6(i) * d(5, 3, i) + pz6(i) * d(5, 5, i) + row6(i) * pxm6(i)
            c115(i) = pz5(i) * d(1, 3, i) + py5(i) * d(1, 5, i) + px5(i) * d(1, 6, i) + row1(i) * pzm5(i)
            c215(i) = pz5(i) * d(2, 3, i) + py5(i) * d(2, 5, i) + px5(i) * d(2, 6, i) + row2(i) * pzm5(i)
            c315(i) = pz5(i) * d(3, 3, i) + py5(i) * d(3, 5, i) + px5(i) * d(3, 6, i) + row3(i) * pzm5(i)
            c415(i) = pz5(i) * d(4, 3, i) + py5(i) * d(4, 5, i) + px5(i) * d(4, 6, i) + row4(i) * pzm5(i)
            c515(i) = pz5(i) * d(5, 3, i) + py5(i) * d(5, 5, i) + px5(i) * d(5, 6, i) + row5(i) * pzm5(i)
            c615(i) = pz5(i) * d(6, 3, i) + py5(i) * d(6, 5, i) + px5(i) * d(6, 6, i) + row6(i) * pzm5(i)
            c116(i) = px6(i) * d(1, 1, i) + py6(i) * d(1, 4, i) + pz6(i) * d(1, 6, i) + row1(i) * pxm6(i)
            c216(i) = px6(i) * d(2, 1, i) + py6(i) * d(2, 4, i) + pz6(i) * d(2, 6, i) + row2(i) * pxm6(i)
            c316(i) = px6(i) * d(3, 1, i) + py6(i) * d(3, 4, i) + pz6(i) * d(3, 6, i) + row3(i) * pxm6(i)
            c416(i) = px6(i) * d(4, 1, i) + py6(i) * d(4, 4, i) + pz6(i) * d(4, 6, i) + row4(i) * pxm6(i)
            c516(i) = px6(i) * d(5, 1, i) + py6(i) * d(5, 4, i) + pz6(i) * d(5, 6, i) + row5(i) * pxm6(i)
            c616(i) = px6(i) * d(6, 1, i) + py6(i) * d(6, 4, i) + pz6(i) * d(6, 6, i) + row6(i) * pxm6(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 
            'c117(i) = py6(i) * d(0, 1, i) + px6(i) * d(0, 3, i) + pz6(i) * d(0, 4, i) + row1(i) * pym6(i)
            'c217(i) = py6(i) * d(1, 1, i) + px6(i) * d(1, 3, i) + pz6(i) * d(1, 4, i) + row2(i) * pym6(i)
            'c317(i) = py6(i) * d(2, 1, i) + px6(i) * d(2, 3, i) + pz6(i) * d(2, 4, i) + row3(i) * pym6(i)
            'c417(i) = py6(i) * d(3, 1, i) + px6(i) * d(3, 3, i) + pz6(i) * d(3, 4, i) + row4(i) * pym6(i)
            'c517(i) = py6(i) * d(4, 1, i) + px6(i) * d(4, 3, i) + pz6(i) * d(4, 4, i) + row5(i) * pym6(i)
            'c617(i) = py6(i) * d(5, 1, i) + px6(i) * d(5, 3, i) + pz6(i) * d(5, 4, i) + row6(i) * pym6(i)
            'c118(i) = pz6(i) * d(0, 2, i) + py6(i) * d(0, 4, i) + px6(i) * d(0, 5, i) + row1(i) * pzm6(i)
            'c218(i) = pz6(i) * d(1, 2, i) + py6(i) * d(1, 4, i) + px6(i) * d(1, 5, i) + row2(i) * pzm6(i)
            'c318(i) = pz6(i) * d(2, 2, i) + py6(i) * d(2, 4, i) + px6(i) * d(2, 5, i) + row3(i) * pzm6(i)
            'c418(i) = pz6(i) * d(3, 2, i) + py6(i) * d(3, 4, i) + px6(i) * d(3, 5, i) + row4(i) * pzm6(i)
            'c518(i) = pz6(i) * d(4, 2, i) + py6(i) * d(4, 4, i) + px6(i) * d(4, 5, i) + row5(i) * pzm6(i)
            'c618(i) = pz6(i) * d(5, 2, i) + py6(i) * d(5, 4, i) + px6(i) * d(5, 5, i) + row6(i) * pzm6(i)
            c117(i) = py6(i) * d(1, 2, i) + px6(i) * d(1, 4, i) + pz6(i) * d(1, 5, i) + row1(i) * pym6(i)
            c217(i) = py6(i) * d(2, 2, i) + px6(i) * d(2, 4, i) + pz6(i) * d(2, 5, i) + row2(i) * pym6(i)
            c317(i) = py6(i) * d(3, 2, i) + px6(i) * d(3, 4, i) + pz6(i) * d(3, 5, i) + row3(i) * pym6(i)
            c417(i) = py6(i) * d(4, 2, i) + px6(i) * d(4, 4, i) + pz6(i) * d(4, 5, i) + row4(i) * pym6(i)
            c517(i) = py6(i) * d(5, 2, i) + px6(i) * d(5, 4, i) + pz6(i) * d(5, 5, i) + row5(i) * pym6(i)
            c617(i) = py6(i) * d(6, 2, i) + px6(i) * d(6, 4, i) + pz6(i) * d(6, 5, i) + row6(i) * pym6(i)
            c118(i) = pz6(i) * d(1, 3, i) + py6(i) * d(1, 5, i) + px6(i) * d(1, 6, i) + row1(i) * pzm6(i)
            c218(i) = pz6(i) * d(2, 3, i) + py6(i) * d(2, 5, i) + px6(i) * d(2, 6, i) + row2(i) * pzm6(i)
            c318(i) = pz6(i) * d(3, 3, i) + py6(i) * d(3, 5, i) + px6(i) * d(3, 6, i) + row3(i) * pzm6(i)
            c418(i) = pz6(i) * d(4, 3, i) + py6(i) * d(4, 5, i) + px6(i) * d(4, 6, i) + row4(i) * pzm6(i)
            c518(i) = pz6(i) * d(5, 3, i) + py6(i) * d(5, 5, i) + px6(i) * d(5, 6, i) + row5(i) * pzm6(i)
            c618(i) = pz6(i) * d(6, 3, i) + py6(i) * d(6, 5, i) + px6(i) * d(6, 6, i) + row6(i) * pzm6(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418 END
            'c119(i) = px7(i) * d(0, 0, i) + py7(i) * d(0, 3, i) + pz7(i) * d(0, 5, i) + row1(i) * pxm7(i)
            'c219(i) = px7(i) * d(1, 0, i) + py7(i) * d(1, 3, i) + pz7(i) * d(1, 5, i) + row2(i) * pxm7(i)
            'c319(i) = px7(i) * d(2, 0, i) + py7(i) * d(2, 3, i) + pz7(i) * d(2, 5, i) + row3(i) * pxm7(i)
            'c419(i) = px7(i) * d(3, 0, i) + py7(i) * d(3, 3, i) + pz7(i) * d(3, 5, i) + row4(i) * pxm7(i)
            'c519(i) = px7(i) * d(4, 0, i) + py7(i) * d(4, 3, i) + pz7(i) * d(4, 5, i) + row5(i) * pxm7(i)
            'c619(i) = px7(i) * d(5, 0, i) + py7(i) * d(5, 3, i) + pz7(i) * d(5, 5, i) + row6(i) * pxm7(i)
            'c120(i) = py7(i) * d(0, 1, i) + px7(i) * d(0, 3, i) + pz7(i) * d(0, 4, i) + row1(i) * pym7(i)
            'c220(i) = py7(i) * d(1, 1, i) + px7(i) * d(1, 3, i) + pz7(i) * d(1, 4, i) + row2(i) * pym7(i)
            'c320(i) = py7(i) * d(2, 1, i) + px7(i) * d(2, 3, i) + pz7(i) * d(2, 4, i) + row3(i) * pym7(i)
            'c420(i) = py7(i) * d(3, 1, i) + px7(i) * d(3, 3, i) + pz7(i) * d(3, 4, i) + row4(i) * pym7(i)
            'c520(i) = py7(i) * d(4, 1, i) + px7(i) * d(4, 3, i) + pz7(i) * d(4, 4, i) + row5(i) * pym7(i)
            'c620(i) = py7(i) * d(5, 1, i) + px7(i) * d(5, 3, i) + pz7(i) * d(5, 4, i) + row6(i) * pym7(i)
            c119(i) = px7(i) * d(1, 1, i) + py7(i) * d(1, 4, i) + pz7(i) * d(1, 6, i) + row1(i) * pxm7(i)
            c219(i) = px7(i) * d(2, 1, i) + py7(i) * d(2, 4, i) + pz7(i) * d(2, 6, i) + row2(i) * pxm7(i)
            c319(i) = px7(i) * d(3, 1, i) + py7(i) * d(3, 4, i) + pz7(i) * d(3, 6, i) + row3(i) * pxm7(i)
            c419(i) = px7(i) * d(4, 1, i) + py7(i) * d(4, 4, i) + pz7(i) * d(4, 6, i) + row4(i) * pxm7(i)
            c519(i) = px7(i) * d(5, 1, i) + py7(i) * d(5, 4, i) + pz7(i) * d(5, 6, i) + row5(i) * pxm7(i)
            c619(i) = px7(i) * d(6, 1, i) + py7(i) * d(6, 4, i) + pz7(i) * d(6, 6, i) + row6(i) * pxm7(i)
            c120(i) = py7(i) * d(1, 2, i) + px7(i) * d(1, 4, i) + pz7(i) * d(1, 5, i) + row1(i) * pym7(i)
            c220(i) = py7(i) * d(2, 2, i) + px7(i) * d(2, 4, i) + pz7(i) * d(2, 5, i) + row2(i) * pym7(i)
            c320(i) = py7(i) * d(3, 2, i) + px7(i) * d(3, 4, i) + pz7(i) * d(3, 5, i) + row3(i) * pym7(i)
            c420(i) = py7(i) * d(4, 2, i) + px7(i) * d(4, 4, i) + pz7(i) * d(4, 5, i) + row4(i) * pym7(i)
            c520(i) = py7(i) * d(5, 2, i) + px7(i) * d(5, 4, i) + pz7(i) * d(5, 5, i) + row5(i) * pym7(i)
            c620(i) = py7(i) * d(6, 2, i) + px7(i) * d(6, 4, i) + pz7(i) * d(6, 5, i) + row6(i) * pym7(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            'c121(i) = pz7(i) * d(0, 2, i) + py7(i) * d(0, 4, i) + px7(i) * d(0, 5, i) + row1(i) * pzm7(i)
            'c221(i) = pz7(i) * d(1, 2, i) + py7(i) * d(1, 4, i) + px7(i) * d(1, 5, i) + row2(i) * pzm7(i)
            'c321(i) = pz7(i) * d(2, 2, i) + py7(i) * d(2, 4, i) + px7(i) * d(2, 5, i) + row3(i) * pzm7(i)
            'c421(i) = pz7(i) * d(3, 2, i) + py7(i) * d(3, 4, i) + px7(i) * d(3, 5, i) + row4(i) * pzm7(i)
            'c521(i) = pz7(i) * d(4, 2, i) + py7(i) * d(4, 4, i) + px7(i) * d(4, 5, i) + row5(i) * pzm7(i)
            'c621(i) = pz7(i) * d(5, 2, i) + py7(i) * d(5, 4, i) + px7(i) * d(5, 5, i) + row6(i) * pzm7(i)
            'c122(i) = px8(i) * d(0, 0, i) + py8(i) * d(0, 3, i) + pz8(i) * d(0, 5, i) + row1(i) * pxm8(i)
            'c222(i) = px8(i) * d(1, 0, i) + py8(i) * d(1, 3, i) + pz8(i) * d(1, 5, i) + row2(i) * pxm8(i)
            'c322(i) = px8(i) * d(2, 0, i) + py8(i) * d(2, 3, i) + pz8(i) * d(2, 5, i) + row3(i) * pxm8(i)
            'c422(i) = px8(i) * d(3, 0, i) + py8(i) * d(3, 3, i) + pz8(i) * d(3, 5, i) + row4(i) * pxm8(i)
            'c522(i) = px8(i) * d(4, 0, i) + py8(i) * d(4, 3, i) + pz8(i) * d(4, 5, i) + row5(i) * pxm8(i)
            'c622(i) = px8(i) * d(5, 0, i) + py8(i) * d(5, 3, i) + pz8(i) * d(5, 5, i) + row6(i) * pxm8(i)
            c121(i) = pz7(i) * d(1, 3, i) + py7(i) * d(1, 5, i) + px7(i) * d(1, 6, i) + row1(i) * pzm7(i)
            c221(i) = pz7(i) * d(2, 3, i) + py7(i) * d(2, 5, i) + px7(i) * d(2, 6, i) + row2(i) * pzm7(i)
            c321(i) = pz7(i) * d(3, 3, i) + py7(i) * d(3, 5, i) + px7(i) * d(3, 6, i) + row3(i) * pzm7(i)
            c421(i) = pz7(i) * d(4, 3, i) + py7(i) * d(4, 5, i) + px7(i) * d(4, 6, i) + row4(i) * pzm7(i)
            c521(i) = pz7(i) * d(5, 3, i) + py7(i) * d(5, 5, i) + px7(i) * d(5, 6, i) + row5(i) * pzm7(i)
            c621(i) = pz7(i) * d(6, 3, i) + py7(i) * d(6, 5, i) + px7(i) * d(6, 6, i) + row6(i) * pzm7(i)
            c122(i) = px8(i) * d(1, 1, i) + py8(i) * d(1, 4, i) + pz8(i) * d(1, 6, i) + row1(i) * pxm8(i)
            c222(i) = px8(i) * d(2, 1, i) + py8(i) * d(2, 4, i) + pz8(i) * d(2, 6, i) + row2(i) * pxm8(i)
            c322(i) = px8(i) * d(3, 1, i) + py8(i) * d(3, 4, i) + pz8(i) * d(3, 6, i) + row3(i) * pxm8(i)
            c422(i) = px8(i) * d(4, 1, i) + py8(i) * d(4, 4, i) + pz8(i) * d(4, 6, i) + row4(i) * pxm8(i)
            c522(i) = px8(i) * d(5, 1, i) + py8(i) * d(5, 4, i) + pz8(i) * d(5, 6, i) + row5(i) * pxm8(i)
            c622(i) = px8(i) * d(6, 1, i) + py8(i) * d(6, 4, i) + pz8(i) * d(6, 6, i) + row6(i) * pxm8(i)
            ' YC 102418 END

        Next

        For i = lft To llt

            ' YC 102418
            'c123(i) = py8(i) * d(0, 1, i) + px8(i) * d(0, 3, i) + pz8(i) * d(0, 4, i) + row1(i) * pym8(i)
            'c223(i) = py8(i) * d(1, 1, i) + px8(i) * d(1, 3, i) + pz8(i) * d(1, 4, i) + row2(i) * pym8(i)
            'c323(i) = py8(i) * d(2, 1, i) + px8(i) * d(2, 3, i) + pz8(i) * d(2, 4, i) + row3(i) * pym8(i)
            'c423(i) = py8(i) * d(3, 1, i) + px8(i) * d(3, 3, i) + pz8(i) * d(3, 4, i) + row4(i) * pym8(i)
            'c523(i) = py8(i) * d(4, 1, i) + px8(i) * d(4, 3, i) + pz8(i) * d(4, 4, i) + row5(i) * pym8(i)
            'c623(i) = py8(i) * d(5, 1, i) + px8(i) * d(5, 3, i) + pz8(i) * d(5, 4, i) + row6(i) * pym8(i)
            'c124(i) = pz8(i) * d(0, 2, i) + py8(i) * d(0, 4, i) + px8(i) * d(0, 5, i) + row1(i) * pzm8(i)
            'c224(i) = pz8(i) * d(1, 2, i) + py8(i) * d(1, 4, i) + px8(i) * d(1, 5, i) + row2(i) * pzm8(i)
            'c324(i) = pz8(i) * d(2, 2, i) + py8(i) * d(2, 4, i) + px8(i) * d(2, 5, i) + row3(i) * pzm8(i)
            'c424(i) = pz8(i) * d(3, 2, i) + py8(i) * d(3, 4, i) + px8(i) * d(3, 5, i) + row4(i) * pzm8(i)
            'c524(i) = pz8(i) * d(4, 2, i) + py8(i) * d(4, 4, i) + px8(i) * d(4, 5, i) + row5(i) * pzm8(i)
            'c624(i) = pz8(i) * d(5, 2, i) + py8(i) * d(5, 4, i) + px8(i) * d(5, 5, i) + row6(i) * pzm8(i)
            c123(i) = py8(i) * d(1, 2, i) + px8(i) * d(1, 4, i) + pz8(i) * d(1, 5, i) + row1(i) * pym8(i)
            c223(i) = py8(i) * d(2, 2, i) + px8(i) * d(2, 4, i) + pz8(i) * d(2, 5, i) + row2(i) * pym8(i)
            c323(i) = py8(i) * d(3, 2, i) + px8(i) * d(3, 4, i) + pz8(i) * d(3, 5, i) + row3(i) * pym8(i)
            c423(i) = py8(i) * d(4, 2, i) + px8(i) * d(4, 4, i) + pz8(i) * d(4, 5, i) + row4(i) * pym8(i)
            c523(i) = py8(i) * d(5, 2, i) + px8(i) * d(5, 4, i) + pz8(i) * d(5, 5, i) + row5(i) * pym8(i)
            c623(i) = py8(i) * d(6, 2, i) + px8(i) * d(6, 4, i) + pz8(i) * d(6, 5, i) + row6(i) * pym8(i)
            c124(i) = pz8(i) * d(1, 3, i) + py8(i) * d(1, 5, i) + px8(i) * d(1, 6, i) + row1(i) * pzm8(i)
            c224(i) = pz8(i) * d(2, 3, i) + py8(i) * d(2, 5, i) + px8(i) * d(2, 6, i) + row2(i) * pzm8(i)
            c324(i) = pz8(i) * d(3, 3, i) + py8(i) * d(3, 5, i) + px8(i) * d(3, 6, i) + row3(i) * pzm8(i)
            c424(i) = pz8(i) * d(4, 3, i) + py8(i) * d(4, 5, i) + px8(i) * d(4, 6, i) + row4(i) * pzm8(i)
            c524(i) = pz8(i) * d(5, 3, i) + py8(i) * d(5, 5, i) + px8(i) * d(5, 6, i) + row5(i) * pzm8(i)
            c624(i) = pz8(i) * d(6, 3, i) + py8(i) * d(6, 5, i) + px8(i) * d(6, 6, i) + row6(i) * pzm8(i)
            ' YC 102418 END

        Next


        Call fstif0(s)

        Call fstif1(s)
        Call fstif2(s)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine btdb(s,d)
'c
'
'      implicit double precision (a-h,o-z)                             
'
'c
'c                     t
'c===> module to drive b *d*b product for hexahedral element
'c     this routine computes the d*b product
'c
'      common/range/mft,mlt,lft,llt,nftm1
'      
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
'      common/vect19/
'     1 svvl(64,8),volume(64),px(64,8,8),py(64,8,8),pz(64,8,8),
'     2 pxa1(64),pxa2(64),pxa3(64),pxa4(64),pxa5(64),pxa6(64),
'     3 pxa7(64),pxa8(64),pya1(64),pya2(64),pya3(64),pya4(64),
'     4 pya5(64),pya6(64),pya7(64),pya8(64),pza1(64),pza2(64),
'     5 pza3(64),pza4(64),pza5(64),pza6(64),pza7(64),pza8(64),
'     6 pxm1(64),pxm2(64),pxm3(64),pxm4(64),pxm5(64),pxm6(64),
'     7 pxm7(64),pxm8(64),pym1(64),pym2(64),pym3(64),pym4(64),
'     8 pym5(64),pym6(64),pym7(64),pym8(64),pzm1(64),pzm2(64),
'     9 pzm3(64),pzm4(64),pzm5(64),pzm6(64),pzm7(64),pzm8(64),
'     $ row1(64),row2(64),row3(64),row4(64),row5(64),row6(64),
'     & dum19(64)
'      
'c      
'c      
'      dimension s(324,*),d(6,6,*)
'c
'
'      do 8 i=lft,llt
'      row1(i)=d(1,1,i)+d(1,2,i)+d(1,3,i)
'      row2(i)=d(2,1,i)+d(2,2,i)+d(2,3,i)
'      row3(i)=d(3,1,i)+d(3,2,i)+d(3,3,i)
'      row4(i)=d(4,1,i)+d(4,2,i)+d(4,3,i)
'      row5(i)=d(5,1,i)+d(5,2,i)+d(5,3,i)
'      row6(i)=d(6,1,i)+d(6,2,i)+d(6,3,i)
'    8 continue
'      do 10 i=lft,llt
'      c101(i)=px1(i)*d(1,1,i)+py1(i)*d(1,4,i)+pz1(i)*d(1,6,i)
'     1 +row1(i)*pxm1(i)
'      c201(i)=px1(i)*d(2,1,i)+py1(i)*d(2,4,i)+pz1(i)*d(2,6,i)
'     1 +row2(i)*pxm1(i)
'      c301(i)=px1(i)*d(3,1,i)+py1(i)*d(3,4,i)+pz1(i)*d(3,6,i)
'     1 +row3(i)*pxm1(i)
'      c401(i)=px1(i)*d(4,1,i)+py1(i)*d(4,4,i)+pz1(i)*d(4,6,i)
'     1 +row4(i)*pxm1(i)
'      c501(i)=px1(i)*d(5,1,i)+py1(i)*d(5,4,i)+pz1(i)*d(5,6,i)
'     1 +row5(i)*pxm1(i)
'      c601(i)=px1(i)*d(6,1,i)+py1(i)*d(6,4,i)+pz1(i)*d(6,6,i)
'     1 +row6(i)*pxm1(i)
'      c102(i)=py1(i)*d(1,2,i)+px1(i)*d(1,4,i)+pz1(i)*d(1,5,i)
'     1 +row1(i)*pym1(i)
'      c202(i)=py1(i)*d(2,2,i)+px1(i)*d(2,4,i)+pz1(i)*d(2,5,i)
'     1 +row2(i)*pym1(i)
'      c302(i)=py1(i)*d(3,2,i)+px1(i)*d(3,4,i)+pz1(i)*d(3,5,i)
'     1 +row3(i)*pym1(i)
'      c402(i)=py1(i)*d(4,2,i)+px1(i)*d(4,4,i)+pz1(i)*d(4,5,i)
'     1 +row4(i)*pym1(i)
'      c502(i)=py1(i)*d(5,2,i)+px1(i)*d(5,4,i)+pz1(i)*d(5,5,i)
'     1 +row5(i)*pym1(i)
'      c602(i)=py1(i)*d(6,2,i)+px1(i)*d(6,4,i)+pz1(i)*d(6,5,i)
'     1 +row6(i)*pym1(i)
'   10 continue
'      do 20 i=lft,llt
'      c103(i)=pz1(i)*d(1,3,i)+py1(i)*d(1,5,i)+px1(i)*d(1,6,i)
'     1 +row1(i)*pzm1(i)
'      c203(i)=pz1(i)*d(2,3,i)+py1(i)*d(2,5,i)+px1(i)*d(2,6,i)
'     1 +row2(i)*pzm1(i)
'      c303(i)=pz1(i)*d(3,3,i)+py1(i)*d(3,5,i)+px1(i)*d(3,6,i)
'     1 +row3(i)*pzm1(i)
'      c403(i)=pz1(i)*d(4,3,i)+py1(i)*d(4,5,i)+px1(i)*d(4,6,i)
'     1 +row4(i)*pzm1(i)
'      c503(i)=pz1(i)*d(5,3,i)+py1(i)*d(5,5,i)+px1(i)*d(5,6,i)
'     1 +row5(i)*pzm1(i)
'      c603(i)=pz1(i)*d(6,3,i)+py1(i)*d(6,5,i)+px1(i)*d(6,6,i)
'     1 +row6(i)*pzm1(i)
'      c104(i)=px2(i)*d(1,1,i)+py2(i)*d(1,4,i)+pz2(i)*d(1,6,i)
'     1 +row1(i)*pxm2(i)
'      c204(i)=px2(i)*d(2,1,i)+py2(i)*d(2,4,i)+pz2(i)*d(2,6,i)
'     1 +row2(i)*pxm2(i)
'      c304(i)=px2(i)*d(3,1,i)+py2(i)*d(3,4,i)+pz2(i)*d(3,6,i)
'     1 +row3(i)*pxm2(i)
'      c404(i)=px2(i)*d(4,1,i)+py2(i)*d(4,4,i)+pz2(i)*d(4,6,i)
'     1 +row4(i)*pxm2(i)
'      c504(i)=px2(i)*d(5,1,i)+py2(i)*d(5,4,i)+pz2(i)*d(5,6,i)
'     1 +row5(i)*pxm2(i)
'      c604(i)=px2(i)*d(6,1,i)+py2(i)*d(6,4,i)+pz2(i)*d(6,6,i)
'     1 +row6(i)*pxm2(i)
'   20 continue
'      do 30 i=lft,llt
'      c105(i)=py2(i)*d(1,2,i)+px2(i)*d(1,4,i)+pz2(i)*d(1,5,i)
'     1 +row1(i)*pym2(i)
'      c205(i)=py2(i)*d(2,2,i)+px2(i)*d(2,4,i)+pz2(i)*d(2,5,i)
'     1 +row2(i)*pym2(i)
'      c305(i)=py2(i)*d(3,2,i)+px2(i)*d(3,4,i)+pz2(i)*d(3,5,i)
'     1 +row3(i)*pym2(i)
'      c405(i)=py2(i)*d(4,2,i)+px2(i)*d(4,4,i)+pz2(i)*d(4,5,i)
'     1 +row4(i)*pym2(i)
'      c505(i)=py2(i)*d(5,2,i)+px2(i)*d(5,4,i)+pz2(i)*d(5,5,i)
'     1 +row5(i)*pym2(i)
'      c605(i)=py2(i)*d(6,2,i)+px2(i)*d(6,4,i)+pz2(i)*d(6,5,i)
'     1 +row6(i)*pym2(i)
'      c106(i)=pz2(i)*d(1,3,i)+py2(i)*d(1,5,i)+px2(i)*d(1,6,i)
'     1 +row1(i)*pzm2(i)
'      c206(i)=pz2(i)*d(2,3,i)+py2(i)*d(2,5,i)+px2(i)*d(2,6,i)
'     1 +row2(i)*pzm2(i)
'      c306(i)=pz2(i)*d(3,3,i)+py2(i)*d(3,5,i)+px2(i)*d(3,6,i)
'     1 +row3(i)*pzm2(i)
'      c406(i)=pz2(i)*d(4,3,i)+py2(i)*d(4,5,i)+px2(i)*d(4,6,i)
'     1 +row4(i)*pzm2(i)
'      c506(i)=pz2(i)*d(5,3,i)+py2(i)*d(5,5,i)+px2(i)*d(5,6,i)
'     1 +row5(i)*pzm2(i)
'      c606(i)=pz2(i)*d(6,3,i)+py2(i)*d(6,5,i)+px2(i)*d(6,6,i)
'     1 +row6(i)*pzm2(i)
'   30 continue
'      do 40 i=lft,llt
'      c107(i)=px3(i)*d(1,1,i)+py3(i)*d(1,4,i)+pz3(i)*d(1,6,i)
'     1 +row1(i)*pxm3(i)
'      c207(i)=px3(i)*d(2,1,i)+py3(i)*d(2,4,i)+pz3(i)*d(2,6,i)
'     1 +row2(i)*pxm3(i)
'      c307(i)=px3(i)*d(3,1,i)+py3(i)*d(3,4,i)+pz3(i)*d(3,6,i)
'     1 +row3(i)*pxm3(i)
'      c407(i)=px3(i)*d(4,1,i)+py3(i)*d(4,4,i)+pz3(i)*d(4,6,i)
'     1 +row4(i)*pxm3(i)
'      c507(i)=px3(i)*d(5,1,i)+py3(i)*d(5,4,i)+pz3(i)*d(5,6,i)
'     1 +row5(i)*pxm3(i)
'      c607(i)=px3(i)*d(6,1,i)+py3(i)*d(6,4,i)+pz3(i)*d(6,6,i)
'     1 +row6(i)*pxm3(i)
'      c108(i)=py3(i)*d(1,2,i)+px3(i)*d(1,4,i)+pz3(i)*d(1,5,i)
'     1 +row1(i)*pym3(i)
'      c208(i)=py3(i)*d(2,2,i)+px3(i)*d(2,4,i)+pz3(i)*d(2,5,i)
'     1 +row2(i)*pym3(i)
'      c308(i)=py3(i)*d(3,2,i)+px3(i)*d(3,4,i)+pz3(i)*d(3,5,i)
'     1 +row3(i)*pym3(i)
'      c408(i)=py3(i)*d(4,2,i)+px3(i)*d(4,4,i)+pz3(i)*d(4,5,i)
'     1 +row4(i)*pym3(i)
'      c508(i)=py3(i)*d(5,2,i)+px3(i)*d(5,4,i)+pz3(i)*d(5,5,i)
'     1 +row5(i)*pym3(i)
'      c608(i)=py3(i)*d(6,2,i)+px3(i)*d(6,4,i)+pz3(i)*d(6,5,i)
'     1 +row6(i)*pym3(i)
'   40 continue
'      do 50 i=lft,llt
'      c109(i)=pz3(i)*d(1,3,i)+py3(i)*d(1,5,i)+px3(i)*d(1,6,i)
'     1 +row1(i)*pzm3(i)
'      c209(i)=pz3(i)*d(2,3,i)+py3(i)*d(2,5,i)+px3(i)*d(2,6,i)
'     1 +row2(i)*pzm3(i)
'      c309(i)=pz3(i)*d(3,3,i)+py3(i)*d(3,5,i)+px3(i)*d(3,6,i)
'     1 +row3(i)*pzm3(i)
'      c409(i)=pz3(i)*d(4,3,i)+py3(i)*d(4,5,i)+px3(i)*d(4,6,i)
'     1 +row4(i)*pzm3(i)
'      c509(i)=pz3(i)*d(5,3,i)+py3(i)*d(5,5,i)+px3(i)*d(5,6,i)
'     1 +row5(i)*pzm3(i)
'      c609(i)=pz3(i)*d(6,3,i)+py3(i)*d(6,5,i)+px3(i)*d(6,6,i)
'     1 +row6(i)*pzm3(i)
'      c110(i)=px4(i)*d(1,1,i)+py4(i)*d(1,4,i)+pz4(i)*d(1,6,i)
'     1 +row1(i)*pxm4(i)
'      c210(i)=px4(i)*d(2,1,i)+py4(i)*d(2,4,i)+pz4(i)*d(2,6,i)
'     1 +row2(i)*pxm4(i)
'      c310(i)=px4(i)*d(3,1,i)+py4(i)*d(3,4,i)+pz4(i)*d(3,6,i)
'     1 +row3(i)*pxm4(i)
'      c410(i)=px4(i)*d(4,1,i)+py4(i)*d(4,4,i)+pz4(i)*d(4,6,i)
'     1 +row4(i)*pxm4(i)
'      c510(i)=px4(i)*d(5,1,i)+py4(i)*d(5,4,i)+pz4(i)*d(5,6,i)
'     1 +row5(i)*pxm4(i)
'      c610(i)=px4(i)*d(6,1,i)+py4(i)*d(6,4,i)+pz4(i)*d(6,6,i)
'     1 +row6(i)*pxm4(i)
'   50 continue
'      do 60 i=lft,llt
'      c111(i)=py4(i)*d(1,2,i)+px4(i)*d(1,4,i)+pz4(i)*d(1,5,i)
'     1 +row1(i)*pym4(i)
'      c211(i)=py4(i)*d(2,2,i)+px4(i)*d(2,4,i)+pz4(i)*d(2,5,i)
'     1 +row2(i)*pym4(i)
'      c311(i)=py4(i)*d(3,2,i)+px4(i)*d(3,4,i)+pz4(i)*d(3,5,i)
'     1 +row3(i)*pym4(i)
'      c411(i)=py4(i)*d(4,2,i)+px4(i)*d(4,4,i)+pz4(i)*d(4,5,i)
'     1 +row4(i)*pym4(i)
'      c511(i)=py4(i)*d(5,2,i)+px4(i)*d(5,4,i)+pz4(i)*d(5,5,i)
'     1 +row5(i)*pym4(i)
'      c611(i)=py4(i)*d(6,2,i)+px4(i)*d(6,4,i)+pz4(i)*d(6,5,i)
'     1 +row6(i)*pym4(i)
'      c112(i)=pz4(i)*d(1,3,i)+py4(i)*d(1,5,i)+px4(i)*d(1,6,i)
'     1 +row1(i)*pzm4(i)
'      c212(i)=pz4(i)*d(2,3,i)+py4(i)*d(2,5,i)+px4(i)*d(2,6,i)
'     1 +row2(i)*pzm4(i)
'      c312(i)=pz4(i)*d(3,3,i)+py4(i)*d(3,5,i)+px4(i)*d(3,6,i)
'     1 +row3(i)*pzm4(i)
'      c412(i)=pz4(i)*d(4,3,i)+py4(i)*d(4,5,i)+px4(i)*d(4,6,i)
'     1 +row4(i)*pzm4(i)
'      c512(i)=pz4(i)*d(5,3,i)+py4(i)*d(5,5,i)+px4(i)*d(5,6,i)
'     1 +row5(i)*pzm4(i)
'      c612(i)=pz4(i)*d(6,3,i)+py4(i)*d(6,5,i)+px4(i)*d(6,6,i)
'     1 +row6(i)*pzm4(i)
'   60 continue
'      do 70 i=lft,llt
'      c113(i)=px5(i)*d(1,1,i)+py5(i)*d(1,4,i)+pz5(i)*d(1,6,i)
'     1 +row1(i)*pxm5(i)
'      c213(i)=px5(i)*d(2,1,i)+py5(i)*d(2,4,i)+pz5(i)*d(2,6,i)
'     1 +row2(i)*pxm5(i)
'      c313(i)=px5(i)*d(3,1,i)+py5(i)*d(3,4,i)+pz5(i)*d(3,6,i)
'     1 +row3(i)*pxm5(i)
'      c413(i)=px5(i)*d(4,1,i)+py5(i)*d(4,4,i)+pz5(i)*d(4,6,i)
'     1 +row4(i)*pxm5(i)
'      c513(i)=px5(i)*d(5,1,i)+py5(i)*d(5,4,i)+pz5(i)*d(5,6,i)
'     1 +row5(i)*pxm5(i)
'      c613(i)=px5(i)*d(6,1,i)+py5(i)*d(6,4,i)+pz5(i)*d(6,6,i)
'     1 +row6(i)*pxm5(i)
'      c114(i)=py5(i)*d(1,2,i)+px5(i)*d(1,4,i)+pz5(i)*d(1,5,i)
'     1 +row1(i)*pym5(i)
'      c214(i)=py5(i)*d(2,2,i)+px5(i)*d(2,4,i)+pz5(i)*d(2,5,i)
'     1 +row2(i)*pym5(i)
'      c314(i)=py5(i)*d(3,2,i)+px5(i)*d(3,4,i)+pz5(i)*d(3,5,i)
'     1 +row3(i)*pym5(i)
'      c414(i)=py5(i)*d(4,2,i)+px5(i)*d(4,4,i)+pz5(i)*d(4,5,i)
'     1 +row4(i)*pym5(i)
'      c514(i)=py5(i)*d(5,2,i)+px5(i)*d(5,4,i)+pz5(i)*d(5,5,i)
'     1 +row5(i)*pym5(i)
'      c614(i)=py5(i)*d(6,2,i)+px5(i)*d(6,4,i)+pz5(i)*d(6,5,i)
'     1 +row6(i)*pym5(i)
'   70 continue
'      do 80 i=lft,llt
'      c115(i)=pz5(i)*d(1,3,i)+py5(i)*d(1,5,i)+px5(i)*d(1,6,i)
'     1 +row1(i)*pzm5(i)
'      c215(i)=pz5(i)*d(2,3,i)+py5(i)*d(2,5,i)+px5(i)*d(2,6,i)
'     1 +row2(i)*pzm5(i)
'      c315(i)=pz5(i)*d(3,3,i)+py5(i)*d(3,5,i)+px5(i)*d(3,6,i)
'     1 +row3(i)*pzm5(i)
'      c415(i)=pz5(i)*d(4,3,i)+py5(i)*d(4,5,i)+px5(i)*d(4,6,i)
'     1 +row4(i)*pzm5(i)
'      c515(i)=pz5(i)*d(5,3,i)+py5(i)*d(5,5,i)+px5(i)*d(5,6,i)
'     1 +row5(i)*pzm5(i)
'      c615(i)=pz5(i)*d(6,3,i)+py5(i)*d(6,5,i)+px5(i)*d(6,6,i)
'     1 +row6(i)*pzm5(i)
'      c116(i)=px6(i)*d(1,1,i)+py6(i)*d(1,4,i)+pz6(i)*d(1,6,i)
'     1 +row1(i)*pxm6(i)
'      c216(i)=px6(i)*d(2,1,i)+py6(i)*d(2,4,i)+pz6(i)*d(2,6,i)
'     1 +row2(i)*pxm6(i)
'      c316(i)=px6(i)*d(3,1,i)+py6(i)*d(3,4,i)+pz6(i)*d(3,6,i)
'     1 +row3(i)*pxm6(i)
'      c416(i)=px6(i)*d(4,1,i)+py6(i)*d(4,4,i)+pz6(i)*d(4,6,i)
'     1 +row4(i)*pxm6(i)
'      c516(i)=px6(i)*d(5,1,i)+py6(i)*d(5,4,i)+pz6(i)*d(5,6,i)
'     1 +row5(i)*pxm6(i)
'      c616(i)=px6(i)*d(6,1,i)+py6(i)*d(6,4,i)+pz6(i)*d(6,6,i)
'     1 +row6(i)*pxm6(i)
'   80 continue
'      do 90 i=lft,llt
'      c117(i)=py6(i)*d(1,2,i)+px6(i)*d(1,4,i)+pz6(i)*d(1,5,i)
'     1 +row1(i)*pym6(i)
'      c217(i)=py6(i)*d(2,2,i)+px6(i)*d(2,4,i)+pz6(i)*d(2,5,i)
'     1 +row2(i)*pym6(i)
'      c317(i)=py6(i)*d(3,2,i)+px6(i)*d(3,4,i)+pz6(i)*d(3,5,i)
'     1 +row3(i)*pym6(i)
'      c417(i)=py6(i)*d(4,2,i)+px6(i)*d(4,4,i)+pz6(i)*d(4,5,i)
'     1 +row4(i)*pym6(i)
'      c517(i)=py6(i)*d(5,2,i)+px6(i)*d(5,4,i)+pz6(i)*d(5,5,i)
'     1 +row5(i)*pym6(i)
'      c617(i)=py6(i)*d(6,2,i)+px6(i)*d(6,4,i)+pz6(i)*d(6,5,i)
'     1 +row6(i)*pym6(i)
'      c118(i)=pz6(i)*d(1,3,i)+py6(i)*d(1,5,i)+px6(i)*d(1,6,i)
'     1 +row1(i)*pzm6(i)
'      c218(i)=pz6(i)*d(2,3,i)+py6(i)*d(2,5,i)+px6(i)*d(2,6,i)
'     1 +row2(i)*pzm6(i)
'      c318(i)=pz6(i)*d(3,3,i)+py6(i)*d(3,5,i)+px6(i)*d(3,6,i)
'     1 +row3(i)*pzm6(i)
'      c418(i)=pz6(i)*d(4,3,i)+py6(i)*d(4,5,i)+px6(i)*d(4,6,i)
'     1 +row4(i)*pzm6(i)
'      c518(i)=pz6(i)*d(5,3,i)+py6(i)*d(5,5,i)+px6(i)*d(5,6,i)
'     1 +row5(i)*pzm6(i)
'      c618(i)=pz6(i)*d(6,3,i)+py6(i)*d(6,5,i)+px6(i)*d(6,6,i)
'     1 +row6(i)*pzm6(i)
'   90 continue
'      do 100 i=lft,llt
'      c119(i)=px7(i)*d(1,1,i)+py7(i)*d(1,4,i)+pz7(i)*d(1,6,i)
'     1 +row1(i)*pxm7(i)
'      c219(i)=px7(i)*d(2,1,i)+py7(i)*d(2,4,i)+pz7(i)*d(2,6,i)
'     1 +row2(i)*pxm7(i)
'      c319(i)=px7(i)*d(3,1,i)+py7(i)*d(3,4,i)+pz7(i)*d(3,6,i)
'     1 +row3(i)*pxm7(i)
'      c419(i)=px7(i)*d(4,1,i)+py7(i)*d(4,4,i)+pz7(i)*d(4,6,i)
'     1 +row4(i)*pxm7(i)
'      c519(i)=px7(i)*d(5,1,i)+py7(i)*d(5,4,i)+pz7(i)*d(5,6,i)
'     1 +row5(i)*pxm7(i)
'      c619(i)=px7(i)*d(6,1,i)+py7(i)*d(6,4,i)+pz7(i)*d(6,6,i)
'     1 +row6(i)*pxm7(i)
'      c120(i)=py7(i)*d(1,2,i)+px7(i)*d(1,4,i)+pz7(i)*d(1,5,i)
'     1 +row1(i)*pym7(i)
'      c220(i)=py7(i)*d(2,2,i)+px7(i)*d(2,4,i)+pz7(i)*d(2,5,i)
'     1 +row2(i)*pym7(i)
'      c320(i)=py7(i)*d(3,2,i)+px7(i)*d(3,4,i)+pz7(i)*d(3,5,i)
'     1 +row3(i)*pym7(i)
'      c420(i)=py7(i)*d(4,2,i)+px7(i)*d(4,4,i)+pz7(i)*d(4,5,i)
'     1 +row4(i)*pym7(i)
'      c520(i)=py7(i)*d(5,2,i)+px7(i)*d(5,4,i)+pz7(i)*d(5,5,i)
'     1 +row5(i)*pym7(i)
'      c620(i)=py7(i)*d(6,2,i)+px7(i)*d(6,4,i)+pz7(i)*d(6,5,i)
'     1 +row6(i)*pym7(i)
'  100 continue
'      do 110 i=lft,llt
'      c121(i)=pz7(i)*d(1,3,i)+py7(i)*d(1,5,i)+px7(i)*d(1,6,i)
'     1 +row1(i)*pzm7(i)
'      c221(i)=pz7(i)*d(2,3,i)+py7(i)*d(2,5,i)+px7(i)*d(2,6,i)
'     1 +row2(i)*pzm7(i)
'      c321(i)=pz7(i)*d(3,3,i)+py7(i)*d(3,5,i)+px7(i)*d(3,6,i)
'     1 +row3(i)*pzm7(i)
'      c421(i)=pz7(i)*d(4,3,i)+py7(i)*d(4,5,i)+px7(i)*d(4,6,i)
'     1 +row4(i)*pzm7(i)
'      c521(i)=pz7(i)*d(5,3,i)+py7(i)*d(5,5,i)+px7(i)*d(5,6,i)
'     1 +row5(i)*pzm7(i)
'      c621(i)=pz7(i)*d(6,3,i)+py7(i)*d(6,5,i)+px7(i)*d(6,6,i)
'     1 +row6(i)*pzm7(i)
'      c122(i)=px8(i)*d(1,1,i)+py8(i)*d(1,4,i)+pz8(i)*d(1,6,i)
'     1 +row1(i)*pxm8(i)
'      c222(i)=px8(i)*d(2,1,i)+py8(i)*d(2,4,i)+pz8(i)*d(2,6,i)
'     1 +row2(i)*pxm8(i)
'      c322(i)=px8(i)*d(3,1,i)+py8(i)*d(3,4,i)+pz8(i)*d(3,6,i)
'     1 +row3(i)*pxm8(i)
'      c422(i)=px8(i)*d(4,1,i)+py8(i)*d(4,4,i)+pz8(i)*d(4,6,i)
'     1 +row4(i)*pxm8(i)
'      c522(i)=px8(i)*d(5,1,i)+py8(i)*d(5,4,i)+pz8(i)*d(5,6,i)
'     1 +row5(i)*pxm8(i)
'      c622(i)=px8(i)*d(6,1,i)+py8(i)*d(6,4,i)+pz8(i)*d(6,6,i)
'     1 +row6(i)*pxm8(i)
'  110 continue
'      do 120 i=lft,llt
'      c123(i)=py8(i)*d(1,2,i)+px8(i)*d(1,4,i)+pz8(i)*d(1,5,i)
'     1 +row1(i)*pym8(i)
'      c223(i)=py8(i)*d(2,2,i)+px8(i)*d(2,4,i)+pz8(i)*d(2,5,i)
'     1 +row2(i)*pym8(i)
'      c323(i)=py8(i)*d(3,2,i)+px8(i)*d(3,4,i)+pz8(i)*d(3,5,i)
'     1 +row3(i)*pym8(i)
'      c423(i)=py8(i)*d(4,2,i)+px8(i)*d(4,4,i)+pz8(i)*d(4,5,i)
'     1 +row4(i)*pym8(i)
'      c523(i)=py8(i)*d(5,2,i)+px8(i)*d(5,4,i)+pz8(i)*d(5,5,i)
'     1 +row5(i)*pym8(i)
'      c623(i)=py8(i)*d(6,2,i)+px8(i)*d(6,4,i)+pz8(i)*d(6,5,i)
'     1 +row6(i)*pym8(i)
'      c124(i)=pz8(i)*d(1,3,i)+py8(i)*d(1,5,i)+px8(i)*d(1,6,i)
'     1 +row1(i)*pzm8(i)
'      c224(i)=pz8(i)*d(2,3,i)+py8(i)*d(2,5,i)+px8(i)*d(2,6,i)
'     1 +row2(i)*pzm8(i)
'      c324(i)=pz8(i)*d(3,3,i)+py8(i)*d(3,5,i)+px8(i)*d(3,6,i)
'     1 +row3(i)*pzm8(i)
'      c424(i)=pz8(i)*d(4,3,i)+py8(i)*d(4,5,i)+px8(i)*d(4,6,i)
'     1 +row4(i)*pzm8(i)
'      c524(i)=pz8(i)*d(5,3,i)+py8(i)*d(5,5,i)+px8(i)*d(5,6,i)
'     1 +row5(i)*pzm8(i)
'      c624(i)=pz8(i)*d(6,3,i)+py8(i)*d(6,5,i)+px8(i)*d(6,6,i)
'     1 +row6(i)*pzm8(i)
'  120 continue
'
'      call fstif0(s)
'      call fstif1(s)
'      call fstif2(s)
'
'      return
'      end

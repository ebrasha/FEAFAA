'This file contains all the methods of printm.f
Partial Public Class clsInput
    ''' <summary>
    ''' print material model input data
    ''' </summary>
    ''' <param name="n"></param>
    ''' <param name="den"></param>
    ''' <param name="prop"></param>
    ''' <param name="csprop"></param>
    ''' <param name="itype"></param>
    ''' <param name="nip"></param>
    ''' <param name="trefm"></param>
    ''' <param name="rdc"></param>
    ''' <param name="tmecc"></param>
    ''' <param name="headng"></param>
    Public Sub printm(n As Integer, den As Double, prop() As Double,
                      csprop() As Double, itype As Integer,
                      nip As Integer, trefm As Double, rdc() As Double,
                      tmecc As Double, headng() As Double)

        Dim sb = New Text.StringBuilder()

        'TODO where to write?
        Dim msg1 = " " + headng.ToString("######.######")

        sb.Clear()
        sb.AppendLine("")
        'TODO where to write?
        '      write(luo,145) n,model,den,itype,trefm,rdc(1),rdc(2),tmecc
        sb.AppendLine(" material constants set number ....... {0}") 'i5
        sb.AppendLine(Space(4) + " material model .... {1}") 'i5
        sb.AppendLine(Space(5) + "den .............................. ={2}") 'e12.4
        sb.AppendLine(Space(5) + "element type ..................... ={3}") 'i5
        sb.AppendLine(Space(5) + "     eq.0: 8-node solid element            ")
        sb.AppendLine(Space(5) + "     eq.1:  2-node beam Or truss element    ")
        sb.AppendLine(Space(5) + "     eq.2: 4-node membrane or shell element")
        sb.AppendLine(Space(5) + "material reference temperature ... ={4}") 'e12.4
        sb.AppendLine(Space(5) + "Rayleigh damping coefficients:  [C] = a[M] + b[K]")
        sb.AppendLine(Space(5) + "  a .............................. ={5}") ' e12.4
        sb.AppendLine(Space(5) + "  b .............................. ={6}") ' e12.4
        sb.AppendLine(Space(5) + "Thermo-mech. energy conv. coeff... ={7}") ' e12.4
        Dim msg2 = String.Format(sb.ToString(),
                                 n.ToString("#####"),
                                 model.ToString("#####"),
                                 den.ToString("e4"),
                                 itype.ToString("#####"),
                                 trefm.ToString("e4"),
                                 rdc(1).ToString("e4"),
                                 rdc(2).ToString("e4"),
                                 tmecc.ToString("e4"))

        ' NKC
        If model = 1 OrElse model = 56 OrElse model = 57 OrElse model = 58 Then
            'TODO where to write?
            'Write(luo, 150)(prop(i), i = 1, 2)
            Dim msg3 = Space(5) + "e ................................ =" + prop(0).ToString("e4") + Environment.NewLine +
                Space(5) + "vnu .............................. =" + prop(1).ToString("e4")

            prop(30) = prop(1)
            prop(31) = prop(2)
            Call setse1(prop.Skip(30), n)
        ElseIf model = 4 Then
            'TODO where to write?
            '         write(luo,190) (prop(i),i=1,48)
            sb.Clear()
            sb.AppendLine(Space(5) + "temp ............................. ={0}") ',8(1x,e9.2)/
            sb.AppendLine(Space(5) + "e ................................ ={1}") ',8(1x,e9.2)/
            sb.AppendLine(Space(5) + "vnu .............................. ={2}") ',8(1x,e9.2)/
            sb.AppendLine(Space(5) + "alpha ............................ ={3}") ',8(1x,e9.2)/
            sb.AppendLine(Space(5) + "yield ............................ ={4}") ',8(1x,e9.2)/
            sb.AppendLine(Space(5) + "e (harden)....................... ={5}") ',8(1x,e9.2))
            Dim msgVal(5) As String
            For ind1 = 0 To 5
                msgVal(ind1) = ""
                For ind2 = 1 To 8
                    msgVal(ind1) += Space(1) + prop(ind1 * 8 + ind2).ToString("e2")
                Next
            Next
            Dim msg3 = String.Format(sb.ToString(), msgVal(0), msgVal(1), msgVal(2), msgVal(3), msgVal(4), msgVal(5))
            If itype = 1 Then
                If model = 1 Then
                    'TODO where to write?
                    'Write(luo, 151) prop(3)
                    Dim msg4 = Space(5) + "alpha (beams only) ............... =" + prop(2).ToString("e4")

                End If
                If csprop(1) = 0 Then csprop(1) = 5 / 6
                If csprop(2) = 0 Then csprop(2) = 2
                'TODO where to write?
                '         write(luo,340) (csprop(i),i=1,3)
                Dim msg5 = Space(5) + "shear area factor ................ =" + csprop(0).ToString("e.4") + Environment.NewLine +
                    Space(5) + "cross-section integration rule ... =" + csprop(1).ToString("e.4") + Environment.NewLine +
                    Space(5) + "     lt.0.0:absolute value is specified rule #  " + Environment.NewLine +
                    Space(5) + "     eq.1.0:1 x 1 gauss (truss)     " + Environment.NewLine +
                    Space(5) + "     eq.2.0:2 x 2 gauss (4 pt tube)" + Environment.NewLine +
                    Space(5) + "     eq.3.0:3 x 3 gauss " + Environment.NewLine +
                    Space(5) + "     eq.4.0:3 x 3 lobatto " + Environment.NewLine +
                    Space(5) + "     eq.5.0:4 x 4 gauss (16 pt tube)" + Environment.NewLine +
                    Space(5) + "cross section type .................. =" + csprop(2).ToString("e.4") + Environment.NewLine +
                    Space(5) + "     eq.0.0: square" + Environment.NewLine +
                    Space(5) + "     eq.1.0: tubular " + Environment.NewLine +
                    Space(5) + "     eq.2.0: user-defined rule" + Environment.NewLine
                '         write(luo,341) (csprop(i),i=9,16)
                Dim msg6 = Space(5) + "s-fiber lengths         node 1....... =" + csprop(8).ToString("e.4") + Environment.NewLine +
                    Space(5) + "(outer diam) : node 2 ....... =" + csprop(9).ToString("e.4") + Environment.NewLine +
                    Space(5) + "t-fiber lengths         node 1....... =" + csprop(10).ToString("e.4") + Environment.NewLine +
                    Space(5) + "(inner diam):            node 2 ....... =" + csprop(11).ToString("e.4") + Environment.NewLine +
                    Space(5) + "s-reference surface     node 1....... =" + csprop(12).ToString("e.4") + Environment.NewLine +
                    Space(5) + "     eq. 1.0:top         node 2 ....... =" + csprop(13).ToString("e.4") + Environment.NewLine +
                    Space(5) + "     eq. 0.0:middle    '/5x,'     eq.-1.0:bottom" + Environment.NewLine +
                    Space(5) + "t-reference surface:     node 1....... = " + csprop(14).ToString("e.4") + Environment.NewLine +
                    Space(5) + "     eq. 1.0top         node 2 ....... =" + csprop(15).ToString("e.4") + Environment.NewLine +
                    Space(5) + "     eq. 0.0: middle    '/5x,'     eq.-1.0:bottom" + Environment.NewLine

                If Math.Round(csprop(3)) = 1 AndAlso (Math.Round(csprop(2)) = 3 OrElse Math.Round(csprop(2)) = 4) Then
                    'TODO where to write?
                    '            write(lutty,996)
                    '            write(luo,996)
                    Dim msg7 = "" +
                        " *******************************************************" + Environment.NewLine +
                        " *                 - FATAL Error -                     *" + Environment.NewLine +
                        " *   Tubular beam cross section is defined with nine   *" + Environment.NewLine +
                        " *   gauss points.  Only quadrature rules 1, 2, Or 5   *" + Environment.NewLine +
                        " *   may be used for tubes.                            *" + Environment.NewLine +
                        " *******************************************************" + Environment.NewLine
                    ' Call com.adios(2)
                End If
            ElseIf itype = 2 Then
                If csprop(1) = 0 Then csprop(1) = 1
                If csprop(2) = 1 Then csprop(1) = 0
                If csprop(2) = 0 Then csprop(2) = 3
                If csprop(3) = 0 Then csprop(3) = 3
                If Math.Round(csprop(4)) < 0 Then csprop(2) = 0.0
                ' nipmx = Math.Max(nipmx, Math.Round(csprop(2)))   ' QW
                Dim csprop4 = csprop(4)
                If csprop(17) = -1 Then csprop4 = 2
                'TODO where to write?
                '         write(luo,350)csprop(1),csprop(2),csprop(3),csprop4,
                '     &                 csprop(6),csprop(7)
                Dim msg8 = "" +
                    Space(5) + "shear area factor ................ =" + csprop(0).ToString("e4") + Environment.NewLine +
                    Space(5) + "number Of thickness integ. pts. .. =" + csprop(1).ToString("e4") + Environment.NewLine +
                    Space(5) + "     (ignored if user-defined rule)  " + Environment.NewLine +
                    Space(5) + "print out Option ................. =" + csprop(2).ToString("e4") + Environment.NewLine +
                    Space(5) + "     eq.1.0:average resultants & fiber lengths            " + csprop(1).ToString("e4") + Environment.NewLine +
                    Space(5) + "     eq.2.0:resultants at plan points & fiber lengths     " + csprop(1).ToString("e4") + Environment.NewLine +
                    Space(5) + "     eq.3.0:resultants,stresses all points & fiber lengths" + csprop(1).ToString("e4") + Environment.NewLine +
                    Space(5) + "integration rule ................. =" + csprop4.ToString("e4") + Environment.NewLine +
                    Space(5) + "     lt.0.0:absolute value is specified rule #  " + csprop(1).ToString("e4") + Environment.NewLine +
                    Space(5) + "     eq.0.0:gauss (five points maximum)    " + csprop(1).ToString("e4") + Environment.NewLine +
                    Space(5) + "     eq.1.0:trapezoidal rule          " + csprop(1).ToString("e4") + Environment.NewLine +
                    Space(5) + "     eq.2.0:lobatto (five points maximum) " + csprop(1).ToString("e4") + Environment.NewLine +
                    Space(5) + "load curve for shell prestress option =" + csprop(5).ToString("e4") + Environment.NewLine +
                    Space(5) + "magnitude For shell prestress         =" + csprop(6).ToString("e4") + Environment.NewLine
                '         write(luo,360)(csprop(i),i=9,16)
                Dim msg9 = "" +
                    Space(5) + "fiber lengths:           node 1 ....... =" + csprop(8).ToString("e4") + Environment.NewLine +
                    Space(5) + "                         node 2 ....... =" + csprop(8).ToString("e4") + Environment.NewLine +
                    Space(5) + "                         node 3 ....... =" + csprop(10).ToString("e4") + Environment.NewLine +
                    Space(5) + "                         node 4 ....... =" + csprop(11).ToString("e4") + Environment.NewLine +
                    Space(5) + "reference surface:       node 1 ....... =" + csprop(12).ToString("e4") + Environment.NewLine +
                    Space(5) + "     eq. 1.0:top         node 2 ....... =" + csprop(13).ToString("e4") + Environment.NewLine +
                    Space(5) + "     eq. 0.0:middle      node 3 ....... =" + csprop(14).ToString("e4") + Environment.NewLine +
                    Space(5) + "     eq.-1.0:bottom      node 4 ....... =" + csprop(15).ToString("e4") + Environment.NewLine

                '         if(math.round(csprop(4)) = 0  andalso  csprop(2).gt.5.) then
                '           write(lutty,995)
                '           write(luo,995)
                Dim msg10 = "" +
                    " *******************************************************" + Environment.NewLine +
                    " *                 - FATAL ERROR -                     *" + Environment.NewLine +
                    " *  Shell cross section is defined using Gauss rule    *" + Environment.NewLine +
                    " *  And more than five thickness integration points.   *" + Environment.NewLine +
                    " *******************************************************" + Environment.NewLine
                ' Call com.adios(2)
            End If
        End If
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'c      subroutine printm (n,den,prop,csprop,thrmpr,itype,nip,
'c     &                   trefm,rdc,tmecc,nbcrb,headng)
'      subroutine printm (n,den,prop,csprop,itype,nip,
'     &                   trefm,rdc,tmecc,headng)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to print material model input data
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk10/iphase,nelgp,imass,model,np(8)
'      
'      
'      common/sefor/qhg,ishfor,isgso,ibmfor,ibgso,isthk
'     
'      
'      
'      common/mt23/prpsav(48)
'c
'      real*8 headng                                                     vax
'c     dimension prp(48),prop(*),csprop(*),thrmpr(48,*),headng(12),
'c     &          rdc(4),icrb(6)
'      dimension prp(48),prop(*),csprop(*),headng(12),rdc(4)
'c
'c     if(n.eq.1) write(luo,130) headng
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'printm'
'      ik01 = ik01 + 1
'      end if
'
'      write(luo,130) headng
'      write(luo,145) n,model,den,itype,trefm,rdc(1),rdc(2),tmecc
'c
'c NKC
'      if (model.eq.1 .or. model.eq.56 .or.
'     &    model.eq.57 .or. model.eq.58) then
'         write(luo,150) (prop(i), i=1,2)
'         prop(30)=prop(1)
'         prop(31)=prop(2)
'         call setse1(prop(30),n)
'c
'c      elseif (model.eq.2) then
'!         call setse2(prp,prop,prop(22))
'c          write(10,*) 'ik02 call setse2 1 in sub printm'
'
'c         write(luo,160) (prp(i),i=1,10)
'c         aopt=prp(10)
'c         if (aopt.eq.0.) write(luo,163) (prp(i),i=20,22)
'c         if (aopt.eq.1.) write(luo,164) (prp(i),i=11,13)
'c         if (aopt.eq.2.) write(luo,165) (prp(i),i=14,19)
'c
'c      elseif (model.eq.3) then
'c         write(luo,170) (prop(i), i=1,21)
'c         prop(30)=prop(1)
'c         prop(31)=prop(2)
'c         call setse1(prop(30),n)
'c
'      elseif (model.eq.4) then
'         write(luo,190) (prop(i),i=1,48)
'c
'c remove model 5 to model 35 QW 02-22-2017
'c
'      endif
'c
'c     write out cross-sectional properties
'c
'      if(itype.eq.1) then
'         if (model.eq.1) write(luo,151) prop(3)
'         if(csprop(1).eq.0.) csprop(1)=5./6.
'         if(csprop(2).eq.0.) csprop(2)=2.0
'         write(luo,340) (csprop(i),i=1,3)
'         write(luo,341) (csprop(i),i=9,16)
'         if(nint(csprop(3)).eq.1 .and.
'     &     (nint(csprop(2)).eq.3 .or. nint(csprop(2)).eq.4)) then
'            write(lutty,996)
'            write(luo,996)
'            call adios(2)
'         endif
'      elseif(itype.eq.2) then
'         if(csprop(1).eq.0.) csprop(1)=1.0
'         if(csprop(2).eq.1.) csprop(1)=0.0
'         if(csprop(2).eq.0.) csprop(2)=3.0
'         if(csprop(3).eq.0.) csprop(3)=3.0
'         if(nint(csprop(4)).lt.0) csprop(2)=0.0
'         nipmx=max(nipmx,nint(csprop(2)))
'         csprop4 = csprop(4)
'         if(csprop(17).eq.-1)csprop4 = 2
'         write(luo,350)csprop(1),csprop(2),csprop(3),csprop4,
'     &                 csprop(6),csprop(7)
'         write(luo,360)(csprop(i),i=9,16)
'         if(nint(csprop(4)).eq.0 .and. csprop(2).gt.5.) then
'           write(lutty,995)
'           write(luo,995)
'           call adios(2)
'         endif
'      endif
'      return
'c
'  130 format(//1x,12a6)
'  145 format(' material constants set number ....... ',i5  ,
'     &        4x,' material model .... ',i5//,
'     & 5x,'den .............................. =', e12.4/
'     & 5x,'element type ..................... =', i5/
'     & 5x,'     eq.0: 8-node solid element            '/
'     & 5x,'     eq.1: 2-node beam or truss element    '/
'     & 5x,'     eq.2: 4-node membrane or shell element'/
'     & 5x,'material reference temperature ... =', e12.4/
'     & 5x,'Rayleigh damping coefficients:  [C] = a[M] + b[K]'/
'     & 5x,'  a .............................. =', e12.4/
'     & 5x,'  b .............................. =', e12.4/
'     & 5x,'Thermo-mech. energy conv. coeff... =', e12.4/)
'  150 format(
'     & 5x,'e ................................ =', e12.4/
'     & 5x,'vnu .............................. =', e12.4)
'  151 format(
'     & 5x,'alpha (beams only) ............... =', e12.4)
'  160 format(
'     1 5x,'e(a) ............................. =', e12.4/,
'     2 5x,'e(b) ............................. =', e12.4/,
'     3 5x,'e(c) ............................. =', e12.4/,
'     4 5x,'vnu(ba) .......................... =', e12.4/,
'     5 5x,'vnu(ca) .......................... =', e12.4/,
'     6 5x,'vnu(cb) .......................... =', e12.4/,
'     7 5x,'g(ab) ............................ =', e12.4/,
'     8 5x,'g(bc) ............................ =', e12.4/,
'     9 5x,'g(ca) ............................ =', e12.4/,
'     $ 5x,'material axes option ............. =', e12.4)
'  161 format(
'     1 5x,'e(a) ............................. =', e12.4/,
'     2 5x,'e(b) ............................. =', e12.4/,
'     3 5x,'e(c) ............................. =', e12.4/,
'     4 5x,'vnu(ba) .......................... =', e12.4/,
'     5 5x,'vnu(ca) .......................... =', e12.4/,
'     6 5x,'vnu(cb) .......................... =', e12.4/,
'     7 5x,'alpha(a) ......................... =', e12.4/
'     8 5x,'alpha(b) ......................... =', e12.4/
'     9 5x,'alpha(c) ......................... =', e12.4/
'     & 5x,'g(ab) ............................ =', e12.4/,
'     1 5x,'g(bc) ............................ =', e12.4/,
'     2 5x,'g(ca) ............................ =', e12.4/,
'     3 5x,'material axes option ............. =', e12.4)
'  162 format(
'     1  5x,'compliance coefficients (upper triangle)',//,
'     2  2x,6e13.4/,
'     3 15x,5e13.4/,
'     4 28x,4e13.4/,
'     5 41x,3e13.4/,
'     6 54x,2e13.4/,
'     7 67x, e13.4//,
'     8  5x,'alpha(a) ......................... =', e12.4/
'     9  5x,'alpha(b) ......................... =', e12.4/
'     &  5x,'alpha(c) ......................... =', e12.4/
'     1  5x,'material axes option ............. =', e12.4)
'  163 format(/
'     1 10x,'node1 ....................... =', e12.4/,
'     2 10x,'node2 ....................... =', e12.4/,
'     3 10x,'node3 ....................... =', e12.4///)
'  164 format(/
'     1 10x,'xc    ....................... =', e12.4/,
'     2 10x,'yc    ....................... =', e12.4/,
'     3 10x,'zc    ....................... =', e12.4///)
'  165 format(/
'     1 10x,'a1    ....................... =', e12.4/,
'     2 10x,'a2    ....................... =', e12.4/,
'     3 10x,'a3    ....................... =', e12.4/,
'     4 10x,'d1    ....................... =', e12.4/,
'     5 10x,'d2    ....................... =', e12.4/,
'     6 10x,'d3    ....................... =', e12.4///)
'  170 format(
'     & 5x,'e ................................ =', e12.4/
'     & 5x,'vnu .............................. =', e12.4/
'     & 5x,'yield ............................ =', e12.4/
'     & 5x,'e (harden) ....................... =', e12.4/
'     & 5x,'hardening parameter .............. =', e12.4/
'     & 5x,'effective plastic strain ......... =',8(1x,e9.2)/
'     & 5x,'effective stress ................. =',8(1x,e9.2))
'  180 format(
'     & 5x,'shear ............................ =', e12.4/
'     & 5x,'bulk ............................. =', e12.4/
'     & 5x,'a0 ............................... =', e12.4/
'     & 5x,'a1 ............................... =', e12.4/
'     & 5x,'a2 ............................... =', e12.4/
'     & 5x,'pressure cutoff .................. =', e12.4/
'     & 5x,'unloading parameter .............. =', e12.4/
'     & 5x,'     eq.0: volumetric crushing      '/
'     & 5x,'     eq.1: no volumetric crushing   '/
'     & 5x,'ln(v/v0) ......................... =',9(1x,e8.1)/
'     & 5x,'pressure ......................... =',9(1x,e8.1))
'  190 format(
'     & 5x,'temp ............................. =',8(1x,e9.2)/
'     & 5x,'e ................................ =',8(1x,e9.2)/
'     & 5x,'vnu .............................. =',8(1x,e9.2)/
'     & 5x,'alpha ............................ =',8(1x,e9.2)/
'     & 5x,'yield ............................ =',8(1x,e9.2)/
'     & 5x,'e (harden) ....................... =',8(1x,e9.2))
'  200 format(
'     1 5x,'bulk ............................. =', e12.4/
'     2 5x,'vnu .............................. =', e12.4/
'     3 5x,'short time shear modulus ......... =', e12.4/
'     4 5x,'long time shear modulus .......... =', e12.4/
'     5 5x,'decay constant ................... =', e12.4)
'  240 format(
'     1 5x,'temp ............................. =',8(e9.2,1x)/
'     2 5x,'2g ............................... =',8(e9.2,1x)/
'     3 5x,'bulk ............................. =',8(e9.2,1x)/
'     4 5x,'alpha ............................ =',8(e9.2,1x)/
'     5 5x,'a0 ............................... =',8(e9.2,1x)/
'     6 5x,'b0 ............................... =',8(e9.2,1x))
'  250 format(
'     & 5x,'A ................................ =', e12.4/
'     & 5x,'B ................................ =', e12.4/
'     & 5x,'vnu .............................. =', e12.4/
'     & 5x,'augmentation flag ................ =', e12.4/
'     & 5x,'     eq.0.0: inactive               '/
'     & 5x,'     eq.1.0: active                 '/
'     & 5x,'augmentation tolerance ........... =', e12.4/)
'  251 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' *  Hyperelastic material model #15 augmentation       *',/
'     &' *  tolerance must be non-zero when augmentation flag  *',/
'     &' *  is active.                                         *',/
'     &' *******************************************************')
'  252 format(/
'     &' *******************************************************',/
'     &' *                   - WARNING -                       *',/
'     &' *  Material model type ',i3,', model #',i3,' requires the   *',/
'     &' *  geometric stiffness flag on CC#8 to be set.  The   *',/
'     &' *  flag has just been automatically activated.        *',/
'     &' *******************************************************')
'c 260 format(' MATERIAL DECK ECHO:'/
'c    &      ' card   -- 1 --     -- 2 --     -- 3 --     -- 4 --',
'c    &         '     -- 5 --     -- 6 --     -- 7 --     -- 8 --',
'c    &/'  1: ',8(1x,1pe11.4),
'c    &/'  2: ',8(1x,1pe11.4),
'c    &/'  3: ',8(1x,1pe11.4),
'c    &/'  4: ',8(1x,1pe11.4),
'c    &/'  5: ',8(1x,1pe11.4),
'c    &/'  6: ',8(1x,1pe11.4))
'  261 format(
'     & 5x,'Young"s modulus load curve # ........ =', e12.4/
'     & 5x,'Poisson"s ratio load curve # ........ =', e12.4/
'     & 5x,'CTE load curve # .................... =', e12.4/
'     & 5x,'Flow Strength Model ................. =', e12.4/
'     & 5x,'     eq.0.0: Elastic                '/
'     & 5x,'     eq.1.0: Elastic - plastic      '/
'     & 5x,'     eq.2.0: Elastic - Visoplastic  '/
'     & 5x,'     eq.3.0: Elastic - Visoplastic with recovery '/
'     & 5x,'Fluid Flow Model .................... =', e12.4/
'     & 5x,'     eq.0.0: None                   '/
'     & 5x,'     eq.1.0: Newtonian              '/
'     & 5x,'Void Growth Model ................... =', e12.4/
'     & 5x,'     eq.0.0: None                   '/
'     & 5x,'     eq.1.0: Pressure cut-off       '/
'     & 5x,'     eq.2.0: Gurson Void Growth     ')
' 2615 format(
'     & 5x,'Liquid - Solid Transition Model ..... =', e12.4/
'     & 5x,'     eq.0.0: None                   '/
'     & 5x,'     eq.1.0: Fraction solid interpolation'/
'     & 5x,'Plot Database Variable .............. =', e12.4/
'     & 5x,'     eq.0.0: Eff. plastic strain    '/
'     & 5x,'     eq.1.0: Void strain            '/
'     & 5x,'     eq.2.0: Flow strength          '/
'     & 5x,'     eq.3.0: Eff. strain rate       '/)
'  262 format(' Elastic Flow Strength Model 0'/
'     & 5x,'Bulk Modulus ........................ =', e12.4/)
'  263 format(' Elastic - Plastic Flow Strength Model 1'/
'     & 5x,'Bulk Modulus ........................ =', e12.4/
'     & 5x,'Yield Strength load curve # ......... =', e12.4/
'     & 5x,'Plastic Tangent load curve # ........ =', e12.4/)
'  264 format(' Elastic - Viscoplastic Flow Strength Model 2'/
'     & 5x,'Bulk Modulus ........................ =', e12.4/
'     & 5x,'Initial Strength load curve # ....... =', e12.4/
'     & 5x,'Flow Strength Coef. A load curve # .. =', e12.4/
'     & 5x,'Flow Strength Exp. m load curve # ... =', e12.4/
'     & 5x,'Strengthening Coef. B load curve # .. =', e12.4/
'     & 5x,'Strengthening Exp. n load curve # ... =', e12.4/)
'  265 format(
'     & ' Elastic - Viscoplastic Flow Strength with Recovery Model 3'/
'     & 5x,'Bulk Modulus ........................ =', e12.4/
'     & 5x,'Initial Strength load curve # ....... =', e12.4/
'     & 5x,'Flow Strength Coef. A load curve # .. =', e12.4/
'     & 5x,'Flow Strength Exp. m load curve # ... =', e12.4/
'     & 5x,'Strengthening Coef. k1 load curve # . =', e12.4/
'     & 5x,'Strengthening Exp. n1 load curve # .. =', e12.4/
'     & 5x,'Recovery Coef. k2 load curve # ...... =', e12.4/
'     & 5x,'Recovery Exp. n2 load curve # ....... =', e12.4/
'     & 5x,'Recovery Exp. n3 load curve # ....... =', e12.4/)
' 2655 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' * Thermal Plastic Melt material type 16, model #',i5,' *',/
'     &' *    ',i5,' is an invalid Flow Strength Model option   *',/
'     &' *******************************************************')
'  266 format(' Fluid Flow Model 1'/
'     & 5x,'Fluid viscosity load curve # ........ =', e12.4/)
' 2665 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' * Thermal Plastic Melt material type 16, model #',i5,' *',/
'     &' *      ',i5,' is an invalid Fluid Flow Model option     *',/
'     &' *******************************************************')
'  267 format(' Void Growth Model 1'/
'     & 5x,'Initial Void Strain ................. =', e12.4/
'     & 5x,'Tensile Pressure load curve # ....... =', e12.4/
'     & 5x,'Compressive Pressure load curve # ... =', e12.4/
'     & 5x,'Consistent Elastic Moduli Flag    ... =', e12.4/
'     & 5x,'     eq.0.0: off'/
'     & 5x,'     eq.1.0: on'/)
' 2675 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' * Thermal Plastic Melt material type 16, model #',i5,' *',/
'     &' *     ',i5,' is an invalid Void Growth Model option    *',/
'     &' *******************************************************')
'  268 format(' Void Growth Model 2'/
'     & 5x,'Initial Void Strain ................. =', e12.4/
'     & 5x,'First Gurson parameter .............. =', e12.4/
'     & 5x,'Second Gurson parameter ............. =', e12.4/
'     & 5x,'Consistent Elastic Moduli Flag    ... =', e12.4/
'     & 5x,'     eq.0.0: off'/
'     & 5x,'     eq.1.0: on'/)
'  269 format(' Liquid-Solid Transition Model 1'/
'     & 5x,'Fraction solid load curve # ......... =', e12.4/)
' 2695 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' * Thermal Plastic Melt material type 16, model #',i5,' *',/
'     &' *   ',i5,' is an invalid L/S Transition Model option   *',/
'     &' *******************************************************')
'  270 format(' Foundation Stiffness (upper triangle):'/
'     &3x,1pe12.5,1x,1pe12.5,1x,1pe12.5,1x,1pe12.5,1x,1pe12.5,1x,1pe12.5/
'     &16x,1pe12.5,1x,1pe12.5,1x,1pe12.5,1x,1pe12.5,1x,1pe12.5/
'     &29x,1pe12.5,1x,1pe12.5,1x,1pe12.5,1x,1pe12.5/
'     &42x,1pe12.5,1x,1pe12.5,1x,1pe12.5/
'     &55x,1pe12.5,1x,1pe12.5/
'     &68x,1pe12.5)
'  272 format(
'     & 5x,'Mooney-Rivlin Coefficient C1 ..... =', e12.4/
'     & 5x,'Mooney-Rivlin Coefficient C2 ..... =', e12.4/
'     & 5x,'Exponential Stress Coeff. C3 ..... =', e12.4/
'     & 5x,'Fiber Uncrimping Coeff.   C4 ..... =', e12.4/
'     & 5x,'Straightened Fiber Mod.   C5 ..... =', e12.4/
'     & 5x,'Bulk Modulus  .................... =', e12.4/
'     & 5x,'Straightened Fiber Stretch  ...... =', e12.4/
'     & 5x,'Viscoelastic Spectral Strength 1.. =', e12.4/
'     & 5x,'     ne.0: viscoelasticity enabled  '/
'     & 5x,'Viscoelastic Relaxation Time   1.. =', e12.4/
'     & 5x,'Viscoelastic Spectral Strength 2.. =', e12.4/
'     & 5x,'Viscoelastic Relaxation Time   2.. =', e12.4/
'     & 5x,'Viscoelastic Spectral Strength 3.. =', e12.4/
'     & 5x,'Viscoelastic Relaxation Time   3.. =', e12.4/
'     & 5x,'Viscoelastic Spectral Strength 4.. =', e12.4/
'     & 5x,'Viscoelastic Relaxation Time   4.. =', e12.4/
'     & 5x,'Viscoelastic Spectral Strength 5.. =', e12.4/
'     & 5x,'Viscoelastic Relaxation Time   5.. =', e12.4/
'     & 5x,'Viscoelastic Spectral Strength 6.. =', e12.4/
'     & 5x,'Viscoelastic Relaxation Time   6.. =', e12.4/
'     & 5x,'Initial Stretch Flag ............. =', e12.4/
'     & 5x,'     eq.1.0: active                 '/
'     & 5x,'Initial Stretch Load Curve ....... =', e12.4/
'     & 5x,'Augmentation Flag ................ =', e12.4/
'     & 5x,'     eq.1.0: active                 '/
'     & 5x,'Augmentation tolerance ........... =', e12.4/
'     & 5x,'material axes option ............. =', e12.4)
'  273 format(/
'     1 10x,'node1 ....................... =', e12.4/,
'     2 10x,'node2 ....................... =', e12.4/,
'     3 10x,'node3 ....................... =', e12.4///)
'  274 format(/
'     1 10x,'xc    ....................... =', e12.4/,
'     2 10x,'yc    ....................... =', e12.4/,
'     3 10x,'zc    ....................... =', e12.4///)
'  275 format(/
'     1 10x,'a1    ....................... =', e12.4/,
'     2 10x,'a2    ....................... =', e12.4/,
'     3 10x,'a3    ....................... =', e12.4/,
'     4 10x,'d1    ....................... =', e12.4/,
'     5 10x,'d2    ....................... =', e12.4/,
'     6 10x,'d3    ....................... =', e12.4///)
'  276 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' *  Transversely Isotropic Hyperelastic material #18,  *',/
'     &' *  model #',i5,':  Augmentation tolerance must be      *',/
'     &' *  non-zero when augmentation flag is active.         *',/
'     &' *******************************************************')
'  278 format(
'     & 5x,'Young"s modulus "e" .............. =',1pe12.4/
'     & 5x,'Poisson"s ratio "vnu" ............ =',1pe12.4/
'     & 5x,'stress coefficient "k" ........... =',1pe12.4/
'     & 5x,'  k<0 => load curve #(-k)'/
'     & 5x,'strain exponent "m" .............. =',1pe12.4/
'     & 5x,'  m<0 => load curve #(-m)'/
'     & 5x,'strain-rate exponent "n".......... =',1pe12.4/
'     & 5x,'  n<0 => load curve #(-n)'/
'     & 5x,'initial strain rate .............. =',1pe12.4)
'  279 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' *  Material model #19 is valid only for shell         *',/
'     &' *  elements (element type flag = 2), not solid or     *',/
'     &' *  beam elements                                      *',/
'     &' *******************************************************')
'  280 format(
'     & 5x,' ----- Slide Surface Data -----',/
'     & 5x,'e   .............................. =', e12.4/
'     & 5x,'vnu .............................. =', e12.4/
'     &/5x,' ----- Boundary Conditions -----    ',/
'     & 5x,'    code < 0:  fixed                ',/
'     & 5x,'    code = 0:  free                 ',/
'     & 5x,'    code > 0:  load curve number    ',/
'     & 5x,'code for X-displacement .......... =', i5/
'     & 5x,'code for Y-displacement .......... =', i5/
'     & 5x,'code for Z-displacement .......... =', i5/
'     & 5x,'code for X-rotation .............. =', i5/
'     & 5x,'code for Y-rotation .............. =', i5/
'     & 5x,'code for Z-rotation .............. =', i5/)
'  281 format(
'     & 5x,' ----- Center Of Mass -----',/
'     & 5x,'X-cm ............................. =', 1pe12.4/
'     & 5x,'Y-cm ............................. =', 1pe12.4/
'     & 5x,'Z-cm ............................. =', 1pe12.4)
'  282 format(
'     & 5x,' ----- Center Of Mass -----',/
'     & 5x,' (to be computed by NIKE3D)      ')
'  283 format(
'     & 5x,' --- Moments of Inertia ---',/
'     & 5x,'rotational inertia Ixx  .......... =', 1pe12.4/
'     & 5x,'rotational inertia Iyy  .......... =', 1pe12.4/
'     & 5x,'rotational inertia Izz  .......... =', 1pe12.4/
'     & 5x,'rotational inertia Ixy  .......... =', 1pe12.4/
'     & 5x,'rotational inertia Ixz  .......... =', 1pe12.4/
'     & 5x,'rotational inertia Iyz  .......... =', 1pe12.4)
'  284 format(
'     & 5x,' --- Moments of Inertia ---',/
'     & 5x,' (to be computed by NIKE3D)      ')
'  285 format(
'     & 5x,' --- Initial Velocities ---',/
'     & 5x,'translational velocity Vx  .......... =', 1pe12.4/
'     & 5x,'translational velocity Vy  .......... =', 1pe12.4/
'     & 5x,'translational velocity Vz  .......... =', 1pe12.4/
'     & 5x,'rotational    velocity Wx  .......... =', 1pe12.4/
'     & 5x,'rotational    velocity Wy  .......... =', 1pe12.4/
'     & 5x,'rotational    velocity Wz  .......... =', 1pe12.4)
'  286 format(
'     & 5x,' --- Initial Velocities ---',/
'     & 5x,'   (zero initial velocity)      ')
'  290 format(
'     & 5x,'e ................................ =', e12.4/
'     & 5x,'vnu .............................. =', e12.4/
'     & 5x,'yield ............................ =', e12.4/
'     & 5x,'e (harden) ....................... =', e12.4/
'     & 5x,'hardening parameter .............. =', e12.4/
'     & 5x,'effective plastic strain ......... =',8(1x,e9.2)/
'     & 5x,'effective stress ................. =',8(1x,e9.2))
'  291 format(
'     & 5x,'FLD Load Curves Numbers:'/
'     & 5x,'  failure option (IFLD) .......... =', e12.4/
'     & 5x,'  diagram,  left side (LCLH) ..... =', e12.4/
'     & 5x,'  diagram, right side (LCRH) ..... =', e12.4/
'     & 5x,'  pressure dependence (LCPX) ..... =', e12.4/
'     & 5x,'  FLD rate depencence (LCEDF) .... =', e12.4/
'     & 5x,'  yield stress rate dep. (LCEDM) . =', e12.4/
'     & 5x,'scale factor for deviatoric fail.. =', e12.4)
'  300   format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' *  In the current version of NIKE3D, each Gauss point *',/
'     &' *  should not have more than 6 crystallites.          *',/
'     &' *******************************************************')
'  301 format(5x,
'     & 'Crystal Type ............  FCC')
'  302 format(5x,
'     & 'Crystal Type ............  BCC with Pencil Glide')
'  303 format(5x,
'     & 'Crystal Type ............  BCC with {110} & {112}',
'     & ' slip planes')
'  304 format(5x,
'     & 'Crystal Type ............  BCC with {110}, {112}',
'     & ' & {123} slip planes')
'  305 format(5x,
'     & 'Crystal Type ............  HCP')
'  306 format(
'     & 5x, 'number of crystallite per Gauss point ... =', e12.4/
'     & 5x, 'crystal slip system indicator ... =', e12.4/
'     & 5x, 'orthotropic/isotropic elasticity indicator ... =', e12.4/
'     & 5x,'ptime ............................ =', e12.4/
'     & 5x,'dgammx ........................... =', e12.4/
'     & 5x,'tresmx ........................... =', e12.4/
'     & 5x,'t0 ............................... =', e12.4/
'     & 5x,'dummy ............................ =', e12.4/
'     & 5x,'xk ............................... =', e12.4/
'     & 5x,'xn ............................... =', e12.4/
'     & 5x,'tau0 ............................. =', 3(1x,e12.4)/
'     & 5x,'hlat ............................. =', e12.4/
'     & 5x,'a ................................ =', e12.4/
'     & 5x,'hxm .............................. =', e12.4/
'     & 5x,'theta ............................ =', e12.4/
'     & 5x,'C1111,C1122,C1212 ................ =', 3(1x,e12.4))
'  307 format(
'     & 5x,'Initial Crystal Orientation Angles (Eulerian) ......... ='/
'     & 5x,'..................................  ', 3(1x,e12.4))
'  332 format(
'     1 5x,'e ................................ =', e12.4/
'     2 5x,'vnu .............................. =', e12.4/
'     3 5x,'k ................................ =', e12.4/
'     3 5x,'m ................................ =', e12.4)
'  333 format(
'     & 5x,'temp ............................. =',8(1x,e9.2)/
'     & 5x,'e ................................ =',8(1x,e9.2)/
'     & 5x,'vnu .............................. =',8(1x,e9.2)/
'     & 5x,'alpha ............................ =',8(1x,e9.2)/
'     & 5x,'k................................. =',8(1x,e9.2)/
'     & 5x,'m (harden) ....................... =',8(1x,e9.2))
'  334 format(
'     & 5x,'vnu .............................. =',e9.4/
'     & 5x,'m (time exponent) ................ =',e9.4/
'     & 5x,'temp ............................. =',8(1x,e9.2)/
'     & 5x,'e ................................ =',8(1x,e9.2)/
'     & 5x,'a ................................ =',8(1x,e9.2)/
'     & 5x,'n (stress exponent) .............. =',8(1x,e9.2)/
'     & 5x,'alpha ............................ =',8(1x,e9.2))
'  336 format(
'     1 5x,'reference shear strain ........... =', e12.4/
'     2 5x,'reference shear stress ........... =', e12.4/
'     3 5x,'stress coefficient ............... =', e12.4/
'     4 5x,'stress exponent .................. =', e12.4/
'     5 5x,'bulk modulus ..................... =', e12.4/)
'  338 format(
'     & 5x,'e ................................ =', e12.4/
'     & 5x,'vnu .............................. =', e12.4/
'     & 5x,'tensile limit .................... =', e12.4/
'     & 5x,'shear limit ...................... =', e12.4/
'     & 5x,'compressive yield stress ......... =', e12.4/
'     & 5x,'     eq.0.0: no comp. plasticity          '/
'     & 5x,'fracture toughness ............... =', e12.4/
'     & 5x,'shear retention .................. =', e12.4/
'     & 5x,'viscosity ........................ =', e12.4/)
'  340 format(
'     & 5x,'shear area factor ................ =', e12.4/
'     & 5x,'cross-section integration rule ... =', e12.4/
'     & 5x,'     lt.0.0:absolute value is specified rule #  '/
'     & 5x,'     eq.1.0:1 x 1 gauss (truss)     '/
'     & 5x,'     eq.2.0:2 x 2 gauss (4 pt tube)'/
'     & 5x,'     eq.3.0:3 x 3 gauss '/
'     & 5x,'     eq.4.0:3 x 3 lobatto '/
'     & 5x,'     eq.5.0:4 x 4 gauss (16 pt tube)'//
'     & 5x,'cross section type .................. =', e12.4/
'     & 5x,'     eq.0.0: square'/
'     & 5x,'     eq.1.0: tubular'/
'     & 5x,'     eq.2.0: user-defined rule'/)
'  341 format(
'     & 5x,'s-fiber lengths:         node 1 ....... =', e12.4/
'     & 5x,'(outer diam):            node 2 ....... =', e12.4//
'     & 5x,'t-fiber lengths:         node 1 ....... =', e12.4/
'     & 5x,'(inner diam):            node 2 ....... =', e12.4//
'     & 5x,'s-reference surface:     node 1 ....... =', e12.4/
'     & 5x,'     eq. 1.0:top         node 2 ....... =', e12.4/
'     & 5x,'     eq. 0.0:middle    '/5x,'     eq.-1.0:bottom'//
'     & 5x,'t-reference surface:     node 1 ....... =', e12.4/
'     & 5x,'     eq. 1.0:top         node 2 ....... =', e12.4/
'     & 5x,'     eq. 0.0:middle    '/5x,'     eq.-1.0:bottom'//)
'  350 format(
'     & 5x,'shear area factor ................ =', e12.4//
'     & 5x,'number of thickness integ. pts. .. =', e12.4/
'     & 5x,'     (ignored if user-defined rule)  '//
'     & 5x,'print out option ................. =', e12.4/
'     & 5x,'     eq.1.0:average resultants & fiber lengths            '/
'     & 5x,'     eq.2.0:resultants at plan points & fiber lengths     '/
'     & 5x,'     eq.3.0:resultants,stresses all points & fiber lengths'//
'     $ 5x,'integration rule ................. =', e12.4/
'     $ 5x,'     lt.0.0:absolute value is specified rule #  '/
'     $ 5x,'     eq.0.0:gauss (five points maximum)    '/
'     $ 5x,'     eq.1.0:trapezoidal rule          '/
'     $ 5x,'     eq.2.0:lobatto (five points maximum) '/
'     $ 5x,'load curve for shell prestress option =',e12.4/
'     $ 5x,'magnitude for shell prestress         =',e12.4//)
'  360 format(
'     & 5x,'fiber lengths:           node 1 ....... =', e12.4/
'     & 5x,'                         node 2 ....... =', e12.4/
'     & 5x,'                         node 3 ....... =', e12.4/
'     & 5x,'                         node 4 ....... =', e12.4//
'     & 5x,'reference surface:       node 1 ....... =', e12.4/
'     & 5x,'     eq. 1.0:top         node 2 ....... =', e12.4/
'     & 5x,'     eq. 0.0:middle      node 3 ....... =', e12.4/
'     & 5x,'     eq.-1.0:bottom      node 4 ....... =', e12.4//)
'  430 format(
'     1 5x,'material axes option .................. =', e12.4)
'  450 format(/
'     1 10x,'node1 ............................ =', e12.4/,
'     2 10x,'node2 ............................ =', e12.4/,
'     3 10x,'node3 ............................ =', e12.4///)
'  451 format(/
'     1 10x,'xc    ............................ =', e12.4/,
'     2 10x,'yc    ............................ =', e12.4/,
'     3 10x,'zc    ............................ =', e12.4///)
'  452 format(/
'     1 10x,'a1    ............................ =', e12.4/,
'     2 10x,'a2    ............................ =', e12.4/,
'     3 10x,'a3    ............................ =', e12.4/,
'     4 10x,'d1    ............................ =', e12.4/,
'     5 10x,'d2    ............................ =', e12.4/,
'     6 10x,'d3    ............................ =', e12.4///)
'  453 format(/
'     1 10x,'a1    ............................ =', e12.4/,
'     2 10x,'a2    ............................ =', e12.4/,
'     3 10x,'a3    ............................ =', e12.4/,
'     4 10x,'angle ............................ =', e12.4///)
'  460 format(
'     & 5x,'fc ............................... =', e12.4/
'     & 5x,'eo ............................... =', e12.4/
'     & 5x,'z  ............................... =', e12.4/
'     & 5x,'eu ............................... =', e12.4/
'     & 5x,'plasticity factor ................ =', e12.4//)
'  470 format(
'     & 5x,'eo ............................... =', e12.4/
'     & 5x,'e1 ............................... =', e12.4/
'     & 5x,'sy ............................... =', e12.4/
'     & 5x,'r0 ............................... =', e12.4/
'     & 5x,'a1 ............................... =', e12.4/
'     & 5x,'a2 ............................... =', e12.4//)
'  860 format(
'     1(5x,'e(a) ............................. =', 3e11.2))
'  870 format(
'     1(5x,'e(b) ............................. =', 3e11.2))
'  880 format(
'     1(5x,'e(c) ............................. =', 3e11.2))
'  890 format(
'     1(5x,'vnu(ba) .......................... =', 3e11.2))
'  900 format(
'     1(5x,'vnu(ca) .......................... =', 3e11.2))
'  910 format(
'     1(5x,'vnu(cb) .......................... =', 3e11.2))
'  920 format(
'     1(5x,'alpha(a) ......................... =', 3e11.2))
'  930 format(
'     1(5x,'alpha(b) ......................... =', 3e11.2))
'  940 format(
'     1(5x,'alpha(c) ......................... =', 3e11.2))
'  950 format(
'     1(5x,'g(ab) ............................ =', 3e11.2))
'  960 format(
'     1(5x,'g(bc) ............................ =', 3e11.2))
'  970 format(
'     1(5x,'g(ca) ............................ =', 3e11.2))
'  980 format(
'     1(5x,'temps ............................ =', 3e11.2))
'  990 format(
'     1(5x,'material angles .................. =', 3e11.2))
'  991 format(///)
'  995 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' *  Shell cross section is defined using Gauss rule    *',/
'     &' *  and more than five thickness integration points.   *',/
'     &' *******************************************************')
'  996 format(/
'     &' *******************************************************',/
'     &' *                 - FATAL ERROR -                     *',/
'     &' *   Tubular beam cross section is defined with nine   *',/
'     &' *   gauss points.  Only quadrature rules 1, 2, or 5   *',/
'     &' *   may be used for tubes.                            *',/
'     &' *******************************************************')
'      end

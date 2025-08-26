Partial Public Class clsPrintOut  
    Sub stmaxp(ByVal path As String, ByRef nnd As Integer, ByRef xx(,) As Double, ByRef ss(,) As Double, ByRef mn() As Integer, ByRef nstep As Integer, ByVal ModelOut As Integer)

        Dim FileNo As Integer, FileName As String
        FileNo = FreeFile()
        FileName = path & "Output-Max Stress.txt"

        Dim fortran As Boolean = True      ' print FORTRAN format for comparison. ' QW 04-15-2019

        'FileOpen(FileNo, FileName, OpenMode.Output)    ' YC 102418-012819
        If ModelOut = 1 Then
            If nstep = 1 And clsCom.Ntimestep = 1 Then
                FileOpen(FileNo, FileName, OpenMode.Output)
            Else
                FileOpen(FileNo, FileName, OpenMode.Append)
            End If
        End If


        Dim ls(nnd) As Integer
        Dim i, iz As Integer

        For i = 1 To nnd
            ls(i) = -1
        Next i
        iz = 0
        If ModelOut = 1 Then
            'PrintLine(FileNo, " Step = " & clsCom.Ntimestep)
            PrintLine(FileNo, " Step = " & nstep)
            PrintLine(FileNo, " Maximum Horizontal Tensile Stress on the Concrete Slab")
            PrintLine(FileNo, " location direction node              x              y              z         stress")
        End If
        For i = 1 To nnd
            If (ss(7, i) > 0) Then
                For l = 1 To 6
                    ss(l, i) = ss(l, i) / ss(7, i)
                Next l
                ss(7, i) = Math.Sqrt(Math.Abs(0.5 * ((ss(1, i) - ss(2, i)) ^ 2 + (ss(2, i) - ss(3, i)) ^ 2 + (ss(3, i) - ss(1, i)) ^ 2) + 3 * (ss(4, i) ^ 2 + ss(5, i) ^ 2 + ss(6, i) ^ 2)))
            End If
        Next i

        Dim eo, zmin, zmax, zmin1, zmax1, zmin12, zmax12 As Double
        Dim icase, noverlay As Integer

        eo = 0.1
        icase = 1
        noverlay = 0
        zmax = -10000
        zmin = 10000

        For i = 1 To nnd
            If (mn(i) = 1) Then
                ls(i) = -2
                If (xx(3, i) > zmax) Then
                    zmax = xx(3, i)
                End If
                If (xx(3, i) < zmin) Then
                    zmin = xx(3, i)
                End If
            End If
        Next i

        zmax1 = zmax
        zmin1 = zmin
        zmax12 = -10000
        zmin12 = 10000

        For i = 1 To nnd
            If (mn(i) = 12) Then
                ls(i) = -2
                If (xx(3, i) > zmax12) Then
                    zmax12 = xx(3, i)
                End If
                If (xx(3, i) < zmin12) Then
                    zmin12 = xx(3, i)
                End If
            End If
        Next i

        If (Math.Abs(zmin12 - 10000) <= eo) Then noverlay = 1

        Dim sxmax1, symax1 As Double
        Dim inx1, iny1 As Integer
        Dim sxmax2, symax2 As Double
        Dim idir2, inx2, iny2 As Integer
        Dim fmt300 As String

10:     sxmax1 = 0
        symax1 = 0
        For i = 1 To nnd
            If ((ls(i) = -2) And (Math.Abs(zmax - xx(3, i)) <= eo) And (mn(i) = icase)) Then
                If (ss(1, i) > sxmax1) Then
                    sxmax1 = ss(1, i)
                    inx1 = i
                End If
                If (ss(2, i) > symax1) Then
                    symax1 = ss(2, i)
                    iny1 = i
                End If
            End If
        Next i


        sxmax2 = 0
        symax2 = 0
        idir2 = 1
        For i = 1 To nnd
            If ((ls(i) = -2) And (Math.Abs(xx(3, i) - zmin) <= eo) And (mn(i) = icase)) Then
                If (ss(1, i) > sxmax2) Then
                    sxmax2 = ss(1, i)
                    inx2 = i
                End If
                If (ss(2, i) > symax2) Then
                    symax2 = ss(2, i)
                    iny2 = i
                End If
            End If
        Next i
        If ModelOut = 1 Then
            If fortran Then
                fmt300 = "      Top         x" & LPad(5, Format(inx1, "0")) & LPad(15, PFortran(xx(1, inx1))) & LPad(15, PFortran(xx(2, inx1))) & LPad(15, PFortran(xx(3, inx1))) & LPad(15, PFortran(sxmax1)) & NL1 &
                         "      Top         y" & LPad(5, Format(iny1, "0")) & LPad(15, PFortran(xx(1, iny1))) & LPad(15, PFortran(xx(2, iny1))) & LPad(15, PFortran(xx(3, iny1))) & LPad(15, PFortran(symax1)) & NL1 &
                         "   Bottom         x" & LPad(5, Format(inx2, "0")) & LPad(15, PFortran(xx(1, inx2))) & LPad(15, PFortran(xx(2, inx2))) & LPad(15, PFortran(xx(3, inx2))) & LPad(15, PFortran(sxmax2)) & NL1 &
                         "   Bottom         y" & LPad(5, Format(iny2, "0")) & LPad(15, PFortran(xx(1, iny2))) & LPad(15, PFortran(xx(2, iny2))) & LPad(15, PFortran(xx(3, iny2))) & LPad(15, PFortran(symax2))
            Else
                fmt300 = "      Top         x" & LPad(5, Format(inx1, "0")) & LPad(15, Format(xx(1, inx1), "0.00000E+00")) & LPad(15, Format(xx(2, inx1), "0.00000E+00")) & LPad(15, Format(xx(3, inx1), "0.00000E+00")) & LPad(15, Format(sxmax1, "0.00000E+00")) & NL1 &
                         "      Top         y" & LPad(5, Format(iny1, "0")) & LPad(15, Format(xx(1, iny1), "0.00000E+00")) & LPad(15, Format(xx(2, iny1), "0.00000E+00")) & LPad(15, Format(xx(3, iny1), "0.00000E+00")) & LPad(15, Format(symax1, "0.00000E+00")) & NL1 &
                         "   Bottom         x" & LPad(5, Format(inx2, "0")) & LPad(15, Format(xx(1, inx2), "0.00000E+00")) & LPad(15, Format(xx(2, inx2), "0.00000E+00")) & LPad(15, Format(xx(3, inx2), "0.00000E+00")) & LPad(15, Format(sxmax2, "0.00000E+00")) & NL1 &
                         "   Bottom         y" & LPad(5, Format(iny2, "0")) & LPad(15, Format(xx(1, iny2), "0.00000E+00")) & LPad(15, Format(xx(2, iny2), "0.00000E+00")) & LPad(15, Format(xx(3, iny2), "0.00000E+00")) & LPad(15, Format(symax2, "0.00000E+00"))
            End If
            PrintLine(FileNo, fmt300)
        End If
        If ((noverlay <> 1) And (icase = 1)) Then
            icase = 12
            zmax = zmax12
            zmin = zmin12
            If ModelOut = 1 Then PrintLine(FileNo, " Overlay")
            GoTo 10
        End If

        zmin = 10000
        For i = 1 To nnd
            If (((mn(i) <> 0) And mn(i) <> 1) And (mn(i) <> 12)) Then
                ls(i) = 0
                If (xx(3, i) < zmin) Then
                    zmin = xx(3, i)
                End If
            End If
        Next i

20:     zmax = -10000
        For i = 1 To nnd
            If ((ls(i) = 0) And (xx(3, i) > zmax)) Then
                zmax = xx(3, i)
            End If
        Next i

        If ((zmax - zmin) < eo) Then
            GoTo 30
        End If

        iz = iz + 1
        For i = 1 To nnd
            If (((zmax - xx(3, i)) <= eo) And (ls(i) = 0)) Then
                ls(i) = iz
            End If
        Next i
        GoTo 20

30:     If ModelOut = 1 Then
            PrintLine(FileNo)
            PrintLine(FileNo, " Maximum Vertical Compressive Stress on the Foundation")
            PrintLine(FileNo, "    layer direction node              x              y              z         stress")
        End If
        Dim szmin As Double, inz As Integer, fmt400 As String
        For iiz = 1 To iz
            szmin = 10000
            For i = 1 To nnd
                If ((ls(i) = iiz) And (ss(3, i) < szmin)) Then
                    szmin = ss(3, i)
                    inz = i
                End If
            Next i
            If ModelOut = 1 Then
                If fortran Then
                    fmt400 = LPad(9, Format(iiz, "0")) & "         z" & LPad(5, Format(inz, "0")) & LPad(15, PFortran(xx(1, inz))) & LPad(15, PFortran(xx(2, inz))) & LPad(15, PFortran(xx(3, inz))) & LPad(15, PFortran(szmin))
                Else
                    fmt400 = LPad(9, Format(iiz, "0")) & "         z" & LPad(5, Format(inz, "0")) & LPad(15, Format(xx(1, inz), "0.00000E+00")) & LPad(15, Format(xx(2, inz), "0.00000E+00")) & LPad(15, Format(xx(3, inz), "0.00000E+00")) & LPad(15, Format(szmin, "0.00000E+00"))
                End If
                PrintLine(FileNo, fmt400)   ' QW 12-12-2018-
            End If
        Next iiz
        If ModelOut = 1 Then
            PrintLine(FileNo)
            PrintLine(FileNo)
            FileClose(FileNo)
        End If
    End Sub
    Private Function PFortran(ByVal x As Double) As String
        Dim I, N As Integer, y As Double
        If x >= 1 Or x <= -1 Then
            For I = 1 To 20
                y = x / (10 ^ I)
                If Math.Abs(y) < 1 Then
                    N = I
                    GoTo 10
                End If
            Next
        Else
            y = x
            N = 0
            GoTo 10
        End If
10:
        PFortran = Format(y, "0.00000") & "E+" & Format(N, "00")

    End Function
End Class




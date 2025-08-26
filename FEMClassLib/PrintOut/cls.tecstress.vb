Partial Public Class clsPrintOut  
    Sub tecstress(ByRef nnd As Integer, ByRef nne As Integer, ByRef moe(,) As Integer, ByRef nnt As Integer, ByRef np As Integer, ByRef mee() As Integer, ByVal path As String)


        Const a As Double = 0.5773502691896, b As Double = 1
        Const cc As Double = 1, fx As Double = 100, fy As Double = 100, fz As Double = 100  'scale factors

        Dim c As Double
        c = b / (2 * a)


        ' v3.0 005/YC 092820-2
        Dim aa, bb, cc0, dd As Double
        aa = (5 + 3 * Math.Sqrt(3)) / 4 : bb = -(Math.Sqrt(3) + 1) / 4 : cc0 = (Math.Sqrt(3) - 1) / 4 : dd = (5 - 3 * Math.Sqrt(3)) / 4
        Dim m11 = aa, m12 = bb, m13 = cc0, m14 = bb, m15 = bb, m16 = cc0, m17 = dd, m18 = cc0,
         m21 = m12, m22 = aa, m23 = bb, m24 = cc0, m25 = cc0, m26 = bb, m27 = cc0, m28 = dd,
         m31 = m13, m32 = m23, m33 = aa, m34 = bb, m35 = dd, m36 = cc0, m37 = bb, m38 = cc0,
         m41 = m14, m42 = m24, m43 = m34, m44 = aa, m45 = cc0, m46 = dd, m47 = cc0, m48 = bb,
         m51 = m15, m52 = m25, m53 = m35, m54 = m45, m55 = aa, m56 = bb, m57 = cc0, m58 = bb,
         m61 = m16, m62 = m26, m63 = m36, m64 = m46, m65 = m56, m66 = aa, m67 = bb, m68 = cc0,
         m71 = m17, m72 = m27, m73 = m37, m74 = m47, m75 = m57, m76 = m67, m77 = aa, m78 = bb,
         m81 = m18, m82 = m28, m83 = m38, m84 = m48, m85 = m58, m86 = m68, m87 = m78, m88 = aa
        ' v3.0 005/YC 092820-2 END


        Dim xdefomed(3, nnd) As Double 'postion after deformation
        Dim ss(nnd, 7), tt(nne, 7, 8) As Double

        Dim i, j, it, ime, ip, nl As Integer
        Dim fmt As String


        For it = 1 To nnt


            clsCom.Ntimestep = it   ' YC 121219-2


            Dim s As String, ns As Integer
            If clsCom.Ntimestep = 1 Then
                For ns = 2 To 20
                    s = path & "model_stress_" & ns & ".dat"
                    If System.IO.File.Exists(s) = True Then
                        System.IO.File.Delete(s)
                    End If
                Next
            End If
            Dim FileNo As Integer, FileName As String
            FileNo = FreeFile()
            FileName = path & "model_stress_" & clsCom.Ntimestep & ".dat"

            FileOpen(FileNo, FileName, OpenMode.Output)

            Dim header As String = "TITLE=""Model_STRESS""" & NL1 &
                                   "VARIABLES=""X"",""Y"",""Z"",""XX"",""YY"",""ZZ"", ""XY"",""YZ"",""ZX"",""MISES"",""TEMPERATURE"""

            PrintLine(FileNo, header)

            For i = 1 To nne
                For j = 1 To 6


                    ' v3.0 005/YC 092820-2
                    'tt(i, j, 1) = (st(i, j, 1, it) - st(i, j, 7, it)) * c + 0.5 * (st(i, j, 1, it) + st(i, j, 7, it))
                    'tt(i, j, 2) = (st(i, j, 2, it) - st(i, j, 8, it)) * c + 0.5 * (st(i, j, 2, it) + st(i, j, 8, it))
                    'tt(i, j, 3) = (st(i, j, 3, it) - st(i, j, 5, it)) * c + 0.5 * (st(i, j, 3, it) + st(i, j, 5, it))
                    'tt(i, j, 4) = (st(i, j, 4, it) - st(i, j, 6, it)) * c + 0.5 * (st(i, j, 4, it) + st(i, j, 6, it))
                    'tt(i, j, 5) = (st(i, j, 5, it) - st(i, j, 3, it)) * c + 0.5 * (st(i, j, 5, it) + st(i, j, 3, it))
                    'tt(i, j, 6) = (st(i, j, 6, it) - st(i, j, 4, it)) * c + 0.5 * (st(i, j, 6, it) + st(i, j, 4, it))
                    'tt(i, j, 7) = (st(i, j, 7, it) - st(i, j, 1, it)) * c + 0.5 * (st(i, j, 7, it) + st(i, j, 1, it))
                    'tt(i, j, 8) = (st(i, j, 8, it) - st(i, j, 2, it)) * c + 0.5 * (st(i, j, 8, it) + st(i, j, 2, it))
                    tt(i, j, 1) = m11 * st(i, j, 1, it) + m12 * st(i, j, 2, it) + m13 * st(i, j, 3, it) + m14 * st(i, j, 4, it) + m15 * st(i, j, 5, it) + m16 * st(i, j, 6, it) + m17 * st(i, j, 7, it) + m18 * st(i, j, 8, it)
                    tt(i, j, 2) = m21 * st(i, j, 1, it) + m22 * st(i, j, 2, it) + m23 * st(i, j, 3, it) + m24 * st(i, j, 4, it) + m25 * st(i, j, 5, it) + m26 * st(i, j, 6, it) + m27 * st(i, j, 7, it) + m28 * st(i, j, 8, it)
                    tt(i, j, 3) = m31 * st(i, j, 1, it) + m32 * st(i, j, 2, it) + m33 * st(i, j, 3, it) + m34 * st(i, j, 4, it) + m35 * st(i, j, 5, it) + m36 * st(i, j, 6, it) + m37 * st(i, j, 7, it) + m38 * st(i, j, 8, it)
                    tt(i, j, 4) = m41 * st(i, j, 1, it) + m42 * st(i, j, 2, it) + m43 * st(i, j, 3, it) + m44 * st(i, j, 4, it) + m45 * st(i, j, 5, it) + m46 * st(i, j, 6, it) + m47 * st(i, j, 7, it) + m48 * st(i, j, 8, it)
                    tt(i, j, 5) = m51 * st(i, j, 1, it) + m52 * st(i, j, 2, it) + m53 * st(i, j, 3, it) + m54 * st(i, j, 4, it) + m55 * st(i, j, 5, it) + m56 * st(i, j, 6, it) + m57 * st(i, j, 7, it) + m58 * st(i, j, 8, it)
                    tt(i, j, 6) = m61 * st(i, j, 1, it) + m62 * st(i, j, 2, it) + m63 * st(i, j, 3, it) + m64 * st(i, j, 4, it) + m65 * st(i, j, 5, it) + m66 * st(i, j, 6, it) + m67 * st(i, j, 7, it) + m68 * st(i, j, 8, it)
                    tt(i, j, 7) = m71 * st(i, j, 1, it) + m72 * st(i, j, 2, it) + m73 * st(i, j, 3, it) + m74 * st(i, j, 4, it) + m75 * st(i, j, 5, it) + m76 * st(i, j, 6, it) + m77 * st(i, j, 7, it) + m78 * st(i, j, 8, it)
                    tt(i, j, 8) = m81 * st(i, j, 1, it) + m82 * st(i, j, 2, it) + m83 * st(i, j, 3, it) + m84 * st(i, j, 4, it) + m85 * st(i, j, 5, it) + m86 * st(i, j, 6, it) + m87 * st(i, j, 7, it) + m88 * st(i, j, 8, it)
                    ' v3.0 005/YC 092820-2 END


                Next j
            Next i

            For i = 1 To nnd
                'ss(i,1:7)=0.
                For j = 1 To 7
                    ss(i, j) = 0
                Next j

                nl = 0
                For j = 1 To nne
                    For k = 1 To 8
                        If (moe(j, k) = i) Then
                            For l = 1 To 6
                                ss(i, l) = ss(i, l) + tt(j, l, k)
                            Next l
                            nl = nl + 1
                        End If
                    Next k

                Next j

                If (nl <> 0) Then
                    For l = 1 To 6
                        ss(i, l) = ss(i, l) / nl
                    Next l
                    ss(i, 7) = Math.Sqrt(Math.Abs(0.5 * ((ss(i, 1) - ss(i, 2)) ^ 2 + (ss(i, 2) - ss(i, 3)) ^ 2 + (ss(i, 3) - ss(i, 1)) ^ 2) + 3 * (ss(i, 4) ^ 2 + ss(i, 5) ^ 2 + ss(i, 6) ^ 2)))
                End If

            Next i



            ime = 0
            For ip = 1 To np
                If (ip > 1) Then
                    ime = ime + mee(ip - 1)
                End If

                PrintLine(FileNo, "ZONE T=""nn." & ip & """")
                PrintLine(FileNo, "N=" & LPad(5, nnd) & ",E=" & LPad(5, mee(ip)) & ",F=FEPOINT,ET=BRICK")

                For i = 1 To nnd
                    xdefomed(1, i) = x(1, i) + cc * fx * snl(i, 1, it)
                    xdefomed(2, i) = x(2, i) + cc * fy * snl(i, 2, it)
                    xdefomed(3, i) = x(3, i) + cc * fz * snl(i, 3, it)

                    fmt = ""
                    For j = 1 To 3
                        fmt = fmt & LPad(20, Format(xdefomed(j, i), "0.0000000000000E+00")) & " "
                    Next j

                    For j = 1 To 7
                        fmt = fmt & LPad(20, Format(ss(i, j), "0.0000000000000E+00")) & " "
                    Next j

                    fmt = fmt & LPad(20, Format(snl(i, 4, it), "0.0000000000000E+00"))

                    PrintLine(FileNo, fmt)
                Next i

                For i = ime + 1 To ime + mee(ip)
                    For j = 1 To 8
                        Print(FileNo, LPad(8, moe(i, j)))
                    Next j
                    PrintLine(FileNo)
                Next i

            Next ip


            FileClose(FileNo)
        Next it

    End Sub
End Class




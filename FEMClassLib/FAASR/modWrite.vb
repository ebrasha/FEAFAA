Module modWrite

    ' Public clsCom As New clsRead
    Sub Check2D(ByVal D(,) As Integer, ByVal ii As Integer, ByVal jj As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        Dim i, j As Integer
        Dim s As String
        For j = 1 To jj
            s = Format(j, "0")
            For i = 1 To ii
                s = s & ", " & Format(D(i, j), "0")
            Next
            PrintLine(LFNo, s)
        Next
        FileClose(LFNo)

    End Sub
    Sub Check2D1(ByVal D(,) As Double, ByVal ii As Integer, ByVal jj As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        Dim s As String

        s = Format(jj, "0")
        s = s & ", " & Format(D(ii, jj), "0.00000000E+00")
        PrintLine(LFNo, s)
        FileClose(LFNo)

    End Sub
    Sub Check2D(ByVal D(,) As Double, ByVal ii As Integer, ByVal jj As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        'FileOpen(LFNo, FileName, OpenMode.Output, , , 1024)
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        Dim i, j As Integer
        Dim s As String

        For j = 1 To jj
            s = Format(j, "0")
            For i = 1 To ii
                s = s & ", " & Format(D(i, j), "0.00000000E+00")
            Next
            PrintLine(LFNo, s)
        Next
        FileClose(LFNo)

    End Sub
    Sub Check2DT(ByVal D(,) As Integer, ByVal ii As Integer, ByVal jj As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        Dim i, j As Integer
        Dim s As String
        For i = 1 To ii
            s = Format(i, "0")
            For j = 1 To jj
                s = s & ", " & Format(D(i, j), "0")
            Next
            PrintLine(LFNo, s)
        Next
        FileClose(LFNo)

    End Sub
    Sub Check2DT(ByVal D(,) As Double, ByVal ii As Integer, ByVal jj As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        Dim i, j As Integer
        Dim s As String
        For i = 1 To ii
            s = Format(i, "0")
            For j = 1 To jj
                s = s & ", " & Format(D(i, j), "0.000000000000E+00")
            Next
            PrintLine(LFNo, s)
        Next
        FileClose(LFNo)

    End Sub
    Sub Check3D(ByVal D(,,) As Double, ByVal ii As Integer, ByVal jj As Integer, ByVal kk As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check.csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Output, , , 1024)
        Dim i, j, k As Integer
        Dim s As String
        For i = 1 To ii
            For j = 1 To jj
                s = Format(i, "0") & "," & Format(j, "0")
                For k = 1 To kk
                    s = s & ", " & Format(D(i, j, k), "0.000000000E+00")
                Next
                PrintLine(LFNo, s)
            Next
        Next
        FileClose(LFNo)

    End Sub
    Sub Check1D(ByVal D() As Integer, ByVal ii As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        Dim i As Integer
        Dim s As String

        For i = 1 To ii
            s = Format(D(i), "0")
            PrintLine(LFNo, s)
        Next i
        FileClose(LFNo)

    End Sub
    Sub Check1D(ByVal D() As Double, ByVal ii As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        Dim i As Integer
        Dim s As String

        For i = 1 To ii
            s = Format(i, "0")
            s = s & ", " & Format(D(i), "0.00000000E+00")
            PrintLine(LFNo, s)
        Next i
        FileClose(LFNo)

    End Sub
    Sub Check1D2(ByVal D() As Double, ByVal ii As Integer, ByVal jj As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check.csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        Dim i, j As Integer
        Dim s As String
        For i = 1 To ii
            s = Format(D(ii * (i - 1) + 1), "0.0000000E+00")
            For j = 2 To jj
                s = s & "," & Format(D(ii * (i - 1) + j), "0.0000000E+00")
            Next j
            PrintLine(LFNo, s)
        Next i
        FileClose(LFNo)

    End Sub
    Sub Check13(ByVal ng As Integer, ByVal D As Double, ByVal E As Double, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        Dim s As String
        s = Format(ng, "0") & "," & Format(D, "0.0000000E+00") & "," & Format(E, "0.0000000E+00")
        PrintLine(LFNo, s)
        FileClose(LFNo)

    End Sub
    Sub Checkrhs(ByVal D() As Double, ByVal E(,) As Integer, ByVal ng As Integer, ByVal i As Integer)
        Dim FileName As String
        Dim LFNo As Integer
        Dim s As String
        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check.csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        s = Format(ng, "0") & "," & Format(i, "0")
        For j = 1 To 24
            s = s & "," & Format(D(E(j, i)), "0.0000000E+00")
        Next j
        PrintLine(LFNo, s)
        FileClose(LFNo)

    End Sub
    Sub Checkrhs2(ByVal D() As Integer, ByVal E() As Double, ByVal i As Integer)
        Dim FileName As String
        Dim LFNo As Integer
        Dim s As String
        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check.csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        s = Format(i, "0")
        For j = 1 To 15
            s = s & "," & Format(D(j), "0") & "," & Format(E(j), "0.0000000E+00")
        Next j
        PrintLine(LFNo, s)
        FileClose(LFNo)

    End Sub
    Sub Checka2(ByVal D() As Double, ByVal E() As Double, ByVal n As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer
        Dim s As String
        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        s = ""
        For j = 1 To n
            If D(j) <> 0 Or E(j) <> 0 Then
                s = Format(j, "0") & "," & Format(D(j), "0.0000000E+00") & "," & Format(E(j), "0.0000000E+00")
                PrintLine(LFNo, s)
            End If
        Next j

        FileClose(LFNo)

    End Sub
    Sub Checka22(ByVal D(,) As Double, ByVal E(,) As Double, ByVal n As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer
        Dim s As String
        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)
        s = ""
        For j = 1 To n
            'If D(j) <> 0 Or E(j) <> 0 Then
            s = Format(j, "0") & "," & Format(D(1, j), "0.0000000E+00") & "," & Format(E(1, j), "0.0000000E+00")
            PrintLine(LFNo, s)
            'End If
        Next j

        FileClose(LFNo)

    End Sub
    Sub Checkrdot(ByVal rhsc As Double, ByVal ng As Integer, ByVal i As Integer)
        Dim FileName As String
        Dim LFNo As Integer
        Dim s As String
        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check.csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        s = Format(ng, "0") & "," & Format(i, "0")
        s = s & "," & Format(rhsc, "0.0000000E+00")

        PrintLine(LFNo, s)
        FileClose(LFNo)

    End Sub
    Sub Check5(ByVal A As Double, ByVal B As Double, ByVal C As Double, ByVal D As Double, ByVal E As Double)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check.csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        Dim s As String
        s = Format(A, "0.0000000E+00") & "," & Format(B, "0.0000000E+00") & "," & Format(C, "0.0000000E+00") & "," & Format(D, "0.0000000E+00") & "," & Format(E, "0.0000000E+00")
        PrintLine(LFNo, s)
        FileClose(LFNo)

    End Sub
    Sub Check4(ByVal A As Integer, ByVal B As Integer, ByVal C As Integer, ByVal D As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        Dim s As String
        s = Format(A, "0") & "," & Format(B, "0") & "," & Format(C, "0") & "," & Format(D, "0")
        PrintLine(LFNo, s)
        FileClose(LFNo)

    End Sub
    Sub Check1D3(ByVal D() As Double, ByVal E() As Double, ByVal F() As Double, ByVal n As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim i, LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        Dim s As String
        For i = 1 To n
            s = Format(i, "0")
            s = s & "," & Format(D(i), "0.0000000E+00") & "," & Format(E(i), "0.0000000E+00") & "," & Format(F(i), "0.0000000E+00")
            PrintLine(LFNo, s)
        Next i
        FileClose(LFNo)

    End Sub
    Sub Check1D32(ByVal D() As Double, ByVal E() As Double, ByVal F(,) As Double, ByVal n As Integer, ByVal ipt As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim i, LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        Dim s As String
        For i = 1 To n
            s = Format(i, "0")
            s = s & "," & Format(D(i), "0.0000000E+00") & "," & Format(E(i), "0.0000000E+00") & "," & Format(F(i, ipt), "0.0000000E+00")
            PrintLine(LFNo, s)
        Next i
        FileClose(LFNo)

    End Sub
    Sub Check1D33(ByVal D(,) As Double, ByVal E(,) As Double, ByVal F() As Double, ByVal n As Integer, ByVal j As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim i, LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        Dim s As String
        For i = 1 To n
            s = Format(j, "0")
            s = s & "," & Format(D(1, j + i), "0.0000000E+00") & "," & Format(E(1, j + i), "0.0000000E+00") & "," & Format(F(i), "0.0000000E+00")
            PrintLine(LFNo, s)
        Next i
        FileClose(LFNo)

    End Sub
    Sub Check3(ByVal D() As Double, ByVal E() As Double, ByVal F() As Double, ByVal i As Integer, ByVal istep As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check-" & istep & ".csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        Dim s As String
        s = Format(i, "0")
        s = s & "," & Format(D(i), "0.0000000E+00") & "," & Format(E(i), "0.0000000E+00") & "," & Format(F(i), "0.0000000E+00")
        PrintLine(LFNo, s)

        FileClose(LFNo)

    End Sub
    Sub CheckName(ByVal A As String)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check.csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        Dim s As String
        s = A
        PrintLine(LFNo, s)
        FileClose(LFNo)

    End Sub

    Sub CheckName2(ByVal A As String, ByVal i As Integer)
        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\check.csv"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Append, , , 1024)

        Dim s As String
        s = A & Format(i, "0")
        PrintLine(LFNo, s)
        FileClose(LFNo)

    End Sub
    Sub read(ByVal D() As Double, ByVal n As Integer)
        Dim FileName As String
        Dim i As Integer
        Dim filereader As System.IO.StreamReader

        Dim ReadLine, elements() As String

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = clsCom.WorkingDir & "\read.txt"
        'LFNo = FreeFile()
        'FileOpen(LFNo, FileName, OpenMode.Input)
        filereader = My.Computer.FileSystem.OpenTextFileReader(FileName)
        For i = 1 To n
            ReadLine = filereader.ReadLine()
            Dim modified As String = ReadLine.Insert(18, " ")
            elements = modified.Split(New Char() {" "c},
                                    StringSplitOptions.RemoveEmptyEntries)
            D(i) = elements(3)
        Next i
        'FileClose(LFNo)

    End Sub
End Module

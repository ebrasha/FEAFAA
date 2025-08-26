Partial Public Class clsCom

    'Common Fortran Library functions not in VB

    Public StartTime As Double

    Public lutty As Integer 'file "nike3d.txt"
    Public VersionNo As String = " 3.3.2.FAA.2.3", CompileDate As String = " 07/20/10"

    Public cpuio(3, 24), cpuip(3, 24) As Double 'time recording

    Public lki, lkj As Integer ' "adios", "timing"  YC?

    Public avlitr(2, 1600) As Double ' "adios"
    Public ncstep, numlit, maxlit, itrtot, irftot, icalls As Integer





    Private NL1 As String = Environment.NewLine



    Sub header(ByRef FileNo As Integer)

        Dim Header As String = "1Rigid Vehicle Pavement" & NL1 &
                           "                   nike3d (version " & VersionNo & ") compiled " & CompileDate
        PrintLine(lutty, Header)

    End Sub


    Sub exita(ByRef n As Integer)

        PrintLine(lutty, "  program stop" & LPad(5, Format(n, "0")) & NL1 & NL1 & NL1)

    End Sub


    Sub timing(ByRef cpuio(,) As Double, ByRef cpuip(,) As Double, ByRef k As Integer, ByRef l As Integer)

        Dim t(3), tp(3) As Double
        Dim i, j As Integer

        Call timuse(t(1), t(3), t(2))

        'GoTo (20,50,70,90) l
        If l = 1 Then GoTo 20
        If l = 2 Then GoTo 50
        If l = 3 Then GoTo 70
        If l = 4 Then GoTo 90


20:     For i = 1 To k
            For j = 1 To 3
                cpuio(j, i) = 0.0
                cpuip(j, i) = t(j)
                tp(j) = t(j)
            Next j
        Next i
        lki = k
        lkj = 0
        Exit Sub


50:     For j = 1 To 3
            cpuip(j, k) = t(j)
            tp(j) = t(j)
        Next j
        lki = k
        Exit Sub


70:     For j = 1 To 3
            cpuio(j, k) = cpuio(j, k) + t(j) - cpuip(j, k)
            tp(j) = t(j)
        Next j
        lkj = k
        Exit Sub

90:     For j = 1 To 3
            cpuio(j, k) = cpuio(j, k) + t(j) - tp(j)
            tp(j) = t(j)
        Next j
        lkj = k
        Exit Sub

    End Sub

    Sub timuse(ByRef t1 As Double, ByRef t2 As Double, ByRef t3 As Double)

        Dim hpfac As Double
        Dim ttm(8) As Double

        hpfac = 100

        t1 = ttm(1) / hpfac
        t2 = 0
        t3 = ttm(2) / hpfac

        'Call CPU_TIME(t3) 'YC?
        t3 = Microsoft.VisualBasic.Timer - StartTime

    End Sub


    Public Function LPad(ByRef N As Integer, ByRef SS As String) As String
        ' Adds leading spaces to variant string SS to make it N characters long.
        ' Used to format output to a file. #### characters in a Format function
        ' do not force spaces like QuickBasic.
        ' Typically, SS = Format(XX, "0.00")
        Dim ITemp As Short
        ITemp = CShort(Len(SS))
        If ITemp > N Then N = ITemp ' Length = Len if Len > N
        LPad = Space(N - ITemp) & SS
    End Function



    Sub ArrayExtract1Dfrom1D(ByRef A1D() As Double, ByRef InxDiff1 As Integer, ByRef Asub1D() As Double, ByRef n1 As Integer)

        Dim i1 As Integer

        For i1 = 1 To n1
            Asub1D(i1) = A1D(InxDiff1 + i1)
        Next i1

    End Sub

    Sub ArrayExtract1Dfrom1D(ByRef A1D() As Integer, ByRef InxDiff1 As Integer, ByRef Asub1D() As Integer, ByRef n1 As Integer)

        Dim i1 As Integer

        For i1 = 1 To n1
            Asub1D(i1) = A1D(InxDiff1 + i1)
        Next i1

    End Sub

    Sub ArrayExtract1Dfrom2D(ByRef A2D(,) As Integer, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer, ByRef Asub1D() As Integer, ByRef n1 As Integer)

        Dim i1 As Integer

        For i1 = 1 To n1
            Asub1D(i1) = A2D(InxDiff1 + i1, InxDiff2 + 1)
        Next i1

    End Sub

    Sub ArrayExtract1Dfrom2D(ByRef A2D(,) As Double, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer, ByRef Asub1D() As Double, ByRef n1 As Integer)

        Dim i1 As Integer

        For i1 = 1 To n1
            Asub1D(i1) = A2D(InxDiff1 + i1, InxDiff2 + 1)
        Next i1

    End Sub

    Sub ArrayExtract2Dfrom2D(ByRef A2D(,) As Integer, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer, ByRef Asub2D(,) As Integer, ByRef n1 As Integer, ByRef n2 As Integer)

        Dim i1, i2 As Integer

        For i1 = 1 To n1
            For i2 = 1 To n2
                Asub2D(i1, i2) = A2D(InxDiff1 + i1, InxDiff2 + i2)
            Next i2
        Next i1

    End Sub

    Sub ArrayExtract2Dfrom2D(ByRef A2D(,) As Double, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer, ByRef Asub2D(,) As Double, ByRef n1 As Integer, ByRef n2 As Integer)

        Dim i1, i2 As Integer

        For i1 = 1 To n1
            For i2 = 1 To n2
                Asub2D(i1, i2) = A2D(InxDiff1 + i1, InxDiff2 + i2)
            Next i2
        Next i1

    End Sub

    Sub ArrayExtract2Dfrom3D(ByRef A3D(,,) As Double, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer, ByRef InxDiff3 As Integer, ByRef Asub2D(,) As Double, ByRef n1 As Integer, ByRef n2 As Integer)

        Dim i1, i2 As Integer

        For i1 = 1 To n1
            For i2 = 1 To n2
                Asub2D(i1, i2) = A3D(InxDiff1 + i1, InxDiff2 + i2, InxDiff3)
            Next i2
        Next i1

    End Sub

    Sub ArrayExtract3Dfrom3D(ByRef A3D(,,) As Double, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer, ByRef InxDiff3 As Integer, ByRef Asub3D(,,) As Double, ByRef n1 As Integer, ByRef n2 As Integer, ByRef n3 As Integer)

        Dim i1, i2, i3 As Integer

        For i1 = 1 To n1
            For i2 = 1 To n2
                For i3 = 1 To n3
                    Asub3D(i1, i2, i3) = A3D(InxDiff1 + i1, InxDiff2 + i2, InxDiff3 + i3)
                Next
            Next i2
        Next i1

    End Sub



    Sub ArrayInsert1Dto1D(ByRef Asub1D() As Double, ByRef n1 As Integer, ByRef A1D() As Double, ByRef InxDiff1 As Integer)

        Dim i1 As Integer

        For i1 = 1 To n1
            A1D(InxDiff1 + i1) = Asub1D(i1)
        Next i1

    End Sub

    Sub ArrayInsert1Dto1D(ByRef Asub1D() As Integer, ByRef n1 As Integer, ByRef A1D() As Integer, ByRef InxDiff1 As Integer)

        Dim i1 As Integer

        For i1 = 1 To n1
            A1D(InxDiff1 + i1) = Asub1D(i1)
        Next i1

    End Sub

    Sub ArrayInsert1Dto2D(ByRef Asub1D() As Double, ByRef n1 As Integer, ByRef A2D(,) As Double, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer)

        Dim i1 As Integer

        For i1 = 1 To n1
            A2D(InxDiff1 + i1, InxDiff2 + 1) = Asub1D(i1)
        Next i1

    End Sub

    Sub ArrayInsert1Dto2D(ByRef Asub1D() As Integer, ByRef n1 As Integer, ByRef A2D(,) As Integer, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer)

        Dim i1 As Integer

        For i1 = 1 To n1
            A2D(InxDiff1 + i1, InxDiff2 + 1) = Asub1D(i1)
        Next i1

    End Sub

    Sub ArrayInsert2Dto2D(ByRef Asub2D(,) As Double, ByRef n1 As Integer, ByRef n2 As Integer, ByRef A2D(,) As Double, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer)

        Dim i1, i2 As Integer

        For i1 = 1 To n1
            For i2 = 1 To n2
                A2D(InxDiff1 + i1, InxDiff2 + i2) = Asub2D(i1, i2)
            Next i2
        Next i1

    End Sub
    Sub ArrayInsert2Dto2D(ByRef Asub2D(,) As Integer, ByRef n1 As Integer, ByRef n2 As Integer, ByRef A2D(,) As Integer, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer)

        Dim i1, i2 As Integer

        For i1 = 1 To n1
            For i2 = 1 To n2
                A2D(InxDiff1 + i1, InxDiff2 + i2) = Asub2D(i1, i2)
            Next i2
        Next i1

    End Sub

    Sub ArrayInsert2Dto3D(ByRef Asub2D(,) As Double, ByRef n1 As Integer, ByRef n2 As Integer, ByRef A3D(,,) As Double, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer, ByRef InxDiff3 As Integer)

        Dim i1, i2 As Integer

        For i1 = 1 To n1
            For i2 = 1 To n2
                A3D(InxDiff1 + i1, InxDiff2 + i2, InxDiff3) = Asub2D(i1, i2)
            Next i2
        Next i1

    End Sub

    Sub ArrayInsert3Dto3D(ByRef Asub3D(,,) As Double, ByRef n1 As Integer, ByRef n2 As Integer, ByRef n3 As Integer, ByRef A3D(,,) As Double, ByRef InxDiff1 As Integer, ByRef InxDiff2 As Integer, ByRef InxDiff3 As Integer)

        Dim i1, i2, i3 As Integer

        For i1 = 1 To n1
            For i2 = 1 To n2
                For i3 = 1 To n3
                    A3D(InxDiff1 + i1, InxDiff2 + i2, InxDiff3 + i3) = Asub3D(i1, i2, i3)
                Next
            Next i2
        Next i1

    End Sub



    Sub ArrayConvert1Dto2D(ByRef A1D() As Double, ByRef A2D(,) As Double, ByRef n1 As Integer, ByRef n2 As Integer)
        Dim i1, i2 As Integer

        ' stored by column, in compliance with FORTRAN
        For i1 = 1 To n1
            For i2 = 1 To n2
                A2D(i1, i2) = A1D((i2 - 1) * n1 + i1)
            Next i2
        Next i1

    End Sub

    Sub ArrayConvert1Dto2D(ByRef A1D() As Integer, ByRef A2D(,) As Integer, ByRef n1 As Integer, ByRef n2 As Integer)
        Dim i1, i2 As Integer

        ' stored by column, in compliance with FORTRAN
        For i1 = 1 To n1
            For i2 = 1 To n2
                A2D(i1, i2) = A1D((i2 - 1) * n1 + i1)
            Next i2
        Next i1

    End Sub

    Sub ArrayConvert1Dto3D(ByRef A1D() As Double, ByRef A3D(,,) As Double, ByVal n1 As Integer, ByVal n2 As Integer, ByVal n3 As Integer)
        Dim i1, i2, i3 As Integer

        ' stored by column, in compliance with FORTRAN
        For i1 = 1 To n1
            For i2 = 1 To n2
                For i3 = 1 To n3
                    A3D(i1, i2, i3) = A1D((i3 - 1) * n1 * n2 + (i2 - 1) * n1 + i1)
                Next i3
            Next i2
        Next i1

    End Sub

    Sub ArrayConvert2Dto1D(ByRef A2D(,) As Double, ByRef n1 As Integer, ByRef n2 As Integer, ByRef A1D() As Double)
        Dim i1, i2 As Integer

        ' stored by column, in compliance with FORTRAN
        For i1 = 1 To n1
            For i2 = 1 To n2
                A1D((i2 - 1) * n1 + i1) = A2D(i1, i2)
            Next i2
        Next i1

    End Sub

    Sub ArrayConvert2Dto1D(ByRef A2D(,) As Integer, ByRef n1 As Integer, ByRef n2 As Integer, ByRef A1D() As Integer)
        Dim i1, i2 As Integer

        ' stored by column, in compliance with FORTRAN
        For i1 = 1 To n1
            For i2 = 1 To n2
                A1D((i2 - 1) * n1 + i1) = A2D(i1, i2)
            Next i2
        Next i1

    End Sub

    Sub ArrayConvert3Dto1D(ByRef A3D(,,) As Double, ByVal n1 As Integer, ByVal n2 As Integer, ByVal n3 As Integer, ByRef A1D() As Double)
        Dim i1, i2, i3 As Integer

        ' stored by column, in compliance with FORTRAN
        For i1 = 1 To n1
            For i2 = 1 To n2
                For i3 = 1 To n3
                    A1D((i3 - 1) * n1 * n2 + (i2 - 1) * n1 + i1) = A3D(i1, i2, i3)
                Next i3
            Next i2
        Next i1

    End Sub

    Sub ArrayConvert2Dto3D(ByRef A2D(,) As Double, ByRef m1 As Integer, ByRef m2 As Integer, ByRef A3D(,,) As Double, ByVal n1 As Integer, ByVal n2 As Integer, ByVal n3 As Integer)

        Dim A1D(m1 * m2) As Double
        Call ArrayConvert2Dto1D(A2D, m1, m2, A1D)

        Call ArrayConvert1Dto3D(A1D, A3D, n1, n2, n3)

    End Sub

    Sub ArrayConvert3Dto2D(ByRef A3D(,,) As Double, ByVal n1 As Integer, ByVal n2 As Integer, ByVal n3 As Integer, ByRef A2D(,) As Double, ByRef m1 As Integer, ByRef m2 As Integer)

        Dim A1D(n1 * n2 * n3) As Double
        Call ArrayConvert3Dto1D(A3D, n1, n2, n3, A1D)

        Call ArrayConvert1Dto2D(A1D, A2D, m1, m2)

    End Sub

End Class


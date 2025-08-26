Partial Public Class clsPrintOut  
    Sub prde(ByRef mtypde() As Integer, ByRef ixde(,) As Integer, ByRef dehv(,) As Double, ByRef nmelde As Integer, ByRef x(,) As Double, ByVal nstep As Integer, ByVal timep As Double, ByVal path As String)


        'If (mpri <= 0) Then GoTo 1000

        If nstep = 0 Then GoTo 1000
        Dim s As String, ns As Integer
        If clsCom.Ntimestep = 1 Then
            For ns = 2 To 20
                s = path & "Output-Discrete Element-Step " & ns & ".txt"
                If System.IO.File.Exists(s) = True Then
                    System.IO.File.Delete(s)
                End If
            Next
        End If
        Dim FileNo As Integer, FileName As String
        FileNo = FreeFile()
        FileName = path & "Output-Discrete Element-Step " & clsCom.Ntimestep & ".txt"

        FileOpen(FileNo, FileName, OpenMode.Output)

        Dim nn As Integer
        Dim xl0 As Double
        Dim fmt2100, fmt2200 As String


        For nn = 1 To nmelde
            If (nprint > 0) Then GoTo 100

            nprint = 40
            Call header(FileNo)

            fmt2100 = " d i s c r e t e  e l e m e n t  d a t a  f o r  t i m e   s t e p " & LPad(5, Format(clsCom.Ntimestep, "0")) & " ( time = " & LPad(10, Format(timep, "0.0000E+00")) & " )" & NL1 &
                      "                matl               length" & NL1 &
                      "      element  number     original        current           force" & NL1

            PrintLine(FileNo, fmt2100)

100:        xl0 = Math.Sqrt((x(1, ixde(1, nn)) - x(1, ixde(2, nn))) ^ 2 +
                 (x(2, ixde(1, nn)) - x(2, ixde(2, nn))) ^ 2 +
                 (x(3, ixde(1, nn)) - x(3, ixde(2, nn))) ^ 2)


            fmt2200 = "      " & LPad(5, Format(nn, "0")) & "      " & LPad(2, Format(ixde(3, nn), "0")) & "     " & LPad(12, Format(xl0, "0.00000E+00")) & "    " & LPad(12, Format(dehv(11, nn), "0.00000E+00")) & "     " & LPad(12, Format(dehv(10, nn), "0.00000E+00"))
            PrintLine(FileNo, fmt2200)

            nprint = nprint - 1
200:    Next nn

        FileClose(FileNo)   ' YC 102418-012819
        Return

1000:   If (kpri <= 0) Then Return


        'FileClose(FileNo)  ' YC 102418-012819

    End Sub
End Class




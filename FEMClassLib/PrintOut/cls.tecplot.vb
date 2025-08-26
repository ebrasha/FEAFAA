Partial Public Class clsPrintOut  
    Sub tecplot(ByRef nnd As Integer, ByRef nne As Integer, ByRef cnl(,) As Double, ByRef tnl() As Double,
                ByRef nob() As Integer, ByRef moe(,) As Integer, ByRef ncv As Integer, ByRef np As Integer,
                ByRef mee() As Integer, ByRef ns As Integer, ByRef ns1() As Integer, ByRef ns2() As Integer,
                ByRef ms() As Integer, ByVal path As String)

        Dim FileNo As Integer, FileName As String
        FileNo = FreeFile()
        FileName = path & "model_load.dat"

        FileOpen(FileNo, FileName, OpenMode.Output)

        Dim header As String = "TITLE=""Model""" & NL1 &
                               "VARIABLES=""X"",""Y"",""Z"",""Constrain X"",""Constrain Y"",""Constrain Z"", ""Spring X"",""Spring Y"",""Spring Z"""

        Dim i, j, k As Integer
        For i = 1 To ncv
            header = header & """Load_ " & i & " X""" & """Load_ " & i & " Y""" & """Load_ " & i & " Z"""

        Next

        header = header & """Temperature"""

        PrintLine(FileNo, header)

        Dim ncs(3), nsp(3) As Integer

        Dim ime As Integer = 0
        Dim ip As Integer
        Dim fmt As String

        For ip = 1 To np

            If (ip > 1) Then
                ime = ime + mee(ip - 1)
            End If

            PrintLine(FileNo, "ZONE T=""nn." & ip & """")
            PrintLine(FileNo, "N=" & LPad(5, nnd) & ",E=" & LPad(8, mee(ip)) & ",F=FEPOINT,ET=BRICK")


            For i = 1 To nnd

                'ncs(1:3)=0
                'nsp(1:3)=0
                For j = 1 To 3
                    ncs(j) = 0
                    nsp(j) = 0
                Next j

                For j = 1 To 3
                    If (nob(i) = j) Then ncs(j) = 1
                Next j

                For j = 4 To 5
                    If (nob(i) = j) Then
                        ncs(j - 3) = 1
                        ncs(j - 2) = 1
                    End If
                Next j

                If (nob(i) = 6) Then
                    ncs(1) = 1
                    ncs(3) = 1
                End If

                If (nob(i) = 7) Then
                    'ncs(1:3)=1
                    For j = 1 To 3
                        ncs(j) = 1
                    Next j
                End If

                For k = 1 To ns
                    If (ns1(k) = i Or ns2(k) = i) Then
                        For j = 1 To 3
                            If (ms(k) = j) Then nsp(j) = 1
                        Next j
                        For j = 4 To 5
                            If (ms(k) = j) Then
                                nsp(j - 3) = 1
                                nsp(j - 2) = 1
                            End If
                        Next j
                        If (ms(k) = 6) Then
                            nsp(1) = 1
                            nsp(3) = 1
                        End If
                        If (ms(k) = 7) Then
                            'nsp(1:3)=1
                            For j = 1 To 3
                                nsp(j) = 1
                            Next j
                        End If
                    End If
                Next k

                fmt = ""
                For j = 1 To 3
                    fmt = fmt & LPad(20, Format(x(j, i), "0.0000000000000E+00")) & " "
                Next j

                For j = 1 To 3
                    fmt = fmt & Format(ncs(j), "0") & " "
                Next j

                For j = 1 To 3
                    fmt = fmt & Format(nsp(j), "0") & " "
                Next j

                For j = 1 To 3 * ncv
                    fmt = fmt & LPad(20, Format(cnl(i, j), "0.0000000000000E+00")) & " "
                Next j

                fmt = fmt & LPad(20, Format(tnl(i), "0.0000000000000E+00"))

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

    End Sub
End Class




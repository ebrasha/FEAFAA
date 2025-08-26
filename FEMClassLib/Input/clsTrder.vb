
Partial Public Class clsRead

    Sub trder(ByVal readTextAll() As String, ByRef Line As Integer)
        Dim ReadLine, elements() As String
        Dim n, nl, iLine As Integer
        ReDim clsCom.tmode(clsCom.numnp), clsCom.tbase(clsCom.numnp)

        Dim anl, dtm, dtb As Double, j, i1 As Integer

15:     Dim i = 0, ie = 1


        For iLine = 1 To clsCom.numnp
            Line = Line + 1
            ReadLine = readTextAll(Line)
            Dim modified As String = ReadLine.Insert(18, " ")
            elements = modified.Split(New Char() {" "c},
                                    StringSplitOptions.RemoveEmptyEntries)
            n = elements(0) : clsCom.tmode(n) = elements(1)
            clsCom.tbase(n) = elements(2)

40:         If i = 0 Then
                GoTo 110
            Else
                GoTo 50
            End If

50:         nl = n - i
            If nl - 1 < 0 Then
                GoTo 110
            ElseIf nl - 1 = 0 Then
                GoTo 100
            ElseIf nl - 1 > 0 Then
                GoTo 60
            End If

60:         nl = nl / ie
            If (i + nl * ie - n) = 0 Then
                GoTo 70
            Else
                GoTo 170
            End If

70:         If nl - 1 <= 0 Then
                GoTo 100
            ElseIf nl - 1 > 0 Then
                GoTo 80
            End If

80:         anl = nl
            dtm = (clsCom.tmode(n) - clsCom.tmode(i)) / anl
            dtb = (clsCom.tbase(n) - clsCom.tbase(i)) / anl
            nl = n - 2 * ie

            For j = i To nl Step ie
                i1 = j + ie
                clsCom.tmode(i1) = clsCom.tmode(j) + dtm
                clsCom.tbase(i1) = clsCom.tbase(j) + dtb
            Next j

100:        If (clsCom.numnp - n) < 0 Then
                GoTo 170
            ElseIf (clsCom.numnp - n) = 0 Then
                GoTo 120
            Else
                GoTo 110
            End If

110:        i = n

        Next iLine

120:    Return

170:    PrintLine("fatal error on temperature profile cards")

    End Sub

End Class

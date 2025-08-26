
Partial Public Class clsRead

    Sub cnlds(ByVal readTextAll() As String, ByRef Line As Integer)
        Dim ReadLine, elements() As String
        Dim i As Integer
        ReDim clsCom.nod(clsCom.nload), clsCom.idirn(clsCom.nload), clsCom.ncur(clsCom.nload), clsCom.fac(clsCom.nload)
        For i = 1 To clsCom.nload
            Line = Line + 1
            ReadLine = readTextAll(Line)
            Dim modified As String = ReadLine.Insert(18, " ")
            elements = modified.Split(New Char() {" "c},
                                    StringSplitOptions.RemoveEmptyEntries)
            clsCom.nod(i) = elements(0) : clsCom.idirn(i) = elements(1)
            clsCom.ncur(i) = elements(2) : clsCom.fac(i) = elements(3)
            If clsCom.fac(i) = 0 Then clsCom.fac(i) = 1.0
        Next i

    End Sub

End Class


Partial Public Class clsRead

    Sub ldcvs(ByVal readTextAll() As String, ByRef Line As Integer)
        Dim ReadLine, elements() As String
        Dim i, j, kk, ll, npts As Integer
        Dim rv(clsCom.nptm), timv(clsCom.nptm) As Double
        ReDim clsCom.pld(2 * clsCom.nlcur * clsCom.nptm), clsCom.npc(clsCom.nlcur + 1)
        clsCom.npc(1) = 1
        For j = 1 To clsCom.nlcur
            ReadLine = readTextAll(Line)
            elements = ReadLine.Split(New Char() {" "c},
                                            StringSplitOptions.RemoveEmptyEntries)
            ll = elements(0) : npts = elements(1)
            For i = 1 To npts
                Line = Line + 1
                ReadLine = readTextAll(Line)
                Dim modified As String = ReadLine.Insert(10, " ")
                elements = modified.Split(New Char() {" "c},
                                        StringSplitOptions.RemoveEmptyEntries)
                timv(i) = elements(0) : rv(i) = elements(1)
            Next i
            clsCom.npc(j + 1) = clsCom.npc(j) + 2 * npts
            i = clsCom.npc(j)
            For kk = 1 To npts
                clsCom.pld(i) = timv(kk)
                i = i + 1
                clsCom.pld(i) = rv(kk)
                i = i + 1
            Next kk
            Line = Line + 1
        Next j
        clsCom.nptst = clsCom.npc(clsCom.nlcur + 1)
    End Sub

End Class

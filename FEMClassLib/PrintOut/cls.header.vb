Partial Public Class clsPrintOut
    Sub header(ByRef FileNo As Integer)
        Dim VersionNo As String = "1.0 "
        Dim CompileDate As String = "04/15/19"
        Dim Header As String = NL1 & " Rigid Vehicle Pavement" & NL1 &
                           "                   FAASR3D (version " & VersionNo & ") compiled " & CompileDate
        PrintLine(FileNo, Header)

    End Sub
End Class




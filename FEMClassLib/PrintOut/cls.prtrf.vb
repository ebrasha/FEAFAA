Partial Public Class clsPrintOut  
    Sub prtrf(ByVal path As String, ByRef idp(,) As Integer, ByRef nodes() As Integer, ByRef idir() As Integer, ByRef rf() As Integer, ByRef numudc As Integer, ByRef timep As Double)

        If (numudc = 0) Then Exit Sub


        Dim ad() As String = {"", "Fx", "Fy", "Fz", "Mx", "My", "Mz"}

        Dim FileNo As Integer, FileName As String
        FileNo = FreeFile()
        FileName = path & "Output-Nodal Reaction Force-Step " & clsCom.Ntimestep & ".txt"

        FileOpen(FileNo, FileName, OpenMode.Output)

        If (iterp2 > 0 And numudc <> 0) Then
            Dim fmt305 As String
            fmt305 = " ===== TOTAL INTERFACE FORCE DATA =====" & NL1 & NL1 &
                " ************************************************************" & NL1 &
                " Nodal reaction forces not calculated for time " & LPad(10, Format(timep, ".00000E+00")) & NL1 &
                " since no equilibrium iterations were made at this time.  *" & NL1 &
                " ************************************************************" & NL1

            PrintLine(FileNo, fmt305)

            Exit Sub
        End If

        Dim fmt100, fmt200, fmt300 As String
        fmt100 = " ===== NODAL REACTION FORCE DATA =====" & NL1 &
               "     time =" & LPad(12, Format(timep, "0.00000E+00")) & NL1 &
               "      node   direction    value "

        PrintLine(FileNo, fmt100)

        Dim n, i, nodesn, idirn As Integer
        Dim id(6) As Integer
        For n = 1 To numudc
            nodesn = nodes(n)
            idirn = idir(n)

            'Call unpkid(id, idp(1, nodesn), 1)
            Call unpkid(id, idp, nodesn, 1)
            'For i = 1 To 6
            '    id(i) = idp(i, nodesn)
            'Next i


            For i = 1 To 6
                If (idirn = id(i)) Then GoTo 20
            Next i


            fmt300 = " ************************************************************" & NL1 &
                     " *                   - FATAL ERROR -                        *" & NL1 &
                     " * Unable to identify nodal reaction force, node #" & LPad(7, Format(nodesn, "0")) & "   *" & NL1 &
                     " ************************************************************" & NL1 & NL1 & NL1
            PrintLine(FileNo, fmt300)
            Call adios(2)

20:         fmt200 = "   " & LPad(7, Format(nodesn, "0")) & "      " & LPad(2, Format(ad(i), "0")) & "     " & LPad(11, Format(rf(n), " 0.00000E+00"))
            PrintLine(FileNo, fmt200)
        Next n

        FileClose(FileNo)

    End Sub
End Class




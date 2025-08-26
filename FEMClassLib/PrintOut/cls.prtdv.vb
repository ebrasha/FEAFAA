Partial Public Class clsPrintOut  
    Sub prtdv(ByVal u() As Double, ByRef idp(,) As Integer, ByRef numnp As Integer, ByRef itemp As Integer, ByRef tnew() As Double,
              ByVal npb As Integer, ByVal ipnode(,) As Integer, ByVal nstep As Integer, ByVal timep As Double, ByVal path As String)

        If nstep = 0 Then GoTo 1001


        clsCom.Ntimestep = nstep    ' YC 121219-2


        Dim s As String, ns As Integer
        If clsCom.Ntimestep = 1 Then
            For ns = 2 To 20
                s = path & "Output-Nodal Displacement-Step " & ns & ".txt"
                If System.IO.File.Exists(s) = True Then
                    System.IO.File.Delete(s)
                End If
            Next
        End If
        Dim FileNo As Integer, FileName As String
        FileNo = FreeFile()
        FileName = path & "Output-Nodal Displacement-Step " & clsCom.Ntimestep & ".txt"
        FileOpen(FileNo, FileName, OpenMode.Output)

        Dim nq, ntimes, node1, node2 As Integer
        Dim l, ic, j, k, i, kk As Integer
        Dim d(9) As Double
        Dim id(6) As Integer

        nq = 3
        ntimes = 1

        For l = 1 To ntimes
            ic = 0
            For j = 1 To npb
                node1 = ipnode(1, j)
                If (node1 = 0) Then GoTo 150
                node2 = ipnode(2, j)
                For k = node1 To node2
                    For i = 1 To 6
                        d(i) = 0.0
110:                Next i

                    'Call unpkid(id, idp(1, k), 1)
                    Call unpkid(id, idp, k, 1)
                    'For i = 1 To 6
                    '    id(i) = idp(i, k)
                    'Next i


                    For i = 1 To 3
                        kk = id(i)
                        If (kk = 0) Then GoTo 120
                        d(i) = d(i) + u(kk)
120:                Next i

                    If (nq <> 6) Then GoTo 122

                    For i = 4 To 6
                        kk = id(i)
                        If (kk = 0) Then GoTo 121
                        d(i) = d(i) + u(kk)
121:                Next i

122:                If (ic > 0) Then GoTo 125
                    ic = 50
                    Call header(FileNo)

                    If (l = 1) Then
                        Dim fmt190 As String
                        fmt190 = " n o d a l   p r i n t   o u t   f o r   t i m e   s t e p " & LPad(5, Format(clsCom.Ntimestep, "0")) & "  ( at time " & LPad(11, Format(timep, "0.0000E+00")) & " )"
                        PrintLine(FileNo, fmt190)
                    End If

                    If (nq = 3) Then
                        If (l = 1 And itemp = 0) Then PrintLine(FileNo, "   node     x-disp       y-disp       z-disp")
                        If (l = 1 And itemp <> 0) Then PrintLine(FileNo, "   node     x-disp       y-disp       z-disp       temp")
                        If (l = 2) Then PrintLine(FileNo, "   node     x-vel        y-vel        z-vel ")
                        If (l = 3) Then PrintLine(FileNo, "   node     x-accl       y-accl       z-accl")
                    Else
                        If (l = 1 And itemp = 0) Then PrintLine(FileNo, "   node     x-disp       y-disp       z-disp       x-rot        y-rot        z-rot")
                        If (l = 1 And itemp <> 0) Then PrintLine(FileNo, "   node     x-disp       y-disp       z-disp       x-rot        y-rot        z-rot        temp")
                        If (l = 2) Then PrintLine(FileNo, "   node     x-vel        y-vel        z-vel      x-rot vel    y-rot vel    z-rot vel")
                        If (l = 3) Then PrintLine(FileNo, "'   node     x-accl       y-accl       z-accl     x-rot acc    y-rot acc    z-rot acc")
                    End If

125:                ic = ic - 1

                    Dim fmt280 As String
                    fmt280 = LPad(6, Format(k, "0")) & "  "


                    For i = 1 To nq
                        fmt280 = fmt280 & LPad(12, Format(d(i), "0.00000E+00")) & " "
                        If IsFAASR3DPlot Then snl(k, i, nstep) = d(i) 'YC for tecplot format file,YC 102418-012819
                    Next i

                    If (itemp <> 0 And l = 1) Then
                        fmt280 = fmt280 & LPad(12, Format(tnew(k), "0.00000E+00"))
                        If IsFAASR3DPlot Then snl(k, 4, nstep) = tnew(k) 'YC for tecplot format file,YC 102418-012819
                    End If



                    PrintLine(FileNo, fmt280)

130:            Next k
150:        Next j
        Next l

        FileClose(FileNo)
1001:
    End Sub
End Class




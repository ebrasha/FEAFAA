Partial Public Class clsFEM
    Private Sub WriteNike(ByVal IPC As InputCards, ByVal WorkingDir As String)

        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = WorkingDir & "\nike.txt"
        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Output, , , 1024)
        Dim i, j, k As Integer
        Dim s As String

        For i = 1 To IPC.nmmat
            s = Format(IPC.matype(i), "0") & "  " & Format(IPC.den(i), "0.000E+00")
            PrintLine(LFNo, s)
            For j = 1 To 6
                s = " "
                For k = 1 To 8
                    s = s & " " & Format(IPC.prop(6 * (k - 1) + j, i), "0.000E+00")
                Next k
                PrintLine(LFNo, s)
            Next j
        Next i

        For i = 1 To IPC.numnp
            s = Format(i, "0")
            For j = 1 To 6
                s = s & " " & Format(IPC.idp(j, i), "0")
            Next j
            For j = 1 To 3
                s = s & " " & Format(IPC.x(j, i), "0.000E+00")
            Next j
            PrintLine(LFNo, s)
        Next i

        For i = 0 To IPC.numelh - 1
            s = Format(i + 1, "0")
            s = s & " " & Format(IPC.ia(9 * i), "0")
            For j = 1 To 8
                s = s & " " & Format(IPC.ia(9 * i + j), "0")
            Next j
            PrintLine(LFNo, s)
        Next i

        s = Format(IPC.nmmtde, "0") & " " & Format(IPC.nmelde, "0") & " " & Format(IPC.nmmass, "0")
        PrintLine(LFNo, s)
        For i = 1 To IPC.nmmtde
            s = Format(i, "0") & " " & Format(IPC.mtypde(i), "0")
            PrintLine(LFNo, s)
            s = Format(IPC.cmde(1, i), "0.000E+00") & " " & Format(IPC.cmde(2, i), "0.000E+00")
            PrintLine(LFNo, s)
        Next i
        For i = 1 To IPC.nmelde
            s = Format(i, "0") & " " & Format(IPC.ixde(1, i), "0") & " " & Format(IPC.ixde(2, i), "0") & " " & Format(IPC.ixde(3, i), "0") & " " & Format(IPC.sclf(i), "0")
            PrintLine(LFNo, s)
        Next i

        For i = 1 To IPC.numsv
            s = Format(IPC.iparm(1, i), "0") & " " & Format(IPC.iparm(2, i), "0") & " " & Format(IPC.iparm(5, i), "0") & " " & Format(IPC.stfsf(i), "0.000E+00")
            s = s & " " & Format(IPC.fric(1, i), "0.000E+00") & " " & Format(IPC.fric(3, i), "0.000E+00") & " " & Format(IPC.fric(3, i), "0.000E+00")
            s = s & " " & Format(IPC.pend(i), "0.000E+00") & " " & Format(IPC.ngap(i), "0")
            If IPC.fric(1, i) = 0.005 Then
                s = s & " " & Format(1, "0")
                PrintLine(LFNo, s)
                s = Format(IPC.iaug(i), "0.000E+00") & " " & Format(IPC.altol(1, i), "0.000E+00") & " " & Format(IPC.altol(2, i), "0.000E+00")
                s = s & " " & Format(IPC.sfact(i), "0.000E+00") & " " & Format(IPC.tdeath(i), "0.000E+00") & " " & Format(IPC.tbury(i), "0.000E+00") & " " & Format(IPC.ifd(i), "0.000E+00")
                PrintLine(LFNo, s)
            Else
                s = s & " " & Format(0, "0")
                PrintLine(LFNo, s)
            End If
        Next i

        For i = 1 To IPC.numsv
            For j = 1 To IPC.iparm(1, i)
                s = Format(j, "0")
                For k = 1 To 4
                    s = s & " " & Format(IPC.irects(k, j), "0")
                Next k
                PrintLine(LFNo, s)
            Next j
            For j = 1 To IPC.iparm(2, i)
                s = Format(j, "0")
                For k = 1 To 4
                    s = s & " " & Format(IPC.irectm(k, j), "0")
                Next k
                PrintLine(LFNo, s)
            Next j
        Next i

        For i = 1 To IPC.nlcur
            s = Format(i, "0") & " " & Format(IPC.nptm, "0")
            PrintLine(LFNo, s)
            For j = 1 To IPC.nptm
                s = Format(IPC.pld(IPC.npc(i) + (IPC.nptm - 1) * (j - 1)), "0.000E+00")
                s = s & " " & Format(IPC.pld(IPC.npc(i) + (IPC.nptm - 1) * (j - 1) + 1), "0.000E+00")
                PrintLine(LFNo, s)
            Next j
            's = Format(IPC.pld(IPC.npc(i)), "0.000E+00")
            'For j = 1 To IPC.nptm - 1
            's = s & " " & Format(IPC.pld(IPC.npc(i) + j), "0.000E+00")
            'PrintLine(LFNo, s)
            'Next j
            's = Format(IPC.pld(IPC.npc(i) + IPC.nptm), "0.000E+00")
            'For j = 1 To IPC.nptm - 1
            's = s & " " & Format(IPC.pld(IPC.npc(i) + IPC.nptm + j), "0.000E+00")
            'PrintLine(LFNo, s)
            'Next j
        Next i

        For i = 1 To IPC.nload
            s = Format(IPC.nod(i), "0") & " " & Format(IPC.idirn(i), "0") & " " & Format(IPC.ncur(i), "0") & " " & Format(IPC.fac(i), "0.000E+00")
            PrintLine(LFNo, s)
        Next i

        If IPC.itemp = 1 Then
            For i = 1 To IPC.numnp
                s = Format(i, "0") & " " & Format(IPC.tmode(i), "0.000E+00") & " " & Format(IPC.tbase(i), "0.000E+00")
                PrintLine(LFNo, s)
            Next i
        End If

        FileClose(LFNo)
        '
        'Copy information from ------- Sub Material_No_Inf()
        'IFOption = "sv"
        'IFOption1 = "sl" 'sliding only - interface type 'LIA
        'IFOption2 = "tied" 'tied sliding - interface type 'LIA
        'IFOption3 = "sv" 'sliding with voids - interface type 'LIA

        'If Factor2 > 0.001! Then
        'Friction = 0.005
        'Else
        'Friction = 0.0#
        'End If
    End Sub
End Class

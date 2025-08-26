Partial Public Class clsPrintOut  
    Sub oocbsh(ByRef nelu As Integer, ByRef neln As Integer, ByRef ioff As Integer, ByRef nelbkf As Integer, ByRef ireor() As Integer,
               ByVal incflg As Integer, ByVal nh04 As Integer, ByVal nh05 As Integer, ByVal nblk As Integer, ByVal ihv() As Integer,
               ByVal numelg() As Integer, ByVal nelpg As Integer)

        Dim nelbk, lblock, nbl, nb As Integer
        'Dim iadd As Integer ' YC?

        numblk = nbtot 'see common/oocbsb  YC 102418=012819

        ioff = 0
        nelbk = nelbkf
        neln = ireor(nelu)

        lblock = 0
        nbl = 0

        For nb = 1 To numblk

            nbl = nbl + lblock
            ' Call gethexg(nb, numelg, ihv, nelpg, nblk, lblock)
            Call objInit.gethexg(nb, numelg, ihv, nelpg, nblk, lblock, incflg, nh04, nh05)
            nelbk = nelpg
            ioff = nblk + nelpg

            If (neln <= ioff) Then GoTo 200
        Next nb

        Dim fmt1000 As String
        fmt1000 = " ************************************************************" & NL1 &
                  " *                   - FATAL ERROR -                        *" & NL1 &
                  " *             EBE print/plot redroer error                 *" & NL1 &
                  " ************************************************************" & NL1 & NL1 & NL1
        PrintLine(lutty, fmt1000)

        Call adios(2)

200:    ioff = nelbk - ioff + neln
        ' If (nb <> nbc) Then
        'iadd = iadd0 + nbl
        'Call blkcpy(anegb2, anegb1, length)
        'nbc = nb
        'End If
    End Sub
End Class




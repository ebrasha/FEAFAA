Partial Public Class clsFEM
    Private Sub TransData(ByVal IPC As InputCards, ByRef StopFEDFAA As Short, ByVal FEDFAAStopped As Short, ByVal IDtyp As Short, ByVal I1 As Short,
                       ByVal WorkingDir As String, ByVal Filenameonly As String)

        With IPC
            ' Public ControlCards,  MaterialCards, ElementCards
            clsCom.nmmat = .nmmat : clsCom.numnp = .numnp : clsCom.numelh = .numelh : clsCom.numsv = .numsv
            clsCom.inpsd = .inpsd : clsCom.ntime = .ntime : clsCom.nlcur = .nlcur : clsCom.nptm = .nptm
            clsCom.nload = .nload : clsCom.numpc = .numpc : clsCom.numdc = .numdc : clsCom.nrcc = .nrcc
            clsCom.matype = .matype : clsCom.dt = 1.0 : clsCom.den = .den : clsCom.prop = .prop
            clsCom.idp = .idp : clsCom.x = .x : clsCom.nob = .nob

            ' Public SpringCards
            clsCom.nmmtde = .nmmtde : clsCom.nmelde = .nmelde : clsCom.nmmass = .nmmass
            clsCom.mtypde = .mtypde : clsCom.ixde = .ixde
            clsCom.cmde = .cmde : clsCom.sclf = .sclf

            ' Public InterfaceCards
            clsCom.nrttlm = .nrttlm : clsCom.nrttls = .nrttls
            clsCom.iparm = .iparm : clsCom.iaug = .iaug : clsCom.ifd = .ifd : clsCom.ngap = .ngap : clsCom.irects = .irects : clsCom.irectm = .irectm
            clsCom.fric = .fric : clsCom.pend = .pend : clsCom.altol = .altol : clsCom.sfact = .sfact : clsCom.tdeath = .tdeath : clsCom.tbury = .tbury : clsCom.stfsf = .stfsf
            'clsCom.nsmmax = .nsmmax

            ' Public LoadCards
            clsCom.nod = .nod : clsCom.idirn = .idirn : clsCom.ncur = .ncur : clsCom.npc = .npc : clsCom.nptst = .nptst : clsCom.itemp = .itemp : clsCom.itread = .itread
            clsCom.fac = .fac : clsCom.pld = .pld : clsCom.tmode = .tmode : clsCom.tbase = .tbase
        End With
        ' Public PathSet
        clsCom.StopFEDFAA = StopFEDFAA : clsCom.FEDFAAStopped = FEDFAAStopped : clsCom.IDTyp = IDtyp : clsCom.I1 = I1
        clsCom.WorkingDir = WorkingDir : clsCom.FilenameOnly = Filenameonly


        Dim I, J As Integer
        ReDim clsCom.ixh(9, clsCom.numelh), clsCom.ixe(9, clsCom.numelh)
        For I = 1 To clsCom.numelh
            clsCom.ixh(1, I) = IPC.ia(9 * (I - 1))
            clsCom.ixe(1, I) = IPC.ia(9 * (I - 1))
            For J = 2 To 9
                clsCom.ixh(J, I) = IPC.ia(9 * (I - 1) + J - 1)
                clsCom.ixe(J, I) = IPC.ia(9 * (I - 1) + J - 1)
            Next J
        Next I

    End Sub
End Class

Partial Public Class clsCom

    Public Sub comvari()
        ' Public ControlCards,  MaterialCards, ElementCards
        nmmat = clsCom.nmmat : numnp = clsCom.numnp : numelh = clsCom.numelh : numsv = clsCom.numsv
        inpsd = clsCom.numelh : ntime = clsCom.ntime : nlcur = clsCom.nlcur : nptm = clsCom.nptm
        nload = clsCom.nload : numpc = clsCom.numpc : numdc = clsCom.numdc : nrcc = clsCom.nrcc
        matype = clsCom.matype : idp = clsCom.idp : ixh = clsCom.ixh : dt = clsCom.dt
        den = clsCom.den : prop = clsCom.prop : x = clsCom.x : nob = clsCom.nob

        ' Public SpringCards
        nmmtde = clsCom.nmmtde : nmelde = clsCom.nmelde : nmmass = clsCom.nmmass
        mtypde = clsCom.mtypde : ixde = clsCom.ixde
        cmde = clsCom.cmde : sclf = clsCom.sclf

        ' Public InterfaceCards
        nrttlm = clsCom.nrttlm : nrttls = clsCom.nrttls : iacc = clsCom.iacc : ifl = clsCom.ifl : nifd = clsCom.nifd : nsntl = clsCom.nsntl : nmntl = clsCom.nmntl
        iparm = clsCom.iparm : iaug = clsCom.iaug : ifd = clsCom.ifd : ngap = clsCom.ngap : irects = clsCom.irects : irectm = clsCom.irectm : nsf = clsCom.nsf
        fric = clsCom.fric : pend = clsCom.pend : altol = clsCom.altol : sfact = clsCom.sfact : tdeath = clsCom.tdeath : tbury = clsCom.tbury : stfsf = clsCom.stfsf
        nsmmax = clsCom.nsmmax

        ' Public LoadCards
        nod = clsCom.nod : idirn = clsCom.idirn : ncur = clsCom.ncur : npc = clsCom.npc : nptst = clsCom.nptst : itemp = clsCom.itemp : itread = clsCom.itread
        fac = clsCom.fac : pld = clsCom.pld : tmode = clsCom.tmode : tbase = clsCom.tbase

        ' Public PathSet
        StopFEDFAA = clsCom.StopFEDFAA : FEDFAAStopped = clsCom.FEDFAAStopped : IDTyp = clsCom.IDTyp : I1 = clsCom.I1
        WorkingDir = clsCom.WorkingDir : FilenameOnly = clsCom.FilenameOnly

    End Sub

End Class

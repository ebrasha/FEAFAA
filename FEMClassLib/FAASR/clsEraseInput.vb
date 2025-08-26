Partial Public Class clsFEM

    Sub EraseInput(ByVal lutty)
        FileClose(lutty)
        Erase clsCom.idp, clsCom.ixh, clsCom.mtypde, clsCom.ixde, clsCom.iparm, clsCom.iaug, clsCom.ifd, clsCom.ngap, clsCom.irects, clsCom.irectm, clsCom.nod, clsCom.idirn, clsCom.ncur, clsCom.npc
        Erase clsCom.matype, clsCom.den, clsCom.prop, clsCom.x, clsCom.cmde, clsCom.sclf, clsCom.fric, clsCom.pend, clsCom.altol, clsCom.sfact, clsCom.tdeath, clsCom.tbury, clsCom.stfsf, clsCom.fac, clsCom.pld, clsCom.tmode, clsCom.tbase

        clsCom.nmmat = Nothing : clsCom.numnp = Nothing : clsCom.numelh = Nothing : clsCom.numsv = Nothing : clsCom.inpsd = Nothing : clsCom.ntime = Nothing
        clsCom.nmmtde = Nothing : clsCom.nmelde = Nothing : clsCom.nmmass = Nothing
        clsCom.nrttlm = Nothing : clsCom.nrttls = Nothing : clsCom.numsv = Nothing
        clsCom.nptst = Nothing : clsCom.itemp = Nothing : clsCom.itread = Nothing
        clsCom.StopFEDFAA = Nothing : clsCom.FEDFAAStopped = Nothing : clsCom.I1 = Nothing : clsCom.IDTyp = Nothing

        GC.Collect()

    End Sub

End Class

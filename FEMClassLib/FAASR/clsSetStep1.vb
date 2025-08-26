Partial Public Class clsFEM

    Sub Setstep1(ByRef nod0() As Integer, ByRef idirn0() As Integer, ByRef ncur0() As Integer, ByRef fac0() As Double)
        Dim nlcur, nptm As Integer
        nlcur = 1 : nptm = 2
        ' clsLdcvs
        Dim npc(nlcur + 1), nptst As Integer
        Dim pld(2 * nlcur * nptm) As Double
        npc(1) = 1 : npc(2) = 5 : nptst = 5
        pld(1) = 0 : pld(2) = 0 : pld(3) = 1 : pld(4) = 1
        ' clsCnlds
        nod0 = clsCom.nod : idirn0 = clsCom.idirn : ncur0 = clsCom.ncur : fac0 = clsCom.fac
        clsCom.nlcur = nlcur : clsCom.nptm = nptm
        clsCom.npc = npc : clsCom.pld = pld
        clsCom.nptst = nptst
    End Sub

End Class

Partial Public Class clsCom

    ' Public ControlCards
    Public Shared nmmat, numnp, numelh, numsv, inpsd, ntime, Ntimestep As Integer
    Public Shared nlcur, nptm, nload, numpc, numdc, nrcc As Integer
    Public Shared dt As Double

    ' Public MaterialCards
    Public Shared matype(), incomp() As Integer
    Public Shared den(), prop(,), rdc(,) As Double

    ' Public ElementCards
    Public Shared idp(,) As Integer
    Public Shared nob() As Integer
    Public Shared x(,) As Double
    Public Shared ixh(,) As Integer
    Public Shared ixe(,) As Integer

    ' Public SpringCards
    Public Shared nmmtde, nmelde, nmmass As Integer
    Public Shared mtypde(), ixde(,) As Integer
    Public Shared cmde(,), sclf() As Double

    ' Public InterfaceCards
    Public Shared nrttlm, nrttls, iacc, ifl, nifd, nsntl, nmntl, nsmmax As Integer
    Public Shared iparm(,), iaug(), ifd(), ngap(), irects(,), irectm(,), nsf(,) As Integer
    Public Shared fric(,), pend(), altol(,), sfact(), tdeath(), tbury(), stfsf() As Double

    ' Public LoadCards
    Public Shared nod(), idirn(), ncur(), npc(), nptst, itemp, itread As Integer
    Public Shared fac(), pld(), tmode(), tbase() As Double

    ' Public Responses
    Public Shared stress1(), stress8() As Double

    ' Public PathSet
    Public Shared StopFEDFAA, FEDFAAStopped As Short
    Public Shared IDTyp, I1 As Short
    Public Shared WorkingDir, FilenameOnly As String

    ' Default settings

End Class

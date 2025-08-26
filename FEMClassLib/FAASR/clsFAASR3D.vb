Option Strict On
Option Explicit On
Imports System.Text.RegularExpressions
Partial Public Class clsFEM
    Public com As New clsCom
    Public nik As New clsInput
    Public ini As New clsInitial
    Public objSolve As New clsSolve
    Public Structure InputCards
        Dim nmmat, numnp, numelh, numsv, inpsd, ntime As Integer
        Dim nlcur, nptm, nload, numpc, numdc, nrcc As Integer
        Dim matype() As Integer
        Dim den(), prop(,) As Double
        Dim idp(,), nob() As Integer
        Dim x(,) As Double
        Dim ia() As Integer
        Dim nmmtde, nmelde, nmmass As Integer
        Dim mtypde(), ixde(,) As Integer
        Dim cmde(,), sclf() As Double
        Dim nrttlm, nrttls As Integer
        Dim iparm(,), iaug(), ifd(), ngap(), irects(,), irectm(,) As Integer
        Dim fric(,), pend(), altol(,), sfact(), tdeath(), tbury(), stfsf() As Double
        Dim nod(), idirn(), ncur(), npc(), nptst, itemp, itread As Integer
        Dim fac(), pld(), tmode(), cnwmk(), tbase() As Double
        Dim iacc As Integer
    End Structure

    Public IPC As InputCards

    Public Sub FAASR3D(ByVal IPC As InputCards, ByRef Stress1() As Double, ByRef Stress8() As Double,
                       ByRef StopFEDFAA As Short, ByVal FEDFAAStopped As Short, ByVal IDtyp As Short, ByVal I1 As Short,
                       ByVal WorkingDir As String, ByVal Filenameonly As String, ByVal ModelOut As Integer)
        Dim newtext As String
        'Call WriteNike(IPC, WorkingDir)
        Call TransData(IPC, StopFEDFAA, FEDFAAStopped, IDtyp, I1, WorkingDir, Filenameonly)
        Dim seprun As Boolean = False    ' set true as run one by one
        ReDim Stress1(40), Stress8(40)
        Dim bIDCase As String = Nothing
        Dim t1 As String = "0"
        Dim lutty, IStep As Integer
        'Call modWrite.WriteNike()  ' Used for check input values
        Call com.get_time(t1)
        Call EraseOutput()


        'Call InitialSet(I1, bIDCase, lutty, WorkingDir)    ' YC 121219-4
        Call InitialSet(I1, bIDCase, lutty, WorkingDir, Filenameonly)


        Call WinYield(StopFEDFAA, FEDFAAStopped)
        Dim Ntimestep As Integer = 1
        Dim nload0 As Integer = clsCom.nload
        Dim nod0(nload0), idirn0(nload0), ncur0(nload0) As Integer
        Dim fac0(nload0) As Double
        If seprun Then
            Ntimestep = clsCom.ntime : clsCom.ntime = 1
            Call Setstep1(nod0, idirn0, ncur0, fac0)
        End If

        For IStep = 1 To Ntimestep
            clsCom.Ntimestep = IStep
            If seprun Then
                Call Setstep2(IStep, nload0, nod0, idirn0, ncur0, fac0)
            End If
            If IStep = 1 Then
                Call nik.inputi(lutty)
                Call com.get_time(t1)
                newtext = "Input File Done, " & t1 & vbCrLf
                Print(lutty, newtext)
            End If
            Call ini.intial(nik)
            Call com.get_time(t1)
            newtext = vbCrLf & "Aircraft #" & IStep & " Initialize Data Done, " & t1 & vbCrLf
            Print(lutty, newtext)
            Call com.get_time(t1)
            Call objSolve.solve(ini, IStep, Stress1, Stress8, ModelOut)

        Next IStep
        Call com.get_time(t1)
        newtext = vbCrLf & vbCrLf & "FAASR3D Program Done, " & t1
        Print(lutty, newtext)
        Call EraseInput(lutty)

    End Sub
End Class

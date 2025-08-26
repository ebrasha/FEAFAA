'YC 101215 092016
' change "Single" to "Double", "CSng" to "CDbl" for FAASR3D by YC 102418-041519 

Option Strict On
Option Explicit On
Imports System.IO
Imports System.Text.RegularExpressions

Public Class clsMesh

    Public nikeinstrFile As String
    Public nikeinloaFile As String

    Public NLcd As Integer = 0
    Public NSld As Integer = 0
    Public NMat As Integer = 0
    Public NPart As Integer = 0
    Public NPartSld As Integer = 0
    Public NPartBC As Integer = 0
    Public NPartMat As Integer = 0

    Public itpro As Integer

    Public NNode As Integer = 0, NNd As Integer = 0
    Public NBrckEle As Integer = 0
    Public NSpringElement As Integer = 0, NSpringType As Integer = 0
    Public TotNNodalLoad As Integer = 0

    Public IDSolver, NAC As Integer

    Public ZMin, ZMax As Double

    Public ErrTol As Double = 0.0001

    Public Structure LoadCurveCharacteristics
        Dim LdPntTime() As Double
        Dim LdPntMag() As Double
    End Structure

    Public Structure SlidingCharacteristics
        Dim IdxSld As Integer
        Dim TypSld As String
        Dim PenaltySld As Double
        Dim FrictionSld As Double
        Dim TypSldMerge As String
    End Structure

    Public Structure MaterialCharacteristics
        Dim IdxMat As Integer
        Dim KeyMatPrpty As Integer
        Dim MatPara(,) As Double
        Dim DensityMat As Double
    End Structure

    Public Structure PartCharacteristics
        Dim NXCtr, NYCtr, NZCtr As Integer
        Dim NdXCtr(), NdYCtr(), NdZCtr() As Integer
        Dim CoordXCtr(), CoordYCtr(), CoordZCtr() As Double
    End Structure

    Public Structure PartSldCharacteristics
        Dim IdxPart As Integer
        Dim SldNdIdxCtrIni(), SldNdIdxCtrEnd() As Integer
        Dim SldIdx As Integer
        Dim SldSmTyp As String
        Dim SldDir() As Integer
    End Structure

    Public Structure PartBCCharacteristics
        Dim IdxPart As Integer
        Dim BCNdIdxCtrIni(), BCNdIdxCtrEnd() As Integer
        Dim StrBCValue As String
    End Structure

    Public Structure PartMatCharacteristics
        Dim IdxPart As Integer
        Dim MatNdIdxCtrIni(), MatNdIdxCtrEnd() As Integer
        Dim MatIdx As Integer
    End Structure

    Public Structure ACLoadCharacteristics
        Dim NWhls As Integer
        Dim PcntCt As Double
        Dim XTr(), YTr() As Double
        Dim XDim, YDim As Double
        Dim IndxSy As Integer
        Dim ScaleX, ScaleY As Double
        Dim NSlabs As Integer
        Dim XGrid(), YGrid() As Double
        Dim f1, f2 As Double
        Dim Alpha As Double
        Dim EqStifX, EqStifY As Double, KeyEqStif As Integer
        Dim EqStif12X, EqStif12Y As Double, KeyEqStif12 As Integer
        Dim EqEdgStifX, EqEdgStifY As Double, KeyEqEdgStif As Integer
        Dim IndxIm As Integer
        Dim IndxFn As Integer
        Dim ThkSlb As Double
        Dim IndxOvrl As Integer
        Dim ThkOvr As Double
        Dim IsThermal As Boolean, LETG, Cte As Double, CurlShapara As Integer
    End Structure

    Public Structure NodeCharacteristics
        Dim BXYZ, BRXYZ As Integer
        Dim X, Y, Z As Double
        Dim XMinusMesh, YMinusMesh, ZMinusMesh As Double
        Dim XPlusMesh, YPlusMesh, ZPlusMesh As Double
        Dim IdxMat() As Integer
        Dim IdxPart() As Integer
        Dim IdxSld As Integer
        Dim TypSldSm As String
        Dim Temp As Double
    End Structure

    Public Structure BrickElementCharacteristics
        Dim IdxMat As Integer
        Dim Node() As Integer
    End Structure

    Public Structure SpringTypeCharacteristics
        Dim Stiffness As Double
        Dim Direction As Integer
    End Structure

    Public Structure SpringElementCharacteristics
        Dim Node1, Node2 As Integer
        Dim IdxSprType As Integer
        Dim Stiffness As Double
        Dim Direction As Integer
    End Structure

    Public Structure SlidingElementCharacteristics
        Dim NSldElement As Integer
        Dim Element(,) As Integer
        Dim PenaltySld As Double
        Dim FrictionSld As Double
        Dim IdxDirection As Integer
    End Structure

    Public Structure NodalLoadCharacteristics
        Dim NNodalLoad As Integer
        Dim Node() As Integer
        Dim Load() As Double
    End Structure

    Public LCD() As LoadCurveCharacteristics
    Public SLD() As SlidingCharacteristics
    Public MAT() As MaterialCharacteristics

    Public PART() As PartCharacteristics
    Public PARTSLD() As PartSldCharacteristics
    'Public PARTSLDNEG(), PARTSLDPOS() As PartSldCharacteristics
    Public PARTBC() As PartBCCharacteristics
    Public PARTMAT() As PartMatCharacteristics

    Public ACLoad() As ACLoadCharacteristics

    Public Node() As NodeCharacteristics
    Public Nd() As NodeCharacteristics
    Public BrickElement() As BrickElementCharacteristics

    Public SpringType() As SpringTypeCharacteristics
    Public SpringElement() As SpringElementCharacteristics

    Public SlidingElement() As SlidingElementCharacteristics

    Public NodalLoad() As NodalLoadCharacteristics

    'Public Sub MeshGeneration(ByVal WorkingDir As String)

    ' QW 09-15-2019
    ' Public Sub MeshGeneration(ByRef LCD0() As LoadCurveCharacteristics, _
    '                          ByVal SLD0() As SlidingCharacteristics, _
    '                          ByVal MAT0() As MaterialCharacteristics, _
    '                          ByVal PART0() As PartCharacteristics, _
    '                          ByVal PARTSLD0() As PartSldCharacteristics, _
    '                          ByVal PARTBC0() As PartBCCharacteristics, _
    '                          ByVal PARTMAT0() As PartMatCharacteristics,
    '                         ByVal ACload0() As ACLoadCharacteristics,
    '                         ByVal WorkingDir As String)
    Public Sub MeshGeneration(ByRef LCD0() As LoadCurveCharacteristics,
                      ByRef ND0() As NodeCharacteristics,
                      ByRef BrickElement0() As BrickElementCharacteristics,
                      ByRef SpringType0() As SpringTypeCharacteristics,
                      ByRef SpringElement0() As SpringElementCharacteristics,
                      ByRef SlidingElement0() As SlidingElementCharacteristics,
                      ByRef NodalLoad0() As NodalLoadCharacteristics,
                      ByVal SLD0() As SlidingCharacteristics,
                      ByVal MAT0() As MaterialCharacteristics,
                      ByVal PART0() As PartCharacteristics,
                      ByVal PARTSLD0() As PartSldCharacteristics,
                      ByVal PARTBC0() As PartBCCharacteristics,
                      ByVal PARTMAT0() As PartMatCharacteristics,
                      ByVal ACload0() As ACLoadCharacteristics,
                      ByVal WorkingDir As String, ByVal Filenameonly As String, ByVal ModelOut As Integer)
        ' End QW 09-15-2019


        'nikeinstrFile = WorkingDir & "nikein.ing"
        'nikeinloaFile = WorkingDir & "nikein.rgd"

        'Call ReadStrFile(nikeinstrFile)

        ''Call ReadLoaFile(nikeinloaFile)

        LCD = LCD0 : NLcd = LCD.Length - 1
        SLD = SLD0 : NSld = SLD.Length - 1
        MAT = MAT0 : NMat = MAT.Length - 1
        PART = PART0 : NPart = PART.Length - 1
        PARTSLD = PARTSLD0 : NPartSld = PARTSLD.Length - 1
        PARTBC = PARTBC0 : NPartBC = PARTBC.Length - 1
        PARTMAT = PARTMAT0 : NPartMat = PARTMAT.Length - 1
        ACLoad = ACload0 : NAC = ACLoad.Length - 1

        If ACLoad(1).IsThermal Then itpro = 1

        Call NodeGeneration()

        Call BrickElementGeneration()

        Call SpringElementGeneration()

        Call SlidingElementGeneration()

        If ACLoad(1).Alpha = 0 Or ACLoad(1).Alpha = 90 Then
            Call NodalLoadGeneration()
        Else
            Call NodalLoadAngleGeneration()
        End If

        If itpro = 1 Then Call NodalTempGeneration()
        'Call WriteNikeinFile(WorkingDir)
        If ModelOut = 1 Then        ' QW 09-15-201
            Dim path As String = WorkingDir & "PrintOut-" & Filenameonly & "\"
            If Not Directory.Exists(path) Then
                Directory.CreateDirectory(path)
            End If
            Call WriteNikeinFile(WorkingDir & "PrintOut-" & Filenameonly)
        End If


        ' QW 08-23-2017
        ND0 = Nd : BrickElement0 = BrickElement : SpringType0 = SpringType : SpringElement0 = SpringElement
        SlidingElement0 = SlidingElement : NodalLoad0 = NodalLoad
        ' End  QW 08-23-2017

    End Sub

    Private Sub ReadStrFile(ByVal strFile As String)

        Const StrInt As String = "((-?)(\d+))"    ' integer string
        Const StrFloat As String = "((-?)(\d+\.?\d*))"     'float string
        Const StrScientific As String = StrFloat & "((e|E)([-+])\d+){0,1}"  'scientific (or float) string

        Dim reLcdHeaderPattern As String = "(\s*)(lcd)(\s+)" & "(" & "?<ldidx>" & StrInt & ")" & "(\s+)" & "(" & "?<ldpntnum>" & StrInt & ")"
        Dim reSldPattern As String = "(\s*)(si)(\s+)" & "(" & "?<sldidx>" & StrInt & ")" & "(\s*)(?<sldtyp>\w+)" & "(\s+)(pnlt)(\s+)" & "(" & "?<sldpnlt>" & StrFloat & ")" & "(\s+)(fric)(\s+)" & "(" & "?<sldfric>" & StrFloat & ")" & "(\s+)(?<sldmergetyp>\w+)" & "(\s*)"
        Dim reMatHeaderPattern As String = "(\s*)(mat)(\s+)" & "(" & "?<matidx>" & StrInt & ")" & "(\s+)" & "(" & "?<matproperty>" & StrInt & ")"
        Dim rePartSldPattern As String = "(\s*)(si)([+-])(\s+)" & "(" & "?<sldndxi>" & StrInt & ")" & "(\s+)" & "(" & "?<sldndyi>" & StrInt & ")" & "(\s+)" & "(" & "?<sldndzi>" & StrInt & ")" & "(\s+)" _
                                       & "(" & "?<sldndxe>" & StrInt & ")" & "(\s+)" & "(" & "?<sldndye>" & StrInt & ")" & "(\s+)" & "(" & "?<sldndze>" & StrInt & ")" & "(\s+)" _
                                       & "(" & "?<sldidx>" & StrInt & ")" & "(\s+)(?<sldsmtype>\w+)(\s+)" _
                                       & "(" & "?<slddirx>" & StrInt & ")" & "(\s+)" & "(" & "?<slddiry>" & StrInt & ")" & "(\s+)" & "(" & "?<slddirz>" & StrInt & ")" & "(\s*)"
        Dim rePartBCPattern As String = "(\s*)(b)(\s+)" & "(" & "?<bndryndxi>" & StrInt & ")" & "(\s+)" & "(" & "?<bndryndyi>" & StrInt & ")" & "(\s+)" & "(" & "?<bndryndzi>" & StrInt & ")" & "(\s+)" _
                                       & "(" & "?<bndryndxe>" & StrInt & ")" & "(\s+)" & "(" & "?<bndryndye>" & StrInt & ")" & "(\s+)" & "(" & "?<bndryndze>" & StrInt & ")" & "(\s+)" _
                                       & "(?<bndryval>\d{6})" & "(\s*)"
        Dim rePartMatePattern As String = "(\s*)(mate)(\s+)" & "(" & "?<matidx>" & StrInt & ")" & "(\s*)"
        Dim rePartMtPattern As String = "(\s*)(mt)(\s+)" & "(" & "?<matndxi>" & StrInt & ")" & "(\s+)" & "(" & "?<matndyi>" & StrInt & ")" & "(\s+)" & "(" & "?<matndzi>" & StrInt & ")" & "(\s+)" _
                                       & "(" & "?<matndxe>" & StrInt & ")" & "(\s+)" & "(" & "?<matndye>" & StrInt & ")" & "(\s+)" & "(" & "?<matndze>" & StrInt & ")" & "(\s+)" _
                                      & "(" & "?<matidx>" & StrInt & ")" & "(\s*)"

        Dim STRFileLines() As String, NLen As Integer
        Dim ReadLineString As String

        Try

            STRFileLines = System.IO.File.ReadAllLines(strFile)
            NLen = STRFileLines.Length

            Dim ilen As Integer
            Dim NPartSldNeg As Integer = 0, NPartSldPos As Integer = 0
            '******************************************************************
            '******************************************************************
            ' to obtain the number of load, sliding interface, material and part
            For ilen = 0 To NLen - 1
                ReadLineString = STRFileLines(ilen)

                If Regex.IsMatch(ReadLineString, "(lcd)(\s+)") Then
                    NLcd = NLcd + 1
                End If

                If Regex.IsMatch(ReadLineString, "(si)(\s+)") Then
                    NSld = NSld + 1
                End If

                If Regex.IsMatch(ReadLineString, "(mat)(\s+)") Then
                    NMat = NMat + 1
                End If

                If Regex.IsMatch(ReadLineString, "(start)") Then
                    NPart = NPart + 1
                End If

                If Regex.IsMatch(ReadLineString, "(si\-)(\s+)") Then
                    NPartSldNeg = NPartSldNeg + 1
                End If

                If Regex.IsMatch(ReadLineString, "(si\+)(\s+)") Then
                    NPartSldPos = NPartSldPos + 1
                End If

                If Regex.IsMatch(ReadLineString, rePartBCPattern) Then
                    NPartBC = NPartBC + 1
                End If

                If Regex.IsMatch(ReadLineString, "(mate|mt)(\s+)") Then
                    NPartMat = NPartMat + 1
                End If

            Next
            '******************************************************************
            '******************************************************************

            If NPartSldNeg <> NPartSldPos Then
                MsgBox("Error: The number of si+ is not equal to the number of si- in .str file", MsgBoxStyle.OkOnly, "File Error")
                Exit Sub
            End If

            NPartSld = NPartSldNeg + NPartSldPos

            ReDim LCD(NLcd)
            ReDim SLD(NSld)
            ReDim MAT(NMat)
            ReDim PART(NPart)
            ReDim PARTSLD(NPartSld)
            'ReDim PARTSLDNEG(NPartSldNeg), PARTSLDPOS(NPartSldPos)
            ReDim PARTBC(NPartBC)
            ReDim PARTMAT(NPartMat)

            Dim reLcdHeader As New Regex(reLcdHeaderPattern)
            Dim IdxLd, NumLdPnt As Integer
            Dim reLdPntPattern As String = "(\s*)" & "(" & "?<ldtimpnt>" & StrFloat & ")" & "(\s+)" & "(" & "?<ldmagpnt>" & StrFloat & ")" & "(\s*)"
            Dim reLdPnt As New Regex(reLdPntPattern)
            Dim isLdPntReading As Boolean = False

            Dim reSld As New Regex(reSldPattern)

            Dim reTpro As New Regex("(\s*)(tpro)(\s+)" & "(" & "?<itpro>" & StrInt & ")")

            Dim reMatHeader As New Regex(reMatHeaderPattern)
            Dim isMatReading As Boolean = False

            Dim reInt As New Regex("(\s*)" & StrInt & "(\s*)")   'Regular Expression of integer
            Dim reFloat As New Regex("(\s*)" & StrFloat & "(\s*)")   'Regular Expression of float
            Dim reScientific As New Regex("(\s*)" & StrScientific & "(\s*)")   'Regular Expression of scientific and float

            Dim isNdXCtrReading As Boolean = False, isNdYCtrReading As Boolean = False, isNdZCtrReading As Boolean = False
            Dim isXCtrReading As Boolean = False, isYCtrReading As Boolean = False, isZCtrReading As Boolean = False
            Dim isPartReading As Boolean = False

            Dim rePartSld As New Regex(rePartSldPattern)
            Dim rePartBC As New Regex(rePartBCPattern)
            Dim rePartMate As New Regex(rePartMatePattern)
            Dim rePartMt As New Regex(rePartMtPattern)

            Dim m As Match

            Dim iLcd As Integer = 0
            Dim iSld As Integer = 0
            Dim iMat As Integer = 0
            Dim iPart As Integer = 0
            Dim iPartSld As Integer
            'Dim iPartSldNeg As Integer = 0, iPartSldPos As Integer = 0
            Dim iPartBc As Integer = 0
            Dim iPartMat As Integer = 0

            Dim i, j As Integer
            '******************************************************************
            '******************************************************************
            For ilen = 0 To NLen - 1
                ReadLineString = STRFileLines(ilen)

                '********************** to read in Load definition****************************
                If Regex.IsMatch(ReadLineString, "(lcd)(\s+)") Then

                    iLcd = iLcd + 1

                    For Each m In reLcdHeader.Matches(ReadLineString)
                        IdxLd = CInt(m.Groups("ldidx").Value)
                        NumLdPnt = CInt(m.Groups("ldpntnum").Value)
                    Next

                    If NLcd <> NumLdPnt - 1 Then
                        MsgBox("The number of load and the number of load point defintion is not consistent!" & vbCrLf, MsgBoxStyle.OkOnly, "File Error")
                        Exit Sub
                    End If

                    ReDim LCD(iLcd).LdPntTime(NumLdPnt), LCD(iLcd).LdPntMag(NumLdPnt)

                    isLdPntReading = True
                    j = 1

                    GoTo nxtln
                End If

                If isLdPntReading Then

                    For Each m In reLdPnt.Matches(ReadLineString)
                        LCD(iLcd).LdPntTime(j) = CDbl(m.Groups("ldtimpnt").Value)
                        LCD(iLcd).LdPntMag(j) = CDbl(m.Groups("ldmagpnt").Value)
                        j = j + 1
                    Next

                    If j > NumLdPnt Then isLdPntReading = False

                    GoTo nxtln
                End If
                '********************** read in Load definition END****************************


                '**********************to read in temperature indicator ****************************
                If Regex.IsMatch(ReadLineString, "(tpro)(\s+)") Then
                    For Each m In reTpro.Matches(ReadLineString)
                        itpro = CInt(m.Groups("itpro").Value)
                    Next
                End If
                '**********************read in temperature indicator END****************************


                '**********************to read in sliding interface definition ****************************
                If Regex.IsMatch(ReadLineString, "(si)(\s+)") Then
                    iSld = iSld + 1

                    For Each m In reSld.Matches(ReadLineString)
                        SLD(iSld).IdxSld = CInt(m.Groups("sldidx").Value)
                        SLD(iSld).TypSld = m.Groups("sldtyp").Value
                        SLD(iSld).PenaltySld = CDbl(m.Groups("sldpnlt").Value)
                        SLD(iSld).FrictionSld = CDbl(m.Groups("sldfric").Value)
                        SLD(iSld).TypSldMerge = m.Groups("sldmergetyp").Value
                    Next
                End If
                '**********************read in sliding interface definition ****************************


                '**********************to read in material definition ****************************
                If Regex.IsMatch(ReadLineString, "(mat)(\s+)") Then
                    iMat = iMat + 1

                    For Each m In reMatHeader.Matches(ReadLineString)
                        MAT(iMat).IdxMat = CInt(m.Groups("matidx").Value)
                        MAT(iMat).KeyMatPrpty = CInt(m.Groups("matproperty").Value)
                    Next

                    isMatReading = True
                    i = 0

                    ReDim MAT(iMat).MatPara(6, 8)

                    GoTo nxtln
                End If

                If isMatReading Then

                    If Regex.IsMatch(ReadLineString, "(temp)(\s+)") Then
                        i = i + 1
                        j = 1
                        For Each m In reFloat.Matches(ReadLineString)
                            If j <= 8 Then
                                MAT(iMat).MatPara(i, j) = CDbl(m.Value)
                                j = j + 1
                            End If
                        Next
                        GoTo nxtln
                    End If

                    If Regex.IsMatch(ReadLineString, "(e)(\s+)") Then
                        i = i + 1
                        j = 1
                        For Each m In reFloat.Matches(ReadLineString)
                            If j <= 8 Then
                                MAT(iMat).MatPara(i, j) = CDbl(m.Value)
                                j = j + 1
                            End If
                        Next
                        GoTo nxtln
                    End If

                    If Regex.IsMatch(ReadLineString, "(pr)(\s+)") Then
                        i = i + 1
                        j = 1
                        For Each m In reFloat.Matches(ReadLineString)
                            If j <= 8 Then
                                MAT(iMat).MatPara(i, j) = CDbl(m.Value)
                                j = j + 1
                            End If
                        Next
                        GoTo nxtln
                    End If

                    If Regex.IsMatch(ReadLineString, "(alpha)(\s+)") Then
                        i = i + 1
                        j = 1
                        For Each m In reScientific.Matches(ReadLineString)
                            If j <= 8 Then
                                MAT(iMat).MatPara(i, j) = CDbl(m.Value)
                                j = j + 1
                            End If
                        Next
                        GoTo nxtln
                    End If

                    If Regex.IsMatch(ReadLineString, "(ro)(\s+)") Then
                        For Each m In reScientific.Matches(ReadLineString)
                            MAT(iMat).DensityMat = CDbl(m.Value)
                        Next
                        GoTo nxtln
                    End If

                    If Regex.IsMatch(ReadLineString, "(endmat)") Then isMatReading = False

                    GoTo nxtln
                End If
                '********************** read in material definition END ****************************


                '********************** to read in Part definition****************************
                If Regex.IsMatch(ReadLineString, "(start)") Then
                    iPart = iPart + 1
                    isPartReading = True
                    isNdXCtrReading = True

                    GoTo nxtln
                End If

                If isNdXCtrReading Then

                    PART(iPart).NXCtr = reInt.Matches(ReadLineString).Count
                    ReDim PART(iPart).NdXCtr(PART(iPart).NXCtr)

                    j = 1
                    For Each m In reInt.Matches(ReadLineString)
                        PART(iPart).NdXCtr(j) = CInt(m.Value)
                        j = j + 1
                    Next

                    isNdXCtrReading = False
                    isNdYCtrReading = True

                    GoTo nxtln
                End If


                If isNdYCtrReading Then

                    PART(iPart).NYCtr = reInt.Matches(ReadLineString).Count
                    ReDim PART(iPart).NdYCtr(PART(iPart).NYCtr)

                    j = 1
                    For Each m In reInt.Matches(ReadLineString)
                        PART(iPart).NdYCtr(j) = CInt(m.Value)
                        j = j + 1
                    Next

                    isNdYCtrReading = False
                    isNdZCtrReading = True

                    GoTo nxtln
                End If


                If isNdZCtrReading Then

                    PART(iPart).NZCtr = reInt.Matches(ReadLineString).Count
                    ReDim PART(iPart).NdZCtr(PART(iPart).NZCtr)

                    j = 1
                    For Each m In reInt.Matches(ReadLineString)
                        PART(iPart).NdZCtr(j) = CInt(m.Value)
                        j = j + 1
                    Next

                    isNdZCtrReading = False
                    isXCtrReading = True

                    GoTo nxtln

                End If


                If isXCtrReading Then

                    ReDim PART(iPart).CoordXCtr(PART(iPart).NXCtr)

                    j = 1
                    For Each m In reFloat.Matches(ReadLineString)
                        PART(iPart).CoordXCtr(j) = CDbl(m.Value)
                        j = j + 1
                    Next

                    isXCtrReading = False
                    isYCtrReading = True

                    GoTo nxtln
                End If

                If isYCtrReading Then

                    ReDim PART(iPart).CoordYCtr(PART(iPart).NYCtr)

                    j = 1
                    For Each m In reFloat.Matches(ReadLineString)
                        PART(iPart).CoordYCtr(j) = CDbl(m.Value)
                        j = j + 1
                    Next

                    isYCtrReading = False
                    isZCtrReading = True

                    GoTo nxtln
                End If

                If isZCtrReading Then

                    ReDim PART(iPart).CoordZCtr(PART(iPart).NZCtr)

                    j = 1
                    For Each m In reFloat.Matches(ReadLineString)
                        PART(iPart).CoordZCtr(j) = CDbl(m.Value)
                        j = j + 1
                    Next

                    isZCtrReading = False

                    GoTo nxtln
                End If

                If isPartReading And Regex.IsMatch(ReadLineString, "(si)([+-])(\s+)") Then
                    iPartSld = iPartSld + 1

                    ReDim PARTSLD(iPartSld).SldNdIdxCtrIni(3), PARTSLD(iPartSld).SldNdIdxCtrEnd(3), PARTSLD(iPartSld).SldDir(3)

                    PARTSLD(iPartSld).IdxPart = iPart
                    For Each m In rePartSld.Matches(ReadLineString)
                        PARTSLD(iPartSld).SldNdIdxCtrIni(1) = CInt(m.Groups("sldndxi").Value)
                        PARTSLD(iPartSld).SldNdIdxCtrIni(2) = CInt(m.Groups("sldndyi").Value)
                        PARTSLD(iPartSld).SldNdIdxCtrIni(3) = CInt(m.Groups("sldndzi").Value)
                        PARTSLD(iPartSld).SldNdIdxCtrEnd(1) = CInt(m.Groups("sldndxe").Value)
                        PARTSLD(iPartSld).SldNdIdxCtrEnd(2) = CInt(m.Groups("sldndye").Value)
                        PARTSLD(iPartSld).SldNdIdxCtrEnd(3) = CInt(m.Groups("sldndze").Value)
                        PARTSLD(iPartSld).SldIdx = CInt(m.Groups("sldidx").Value)
                        PARTSLD(iPartSld).SldSmTyp = m.Groups("sldsmtype").Value
                        PARTSLD(iPartSld).SldDir(1) = CInt(m.Groups("slddirx").Value)
                        PARTSLD(iPartSld).SldDir(2) = CInt(m.Groups("slddiry").Value)
                        PARTSLD(iPartSld).SldDir(3) = CInt(m.Groups("slddirz").Value)
                    Next

                    GoTo nxtln
                End If

                'If isPartReading And Regex.IsMatch(ReadLineString, "(si\-)(\s+)") Then
                '    iPartSldNeg = iPartSldNeg + 1

                '    ReDim PARTSLDNEG(iPartSldNeg).SldNdIdxCtrIni(3), PARTSLDNEG(iPartSldNeg).SldNdIdxCtrEnd(3), PARTSLDNEG(iPartSldNeg).SldDir(3)

                '    PARTSLDNEG(iPartSldNeg).IdxPart = iPart
                '    For Each m In rePartSld.Matches(ReadLineString)
                '        PARTSLDNEG(iPartSldNeg).SldNdIdxCtrIni(1) = CInt(m.Groups("sldndxi").Value)
                '        PARTSLDNEG(iPartSldNeg).SldNdIdxCtrIni(2) = CInt(m.Groups("sldndyi").Value)
                '        PARTSLDNEG(iPartSldNeg).SldNdIdxCtrIni(3) = CInt(m.Groups("sldndzi").Value)
                '        PARTSLDNEG(iPartSldNeg).SldNdIdxCtrEnd(1) = CInt(m.Groups("sldndxe").Value)
                '        PARTSLDNEG(iPartSldNeg).SldNdIdxCtrEnd(2) = CInt(m.Groups("sldndye").Value)
                '        PARTSLDNEG(iPartSldNeg).SldNdIdxCtrEnd(3) = CInt(m.Groups("sldndze").Value)
                '        PARTSLDNEG(iPartSldNeg).SldIdx = CInt(m.Groups("sldidx").Value)
                '        PARTSLDNEG(iPartSldNeg).SldSmTyp = m.Groups("sldsmtype").Value
                '        PARTSLDNEG(iPartSldNeg).SldDir(1) = CInt(m.Groups("slddirx").Value)
                '        PARTSLDNEG(iPartSldNeg).SldDir(2) = CInt(m.Groups("slddiry").Value)
                '        PARTSLDNEG(iPartSldNeg).SldDir(3) = CInt(m.Groups("slddirz").Value)
                '    Next

                '    GoTo nxtln
                'End If

                'If isPartReading And Regex.IsMatch(ReadLineString, "(si\+)(\s+)") Then
                '    iPartSldPos = iPartSldPos + 1

                '    ReDim PARTSLDPOS(iPartSldPos).SldNdIdxCtrIni(3), PARTSLDPOS(iPartSldPos).SldNdIdxCtrEnd(3), PARTSLDPOS(iPartSldPos).SldDir(3)

                '    PARTSLDPOS(iPartSldPos).IdxPart = iPart
                '    For Each m In rePartSld.Matches(ReadLineString)
                '        PARTSLDPOS(iPartSldPos).SldNdIdxCtrIni(1) = CInt(m.Groups("sldndxi").Value)
                '        PARTSLDPOS(iPartSldPos).SldNdIdxCtrIni(2) = CInt(m.Groups("sldndyi").Value)
                '        PARTSLDPOS(iPartSldPos).SldNdIdxCtrIni(3) = CInt(m.Groups("sldndzi").Value)
                '        PARTSLDPOS(iPartSldPos).SldNdIdxCtrEnd(1) = CInt(m.Groups("sldndxe").Value)
                '        PARTSLDPOS(iPartSldPos).SldNdIdxCtrEnd(2) = CInt(m.Groups("sldndye").Value)
                '        PARTSLDPOS(iPartSldPos).SldNdIdxCtrEnd(3) = CInt(m.Groups("sldndze").Value)
                '        PARTSLDPOS(iPartSldPos).SldIdx = CInt(m.Groups("sldidx").Value)
                '        PARTSLDPOS(iPartSldPos).SldSmTyp = m.Groups("sldsmtype").Value
                '        PARTSLDPOS(iPartSldPos).SldDir(1) = CInt(m.Groups("slddirx").Value)
                '        PARTSLDPOS(iPartSldPos).SldDir(2) = CInt(m.Groups("slddiry").Value)
                '        PARTSLDPOS(iPartSldPos).SldDir(3) = CInt(m.Groups("slddirz").Value)
                '    Next

                '    GoTo nxtln
                'End If

                If isPartReading And Regex.IsMatch(ReadLineString, "(b)(\s+)") Then
                    iPartBc = iPartBc + 1

                    ReDim PARTBC(iPartBc).BCNdIdxCtrIni(3), PARTBC(iPartBc).BCNdIdxCtrEnd(3)

                    PARTBC(iPartBc).IdxPart = iPart
                    For Each m In rePartBC.Matches(ReadLineString)
                        PARTBC(iPartBc).BCNdIdxCtrIni(1) = CInt(m.Groups("bndryndxi").Value)
                        PARTBC(iPartBc).BCNdIdxCtrIni(2) = CInt(m.Groups("bndryndyi").Value)
                        PARTBC(iPartBc).BCNdIdxCtrIni(3) = CInt(m.Groups("bndryndzi").Value)
                        PARTBC(iPartBc).BCNdIdxCtrEnd(1) = CInt(m.Groups("bndryndxe").Value)
                        PARTBC(iPartBc).BCNdIdxCtrEnd(2) = CInt(m.Groups("bndryndye").Value)
                        PARTBC(iPartBc).BCNdIdxCtrEnd(3) = CInt(m.Groups("bndryndze").Value)
                        If Len(m.Groups("bndryval").Value) <> 6 Then
                            MsgBox("Error: The digit number of boundary value is not equal to 6", MsgBoxStyle.OkOnly, "File Error")
                            Exit Sub
                        End If
                        PARTBC(iPartBc).StrBCValue = m.Groups("bndryval").Value
                    Next

                    GoTo nxtln
                End If

                If isPartReading And Regex.IsMatch(ReadLineString, "(mate)(\s+)") Then
                    iPartMat = iPartMat + 1

                    ReDim PARTMAT(iPartMat).MatNdIdxCtrIni(3), PARTMAT(iPartMat).MatNdIdxCtrEnd(3)

                    PARTMAT(iPartMat).IdxPart = iPart
                    PARTMAT(iPartMat).MatNdIdxCtrIni(1) = 1
                    PARTMAT(iPartMat).MatNdIdxCtrIni(2) = 1
                    PARTMAT(iPartMat).MatNdIdxCtrIni(3) = 1
                    PARTMAT(iPartMat).MatNdIdxCtrEnd(1) = PART(iPart).NXCtr
                    PARTMAT(iPartMat).MatNdIdxCtrEnd(2) = PART(iPart).NYCtr
                    PARTMAT(iPartMat).MatNdIdxCtrEnd(3) = PART(iPart).NZCtr

                    For Each m In rePartMate.Matches(ReadLineString)
                        PARTMAT(iPartMat).MatIdx = CInt(m.Groups("matidx").Value)
                    Next

                    GoTo nxtln
                End If

                If isPartReading And Regex.IsMatch(ReadLineString, "(mt)(\s+)") Then
                    iPartMat = iPartMat + 1

                    ReDim PARTMAT(iPartMat).MatNdIdxCtrIni(3), PARTMAT(iPartMat).MatNdIdxCtrEnd(3)

                    PARTMAT(iPartMat).IdxPart = iPart
                    For Each m In rePartMt.Matches(ReadLineString)
                        PARTMAT(iPartMat).MatNdIdxCtrIni(1) = CInt(m.Groups("matndxi").Value)
                        PARTMAT(iPartMat).MatNdIdxCtrIni(2) = CInt(m.Groups("matndyi").Value)
                        PARTMAT(iPartMat).MatNdIdxCtrIni(3) = CInt(m.Groups("matndzi").Value)
                        PARTMAT(iPartMat).MatNdIdxCtrEnd(1) = CInt(m.Groups("matndxe").Value)
                        PARTMAT(iPartMat).MatNdIdxCtrEnd(2) = CInt(m.Groups("matndye").Value)
                        PARTMAT(iPartMat).MatNdIdxCtrEnd(3) = CInt(m.Groups("matndze").Value)
                        PARTMAT(iPartMat).MatIdx = CInt(m.Groups("matidx").Value)
                    Next

                    GoTo nxtln
                End If

                If isPartReading And Regex.IsMatch(ReadLineString, "(end)") Then
                    isPartReading = False
                    GoTo nxtln
                End If
                '********************** read in Part definition END****************************

nxtln:      Next
            '******************************************************************
            '******************************************************************

        Catch ex As Exception
            'MsgBox("Error " & ex.Message & vbCr & "Occured reading the .str file in ReadStrFile." & vbCrLf & "at the following data:" & vbCrLf & ex.Data.Item(0).ToString, MsgBoxStyle.OkOnly, "File Error")
            MsgBox("Error " & ex.Message & vbCr & "Occured reading the .str file in ReadStrFile.", MsgBoxStyle.OkOnly, "File Error")
            Exit Sub
        End Try

    End Sub

    Private Sub ReadLoaFile(ByVal loaFile As String)

        Const StrFloat As String = "((-?)(\d+\.?\d*))"     'float string
        Const StrScientific As String = StrFloat & "((e|E)([-+])\d+){0,1}"  'scientific (or float) string

        'Dim reTwoNumPara As New Regex("(\s*)" & "(" & "?<para1>" & StrFloat & ")" & "(\s+)" & "(" & "?<para2>" & StrFloat & ")" & "(\s*)")
        'Dim reThreeNumPara As New Regex("(\s*)" & "(" & "?<para1>" & StrFloat & ")" & "(\s+)" & "(" & "?<para2>" & StrFloat & ")" & "(\s+)" & "(" & "?<para3>" & StrFloat & ")" & "(\s*)")
        Dim reTwoNumPara As New Regex("(\s*)" & "(" & "?<para1>" & StrScientific & ")" & "(\s+)" & "(" & "?<para2>" & StrScientific & ")" & "(\s*)")
        Dim reThreeNumPara As New Regex("(\s*)" & "(" & "?<para1>" & StrScientific & ")" & "(\s+)" & "(" & "?<para2>" & StrScientific & ")" & "(\s+)" & "(" & "?<para3>" & StrScientific & ")" & "(\s*)")


        Dim m As Match
        '******************************************************************
        '******************************************************************
        '******************************************************************

        Try

            FileOpen(1, loaFile, OpenMode.Input)

            IDSolver = CInt(LineInput(1))
            NAC = CInt(LineInput(1))

            ReDim ACLoad(NAC)

            Dim iAC, j As Integer

            For iAC = 1 To NAC
                ACLoad(iAC).NWhls = CInt(LineInput(1))

                ReDim ACLoad(iAC).XTr(ACLoad(iAC).NWhls), ACLoad(iAC).YTr(ACLoad(iAC).NWhls)

                'ACLoad(iAC).XGrid = CDbl(LineInput(1))
                'ACLoad(iAC).YGrid = CDbl(LineInput(1))
                ACLoad(iAC).PcntCt = CDbl(LineInput(1))

                For j = 1 To ACLoad(iAC).NWhls
                    For Each m In reTwoNumPara.Matches(LineInput(1))
                        ACLoad(iAC).XTr(j) = CDbl(m.Groups("para1").Value)
                        ACLoad(iAC).YTr(j) = CDbl(m.Groups("para2").Value)
                    Next
                Next

                ACLoad(iAC).XDim = CDbl(LineInput(1))
                ACLoad(iAC).YDim = CDbl(LineInput(1))
                ACLoad(iAC).IndxSy = CInt(LineInput(1))
                ACLoad(iAC).ScaleX = CDbl(LineInput(1))
                ACLoad(iAC).ScaleY = CDbl(LineInput(1))
                ACLoad(iAC).NSlabs = CInt(LineInput(1))

                ReDim ACLoad(iAC).XGrid(ACLoad(iAC).NSlabs), ACLoad(iAC).YGrid(ACLoad(iAC).NSlabs)
                For iSlab = 1 To ACLoad(iAC).NSlabs
                    For Each m In reTwoNumPara.Matches(LineInput(1))
                        ACLoad(iAC).XGrid(iSlab) = CDbl(m.Groups("para1").Value)
                        ACLoad(iAC).YGrid(iSlab) = CDbl(m.Groups("para2").Value)
                    Next
                Next iSlab

                For Each m In reTwoNumPara.Matches(LineInput(1))
                    ACLoad(iAC).f1 = CDbl(m.Groups("para1").Value)
                    ACLoad(iAC).f2 = CDbl(m.Groups("para2").Value)
                Next

                ACLoad(iAC).Alpha = CDbl(LineInput(1))

                For Each m In reThreeNumPara.Matches(LineInput(1))
                    ACLoad(iAC).EqStifX = CDbl(m.Groups("para1").Value)
                    ACLoad(iAC).EqStifY = CDbl(m.Groups("para2").Value)
                    ACLoad(iAC).KeyEqStif = CInt(m.Groups("para3").Value)
                Next

                For Each m In reThreeNumPara.Matches(LineInput(1))
                    ACLoad(iAC).EqStif12X = CDbl(m.Groups("para1").Value)
                    ACLoad(iAC).EqStif12Y = CDbl(m.Groups("para2").Value)
                    ACLoad(iAC).KeyEqStif12 = CInt(m.Groups("para3").Value)
                Next

                For Each m In reThreeNumPara.Matches(LineInput(1))
                    ACLoad(iAC).EqEdgStifX = CDbl(m.Groups("para1").Value)
                    ACLoad(iAC).EqEdgStifY = CDbl(m.Groups("para2").Value)
                    ACLoad(iAC).KeyEqEdgStif = CInt(m.Groups("para3").Value)
                Next

                ACLoad(iAC).IndxIm = CInt(LineInput(1))
                ACLoad(iAC).IndxFn = CInt(LineInput(1))
                ACLoad(iAC).ThkSlb = CDbl(LineInput(1))
                ACLoad(iAC).IndxOvrl = CInt(LineInput(1))

                If ACLoad(iAC).IndxOvrl = 1 Then ACLoad(iAC).ThkOvr = CDbl(LineInput(1))

                If itpro = 1 Then
                    For Each m In reThreeNumPara.Matches(LineInput(1))
                        ACLoad(iAC).LETG = CDbl(m.Groups("para1").Value)
                        ACLoad(iAC).CurlShapara = CInt(m.Groups("para2").Value)
                        ACLoad(iAC).Cte = CDbl(m.Groups("para3").Value)
                    Next
                End If

            Next iAC

        Catch ex As Exception
            MsgBox("Error " & ex.Message & vbCr & "Occured reading the .loa file in ReadLoaFile.", MsgBoxStyle.OkOnly, "File Error")
            Exit Sub
        End Try

        FileClose(1)

    End Sub

    Private Sub NodeGeneration()

        Call NodebyPart()

        Call NodeMerge()

        'Call NodeBC()

    End Sub

    Private Sub NodebyPart()
        ' assume no sliding interface inside each part 

        Dim NNodeX, NNodeY, NNodeZ As Integer

        Dim iPart As Integer


        '******************************************************************
        '***********to get the total node and brick element number before merge**************
        NNode = 0 : NBrckEle = 0
        For iPart = 1 To NPart
            With PART(iPart)
                If .NXCtr > 1 Then
                    NNodeX = .NdXCtr(.NXCtr) - .NdXCtr(1) + 1
                Else
                    NNodeX = 1
                End If

                If .NYCtr > 1 Then
                    NNodeY = .NdYCtr(.NYCtr) - .NdYCtr(1) + 1
                Else
                    NNodeY = 1
                End If

                If .NZCtr > 1 Then
                    NNodeZ = .NdZCtr(.NZCtr) - .NdZCtr(1) + 1
                Else
                    NNodeZ = 1
                End If

                NNode = NNode + NNodeX * NNodeY * NNodeZ
                NBrckEle = NBrckEle + (NNodeX - 1) * (NNodeY - 1) * (NNodeZ - 1)
            End With
        Next
        '***********get the total node number and brick element before merge END**************
        '******************************************************************

        ReDim Node(NNode)

        Dim iNode As Integer

        Dim iXCtr, iYCtr, iZCtr As Integer
        Dim iNdX1, iNdy1, iNdz1 As Integer
        Dim iNdX2, iNdY2, iNdZ2 As Integer
        Dim iNdX, iNdY, iNdZ As Integer

        Dim XStart, YStart, ZStart As Double
        Dim XEnd, YEnd, ZEnd As Double
        Dim XMesh0, YMesh0, ZMesh0 As Double

        Dim XND, YND, ZND As Double

        Dim iNodePart As Integer

        Dim iPartMat, IdxMatTemp As Integer

        Dim iPartSld, IdxSldTemp As Integer, TypSldSmTemp As String, IsPartSld As Boolean
        Dim NdSldX1Temp, NdSldY1Temp, NdSldZ1Temp, NdSldX2Temp, NdSldY2Temp, NdSldZ2Temp As Integer
        Dim SldX1Temp, SldY1Temp, SldZ1Temp, SldX2Temp, SldY2Temp, SldZ2Temp As Double

        Dim iPartBC, BXYZTemp, BRXYZTemp As Integer, IsPartBC As Boolean
        Dim NdBCX1Temp, NdBCY1Temp, NdBCZ1Temp, NdBCX2Temp, NdBCY2Temp, NdBCZ2Temp As Integer
        Dim BCX1Temp, BCY1Temp, BCZ1Temp, BCX2Temp, BCY2Temp, BCZ2Temp As Double

        iNode = 0
        ZMin = PART(1).CoordZCtr(1) : ZMax = PART(1).CoordZCtr(1)
        For iPart = 1 To NPart

            With PART(iPart)

                If ZMin > Math.Min(.CoordZCtr(1), .CoordZCtr(.NZCtr)) Then  'Minimum Z to define infinte element BC 
                    ZMin = Math.Min(.CoordZCtr(1), .CoordZCtr(.NZCtr))
                End If

                If ZMax < Math.Max(.CoordZCtr(1), .CoordZCtr(.NZCtr)) Then  'Maximum Z to define Nodal Load 
                    ZMax = Math.Max(.CoordZCtr(1), .CoordZCtr(.NZCtr))
                End If

                iXCtr = 0 : iYCtr = 0 : iZCtr = 0

iXCtrloop:      iXCtr = iXCtr + 1
                If .NXCtr > 1 Then
                    Do While iXCtr <= .NXCtr - 1
                        iNdX1 = .NdXCtr(iXCtr)
                        iNdX2 = .NdXCtr(iXCtr + 1)
                        XStart = .CoordXCtr(iXCtr)
                        XEnd = .CoordXCtr(iXCtr + 1)
                        XMesh0 = (XEnd - XStart) / (iNdX2 - iNdX1)
                        GoTo iYCtrloop
                    Loop
                Else
                    iNdX1 = 1
                    iNdX2 = 1
                    XStart = .CoordXCtr(1)
                    XEnd = .CoordXCtr(1)
                    XMesh0 = 0
                End If


iYCtrloop:      iYCtr = iYCtr + 1
                If .NYCtr > 1 Then
                    Do While iYCtr <= .NYCtr - 1
                        iNdy1 = .NdYCtr(iYCtr)
                        iNdY2 = .NdYCtr(iYCtr + 1)
                        YStart = .CoordYCtr(iYCtr)
                        YEnd = .CoordYCtr(iYCtr + 1)
                        YMesh0 = (YEnd - YStart) / (iNdY2 - iNdy1)
                        GoTo iZCtrloop
                    Loop
                Else
                    iNdy1 = 1
                    iNdY2 = 1
                    YStart = .CoordYCtr(1)
                    YEnd = .CoordYCtr(1)
                    YMesh0 = 0
                End If

iZCtrloop:      iZCtr = iZCtr + 1
                If .NZCtr > 1 Then
                    Do While iZCtr <= .NZCtr - 1
                        iNdz1 = .NdZCtr(iZCtr)
                        iNdZ2 = .NdZCtr(iZCtr + 1)
                        ZStart = .CoordZCtr(iZCtr)
                        ZEnd = .CoordZCtr(iZCtr + 1)
                        ZMesh0 = (ZEnd - ZStart) / (iNdZ2 - iNdz1)
                        GoTo iPartMatlabel
                    Loop
                Else
                    iNdz1 = 1
                    iNdZ2 = 1
                    ZStart = .CoordZCtr(1)
                    ZEnd = .CoordZCtr(1)
                    ZMesh0 = 0
                End If

iPartMatlabel:
                IdxMatTemp = 0
                For iPartMat = 1 To NPartMat
                    If PARTMAT(iPartMat).IdxPart = iPart And _
                        iXCtr >= PARTMAT(iPartMat).MatNdIdxCtrIni(1) And iXCtr < PARTMAT(iPartMat).MatNdIdxCtrEnd(1) And _
                        iYCtr >= PARTMAT(iPartMat).MatNdIdxCtrIni(2) And iYCtr < PARTMAT(iPartMat).MatNdIdxCtrEnd(2) And _
                        iZCtr >= PARTMAT(iPartMat).MatNdIdxCtrIni(3) And iZCtr < PARTMAT(iPartMat).MatNdIdxCtrEnd(3) Then

                        IdxMatTemp = PARTMAT(iPartMat).MatIdx

                        'GoTo iNodeloop
                        GoTo iPartSldlabel
                    End If
                Next iPartMat


iPartSldlabel:
                IdxSldTemp = 0
                TypSldSmTemp = Nothing
                For iPartSld = 1 To NPartSld
                    If PARTSLD(iPartSld).IdxPart = iPart And _
                        iXCtr >= PARTSLD(iPartSld).SldNdIdxCtrIni(1) And iXCtr <= PARTSLD(iPartSld).SldNdIdxCtrEnd(1) And _
                        iYCtr >= PARTSLD(iPartSld).SldNdIdxCtrIni(2) And iYCtr <= PARTSLD(iPartSld).SldNdIdxCtrEnd(2) And _
                        iZCtr >= PARTSLD(iPartSld).SldNdIdxCtrIni(3) And iZCtr <= PARTSLD(iPartSld).SldNdIdxCtrEnd(3) Then

                        IsPartSld = True

                        IdxSldTemp = PARTSLD(iPartSld).SldIdx
                        TypSldSmTemp = PARTSLD(iPartSld).SldSmTyp

                        NdSldX1Temp = PARTSLD(iPartSld).SldNdIdxCtrIni(1)
                        NdSldY1Temp = PARTSLD(iPartSld).SldNdIdxCtrIni(2)
                        NdSldZ1Temp = PARTSLD(iPartSld).SldNdIdxCtrIni(3)
                        NdSldX2Temp = PARTSLD(iPartSld).SldNdIdxCtrEnd(1)
                        NdSldY2Temp = PARTSLD(iPartSld).SldNdIdxCtrEnd(2)
                        NdSldZ2Temp = PARTSLD(iPartSld).SldNdIdxCtrEnd(3)

                        SldX1Temp = PART(iPart).CoordXCtr(NdSldX1Temp)
                        SldX2Temp = PART(iPart).CoordXCtr(NdSldX2Temp)
                        SldY1Temp = PART(iPart).CoordYCtr(NdSldY1Temp)
                        SldY2Temp = PART(iPart).CoordYCtr(NdSldY2Temp)
                        SldZ1Temp = PART(iPart).CoordZCtr(NdSldZ1Temp)
                        SldZ2Temp = PART(iPart).CoordZCtr(NdSldZ2Temp)

                        GoTo iPartBClabel
                    End If
                Next iPartSld


iPartBClabel:
                BXYZTemp = 0
                BRXYZTemp = 0
                For iPartBC = 1 To NPartBC
                    If PARTBC(iPartBC).IdxPart = iPart And _
                         iXCtr >= Math.Max(PARTBC(iPartBC).BCNdIdxCtrIni(1), 1) And iXCtr <= Math.Max(PARTBC(iPartBC).BCNdIdxCtrEnd(1), 1) And _
                         iYCtr >= Math.Max(PARTBC(iPartBC).BCNdIdxCtrIni(2), 1) And iYCtr <= Math.Max(PARTBC(iPartBC).BCNdIdxCtrEnd(2), 1) And _
                         iZCtr >= Math.Max(PARTBC(iPartBC).BCNdIdxCtrIni(3), 1) And iZCtr <= Math.Max(PARTBC(iPartBC).BCNdIdxCtrEnd(3), 1) Then ' Math.Max function to change 0 to 1

                        IsPartBC = True


                        ' YC 101215-030719
                        'BXYZTemp = CInt(PARTBC(iPartBC).StrBCValue.Substring(0, 1)) * CInt(2 ^ 2) _
                        '         + CInt(PARTBC(iPartBC).StrBCValue.Substring(1, 1)) * CInt(2 ^ 1) _
                        '         + CInt(PARTBC(iPartBC).StrBCValue.Substring(2, 1)) * CInt(2 ^ 0)
                        'BRXYZTemp = CInt(PARTBC(iPartBC).StrBCValue.Substring(3, 1)) * CInt(2 ^ 2) _
                        '          + CInt(PARTBC(iPartBC).StrBCValue.Substring(4, 1)) * CInt(2 ^ 1) _
                        '          + CInt(PARTBC(iPartBC).StrBCValue.Substring(5, 1)) * CInt(2 ^ 0)
                        Select Case PARTBC(iPartBC).StrBCValue.Substring(0, 3)
                            Case "000" : BXYZTemp = 0
                            Case "100" : BXYZTemp = 1
                            Case "010" : BXYZTemp = 2
                            Case "001" : BXYZTemp = 3
                            Case "110" : BXYZTemp = 4
                            Case "011" : BXYZTemp = 5
                            Case "101" : BXYZTemp = 6
                            Case "111" : BXYZTemp = 7
                        End Select

                        Select Case PARTBC(iPartBC).StrBCValue.Substring(3, 3)
                            Case "000" : BRXYZTemp = 0
                            Case "100" : BRXYZTemp = 1
                            Case "010" : BRXYZTemp = 2
                            Case "001" : BRXYZTemp = 3
                            Case "110" : BRXYZTemp = 4
                            Case "011" : BRXYZTemp = 5
                            Case "101" : BRXYZTemp = 6
                            Case "111" : BRXYZTemp = 7
                        End Select
                        ' YC 101215-030719 END


                        NdBCX1Temp = Math.Max(PARTBC(iPartBC).BCNdIdxCtrIni(1), 1)      ' Math.Max function to change 0 to 1
                        NdBCY1Temp = Math.Max(PARTBC(iPartBC).BCNdIdxCtrIni(2), 1)
                        NdBCZ1Temp = Math.Max(PARTBC(iPartBC).BCNdIdxCtrIni(3), 1)
                        NdBCX2Temp = Math.Max(PARTBC(iPartBC).BCNdIdxCtrEnd(1), 1)
                        NdBCY2Temp = Math.Max(PARTBC(iPartBC).BCNdIdxCtrEnd(2), 1)
                        NdBCZ2Temp = Math.Max(PARTBC(iPartBC).BCNdIdxCtrEnd(3), 1)

                        BCX1Temp = PART(iPart).CoordXCtr(NdBCX1Temp)
                        BCX2Temp = PART(iPart).CoordXCtr(NdBCX2Temp)
                        BCY1Temp = PART(iPart).CoordYCtr(NdBCY1Temp)
                        BCY2Temp = PART(iPart).CoordYCtr(NdBCY2Temp)
                        BCZ1Temp = PART(iPart).CoordZCtr(NdBCZ1Temp)
                        BCZ2Temp = PART(iPart).CoordZCtr(NdBCZ2Temp)

                        GoTo iNodeloop
                    End If
                Next iPartBC


iNodeloop:
                For iNdX = iNdX1 To iNdX2
                    XND = (XStart + (iNdX - iNdX1) * XMesh0) * ACLoad(1).ScaleX
                    For iNdY = iNdy1 To iNdY2
                        YND = (YStart + (iNdY - iNdy1) * YMesh0) * ACLoad(1).ScaleY
                        For iNdZ = iNdz1 To iNdZ2
                            ZND = (ZStart + (iNdZ - iNdz1) * ZMesh0)

                            If iNode >= 1 Then
                                For ii = iNodePart + 1 To iNode
                                    If Math.Abs(XND - Node(ii).X) < ErrTol * ACLoad(1).ScaleX And _
                                        Math.Abs(YND - Node(ii).Y) < ErrTol * ACLoad(1).ScaleY And _
                                        Math.Abs(ZND - Node(ii).Z) < ErrTol Then

                                        If Math.Abs(XND - Math.Min(XStart * ACLoad(1).ScaleX, XEnd * ACLoad(1).ScaleX)) < ErrTol * ACLoad(1).ScaleX Then
                                            Node(ii).XPlusMesh = Math.Abs(XMesh0) * ACLoad(1).ScaleX
                                        ElseIf Math.Abs(XND - Math.Max(XStart * ACLoad(1).ScaleX, XEnd * ACLoad(1).ScaleX)) < ErrTol * ACLoad(1).ScaleX Then
                                            Node(ii).XMinusMesh = Math.Abs(XMesh0) * ACLoad(1).ScaleX
                                        End If

                                        If Math.Abs(YND - Math.Min(YStart * ACLoad(1).ScaleY, YEnd * ACLoad(1).ScaleY)) < ErrTol * ACLoad(1).ScaleY Then
                                            Node(ii).YPlusMesh = Math.Abs(YMesh0) * ACLoad(1).ScaleY
                                        ElseIf Math.Abs(YND - Math.Max(YStart * ACLoad(1).ScaleY, YEnd * ACLoad(1).ScaleY)) < ErrTol * ACLoad(1).ScaleY Then
                                            Node(ii).YMinusMesh = Math.Abs(YMesh0) * ACLoad(1).ScaleY
                                        End If

                                        If Math.Abs(ZND - Math.Min(ZStart, ZEnd)) < ErrTol Then
                                            Node(ii).ZPlusMesh = Math.Abs(ZMesh0)
                                        ElseIf Math.Abs(ZND - Math.Max(ZStart, ZEnd)) < ErrTol Then
                                            Node(ii).ZMinusMesh = Math.Abs(ZMesh0)
                                        End If

                                        GoTo nextNodelabel
                                    End If
                                Next ii
                            End If

                            iNode = iNode + 1

                            Node(iNode).X = XND : Node(iNode).Y = YND : Node(iNode).Z = ZND

                            'Node Sliding 
                            If IsPartSld Then
                                If XND > (Math.Min(SldX1Temp, SldX2Temp) - ErrTol) * ACLoad(1).ScaleX And XND < (Math.Max(SldX1Temp, SldX2Temp) + ErrTol) * ACLoad(1).ScaleX And
                                   YND > (Math.Min(SldY1Temp, SldY2Temp) - ErrTol) * ACLoad(1).ScaleY And YND < (Math.Max(SldY1Temp, SldY2Temp) + ErrTol) * ACLoad(1).ScaleY And
                                   ZND > (Math.Min(SldZ1Temp, SldZ2Temp) - ErrTol) And ZND < (Math.Max(SldZ1Temp, SldZ2Temp) + ErrTol) Then

                                    Node(iNode).IdxSld = IdxSldTemp
                                    Node(iNode).TypSldSm = TypSldSmTemp

                                End If
                            End If
                            'Node Sliding END

                            'Node BC
                            'initializating
                            Node(iNode).BXYZ = 0
                            Node(iNode).BRXYZ = 7   'default non-rotational node 

                            If ACLoad(1).IndxSy = 1 Then    'symmetric to y-axial 
                                If Math.Abs(YND - 0) < ErrTol Then
                                    Node(iNode).BXYZ = 2
                                End If
                            End If
                            'initializating

                            If IsPartBC Then    'will override initial BC
                                If XND > (Math.Min(BCX1Temp, BCX2Temp) - ErrTol) * ACLoad(1).ScaleX And XND < (Math.Max(BCX1Temp, BCX2Temp) + ErrTol) * ACLoad(1).ScaleX And
                                   YND > (Math.Min(BCY1Temp, BCY2Temp) - ErrTol) * ACLoad(1).ScaleY And YND < (Math.Max(BCY1Temp, BCY2Temp) + ErrTol) * ACLoad(1).ScaleY And
                                   ZND > (Math.Min(BCZ1Temp, BCZ2Temp) - ErrTol) And ZND < (Math.Max(BCZ1Temp, BCZ2Temp) + ErrTol) Then

                                    Node(iNode).BXYZ = BXYZTemp
                                    Node(iNode).BRXYZ = BRXYZTemp

                                End If
                            End If
                            'Node BC END


                            If Math.Abs(XND - Math.Min(XStart * ACLoad(1).ScaleX, XEnd * ACLoad(1).ScaleX)) < ErrTol * ACLoad(1).ScaleX Then
                                Node(iNode).XPlusMesh = Math.Abs(XMesh0) * ACLoad(1).ScaleX
                            ElseIf Math.Abs(XND - Math.Max(XStart * ACLoad(1).ScaleX, XEnd * ACLoad(1).ScaleX)) < ErrTol * ACLoad(1).ScaleX Then
                                Node(iNode).XMinusMesh = Math.Abs(XMesh0) * ACLoad(1).ScaleX
                            Else
                                Node(iNode).XPlusMesh = Math.Abs(XMesh0) * ACLoad(1).ScaleX
                                Node(iNode).XMinusMesh = Math.Abs(XMesh0) * ACLoad(1).ScaleX
                            End If

                            If Math.Abs(YND - Math.Min(YStart * ACLoad(1).ScaleY, YEnd * ACLoad(1).ScaleY)) < ErrTol * ACLoad(1).ScaleY Then
                                Node(iNode).YPlusMesh = Math.Abs(YMesh0) * ACLoad(1).ScaleY
                            ElseIf Math.Abs(YND - Math.Max(YStart * ACLoad(1).ScaleY, YEnd * ACLoad(1).ScaleY)) < ErrTol * ACLoad(1).ScaleY Then
                                Node(iNode).YMinusMesh = Math.Abs(YMesh0) * ACLoad(1).ScaleY
                            Else
                                Node(iNode).YPlusMesh = Math.Abs(YMesh0) * ACLoad(1).ScaleY
                                Node(iNode).YMinusMesh = Math.Abs(YMesh0) * ACLoad(1).ScaleY
                            End If

                            If Math.Abs(ZND - Math.Min(ZStart, ZEnd)) < ErrTol Then
                                Node(iNode).ZPlusMesh = Math.Abs(ZMesh0)
                            ElseIf Math.Abs(ZND - Math.Max(ZStart, ZEnd)) < ErrTol Then
                                Node(iNode).ZMinusMesh = Math.Abs(ZMesh0)
                            Else
                                Node(iNode).ZPlusMesh = Math.Abs(ZMesh0)
                                Node(iNode).ZMinusMesh = Math.Abs(ZMesh0)
                            End If

                            ReDim Node(iNode).IdxPart(2), Node(iNode).IdxMat(2)

                            Node(iNode).IdxPart(1) = iPart : Node(iNode).IdxPart(2) = iPart
                            Node(iNode).IdxMat(1) = IdxMatTemp : Node(iNode).IdxMat(2) = IdxMatTemp

nextNodelabel:
                        Next iNdZ
                    Next iNdY
                Next iNdX


                If iZCtr + 1 <= .NZCtr - 1 Then
                    GoTo iZCtrloop
                End If

                If iYCtr + 1 <= .NYCtr - 1 Then
                    iZCtr = 0
                    GoTo iYCtrloop
                End If

                If iXCtr + 1 <= .NXCtr - 1 Then
                    iZCtr = 0
                    iYCtr = 0
                    GoTo iXCtrloop
                End If

            End With

            iNodePart = iNode

        Next

        If iNodePart <> NNode Then
            MsgBox("The number of node before merge is not consistent!" & vbCrLf, MsgBoxStyle.OkOnly, "Error")
            Exit Sub
        End If

        'infinite element BC
        For iNode = 1 To NNode
            If Math.Abs(Node(iNode).Z - ZMin) < ErrTol Then
                Node(iNode).BXYZ = 7
            End If
        Next iNode

    End Sub

    Private Sub NodeMerge()
        ' assume sliding only along z (overlay/slab/foundation) 

        Dim iNode As Integer


        'Dim iPart As Integer
        'Dim iPartSld As Integer
        'Dim NdSldX1Temp, NdSldY1Temp, NdSldZ1Temp, NdSldX2Temp, NdSldY2Temp, NdSldZ2Temp As Integer
        'Dim X1Temp, Y1Temp, Z1Temp, X2Temp, Y2Temp, Z2Temp As double

        'For iPartSld = 1 To NPartSld
        '    iPart = PARTSLD(iPartSld).IdxPart

        '    NdSldX1Temp = PARTSLD(iPartSld).SldNdIdxCtrIni(1)
        '    NdSldY1Temp = PARTSLD(iPartSld).SldNdIdxCtrIni(2)
        '    NdSldZ1Temp = PARTSLD(iPartSld).SldNdIdxCtrIni(3)
        '    NdSldX2Temp = PARTSLD(iPartSld).SldNdIdxCtrEnd(1)
        '    NdSldY2Temp = PARTSLD(iPartSld).SldNdIdxCtrEnd(2)
        '    NdSldZ2Temp = PARTSLD(iPartSld).SldNdIdxCtrEnd(3)

        '    X1Temp = PART(iPart).CoordXCtr(NdSldX1Temp)
        '    X2Temp = PART(iPart).CoordXCtr(NdSldX2Temp)
        '    Y1Temp = PART(iPart).CoordYCtr(NdSldY1Temp)
        '    Y2Temp = PART(iPart).CoordYCtr(NdSldY2Temp)
        '    Z1Temp = PART(iPart).CoordZCtr(NdSldZ1Temp)
        '    Z2Temp = PART(iPart).CoordZCtr(NdSldZ2Temp)


        '    For iNode = 1 To NNode
        '        If Node(iNode).X > (Math.Min(X1Temp, X2Temp) - ErrTol) * ACLoad(1).ScaleX And Node(iNode).X < (Math.Max(X1Temp, X2Temp) + ErrTol) * ACLoad(1).ScaleX And
        '            Node(iNode).Y > (Math.Min(Y1Temp, Y2Temp) - ErrTol) * ACLoad(1).ScaleY And Node(iNode).Y < (Math.Max(Y1Temp, Y2Temp) + ErrTol) * ACLoad(1).ScaleY And
        '            Node(iNode).Z > (Math.Min(Z1Temp, Z2Temp) - ErrTol) And Node(iNode).Z < (Math.Max(Z1Temp, Z2Temp) + ErrTol) Then

        '            Node(iNode).IdxSld = PARTSLD(iPartSld).SldIdx
        '            Node(iNode).TypSldSm = PARTSLD(iPartSld).SldSmTyp

        '        End If
        '    Next

        'Next iPartSld



        ReDim Nd(NNode)
        Dim iNd As Integer = 0

        For iNode = 1 To NNode

            If iNode >= 2 Then
                For ii = 1 To iNd
                    If Math.Abs(Nd(ii).X - Node(iNode).X) < ErrTol * ACLoad(1).ScaleX And _
                        Math.Abs(Nd(ii).Y - Node(iNode).Y) < ErrTol * ACLoad(1).ScaleY And _
                        Math.Abs(Nd(ii).Z - Node(iNode).Z) < ErrTol And _
                        Nd(ii).IdxSld = 0 And Node(iNode).IdxSld = 0 Then

                        Nd(ii).XPlusMesh = Math.Max(Nd(ii).XPlusMesh, Node(iNode).XPlusMesh)
                        Nd(ii).XMinusMesh = Math.Max(Nd(ii).XMinusMesh, Node(iNode).XMinusMesh)

                        Nd(ii).YPlusMesh = Math.Max(Nd(ii).YPlusMesh, Node(iNode).YPlusMesh)
                        Nd(ii).YMinusMesh = Math.Max(Nd(ii).YMinusMesh, Node(iNode).YMinusMesh)

                        Nd(ii).ZPlusMesh = Math.Max(Nd(ii).ZPlusMesh, Node(iNode).ZPlusMesh)
                        Nd(ii).ZMinusMesh = Math.Max(Nd(ii).ZMinusMesh, Node(iNode).ZMinusMesh)

                        Nd(ii).IdxPart(2) = Node(iNode).IdxPart(2)
                        Nd(ii).IdxMat(2) = Node(iNode).IdxMat(2)

                        GoTo nextiNodeLabel

                    End If
                Next ii


            End If

            iNd = iNd + 1
            Nd(iNd) = Node(iNode)

nextiNodeLabel:
        Next iNode

        NNd = iNd

        ReDim Preserve Nd(NNd)  ' v3.0 003/YC 092820

    End Sub

    Private Sub NodeBC()

        Dim iNd As Integer

        For iNd = 1 To NNd
            Nd(iNd).BRXYZ = 7       'non-rotatioal node
            Nd(iNd).BXYZ = 0        'initializating
        Next iNd

        If ACLoad(1).IndxSy = 1 Then    'symmetric to y-axial 
            For iNd = 1 To NNd
                If Math.Abs(Nd(iNd).Y - 0) < ErrTol Then
                    Nd(iNd).BXYZ = 2
                End If
            Next iNd
        End If

        For iNd = 1 To NNd      'infinite element BC 
            If Math.Abs(Nd(iNd).Z - ZMin) < ErrTol Then
                Nd(iNd).BXYZ = 7
            End If
        Next iNd


        Dim iPartBc As Integer
        Dim iPart As Integer

        Dim NdBCX1Temp, NdBCY1Temp, NdBCZ1Temp, NdBCX2Temp, NdBCY2Temp, NdBCZ2Temp As Integer
        Dim X1Temp, Y1Temp, Z1Temp, X2Temp, Y2Temp, Z2Temp As Double
        Dim BXYZtemp, BRXYZtemp As Integer

        For iPartBc = 1 To NPartBC


            iPart = PARTBC(iPartBc).IdxPart

            NdBCX1Temp = PARTBC(iPartBc).BCNdIdxCtrIni(1)
            NdBCY1Temp = PARTBC(iPartBc).BCNdIdxCtrIni(2)
            NdBCZ1Temp = PARTBC(iPartBc).BCNdIdxCtrIni(3)
            NdBCX2Temp = PARTBC(iPartBc).BCNdIdxCtrEnd(1)
            NdBCY2Temp = PARTBC(iPartBc).BCNdIdxCtrEnd(2)
            NdBCZ2Temp = PARTBC(iPartBc).BCNdIdxCtrEnd(3)

            If NdBCX1Temp = 0 Then NdBCX1Temp = 1
            If NdBCY1Temp = 0 Then NdBCY1Temp = 1
            If NdBCZ1Temp = 0 Then NdBCZ1Temp = 1
            If NdBCX2Temp = 0 Then NdBCX2Temp = 1
            If NdBCY2Temp = 0 Then NdBCY2Temp = 1
            If NdBCZ2Temp = 0 Then NdBCZ2Temp = 1

            X1Temp = PART(iPart).CoordXCtr(NdBCX1Temp)
            X2Temp = PART(iPart).CoordXCtr(NdBCX2Temp)
            Y1Temp = PART(iPart).CoordYCtr(NdBCY1Temp)
            Y2Temp = PART(iPart).CoordYCtr(NdBCY2Temp)
            Z1Temp = PART(iPart).CoordZCtr(NdBCZ1Temp)
            Z2Temp = PART(iPart).CoordZCtr(NdBCZ2Temp)


            For iNd = 1 To NNd
                If Nd(iNd).X > (Math.Min(X1Temp, X2Temp) - ErrTol) * ACLoad(1).ScaleX And Nd(iNd).X < (Math.Max(X1Temp, X2Temp) + ErrTol) * ACLoad(1).ScaleX And
                    Nd(iNd).Y > (Math.Min(Y1Temp, Y2Temp) - ErrTol) * ACLoad(1).ScaleY And Nd(iNd).Y < (Math.Max(Y1Temp, Y2Temp) + ErrTol) * ACLoad(1).ScaleY And
                    Nd(iNd).Z > (Math.Min(Z1Temp, Z2Temp) - ErrTol) And Nd(iNd).Z < (Math.Max(Z1Temp, Z2Temp) + ErrTol) Then

                    BXYZtemp = CInt(PARTBC(iPartBc).StrBCValue.Substring(0, 1)) * CInt(2 ^ 2) _
                                    + CInt(PARTBC(iPartBc).StrBCValue.Substring(1, 1)) * CInt(2 ^ 1) _
                                    + CInt(PARTBC(iPartBc).StrBCValue.Substring(2, 1)) * CInt(2 ^ 0)
                    BRXYZtemp = CInt(PARTBC(iPartBc).StrBCValue.Substring(3, 1)) * CInt(2 ^ 2) _
                                    + CInt(PARTBC(iPartBc).StrBCValue.Substring(4, 1)) * CInt(2 ^ 1) _
                                    + CInt(PARTBC(iPartBc).StrBCValue.Substring(5, 1)) * CInt(2 ^ 0)

                    Nd(iNd).BXYZ = Math.Max(Nd(iNd).BXYZ, BXYZtemp)
                    Nd(iNd).BRXYZ = Math.Max(Nd(iNd).BRXYZ, BRXYZtemp)
                End If
            Next


        Next iPartBc


    End Sub

    Private Sub BrickElementGeneration()

        ReDim BrickElement(NBrckEle)

        Dim iBrckEle, iPart, iNd, iPartMat As Integer

        Dim XNdStart, YNdStart, ZNdStart As Double
        Dim XMesh0, YMesh0, ZMesh0 As Double

        Dim XND, YND, ZND As Double
        Dim XEleIP1, YEleIP1, ZEleIP1 As Double

        Dim XNDEle, YNDEle, ZNDEle As Double
        Dim IP, NdEle(8) As Integer

        Dim iXCtr, iYCtr, iZCtr As Integer
        Dim iNdX1, iNdY1, iNdZ1 As Integer
        Dim iNdX2, iNdY2, iNdZ2 As Integer
        Dim iNdX, iNdY, iNdZ As Integer



        Dim IdxMatTemp As Integer

        iBrckEle = 0
        For iPart = 1 To NPart

            With PART(iPart)

                iXCtr = 0 : iYCtr = 0 : iZCtr = 0

iXCtrloop:      iXCtr = iXCtr + 1
                If .NXCtr > 1 Then
                    Do While iXCtr <= .NXCtr - 1
                        iNdX1 = .NdXCtr(iXCtr)
                        iNdX2 = .NdXCtr(iXCtr + 1)
                        XNdStart = .CoordXCtr(iXCtr)
                        XMesh0 = (.CoordXCtr(iXCtr + 1) - .CoordXCtr(iXCtr)) / (iNdX2 - iNdX1)
                        GoTo iYCtrloop
                    Loop
                Else
                    GoTo NextPartLabel
                End If


iYCtrloop:      iYCtr = iYCtr + 1
                If .NYCtr > 1 Then
                    Do While iYCtr <= .NYCtr - 1
                        iNdY1 = .NdYCtr(iYCtr)
                        iNdY2 = .NdYCtr(iYCtr + 1)
                        YNdStart = .CoordYCtr(iYCtr)
                        YMesh0 = (.CoordYCtr(iYCtr + 1) - .CoordYCtr(iYCtr)) / (iNdY2 - iNdY1)
                        GoTo iZCtrloop
                    Loop
                Else
                    GoTo NextPartLabel
                End If

iZCtrloop:      iZCtr = iZCtr + 1
                If .NZCtr > 1 Then
                    Do While iZCtr <= .NZCtr - 1
                        iNdZ1 = .NdZCtr(iZCtr)
                        iNdZ2 = .NdZCtr(iZCtr + 1)
                        ZNdStart = .CoordZCtr(iZCtr)
                        ZMesh0 = (.CoordZCtr(iZCtr + 1) - .CoordZCtr(iZCtr)) / (iNdZ2 - iNdZ1)
                        GoTo iPartMatloop
                    Loop
                Else
                    GoTo NextPartLabel
                End If

iPartMatloop:
                IdxMatTemp = 0
                For iPartMat = 1 To NPartMat
                    If PARTMAT(iPartMat).IdxPart = iPart And _
                        iXCtr >= PARTMAT(iPartMat).MatNdIdxCtrIni(1) And iXCtr < PARTMAT(iPartMat).MatNdIdxCtrEnd(1) And _
                        iYCtr >= PARTMAT(iPartMat).MatNdIdxCtrIni(2) And iYCtr < PARTMAT(iPartMat).MatNdIdxCtrEnd(2) And _
                        iZCtr >= PARTMAT(iPartMat).MatNdIdxCtrIni(3) And iZCtr < PARTMAT(iPartMat).MatNdIdxCtrEnd(3) Then

                        IdxMatTemp = PARTMAT(iPartMat).MatIdx
                        GoTo iBrckEleloop
                    End If
                Next iPartMat

iBrckEleloop:
                For iNdZ = iNdZ1 To iNdZ2 - 1
                    ZND = (ZNdStart + (iNdZ - iNdZ1) * ZMesh0)
                    For iNdY = iNdY1 To iNdY2 - 1
                        YND = (YNdStart + (iNdY - iNdY1) * YMesh0) * ACLoad(1).ScaleY
                        For iNdX = iNdX1 To iNdX2 - 1
                            XND = (XNdStart + (iNdX - iNdX1) * XMesh0) * ACLoad(1).ScaleX

                            XEleIP1 = Math.Min(XND, XND + XMesh0 * ACLoad(1).ScaleX)
                            YEleIP1 = Math.Min(YND, YND + YMesh0 * ACLoad(1).ScaleY)
                            ZEleIP1 = Math.Min(ZND, ZND + ZMesh0)

                            IP = 1
IPloop:                     Select Case IP
                                Case 1 : XNDEle = XEleIP1 : YNDEle = YEleIP1 : ZNDEle = ZEleIP1
                                Case 2 : XNDEle = XEleIP1 + Math.Abs(XMesh0) * ACLoad(1).ScaleX : YNDEle = YEleIP1 : ZNDEle = ZEleIP1
                                Case 3 : XNDEle = XEleIP1 + Math.Abs(XMesh0) * ACLoad(1).ScaleX : YNDEle = YEleIP1 + Math.Abs(YMesh0) * ACLoad(1).ScaleY : ZNDEle = ZEleIP1
                                Case 4 : XNDEle = XEleIP1 : YNDEle = YEleIP1 + Math.Abs(YMesh0) * ACLoad(1).ScaleY : ZNDEle = ZEleIP1
                                Case 5 : XNDEle = XEleIP1 : YNDEle = YEleIP1 : ZNDEle = ZEleIP1 + Math.Abs(ZMesh0)
                                Case 6 : XNDEle = XEleIP1 + Math.Abs(XMesh0) * ACLoad(1).ScaleX : YNDEle = YEleIP1 : ZNDEle = ZEleIP1 + Math.Abs(ZMesh0)
                                Case 7 : XNDEle = XEleIP1 + Math.Abs(XMesh0) * ACLoad(1).ScaleX : YNDEle = YEleIP1 + Math.Abs(YMesh0) * ACLoad(1).ScaleY : ZNDEle = ZEleIP1 + Math.Abs(ZMesh0)
                                Case 8 : XNDEle = XEleIP1 : YNDEle = YEleIP1 + Math.Abs(YMesh0) * ACLoad(1).ScaleY : ZNDEle = ZEleIP1 + Math.Abs(ZMesh0)
                            End Select

                            For iNd = 1 To NNd
                                If (iPart = Nd(iNd).IdxPart(1) Or iPart = Nd(iNd).IdxPart(2)) Then
                                    If Math.Abs(XNDEle - Nd(iNd).X) < ErrTol * ACLoad(1).ScaleX And _
                                                   Math.Abs(YNDEle - Nd(iNd).Y) < ErrTol * ACLoad(1).ScaleY And _
                                                   Math.Abs(ZNDEle - Nd(iNd).Z) < ErrTol Then

                                        NdEle(IP) = iNd
                                        If IP < 8 Then
                                            IP = IP + 1
                                            GoTo IPloop
                                        End If

                                        If IP = 8 Then
                                            iBrckEle = iBrckEle + 1

                                            BrickElement(iBrckEle).IdxMat = IdxMatTemp
                                            'For iPartMat = 1 To NPartMat

                                            '    If PARTMAT(iPartMat).IdxPart = iPart And _
                                            '        iXCtr >= PARTMAT(iPartMat).MatNdIdxCtrIni(1) And iXCtr <= PARTMAT(iPartMat).MatNdIdxCtrEnd(1) And _
                                            '        iYCtr >= PARTMAT(iPartMat).MatNdIdxCtrIni(2) And iYCtr <= PARTMAT(iPartMat).MatNdIdxCtrEnd(2) And _
                                            '        iZCtr >= PARTMAT(iPartMat).MatNdIdxCtrIni(3) And iZCtr <= PARTMAT(iPartMat).MatNdIdxCtrEnd(3) Then

                                            '        BrickElement(iBrckEle).IdxMat = PARTMAT(iPartMat).MatIdx

                                            '    End If

                                            'Next iPartMat

                                            ReDim BrickElement(iBrckEle).Node(8)
                                            For IP = 1 To 8
                                                BrickElement(iBrckEle).Node(IP) = NdEle(IP)
                                            Next IP

                                            GoTo nextElementlabel
                                        End If

                                    End If
                                End If

                            Next iNd

nextElementlabel:
                        Next iNdX
                    Next iNdY
                Next iNdZ



                If iZCtr + 1 <= .NZCtr - 1 Then
                    GoTo iZCtrloop
                End If

                If iYCtr + 1 <= .NYCtr - 1 Then
                    iZCtr = 0
                    GoTo iYCtrloop
                End If

                If iXCtr + 1 <= .NXCtr - 1 Then
                    iZCtr = 0
                    iYCtr = 0
                    GoTo iXCtrloop
                End If

            End With


NextPartLabel:
        Next iPart

        'If iNodePart <> NNode Then
        '    MsgBox("The number of node before merge is not consistent!" & vbCrLf, MsgBoxStyle.OkOnly, "Error")
        '    Exit Sub
        'End If

    End Sub

    Private Sub SpringElementGeneration()

        'ReDim SpringElement(1000), SpringType(100)     ' v3.0 YC 052620
        ReDim SpringElement(2000), SpringType(100)

        Dim iSpringElement, iSpringType As Integer
        Dim iNd, jNd As Integer

        iSpringElement = 0
        For iNd = 1 To NNd
            If Nd(iNd).IdxMat(1) = 1 Or Nd(iNd).IdxMat(1) = 12 Then     'limit spring to concrete (overlay) layer

                If Math.Abs(Nd(iNd).X - (-300) * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                   Math.Abs(Nd(iNd).X - 0 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                   Math.Abs(Nd(iNd).X - 300 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                   Math.Abs(Nd(iNd).X - 600 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                   Math.Abs(Nd(iNd).Y - (-450) * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                   Math.Abs(Nd(iNd).Y - (-150) * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                   Math.Abs(Nd(iNd).Y - 150 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                   Math.Abs(Nd(iNd).Y - 450 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Then

                    For jNd = iNd + 1 To NNd
                        If Math.Abs(Math.Abs(Nd(jNd).X - Nd(iNd).X) - 0.02 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX And _
                                Math.Abs(Nd(jNd).Y - Nd(iNd).Y) < ErrTol * ACLoad(1).ScaleY And _
                                Math.Abs(Nd(jNd).Z - Nd(iNd).Z) < ErrTol Then   ' x-Spring (y-joint)

                            If ACLoad(1).KeyEqStif = 1 Then    ' vetical spring to simulate joint in FEAFAA
                                If (Nd(jNd).IdxMat(1) = 1 And Math.Abs(Nd(jNd).Z) < ErrTol) Or
                                   (Nd(iNd).IdxMat(1) = 12 And ACLoad(1).IndxOvrl = 1 And Math.Abs(Nd(jNd).Z - ACLoad(1).ThkSlb) < ErrTol) Then
                                    iSpringElement = iSpringElement + 1
                                    SpringElement(iSpringElement).Node1 = iNd
                                    SpringElement(iSpringElement).Node2 = jNd
                                    SpringElement(iSpringElement).Direction = 3
                                    SpringElement(iSpringElement).Stiffness = (Nd(iNd).YPlusMesh + Nd(iNd).YMinusMesh) * ACLoad(1).EqStifX / 2
                                End If
                            End If


                            If ACLoad(1).KeyEqStif12 = 1 Then    ' horrizontal spring to simulate infinite slab in FF
                                iSpringElement = iSpringElement + 1
                                SpringElement(iSpringElement).Node1 = iNd
                                SpringElement(iSpringElement).Node2 = jNd
                                SpringElement(iSpringElement).Direction = 1
                                SpringElement(iSpringElement).Stiffness = (Nd(iNd).YPlusMesh + Nd(iNd).YMinusMesh) * ACLoad(1).EqStif12X / 2
                            End If
                        End If

                        If Math.Abs(Nd(jNd).X - Nd(iNd).X) < ErrTol * ACLoad(1).ScaleX And _
                            Math.Abs(Math.Abs(Nd(jNd).Y - Nd(iNd).Y) - 0.02 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY And _
                            Math.Abs(Nd(jNd).Z - Nd(iNd).Z) < ErrTol * ACLoad(1).ScaleX Then   ' y-Spring (x-joint)

                            If ACLoad(1).KeyEqStif = 1 Then    ' vetical spring to simulate joint in FEAFAA
                                If (Nd(jNd).IdxMat(1) = 1 And Math.Abs(Nd(jNd).Z) < ErrTol) Or
                                   (Nd(iNd).IdxMat(1) = 12 And ACLoad(1).IndxOvrl = 1 And Math.Abs(Nd(jNd).Z - ACLoad(1).ThkSlb) < ErrTol) Then
                                    iSpringElement = iSpringElement + 1
                                    SpringElement(iSpringElement).Node1 = iNd
                                    SpringElement(iSpringElement).Node2 = jNd
                                    SpringElement(iSpringElement).Direction = 3
                                    SpringElement(iSpringElement).Stiffness = (Nd(iNd).XPlusMesh + Nd(iNd).XMinusMesh) * ACLoad(1).EqStifY / 2
                                End If
                            End If

                            If ACLoad(1).KeyEqStif12 = 1 Then    ' horrizontal spring to simulate infinite slab in FF
                                iSpringElement = iSpringElement + 1
                                SpringElement(iSpringElement).Node1 = iNd
                                SpringElement(iSpringElement).Node2 = jNd
                                SpringElement(iSpringElement).Direction = 2
                                SpringElement(iSpringElement).Stiffness = (Nd(iNd).XPlusMesh + Nd(iNd).XMinusMesh) * ACLoad(1).EqStif12Y / 2
                            End If
                        End If

                        'If ACLoad(1).KeyEqEdgStif = 1 Then    ' horrizontal spring to fix slab rigid movement in FEAFAA (for computational stability)

                        '    If (Nd(iNd).IdxMat(1) = 1 And Math.Abs(Nd(iNd).Z - ACLoad(1).ThkSlb) < ErrTol And Math.Abs(Nd(jNd).Z) < ErrTol) Or
                        '       (Nd(iNd).IdxMat(1) = 12 And ACLoad(1).IndxOvrl = 1 And Math.Abs(Nd(iNd).Z - (ACLoad(1).ThkSlb + ACLoad(1).ThkOvr)) < ErrTol And Math.Abs(Nd(jNd).Z - ACLoad(1).ThkSlb) < ErrTol) Then

                        '        If (Math.Abs(Math.Abs(Nd(jNd).X - Nd(iNd).X) - 0.08 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                        '           Math.Abs(Math.Abs(Nd(jNd).X - Nd(iNd).X) - 0.1 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX) And _
                        '           (Math.Abs(Nd(jNd).Y - Nd(iNd).Y) < ErrTol * ACLoad(1).ScaleY Or
                        '            Math.Abs(Math.Abs(Nd(jNd).Y - Nd(iNd).Y) - 0.02 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY) Then   ' x-Spring (y-joint)
                        '            iSpringElement = iSpringElement + 1
                        '            SpringElement(iSpringElement).Node1 = iNd
                        '            SpringElement(iSpringElement).Node2 = jNd
                        '            SpringElement(iSpringElement).Direction = 1
                        '            SpringElement(iSpringElement).Stiffness = ACLoad(1).EqEdgStifX
                        '        End If

                        '        If (Math.Abs(Nd(jNd).X - Nd(iNd).X) < ErrTol * ACLoad(1).ScaleX Or
                        '            Math.Abs(Math.Abs(Nd(jNd).X - Nd(iNd).X) - 0.02 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX) And _
                        '           (Math.Abs(Math.Abs(Nd(jNd).Y - Nd(iNd).Y) - 0.1 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                        '            Math.Abs(Math.Abs(Nd(jNd).Y - Nd(iNd).Y) - 0.08 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY) Then   ' y-Spring (x-joint)
                        '            iSpringElement = iSpringElement + 1
                        '            SpringElement(iSpringElement).Node1 = iNd
                        '            SpringElement(iSpringElement).Node2 = jNd
                        '            SpringElement(iSpringElement).Direction = 2
                        '            SpringElement(iSpringElement).Stiffness = ACLoad(1).EqEdgStifX
                        '        End If

                        '    End If

                        'End If

                    Next jNd
                End If

            End If
        Next iNd



        If ACLoad(1).KeyEqEdgStif = 1 Then    ' horrizontal spring to fix slab rigid movement in FEAFAA (for computational stability)
            For iNd = 1 To NNd
                If Nd(iNd).IdxMat(1) = 1 Or Nd(iNd).IdxMat(1) = 12 Then

                    If (Math.Abs(Nd(iNd).X - (-300) * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                        Math.Abs(Nd(iNd).X - (-0.02) * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                        Math.Abs(Nd(iNd).X - 0 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                        Math.Abs(Nd(iNd).X - 300 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                        Math.Abs(Nd(iNd).X - 300.02 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                        Math.Abs(Nd(iNd).X - 600 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX) And
                        (Math.Abs(Nd(iNd).Y - (-450) * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                        Math.Abs(Nd(iNd).Y - (-150.02) * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                        Math.Abs(Nd(iNd).Y - (-150) * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                        Math.Abs(Nd(iNd).Y - 150 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                        Math.Abs(Nd(iNd).Y - 150.02 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                        Math.Abs(Nd(iNd).Y - 450 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY) Then

                        For jNd = iNd + 1 To NNd
                            If (Nd(iNd).IdxMat(1) = 1 And Math.Abs(Nd(iNd).Z - ACLoad(1).ThkSlb) < ErrTol And Math.Abs(Nd(jNd).Z) < ErrTol) Or
                               (Nd(iNd).IdxMat(1) = 12 And ACLoad(1).IndxOvrl = 1 And Math.Abs(Nd(iNd).Z - (ACLoad(1).ThkSlb + ACLoad(1).ThkOvr)) < ErrTol And Math.Abs(Nd(jNd).Z - ACLoad(1).ThkSlb) < ErrTol) Then

                                If (Math.Abs(Math.Abs(Nd(jNd).X - Nd(iNd).X) - 0.08 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX Or
                                   Math.Abs(Math.Abs(Nd(jNd).X - Nd(iNd).X) - 0.1 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX) And _
                                   (Math.Abs(Nd(jNd).Y - Nd(iNd).Y) < ErrTol * ACLoad(1).ScaleY Or
                                    Math.Abs(Math.Abs(Nd(jNd).Y - Nd(iNd).Y) - 0.02 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY) Then   ' x-Spring (y-joint)
                                    iSpringElement = iSpringElement + 1
                                    SpringElement(iSpringElement).Node1 = iNd
                                    SpringElement(iSpringElement).Node2 = jNd
                                    SpringElement(iSpringElement).Direction = 1
                                    SpringElement(iSpringElement).Stiffness = ACLoad(1).EqEdgStifX
                                End If

                                If (Math.Abs(Nd(jNd).X - Nd(iNd).X) < ErrTol * ACLoad(1).ScaleX Or
                                    Math.Abs(Math.Abs(Nd(jNd).X - Nd(iNd).X) - 0.02 * ACLoad(1).ScaleX) < ErrTol * ACLoad(1).ScaleX) And _
                                   (Math.Abs(Math.Abs(Nd(jNd).Y - Nd(iNd).Y) - 0.1 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY Or
                                    Math.Abs(Math.Abs(Nd(jNd).Y - Nd(iNd).Y) - 0.08 * ACLoad(1).ScaleY) < ErrTol * ACLoad(1).ScaleY) Then   ' y-Spring (x-joint)
                                    iSpringElement = iSpringElement + 1
                                    SpringElement(iSpringElement).Node1 = iNd
                                    SpringElement(iSpringElement).Node2 = jNd
                                    SpringElement(iSpringElement).Direction = 2
                                    SpringElement(iSpringElement).Stiffness = ACLoad(1).EqEdgStifX
                                End If

                            End If
                        Next jNd
                    End If

                End If
            Next iNd
        End If


        NSpringElement = iSpringElement
        ReDim Preserve SpringElement(NSpringElement)

        NSpringType = 0
        If NSpringElement >= 1 Then
            SpringType(1).Direction = SpringElement(1).Direction
            SpringType(1).Stiffness = SpringElement(1).Stiffness
            SpringElement(1).IdxSprType = 1
            NSpringType = 1
        End If

        Dim IsNewSpringType As Boolean
        For iSpringElement = 2 To NSpringElement

            IsNewSpringType = True
            For iSpringType = 1 To NSpringType
                If (Math.Abs(SpringElement(iSpringElement).Stiffness - SpringType(iSpringType).Stiffness) < ErrTol Or
                    Math.Abs(SpringElement(iSpringElement).Stiffness - SpringType(iSpringType).Stiffness) < ErrTol * SpringType(iSpringType).Stiffness) And _
                    SpringElement(iSpringElement).Direction = SpringType(iSpringType).Direction Then
                    SpringElement(iSpringElement).IdxSprType = iSpringType
                    IsNewSpringType = False
                    GoTo nextSpringElementLabel
                End If
            Next iSpringType

            If IsNewSpringType = True Then
                NSpringType = NSpringType + 1
                SpringType(NSpringType).Direction = SpringElement(iSpringElement).Direction
                SpringType(NSpringType).Stiffness = SpringElement(iSpringElement).Stiffness
                SpringElement(iSpringElement).IdxSprType = NSpringType
            End If

nextSpringElementLabel:
        Next iSpringElement

        ReDim Preserve SpringType(NSpringType)

    End Sub

    Private Sub SlidingElementGeneration()

        ReDim SlidingElement(NPartSld)

        Dim i, i0 As Integer
        Dim iSld, iNd As Integer
        Dim iPartSld As Integer

        Dim IdxSldTemp, IdxPartTemp As Integer

        Dim iXCtr1, iYCtr1, iZCtr1 As Integer
        Dim iXCtr2, iYCtr2, iZCtr2 As Integer

        Dim iXCtr, iYCtr, iZCtr As Integer
        Dim iNdX1, iNdY1 As Integer
        Dim iNdX2, iNdY2 As Integer
        Dim iNdX, iNdY As Integer

        Dim XNdStart, YNdStart As Double
        Dim XMesh0, YMesh0 As Double

        Dim XND, YND, ZND As Double

        Dim XNDEle, YNDEle, ZNDEle As Double
        Dim IP, NdEle(4) As Integer
        Dim XEleIP1, YEleIP1, ZEleIP1 As Double

        Dim NSldEleTemp, iSldEle As Integer

        i0 = 0
        For iSld = 1 To NSld

            IdxSldTemp = SLD(iSld).IdxSld

            For iPartSld = 1 To NPartSld
                If PARTSLD(iPartSld).SldIdx = IdxSldTemp Then

                    IdxPartTemp = PARTSLD(iPartSld).IdxPart

                    If PARTSLD(iPartSld).SldSmTyp = "s" Then i = i0 + 1
                    If PARTSLD(iPartSld).SldSmTyp = "m" Then i = i0 + 2

                    With PART(IdxPartTemp)

                        NSldEleTemp = 0
                        iSldEle = 0

                        iXCtr1 = PARTSLD(iPartSld).SldNdIdxCtrIni(1)
                        iXCtr2 = PARTSLD(iPartSld).SldNdIdxCtrEnd(1)

                        iYCtr1 = PARTSLD(iPartSld).SldNdIdxCtrIni(2)
                        iYCtr2 = PARTSLD(iPartSld).SldNdIdxCtrEnd(2)

                        iZCtr1 = PARTSLD(iPartSld).SldNdIdxCtrIni(3)
                        iZCtr2 = PARTSLD(iPartSld).SldNdIdxCtrEnd(3)

                        If iZCtr1 <> iZCtr2 Then
                            MsgBox("Error: Sliding element is not z-directional", MsgBoxStyle.OkOnly, "File Error")     'only consider z-direction sliding
                            Exit Sub
                        End If

                        SlidingElement(i).IdxDirection = 3
                        SlidingElement(i).FrictionSld = SLD(iSld).FrictionSld
                        SlidingElement(i).PenaltySld = SLD(iSld).PenaltySld

                        NSldEleTemp = (.NdXCtr(iXCtr2) - .NdXCtr(iXCtr1)) * (.NdYCtr(iYCtr2) - .NdYCtr(iYCtr1))

                        SlidingElement(i).NSldElement = NSldEleTemp
                        ReDim SlidingElement(i).Element(NSldEleTemp, 4)

                        iZCtr = iZCtr1
                        ZND = .CoordZCtr(iZCtr)

                        For iXCtr = iXCtr1 To iXCtr2 - 1


                            iNdX1 = .NdXCtr(iXCtr)
                            iNdX2 = .NdXCtr(iXCtr + 1)
                            XNdStart = .CoordXCtr(iXCtr)
                            XMesh0 = (.CoordXCtr(iXCtr + 1) - .CoordXCtr(iXCtr)) / (iNdX2 - iNdX1)

                            For iNdX = iNdX1 To iNdX2 - 1
                                XND = (XNdStart + (iNdX - iNdX1) * XMesh0) * ACLoad(1).ScaleX



                                For iYCtr = iYCtr1 To iYCtr2 - 1

                                    iNdY1 = .NdYCtr(iYCtr)
                                    iNdY2 = .NdYCtr(iYCtr + 1)
                                    YNdStart = .CoordYCtr(iYCtr)
                                    YMesh0 = (.CoordYCtr(iYCtr + 1) - .CoordYCtr(iYCtr)) / (iNdY2 - iNdY1)

                                    For iNdY = iNdY1 To iNdY2 - 1
                                        YND = (YNdStart + (iNdY - iNdY1) * YMesh0) * ACLoad(1).ScaleY

                                        XEleIP1 = Math.Min(XND, XND + XMesh0 * ACLoad(1).ScaleX)
                                        YEleIP1 = Math.Min(YND, YND + YMesh0 * ACLoad(1).ScaleY)
                                        ZEleIP1 = ZND

                                        IP = 1
IPloop:                                 Select Case IP
                                            Case 1 : XNDEle = XEleIP1 : YNDEle = YEleIP1 : ZNDEle = ZEleIP1
                                            Case 2
                                                If PARTSLD(iPartSld).SldSmTyp = "s" Then
                                                    XNDEle = XEleIP1 : YNDEle = YEleIP1 + Math.Abs(YMesh0) * ACLoad(1).ScaleY : ZNDEle = ZEleIP1
                                                ElseIf PARTSLD(iPartSld).SldSmTyp = "m" Then
                                                    XNDEle = XEleIP1 + Math.Abs(XMesh0) * ACLoad(1).ScaleX : YNDEle = YEleIP1 : ZNDEle = ZEleIP1
                                                End If
                                            Case 3 : XNDEle = XEleIP1 + Math.Abs(XMesh0) * ACLoad(1).ScaleX : YNDEle = YEleIP1 + Math.Abs(YMesh0) * ACLoad(1).ScaleY : ZNDEle = ZEleIP1
                                            Case 4
                                                If PARTSLD(iPartSld).SldSmTyp = "s" Then
                                                    XNDEle = XEleIP1 + Math.Abs(XMesh0) * ACLoad(1).ScaleX : YNDEle = YEleIP1 : ZNDEle = ZEleIP1
                                                ElseIf PARTSLD(iPartSld).SldSmTyp = "m" Then
                                                    XNDEle = XEleIP1 : YNDEle = YEleIP1 + Math.Abs(YMesh0) * ACLoad(1).ScaleY : ZNDEle = ZEleIP1
                                                End If
                                        End Select



                                        For iNd = 1 To NNd
                                            If (Nd(iNd).IdxPart(1) = IdxPartTemp And Nd(iNd).IdxPart(2) = IdxPartTemp) Then
                                                If Math.Abs(XNDEle - Nd(iNd).X) < ErrTol * ACLoad(1).ScaleX And _
                                                   Math.Abs(YNDEle - Nd(iNd).Y) < ErrTol * ACLoad(1).ScaleY And _
                                                   Math.Abs(ZNDEle - Nd(iNd).Z) < ErrTol Then

                                                    NdEle(IP) = iNd
                                                    If IP < 4 Then
                                                        IP = IP + 1
                                                        GoTo IPloop
                                                    End If

                                                    If IP = 4 Then
                                                        iSldEle = iSldEle + 1

                                                        For IP = 1 To 4
                                                            SlidingElement(i).Element(iSldEle, IP) = NdEle(IP)
                                                        Next IP

                                                        GoTo nextElementlabel
                                                    End If


                                                End If
                                            End If
                                        Next iNd

nextElementlabel:
                                    Next iNdY
                                Next iYCtr

                            Next iNdX
                        Next iXCtr

                    End With

                End If
            Next iPartSld

            i0 = i
        Next iSld

    End Sub

    Private Sub NodalLoadGeneration()

        ReDim NodalLoad(NAC)

        Dim xgrid, ygrid As Double
        Dim xtireleft0, xtireright0, ytiretop0, ytirebottom0 As Double
        Dim xtireleft, xtireright, ytiretop, ytirebottom As Double
        Dim xslabmin, xslabmax, yslabmin, yslabmax As Double
        Dim xdimjoint, ydimjoint As Double
        Dim x, y, x1, y1 As Double
        Dim tribarea As Double

        Dim icnt As Integer

        Dim iAC, iWhls, iNd As Integer
        Dim j, jj As Integer


        TotNNodalLoad = 0
        For iAC = 1 To NAC

            ReDim NodalLoad(iAC).Node(400), NodalLoad(iAC).Load(400)

            With ACLoad(iAC)

                If .NSlabs = 1 Then 'regardless of symmetry
                    xslabmin = .ScaleX * (0) : xslabmax = .ScaleX * (300)
                    yslabmin = .ScaleY * (-150) : yslabmax = .ScaleY * (150)
                End If

                If .NSlabs = 2 Then
                    xslabmin = .ScaleX * (-300) : xslabmax = .ScaleX * (300)
                    yslabmin = .ScaleY * (-150) : yslabmax = .ScaleY * (150)
                End If

                If .NSlabs = 4 Then
                    xslabmin = .ScaleX * (-300) : xslabmax = .ScaleX * (300)
                    yslabmin = .ScaleY * (-150) : yslabmax = .ScaleY * (450)
                End If

                If .NSlabs = 6 Then
                    xslabmin = .ScaleX * (-300) : xslabmax = .ScaleX * (600)
                    yslabmin = .ScaleY * (-150) : yslabmax = .ScaleY * (450)
                End If

                If .NSlabs = 9 Then
                    xslabmin = .ScaleX * (-300) : xslabmax = .ScaleX * (600)
                    yslabmin = .ScaleY * (-450) : yslabmax = .ScaleY * (450)
                End If


                icnt = 0
                For iWhls = 1 To .NWhls

                    xtireleft0 = .XTr(iWhls) - .XDim / 2 : xtireright0 = .XTr(iWhls) + .XDim / 2
                    ytirebottom0 = .YTr(iWhls) - .YDim / 2 : ytiretop0 = .YTr(iWhls) + .YDim / 2

                    For iNd = 1 To NNd
                        If Math.Abs(Nd(iNd).Z - ZMax) < ErrTol Then 'find node at slab top surface (ZMax)

                            If Nd(iNd).X > xslabmax + ErrTol Or Nd(iNd).X < xslabmin - ErrTol Or _
                               Nd(iNd).Y > yslabmax + ErrTol Or Nd(iNd).Y < yslabmin - ErrTol Then    ' to eliminate load allocation on dummy joint node
                                GoTo NextNodelabel
                            End If

                            If (Nd(iNd).X - Nd(iNd).XMinusMesh / 2) < xtireright0 And (Nd(iNd).X + Nd(iNd).XPlusMesh / 2) > xtireleft0 And _
                               (Nd(iNd).Y - Nd(iNd).YMinusMesh / 2) < ytiretop0 And (Nd(iNd).Y + Nd(iNd).YPlusMesh / 2) > ytirebottom0 Then

                                xtireleft = xtireleft0 : xtireright = xtireright0
                                ytirebottom = ytirebottom0 : ytiretop = ytiretop0

                                If Nd(iNd).X > .ScaleX * (0.0 - ErrTol) And Nd(iNd).X < .ScaleX * (300.0 + ErrTol) And _
                                   Nd(iNd).Y > .ScaleY * (-150.0 - ErrTol) And Nd(iNd).Y < .ScaleY * (150.0 + ErrTol) Then  ' Slab 1

                                    'If xtireleft0 > .ScaleX * (300) Or xtireright0 < .ScaleX * (0.0) Then GoTo NextNodelabel 'load not on slab 1, duplcate with IF in Ln1898 since X(Y)Minus(Plus)Mesh could be 0?
                                    'If ytiretop0 > .ScaleY * (150) Or ytirebottom0 < .ScaleY * (-150.0) Then GoTo NextNodelabel 'load not on slab 1

                                    If xtireleft0 < .ScaleX * (-0.02) And xtireright0 > .ScaleX * (0.0) Then xtireleft = CDbl(.ScaleX * (-0.02 + 0.02 / 2)) 'load on joint x(-0.02,0)
                                    If xtireleft0 < .ScaleX * (300) And xtireright0 > .ScaleX * (300.2) Then xtireright = CDbl(.ScaleX * (300 + 0.02 / 2)) 'load on joint x(300,300.02)
                                    If ytirebottom0 < .ScaleY * (-150.02) And ytiretop0 > .ScaleY * (-150) Then ytirebottom = CDbl(.ScaleY * (-150.02 + 0.02 / 2)) 'load on joint y(-150.02,-150)
                                    If ytirebottom0 < .ScaleY * (150) And ytiretop0 > .ScaleY * (150.02) Then ytiretop = CDbl(.ScaleY * (150 + 0.02 / 2)) 'load on joint y(150,150.02)

                                End If

                                If .NSlabs = 1 Then GoTo 535


                                If Nd(iNd).X > .ScaleX * (-300.0 - ErrTol) And Nd(iNd).X < .ScaleX * (-0.02 + ErrTol) And _
                                   Nd(iNd).Y > .ScaleY * (-150.0 - ErrTol) And Nd(iNd).Y < .ScaleY * (150.0 + ErrTol) Then  ' Slab 2

                                    'If xtireleft0 > .ScaleX * (-0.02) Or xtireright0 < .ScaleX * (-300.0) Then GoTo NextNodelabel 'load not on slab 2
                                    'If ytiretop0 > .ScaleY * (150) Or ytirebottom0 < .ScaleY * (-150.0) Then GoTo NextNodelabel 'load not on slab 2

                                    If xtireleft0 < .ScaleX * (-300.02) And xtireright0 > .ScaleX * (-300) Then xtireleft = CDbl(.ScaleX * (-300.02 + 0.02 / 2)) 'load on joint x(-300.02,-300)
                                    If xtireleft0 < .ScaleX * (-0.02) And xtireright0 > .ScaleX * (0.0) Then xtireright = CDbl(.ScaleX * (-0.02 + 0.02 / 2)) 'load on joint x(-0.02,0)
                                    If ytirebottom0 < .ScaleY * (-150.02) And ytiretop0 > .ScaleY * (-150) Then ytirebottom = CDbl(.ScaleY * (-150.02 + 0.02 / 2)) 'load on joint y(-150.02,-150)
                                    If ytirebottom0 < .ScaleY * (150) And ytiretop0 > .ScaleY * (150.02) Then ytiretop = CDbl(.ScaleY * (150 + 0.02 / 2)) 'load on joint y(150,150.02)

                                End If

                                If .NSlabs = 2 Then GoTo 535


                                If Nd(iNd).X > .ScaleX * (0.0 - ErrTol) And Nd(iNd).X < .ScaleX * (300.0 + ErrTol) And _
                                   Nd(iNd).Y > .ScaleY * (150.02 - ErrTol) And Nd(iNd).Y < .ScaleY * (450.0 + ErrTol) Then  ' Slab 3

                                    'If xtireleft0 > .ScaleX * (300) Or xtireright0 < .ScaleX * (0.0) Then GoTo NextNodelabel 'load not on slab 3
                                    'If ytiretop0 > .ScaleY * (450) Or ytirebottom0 < .ScaleY * (150.02) Then GoTo NextNodelabel 'load not on slab 3

                                    If xtireleft0 < .ScaleX * (-0.02) And xtireright0 > .ScaleX * (0.0) Then xtireleft = CDbl(.ScaleX * (-0.02 + 0.02 / 2)) 'load on joint x(-0.02,0)
                                    If xtireleft0 < .ScaleX * (300) And xtireright0 > .ScaleX * (300.2) Then xtireright = CDbl(.ScaleX * (300 + 0.02 / 2)) 'load on joint x(300,300.02)
                                    If ytirebottom0 < .ScaleY * (150) And ytiretop0 > .ScaleY * (150.02) Then ytirebottom = CDbl(.ScaleY * (150 + 0.02 / 2)) 'load on joint y(150,150.02)
                                    If ytirebottom0 < .ScaleY * (450) And ytiretop0 > .ScaleY * (450.02) Then ytiretop = CDbl(.ScaleY * (450 + 0.02 / 2)) 'load on joint y(450,450.02)

                                End If


                                If Nd(iNd).X > .ScaleX * (-300.0 - ErrTol) And Nd(iNd).X < .ScaleX * (-0.02 + ErrTol) And _
                                   Nd(iNd).Y > .ScaleY * (150.02 - ErrTol) And Nd(iNd).Y < .ScaleY * (450.0 + ErrTol) Then  ' Slab 4

                                    'If xtireleft0 > .ScaleX * (-0.02) Or xtireright0 < .ScaleX * (-300.0) Then GoTo NextNodelabel 'load not on slab 4
                                    'If ytiretop0 > .ScaleY * (450) Or ytirebottom0 < .ScaleY * (150.02) Then GoTo NextNodelabel 'load not on slab 4

                                    If xtireleft0 < .ScaleX * (-300.02) And xtireright0 > .ScaleX * (-300) Then xtireleft = CDbl(.ScaleX * (-300.02 + 0.02 / 2)) 'load on joint x(-300.02,-300)
                                    If xtireleft0 < .ScaleX * (-0.02) And xtireright0 > .ScaleX * (0.0) Then xtireright = CDbl(.ScaleX * (-0.02 + 0.02 / 2)) 'load on joint x(-0.02,0)
                                    If ytirebottom0 < .ScaleY * (150) And ytiretop0 > .ScaleY * (150.02) Then ytirebottom = CDbl(.ScaleY * (150 + 0.02 / 2)) 'load on joint y(150,150.02)
                                    If ytirebottom0 < .ScaleY * (450) And ytiretop0 > .ScaleY * (450.02) Then ytiretop = CDbl(.ScaleY * (450 + 0.02 / 2)) 'load on joint y(450,450.02)

                                End If

                                If .NSlabs = 4 Then GoTo 535


                                If Nd(iNd).X > .ScaleX * (300.02 - ErrTol) And Nd(iNd).X < .ScaleX * (600.0 + ErrTol) And _
                                  Nd(iNd).Y > .ScaleY * (-150.0 - ErrTol) And Nd(iNd).Y < .ScaleY * (150.0 + ErrTol) Then  ' Slab 5

                                    'If xtireleft0 > .ScaleX * (600) Or xtireright0 < .ScaleX * (300.02) Then GoTo NextNodelabel 'load not on slab 5
                                    'If ytiretop0 > .ScaleY * (150) Or ytirebottom0 < .ScaleY * (-150.0) Then GoTo NextNodelabel 'load not on slab 5

                                    If xtireleft0 < .ScaleX * (300) And xtireright0 > .ScaleX * (300.2) Then xtireleft = CDbl(.ScaleX * (300 + 0.02 / 2)) 'load on joint x(300,300.02)
                                    If xtireleft0 < .ScaleX * (600) And xtireright0 > .ScaleX * (600.02) Then xtireright = CDbl(.ScaleX * (600 + 0.02 / 2)) 'load on joint x(600.,600.02)
                                    If ytirebottom0 < .ScaleY * (-150.02) And ytiretop0 > .ScaleY * (-150) Then ytirebottom = CDbl(.ScaleY * (-150.02 + 0.02 / 2)) 'load on joint y(-150.02,-150)
                                    If ytirebottom0 < .ScaleY * (150) And ytiretop0 > .ScaleY * (150.02) Then ytiretop = CDbl(.ScaleY * (150 + 0.02 / 2)) 'load on joint y(150,150.02)

                                End If


                                If Nd(iNd).X > .ScaleX * (300.02 - ErrTol) And Nd(iNd).X < .ScaleX * (600.0 + ErrTol) And _
                                   Nd(iNd).Y > .ScaleY * (150.02 - ErrTol) And Nd(iNd).Y < .ScaleY * (450.0 + ErrTol) Then  ' Slab 6

                                    'If xtireleft0 > .ScaleX * (600) Or xtireright0 < .ScaleX * (300.02) Then GoTo NextNodelabel 'load not on slab 6
                                    'If ytiretop0 > .ScaleY * (450) Or ytirebottom0 < .ScaleY * (150.02) Then GoTo NextNodelabel 'load not on slab 6

                                    If xtireleft0 < .ScaleX * (300) And xtireright0 > .ScaleX * (300.2) Then xtireleft = CDbl(.ScaleX * (300 + 0.02 / 2)) 'load on joint x(300,300.02)
                                    If xtireleft0 < .ScaleX * (600) And xtireright0 > .ScaleX * (600.02) Then xtireright = CDbl(.ScaleX * (600 + 0.02 / 2)) 'load on joint x(600.,600.02)
                                    If ytirebottom0 < .ScaleY * (150) And ytiretop0 > .ScaleY * (150.02) Then ytirebottom = CDbl(.ScaleY * (150 + 0.02 / 2)) 'load on joint y(150,150.02)
                                    If ytirebottom0 < .ScaleY * (450) And ytiretop0 > .ScaleY * (450.02) Then ytiretop = CDbl(.ScaleY * (450 + 0.02 / 2)) 'load on joint y(450,450.02)

                                End If

                                If .NSlabs = 6 Then GoTo 535


                                If Nd(iNd).X > .ScaleX * (0.0 - ErrTol) And Nd(iNd).X < .ScaleX * (300.0 + ErrTol) And _
                                   Nd(iNd).Y > .ScaleY * (-450.0 - ErrTol) And Nd(iNd).Y < .ScaleY * (-150.02 + ErrTol) Then  ' Slab 7

                                    'If xtireleft0 > .ScaleX * (300) Or xtireright0 < .ScaleX * (0.0) Then GoTo NextNodelabel 'load not on slab 7
                                    'If ytiretop0 > .ScaleY * (-150.02) Or ytirebottom0 < .ScaleY * (-450) Then GoTo NextNodelabel 'load not on slab 7

                                    If xtireleft0 < .ScaleX * (-0.02) And xtireright0 > .ScaleX * (0.0) Then xtireleft = CDbl(.ScaleX * (-0.02 + 0.02 / 2)) 'load on joint x(-0.02,0)
                                    If xtireleft0 < .ScaleX * (300) And xtireright0 > .ScaleX * (300.2) Then xtireright = CDbl(.ScaleX * (300 + 0.02 / 2)) 'load on joint x(300,300.02)
                                    If ytirebottom0 < .ScaleY * (-450.02) And ytiretop0 > .ScaleY * (-450) Then ytirebottom = CDbl(.ScaleY * (-450.02 + 0.02 / 2)) 'load on joint y(-450.02,-450)
                                    If ytirebottom0 < .ScaleY * (-150.02) And ytiretop0 > .ScaleY * (-150) Then ytiretop = CDbl(.ScaleY * (-150.02 + 0.02 / 2)) 'load on joint y(-150.02,-150)

                                End If


                                If Nd(iNd).X > .ScaleX * (-300.0 - ErrTol) And Nd(iNd).X < .ScaleX * (-0.02 + ErrTol) And _
                                   Nd(iNd).Y > .ScaleY * (-450.0 - ErrTol) And Nd(iNd).Y < .ScaleY * (-150.02 + ErrTol) Then  ' Slab 8

                                    'If xtireleft0 > .ScaleX * (-0.02) Or xtireright0 < .ScaleX * (-300.0) Then GoTo NextNodelabel 'load not on slab 8
                                    'If ytiretop0 > .ScaleY * (-150.02) Or ytirebottom0 < .ScaleY * (-450) Then GoTo NextNodelabel 'load not on slab 8

                                    If xtireleft0 < .ScaleX * (-300.02) And xtireright0 > .ScaleX * (-300) Then xtireleft = CDbl(.ScaleX * (-300.02 + 0.02 / 2)) 'load on joint x(-300.02,-300)
                                    If xtireleft0 < .ScaleX * (-0.02) And xtireright0 > .ScaleX * (0.0) Then xtireright = CDbl(.ScaleX * (-0.02 + 0.02 / 2)) 'load on joint x(-0.02,0)
                                    If ytirebottom0 < .ScaleY * (-450.02) And ytiretop0 > .ScaleY * (-450) Then ytirebottom = CDbl(.ScaleY * (-450.02 + 0.02 / 2)) 'load on joint y(-450.02,-450)
                                    If ytirebottom0 < .ScaleY * (-150.02) And ytiretop0 > .ScaleY * (-150) Then ytiretop = CDbl(.ScaleY * (-150.02 + 0.02 / 2)) 'load on joint y(-150.02,-150)

                                End If


                                If Nd(iNd).X > .ScaleX * (300.02 - ErrTol) And Nd(iNd).X < .ScaleX * (600.0 + ErrTol) And _
                                   Nd(iNd).Y > .ScaleY * (-450.0 - ErrTol) And Nd(iNd).Y < .ScaleY * (-150.02 + ErrTol) Then  ' Slab 9

                                    'If xtireleft0 > .ScaleX * (600) Or xtireright0 < .ScaleX * (300.02) Then GoTo NextNodelabel 'load not on slab 9
                                    'If ytiretop0 > .ScaleY * (-150.02) Or ytirebottom0 < .ScaleY * (-450) Then GoTo NextNodelabel 'load not on slab 9

                                    If xtireleft0 < .ScaleX * (300) And xtireright0 > .ScaleX * (300.2) Then xtireleft = CDbl(.ScaleX * (300 + 0.02 / 2)) 'load on joint x(300,300.02)
                                    If xtireleft0 < .ScaleX * (600) And xtireright0 > .ScaleX * (600.02) Then xtireright = CDbl(.ScaleX * (600 + 0.02 / 2)) 'load on joint x(600.,600.02)
                                    If ytirebottom0 < .ScaleY * (-450.02) And ytiretop0 > .ScaleY * (-450) Then ytirebottom = CDbl(.ScaleY * (-450.02 + 0.02 / 2)) 'load on joint y(-450.02,-450)
                                    If ytirebottom0 < .ScaleY * (-150.02) And ytiretop0 > .ScaleY * (-150) Then ytiretop = CDbl(.ScaleY * (-150.02 + 0.02 / 2)) 'load on joint y(-150.02,-150)

                                End If


535:
                                xdimjoint = xtireright - xtireleft
                                ydimjoint = ytiretop - ytirebottom

                                If (icnt > 0) Then
                                    For j = 1 To icnt
                                        If NodalLoad(iAC).Node(j) = iNd Then
                                            jj = j
                                            GoTo 55
                                        End If
                                    Next j
                                End If




                                icnt = icnt + 1
                                jj = icnt

55:
                                x = Math.Min(.XDim, xdimjoint)
                                y = Math.Min(.YDim, ydimjoint)

                                xgrid = (Nd(iNd).XPlusMesh + Nd(iNd).XMinusMesh) / 2
                                If (xgrid < x) Then x = xgrid
                                x1 = (Nd(iNd).X + Nd(iNd).XPlusMesh / 2) - xtireleft
                                If (x1 < x) Then x = x1
                                x1 = xtireright - (Nd(iNd).X - Nd(iNd).XMinusMesh / 2)
                                If (x1 < x) Then x = x1

                                ygrid = (Nd(iNd).YPlusMesh + Nd(iNd).YMinusMesh) / 2
                                If (ygrid < y) Then y = ygrid
                                y1 = (Nd(iNd).Y + Nd(iNd).YPlusMesh / 2) - ytirebottom
                                If (y1 < y) Then y = y1
                                y1 = ytiretop - (Nd(iNd).Y - Nd(iNd).YMinusMesh / 2)
                                If (y1 < y) Then y = y1

                                tribarea = x * y

                                NodalLoad(iAC).Node(jj) = iNd
                                NodalLoad(iAC).Load(jj) = NodalLoad(iAC).Load(jj) + tribarea * .PcntCt

                            End If
                        End If

NextNodelabel:
                    Next iNd
                Next iWhls

            End With

            NodalLoad(iAC).NNodalLoad = icnt
            ReDim Preserve NodalLoad(iAC).Node(icnt), NodalLoad(iAC).Load(icnt)

            TotNNodalLoad = TotNNodalLoad + icnt

        Next iAC

    End Sub

    Private Sub NodalLoadAngleGeneration()

        ReDim NodalLoad(NAC)

        Dim nx, ny As Integer
        Dim alphar, areasd As Double

        Dim xtire, ytire, xstart, ystart, xcl, ycl, x1, y1, xpoint, ypoint As Double

        Dim near, icnt As Integer
        Dim dd As Double, dist As Double

        Dim iAC, iWhls, j, k, jj, iNd As Integer

        nx = 100 : ny = 100
        TotNNodalLoad = 0
        For iAC = 1 To NAC

            ReDim NodalLoad(iAC).Node(200), NodalLoad(iAC).Load(200)

            With ACLoad(iAC)

                alphar = CDbl(.Alpha * Math.PI / 180)

                icnt = 0
                For iWhls = 1 To .NWhls
                    xtire = .XTr(iWhls)
                    ytire = -.YTr(iWhls)

                    xstart = CDbl(xtire + 0.5 * .YDim * (1 - (1 / ny)) * Math.Sin(alphar))
                    ystart = CDbl(ytire - 0.5 * .YDim * (1 - (1 / ny)) * Math.Cos(alphar))

                    areasd = .XDim * .YDim / (nx * ny)

                    For j = 1 To ny
                        xcl = CDbl(xstart - (j - 1) * (.YDim / ny) * Math.Sin(alphar))
                        ycl = CDbl(ystart + (j - 1) * (.YDim / ny) * Math.Cos(alphar))

                        x1 = CDbl(xcl - 0.5 * .XDim * (1 - (1 / nx)) * Math.Cos(alphar))
                        y1 = CDbl(ycl - 0.5 * .XDim * (1 - (1 / ny)) * Math.Sin(alphar))

                        For k = 1 To nx
                            xpoint = CDbl(x1 + (k - 1) * (.XDim / nx) * Math.Cos(alphar))
                            ypoint = CDbl(y1 + (k - 1) * (.XDim / nx) * Math.Sin(alphar))

                            dist = 1.0E+30
                            For iNd = 1 To NNd
                                If Math.Abs(Nd(iNd).Z - ZMax) < ErrTol Then 'at slab\overlay surface
                                    dd = CDbl((Nd(iNd).X - xpoint) ^ 2 + (Nd(iNd).Y - ypoint) ^ 2 + Nd(iNd).Z ^ 2)
                                    If ((dd >= dist) And (iNd <> 1)) Then GoTo 100
                                    near = iNd
                                    dist = dd
                                End If
100:                        Next iNd

                            If (icnt > 0) Then
                                For jj = 1 To icnt
                                    If NodalLoad(iAC).Node(jj) = near Then
                                        GoTo 60
                                    End If
                                Next jj
                            End If

                            icnt = icnt + 1
                            jj = icnt
                            NodalLoad(iAC).Node(jj) = near
60:                         NodalLoad(iAC).Load(jj) = NodalLoad(iAC).Load(jj) + areasd * .PcntCt
                        Next k

                    Next j
                Next iWhls

            End With

            NodalLoad(iAC).NNodalLoad = icnt
            ReDim Preserve NodalLoad(iAC).Node(icnt), NodalLoad(iAC).Load(icnt)

            TotNNodalLoad = TotNNodalLoad + icnt

        Next iAC

    End Sub

    Private Sub NodalTempGeneration()

        Dim TempTop, TempBtm As Double
        Dim ZTempTop, ZTempbtm As Double

        Dim iNd, iPart As Integer
        Dim xmax, xmin, ymax, ymin, r, rmax As Double
        Dim xcenter, ycenter As Double

        'Temperature only assinged to slab or overlay
        If ACLoad(1).IndxOvrl = 1 Then
            TempTop = CDbl(0.5 * ACLoad(1).LETG * ACLoad(1).ThkOvr)
            TempBtm = -CDbl(0.5 * ACLoad(1).LETG * ACLoad(1).ThkOvr)
            ZTempTop = ZMax
            ZTempbtm = ZMax - ACLoad(1).ThkOvr
        Else
            TempTop = CDbl(0.5 * ACLoad(1).LETG * ACLoad(1).ThkSlb)
            TempBtm = -CDbl(0.5 * ACLoad(1).LETG * ACLoad(1).ThkSlb)
            ZTempTop = ACLoad(1).ThkSlb
            ZTempbtm = 0
        End If

        iPart = 0
        For iNd = 1 To NNd
            If (ACLoad(1).IndxOvrl = 1 And Nd(iNd).IdxMat(1) = 12) Or
                ACLoad(1).IndxOvrl = 0 And Nd(iNd).IdxMat(1) = 1 Then

                If ACLoad(1).CurlShapara = 0 Then ' circular/spherical slab curling(linear temperature)
                    If Math.Abs(Nd(iNd).Z - ZTempTop) < ErrTol Then Nd(iNd).Temp = TempTop
                    If Math.Abs(Nd(iNd).Z - ZTempbtm) < ErrTol Then Nd(iNd).Temp = TempBtm
                ElseIf ACLoad(1).CurlShapara <> 0 Then  'catenary slab curling
                    If Nd(iNd).IdxPart(1) <> iPart Then
                        iPart = Nd(iNd).IdxPart(1)
                        xmax = Math.Max(PART(iPart).CoordXCtr(1), PART(iPart).CoordXCtr(PART(iPart).NXCtr))
                        xmin = Math.Min(PART(iPart).CoordXCtr(1), PART(iPart).CoordXCtr(PART(iPart).NXCtr))
                        ymax = Math.Max(PART(iPart).CoordYCtr(1), PART(iPart).CoordYCtr(PART(iPart).NYCtr))
                        ymin = Math.Min(PART(iPart).CoordYCtr(1), PART(iPart).CoordYCtr(PART(iPart).NYCtr))
                        xcenter = CDbl(0.5 * (xmin + xmax))
                        ycenter = CDbl(0.5 * (ymin + ymax))
                        rmax = CDbl(0.5 * Math.Sqrt((xmax - xmin) ^ 2 + (ymax - ymin) ^ 2))
                    End If
                    r = CDbl(Math.Sqrt((Nd(iNd).X - xcenter) ^ 2 + (Nd(iNd).Y - ycenter) ^ 2))
                    If Math.Abs(Nd(iNd).Z - ZTempTop) < ErrTol Then Nd(iNd).Temp = CDbl((TempTop + TempBtm) / 2.0 + (TempTop - TempBtm) / 2.0 * ACLoad(1).CurlShapara * (Math.Cosh(r / rmax)))
                    If Math.Abs(Nd(iNd).Z - ZTempbtm) < ErrTol Then Nd(iNd).Temp = CDbl((TempTop + TempBtm) / 2.0 - (TempTop - TempBtm) / 2.0 * ACLoad(1).CurlShapara * (Math.Cosh(r / rmax)))
                End If

            End If
        Next iNd

    End Sub

    Private Sub WriteNikeinFile(ByVal WorkingDir As String)

        Dim FileName As String
        Dim LFNo As Integer

        'FileName$ = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FAARFIELD\nikein"
        FileName$ = WorkingDir & "\input.txt"               ' QW 09-15-2019

        LFNo = FreeFile()
        FileOpen(LFNo, FileName, OpenMode.Output, , , 1024)

        Call WriteControlCard(LFNo)
        Call WriteMaterial(LFNo)
        'Call WriteZipWip(LFNo)                             ' QW 11-01-2019
        Call WriteNode(LFNo)
        Call WriteBrickElement(LFNo)
        Call WriteSpringElement(LFNo)
        Call WriteSlidingElement(LFNo)
        Call WriteLoadCurve(LFNo)
        Call WriteNodalLoad(LFNo)

        If itpro = 1 Then Call WriteNodalTemp(LFNo)

        FileClose(LFNo)

    End Sub

    Private Sub WriteControlCard(ByRef LFNo As Integer)
        Dim s As String

        PrintLine(LFNo, "Rigid Vehicle Pavement")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*----------------- ANALYSIS INPUT DATA FOR NIKE3D ---------------------*")
        PrintLine(LFNo, "*")

        'PrintLine(LFNo, "* Generated by MeshClassLib       (10/12/15)") ' v3.0 YC 052620-v3.0 003 YC
        PrintLine(LFNo, "* Generated by MeshClassLib       (05/26/20)")

        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*-------------------------- CONTROL CARD #2 ---------------------------*")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "* Input format[1], Number of Materials[2], Number of Nodal Points[3],")
        PrintLine(LFNo, "* Number of Brick Elements[4], Number of Beam Elements[5], Number of ")
        PrintLine(LFNo, "* Shell Elements[6], Number of 1-D Slide Lines[7], Number of Sliding")
        PrintLine(LFNo, "* Interfaces[8], Number of symmetry planes[9], Disc. Element flag[10]")

        'Format 'FL',i3,4i10,4i5
        s = "FL" & LPad(3, Format(NMat, "0"))
        s = s & LPad(10, Format(NNd, "0")) & LPad(10, Format(NBrckEle, "0")) & LPad(10, Format(0, "0")) & LPad(10, Format(0, "0"))
        s = s & LPad(5, Format(0, "0")) & LPad(5, Format(NPartSld / 2, "0")) & LPad(5, Format(0, "0")) & LPad(5, Format(1, "0"))
        PrintLine(LFNo, s)

        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*-------------------------- CONTROL CARD #3 ---------------------------*")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "* Number of time steps[1], Time step size [2], Auto time step control,")
        PrintLine(LFNo, "* flag[3], Max. number of restarts allowable per step[4],  Optimal")
        PrintLine(LFNo, "* number of iterations per step[5], Min. allowable time step size[6],")
        PrintLine(LFNo, "* Max. allowable time step size[7]")
        'Format i10,e10.3,5x,2i5,2e10.3
        PrintLine(LFNo, LPad(10, Format(NAC, "0")) & " 1.000E+00         0    0 3.333E-01 3.000E+00")

        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*-------------------------- CONTROL CARD #4 ---------------------------*")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "* Number of load curves[1], Max. number of points defining load")
        PrintLine(LFNo, "* curve[2], Number of concentrated nodal load cards[3], Number of")
        PrintLine(LFNo, "* element surfaces with pressure loadings[4], Number of displacement")
        PrintLine(LFNo, "* boundary condition cards[5], Number of beam elements with aerodynamic")
        PrintLine(LFNo, "* loads[6], Number of nodal constraint cards[7], X-acceleration body")
        PrintLine(LFNo, "* force[8], Y-acceleration body force[9], Z-acceleration body force[10],")
        PrintLine(LFNo, "* X-rotational body force[11], Y-rotational body force[12],")
        PrintLine(LFNo, "* Z-rotational body force[13], Number of nodes with steering")
        PrintLine(LFNo, "* boundary conditions[14], Number of nodes with foundation")
        PrintLine(LFNo, "* boundary conditions[15]")

        'Format 15i5
        s = LPad(5, Format(NLcd, "0")) & LPad(5, Format(NLcd + 1, "0")) & LPad(5, Format(TotNNodalLoad, "0"))
        s = s & "    0    0    0    0    0    0    0    0    0    0    0    0"
        PrintLine(LFNo, s)

        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*-------------------------- CONTROL CARD #5 ---------------------------*")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "* Output print interval[1], Output plot interval[2], Number of nodal")
        PrintLine(LFNo, "* print blocks[3], Number of brick element print blocks[4],  Number of")
        PrintLine(LFNo, "* beam element print blocks[5], Number of shell element print blocks[6],")
        PrintLine(LFNo, "* Reserved[7], No. of time steps between restarts[8], Shell strain")
        PrintLine(LFNo, "* flag[9], Sense Switches 3, 6 & 7[10]")
        PrintLine(LFNo, "    1    0    0    0    0    0        -1    000000                   1")

        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*-------------------------- CONTROL CARD #6 ---------------------------*")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "* Iteration method[1], Bandwidth min. flag[2], No. of time steps between")
        PrintLine(LFNo, "* stiffness reform[3], No. of time steps between eq.  iterations[4], Max.")
        PrintLine(LFNo, "* No. of iterations between reforms[5], Max. No. of reforms per time")
        PrintLine(LFNo, "* step[6], displacement tolerance[7], energy tolerance[8],  Line search")
        PrintLine(LFNo, "* tolerance[9]")
        If IDSolver = 0 Then PrintLine(LFNo, "    1    1         0         0   30    0 0.000E+00 0.000E+00           9.000E-01") 'EBE solver
        If IDSolver = 1 Then PrintLine(LFNo, "    1    1        50         0   30    0 0.000E+00 0.000E+00           9.000E-01") 'direct solver

        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*-------------------------- CONTROL CARD #7 ---------------------------*")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "* Analysis type[1], Initial condition flag[2], Thermal effects flag[3],")
        PrintLine(LFNo, "* Temperature profile flag[4], Number of eigenvalues/vectors[5],")
        PrintLine(LFNo, "* Frequency shift[6], First Newmark parm.[7], Second Newmark  parm.[8]")
        If itpro = 1 Then
            PrintLine(LFNo, "    0    0    1    1    0 0.000E+00 5.000E-01 2.500E-01")
        Else
            PrintLine(LFNo, "    0    0    0    0    0 0.000E+00 5.000E-01 2.500E-01")
        End If

        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*-------------------------- CONTROL CARD #8 ---------------------------*")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "* Percentage of memory[1], Buffer size[2], Stiffness storage flag[3],")
        PrintLine(LFNo, "* BFGS update storage[4], Brick element formulation[5], Brick element")
        PrintLine(LFNo, "* stiffness[6], Shell formulation[7], Hourglass control[8]")
        PrintLine(LFNo, "* Shell Geom Stiffness Flag[9], Beam Formulation[10], Beam Geometric")
        PrintLine(LFNo, "* Stiffness Flag ")
        PrintLine(LFNo, "    0  60000000    1    1    2    1    1 0.000E+00    1    1    1")

        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*-------------------------- CONTROL CARD #9 ---------------------------*")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "* No. of unload steps[1], Solution method during unload[2], Node number")
        PrintLine(LFNo, "* for displacement[3], Direction of displacement[4], Arc length[5],")
        PrintLine(LFNo, "* Constaint method[6], Damping flag[7], No. of user spec. integration")
        PrintLine(LFNo, "* rules[8], Max. no. of integration points[9], No. of integration rules")
        PrintLine(LFNo, "* for shells[10], Max. no. of integration points[11]")
        'PrintLine(LFNo, "    0    0    0    0 0.000E+00    0    0    0    0    1    5")
        PrintLine(LFNo, "    0    0    0    0 0.000E+00    0    0    0    0    0    0")     ' QW 11-01-2019
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*-------------------------- CONTROL CARD #10 ---------------------------*")
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "* Linear Eq. solver[1], Iteration limit[2], Convergence tolerance[3],")
        PrintLine(LFNo, "* Buffer size[4], Output option[5], Data Storage Option[6]")
        If IDSolver = 0 Then PrintLine(LFNo, "    1 9999 0.000E+00    0    0    1") 'EBE solver
        If IDSolver = 1 Then PrintLine(LFNo, "    0    0 0.000E+00    0    0    1") 'direct solver

    End Sub

    Private Sub WriteMaterial(ByRef LFNo As Integer)
        Dim iMat, i, j As Integer
        Dim s As String

        For iMat = 1 To NMat
            'Format 2i5,e10.4,i5
            s = LPad(5, Format(MAT(iMat).IdxMat, "0")) & LPad(5, Format(MAT(iMat).KeyMatPrpty, "0")) & LPad(10, Format(MAT(iMat).DensityMat, "0.0000E+00")) & LPad(5, Format(0, "0"))
            s = s.Replace(",", ".") 'YC 040517
            PrintLine(LFNo, s)

            s = "material type # " & RPad(3, Format(MAT(iMat).KeyMatPrpty, "0"))
            If MAT(iMat).KeyMatPrpty = 1 Then ' elastic
                s = s & "(elastic)"
            ElseIf MAT(iMat).KeyMatPrpty = 4 Then
                s = s & "(thermo-elastic-plastic)"
            ElseIf MAT(iMat).KeyMatPrpty = 56 Then ' infinite element
                s = s & "(inf. elem.)"
            End If
            PrintLine(LFNo, s)

            'Format 8e10.3
            For i = 1 To 6
                s = vbNullString
                For j = 1 To 8
                    s = s & LPad(10, Format(MAT(iMat).MatPara(i, j), "0.000E+00"))
                Next j
                s = s.Replace(",", ".") 'YC 040517
                PrintLine(LFNo, s)
            Next i

        Next iMat

    End Sub

    Private Sub WriteZipWip(ByRef LFNo As Integer)
        Dim Zip As Double() = {-1, -0.65465367, 0, 0.65465367, 1}
        Dim Wip As Double() = {0.1, 0.54444444, 0.71111111, 0.54444444, 0.1}

        Dim s As String
        Dim i As Integer

        'Format 2i5
        s = LPad(5, Format(5, "0")) & LPad(5, Format(0, "0"))
        PrintLine(LFNo, s)

        'Format 2e10.3
        For i = 0 To 4
            s = LPad(10, Format(Zip(i), "0.000E+00")) & LPad(10, Format(Wip(i), "0.000E+00"))
            s = s.Replace(",", ".") 'YC 040517
            PrintLine(LFNo, s)
        Next i

    End Sub

    Private Sub WriteNode(ByRef LFNo As Integer)
        Dim s As String

        'NODE format(i8,f5.0,1p,3e20.13,0p,f5.0)
        Dim iNd As Integer
        For iNd = 1 To NNd
            s = LPad(8, Format(iNd, "0"))
            s = s & LPad(5, Format(Nd(iNd).BXYZ, "0")) ': Call MakeDecimalPeriod(s)
            s = s & LPad(20, Format(Nd(iNd).X, "0.0000000000000E+00")) ': Call MakeDecimalPeriod(s)
            s = s & LPad(20, Format(Nd(iNd).Y, "0.0000000000000E+00")) ': Call MakeDecimalPeriod(s)
            s = s & LPad(20, Format(Nd(iNd).Z, "0.0000000000000E+00")) ': Call MakeDecimalPeriod(s)
            s = s & LPad(4, Format(Nd(iNd).BRXYZ, "0")) ': Call MakeDecimalPeriod(s)
            s = s.Replace(",", ".") 'YC 040517
            PrintLine(LFNo, s)
        Next iNd
        'NODE END
    End Sub

    Private Sub WriteBrickElement(ByRef LFNo As Integer)
        Dim s As String

        'BRICK ELEMENT format(i8,i5,8i8)
        Dim iBrckEle, IP As Integer
        For iBrckEle = 1 To NBrckEle
            s = LPad(8, Format(iBrckEle, "0"))
            s = s & LPad(5, Format(BrickElement(iBrckEle).IdxMat, "0"))
            For IP = 1 To 8
                s = s & LPad(8, Format(BrickElement(iBrckEle).Node(IP), "0"))
            Next IP
            PrintLine(LFNo, s)
        Next iBrckEle
        'BRICK ELEMENT END
    End Sub

    Private Sub WriteSpringElement(ByRef LFNo As Integer)
        Dim s As String

        ' **********************************'SPRING ELEMENT*********************************************
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*------------------ DISCRETE ELEMENT DECK ------------------*")
        PrintLine(LFNo, "*")

        'format(1p,3i5)
        s = LPad(5, Format(NSpringType, "0")) & LPad(5, Format(NSpringElement, "0")) & LPad(5, Format(0, "0"))
        PrintLine(LFNo, s)

        'spring type format(1p,2i5), format(1p,2e10.3)
        Dim iSpringType As Integer
        For iSpringType = 1 To NSpringType
            s = LPad(5, Format(iSpringType, "0")) & LPad(5, Format(1, "0"))
            PrintLine(LFNo, s)
            s = LPad(10, Format(SpringType(iSpringType).Stiffness, "0.000E+00")) & LPad(10, Format(SpringType(iSpringType).Direction, "0.000E+00"))
            s = s.Replace(",", ".") 'YC 040517
            PrintLine(LFNo, s)
        Next

        'spring element format(1p,i5,i8,i8,i5)
        Dim iSpringElement As Integer
        For iSpringElement = 1 To NSpringElement
            s = LPad(5, Format(iSpringElement, "0"))
            s = s & LPad(8, Format(SpringElement(iSpringElement).Node1, "0"))
            s = s & LPad(8, Format(SpringElement(iSpringElement).Node2, "0"))
            s = s & LPad(5, Format(SpringElement(iSpringElement).IdxSprType, "0"))
            PrintLine(LFNo, s)
        Next iSpringElement
        ' **********************************'SPRING ELEMENT END*********************************************
    End Sub

    Private Sub WriteSlidingElement(ByRef LFNo As Integer)
        Dim s As String

        ' **********************************'SLIDING ELEMENT*********************************************
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*--------------------- SLIDING INTERFACE DEFINITIONS ---------------------*")
        PrintLine(LFNo, "*")

        Dim iPartSld2 As Integer
        'format(1p,2i8,i4,5e10.3,2i5) w/o format(1p,i5,5e10.3) partially bonded
        For iPartSld2 = 1 To CInt(NPartSld / 2)
            s = LPad(8, Format(SlidingElement(2 * iPartSld2 - 1).NSldElement, "0")) 'no# of sliding element of type "s" 
            s = s & LPad(8, Format(SlidingElement(2 * iPartSld2).NSldElement, "0")) 'no# of slding element of type "s"
            s = s & LPad(4, Format(SlidingElement(2 * iPartSld2).IdxDirection, "0")) ' slding element direction
            s = s & LPad(10, Format(SlidingElement(2 * iPartSld2).PenaltySld, "0.000E+00"))    'slding element friction
            s = s & LPad(10, Format(SlidingElement(2 * iPartSld2).FrictionSld, "0.000E+00"))    'sliding element penalty
            s = s & LPad(10, Format(0, "0.000E+00"))
            s = s & LPad(10, Format(0, "0.000E+00"))
            s = s & LPad(10, Format(0, "0.000E+00"))
            s = s & LPad(5, Format(0, "0"))

            If Math.Abs(SlidingElement(2 * iPartSld2).FrictionSld) < ErrTol Then 'unbonded
                s = s & LPad(5, Format(0, "0"))
                s = s.Replace(",", ".") 'YC 040517
                PrintLine(LFNo, s)
            ElseIf Math.Abs(SlidingElement(2 * iPartSld2).FrictionSld - 0.005) < ErrTol Then   'partially bonded
                s = s & LPad(5, Format(1, "0"))
                s = s.Replace(",", ".") 'YC 040517
                PrintLine(LFNo, s)
                s = LPad(5, Format(0, "0"))
                s = s & LPad(10, Format(0, "0.000E+00"))
                s = s & LPad(10, Format(0, "0.000E+00"))
                s = s & LPad(10, Format(ACLoad(1).f2, "0.000E+00"))
                s = s & LPad(10, Format(0, "0.000E+00"))
                s = s & LPad(10, Format(0, "0.000E+00"))
                s = s.Replace(",", ".") 'YC 040517
                PrintLine(LFNo, s)
            End If
        Next iPartSld2

        'format(1p,6i8)
        For iPartSld = 1 To NPartSld
            Dim iSldEle As Integer
            For iSldEle = 1 To SlidingElement(iPartSld).NSldElement
                s = LPad(8, Format(iSldEle, "0"))
                For IP = 1 To 4
                    s = s & LPad(8, Format(SlidingElement(iPartSld).Element(iSldEle, IP), "0"))
                Next IP
                PrintLine(LFNo, s)
            Next iSldEle
        Next iPartSld
        ' **********************************'SLIDING ELEMENT END*********************************************
    End Sub

    Private Sub WriteLoadCurve(ByRef LFNo As Integer)
        Dim s As String

        'LOAD CURVE format(2i5) and format(2e10.4)
        Dim iLcd, jLcd As Integer
        For iLcd = 1 To NLcd
            s = LPad(5, Format(iLcd, "0")) & LPad(5, Format(NLcd + 1, "0"))
            PrintLine(LFNo, s)
            For jLcd = 1 To NLcd + 1
                s = LPad(10, Format(LCD(iLcd).LdPntTime(jLcd), "0.0000E+00")) & LPad(10, Format(LCD(iLcd).LdPntMag(jLcd), "0.0000E+00"))
                s = s.Replace(",", ".") 'YC 040517
                PrintLine(LFNo, s)
            Next jLcd
        Next iLcd
    End Sub

    Private Sub WriteNodalLoad(ByRef LFNo As Integer)
        Dim s As String

        ' **********************************NODAL LOAD*********************************************
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*------------ CONCENTRATED NODAL LOADS --------------------------------*")
        PrintLine(LFNo, "*")
        'Nodal LOAD format(1p,i8,2i5, e10.3)
        Dim iAC, iNodalLoad As Integer
        For iAC = 1 To NAC
            With NodalLoad(iAC)
                For iNodalLoad = 1 To .NNodalLoad
                    s = LPad(8, Format(.Node(iNodalLoad), "0"))
                    s = s & LPad(5, Format(3, "0")) ' z-diretional load
                    s = s & LPad(5, Format(iAC, "0"))
                    s = s & LPad(10, Format(-.Load(iNodalLoad), "0.000E+00"))
                    s = s.Replace(",", ".") 'YC 040517
                    PrintLine(LFNo, s)
                Next iNodalLoad
            End With
        Next iAC
        ' **********************************NODAL LOAD END*********************************************
    End Sub

    Private Sub WriteNodalTemp(ByRef LFNo As Integer)
        Dim s As String

        ' **********************************NODAL Temperature*********************************************
        PrintLine(LFNo, "*")
        PrintLine(LFNo, "*------------ TEMPERATURE PROFILE DECK --------------------------------*")
        PrintLine(LFNo, "*")
        'Nodal Temperature format(i8,2f10.1)
        Dim iNd As Integer
        iNd = 1
        s = LPad(8, Format(iNd, "0"))
        s = s & LPad(10, Format(Nd(iNd).Temp, "0.0"))
        s = s & LPad(10, Format(0, "0.0"))
        s = s.Replace(",", ".") 'YC 040517
        PrintLine(LFNo, s)

        For iNd = 2 To NNd - 1
            If Nd(iNd).Temp <> Nd(iNd - 1).Temp Then
                s = LPad(8, Format(iNd, "0"))
                s = s & LPad(10, Format(Nd(iNd).Temp, "0.0"))
                s = s & LPad(10, Format(0, "0.0"))
                s = s.Replace(",", ".") 'YC 040517
                PrintLine(LFNo, s)
            End If
        Next

        If Nd(iNd - 1).Temp <> Nd(iNd).Temp Then
            s = LPad(8, Format(iNd - 1, "0"))
            s = s & LPad(10, Format(Nd(iNd - 1).Temp, "0.0"))
            s = s & LPad(10, Format(0, "0.0"))
            s = s.Replace(",", ".") 'YC 040517
            PrintLine(LFNo, s)
        End If

        iNd = NNd
        s = LPad(8, Format(iNd, "0"))
        s = s & LPad(10, Format(Nd(iNd).Temp, "0.0"))
        s = s & LPad(10, Format(0, "0.0"))
        s = s.Replace(",", ".") 'YC 040517
        PrintLine(LFNo, s)
        ' **********************************NODAL Temperature END*********************************************
    End Sub
End Class

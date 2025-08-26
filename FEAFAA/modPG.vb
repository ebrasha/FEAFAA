Option Strict Off
Option Explicit On
Imports System.Runtime.InteropServices
Imports System.Text.RegularExpressions ' YC 070516
Imports Microsoft.VisualBasic.Compatibility

Public Module modAutoMesh

    'Declare Sub INGRIDMAIN Lib "ingrid3.dll" (ByRef SurfTemp As Double, ByRef BotmTemp As Double, ByRef CurlShapePara As Double, ByRef ThermCoef As Double)
    'Declare Sub INGRIDMAIN Lib "ingrid3.dll" (ByRef FlNmString As String)

    <DllImport(("Library\ingrid3.dll"), CallingConvention:=CallingConvention.Winapi)> _
    Public Sub INGRIDMAIN(ByVal WorkingDirectory As String, ByVal LenWorkingDirectory As Integer)     'for INGRID 120413 YGC 111314

        'Public Sub INGRIDMAIN(ByRef FlNmString As String)
        'Public Sub INGRIDMAIN(ByRef WorkingDirectory As String)


    End Sub

    Public F6, F7, F8, F9, F10, F11U, F11L, F12, F13 As Double
    Public I As Integer
    Public XtrMax, YtrMax, XtrMin, YtrMin As Double
    Public Nparts2 As Integer

    Public Structure par1
        Dim a, b, c, d, e, f As Double
        Dim c1, a1, d1 As Double
        Dim c3, a3, d3 As Double
        Dim c7, a7, d7 As Double
    End Structure

    'Public a, b, c, d, e, f As double
    'Public c1, a1, d1 As double
    'Public c3, a3, d3 As double
    'Public c7, a7, d7 As double


    'Structure MyType
    'MyArray() As Integer
    'MyFixedString As String
    'End Structure

    Dim FlNmString As New VB6.FixedLengthString(72)

    Public xdim, ContactArea, TireLoad, LContact, ydim As Double
    Public FileNameBody, Filnam, TextFileName, IngridFile As String
    Public FileNameLength As Short
    Public E1, PR1 As Double
    Public ShrMod As Double
    Public SymmSngl, Interior, SymmDbl, SubBaseExists As Boolean
    Public IndxFndn, IndxSymm, IndxOvrl As Short
    Public Delta1, Delta0, Delta2 As Double
    Public Delta1X, Delta0X, Delta2X As Double
    Public Delta1Y, Delta0Y, Delta2Y As Double
    Public CrackedBase As Boolean
    Public Xfp, Yfp As Double
    Public Xtr(20) As Double
    Public Ytr(20) As Double
    Public RTr(20) As Double
    Public ThTr(20) As Double
    Public Xgrid, Ygrid As Double
    Public Pres As Double
    Public Cstring, Astring, Bstring, Dstring As String
    Public MeshParamY, DepthOfSubgrade, MeshParamX, MeshParam As Double
    Public MeshParamList(12) As Double
    Public ShearModulusX, ShearModulusY As Double
    Public F2, F1, NSlabs As Short
    Public F4, F3, F5 As Double
    Public Factor2, LParam, Factor1, Friction As Double
    Public IFOption As String
    Public CosOfAngle, Angle1, Angle2, Hypot As Double
    Public MeshErr As Short
    Public SymmErr, FileErr As Double
    Public EMod(6) As Double
    Public PoissonsRatio(6) As Double
    Public LayerThickness(6) As Double
    Public Thk4, TS, Thk3, Thk5 As Double
    Public NumSB As Short
    Public InfiniteElementHeight As Double
    Public NumBlcksCrseMeshX, NumBlcksFineMeshX, NumBlcksFineMeshY, NumBlcksCrseMeshY As Short
    Public NumBlcksFineMeshYY, NumBlcksCrseMeshYY As Short
    Public NoPts1, NoPts2 As Short
    Public OneSlab As Boolean

    Public SurfTemp As Double, BotmTemp As Double, LETG As Double 'for INFRID 120413 by YC 111314
    Public CurlShapePara As Double, ThermCoef As Double

    Public MovingDistanceX, MovingDistanceY, MovingDistance, MovingSpeed, StepMovingTime As Double 'YGC 111314
    Public NMovingStep As Integer 'YGC 111314

    ' YC 070516
    Public IndxModes As Integer

    Public INGString As String

    Public NLcd, NSld, NMat, NPart As Integer

    ' YC 040620   
    ' QW 08-23-2017 
    Public NNd As Integer, NBrckEle As Integer

    Public Nd() As FAAMeshClassLib.clsMesh.NodeCharacteristics
    Public BrickElement() As FAAMeshClassLib.clsMesh.BrickElementCharacteristics
    Public SpringType() As FAAMeshClassLib.clsMesh.SpringTypeCharacteristics
    Public SpringElement() As FAAMeshClassLib.clsMesh.SpringElementCharacteristics
    Public SlidingElement() As FAAMeshClassLib.clsMesh.SlidingElementCharacteristics
    Public NodalLoad() As FAAMeshClassLib.clsMesh.NodalLoadCharacteristics
    Public IPC As FEMClassLib.clsFEM.InputCards
    ' End QW 08-23-2017 

    Public NoOutFiles As Boolean, ModelOut As Integer = 1        'QW 09-15-2019

    Const gAC As Integer = 40
    Dim Stress1(gAC * 2), Stress8(gAC * 2) As Double
    Dim StopFEDFAA As Short, FEDFAAStopped As Short
    Dim gDesignType As Integer, iSymCase As Short
    Dim WorkingDir0 As String

    Public gPrintOutFolder As String ' YC 040620-2
    'YC 040620 END


    Public NPartSld, NPartBC, NPartMat As Integer

    Public itpro As Integer

    Public LCD() As FAAMeshClassLib.clsMesh.LoadCurveCharacteristics
    Public SLD() As FAAMeshClassLib.clsMesh.SlidingCharacteristics
    Public MAT() As FAAMeshClassLib.clsMesh.MaterialCharacteristics

    Public PART() As FAAMeshClassLib.clsMesh.PartCharacteristics
    Public PARTSLD() As FAAMeshClassLib.clsMesh.PartSldCharacteristics
    Public PARTBC() As FAAMeshClassLib.clsMesh.PartBCCharacteristics
    Public PARTMAT() As FAAMeshClassLib.clsMesh.PartMatCharacteristics

    Public ACLoad() As FAAMeshClassLib.clsMesh.ACLoadCharacteristics
    ' YC 070516 END



    Sub AMMain(ByRef Filnam As String, ByRef Interior As Boolean)

        'This program creates the input file for ingrid
        Dim param1 As par1

        SymmDbl = False
        MeshErr = 0 : SymmErr = 0 : FileErr = 0
        OneSlab = False

        ' YGC 111314
        If Application.win.IsMovingLoad Then
            MovingDistanceX = CSng(Application.win.txtXCoordMoved.Text) - CSng(Application.win.txtXCoord.Text)
            MovingDistanceY = CSng(Application.win.txtYCoordMoved.Text) - CSng(Application.win.txtYCoord.Text)
            MovingDistance = (MovingDistanceX ^ 2 + MovingDistanceY ^ 2) ^ 0.5 'in inch

            'NMovingStep = MovingDistanceX / 12 / CSng(Application.win.cmbDimX.Text) * CInt(Application.win.cmbMesh.Text) + 1
            NMovingStep = CSng(Application.win.txtNGearPositons.Text)

            MovingSpeed = 1  'CSng(Application.win.txtMovingSpeed.Text) 'in mph (1 mph=17.6 in/s)

            StepMovingTime = MovingDistance / (MovingSpeed * 17.6) / (NMovingStep - 1)  'in sec
        End If
        ' YGC 111314 END

        Call Txtfl(Filnam, TextFileName)
        If FileErr <> 0 Then GoTo 999
        Call SetMesh(param1)

        SubBaseExists = False
        If Application.win.Overlay = True Then

            If Application.win.NumberOfLayers > 4 Then
                SubBaseExists = True
            End If

        ElseIf Application.win.NumberOfLayers > 3 Then
            SubBaseExists = True
        End If

        Call Application.win.GearLoads(param1)

        If MeshErr = 1 Then GoTo 999
        Call Interface_Renamed()

        'Call WriteTextFile()   'YC 070516

        IngridFile = Filnam & ".ing"
        FileOpen(7, IngridFile, OpenMode.Output)
        Call Material()
        Call SlabHexEdg(param1)
        Call BaseMesh(param1)
        Call DummyNodes()

        PrintLine(7, INGString) 'YC 070516

        Call CreateIngParms(INGString) 'YC 070516

        Call Toler()
        FileClose(7)


        IO.File.Delete(IngridFile)   ' YC 040620


        Call WriteTextFile()  'YC 070516

999:
    End Sub

    Sub SlabHexEdg(ByRef p1 As par1)

        Dim NumBlcksCrseMeshNegX As Double
        Dim NumBlcksFineMeshNegX As Double

        'PrintLine(7, "exch 1 2 3"
        NumBlcksFineMeshX = CInt(p1.c / Delta2X)
        NumBlcksFineMeshY = CInt(p1.e / Delta2Y)
        NumBlcksFineMeshNegX = CInt(0.5 * p1.c / Delta2X)
        NumBlcksCrseMeshNegX = CInt((300.0# - 0.5 * p1.c) / Delta0X)
        NumBlcksCrseMeshX = CInt((300.0# - p1.c) / Delta0X)
        NumBlcksCrseMeshY = CInt((150.0# - p1.e) / Delta0Y)

        NoPts1 = CInt(Application.win.cmbMesh.Text) + 1

        'YC 070516
        'NoPts2 = CInt(Application.win.cmbMesh.Text) / 2 + 1    

        Dim YSlabmin As Double  'YC 070516

        If SymmSngl = True Then
            NoPts2 = CInt((NoPts1 - 1) / 2) + 1
            YSlabmin = 0
        Else
            NoPts2 = NoPts1
            YSlabmin = -150
        End If
        'YC 070516 END

        'Modified to suppress parameters YC 070516
        '' Parameters
        'PrintLine(7, "")
        'PrintLine(7, "parameter tk " & -LayerThickness(1))
        'PrintLine(7, "tkol " & -LayerThickness(0))
        'PrintLine(7, "cc " & p1.c)
        'PrintLine(7, "ee " & p1.e)
        'PrintLine(7, "c3 " & p1.c3)
        'PrintLine(7, "c7 " & p1.c7)
        'PrintLine(7, "NFX " & NumBlcksFineMeshX)
        'PrintLine(7, "NFY " & NumBlcksFineMeshY)
        'PrintLine(7, "NFNX " & NumBlcksFineMeshNegX)
        'PrintLine(7, "NCNX " & NumBlcksCrseMeshNegX & ";")

        Dim tk, tkol As Double
        tk = -LayerThickness(1)
        tkol = -LayerThickness(0)
        'Modified to suppress parameters YC 070516 END


        'slab 1 - loaded slab
500:    If Application.win.Overlay = True Then 'create the overlay layer

            'rewritten by YC 070516
            'PrintLine(7, "start")
            'If SymmSngl = True Then
            '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts2 & "; 1 2;")
            'Else
            '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
            'End If
            'PrintLine(7, "0.0 300.")
            'If SymmSngl = True Then
            '    PrintLine(7, "0.0 150.")
            'Else
            '    PrintLine(7, "-150. 150.")
            'End If
            'PrintLine(7, "[-%tk][-%tk - %tkol]") 
            'If SymmSngl = True Then
            '    PrintLine(7, "si+ 1 1 1 2 2 1 10 s 0 0 1")
            'Else
            '    PrintLine(7, "si+ 1 1 1 2 2 1 10 s 0 0 1")
            'End If
            'PrintLine(7, "mate 12")
            'PrintLine(7, "end")
            INGString = INGString & "start" & vbCrLf &
                "1 " & NoPts1 & ";" & vbCrLf &
                "1 " & NoPts2 & ";" & vbCrLf &
                "1 2;" & vbCrLf &
                "0.0 300." & vbCrLf &
                YSlabmin & " 150" & vbCrLf &
                (-tk) & " " & (-tk - tkol) & vbCrLf &
                "si+ 1 1 1 2 2 1 10 s 0 0 1" & vbCrLf &
                "mate 12" & vbCrLf &
                "end" & vbCrLf & vbCrLf
            'rewritten by YC 070516 END

        End If

        'create the base slab, or slab if no overlay
        'rewritten by YC 070516
        'PrintLine(7, "start")
        'If SymmSngl = True Then
        '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts2 & "; 1 2;")
        'Else
        '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
        'End If
        'PrintLine(7, "0.0 300.")
        'If SymmSngl = True Then
        '    PrintLine(7, "0.0 150.")
        'Else
        '    PrintLine(7, "-150. 150.")
        'End If
        'PrintLine(7, "0 [-%tk]") 

        'If SymmSngl = True Then
        '    PrintLine(7, "si- 1 1 1 2 2 1 1 s 0 0 1")
        'Else
        '    PrintLine(7, "si- 1 1 1 2 2 1 1 s 0 0 1")
        'End If
        'If Application.win.Overlay = True Then 'define a contact surface on top of the base slab
        '    If SymmSngl = True Then
        '        PrintLine(7, "si- 1 1 2 2 2 2 10 m 0 0 1")
        '    Else
        '        PrintLine(7, "si- 1 1 2 2 2 2 10 m 0 0 1")
        '    End If
        'End If
        'PrintLine(7, "mate 1")
        'PrintLine(7, "end")
        INGString = INGString & "start" & vbCrLf &
                "1 " & NoPts1 & ";" & vbCrLf &
                "1 " & NoPts2 & ";" & vbCrLf &
                "1 2;" & vbCrLf &
                "0.0 300." & vbCrLf &
                YSlabmin & " 150" & vbCrLf &
                "0 " & (-tk) & vbCrLf &
                "si- 1 1 1 2 2 1 1 s 0 0 1" & vbCrLf

        If Application.win.Overlay = True Then INGString = INGString & "si- 1 1 2 2 2 2 10 m 0 0 1" & vbCrLf

        INGString = INGString &
                "mate 1" & vbCrLf &
                "end" & vbCrLf & vbCrLf
        'rewritten by YC 070516 END

        'If OneSlab = True Then GoTo 100
        If NSlabs = 1 Then GoTo 100
        'Modified for variable slab number by YGC 060110


        'slab 2 - unloaded slab
        'PrintLine(7, "")
        If Application.win.Overlay = True Then 'create the overlay layer

            'rewritten by YC 070516
            'PrintLine(7, "start")
            'If SymmSngl = True Then
            '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts2 & "; 1 2;")
            'Else
            '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
            'End If
            'PrintLine(7, "-300.0 -0.02")
            'If SymmSngl = True Then
            '    PrintLine(7, "0.0 150.")
            'Else
            '    PrintLine(7, "-150. 150.")
            'End If
            'PrintLine(7, "[-%tk][-%tk - %tkol]")
            'If SymmSngl = True Then
            '    PrintLine(7, "si+ 1 1 1 2 2 1 11 s 0 0 1")
            'Else
            '    PrintLine(7, "si+ 1 1 1 2 2 1 11 s 0 0 1")
            'End If
            'PrintLine(7, "mate 12")
            'PrintLine(7, "end")
            INGString = INGString & "start" & vbCrLf &
                "1 " & NoPts1 & ";" & vbCrLf &
                "1 " & NoPts2 & ";" & vbCrLf &
                "1 2;" & vbCrLf &
                "-300.0 -0.02" & vbCrLf &
                YSlabmin & " 150" & vbCrLf &
                (-tk) & " " & (-tk - tkol) & vbCrLf &
                "si+ 1 1 1 2 2 1 11 s 0 0 1" & vbCrLf &
                "mate 12" & vbCrLf &
                "end" & vbCrLf & vbCrLf

            'rewritten by YC 070516 END

        End If

        'create the base slab, or slab if no overlay
        'rewritten by YC 070516
        'PrintLine(7, "start")
        'If SymmSngl = True Then
        '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts2 & "; 1 2;")
        'Else
        '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
        'End If
        'PrintLine(7, "-300.0 -0.02")
        'If SymmSngl = True Then
        '    PrintLine(7, "0.0 150.")
        'Else
        '    PrintLine(7, "-150. 150.")
        'End If
        'PrintLine(7, "0 [-%tk]") 
        'If SymmSngl = True Then
        '    PrintLine(7, "si- 1 1 1 2 2 1 2 s 0 0 1")
        'Else
        '    PrintLine(7, "si- 1 1 1 2 2 1 2 s 0 0 1")
        'End If
        'If Application.win.Overlay = True Then 'define a contact surface on top of the base slab
        '    If SymmSngl = True Then
        '        PrintLine(7, "si- 1 1 2 2 2 2 11 m 0 0 1")
        '    Else
        '        PrintLine(7, "si- 1 1 2 2 2 2 11 m 0 0 1")
        '    End If
        'End If
        'PrintLine(7, "mate 1")
        'PrintLine(7, "end")
        INGString = INGString & "start" & vbCrLf &
                "1 " & NoPts1 & ";" & vbCrLf &
                "1 " & NoPts2 & ";" & vbCrLf &
                "1 2;" & vbCrLf &
                "-300.0 -0.02" & vbCrLf &
                YSlabmin & " 150" & vbCrLf &
                "0 " & (-tk) & vbCrLf &
                "si- 1 1 1 2 2 1 2 s 0 0 1" & vbCrLf

        If Application.win.Overlay = True Then INGString = INGString & "si- 1 1 2 2 2 2 11 m 0 0 1" & vbCrLf

        INGString = INGString &
                "mate 1" & vbCrLf &
                "end" & vbCrLf & vbCrLf

        'rewritten by YC 070516 END

        If NSlabs = 2 Then GoTo 100
        'Modified for variable slab number by YGC 060110


        'slab 3
        'PrintLine(7, "")
        If Application.win.Overlay = True Then 'create the overlay layer

            'rewritten by YC 070516 END
            'PrintLine(7, "start")
            'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
            'PrintLine(7, "0 300.0")
            'PrintLine(7, "150.02 450.0")
            'PrintLine(7, "[-%tk][-%tk - %tkol]")  
            'PrintLine(7, "si+ 1 1 1 2 2 1 12 s 0 0 1")
            'PrintLine(7, "mate 12")
            'PrintLine(7, "end")
            INGString = INGString & "start" & vbCrLf &
                "1 " & NoPts1 & ";" & vbCrLf &
                "1 " & NoPts2 & ";" & vbCrLf &
                "1 2;" & vbCrLf &
                "0 300.0" & vbCrLf &
                "150.02 450.0" & vbCrLf &
                (-tk) & " " & (-tk - tkol) & vbCrLf &
                "si+ 1 1 1 2 2 1 12 s 0 0 1" & vbCrLf &
                "mate 12" & vbCrLf &
                "end" & vbCrLf & vbCrLf
            'rewritten by YC 070516 END

        End If

        'create the base slab, or slab if no overlay
        'rewritten by YC 070516
        'PrintLine(7, "start")
        'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
        'PrintLine(7, "0. 300.0")
        'PrintLine(7, "150.02 450.0")
        'PrintLine(7, "0 [-%tk]")    
        'PrintLine(7, "si- 1 1 1 2 2 1 3 s 0 0 1")
        'If Application.win.Overlay = True Then 'define a contact surface on top of the base slab
        '    PrintLine(7, "si- 1 1 2 2 2 2 12 m 0 0 1")
        'End If
        'PrintLine(7, "mate 1")
        'PrintLine(7, "end")
        INGString = INGString & "start" & vbCrLf &
        "1 " & NoPts1 & ";" & vbCrLf &
        "1 " & NoPts2 & ";" & vbCrLf &
        "1 2;" & vbCrLf &
        "0. 300.0" & vbCrLf &
        "150.02 450.0" & vbCrLf &
        "0 " & (-tk) & vbCrLf &
        "si- 1 1 1 2 2 1 3 s 0 0 1" & vbCrLf

        If Application.win.Overlay = True Then INGString = INGString & "si- 1 1 2 2 2 2 12 m 0 0 1" & vbCrLf

        INGString = INGString &
                "mate 1" & vbCrLf &
                "end" & vbCrLf & vbCrLf
        'rewritten by YC 070516 END


        'slab 4
        'PrintLine(7, "")
        If Application.win.Overlay = True Then 'create the overlay layer

            'rewritten by YC 070516
            'PrintLine(7, "start")
            'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
            'PrintLine(7, "-300.0 -0.02")
            'PrintLine(7, "150.02 450.0")
            'PrintLine(7, "[-%tk][-%tk - %tkol]") 
            'PrintLine(7, "si+ 1 1 1 2 2 1 13 s 0 0 1")
            'PrintLine(7, "mate 12")
            'PrintLine(7, "end")
            INGString = INGString & "start" & vbCrLf &
                    "1 " & NoPts1 & ";" & vbCrLf &
                    "1 " & NoPts2 & ";" & vbCrLf &
                    "1 2;" & vbCrLf &
                    "-300.0 -0.02" & vbCrLf &
                    "150.02 450.0" & vbCrLf &
                    (-tk) & " " & (-tk - tkol) & vbCrLf &
                    "si+ 1 1 1 2 2 1 13 s 0 0 1" & vbCrLf &
                    "mate 12" & vbCrLf &
                    "end" & vbCrLf & vbCrLf
            'rewritten by YC 070516 END 

        End If

        'create the base slab, or slab if no overlay
        'rewritten by YC 070516
        'PrintLine(7, "start")
        'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
        'PrintLine(7, "-300.0 -0.02")
        'PrintLine(7, "150.02 450.0")
        'PrintLine(7, "0 [-%tk]")   
        'PrintLine(7, "si- 1 1 1 2 2 1 4 s 0 0 1")
        'If Application.win.Overlay = True Then 'define a contact surface on top of the base slab
        '    PrintLine(7, "si- 1 1 2 2 2 2 13 m 0 0 1")
        'End If
        'PrintLine(7, "mate 1")
        'PrintLine(7, "end")
        INGString = INGString & "start" & vbCrLf &
                "1 " & NoPts1 & ";" & vbCrLf &
                "1 " & NoPts2 & ";" & vbCrLf &
                "1 2;" & vbCrLf &
                "-300.0 -0.02" & vbCrLf &
                "150.02 450.0" & vbCrLf &
                "0 " & (-tk) & vbCrLf &
                "si- 1 1 1 2 2 1 4 s 0 0 1" & vbCrLf

        If Application.win.Overlay = True Then INGString = INGString & "si- 1 1 2 2 2 2 13 m 0 0 1" & vbCrLf

        INGString = INGString &
                "mate 1" & vbCrLf &
                "end" & vbCrLf & vbCrLf
        'rewritten by YC 070516 END

        If NSlabs = 4 Then GoTo 100 'Modified for variable slab number by YGC 060110


        'slab 5
        'PrintLine(7, "")
        If Application.win.Overlay = True Then 'create the overlay layer

            'rewritten by YC 070516
            'PrintLine(7, "start")
            'If SymmSngl = True Then
            '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts2 & "; 1 2;")
            'Else
            '    PrintLine(7, "1 " & NoPts1 & "; ")
            '    PrintLine(7, "1 " & NoPts1 & " ; 1 2;")
            'End If
            'PrintLine(7, "300.02 600.0")
            'If SymmSngl = True Then
            '    PrintLine(7, "0.0 150.")
            'Else
            '    PrintLine(7, "-150. 150.")
            'End If
            'PrintLine(7, "[-%tk][-%tk - %tkol]")  
            'If SymmSngl = True Then
            '    PrintLine(7, "si+ 1 1 1 2 2 1 14 s 0 0 1")
            'Else
            '    PrintLine(7, "si+ 1 1 1 2 2 1 14 s 0 0 1")
            'End If
            'PrintLine(7, "mate 12")
            'PrintLine(7, "end")
            INGString = INGString & "start" & vbCrLf &
                    "1 " & NoPts1 & ";" & vbCrLf &
                    "1 " & NoPts2 & ";" & vbCrLf &
                    "1 2;" & vbCrLf &
                    "300.02 600.0" & vbCrLf &
                    "-150. 150." & vbCrLf &
                     (-tk) & " " & (-tk - tkol) & vbCrLf &
                    "si+ 1 1 1 2 2 1 14 s 0 0 1" & vbCrLf &
                    "mate 12" & vbCrLf &
                    "end" & vbCrLf & vbCrLf
            'rewritten by YC 070516 END

        End If

        'create the base slab, or slab if no overlay
        'rewritten by YC 070516
        'PrintLine(7, "start")
        'If SymmSngl = True Then
        '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts2 & "; 1 2;")
        'Else
        '    PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & " ; 1 2;")
        'End If
        'PrintLine(7, "300.02 600.0")
        'If SymmSngl = True Then
        '    PrintLine(7, "0.0 150.")
        'Else
        '    PrintLine(7, "-150. 150.")
        'End If
        'PrintLine(7, "0 [-%tk]")  
        'If SymmSngl = True Then
        '    PrintLine(7, "si- 1 1 1 2 2 1 5 s 0 0 1")
        'Else
        '    PrintLine(7, "si- 1 1 1 2 2 1 5 s 0 0 1")
        'End If
        'If Application.win.Overlay = True Then 'define a contact surface on top of the base slab
        '    If SymmSngl = True Then
        '        PrintLine(7, "si- 1 1 2 2 2 2 14 m 0 0 1")
        '    Else
        '        PrintLine(7, "si- 1 1 2 2 2 2 14 m 0 0 1")
        '    End If

        'End If
        'PrintLine(7, "mate 1")
        'PrintLine(7, "end")
        INGString = INGString & "start" & vbCrLf &
                "1 " & NoPts1 & ";" & vbCrLf &
                "1 " & NoPts2 & ";" & vbCrLf &
                "1 2;" & vbCrLf &
                "300.02 600.0" & vbCrLf &
                "-150. 150" & vbCrLf &
                "0 " & (-tk) & vbCrLf &
                "si- 1 1 1 2 2 1 5 s 0 0 1" & vbCrLf

        If Application.win.Overlay = True Then INGString = INGString & "si- 1 1 2 2 2 2 14 m 0 0 1" & vbCrLf

        INGString = INGString &
                "mate 1" & vbCrLf &
                "end" & vbCrLf & vbCrLf
        'rewritten by YC 070516


        'slab 6
        'PrintLine(7, "")
        If Application.win.Overlay = True Then 'create the overlay layer

            'rewritten by YC 070516
            'PrintLine(7, "start")
            'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
            'PrintLine(7, "300.02 600.0")
            'PrintLine(7, "150.02 450.0")
            'PrintLine(7, "[-%tk][-%tk - %tkol]") 
            'PrintLine(7, "si+ 1 1 1 2 2 1 15 s 0 0 1")
            'PrintLine(7, "mate 12")
            'PrintLine(7, "end")
            INGString = INGString & "start" & vbCrLf &
                    "1 " & NoPts1 & ";" & vbCrLf &
                    "1 " & NoPts2 & ";" & vbCrLf &
                    "1 2;" & vbCrLf &
                    "300.02 600.0" & vbCrLf &
                    "150.02 450.0" & vbCrLf &
                    (-tk) & " " & (-tk - tkol) & vbCrLf &
                    "si+ 1 1 1 2 2 1 15 s 0 0 1" & vbCrLf &
                    "mate 12" & vbCrLf &
                    "end" & vbCrLf & vbCrLf
            'rewritten by YC 070516


        End If


        'create the base slab, or slab if no overlay
        'rewritten by YC 070516
        'PrintLine(7, "start")
        'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
        'PrintLine(7, "300.02 600.0")
        'PrintLine(7, "150.02 450.0")
        'PrintLine(7, "0 [-%tk]")  
        'PrintLine(7, "si- 1 1 1 2 2 1 6 s 0 0 1")
        'If Application.win.Overlay = True Then 'define a contact surface on top of the base slab
        '    PrintLine(7, "si- 1 1 2 2 2 2 15 m 0 0 1")
        'End If
        'PrintLine(7, "mate 1")
        'PrintLine(7, "end")
        INGString = INGString & "start" & vbCrLf &
        "1 " & NoPts1 & ";" & vbCrLf &
        "1 " & NoPts2 & ";" & vbCrLf &
        "1 2;" & vbCrLf &
        "300.02 600.0" & vbCrLf &
        "150.02 450.0" & vbCrLf &
        "0 " & (-tk) & vbCrLf &
        "si- 1 1 1 2 2 1 6 s 0 0 1" & vbCrLf

        If Application.win.Overlay = True Then INGString = INGString & "si- 1 1 2 2 2 2 15 m 0 0 1" & vbCrLf

        INGString = INGString &
                "mate 1" & vbCrLf &
                "end" & vbCrLf & vbCrLf
        'rewritten by YC 070516 END

        If NSlabs = 6 Then GoTo 100 'Modified for variable slab number by YGC 060110


        If SymmSngl = False Then 'slabs 7 8 and 9 for case of no symmetry only

            'slab 7
            'PrintLine(7, "")
            If Application.win.Overlay = True Then 'create the overlay layer

                'rewritten by YC 070516
                'PrintLine(7, "start")
                'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
                'PrintLine(7, "0 300.0")
                'PrintLine(7, "-450.0 -150.02")
                'PrintLine(7, "[-%tk][-%tk - %tkol]")  
                'PrintLine(7, "si+ 1 1 1 2 2 1 16 s 0 0 1")
                'PrintLine(7, "mate 12")
                'PrintLine(7, "end")
                INGString = INGString & "start" & vbCrLf &
                        "1 " & NoPts1 & ";" & vbCrLf &
                        "1 " & NoPts2 & ";" & vbCrLf &
                        "1 2;" & vbCrLf &
                        "0 300.0" & vbCrLf &
                        "-450.0 -150.02" & vbCrLf &
                        (-tk) & " " & (-tk - tkol) & vbCrLf &
                        "si+ 1 1 1 2 2 1 16 s 0 0 1" & vbCrLf &
                        "mate 12" & vbCrLf &
                        "end" & vbCrLf & vbCrLf
                'rewritten by YC 070516 END

            End If

            'create the base slab, or slab if no overlay
            'rewritten by YC 070516
            'PrintLine(7, "start")
            'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
            'PrintLine(7, "0. 300.0")
            'PrintLine(7, "-450.0 -150.02")
            'PrintLine(7, "0 [-%tk]")  
            'PrintLine(7, "si- 1 1 1 2 2 1 7 s 0 0 1")
            'If Application.win.Overlay = True Then 'define a contact surface on top of the base slab
            '    PrintLine(7, "si- 1 1 2 2 2 2 16 m 0 0 1")
            'End If
            'PrintLine(7, "mate 1")
            'PrintLine(7, "end")
            'rewritten by YC 070516 END
            INGString = INGString & "start" & vbCrLf &
                        "1 " & NoPts1 & ";" & vbCrLf &
                        "1 " & NoPts2 & ";" & vbCrLf &
                        "1 2;" & vbCrLf &
                        "0. 300.0" & vbCrLf &
                        "-450.0 -150.02" & vbCrLf &
                        "0 " & (-tk) & vbCrLf &
                        "si- 1 1 1 2 2 1 7 s 0 0 1" & vbCrLf

            If Application.win.Overlay = True Then INGString = INGString & "si- 1 1 2 2 2 2 16 m 0 0 1" & vbCrLf

            INGString = INGString &
                    "mate 1" & vbCrLf &
                    "end" & vbCrLf & vbCrLf


            'slab 8
            PrintLine(7, "")
            If Application.win.Overlay = True Then 'create the overlay layer

                'rewritten by YC 070516
                'PrintLine(7, "start")
                'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
                'PrintLine(7, "-300.0 -0.02")
                'PrintLine(7, "-450.0 -150.02")
                'PrintLine(7, "[-%tk][-%tk - %tkol]")    
                'PrintLine(7, "si+ 1 1 1 2 2 1 17 s 0 0 1")
                'PrintLine(7, "mate 12")
                'PrintLine(7, "end")
                INGString = INGString & "start" & vbCrLf &
                        "1 " & NoPts1 & ";" & vbCrLf &
                        "1 " & NoPts2 & ";" & vbCrLf &
                        "1 2;" & vbCrLf &
                        "-300.0 -0.02" & vbCrLf &
                        "-450.0 -150.02" & vbCrLf &
                        (-tk) & " " & (-tk - tkol) & vbCrLf &
                        "si+ 1 1 1 2 2 1 17 s 0 0 1" & vbCrLf &
                        "mate 12" & vbCrLf &
                        "end" & vbCrLf & vbCrLf
                'rewritten by YC 070516 END

            End If


            'create the base slab, or slab if no overlay
            'rewritten by YC 070516
            'PrintLine(7, "start")
            'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
            'PrintLine(7, "-300.0 -0.02")
            'PrintLine(7, "-450.0 -150.02")
            'PrintLine(7, "0 [-%tk]")    
            'PrintLine(7, "si- 1 1 1 2 2 1 8 s 0 0 1")
            'If Application.win.Overlay = True Then 'define a contact surface on top of the base slab
            '    PrintLine(7, "si- 1 1 2 2 2 2 17 m 0 0 1")
            'End If
            'PrintLine(7, "mate 1")
            'PrintLine(7, "end")
            INGString = INGString & "start" & vbCrLf &
            "1 " & NoPts1 & ";" & vbCrLf &
            "1 " & NoPts2 & ";" & vbCrLf &
            "1 2;" & vbCrLf &
            "-300.0 -0.02" & vbCrLf &
            "-450.0 -150.02" & vbCrLf &
            "0 " & (-tk) & vbCrLf &
            "si- 1 1 1 2 2 1 8 s 0 0 1" & vbCrLf

            If Application.win.Overlay = True Then INGString = INGString & "si- 1 1 2 2 2 2 17 m 0 0 1" & vbCrLf

            INGString = INGString &
                    "mate 1" & vbCrLf &
                    "end" & vbCrLf & vbCrLf
            'rewritten by YC 070516 END


            'slab 9
            PrintLine(7, "")
            If Application.win.Overlay = True Then 'create the overlay layer

                'rewritten by YC 070516 
                'PrintLine(7, "start")
                'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
                'PrintLine(7, "300.02 600.0")
                'PrintLine(7, "-450.0 -150.02")
                'PrintLine(7, "[-%tk][-%tk - %tkol]")  
                'PrintLine(7, "si+ 1 1 1 2 2 1 18 s 0 0 1")
                'PrintLine(7, "mate 12")
                'PrintLine(7, "end")
                'rewritten by YC 070516 END
                INGString = INGString & "start" & vbCrLf &
                        "1 " & NoPts1 & ";" & vbCrLf &
                        "1 " & NoPts2 & ";" & vbCrLf &
                        "1 2;" & vbCrLf &
                        "300.02 600.0" & vbCrLf &
                        "-450.0 -150.02" & vbCrLf &
                        (-tk) & " " & (-tk - tkol) & vbCrLf &
                        "si+ 1 1 1 2 2 1 18 s 0 0 1" & vbCrLf &
                        "mate 12" & vbCrLf &
                        "end" & vbCrLf & vbCrLf

            End If

            'create the base slab, or slab if no overlay
            'rewritten by YC 070516
            'PrintLine(7, "start")
            'PrintLine(7, "1 " & NoPts1 & "; 1 " & NoPts1 & "; 1 2;")
            'PrintLine(7, "300.02 600.0")
            'PrintLine(7, "-450.0 -150.02")
            'PrintLine(7, "0 [-%tk]")      
            'PrintLine(7, "si- 1 1 1 2 2 1 9 s 0 0 1")
            'If Application.win.Overlay = True Then 'define a contact surface on top of the base slab
            '    PrintLine(7, "si- 1 1 2 2 2 2 18 m 0 0 1")
            'End If
            'PrintLine(7, "mate 1")
            'PrintLine(7, "end")
            INGString = INGString & "start" & vbCrLf &
                    "1 " & NoPts1 & ";" & vbCrLf &
                    "1 " & NoPts2 & ";" & vbCrLf &
                    "1 2;" & vbCrLf &
                    "300.02 600.0" & vbCrLf &
                    "-450.0 -150.02" & vbCrLf &
                    "0 " & (-tk) & vbCrLf &
                    "si- 1 1 1 2 2 1 9 s 0 0 1" & vbCrLf

            If Application.win.Overlay = True Then INGString = INGString & "si- 1 1 2 2 2 2 18 m 0 0 1" & vbCrLf

            INGString = INGString &
                    "mate 1" & vbCrLf &
                    "end" & vbCrLf & vbCrLf
            'rewritten by YC 070516 END

        End If ' end for SymmSngl=false

100:

    End Sub

    Sub Interface_Renamed()
        'This subroutine calculates the stiffness multiplier for weak bond interface model.
        'Tangential interface stiffness is given by:
        'KTang = Factor1 * Factor2 * KInterface
        'where KInterface = interface stiffness computed by NIKE3D contact surface routine (Type3)
        'Factor1 = NIKE3D Penalty Stiffness Scale Factor (=10.0 by default)
        'Factor2 = LParam/(1 - LParam) for 0.001 <= LParam <= 0.99
        '   = 100 for LParam > 99
        '   = 0.001 for LParam < 0.001
        'LParam is the interface input variable that ranges from 0 to 1
        'with LParam = 0 for fully unbonded and LParam = 1 for fully bonded.
        LParam = CSng(Application.win.IFParam) / 10.0#
        If LParam < 0.001 Then
            Factor2 = 0.001
        ElseIf LParam > 0.99 Then
            Factor2 = 100.0#
        Else
            Factor2 = LParam / (1.0# - LParam)
        End If
        Factor1 = 10.0#
    End Sub

    Sub Material()

        PrintLine(7, "Rigid Vehicle Pavement")
        PrintLine(7, "")
        PrintLine(7, "nk3d")
        PrintLine(7, "anal stat")
        PrintLine(7, "prcd 1 2 0 1.0 1.0 1.0")

        'YGC 111314
        INGString = Nothing 'YC 070516
        If Application.win.IsMovingLoad Then

            Dim ii, jj As Integer
            Dim SS1, SS2 As String

            For ii = 1 To NMovingStep
                SS1 = "lcd " & CStr(ii) & " " & CStr(NMovingStep + 1)

                'PrintLine(7, SS1)       'rewritten for MeshclassLib by YC 070516
                INGString = INGString & SS1 & vbCrLf

                For jj = 0 To NMovingStep

                    'If jj Mod 8 = 0 And jj > 0 Then PrintLine(7)    'rewritten for MeshclassLib by YC 070516
                    If jj Mod 8 = 0 And jj > 0 Then INGString = INGString & vbCrLf

                    If jj = ii Then
                        SS2 = CSng(jj).ToString("0.0").Replace(",", ".") & " 1.0 "
                    Else
                        SS2 = CSng(jj).ToString("0.0").Replace(",", ".") & " 0.0 "
                    End If

                    'Print(7, SS2)       'rewritten for MeshclassLib by YC 070516
                    INGString = INGString & SS2

                Next jj

                'PrintLine(7)    'rewritten for MeshclassLib by YC 070516
                INGString = INGString & vbCrLf

            Next ii

        Else

            'PrintLine(7, "lcd 2 2 0 0 1.0 1.0")     'rewritten for MeshclassLib by YC 070516
            INGString = "lcd 2 2" & vbCrLf & "0 0 1.0 1.0" & vbCrLf

        End If

        'PrintLine(7)   'rewritten for MeshclassLib by YC 070516
        INGString = INGString & vbCrLf

        'YGC 111314 END

        IFOption = "sv"

        If Factor2 > 0.0010001 Then
            Friction = 0.005
        Else
            Friction = 0.0#
        End If


        If Factor2 > 99.0# Then IFOption = "tied"

        'Modified to compact the code by YGC 052810
        Dim I As Integer 'YC 070516
        For I = 1 To NSlabs
            'PrintLine(7, "si " & I & " " & IFOption & " pnlt " & Factor1 & " fric " & Friction & " nomerge;")    'rewritten for MeshclassLib by YC 070516
            INGString = INGString & "si " & I & " " & IFOption & " pnlt " & Factor1 & " fric " & Friction & " nomerge;" & vbCrLf
        Next I


        If Application.win.Overlay = True Then

            For I = 10 To 9 + NSlabs
                'PrintLine(7, "si " & I & " " & IFOption & " pnlt " & Factor1 & " fric " & Friction & " nomerge;")    'rewritten for MeshclassLib by YC 070516
                INGString = INGString & "si " & I & " " & IFOption & " pnlt " & Factor1 & " fric " & Friction & " nomerge;" & vbCrLf
            Next I
        End If
        'Modify end by YGC 072910

        'PrintLine(7, "")    'YC 070516
        INGString = INGString & vbCrLf

        'concrete slab material
        'Modified by YGC 121710

        'If frmGear.Modes = True Then
        '    PrintLine(7, "mat 1 1")
        '    PrintLine(7, "e " & EMod(1))
        '    PrintLine(7, "pr " & PoissonsRatio(1))
        '    PrintLine(7, "ro 8.3912e-2")
        '    PrintLine(7, "")
        'Else
        '    PrintLine(7, "mat 1 1")
        '    PrintLine(7, "shell")
        '    PrintLine(7, "thick " & LayerThickness(1))
        '    PrintLine(7, "shloc -1")
        '    PrintLine(7, "e " & EMod(1))
        '    PrintLine(7, "pr " & PoissonsRatio(1))
        '    PrintLine(7, "ro 8.3912e-2")
        '    PrintLine(7, "")
        'End If

        'Replaced for marking thermal elastic analysis by YGC 062613 
        If Application.win.chkTempLoad.IsChecked = True Then

            'PrintLine(7, "teo 1 tpro 1")   'rewritten for MeshclassLib by YC 070516
            'PrintLine(7, "")
            INGString = INGString & "teo 1 tpro 1" & vbCrLf & vbCrLf

        End If

        'If Application.win.chkTempLoad.IsChecked = False Then   'corrected to asign temperatue load only to overlay by YC 072516 070516
        If Application.win.chkTempLoad.IsChecked = False Or Application.win.Overlay = True Then

            'rewritten for MeshclassLib by YC 070516
            'PrintLine(7, "mat 1 1")
            'PrintLine(7, "e " & EMod(1))
            'PrintLine(7, "pr " & PoissonsRatio(1))
            'PrintLine(7, "ro 8.3912e-2")
            'PrintLine(7, "")
            INGString = INGString & "mat 1 1" & vbCrLf &
                "e " & EMod(1) & vbCrLf &
                "pr " & PoissonsRatio(1) & vbCrLf &
                "ro 8.3912e-2" & vbCrLf & vbCrLf
            'rewritten for MeshclassLib by YC 070516 END

        Else

            'rewritten for MeshclassLib by YC 070516
            ''PrintLine(7, "teo 1 tpro 1") 'Replaced for marking thermal elastic analysis by YGC 062613 
            ''PrintLine(7, "")
            'PrintLine(7, "mat 1 4")
            'PrintLine(7, "npts 2")
            ''PrintLine(7, "temp -100 100")'Modified for thermal elastic analysis by YGC 062613
            'PrintLine(7, "temp -200 200")
            ''Modified for varied modulus by YGC 050311
            ''PrintLine(7, "e 4.0e+06 4.0e+06")
            ''PrintLine(7, "pr 0.15 0.15")
            'PrintLine(7, "e " & EMod(1) & " " & EMod(1))
            'PrintLine(7, "pr " & PoissonsRatio(1) & " " & PoissonsRatio(1))
            ''Modify end by YGC 050311
            'PrintLine(7, "alpha 5.0e-06 5.0e-06")   
            'PrintLine(7, "ro 8.3912e-2")
            'PrintLine(7, "")

            ThermCoef = CSng(Application.win.txtThermCoef.Text)

            INGString = INGString & "mat 1 4" & vbCrLf &
                "npts 2" & vbCrLf &
                "temp -200 200" & vbCrLf &
                "e " & EMod(1) & " " & EMod(1) & vbCrLf &
                "pr " & PoissonsRatio(1) & " " & PoissonsRatio(1) & vbCrLf &
                "alpha " & ThermCoef & " " & ThermCoef & vbCrLf &
                "ro 8.3912e-2" & vbCrLf & vbCrLf
            'corrected to enable changing thermal coefficent by user for YC 072516 070516
            'rewritten for MeshclassLib by YC 070516 END

        End If
        'Modify end by YGC 121710

        'upper subbase material
        'rewritten for MeshclassLib by YC 070516
        'PrintLine(7, "mat 2 1")
        'PrintLine(7, "e " & EMod(2))
        'PrintLine(7, "pr " & PoissonsRatio(2))
        'PrintLine(7, "ro 1.872e-4")
        'PrintLine(7, "")
        INGString = INGString & "mat 2 1" & vbCrLf &
           "e " & EMod(2) & vbCrLf &
           "pr " & PoissonsRatio(2) & vbCrLf &
           "ro 1.872e-4" & vbCrLf & vbCrLf
        'rewritten for MeshclassLib by YC 070516 END

        '2nd subbase material
        'rewritten for MeshclassLib by YC 070516
        'PrintLine(7, "mat 3 1")
        'PrintLine(7, "e " & EMod(3))
        'PrintLine(7, "pr " & PoissonsRatio(3))
        'PrintLine(7, "ro 1.872e-4")
        'PrintLine(7, "")
        INGString = INGString & "mat 3 1" & vbCrLf &
           "e " & EMod(3) & vbCrLf &
           "pr " & PoissonsRatio(3) & vbCrLf &
           "ro 1.872e-4" & vbCrLf & vbCrLf
        'rewritten for MeshclassLib by YC 070516 END

        '3rd subbase material
        'rewritten for MeshclassLib by YC 070516
        'PrintLine(7, "mat 4 1")
        'PrintLine(7, "e " & EMod(4))
        'PrintLine(7, "pr " & PoissonsRatio(4))
        'PrintLine(7, "ro 1.872e-4")
        'PrintLine(7, "")
        INGString = INGString & "mat 4 1" & vbCrLf &
           "e " & EMod(4) & vbCrLf &
           "pr " & PoissonsRatio(4) & vbCrLf &
           "ro 1.872e-4" & vbCrLf & vbCrLf
        'rewritten for MeshclassLib by YC 070516 END

        '4th subbase material
        'rewritten for MeshclassLib by YC 070516
        'PrintLine(7, "mat 5 1")
        'PrintLine(7, "e " & EMod(5))
        'PrintLine(7, "pr " & PoissonsRatio(5))
        'PrintLine(7, "ro 1.872e-4")
        'PrintLine(7, "")
        INGString = INGString & "mat 5 1" & vbCrLf &
           "e " & EMod(5) & vbCrLf &
           "pr " & PoissonsRatio(5) & vbCrLf &
           "ro 1.872e-4" & vbCrLf & vbCrLf
        'rewritten for MeshclassLib by YC 070516 END

        'subgrade material
        'If frmGear.Winkler = False Then

        'rewritten for MeshclassLib by YC 070516
        'PrintLine(7, "mat 6 1")
        'PrintLine(7, "e " & EMod(6))
        'PrintLine(7, "pr " & PoissonsRatio(6))
        'PrintLine(7, "ro 1.872e-4")
        'PrintLine(7, "")
        INGString = INGString & "mat 6 1" & vbCrLf &
           "e " & EMod(6) & vbCrLf &
           "pr " & PoissonsRatio(6) & vbCrLf &
           "ro 1.872e-4" & vbCrLf & vbCrLf
        'rewritten for MeshclassLib by YC 070516 END

        'End If

        If Application.win.InfiniteElement = True Then

            'rewritten for MeshclassLib by YC 070516
            'PrintLine(7, "mat 7 56") 'infinite direction downward (-z)
            'PrintLine(7, "e " & EMod(6))
            'PrintLine(7, "pr " & PoissonsRatio(6))
            'PrintLine(7, "ro 6.0")
            'PrintLine(7, "")
            ''
            'PrintLine(7, "mat 8 56") 'infinite direction +x
            'PrintLine(7, "e " & EMod(6))
            'PrintLine(7, "pr " & PoissonsRatio(6))
            'PrintLine(7, "ro 1.0")
            'PrintLine(7, "")
            ''
            'PrintLine(7, "mat 9 56") 'infinite direction +y
            'PrintLine(7, "e " & EMod(6))
            'PrintLine(7, "pr " & PoissonsRatio(6))
            'PrintLine(7, "ro 2.0")
            'PrintLine(7, "")
            ''
            'PrintLine(7, "mat 10 56") 'infinite direction -x
            'PrintLine(7, "e " & EMod(6))
            'PrintLine(7, "pr " & PoissonsRatio(6))
            'PrintLine(7, "ro 4.0")
            'PrintLine(7, "")
            ''
            ''If SymmSngl = False Then
            'PrintLine(7, "mat 11 56") 'infinite direction -y
            'PrintLine(7, "e " & EMod(6))
            'PrintLine(7, "pr " & PoissonsRatio(6))
            'PrintLine(7, "ro 5.0")
            'PrintLine(7, "")

            INGString = INGString & "mat 7 56" & vbCrLf &
   "e " & EMod(6) & vbCrLf &
   "pr " & PoissonsRatio(6) & vbCrLf &
   "ro 6.0" & vbCrLf & vbCrLf

            INGString = INGString & "mat 8 56" & vbCrLf &
   "e " & EMod(6) & vbCrLf &
   "pr " & PoissonsRatio(6) & vbCrLf &
   "ro 1.0" & vbCrLf & vbCrLf

            INGString = INGString & "mat 9 56" & vbCrLf &
   "e " & EMod(6) & vbCrLf &
   "pr " & PoissonsRatio(6) & vbCrLf &
   "ro 2.0" & vbCrLf & vbCrLf

            INGString = INGString & "mat 10 56" & vbCrLf &
   "e " & EMod(6) & vbCrLf &
   "pr " & PoissonsRatio(6) & vbCrLf &
   "ro 4.0" & vbCrLf & vbCrLf

            INGString = INGString & "mat 11 56" & vbCrLf &
   "e " & EMod(6) & vbCrLf &
   "pr " & PoissonsRatio(6) & vbCrLf &
   "ro 5.0" & vbCrLf & vbCrLf
            'rewritten for MeshclassLib by YC 070516 END

        End If
        'End If

        If Application.win.Overlay = True Then

            'Modified for thermal elastic material by YGC 062613 
            'PrintLine(7, "mat 12 1") 'PCC Overlay Material
            'PrintLine(7, "e " & EMod(0))
            'PrintLine(7, "pr " & PoissonsRatio(0))

            ''PrintLine(7, "ro 2.247e-4") ' Modified by YGC 100511
            'PrintLine(7, "ro 8.3912e-2")
            'PrintLine(7, "")

            If Application.win.chkTempLoad.IsChecked = False Then

                'rewritten for MeshclassLib by YC 070516
                'PrintLine(7, "mat 12 1") 'PCC Overlay Material
                'PrintLine(7, "e " & EMod(0))
                'PrintLine(7, "pr " & PoissonsRatio(0))
                'PrintLine(7, "ro 8.3912e-2")
                'PrintLine(7, "")
                INGString = INGString & "mat 12 1" & vbCrLf &
    "e " & EMod(0) & vbCrLf &
    "pr " & PoissonsRatio(0) & vbCrLf &
    "ro 8.3912e-2" & vbCrLf & vbCrLf
                'rewritten for MeshclassLib by YC 070516 END

            Else

                'rewritten for MeshclassLib by YC 070516
                'PrintLine(7, "mat 12 4")
                'PrintLine(7, "npts 2")
                'PrintLine(7, "temp -200 200")
                'PrintLine(7, "e " & EMod(0) & " " & EMod(0))
                'PrintLine(7, "pr " & PoissonsRatio(0) & " " & PoissonsRatio(0))
                'PrintLine(7, "alpha 5.0e-06 5.0e-06")
                'PrintLine(7, "ro 8.3912e-2")
                'PrintLine(7, "")

                ThermCoef = CSng(Application.win.txtThermCoef.Text) 'v3.0 003 YC

                INGString = INGString & "mat 12 4" & vbCrLf &
    "npts 2" & vbCrLf &
    "temp -200 200" & vbCrLf &
    "e " & EMod(0) & " " & EMod(0) & vbCrLf &
    "pr " & PoissonsRatio(0) & " " & PoissonsRatio(0) & vbCrLf &
    "alpha " & ThermCoef & " " & ThermCoef & vbCrLf &
    "ro 8.3912e-2" & vbCrLf & vbCrLf
                'corrected to enable changing thermal coefficent for user by YC 072516 070516
                'rewritten for MeshclassLib by YC 070516 END

            End If
            'Modified for thermal elastic material by YGC 062613 END

        End If


        'PrintLine(7, "endmat")  'rewritten for MeshclassLib by YC 070516
        'PrintLine(7, "")
        INGString = INGString & "endmat" & vbCrLf & vbCrLf

    End Sub

    Sub BaseMesh(ByRef p1 As par1)

        TS = 0.0#

        'If frmGear.Overlay = True Then 'suppressed for continuously creating mesh by YGC 072012  
        '    frmGear.NumberOfLayers = frmGear.NumberOfLayers - 1
        'End If

        If SubBaseExists = True Then

            'NumSB: number of subbase layers below Base
            'modified for continuously creating mesh by YGC 072012
            If Application.win.Overlay = True Then
                NumSB = Application.win.NumberOfLayers - 4 '(1 slab overlay+1 slab base+1 SG+1 upper subbase)
            Else
                NumSB = Application.win.NumberOfLayers - 3 '(1 slab base+1 SG+1 upper subbase)
            End If
            'end modify by YGC 072012

            For I = 3 To 2 + NumSB : TS = TS - LayerThickness(I) : Next I
        End If

        'Modified to change foundation mesh density by YGC 121510 
        'F1 = 31 'number of nodes defining the base layer
        'F2 = 16 'number of nodes defining the base layer (with symmetry)

        F1 = CSng(Application.win.cmbFdMesh.Text) + 1
        'YC 070516
        'F2 = CSng(Application.win.cmbFdMesh.Text) / 2 + 1
        If SymmSngl = True Then
            F2 = CInt(F1 - 1) / 2 + 1
        Else
            F2 = F1
        End If
        'YC 070516 END

        ''Modifed to change BS size with slab number by YGC 052810
        ''F5 is modified, F6 is introduced
        If NSlabs = 9 Then
            F3 = -450.0# '-x coordinate of edge of base
            F4 = 750.0# '+x coordinate of edge of base
            F5 = -600.0# '-y coordinate of edge of base
            F6 = 600.0# '+y coordinate of edge of base
        ElseIf NSlabs = 6 Then
            F3 = -450.0# '-x coordinate of edge of base
            F4 = 750.0# '+x coordinate of edge of base
            F5 = -300.0# '-y coordinate of edge of base
            F6 = 600.0# '+y coordinate of edge of base
        ElseIf NSlabs = 4 Then
            F3 = -450.0# '-x coordinate of edge of base
            F4 = 450.0# '+x coordinate of edge of base
            F5 = -300.0# '-y coordinate of edge of base
            F6 = 600.0# '+y coordinate of edge of base
        ElseIf NSlabs = 2 Then
            F3 = -450.0# '-x coordinate of edge of base
            F4 = 450.0# '+x coordinate of edge of base
            F5 = -300.0# '-y coordinate of edge of base
            F6 = 300.0# '+y coordinate of edge of base
        ElseIf NSlabs = 1 Then
            F3 = -150.0# '-x coordinate of edge of base
            F4 = 450.0# '+x coordinate of edge of base
            F5 = -300.0# '-y coordinate of edge of base
            F6 = 300.0# '+y coordinate of edge of base
        End If

        If SymmSngl = True Then F5 = 0 'YC 070516

        ' End Modify by YGC 052810

        'F9 = 9
        If Application.win.InfiniteElement = True Then
            F9 = 2
            F10 = 3
        End If

        If Application.win.Modes = True Then
            F12 = 1
        Else
            F12 = 4
        End If

        'Define the proportions of the infinite element using the rule: Xq - Xc = Xp - Xq
        If Application.win.Overlay = True Then
            InfiniteElementHeight = LayerThickness(0) + LayerThickness(1) + LayerThickness(2) - TS + 15.0#
            F13 = 2.0# * InfiniteElementHeight - LayerThickness(1) - LayerThickness(0) 'this parameter controls the depth to apparent base of the inf. subgrade
        Else
            InfiniteElementHeight = LayerThickness(1) + LayerThickness(2) - TS + 15.0#
            F13 = 2.0# * InfiniteElementHeight - LayerThickness(1) 'this parameter controls the depth to apparent base of the inf. subgrade
        End If

        'subroutine to set up the 3d mesh for the subbase and subgrade layers

        'Modified to suppress parameters by YC 070516
        '' Parameters
        'PrintLine(7, "parameter tk " & -LayerThickness(2))
        'PrintLine(7, "ts " & TS)
        'PrintLine(7, "t3 " & -LayerThickness(3))
        'PrintLine(7, "t4 " & -LayerThickness(4))
        'PrintLine(7, "")

        'If p1.a < 1 Or p1.b < 1 Then
        '    PrintLine(7, "p3 11")
        'Else
        '    PrintLine(7, "p3 6")
        'End If

        'PrintLine(7, "p6 0.0")
        'PrintLine(7, "p7 " & -DepthOfSubgrade)
        'PrintLine(7, "")

        'PrintLine(7, "f1 " & F1) 'number of nodes defining the base layer
        'PrintLine(7, "f2 " & F2) 'number of nodes defining the base layer (with symmetry)

        'PrintLine(7, "f3 " & F3) '-x coordinate of edge of base
        'PrintLine(7, "f4 " & F4) '+x coordinate of edge of base
        'PrintLine(7, "f5 " & F5) '-y coordinate of edge of base
        'PrintLine(7, "f6 " & F6) '+y coordinate of edge of base

        'PrintLine(7, "f9 " & F9) 'number of node layers in top part of subgrade layer
        'If Application.win.InfiniteElement = True Then
        '    PrintLine(7, "f10 " & F10)
        'End If
        'PrintLine(7, "f12 " & F12) 'number of elements through the thickness of subbase layers
        'PrintLine(7, "f13 " & F13) 'this parameter controls the depth to apparent base of the inf. subgrade
        'PrintLine(7, "rs 1.00;")
        'PrintLine(7, "")

        Dim tk, t3, t4, p6 As Double
        tk = -LayerThickness(2)
        t3 = -LayerThickness(3)
        t4 = -LayerThickness(4)
        p6 = 0.0
        'Modified to suppress parameters by YC 070516 END

        ' YC 070516 
        'PrintLine(7, "")
        'PrintLine(7, "exch 1 2 3")
        'PrintLine(7, "gct 4; ryz; ry 180; mz [%ts + %tk]; rzx;")  
        'PrintLine(7, "lev 1 grep 0 1 ; ;")
        'PrintLine(7, "lev 2 grep 2; ;")
        'PrintLine(7, "lev 3 grep 3; ;")
        'PrintLine(7, "lev 4 grep 0 4; ;")
        'PrintLine(7, "")
        INGString = INGString & "exch 1 2 3" & vbCrLf &
        "gct 4; ryz; ry 180; mz " & TS + tk & ";" & " rzx;" & vbCrLf &
        "lev 1 grep 0 1 ; ;" & vbCrLf &
        "lev 2 grep 2 ; ;" & vbCrLf &
        "lev 3 grep 3; ;" & vbCrLf &
        "lev 4 grep 0 4; ;" & vbCrLf & vbCrLf
        'YC 070516 END

        'PrintLine(7, "start") 'BASE and SUBBASE layers
        INGString = INGString & "start" & vbCrLf &
                "1 " & F1 & ";" & vbCrLf &
                "1 " & F2 & ";" & vbCrLf

        If SubBaseExists = True Then

            ' YC 070516
            'If SymmSngl = True Then
            '    PrintLine(7, "1 [%f1]; 1 [%f2];")
            'Else
            '    PrintLine(7, "1 [%f1]; 1 [%f1];")
            'End If
            'Print(7, "1 [1 + %f12] [1 + 2 * %f12]")
            INGString = INGString & "1 " & (1 + F12) & " " & (1 + 2 * F12)
            ' YC 070516 END

            'Modified to correct the base/subbase mesh for overlay by YGC 080612
            'If frmGear.NumberOfLayers > 4 Then Print(7, " [1 + 3 * %f12]") 
            'If frmGear.NumberOfLayers > 5 Then Print(7, " [1 + 4 * %f12]")

            'YC 070516
            'If NumSB > 1 Then Print(7, " [1 + 3 * %f12]")
            'If NumSB > 2 Then Print(7, " [1 + 4 * %f12]")
            If NumSB > 1 Then INGString = INGString & " " & (1 + 3 * F12)
            If NumSB > 2 Then INGString = INGString & " " & (1 + 4 * F12)
            'YC 070516 END

            'Modify ended to correct the base/subbase mesh for overlay by YGC 080612

            'PrintLine(7, ";")      'YC 070516
            INGString = INGString & ";" & vbCrLf

            'YC 070516
            'If SymmSngl = True Then
            '    PrintLine(7, "[%f3] [%f4]")
            '    PrintLine(7, "0. [%f6]")      ' Moddified to introduce f6 by YGC 052810
            'Else
            '    PrintLine(7, "[%f3] [%f4]")
            '    PrintLine(7, "[%f5] [%f6]")   ' Moddified to introduce f6 by YGC 052810
            'End If
            INGString = INGString & F3 & " " & F4 & vbCrLf &
                F5 & " " & F6 & vbCrLf
            ' YC 070516 END

            'If frmGear.NumberOfLayers > 5 Then             'Modified to correct the base/subbase mesh for overlay by YGC 080612
            If NumSB > 2 Then

                'PrintLine(7, "[%ts + %tk] [%t3 + %t4 + %tk] [%t3 + %tk] %tk %p6") ' YC 070516
                INGString = INGString & (TS + tk) & " " & (t3 + t4 + tk) & " " & (t3 + tk) & " " & tk & " " & p6 & vbCrLf

                ''Modified to compact the code by YGC 072910
                For I = 1 To NSlabs
                    'PrintLine(7, "si+ 1 1 5 2 2 5 " & I & " m 0 0 1")  ' YC 070516
                    INGString = INGString & "si+ 1 1 5 2 2 5 " & I & " m 0 0 1" & vbCrLf
                Next I
                ''End Modify by YGC 072910

                ' YC 070516
                'PrintLine(7, "mt 1 1 1 2 2 2 5")
                'PrintLine(7, "mt 1 1 2 2 2 3 4")
                'PrintLine(7, "mt 1 1 3 2 2 4 3")
                'PrintLine(7, "mt 1 1 4 2 2 5 2")
                INGString = INGString & "mt 1 1 1 2 2 2 5" & vbCrLf &
                     "mt 1 1 2 2 2 3 4" & vbCrLf &
                     "mt 1 1 3 2 2 4 3" & vbCrLf &
                     "mt 1 1 4 2 2 5 2" & vbCrLf
                ' YC 070516 END

                'ElseIf frmGear.NumberOfLayers > 4 Then      'Modified to correct the base/subbase mesh for overlay by YGC 080612
            ElseIf NumSB > 1 Then

                'PrintLine(7, "[%ts + %tk] [%t3 + %tk] %tk %p6") ' YC 070516
                INGString = INGString & (TS + tk) & " " & (t3 + tk) & " " & tk & " " & p6 & vbCrLf

                ''Modified to compact the code by YGC 072910
                For I = 1 To NSlabs
                    'PrintLine(7, "si+ 1 1 4 2 2 4 " & I & " m 0 0 1")  ' YC 070516
                    INGString = INGString & "si+ 1 1 4 2 2 4 " & I & " m 0 0 1" & vbCrLf
                Next I

                ' YC 070516
                'PrintLine(7, "mt 1 1 1 2 2 2 4")
                'PrintLine(7, "mt 1 1 2 2 2 3 3")
                'PrintLine(7, "mt 1 1 3 2 2 4 2")
                INGString = INGString & "mt 1 1 1 2 2 2 4" & vbCrLf &
                        "mt 1 1 2 2 2 3 3" & vbCrLf &
                        "mt 1 1 3 2 2 4 2" & vbCrLf
                ' YC 070516 END

                ''End Modify by YGC 072910

            Else
                'PrintLine(7, "[%ts + %tk] %tk %p6") ' YC 070516
                INGString = INGString & (TS + tk) & " " & tk & " " & p6 & vbCrLf

                ''Modified to compact the code by YGC 052810
                For I = 1 To NSlabs
                    'PrintLine(7, "si+ 1 1 3 2 2 3 " & I & " m 0 0 1") ' YC 070516
                    INGString = INGString & "si+ 1 1 3 2 2 3 " & I & " m 0 0 1" & vbCrLf
                Next I

                ' YC 070516
                'PrintLine(7, "mt 1 1 1 2 2 2 3")
                'PrintLine(7, "mt 1 1 2 2 2 3 2")
                INGString = INGString & "mt 1 1 1 2 2 2 3" & vbCrLf &
                        "mt 1 1 2 2 2 3 2" & vbCrLf
                ' YC 070516 END

            End If
        Else

            ' YC 070516
            'If SymmSngl = True Then
            '    PrintLine(7, "1 [%f1]; 1 [%f2];")

            'Else
            '    PrintLine(7, "1 [%f1]; 1 [%f1];")
            'End If
            'PrintLine(7, "1 [1 + %f12]")
            'PrintLine(7, ";")

            'If SymmSngl = True Then
            '    PrintLine(7, "[%f3] [%f4]")
            '    PrintLine(7, "0. [%f6]")      ' Moddified to introduce f6 by YGC 052810
            'Else
            '    PrintLine(7, "[%f3] [%f4]")
            '    PrintLine(7, "[%f5] [%f6]")   ' Moddified to introduce f6 by YGC 052810
            'End If
            'PrintLine(7, "%tk %p6")
            INGString = INGString & "1 " & (1 + F12) & ";" & vbCrLf &
                    F3 & " " & F4 & vbCrLf &
                    F5 & " " & F6 & vbCrLf &
                    tk & " " & p6 & vbCrLf
            ' YC 070516 END

            ''Modified to compact the code by YGC 071610
            For I = 1 To NSlabs
                'PrintLine(7, "si+ 1 1 2 2 2 2 " & I & " m 0 0 1")   ' YC 070516
                INGString = INGString & "si+ 1 1 2 2 2 2 " & I & " m 0 0 1" & vbCrLf
            Next I

            ''Modify end

            'PrintLine(7, "mate 2")            ' YC 070516
            INGString = INGString & "mate 2" & vbCrLf

        End If

        'PrintLine(7, "end")    ' YC 070516
        'PrintLine(7, "")
        INGString = INGString & "end" & vbCrLf & vbCrLf


        ' YC 070516
        'PrintLine(7, "start") 'SUBGRADE layer (regular elements)
        'If SymmSngl = True Then
        '    PrintLine(7, "1 [%f1]; 1 [%f2]; 1 %f10;")
        'Else
        '    PrintLine(7, "1 [%f1]; 1 [%f1]; 1 %f10;")
        'End If
        'If SymmSngl = True Then
        '    PrintLine(7, "[%f3] [%f4]")
        '    PrintLine(7, "0. [%f6]")      ' Moddified to introduce f6 by YGC 052810
        'Else
        '    PrintLine(7, "[%f3] [%f4]")
        '    PrintLine(7, "[%f5] [%f6]")   ' Moddified to introduce f6 by YGC 052810
        'End If
        'PrintLine(7, "[-15. + %tk + %ts] [%tk + %ts]")
        'PrintLine(7, "mate 6")     
        'PrintLine(7, "end")
        'PrintLine(7, "")
        INGString = INGString & "start" & vbCrLf &
                "1 " & F1 & ";" & vbCrLf &
                "1 " & F2 & ";" & vbCrLf &
                "1 " & F10 & ";" & vbCrLf &
                 F3 & " " & F4 & vbCrLf &
                 F5 & " " & F6 & vbCrLf &
                (-15 + tk + TS) & " " & (tk + TS) & vbCrLf &
                "mate 6" & vbCrLf &
                "end" & vbCrLf & vbCrLf
        ' YC 070516 END


        'modified to suppress parameters by YC 070516 
        'PrintLine(7, "start") 'Infinite elements facing downward center part
        'If SymmSngl = True Then
        '    PrintLine(7, "1 [%f1]; 1 [%f2]; 1 %f9;")
        '    PrintLine(7, "[%f3] [%f4]")
        '    PrintLine(7, "0. [%f6]")      ' Moddified to introduce f6 by YGC 052810
        'Else
        '    PrintLine(7, "1 [%f1]; 1 [%f1]; 1 %f9;")
        '    PrintLine(7, "[%f3] [%f4]")
        '    PrintLine(7, "[%f5] [%f6]")   ' Moddified to introduce f6 by YGC 052810
        'End If
        'PrintLine(7, "[-%f13] [-15.+%tk + %ts]")
        'PrintLine(7, "mate 7")
        INGString = INGString & "start" & vbCrLf &
        "1 " & F1 & ";" & vbCrLf &
        "1 " & F2 & ";" & vbCrLf &
        "1 " & F9 & ";" & vbCrLf &
         F3 & " " & F4 & vbCrLf &
         F5 & " " & F6 & vbCrLf &
         -F13 & " " & (-15 + tk + TS) & vbCrLf &
        "mate 7" & vbCrLf
        'modified to suppress parameters by YC 070516 END

        GoTo 99

        'PrintLine(7, "edit")
        'PrintLine(7, "phr 150. -20000. -20000. 20000. 20000. 0.")
        'PrintLine(7, "x = x - ((x-150.) /60.) * (z +15.-%tk - %ts)")
        'PrintLine(7, "phr -20000. -20000. -20000. 150. 20000. 0.")
        'PrintLine(7, "x = x - ((x-150.) /60.) * (z +15.-%tk - %ts)")
        'PrintLine(7, "phr -20000. -20000. -20000. 20000. 20000. 0.")
        'PrintLine(7, "y = y - (y /60.) * (z +15.-%tk - %ts)")

99:
        'PrintLine(7, "end")
        'PrintLine(7, "")
        INGString = INGString & "end" & vbCrLf & vbCrLf

        GoTo 100

        'PrintLine(7, "start") 'infinite elements facing in + x-direction
        'If SymmSngl = True Then
        '    PrintLine(7, "1 2; 1 [%f2]; 1 %f10;")
        '    PrintLine(7, "[%f4] [%f4 + ((%f4-150.)/60.)*(%f13 - 15. + (%tk + %ts))]")
        '    PrintLine(7, "0. [%f5]")
        '    PrintLine(7, "[-15 + %tk + %ts] [%tk + %ts]")
        'Else
        '    PrintLine(7, "1 2; 1 [%f1]; 1 %f10;")
        '    PrintLine(7, "[%f4] [%f4 + ((%f4-150.)/60.)*(%f13 - 15. + (%tk + %ts))]")
        '    PrintLine(7, "[-%f5] [%f5]")
        '    PrintLine(7, "[-15 + %tk + %ts] [%tk + %ts]")
        'End If
        'PrintLine(7, "mate 8")
        'PrintLine(7, "edit")
        'If OneSlab = True Then
        '    PrintLine(7, "phr 332. -20000. -20000. 20000. 20000. 0.")
        'Else
        '    PrintLine(7, "phr 602. -20000. -20000. 20000. 20000. 0.")
        'End If
        'PrintLine(7, "z = z-(z-%tk-%ts)*(%f13-15.+%tk+%ts)/(-15)")
        'PrintLine(7, "y = y - (y /60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        'PrintLine(7, "end")


        'PrintLine(7, "start") 'infinite elements facing in - x-direction
        'If SymmSngl = True Then
        '    PrintLine(7, "1 2; 1 [%f2]; 1 %f10;")
        '    PrintLine(7, "[%f3 - ((150.-%f3)/60.)*(%f13 -15.+(%tk + %ts))] [%f3]")
        '    PrintLine(7, "0.0 [%f5]")
        '    PrintLine(7, "[-15 + %tk + %ts] [%tk + %ts]")
        'Else
        '    PrintLine(7, "1 2; 1 [%f1]; 1 %f10;")
        '    PrintLine(7, "[%f3 - ((150.-%f3)/60.)*(%f13 -15.+(%tk + %ts))] [%f3]")
        '    PrintLine(7, "[-%f5][%f5]")
        '    PrintLine(7, "[-15 + %tk + %ts] [%tk + %ts]")
        'End If
        'PrintLine(7, "mate 10")
        'PrintLine(7, "edit")
        'If OneSlab = True Then
        '    PrintLine(7, "phr -20000. -20000. -20000. -32. 20000. 0.")
        'Else
        '    PrintLine(7, "phr -20000. -20000. -20000. -302. 20000. 0.")
        'End If
        'PrintLine(7, "z = z-(z-%tk-%ts)*(%f13-15.+%tk+%ts)/(-15)")
        'PrintLine(7, "y = y - (y /60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        'PrintLine(7, "end")

        'PrintLine(7, "start") 'Infinite elements facing in + y-direction
        'PrintLine(7, "1 [%f1]; 1 2; 1 %f10;")
        'PrintLine(7, "[%f3] [%f4]")
        'PrintLine(7, "[%f5] [%f5 + (%f5/60.)*(%f13 -15.+%tk + %ts)]")
        'PrintLine(7, "[-15 + %tk + %ts] [%tk + %ts]")
        'PrintLine(7, "mate 6")
        'PrintLine(7, "edit")

        'If OneSlab = True Then
        '    PrintLine(7, "phr -20000. 182. -20000. 20000. 20000. 0.")
        '    PrintLine(7, "z = z-(z-%tk-%ts)*(%f13-15+%tk+%ts)/(-15)")
        '    PrintLine(7, "phr 150. 182. -20000. 20000. 20000. 0.")
        '    PrintLine(7, "x = x -((x-150.)/60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        '    PrintLine(7, "phr -20000. 182. -20000. 150. 20000. 0.")
        '    PrintLine(7, "x = x -((x-150.)/60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        'Else
        '    PrintLine(7, "phr -20000. 452. -20000. 20000. 20000. 0.")
        '    PrintLine(7, "z = z-(z-%tk-%ts)*(%f13-15+%tk+%ts)/(-15)")
        '    PrintLine(7, "phr 150. 452. -20000. 20000. 20000. 0.")
        '    PrintLine(7, "x = x -((x-150.)/60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        '    PrintLine(7, "phr -20000. 452. -20000. 150. 20000. 0.")
        '    PrintLine(7, "x = x -((x-150.)/60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        'End If
        'PrintLine(7, "end")

        'If SymmSngl = False Then
        '    PrintLine(7, "start") 'Infinite elements facing in - y-direction
        '    PrintLine(7, "1 [%f1]; 1 2; 1 %f10;")
        '    PrintLine(7, "[%f3] [%f4]")
        '    PrintLine(7, "[(-%f5) + ((-%f5)/60.)*(%f13 -15.+%tk + %ts)] [-%f5] ")
        '    PrintLine(7, "[-15 + %tk + %ts][%tk + %ts]")
        '    PrintLine(7, "mate 6")
        '    PrintLine(7, "edit")

        '    If OneSlab = True Then
        '        PrintLine(7, "phr -20000. -20000. -20000. 20000. -182. 0.")
        '        PrintLine(7, "z = z-(z-%tk-%ts)*(%f13-15+%tk+%ts)/(-15)")
        '        PrintLine(7, "phr 150. -20000. -20000. 20000. -182. 0.")
        '        PrintLine(7, "x = x -((x-150.)/60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        '        PrintLine(7, "phr -20000. -20000. -20000. 150. -182. 0.")
        '        PrintLine(7, "x = x -((x-150.)/60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        '        PrintLine(7, "end")
        '    Else
        '        PrintLine(7, "phr -20000. -20000. -20000. 20000. -452. 0.")
        '        PrintLine(7, "z = z-(z-%tk-%ts)*(%f13-15+%tk+%ts)/(-15)")
        '        PrintLine(7, "phr 150. -20000. -20000. 20000. -452. 0.")
        '        PrintLine(7, "x = x -((x-150.)/60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        '        PrintLine(7, "phr -20000. -20000. -20000. 150. -452. 0.")
        '        PrintLine(7, "x = x -((x-150.)/60.) * ((-1*%f13)-(-15.+%tk+%ts))")
        '        PrintLine(7, "end")
        '    End If
        'End If

100:


    End Sub

    Sub SetMesh(ByRef p1 As par1)

        'set control parameters for mesh
        Dim Factor As Double

        NSlabs = CSng(Application.win.cmbNSlabs.Text) 'added for variable slabs number YGC 052810
        DepthOfSubgrade = CSng(Application.win.Cutoff)

        Factor = 1.0#

        MeshParamList(1) = 300.0# / (12.0# * Factor)
        MeshParamList(2) = 300.0# / (11.0# * Factor)
        MeshParamList(3) = 300.0# / (10.0# * Factor)
        MeshParamList(4) = 300.0# / (9.0# * Factor)
        MeshParamList(5) = 300.0# / (8.0# * Factor)
        MeshParamList(6) = 300.0# / (7.0# * Factor)
        MeshParamList(7) = 300.0# / (6.0# * Factor)
        MeshParamList(8) = 300.0# / (13.0# * Factor)
        MeshParamList(9) = 300.0# / (14.0# * Factor)
        MeshParamList(10) = 300.0# / (15.0# * Factor)
        MeshParamList(11) = 300.0# / (24.0# * Factor)
        MeshParamList(12) = 300.0# / (30.0# * Factor)

        'MeshParamList(3) = 300# / 5#

        If Application.win.XScaleFactor >= 3.0# Then
            MeshParamX = MeshParamList(12)
        ElseIf Application.win.XScaleFactor > 2.0# Then
            MeshParamX = MeshParamList(11)
        ElseIf Application.win.XScaleFactor > 1.2 Then
            MeshParamX = MeshParamList(10)
        ElseIf Application.win.XScaleFactor > 1.1 Then
            MeshParamX = MeshParamList(9)
        ElseIf Application.win.XScaleFactor > 1.0# Then
            MeshParamX = MeshParamList(8)
        ElseIf Application.win.XScaleFactor > 0.91667 Then
            MeshParamX = MeshParamList(1)
        ElseIf Application.win.XScaleFactor > 0.83333 Then
            MeshParamX = MeshParamList(2)
        ElseIf Application.win.XScaleFactor > 0.75 Then
            MeshParamX = MeshParamList(3)
        ElseIf Application.win.XScaleFactor > 0.66667 Then
            MeshParamX = MeshParamList(4)
        ElseIf Application.win.XScaleFactor > 0.58333 Then
            MeshParamX = MeshParamList(5)
        ElseIf Application.win.XScaleFactor > 0.5 Then
            MeshParamX = MeshParamList(6)
        Else
            MeshParamX = MeshParamList(7)
        End If

        If Application.win.YScaleFactor >= 3.0# Then
            MeshParamY = MeshParamList(12)
        ElseIf Application.win.YScaleFactor > 2.0# Then
            MeshParamY = MeshParamList(11)
        ElseIf Application.win.YScaleFactor > 1.2 Then
            MeshParamY = MeshParamList(10)
        ElseIf Application.win.YScaleFactor > 1.1 Then
            MeshParamY = MeshParamList(9)
        ElseIf Application.win.YScaleFactor > 1.0# Then
            MeshParamY = MeshParamList(8)
        ElseIf Application.win.YScaleFactor > 0.91667 Then
            MeshParamY = MeshParamList(1)
        ElseIf Application.win.YScaleFactor > 0.83333 Then
            MeshParamY = MeshParamList(2)
        ElseIf Application.win.YScaleFactor > 0.75 Then
            MeshParamY = MeshParamList(3)
        ElseIf Application.win.YScaleFactor > 0.66667 Then
            MeshParamY = MeshParamList(4)
        ElseIf Application.win.YScaleFactor > 0.58333 Then
            MeshParamY = MeshParamList(5)
        ElseIf Application.win.YScaleFactor > 0.5 Then
            MeshParamY = MeshParamList(6)
        Else
            MeshParamY = MeshParamList(7)
        End If


        p1.a3 = 210.02
        p1.c3 = 190.02
        p1.d3 = 180.02
        p1.a7 = 390.02
        p1.c7 = 360.02
        p1.d7 = 360.02
        E1 = 4000000.0#
        PR1 = 0.15
        'E1 = 4410000#
        'PR1 = 0.22

        Delta0 = MeshParamX
        Delta1 = MeshParamX / 3.0#
        Delta2 = MeshParamX / 6.0#
        Delta0X = MeshParamX
        Delta1X = MeshParamX / 3.0#
        Delta2X = MeshParamX / 6.0#
        Delta0Y = MeshParamY
        Delta1Y = MeshParamY / 3.0#
        Delta2Y = MeshParamY / 6.0#
        Xgrid = Delta2X
        Ygrid = Delta2Y
        p1.a1 = 3.0# * Delta0X + 0.02
        p1.c1 = 2.0# * (Delta0X + Delta1X) + 0.02
        p1.d1 = 2.0# * Delta0X + 0.02



    End Sub

    Sub DummyNodes()

        'sets up a 2D mesh containing the fixed "dummy" nodes that are used to constrain the corners of slabs

        'PrintLine(7, "c dummy nodes")  YC 070516
        INGString = INGString & "c dummy nodes" & vbCrLf

        'modified to suppress parameter by YC 070516
        'Print(7, "parameter tk " & -LayerThickness(1)) 'slab thickness
        'PrintLine(7, " tkol " & -LayerThickness(0) & ";") 'overlay thickness
        Dim tk As Double = -LayerThickness(1)
        'modified to suppress parameter by YC 070516 END

        If SymmSngl = True Then
            'rewritten by YC 070516
            'PrintLine(7, "start")
            'PrintLine(7, "1 2 3 4; 1 2 3; -1;")
            'PrintLine(7, "-300.1 -0.1 300.1 600.1")
            'PrintLine(7, "0.0 150.0 450.0")
            'PrintLine(7, "0") 'modified to lower than loading surface to eliminate dummy loads by YGC 073010
            '' cancelled for an running error when N>1 by YGC 091310
            'PrintLine(7, "b 1 1 0 4 3 0 111111")
            'PrintLine(7, "end")
            'PrintLine(7, "")

            'PrintLine(7, "start")
            'PrintLine(7, "1 2 3 4; 1 2 3; -1;")
            'PrintLine(7, "-300.0 0.0 300.0 600.0")
            'PrintLine(7, "-0.1 150.1 450.1")
            'PrintLine(7, "0") 'modified to lower than loading surface to eliminate dummy loads by YGC 073010
            '' cancelled for an running error when N>1 by YGC 091310
            'PrintLine(7, "b 1 1 0 4 3 0 111111")
            'PrintLine(7, "end")

            INGString = INGString & "start" & vbCrLf &
                       "1 2 3 4; " & vbCrLf &
                       "1 2 3;" & vbCrLf &
                       "-1;" & vbCrLf &
                       "-300.1 -0.1 300.1 600.1" & vbCrLf &
                       "0.0 150.0 450.0" & vbCrLf &
                       "0" & vbCrLf &
                       "b 1 1 0 4 3 0 111111" & vbCrLf &
                       "end" & vbCrLf & vbCrLf

            INGString = INGString & "start" & vbCrLf &
                       "1 2 3 4; " & vbCrLf &
                       "1 2 3;" & vbCrLf &
                       "-1;" & vbCrLf &
                       "-300.0 0.0 300.0 600.0" & vbCrLf &
                       "-0.1 150.1 450.1" & vbCrLf &
                       "0" & vbCrLf &
                       "b 1 1 0 4 3 0 111111" & vbCrLf &
                       "end" & vbCrLf & vbCrLf
            'rewritten by YC 070516 END
        Else
            'rewritten by YC 070516
            'PrintLine(7, "start")
            'PrintLine(7, "1 2 3 4; 1 2 3 4; -1;")
            'PrintLine(7, "-300.1 -0.1 300.1 600.1")
            'PrintLine(7, "-450.0 -150.0 150.0 450.0")
            'PrintLine(7, "0") 'modified to lower than loading surface to eliminate dummy loads by YGC 073010
            '' cancelled for an running error when N>1 by YGC 091310
            'PrintLine(7, "b 1 1 0 4 4 0 111111")
            'PrintLine(7, "end")
            'PrintLine(7, "")

            'PrintLine(7, "start")
            ''PrintLine(7, "1 2 3 4; 1 2 3 4; -1;")
            'PrintLine(7, "-300.0 0.0 300.0 600.0")
            'PrintLine(7, "-450.1 -150.1 150.1 450.1")
            'PrintLine(7, "0") 'modified to lower than loading surface to eliminate dummy loads by YGC 073010
            '' cancelled for an running error when N>1 by YGC 091310
            'PrintLine(7, "b 1 1 0 4 4 0 111111")
            'PrintLine(7, "end")

            INGString = INGString & "start" & vbCrLf &
                        "1 2 3 4; " & vbCrLf &
                        "1 2 3 4;" & vbCrLf &
                        "-1;" & vbCrLf &
                        "-300.1 -0.1 300.1 600.1" & vbCrLf &
                        "-450.0 -150.0 150.0 450.0" & vbCrLf &
                        "0" & vbCrLf &
                        "b 1 1 0 4 4 0 111111" & vbCrLf &
                        "end" & vbCrLf & vbCrLf

            INGString = INGString & "start" & vbCrLf &
                        "1 2 3 4; " & vbCrLf &
                        "1 2 3 4;" & vbCrLf &
                        "-1;" & vbCrLf &
                        "-300.0 0.0 300.0 600.0" & vbCrLf &
                        "-450.1 -150.1 150.1 450.1" & vbCrLf &
                        "0" & vbCrLf &
                        "b 1 1 0 4 4 0 111111" & vbCrLf &
                        "end" & vbCrLf & vbCrLf
            'rewritten by YC 070516 END
        End If

        If Application.win.Overlay = True Then 'sets up a 2nd 2D mesh for the overlay, if one exists
            If SymmSngl = True Then
                'rewritten by YC 070516
                'PrintLine(7, "start")
                'PrintLine(7, "1 2 3 4; 1 2 3; -1;")
                'PrintLine(7, "-300.1 -0.1 300.1 600.1")
                'PrintLine(7, "0.0 150.0 450.0")
                'PrintLine(7, "[-%tk]") 'modified to lower than loading surface to eliminate dummy loads by YGC 073010
                ' cancelled for an running error when N>1 by YGC 091310  
                'PrintLine(7, "b 1 1 0 4 3 0 111111")
                'PrintLine(7, "end")
                'PrintLine(7, "")

                'PrintLine(7, "start")
                'PrintLine(7, "1 2 3 4; 1 2 3; -1;")
                'PrintLine(7, "-300.0 0.0 300.0 600.0")
                'PrintLine(7, "-0.1 150.1 450.1")
                'PrintLine(7, "[-%tk ]") 'modified to lower than loading surface to eliminate dummy loads by YGC 073010
                ' cancelled for an running error when N>1 by YGC 091310  
                'PrintLine(7, "b 1 1 0 4 3 0 111111")
                'PrintLine(7, "end")

                INGString = INGString & "start" & vbCrLf &
                           "1 2 3 4; " & vbCrLf &
                            "1 2 3;" & vbCrLf &
                            "-1;" & vbCrLf &
                            "-300.1 -0.1 300.1 600.1" & vbCrLf &
                            "0.0 150.0 450.0" & vbCrLf &
                            -tk & vbCrLf &
                            "b 1 1 0 4 3 0 111111" & vbCrLf &
                            "end" & vbCrLf & vbCrLf

                INGString = INGString & "start" & vbCrLf &
                           "1 2 3 4; " & vbCrLf &
                           "1 2 3;" & vbCrLf &
                           "-1;" & vbCrLf &
                           "-300.0 0.0 300.0 600.0" & vbCrLf &
                           "-0.1 150.1 450.1" & vbCrLf &
                           -tk & vbCrLf &
                           "b 1 1 0 4 3 0 111111" & vbCrLf &
                           "end" & vbCrLf & vbCrLf
                'rewritten by YC 070516 END
            Else


                'rewritten by YC 070516
                'PrintLine(7, "start")
                'PrintLine(7, "1 2 3 4; 1 2 3 4; -1;")
                'PrintLine(7, "-300.1 -0.1 300.1 600.1")
                'PrintLine(7, "-450.0 -150.0 150.0 450.0")
                'PrintLine(7, "[-%tk]") 'modified to lower than loading surface to eliminate dummy loads by YGC 073010
                ' cancelled for an running error when N>1 by YGC 091310 
                'PrintLine(7, "b 1 1 0 4 4 0 111111")
                'PrintLine(7, "end")
                'PrintLine(7, "")

                'PrintLine(7, "start")
                'PrintLine(7, "1 2 3 4; 1 2 3 4; -1;")
                'PrintLine(7, "-300.0 0.0 300.0 600.0")
                'PrintLine(7, "-450.1 -150.1 150.1 450.1")
                'PrintLine(7, "[-%tk ]") 'modified to lower than loading surface to eliminate dummy loads by YGC 073010
                ' cancelled for an running error when N>1 by YGC 091310 
                'PrintLine(7, "b 1 1 0 4 4 0 111111")
                'PrintLine(7, "end")

                INGString = INGString & "start" & vbCrLf &
                            "1 2 3 4; " & vbCrLf &
                            "1 2 3 4;" & vbCrLf &
                            "-1;" & vbCrLf &
                            "-300.1 -0.1 300.1 600.1" & vbCrLf &
                            "-450.0 -150.0 150.0 450.0" & vbCrLf &
                           -tk & vbCrLf &
                            "b 1 1 0 4 4 0 111111" & vbCrLf &
                            "end" & vbCrLf & vbCrLf

                INGString = INGString & "start" & vbCrLf &
                            "1 2 3 4; " & vbCrLf &
                            "1 2 3 4;" & vbCrLf &
                            "-1;" & vbCrLf &
                            "-300.0 0.0 300.0 600.0" & vbCrLf &
                            "-450.1 -150.1 150.1 450.1" & vbCrLf &
                            -tk & vbCrLf &
                            "b 1 1 0 4 4 0 111111" & vbCrLf &
                            "end" & vbCrLf & vbCrLf
                'rewritten by YC 070516 END
            End If
        End If

    End Sub

    Sub Toler()

        PrintLine(7, "tp 0.01")
        PrintLine(7, "")
        PrintLine(7, "end")
        PrintLine(7, "")
        PrintLine(7, "rz 40")
        PrintLine(7, "rx -45")
        PrintLine(7, "set tv display")

    End Sub

    Sub Txtfl(ByRef Filnam As String, ByRef TextFileName As String)

        FileNameLength = Len(Filnam)
        'If FileNameLength > 68 Then
        'If FileNameLength > 80 Then 'Modified by YGC 100110
        If FileNameLength > 120 Then 'Modified by YGC 110410
            MsgBox("Error - Path Name Too Long")
            FileErr = 1
            GoTo 999
        End If

        If Mid(Filnam, FileNameLength - 3, 1) = "." Then
            FileNameBody = Mid(Filnam, 1, FileNameLength - 4)
        Else
            FileNameBody = Filnam
        End If
        TextFileName = FileNameBody & ".rgd"
999:
    End Sub

    Sub WriteTextFile()

        'Dim IndxModes, I As Integer    'YC 070516
        Dim I As Integer

        FileOpen(8, TextFileName, OpenMode.Output)

        'YGC 111314
        Dim istep As Integer = 1
        PrintLine(8, 0) 'ebe solver

        If Application.win.cmbLoadingType.SelectedIndex = 0 Then
            PrintLine(8, 1) ' 1 step for static loading
        End If

        If Application.win.cmbLoadingType.SelectedIndex = 1 Then
            PrintLine(8, NMovingStep) ' multiple step for moving loading
        End If
        'YGC 111314

1420:   PrintLine(8, NWheels)

        Xgrid = (CSng(Application.win.cmbDimX.Text) * 12) / CSng(Application.win.cmbMesh.Text)
        Ygrid = (CSng(Application.win.cmbDimY.Text) * 12) / CSng(Application.win.cmbMesh.Text)

        'PrintLine(8, Xgrid) 'for INGRID 120413 YGC 111314
        'PrintLine(8, Ygrid) 'for INGRID 120413 YGC 111314
        PrintLine(8, TirePressure)

        For I = 1 To NWheels
            PrintLine(8, Format(Xtr(I), "##0.000") & " " & Format(-Ytr(I), "##0.000"))
        Next I

        PrintLine(8, Format(Xfp, "##0.000"))
        PrintLine(8, Format(Yfp, "##0.000"))

        IndxSymm = 0
        If SymmDbl = True Then
            IndxSymm = 2
        ElseIf SymmSngl = True Then
            IndxSymm = 1
        End If

        PrintLine(8, Format(IndxSymm, "0"))
        PrintLine(8, Format(Application.win.XScaleFactor, "0.000"))
        PrintLine(8, Format(Application.win.YScaleFactor, "0.000"))
        PrintLine(8, Format(NSlabs, "0"))

        'for INGRID 120413 YGC 111314
        For I = 1 To NSlabs
            Print(8, Xgrid)
            PrintLine(8, Ygrid)
        Next
        'for INGRID 120413 YGC 111314 END

        'Interface stiffness scale factors
        PrintLine(8, Factor1 & " " & Factor2)
        'Angle that CL of Gear makes wrt Joint
        PrintLine(8, Format(Application.win.GearAngle, "#0.00"))

        'for INGRID 120413 YGC 111314
        ''Equivalent Joint Stiffness
        'PrintLine(8, Format(EqJStfX, "######0.0") & " " & Format(EqJStfY, "######0.0"))
        'PrintLine(8, Format(EqEdgStfX, "######0.0") & " " & Format(EqEdgStfY, "######0.0")) 'added for Bounary(Edge) spring by YGC 110110

        PrintLine(8, Format(EqJStfX, "######0.0") & " " & Format(EqJStfY, "######0.0") & " 1")
        PrintLine(8, "1.0e+10 1.0e+10 0")
        PrintLine(8, Format(EqEdgStfX, "######0.0") & " " & Format(EqEdgStfY, "######0.0") & " 1")
        'for INGRID 120413 YGC 111314 END

        'Incompatible Modes?
        IndxModes = 0
        If Application.win.Modes = True Then IndxModes = 1
        PrintLine(8, Format(IndxModes, "0"))

        IndxFndn = 0
        If Application.win.Winkler = True Then
            IndxFndn = 1
        ElseIf Application.win.InfiniteElement = True Then
            IndxFndn = 2
        Else
            IndxFndn = 0
        End If
        PrintLine(8, Format(IndxFndn, "0"))

        If IndxFndn = 1 Then PrintLine(8, Format(Application.win.KSubgrade, "##0.0"))

        'Slab Thickness & Overlay Information
        PrintLine(8, Format(LayerThickness(1), "##0.0"))

        If Application.win.Overlay = True Then
            IndxOvrl = 1
        Else
            IndxOvrl = 0
        End If

        PrintLine(8, Format(IndxOvrl, "0"))
        If IndxOvrl <> 0 Then PrintLine(8, Format(LayerThickness(0), "##0.0"))



        'Added for Temperature Effect by YGC 122210
        If Application.win.chkTempLoad.IsChecked Then

            'for INGRID 120413 YGC 111314
            'SurfTemp = CSng(Application.win.txtSurfTemp.Text)
            'BotmTemp = CSng(Application.win.txtBotmTemp.Text)
            LETG = CSng(Application.win.txtLETG.Text)
            'for INGRID 120413 YGC 111314 END

            'TempDiff = (BotmTemp - SurfTemp) / 2

            ThermCoef = CSng(Application.win.txtThermCoef.Text)

            'If (Application.win.cmbCurlShape.Text = "Circular") Then    ' v3.0 002 YC 052620
            If (Application.win.cmbCurlShape.Text = "Spherical") Then

                CurlShapePara = 0
            Else
                If (Application.win.cmbCurlShape.Text = "Catenary") Then
                    CurlShapePara = 1
                End If
            End If

            'PrintLine(8, SurfTemp & " " & BotmTemp & " " & CurlShapePara & " " & ThermCoef)  'for INGRID 120413 YGC 111314
            PrintLine(8, LETG & " " & CurlShapePara & " " & ThermCoef)

        End If
        'Add end by YGC 122210

        If istep = 1 Then Call CreateRgdParms(OpenMode.Output) 'YC 070516

        'YGC 111314
        If Application.win.IsMovingLoad Then
            istep = istep + 1

            If istep <= NMovingStep Then

                For I = 1 To NWheels
                    Xtr(I) = Xtr(I) + MovingDistanceX / (NMovingStep - 1)
                    Ytr(I) = Ytr(I) - MovingDistanceY / (NMovingStep - 1)
                Next I

                ' YC 101216
                ReDim Preserve ACLoad(istep)  'YC 070516
                ACLoad(istep) = ACLoad(1)

                With ACLoad(istep)
                    ReDim .XTr(NWheels), .YTr(NWheels)
                    For I = 1 To NWheels
                        .XTr(I) = Xtr(I)
                        .YTr(I) = -Ytr(I)
                    Next
                End With
                ' YC 101216 END

                GoTo 1420
            End If

        End If
        'YGC 111314

        FileClose(8)


        IO.File.Delete(TextFileName)   ' YC 040620


    End Sub



    Sub CreateIngParms(ByVal INGString As String)
        ' to create .ing parameters by YC 070516

        'NLcd = NCat(iCat)

        'ReDim LCD(NLcd)

        'Dim iLcd, jLcd As Integer
        'For iLcd = 1 To NLcd
        '    With LCD(iLcd)
        '        ReDim .LdPntTime(NLcd + 1), .LdPntMag(NLcd + 1)
        '        For jLcd = 1 To NLcd + 1
        '            .LdPntTime(jLcd) = jLcd - 1
        '            If jLcd <> iLcd + 1 Then
        '                .LdPntMag(jLcd) = 0
        '            Else
        '                .LdPntMag(jLcd) = 1
        '            End If
        '        Next jLcd
        '    End With
        'Next iLcd



        'Dim NSld As Integer = NSlabs
        'If Overlay And Factor2 <= 0.1 Then NSld = 2 * NSlabs 'rigid overlay

        'ReDim SLD(NSld)

        'Dim iSld As Integer
        'For iSld = 1 To NSld
        '    With SLD(iSld)
        '        .IdxSld = iSld
        '        .TypSld = "sv"
        '        .PenaltySld = Factor1
        '        .FrictionSld = 0.0
        '        If Overlay And Factor2 <= 0.1 And iSld >= CInt(NSld / 2) + 1 Then .FrictionSld = Friction 'rigid overlay
        '        .TypSldMerge = "nomerge"
        '    End With
        'Next iSld



        'Dim NMat As Integer = 11
        'If Overlay Then NMat = 12
        'ReDim MAT(NMat)

        'Dim iMat As Integer
        'For iMat = 1 To NMat
        '    MAT(iMat).IdxMat = iMat
        '    ReDim MAT(iMat).MatPara(6, 8)
        'Next

        'For iMat = 1 To 6
        '    MAT(iMat).KeyMatPrpty = 1
        '    MAT(iMat).MatPara(1, 1) = EMod(iMat)
        '    MAT(iMat).MatPara(2, 1) = PoissonsRatio(iMat)
        '    MAT(iMat).DensityMat = 0.0001872
        'Next iMat
        'MAT(1).DensityMat = 0.083912

        'For iMat = 7 To 11
        '    MAT(iMat).KeyMatPrpty = 56
        '    MAT(iMat).MatPara(1, 1) = EMod(6)
        '    MAT(iMat).MatPara(2, 1) = PoissonsRatio(6)
        'Next iMat
        'MAT(7).DensityMat = 6
        'MAT(8).DensityMat = 1
        'MAT(9).DensityMat = 2
        'MAT(10).DensityMat = 4
        'MAT(11).DensityMat = 5

        'If Overlay Then
        '    iMat = 12
        '    MAT(iMat).IdxMat = iMat
        '    MAT(iMat).KeyMatPrpty = 1
        '    MAT(iMat).MatPara(1, 1) = EMod(0)
        '    MAT(iMat).MatPara(2, 1) = PoissonsRatio(0)
        '    MAT(iMat).DensityMat = 0.083912
        'End If


        'Dim NPart As Integer = 10
        'ReDim PART(NPart)

        'Dim iPart As Integer = 1
        'With PART(iPart)
        '    .NXCtr = 3
        '    .NYCtr = 3

        '    ReDim .NdXCtr(3), .NdYCtr(3)
        '    ReDim .CoordXCtr(3), .CoordYCtr(3)
        '    .NdXCtr(1) = 1 : .NdXCtr(2) = 1 + NX1 : .NdXCtr(3) = 1 + NX1 + NX2
        '    .NdYCtr(1) = 1 : .NdYCtr(2) = 1 + NY1 : .NdYCtr(3) = 1 + NY1 + NY2
        '    .CoordXCtr(1) = 0.0 : .CoordXCtr(2) = 210.0 : .CoordXCtr(3) = 300
        '    .CoordYCtr(1) = 0.0 : .CoordYCtr(2) = 60.0 : .CoordYCtr(3) = 150

        '    If Overlay Then
        '        If Factor2 > 0.1 Then  'AC Overlay 
        '            .NZCtr = 3
        '            ReDim .NdZCtr(3), .CoordZCtr(3)
        '            .NdZCtr(1) = 1 : .NdZCtr(2) = 2 : .NdZCtr(3) = 3
        '            .CoordZCtr(1) = 0.0 : .CoordZCtr(2) = -tkpcc : .CoordZCtr(3) = -tkpcc - tkol
        '        ElseIf Factor2 <= 0.1 Then 'PCC overlay 
        '            .NZCtr = 2
        '            ReDim .NdZCtr(2), .CoordZCtr(2)
        '            .NdZCtr(1) = 1 : .NdZCtr(2) = 2
        '            .CoordZCtr(1) = -tkpcc : .CoordZCtr(2) = -tkpcc - tkol
        '        End If
        '    Else    'New PCC
        '        .NZCtr = 2
        '        ReDim .NdZCtr(2), .CoordZCtr(2)
        '        .NdZCtr(1) = 1 : .NdZCtr(2) = 2
        '        .CoordZCtr(1) = 0.0 : .CoordZCtr(2) = -tkpcc
        '    End If

        'End With

        'If Overlay And Factor2 <= 0.1 Then 'PCC overlay
        '    iPart = iPart + 1
        '    PART(iPart) = PART(iPart - 1)
        '    With PART(iPart)
        '        .NZCtr = 2
        '        ReDim .NdZCtr(2), .CoordZCtr(2)
        '        .NdZCtr(1) = 1 : .NdYCtr(2) = 2
        '        .CoordZCtr(1) = 0.0 : .CoordZCtr(2) = -tkpcc
        '    End With
        'End If


        NLcd = 0 : NSld = 0 : NMat = 0 : NPart = 0
        NPartSld = 0 : NPartBC = 0 : NPartMat = 0

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

            STRFileLines = Split(INGString, vbCrLf)
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
                MsgBox("Error: The number of si+ is not equal to the number of si- in .Ing file", MsgBoxStyle.OkOnly, "File Error")
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

            itpro = 0

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
                        MsgBox("The number of load and the nuumber of load point defintion is not consistent!" & vbCrLf, MsgBoxStyle.OkOnly, "File Error")
                        Exit Sub
                    End If

                    ReDim LCD(iLcd).LdPntTime(NumLdPnt), LCD(iLcd).LdPntMag(NumLdPnt)

                    isLdPntReading = True
                    j = 1

                    GoTo nxtln
                End If

                If isLdPntReading Then

                    For Each m In reLdPnt.Matches(ReadLineString)
                        LCD(iLcd).LdPntTime(j) = CSng(m.Groups("ldtimpnt").Value)
                        LCD(iLcd).LdPntMag(j) = CSng(m.Groups("ldmagpnt").Value)
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
                        SLD(iSld).PenaltySld = CSng(m.Groups("sldpnlt").Value)
                        SLD(iSld).FrictionSld = CSng(m.Groups("sldfric").Value)
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
                                MAT(iMat).MatPara(i, j) = CSng(m.Value)
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
                                MAT(iMat).MatPara(i, j) = CSng(m.Value)
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
                                MAT(iMat).MatPara(i, j) = CSng(m.Value)
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
                                MAT(iMat).MatPara(i, j) = CSng(m.Value)
                                j = j + 1
                            End If
                        Next
                        GoTo nxtln
                    End If

                    If Regex.IsMatch(ReadLineString, "(ro)(\s+)") Then
                        For Each m In reScientific.Matches(ReadLineString)
                            MAT(iMat).DensityMat = CSng(m.Value)
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
                        PART(iPart).CoordXCtr(j) = CSng(m.Value)
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
                        PART(iPart).CoordYCtr(j) = CSng(m.Value)
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
                        PART(iPart).CoordZCtr(j) = CSng(m.Value)
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
            MsgBox("Error " & ex.Message & vbCr & "Occured in Sub CreateIngParms.", MsgBoxStyle.OkOnly, "File Error")
            Exit Sub
        End Try

    End Sub

    Sub CreateRgdParms(ByVal OpenModeV As OpenMode)
        ' to create .rgd parameters by YC 070516

        'Static iAC As Integer

        'If OpenModeV = OpenMode.Output Then
        '    ReDim ACLoad(NCat(iCat))
        '    iAC = 1
        'ElseIf OpenModeV = OpenMode.Append Then
        '    iAC = iAC + 1
        'End If

        ReDim ACLoad(1)

        Dim i, iSlab As Integer

        With ACLoad(1)
            .NWhls = NWheels
            .PcntCt = CSng(TirePressure)

            ReDim .XTr(NWheels), .YTr(NWheels)
            For i = 1 To NWheels
                .XTr(i) = Xtr(i)
                .YTr(i) = -Ytr(i)
            Next

            .XDim = Xfp
            .YDim = Yfp
            .IndxSy = IndxSymm
            .ScaleX = Application.win.XScaleFactor
            .ScaleY = Application.win.YScaleFactor
            .NSlabs = NSlabs

            ReDim .XGrid(NSlabs), .YGrid(NSlabs)
            For iSlab = 1 To NSlabs
                .XGrid(iSlab) = Xgrid
                .YGrid(iSlab) = Ygrid
            Next iSlab

            .f1 = Factor1
            .f2 = Factor2

            .Alpha = Application.win.GearAngle

            .EqStifX = EqJStfX
            .EqStifY = EqJStfY
            .KeyEqStif = 1

            .EqStif12X = 10000000000.0
            .EqStif12Y = 10000000000.0
            .KeyEqStif12 = 0

            .EqEdgStifX = EqEdgStfX
            .EqEdgStifY = EqEdgStfY
            .KeyEqEdgStif = 1

            .IndxIm = IndxModes
            .IndxFn = IndxFndn
            .ThkSlb = LayerThickness(1)
            .IndxOvrl = IndxOvrl

            If IndxOvrl = 1 Then .ThkOvr = LayerThickness(0)

            If itpro = 1 Then
                .IsThermal = True
            Else
                .IsThermal = False
            End If


            If Application.win.chkTempLoad.IsChecked Then
                .LETG = LETG
                .CurlShapara = CurlShapePara
                .Cte = ThermCoef
            End If

        End With

    End Sub



    Sub FAAMESHCall()
        ' YC 040620

        'WorkingDir0 = Environment.CurrentDirectory & "\"     ' YC 040620-1
        WorkingDir0 = gJobFileFolder & "\"


        Dim RunMesh As New FAAMeshClassLib.clsMesh()
        'Call RunMesh.MeshGeneration(LCD, SLD, MAT, PART, PARTSLD, PARTBC, PARTMAT, ACLoad, WorkingDirectory)

        Call RunMesh.MeshGeneration(LCD, Nd, BrickElement, SpringType, SpringElement, SlidingElement, NodalLoad,
                            SLD, MAT, PART, PARTSLD, PARTBC, PARTMAT, ACLoad, WorkingDir0, JobName, ModelOut)   '  QW 09-15-2019 YC 040620

    End Sub



    Sub FAASR3DCall()
        ' YC 040620

        Call Conversion()

        Dim RunFEM As New FEMClassLib.clsFEM()
        Call RunFEM.FAASR3D(IPC, Stress1, Stress8, StopFEDFAA, FEDFAAStopped, CShort(gDesignType), iSymCase, WorkingDir0, JobName, ModelOut)

    End Sub

    Sub Conversion()
        ' QW 08-23-2017 YC 040620
        Dim i, j, k, n As Integer
        With IPC
            NMat = MAT.Length - 1
            .nmmat = NMat
            .inpsd = 1
            .ntime = ACLoad.Length - 1
            .nlcur = LCD.Length - 1
            .nptm = LCD.Length
            .nload = 0
            n = .ntime
            For i = 1 To n
                .nload = .nload + NodalLoad(i).NNodalLoad
            Next i
            .numpc = 0
            .numdc = 0
            .nrcc = 0
            n = Nd.Length - 1
            For i = 0 To n - 2
                If Nd(i + 1).X = 0 And Nd(i + 1).Y = 0 And Nd(i + 1).Z = 0 And Nd(i + 2).X = 0 And Nd(i + 2).Y = 0 And Nd(i + 2).Z = 0 Then
                    GoTo out
                End If
            Next i
out:        .numnp = i + 1
            .numelh = BrickElement.Length - 1
            .nmmtde = SpringType.Length - 1
            .nmelde = SpringElement.Length - 1
            .nmmass = 0

            ' Material
            ReDim .matype(NMat), .den(NMat), .prop(48, NMat)
            For i = 1 To NMat
                .matype(i) = MAT(i).KeyMatPrpty
                .den(i) = MAT(i).DensityMat
                For j = 1 To 6
                    For k = 1 To 8

                        ' YC 121219
                        '.prop(6 * (k - 1) + j, i) = MAT(i).MatPara(j, k)
                        If .matype(i) = 1 Or .matype(i) = 56 Then
                            .prop(6 * (k - 1) + j, i) = MAT(i).MatPara(j, k)
                        ElseIf .matype(i) = 4 Then
                            .prop(8 * (j - 1) + k, i) = MAT(i).MatPara(j, k)
                        End If
                        ' YC 121219 END

                    Next k
                Next j
            Next i
            ' Node and element
            ReDim .idp(6, .numnp), .x(3, .numnp), .ia(9 * .numelh - 1), .nob(.numnp)
            For i = 1 To .numnp
                .nob(i) = Nd(i).BXYZ
                For j = 1 To 6
                    .idp(j, i) = 0
                Next j
                For j = 1 To 3
                    If Nd(i).BXYZ = j Then .idp(j, i) = 1
                Next j
                If Nd(i).BXYZ = 4 Then
                    .idp(1, i) = 1 : .idp(2, i) = 1
                End If
                If Nd(i).BXYZ = 5 Then
                    .idp(2, i) = 1 : .idp(3, i) = 1
                End If
                If Nd(i).BXYZ = 6 Then
                    .idp(1, i) = 1 : .idp(3, i) = 1
                End If
                If Nd(i).BXYZ = 7 Then
                    .idp(1, i) = 1 : .idp(2, i) = 1 : .idp(3, i) = 1
                End If
                For j = 1 To 3
                    If Nd(i).BRXYZ = j Then .idp(j + 3, i) = 1
                Next j
                If Nd(i).BRXYZ = 4 Then
                    .idp(4, i) = 1 : .idp(5, i) = 1
                End If
                If Nd(i).BRXYZ = 5 Then
                    .idp(5, i) = 1 : .idp(6, i) = 1
                End If
                If Nd(i).BRXYZ = 6 Then
                    .idp(4, i) = 1 : .idp(6, i) = 1
                End If
                If Nd(i).BRXYZ = 7 Then
                    .idp(4, i) = 1 : .idp(5, i) = 1 : .idp(6, i) = 1
                End If
                .x(1, i) = Nd(i).X
                .x(2, i) = Nd(i).Y
                .x(3, i) = Nd(i).Z
            Next i
            For i = 0 To .numelh - 1
                .ia(9 * i) = BrickElement(i + 1).IdxMat
                For j = 1 To 8
                    .ia(9 * i + j) = BrickElement(i + 1).Node(j)
                Next j
            Next i
            ' Discrete element
            ReDim .cmde(2, .nmmtde), .mtypde(.nmmtde), .ixde(3, .nmelde), .sclf(.nmelde)
            For i = 1 To .nmmtde
                .cmde(1, i) = SpringType(i).Stiffness
                .cmde(2, i) = SpringType(i).Direction
                .mtypde(i) = 1
            Next i
            For i = 1 To .nmelde
                .ixde(1, i) = SpringElement(i).Node1
                .ixde(2, i) = SpringElement(i).Node2
                .ixde(3, i) = SpringElement(i).IdxSprType
                .sclf(i) = 1
            Next i
            ' Sliding surface element

            .numsv = CInt((PARTSLD.Length - 1) / 2)
            ReDim .iparm(7, .numsv), .fric(3, .numsv), .pend(.numsv), .stfsf(.numsv), .ngap(.numsv)
            ReDim .iaug(.numsv), .sfact(.numsv), .altol(2, .numsv), .tdeath(.numsv), .tbury(.numsv), .ifd(.numsv)
            .nrttls = 0 : .nrttlm = 0
            For i = 1 To .numsv
                .nrttls = .nrttls + SlidingElement(2 * i - 1).NSldElement
                .nrttlm = .nrttlm + SlidingElement(2 * i).NSldElement
                .iparm(1, i) = SlidingElement(2 * i - 1).NSldElement
                .iparm(2, i) = SlidingElement(2 * i).NSldElement
                .iparm(5, i) = SlidingElement(2 * i).IdxDirection
                .stfsf(i) = SlidingElement(2 * i).PenaltySld
                If Math.Abs(SlidingElement(2 * i).FrictionSld - 0.005) < 0.0001 Then
                    .fric(1, i) = 0.005
                    .sfact(i) = ACLoad(1).f2

                    '.sfact(i) = CDbl(LPad(10, Format(ACLoad(1).f2, "0.000E+00"))) ' YC? 102418-041519-
                    .iacc = 1   ' YC 102418-041519
                Else                            ' FrictionSld only set equal to 0.005 or 0 in Sub Material_No_Inf()
                    .fric(1, i) = 0.0
                    .sfact(i) = 0.0

                    .iacc = 0   ' YC 102418-041519

                End If
                .iaug(i) = 0 : .ifd(i) = 0
                .altol(1, i) = 0.0 : .altol(2, i) = 0.0
                .tdeath(i) = 0.0 : .tbury(i) = 0.0
                .fric(2, i) = 0.0 : .fric(3, i) = 0.0
                If .stfsf(i) = 0.0 Then .stfsf(i) = 1.0
                If .ngap(i) = 0 Then .ngap(i) = 999999
                If .ngap(i) < 0 Then .ngap(i) = 0
            Next i
            ReDim .irects(4, .nrttls), .irectm(4, .nrttlm)
            n = 0
            For i = 1 To 2 * .numsv Step 2
                For j = 1 To SlidingElement(i).NSldElement
                    For k = 1 To 4
                        .irects(k, n + j) = SlidingElement(i).Element(j, k)
                    Next k
                Next j
                n = n + SlidingElement(i).NSldElement   ' QW 04-15-2019
            Next i
            n = 0
            For i = 2 To 2 * .numsv Step 2
                For j = 1 To SlidingElement(i).NSldElement
                    For k = 1 To 4
                        .irectm(k, n + j) = SlidingElement(i).Element(j, k)
                    Next k
                Next j
                n = n + SlidingElement(i).NSldElement   ' QW 04-15-2019
            Next i

            ' Load
            NAC = ACLoad.Length - 1

            ReDim .nod(.nload), .idirn(.nload), .ncur(.nload), .fac(.nload), .npc(.nlcur + 1), .pld(.nlcur * .nptm * 2)
            ReDim .tmode(.numnp), .cnwmk(2), .tbase(.numnp)

            'Dim timv(.nptm), rv(.nptm) As double   ' YC 102418-041519
            Dim timv(.nptm), rv(.nptm) As Double

            .npc(1) = 1
            For i = 1 To .nlcur
                For j = 1 To .nptm
                    timv(j) = LCD(i).LdPntTime(j)
                    rv(j) = LCD(i).LdPntMag(j)
                Next j
                .npc(i + 1) = .npc(i) + 2 * .nptm
                j = .npc(i)
                For k = 1 To .nptm
                    .pld(j) = timv(k)
                    j = j + 1
                    .pld(j) = rv(k)
                    j = j + 1
                Next k
            Next i
            .nptst = .npc(.nlcur + 1)
            k = 0
            For i = 1 To NAC
                For j = 1 To NodalLoad(i).NNodalLoad
                    .nod(k + j) = NodalLoad(i).Node(j)
                    .idirn(k + j) = 3
                    .ncur(k + j) = i
                    .fac(k + j) = -NodalLoad(i).Load(j)
                Next j
                k = k + NodalLoad(i).NNodalLoad
            Next i

            If ACLoad(1).IsThermal Then
                .itemp = 1 : .itread = 1
                .cnwmk(1) = 0.5 : .cnwmk(2) = 0.25
                For i = 1 To .numnp
                    .tmode(i) = Nd(i).Temp
                    .tbase(i) = 0
                Next i
            Else
                .itemp = 0 : .itread = 0
                .cnwmk(1) = 0 : .cnwmk(2) = 0
                For i = 1 To .numnp
                    .tmode(i) = 0
                    .tbase(i) = 0
                Next i
            End If

        End With

        ' PathSet

    End Sub


End Module

'http://www.codeguru.com/csharp/.net/net_asp/miscellaneous/article.php/c6975
'http://ondotnet.com/pub/a/dotnet/2001/07/30/vb7.html


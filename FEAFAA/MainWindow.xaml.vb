Imports System.Data


Class MainWindow
    Private m_IsInitializing_Tab As Boolean = True 'VK: to simulate the same event behaviour
    Private m_IsInitializing_Tab3 As Boolean = True 'VK: to simulate the same event behaviour
    Private m_IsInitializing As Boolean = True



    Public Property IsInitializing() As Boolean
        Get
            Return m_IsInitializing
        End Get
        Set(ByVal Value As Boolean)
            m_IsInitializing = Value
        End Set
    End Property

    'New stuff for canvas / picturebox conversion
    Public topLeft As Point
    Public bottomRight As Point

    Private _lastSelectedWheel As Integer

    Public MainTitle As String
    Public SlabsLeft As Double, SlabsRight As Double
    Public SlabsTop As Double, SlabsBottom As Double ' Modified by YGC 060910

    Dim GraphScaleX, GraphScaleY As Double
    Dim XCenter, YCenter As Double

    Dim grdDataTable As New System.Data.DataTable
    Dim ACparams As DataColumn
    Dim Values As DataColumn

    'Tab 2 Pavement Structure
    Public grdLayerTable As New System.Data.DataTable
    Dim colLayers As DataColumn
    Dim colModulus As DataColumn
    Dim colPoissonRatio As DataColumn
    Dim colThickness As DataColumn


    Private mintCurFrame As Short ' Current Frame visible

    Dim Dragging As Boolean
    Dim DragX, DragY As Double

    Public Xcg, Ycg As Double
    Public WhlWidth, WhlLength As Double
    Public GearParallel, SelfWeight As Boolean
    Public InteriorLoad, EdgeLoad As Boolean
    Public XScaleFactor, YScaleFactor As Double

    Public StartWheelIndex As Short
    Public Cutoff As String

    'Tab 2 Pavement Structure
    Public NumberOfLayers As Short
    Private EMod(6), PoissonsRatio(6), LayerThickness(6) As Double
    Public E4 As Double

    'Tab 2 Pavement Structure Slab Mesh
    'Nslabs defined in Module modAutoMesh (modGP.vb)

    'Tab 2 Pavement Structure - Loading
    Public GearAngle As Double
    Public XCoord, YCoord As Double
    Public XCoordMoved, YCoordMoved As Double

    Public IsDisplayMovingLoad As Boolean

    Dim PositionChk As Boolean = False

    'Tab 2 Pavement Structure - Slab Temperature
    'SurtTemp BotmTemp ThermCoef CurlShapePara defined in Module modAutoMesh (modGP.vb)


    Public KSubgrade As Double
    Public Winkler, InfiniteElement As Boolean

    Public SlabTop, SlabLeft, SlabRight, SlabBottom As Double
    Public IFParam As Integer
    Public MaxXOffset, MinXOffset, MaxYOffset, MinYOffset As Double
    Public LogicalXOffset, LogicalYOffset As Boolean
    Public ScaleFactor As Double
    Public MaxVal, MinVal As Double
    Dim icount As Short
    Public Modes, Overlay As Boolean
    Public PCARectangle As Boolean
    Public PointXmax, PointXmin, PointYmax, PointYmin As Double 'YGC 080612
    Public NSlabsX As Integer, NSlabsY As Integer 'YGC 080612
    Public SlabsEdgeX(4) As Double, SlabsEdgeY(4) As Double 'YGC 080612
    Public InputInfoChk As Boolean 'YGC 080612
    Public IsMovingLoad As Boolean
    Public ErrorInput As Boolean

    Private Sub MainWindow_Closing(sender As Object, e As ComponentModel.CancelEventArgs) Handles Me.Closing
        'Dim Cancel As Boolean = EventArgs.Cancel
        ' Dim UnloadMode As System.Windows.Forms.CloseReason = EventArgs.CloseReason

        Dim I As Short

        If NWheels <> libNTires(LibIndex) Then GoTo ExitDataChanged
        If GrossWeight <> libGL(LibIndex) Then GoTo ExitDataChanged
        If NMainGears <> libNMainGears(LibIndex) Then GoTo ExitDataChanged
        If PcntOnMainGears <> libPcntOnMainGears(LibIndex) Then GoTo ExitDataChanged
        If TirePressure <> libCP(LibIndex) Then GoTo ExitDataChanged
        For I = 1 To NWheels
            If XWheels(I) <> libTY(LibIndex, I) Then GoTo ExitDataChanged
            If YWheels(I) <> libTX(LibIndex, I) Then GoTo ExitDataChanged
        Next I

        Exit Sub

ExitDataChanged:

        S = "Data for the current gear has changed." & NL2
        S = S & "Do you want to save the new data" & vbCrLf
        S = S & "before returning to windows?"
        Ret = MsgBox(S, MsgBoxStyle.YesNoCancel, "Save Current Data?")
        If Ret = MsgBoxResult.Yes Then
            BtnSaveACinLibrary_Click(BtnSaveACinLibrary, Nothing) 'CType(Me.Controls("cmdSaveACinLibrary"), Object).PerformClick()
            Call UpdateDataFromLibrary(LibIndex)
        ElseIf Ret = MsgBoxResult.Cancel Then
            e.Cancel = True
        End If
        e.Cancel = False 'EventArgs.Cancel = Cancel
    End Sub

    Private Sub MainWindow_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        If (e.Key = Key.F1) Then
            ShowHelp()
        End If
    End Sub

    Private Sub MainWindow_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded
        Me.IsInitializing = False

        Dim I As Integer

        MainTitle = Me.Title

        XScaleFactor = 1.0 : YScaleFactor = 1.0
        E4 = 22500 : Cutoff = "240" : Winkler = False
        KSubgrade = (22500.0# / 26.0#) ^ 0.778816199

        'Tab2 Layers data
        NumberOfLayers = 4
        EMod(0) = 4000000 : PoissonsRatio(0) = 0.15 : LayerThickness(0) = 12 'PCC Overlay Default Properties
        EMod(1) = 4000000 : EMod(2) = 500000 : EMod(3) = 75000 : EMod(4) = 75000 : EMod(5) = 75000 : EMod(6) = 15000

        PoissonsRatio(1) = 0.15 : PoissonsRatio(2) = 0.2 : PoissonsRatio(3) = 0.35
        PoissonsRatio(4) = 0.35 : PoissonsRatio(5) = 0.35 : PoissonsRatio(6) = 0.4
        LayerThickness(1) = 14.0# : LayerThickness(2) = 8.0# : LayerThickness(3) = 12.0#
        LayerThickness(4) = 12.0# : LayerThickness(5) = 12.0# : LayerThickness(6) = 0.0#


        'Tab2 Slab Mesh
        cmbDimX.SelectedItem = "25" : cmbDimY.SelectedItem = "25"


        cmbNSlabs.SelectedItem = "1"    'v3.0 004
        cmbMesh.SelectedItem = "30" : cmbFdMesh.SelectedItem = "30"


        NSlabs = 1

        'Tab2 Loading
        GearAngle = 0
        GearParallel = True

        cmbNSlabs.ItemsSource = New Object() {"1", "2", "4", "6", "9"}
        cmbDimX.ItemsSource = New Object() {"13", "14", "15", "18.75", "20", "25", "30", "50"}
        cmbDimY.ItemsSource = New Object() {"13", "14", "15", "18.75", "20", "25", "30", "50"}
        cmbMesh.ItemsSource = New Object() {"15", "20", "25", "30", "35", "40", "50", "60", "80", "90", "100", "120"}
        cmbFdMesh.ItemsSource = New Object() {"15", "20", "25", "30", "35", "40", "50", "60", "80", "90", "100", "120"}
        cmbAngle.ItemsSource = New Object() {"0", "30", "45", "60", "90"}

        'cmbCurlShape.ItemsSource = New Object() {"Circular", "Catenary"}   ' v3.0 002 YC 052620
        cmbCurlShape.ItemsSource = New Object() {"Spherical", "Catenary"}

        cmbLoadingType.ItemsSource = New Object() {"Static Load", "Moving Load"}
        cmbLoadingType.SelectedIndex = 0

        'Tab3 Joint Modeling
        optFreshX.IsChecked = True
        optFreshY.IsChecked = True

        EqJStfX = 100000 : EqJStfY = 100000
        ' added for boundary(Edge) spring by YGC 110110
        EqEdgStfX = 1000 : EqEdgStfY = 1000
        cmbCurlShape.SelectedIndex = 0

        'VK: added combos items initialization
        cmbDowelDiameterX.ItemsSource = New Object() {"0", "0.5", "0.625", "0.75", "0.875", "1.0", "1.125", "1.25", "1.375", "1.5"}
        cmbDowelSpacingX.ItemsSource = New Object() {"0", "6", "9", "12", "15", "18", "21", "24"}
        cmbJointOpeningX.ItemsSource = New Object() {"0.125", "0.25", "0.375", "0.50", "0.625"}
        cmbDowelDiameterY.ItemsSource = New Object() {"0", "0.5", "0.625", "0.75", "0.875", "1.0", "1.125", "1.25", "1.375", "1.5"}
        cmbDowelSpacingY.ItemsSource = New Object() {"0", "6", "9", "12", "15", "18", "21", "24"}
        cmbJointOpeningY.ItemsSource = New Object() {"0.125", "0.25", "0.375", "0.50", "0.625"}

        modAutoMesh.CrackedBase = False
        SelfWeight = False
        ''Dir1.Path = Drive1.Drive

        'gJobFileFolder = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)   ' YC 040620-1
        gJobFileFolder = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\FEAFAAWorkByEbraSha"
        If Not IO.Directory.Exists(gJobFileFolder) Then
            IO.Directory.CreateDirectory(gJobFileFolder)
        End If


        'Dir1.SelectedPath = Environment.CurrentDirectory   ' YC 040620-1
        Dir1.SelectedPath = gJobFileFolder


        Dir1.IsEnabled = False
        'txtJobDirectory.IsEnabled = False
        'txtJobDirectory.Background = Brushes.White
        txtJobDirectory.IsReadOnly = True
        txtJobDirectory.Text = gJobFileFolder


        'txtFilNm.Text = "nikein"    ' YC 040620
        txtFilNm.Text = "faasrin"


        LogicalYOffset = False

        ' added for Boundary(Edge) spring by YGC 110110
        Modes = True : Overlay = False
        OptHex.IsChecked = True
        ChkOverlay.IsChecked = False

        'Tab1 Vehicle Parameters
        GrossWeightRow = 0     '.Row = GrossWeightRow : .Text = " Gross Weight (lbs)"
        PcntOnMainGearsRow = 1 '.Row = PcntOnMainGearsRow : .Text = " % GW on Main Gears"
        NMainGearsRow = 2      '.Row = NMainGearsRow : .Text = " No. Main Gears"
        NWheelsRow = 3         '.Row = NWheelsRow : .Text = " Wheels on Main Gear"
        TirePressureRow = 4    '.Row = TirePressureRow : .Text = " Tire Pressure (psi)"

        PCARectangle = True

        Call InitACLib()

        ListBoxAirplaneGroup.ItemsSource = LibACGroupName.Where(Function(g) g IsNot Nothing)
        ListBoxAirplaneGroup.SelectedIndex = 0

        I = lstLibFileIndex
        lstLibFileIndex = I ' Select last group.
        ListBoxLibraryAirplane.SelectedIndex = lstLibFileIndex



        'Tab1 Vehicle Parameters
        GridParams.DataContext = GridParams.Model

        Call DrawGear()
        Call InitGrdLayerData()

        mintCurFrame = 1

        ChDir(My.Application.Info.DirectoryPath)
        ExtFilePath = My.Application.Info.DirectoryPath & "\"

        'lblXCoord.BackColor = BackColor : lblYCoord.BackColor = BackColor
        'lblXSelected.BackColor = BackColor : lblYSelected.BackColor = BackColor

        Call ELPGrid_PavementStructure()

        TabPage1_Enter()
        TabPage2_Enter()
        TabPage3_Enter()
        TabPage1_Enter()

        If chkCrackedBase.IsChecked Then modAutoMesh.CrackedBase = True
        If Not chkCrackedBase.IsChecked Then modAutoMesh.CrackedBase = False

        m_IsInitializing_Tab = False
    End Sub

    Sub ELPGrid_PavementStructure()
        If Not ChkOverlay.IsChecked Then

            For I = 1 To NumberOfLayers - 1
                grdLayerTable(I - 1)(1) = Format(EMod(I), "#,###,##0")
                grdLayerTable(I - 1)(2) = Format(PoissonsRatio(I), "0.00")
                grdLayerTable(I - 1)(3) = Format(LayerThickness(I), "0.00")
            Next

            grdLayerTable(NumberOfLayers - 1)(1) = Format(EMod(6), "#,###,##0")
            grdLayerTable(NumberOfLayers - 1)(2) = Format(PoissonsRatio(6), "0.00")
            grdLayerTable(NumberOfLayers - 1)(3) = "Infinite"

        Else 'PCC Overlay

            For I = 0 To NumberOfLayers - 2
                grdLayerTable(I)(1) = Format(EMod(I), "#,###,##0")
                grdLayerTable(I)(2) = Format(PoissonsRatio(I), "0.00")
                grdLayerTable(I)(3) = Format(LayerThickness(I), "0.00")
            Next

            grdLayerTable(NumberOfLayers - 1)(1) = Format(EMod(6), "#,###,##0")
            grdLayerTable(NumberOfLayers - 1)(2) = Format(PoissonsRatio(6), "0.00")
            grdLayerTable(NumberOfLayers - 1)(3) = "Infinite"

        End If

    End Sub

    Sub DrawGear()
        ' New subroutine to display variable slabs by YGC 060110

        '***************************************************************************************
        '********************************display slab********************************************
        Dim TireLoad As Double, EqWheelArea As Double, ContactLength As Double
        Dim Point1X, Point1Y, Point2X, Point2Y As Double
        Dim Point3X, Point3Y, Point4X, Point4Y As Double

        Dim SlabsDimensionX As Double, SlabsDimensionY As Double
        Dim SlabDspDimX As Double, SlabDspDimY As Double
        Dim Slab1OrgX As Double, Slab1OrgY As Double


        picSlab.Children.Clear()

        Dim lblY As Label = New Label()
        lblY.Content = "Y"
        Canvas.SetLeft(lblY, 10)
        Canvas.SetTop(lblY, 10)
        picSlab.Children.Add(lblY)

        Dim lblX As Label = New Label()
        lblX.Content = "X"
        Canvas.SetLeft(lblX, 219)
        Canvas.SetTop(lblX, 181)
        picSlab.Children.Add(lblX)

        If NSlabs = 9 Then
            NSlabsX = 3
            NSlabsY = 3
            lblXOffset.ToolTip = "Enter offset in the range [" & -CInt(cmbDimX.Text) * 12 & "," & CInt(cmbDimX.Text) * 12 * 2 & "]"
            lblXOffsetMoved.ToolTip = "Enter offset in the range [" & -CInt(cmbDimX.Text) * 12 & "," & CInt(cmbDimX.Text) * 12 * 2 & "]"
            lblYOffset.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 * 3 / 2 & "," & CInt(cmbDimY.Text) * 12 * 3 / 2 & "]"
            lblYOffsetMoved.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 * 3 / 2 & "," & CInt(cmbDimY.Text) * 12 * 3 / 2 & "]"
        ElseIf NSlabs = 6 Then
            NSlabsX = 3
            NSlabsY = 2
            lblXOffset.ToolTip = "Enter offset in the range [" & -CInt(cmbDimX.Text) * 12 & "," & CInt(cmbDimX.Text) * 12 * 2 & "]"
            lblXOffsetMoved.ToolTip = "Enter offset in the range [" & -CInt(cmbDimX.Text) * 12 & "," & CInt(cmbDimX.Text) * 12 * 2 & "]"
            lblYOffset.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 / 2 & "," & CInt(cmbDimY.Text) * 12 * 3 / 2 & "]"
            lblYOffsetMoved.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 / 2 & "," & CInt(cmbDimY.Text) * 12 * 3 / 2 & "]"
        ElseIf NSlabs = 4 Then
            NSlabsX = 2
            NSlabsY = 2
            lblXOffset.ToolTip = "Enter offset in the range [" & -CInt(cmbDimX.Text) * 12 & "," & CInt(cmbDimX.Text) * 12 & "]"
            lblXOffsetMoved.ToolTip = "Enter offset in the range [" & -CInt(cmbDimX.Text) * 12 & "," & CInt(cmbDimX.Text) * 12 & "]"
            lblYOffset.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 / 2 & "," & CInt(cmbDimY.Text) * 12 * 3 / 2 & "]"
            lblYOffsetMoved.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 / 2 & "," & CInt(cmbDimY.Text) * 12 * 3 / 2 & "]"
        ElseIf NSlabs = 2 Then
            NSlabsX = 2
            NSlabsY = 1
            lblXOffset.ToolTip = "Enter offset in the range [" & -CInt(cmbDimX.Text) * 12 & "," & CInt(cmbDimX.Text) * 12 & "]"
            lblXOffsetMoved.ToolTip = "Enter offset in the range [" & -CInt(cmbDimX.Text) * 12 & "," & CInt(cmbDimX.Text) * 12 & "]"
            lblYOffset.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 / 2 & "," & CInt(cmbDimY.Text) * 12 / 2 & "]"
            lblYOffsetMoved.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 / 2 & "," & CInt(cmbDimY.Text) * 12 / 2 & "]"
        Else
            NSlabsX = 1
            NSlabsY = 1
            lblXOffset.ToolTip = "Enter offset in the range [" & 0 & "," & CInt(cmbDimX.Text) * 12 & "]"
            lblXOffsetMoved.ToolTip = "Enter offset in the range [" & 0 & "," & CInt(cmbDimX.Text) * 12 & "]"
            lblYOffset.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 / 2 & "," & CInt(cmbDimY.Text) * 12 / 2 & "]"
            lblYOffsetMoved.ToolTip = "Enter offset in the range [" & -CInt(cmbDimY.Text) * 12 / 2 & "," & CInt(cmbDimY.Text) * 12 / 2 & "]"
        End If



        '' Modified by YGC 060910
        SlabsDimensionX = NSlabsX * CInt(cmbDimX.Text)
        SlabsDimensionY = NSlabsY * CInt(cmbDimY.Text)


        If SlabsDimensionX > SlabsDimensionY Then
            SlabsLeft = 448
            SlabsRight = 2448
            SlabsTop = 1448 - (SlabsDimensionY / SlabsDimensionX) * 1000.0#
            SlabsBottom = 1448 + (SlabsDimensionY / SlabsDimensionX) * 1000.0#
        Else
            SlabsTop = 448
            SlabsBottom = 2448
            SlabsLeft = 1448 - (SlabsDimensionX / SlabsDimensionY) * 1000.0#
            SlabsRight = 1448 + (SlabsDimensionX / SlabsDimensionY) * 1000.0#
        End If

        SlabDspDimX = (SlabsRight - SlabsLeft) / NSlabsX
        SlabDspDimY = (SlabsBottom - SlabsTop) / NSlabsY
        '' Modify end by YGC 060910

        Dim sx, sy As Double
        sx = picSlab.Width / (SlabsRight + SlabsLeft) / 1.0
        sy = picSlab.Height / (SlabsBottom + SlabsTop) / 1.0

        'Canvas position
        Dim cnvTopLeft As Point = New Point(0, 0)
        Dim cnvBottomRight As Point = New Point(17, 21)

        For I = 1 To NSlabsX + 1
            SlabsEdgeY(I) = SlabsLeft + SlabDspDimX * (I - 1)
            Dim line1 As New Line()
            line1.Stroke = Brushes.Blue

            Dim pos1 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsEdgeY(I)), SlabsBottom), cnvTopLeft, cnvBottomRight)
            Dim pos2 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsEdgeY(I)), SlabsTop), cnvTopLeft, cnvBottomRight)
            line1.X1 = pos1.X
            line1.X2 = pos2.X
            line1.Y1 = pos1.Y
            line1.Y2 = pos2.Y
            line1.StrokeThickness = 0.5
            picSlab.Children.Add(line1)
        Next I

        For I = 1 To NSlabsY + 1
            SlabsEdgeX(I) = SlabsTop + SlabDspDimY * (I - 1)
            Dim line1 As New Line()
            line1.Stroke = Brushes.Blue
            Dim pos1 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsLeft, CSng(SlabsEdgeX(I))), cnvTopLeft, cnvBottomRight)
            Dim pos2 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsRight, CSng(SlabsEdgeX(I))), cnvTopLeft, cnvBottomRight)
            line1.X1 = pos1.X
            line1.X2 = pos2.X
            line1.Y1 = pos1.Y
            line1.Y2 = pos2.Y
            line1.StrokeThickness = 0.5
            picSlab.Children.Add(line1)
        Next I

        ' Define the origin of XY on Slab 1 by YGC 060110
        If NSlabs > 1 Then
            Slab1OrgX = SlabsEdgeY(2)
        Else
            Slab1OrgX = SlabsEdgeY(1)
        End If

        If NSlabsY Mod 2 = 0 Then
            Slab1OrgY = 1448 + SlabDspDimY / 2   'corrected by YGC 100810
        Else
            Slab1OrgY = 1448
        End If


        Dim lineH As New Line()
        lineH.StrokeThickness = 0.5
        lineH.Stroke = Brushes.Black
        Dim posH1 = ConvertToLegacyPositionOnPicSlab(New Point(Slab1OrgX, Slab1OrgY), cnvTopLeft, cnvBottomRight)
        Dim posH2 = ConvertToLegacyPositionOnPicSlab(New Point(2895, Slab1OrgY), cnvTopLeft, cnvBottomRight)
        lineH.X1 = posH1.X
        lineH.X2 = posH2.X
        lineH.Y1 = posH1.Y
        lineH.Y2 = posH2.Y
        picSlab.Children.Add(lineH)

        Dim lineV As New Line()
        lineV.StrokeThickness = 0.5
        lineV.Stroke = Brushes.Black
        Dim posV1 = ConvertToLegacyPositionOnPicSlab(New Point(Slab1OrgX, Slab1OrgY), cnvTopLeft, cnvBottomRight)
        Dim posV2 = ConvertToLegacyPositionOnPicSlab(New Point(Slab1OrgX, 0), cnvTopLeft, cnvBottomRight)
        lineV.X1 = posV1.X
        lineV.X2 = posV2.X
        lineV.Y1 = posV1.Y
        lineV.Y2 = posV2.Y
        picSlab.Children.Add(lineV)

        Canvas.SetLeft(lblX, 5 + (SlabsRight + 120) * sx)
        Canvas.SetTop(lblX, -10 + (Slab1OrgY - 300) * sx)

        Canvas.SetLeft(lblY, 5 + (Slab1OrgX + 120) * sy)
        Canvas.SetTop(lblY, -10 + (SlabsTop - 300) * sy)

        modAutoMesh.Interior = False



        '***************************************************************************************
        '********************************display gear*******************************************
        If IsMovingLoad Then
            IsDisplayMovingLoad = True
        End If

111:
        TireLoad = ((CDbl(PcntOnMainGears / 100.0#) * GrossWeight) / NMainGears) * (CDbl(1.0# / NWheels))
        EqWheelArea = TireLoad / TirePressure
        ContactLength = Math.Sqrt(EqWheelArea / 0.5227)
        If PCARectangle = True Then
            WhlWidth = 0.6 * ContactLength 'for PCA rectangular footprint
            WhlLength = 0.8712 * ContactLength 'for PCA rectangular footprint
        Else
            WhlWidth = Math.Sqrt(EqWheelArea) 'for square footprint
            WhlLength = Math.Sqrt(EqWheelArea) 'for square footprint
        End If


        Call TirePositions()

        'Modified by YGC 073010
        'If XDimension > 28.5 Or YDimension > 28.5 Then ScaleFactor = 3.33 / NSlabsX Else: ScaleFactor = 6.67 / NSlabsX 'added by DRB 4/30/01, Modified by YGC 060110
        ' Note the ScaleFactor here is not appropriate to the slab size except for XDimension=30
        If SlabsDimensionX > SlabsDimensionY Then
            ScaleFactor = 2000 / (SlabsDimensionX * 12)
        Else
            ScaleFactor = 2000 / (SlabsDimensionY * 12)
        End If
        'Modify end


        'added to account for load poisition check by YGC 100810
        'Dim PointXmax, PointXmin, PointYmax, PointYmin As double 'YGC 080612

        For I = 1 To NWheels

            If GearAngle > 0 And GearAngle < 90.0# Then
                Angle1 = Math.PI / 2.0# - (-GearAngle * Math.PI / 180 + Math.Atan(modAutoMesh.Xfp / modAutoMesh.Yfp))
                Angle2 = Math.PI / 2.0# - (GearAngle * Math.PI / 180 + Math.Atan(modAutoMesh.Xfp / modAutoMesh.Yfp))
                Point1X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - modAutoMesh.Hypot * Math.Cos(Angle1))
                Point1Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + modAutoMesh.Hypot * Math.Sin(Angle1))
                Point2X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + modAutoMesh.Hypot * Math.Cos(Angle2))
                Point2Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + modAutoMesh.Hypot * Math.Sin(Angle2))
                Point3X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + modAutoMesh.Hypot * Math.Cos(Angle1))
                Point3Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - modAutoMesh.Hypot * Math.Sin(Angle1))
                Point4X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - modAutoMesh.Hypot * Math.Cos(Angle2))
                Point4Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - modAutoMesh.Hypot * Math.Sin(Angle2))

            ElseIf GearAngle = 0 Then


                Point1X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - 0.5 * WhlWidth)
                Point1Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + 0.5 * WhlLength)
                Point2X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + 0.5 * WhlWidth)
                Point2Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + 0.5 * WhlLength)
                Point3X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + 0.5 * WhlWidth)
                Point3Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - 0.5 * WhlLength)
                Point4X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - 0.5 * WhlWidth)
                Point4Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - 0.5 * WhlLength)

            Else


                Point1X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - 0.5 * WhlLength)
                Point1Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + 0.5 * WhlWidth)
                Point2X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + 0.5 * WhlLength)
                Point2Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + 0.5 * WhlWidth)
                Point3X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + 0.5 * WhlLength)
                Point3Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - 0.5 * WhlWidth)
                Point4X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - 0.5 * WhlLength)
                Point4Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - 0.5 * WhlWidth)

            End If


            Dim line1 As New Line()
            Dim line2 As New Line()
            Dim line3 As New Line()
            Dim line4 As New Line()


            If IsDisplayMovingLoad Then
                line1.StrokeDashArray = New DoubleCollection() From {4}
                line2.StrokeDashArray = New DoubleCollection() From {4}
                line3.StrokeDashArray = New DoubleCollection() From {4}
                line4.StrokeDashArray = New DoubleCollection() From {4}
            End If


            line1.Stroke = Brushes.Black
            Dim pos11 = ConvertToLegacyPositionOnPicSlab(New Point(Point1X, Point1Y), cnvTopLeft, cnvBottomRight)
            Dim pos12 = ConvertToLegacyPositionOnPicSlab(New Point(Point2X, Point2Y), cnvTopLeft, cnvBottomRight)
            line1.X1 = pos11.X
            line1.X2 = pos12.X
            line1.Y1 = pos11.Y
            line1.Y2 = pos12.Y
            line1.StrokeThickness = 0.5
            picSlab.Children.Add(line1)


            line2.Stroke = Brushes.Black
            Dim pos21 = ConvertToLegacyPositionOnPicSlab(New Point(Point2X, Point2Y), cnvTopLeft, cnvBottomRight)
            Dim pos22 = ConvertToLegacyPositionOnPicSlab(New Point(Point3X, Point3Y), cnvTopLeft, cnvBottomRight)
            line2.X1 = pos21.X
            line2.X2 = pos22.X
            line2.Y1 = pos21.Y
            line2.Y2 = pos22.Y
            line2.StrokeThickness = 0.5
            picSlab.Children.Add(line2)


            line3.Stroke = Brushes.Black
            Dim pos31 = ConvertToLegacyPositionOnPicSlab(New Point(Point3X, Point3Y), cnvTopLeft, cnvBottomRight)
            Dim pos32 = ConvertToLegacyPositionOnPicSlab(New Point(Point4X, Point4Y), cnvTopLeft, cnvBottomRight)
            line3.X1 = pos31.X
            line3.X2 = pos32.X
            line3.Y1 = pos31.Y
            line3.Y2 = pos32.Y
            line3.StrokeThickness = 0.5
            picSlab.Children.Add(line3)


            line4.Stroke = Brushes.Black
            Dim pos41 = ConvertToLegacyPositionOnPicSlab(New Point(Point4X, Point4Y), cnvTopLeft, cnvBottomRight)
            Dim pos42 = ConvertToLegacyPositionOnPicSlab(New Point(Point1X, Point1Y), cnvTopLeft, cnvBottomRight)
            line4.X1 = pos41.X
            line4.X2 = pos42.X
            line4.Y1 = pos41.Y
            line4.Y2 = pos42.Y
            line4.StrokeThickness = 0.5
            picSlab.Children.Add(line4)

            If I = 1 Then
                PointXmax = Point1X
                PointXmin = Point1X
                PointYmax = Point1Y
                PointYmin = Point1Y
            End If

            PointXmax = Math.Max(PointXmax, Math.Max(Math.Max(Point1X, Point2X), Math.Max(Point3X, Point4X)))
            PointXmin = Math.Min(PointXmin, Math.Min(Math.Min(Point1X, Point2X), Math.Min(Point3X, Point4X)))
            PointYmax = Math.Max(PointYmax, Math.Max(Math.Max(Point1Y, Point2Y), Math.Max(Point3Y, Point4Y)))
            PointYmin = Math.Min(PointYmin, Math.Min(Math.Min(Point1Y, Point2Y), Math.Min(Point3Y, Point4Y)))

        Next I

        If IsMovingLoad And IsDisplayMovingLoad Then

            Dim lineMovingTrack As New Line()
            lineMovingTrack.StrokeDashArray = New DoubleCollection() From {4}
            lineMovingTrack.Stroke = Brushes.Black


            Dim PointCGX = Slab1OrgX + ScaleFactor * XCoord 'CSng(txtXCoord.Text)
            Dim PointCGY = Slab1OrgY - ScaleFactor * YCoord 'CSng(txtYCoord.Text)

            Dim PointCGXMoved = Slab1OrgX + ScaleFactor * XCoordMoved ' CSng(txtXCoordMoved.Text)
            Dim PointCGYMoved = Slab1OrgY - ScaleFactor * YCoordMoved ' CSng(txtYCoordMoved.Text)

            Dim posCG = ConvertToLegacyPositionOnPicSlab(New Point(PointCGX, PointCGY), cnvTopLeft, cnvBottomRight)
            Dim posCGMoved = ConvertToLegacyPositionOnPicSlab(New Point(PointCGXMoved, PointCGYMoved), cnvTopLeft, cnvBottomRight)

            lineMovingTrack.X1 = posCG.X
            lineMovingTrack.X2 = posCGMoved.X
            lineMovingTrack.Y1 = posCG.Y
            lineMovingTrack.Y2 = posCGMoved.Y
            lineMovingTrack.StrokeThickness = 0.5
            picSlab.Children.Add(lineMovingTrack)

            IsDisplayMovingLoad = False
            GoTo 111
        End If


    End Sub

    Sub InitGrdLayerData()
        With grdLayerProps
            colLayers = New DataColumn("Layers")
            colModulus = New DataColumn("EModulus")
            colPoissonRatio = New DataColumn("PoissonRatio")
            colThickness = New DataColumn("Thickness")

            grdLayerTable.Columns.Add(colLayers)
            grdLayerTable.Columns.Add(colModulus)
            grdLayerTable.Columns.Add(colPoissonRatio)
            grdLayerTable.Columns.Add(colThickness)

        End With

        Dim row1, row2, row3, row4 As DataRow
        row1 = grdLayerTable.NewRow
        row2 = grdLayerTable.NewRow
        row3 = grdLayerTable.NewRow
        row4 = grdLayerTable.NewRow
        grdLayerTable.Rows.Add(row1)
        grdLayerTable.Rows.Add(row2)
        grdLayerTable.Rows.Add(row3)
        grdLayerTable.Rows.Add(row4)

        grdLayerTable(0)(0) = "PCC Slab"
        grdLayerTable(1)(0) = "Subbase 1"
        grdLayerTable(2)(0) = "Subbase 2"
        grdLayerTable(3)(0) = "Subgrade"

        grdLayerProps.ItemsSource = grdLayerTable.AsDataView()

    End Sub

    Private Sub ListBoxAirplaneGroup_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles ListBoxAirplaneGroup.SelectionChanged
        Dim I, J As Short

        ' If data has changed, then changing to another group displays
        ' the "do you want to save the data" dialog box. Responding
        ' "Cancel" moves the selection back to the previous group in
        ' the list, calling this subroutine recursively. Hence the
        ' following code to skip the subroutine on recursion.

        If Me.IsInitializing = True Then
            Exit Sub
        End If


        If ChangeDataRet = MsgBoxResult.Cancel Then Exit Sub
        If ChangeDataRet = 0 Then
            Call CheckChangedData()
            If ChangeDataRet <> 0 Then
                ChangeDataRet = 0
                Exit Sub
            End If
        End If

        lstACGroupIndex = ListBoxAirplaneGroup.SelectedIndex
        ILibACGroup = lstACGroupIndex + 1

        For I = ListBoxLibraryAirplane.Items.Count - 1 To 0 Step -1
            ListBoxLibraryAirplane.Items.RemoveAt(I)
        Next I

        If ILibACGroup = NLibACGroups Then
            '    J = libNAC - NBelly
            J = libNAC
        Else
            J = LibACGroup(ILibACGroup + 1) - 1
        End If

        For I = LibACGroup(ILibACGroup) To J
            ListBoxLibraryAirplane.Items.Add(libACName(I))
        Next I

        If IACAddedorRemoved <> 0 Then
            I = IACAddedorRemoved - LibACGroup(ExternalLibraryIndex)
            IACAddedorRemoved = 0
        Else
            I = 0
        End If

        If I >= ListBoxLibraryAirplane.Items.Count Then I = ListBoxLibraryAirplane.Items.Count - 1
        lstLibFileIndex = I
        If I > -1 Then ListBoxLibraryAirplane.SelectedIndex = I
    End Sub
    Public Sub CheckChangedData()
        Dim I, CurrentlstLibFileIndex As Short
        Dim CurrentlstACGroupIndex As Short

        ChangeDataRet = 0

        If NWheels <> libNTires(LibIndex) Then GoTo DataChanged
        If GrossWeight <> libGL(LibIndex) Then GoTo DataChanged
        If NMainGears <> libNMainGears(LibIndex) Then GoTo DataChanged
        If PcntOnMainGears <> libPcntOnMainGears(LibIndex) Then GoTo DataChanged
        If TirePressure <> libCP(LibIndex) Then GoTo DataChanged
        For I = 1 To NWheels
            If XWheels(I) <> libTY(LibIndex, I) Then GoTo DataChanged
            If YWheels(I) <> libTX(LibIndex, I) Then GoTo DataChanged
        Next I

        Exit Sub

DataChanged:

        S = "Data for the current gear has changed." & NL2
        S = S & "Do you want to save the new data?"
        Ret = MsgBox(S, MsgBoxStyle.YesNoCancel, "Save Current Data?")
        If Ret = MsgBoxResult.No Or Ret = MsgBoxResult.Yes Then

            CurrentlstLibFileIndex = ListBoxLibraryAirplane.SelectedIndex
            CurrentlstACGroupIndex = ListBoxAirplaneGroup.SelectedIndex

            ChangeDataRet = MsgBoxResult.Yes
            If Ret = MsgBoxResult.Yes Then
                Call BtnSaveACinLibrary_Click(BtnSaveACinLibrary, Nothing)
            End If
            Call UpdateDataFromLibrary(LibIndex)

            If CurrentlstACGroupIndex = ListBoxAirplaneGroup.SelectedIndex Then

                Dim newListBox As New ListBox
                newListBox.Items.Add(ListBoxAirplaneGroup.SelectedItem)
                newListBox.SelectedItem = ListBoxAirplaneGroup.SelectedItem

                Call ListBoxAirplaneGroup_SelectionChanged(newListBox, Nothing)

            Else
                ListBoxAirplaneGroup.SelectedIndex = CurrentlstACGroupIndex
            End If

            If CurrentlstLibFileIndex = ListBoxLibraryAirplane.SelectedIndex Then

                Dim newListBox1 As New ListBox
                newListBox1.Items.Add(ListBoxLibraryAirplane.SelectedItem)
                newListBox1.SelectedItem = ListBoxLibraryAirplane.SelectedItem


                Call ListBoxLibraryAirplane_SelectionChanged(newListBox1, Nothing)

            Else
                If CurrentlstLibFileIndex >= ListBoxLibraryAirplane.Items.Count Then
                    CurrentlstLibFileIndex = ListBoxLibraryAirplane.Items.Count - 1
                End If
                ListBoxLibraryAirplane.SelectedIndex = CurrentlstLibFileIndex
            End If

        ElseIf Ret = MsgBoxResult.Cancel Then

            ChangeDataRet = MsgBoxResult.Cancel
            CurrentlstLibFileIndex = lstLibFileIndex
            CurrentlstACGroupIndex = lstACGroupIndex

            If CurrentlstACGroupIndex = ListBoxAirplaneGroup.SelectedIndex Then

                Dim newListBox As New ListBox
                newListBox.Items.Add(ListBoxAirplaneGroup.SelectedItem)
                newListBox.SelectedItem = ListBoxAirplaneGroup.SelectedItem

                Call ListBoxAirplaneGroup_SelectionChanged(newListBox, Nothing)

            Else
                ListBoxAirplaneGroup.SelectedIndex = CurrentlstACGroupIndex
            End If

            If CurrentlstLibFileIndex = ListBoxLibraryAirplane.SelectedIndex Then

                Dim newListBox1 As New ListBox
                newListBox1.Items.Add(ListBoxLibraryAirplane.SelectedItem)
                newListBox1.SelectedItem = ListBoxLibraryAirplane.SelectedItem

                Call ListBoxLibraryAirplane_SelectionChanged(newListBox1, Nothing)
            Else
                ListBoxLibraryAirplane.SelectedIndex = CurrentlstLibFileIndex
            End If
        End If

    End Sub

    Private Sub ListBoxLibraryAirplane_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles ListBoxLibraryAirplane.SelectionChanged

        ' If data has changed, then changing to another aircraft displays
        ' the "do you want to save the data" dialog box. Responding
        ' "Cancel" moves the selection back to the previous aircraft in
        ' the list, calling this subroutine recursively. Hence the
        ' following code to skip the subroutine on recursion.

        If Me.IsInitializing = True Or ListBoxLibraryAirplane.Items.Count = 0 Then
            Exit Sub
        End If

        If ListBoxLibraryAirplane.SelectedIndex = -1 Then
            Exit Sub
        End If


        If ChangeDataRet = MsgBoxResult.Cancel Then Exit Sub
        If ChangeDataRet = 0 Then
            Call CheckChangedData()
            If ChangeDataRet <> 0 Then
                ChangeDataRet = 0
                Exit Sub
            End If
        End If


        lstLibFileIndex = ListBoxLibraryAirplane.SelectedIndex
        LibIndex = LibACGroup(lstACGroupIndex + 1) + ListBoxLibraryAirplane.SelectedIndex ' Library index
        '  If libGear$(N) = "H" Then I = I + 1 ' Belly gear = 2 aircraft.

        If libGear(LibIndex) = "S" Then
            Call InitACLib()
        End If


        Units = "English Units"
        JobTitle = libACName(LibIndex)
        GrossWeight = libGL(LibIndex)
        NWheels = libNTires(LibIndex)

        For icount As Integer = 1 To NWheels
            XWheels(icount) = libTY(LibIndex, icount)
            YWheels(icount) = libTX(LibIndex, icount)
        Next icount

        TirePressure = libCP(LibIndex)
        PcntOnMainGears = libPcntOnMainGears(LibIndex)
        NMainGears = libNMainGears(LibIndex)
        InputAlpha = libAlpha(LibIndex)
        Coverages = libCoverages(LibIndex)
        XGridOrigin = libXGridOrigin(LibIndex)
        XGridMax = libXGridMax(LibIndex)
        XGridNPoints = libXGridNPoints(LibIndex)
        YGridOrigin = libYGridOrigin(LibIndex)
        YGridMax = libYGridMax(LibIndex)
        YGridNPoints = libYGridNPoints(LibIndex)

        Call ResetOutputs()
        Call PlotGear() ' Also calls GearCG and updates the parameter output grid.

        Call TirePositions()

        Call WriteParmGrid()

        '33333
        Dim checkedRadioButton As New RadioButton
        checkedRadioButton.IsChecked = True

        If optInterior.IsChecked Then
            Call optInterior_Checked(checkedRadioButton, Nothing)
        End If

        'If Not optInterior.IsChecked Then
        If optEdge.IsChecked Then
            Call optEdge_Checked(checkedRadioButton, Nothing)
        End If

        gXcoord1 = XCoord
        gYcoord1 = YCoord

        gXcoord2 = XCoordMoved
        gYcoord2 = YCoordMoved
        NGearPositions = NMovingStep


    End Sub
    Public Sub TirePositions()

        If GearAngle = 0.0# Or GearAngle = 90.0# Then GoTo 100
        'GearAngle describes the angle the main gear axis makes with the joint.
        'GearAngle=0 means the gear is paralel to the joint.
        'GearAngle=90 means the gear is perpendicular.

        Xfp = WhlWidth
        Yfp = WhlLength
        XtrMin = 0.0#
        XtrMax = 0.0#
        YtrMax = 0.0#
        YtrMin = 0.0#
        For I = 1 To NWheels
            Xtr(I) = YWheels(I) - Ycg
            Ytr(I) = XWheels(I) - Xcg
            'now transform to polar coordinates
            RTr(I) = Math.Sqrt(Xtr(I) * Xtr(I) + Ytr(I) * Ytr(I))
            If Math.Abs(Xtr(I)) < 0.01 Then
                ThTr(I) = Math.PI / 2
            Else
                ThTr(I) = Math.Atan(Ytr(I) / Xtr(I))
            End If

            If Xtr(I) < 0 And Ytr(I) >= 0 Then ThTr(I) = Math.PI - Math.Abs(ThTr(I))
            If Xtr(I) < 0 And Ytr(I) < 0 Then ThTr(I) = Math.PI + Math.Abs(ThTr(I))
            If Xtr(I) >= 0 And Ytr(I) < 0 Then ThTr(I) = 2 * Math.PI - Math.Abs(ThTr(I))
            'add the rotation in radians
            ThTr(I) = ThTr(I) - GearAngle * Math.PI / 180.0#
            'transform back to cartesian coordinates
            Xtr(I) = RTr(I) * Math.Cos(ThTr(I))
            Ytr(I) = RTr(I) * Math.Sin(ThTr(I))
            If Xtr(I) > XtrMax Then XtrMax = Xtr(I)
            If Xtr(I) < XtrMin Then XtrMin = Xtr(I)
            If Ytr(I) > YtrMax Then YtrMax = Ytr(I)
            If Ytr(I) < YtrMin Then YtrMin = Ytr(I)
        Next I
        xdim = XtrMax - XtrMin

        'compute distance from pavement edge to CG of wheel
        For I = 1 To NWheels
            Hypot = Math.Sqrt((0.5 * Xfp) ^ 2 + (0.5 * Yfp) ^ 2)
            Angle1 = Math.Atan(Xfp / Yfp) + GearAngle * Math.PI / 180.0# - Math.PI / 2.0#
            CosOfAngle = Math.Cos(Angle1)
            '     Xtr(I) = Xtr(I) - XtrMin + Hypot * CosOfAngle
        Next I
        GoTo 999

100:    If GearParallel = True Then
            'if gear oriented parallel to joint
            Xfp = WhlWidth
            Yfp = WhlLength
            XtrMin = 0.0#
            XtrMax = 0.0#
            YtrMax = 0.0#
            YtrMin = 0.0#
            For I = 1 To NWheels
                Xtr(I) = YWheels(I) - Ycg
                Ytr(I) = XWheels(I) - Xcg
                If Xtr(I) > XtrMax Then XtrMax = Xtr(I)
                If Xtr(I) < XtrMin Then XtrMin = Xtr(I)
                If Ytr(I) > YtrMax Then YtrMax = Ytr(I)
                If Ytr(I) < YtrMin Then YtrMin = Ytr(I)
            Next I
            xdim = XtrMax - XtrMin


            ydim = YtrMax - YtrMin

            If modWorld.NYSymmetric = NWheels Then SymmSngl = True

        Else
            'If gear oriented perpendicular to joint

            Yfp = WhlWidth
            Xfp = WhlLength
            XtrMin = 0.0#
            XtrMax = 0.0#
            YtrMax = 0.0#
            YtrMin = 0.0#
            For I = 1 To NWheels
                Xtr(I) = XWheels(I) - Xcg
                Ytr(I) = YWheels(I) - Ycg

                Ytr(I) = -Ytr(I)    'corrected to consistent with angle 89.9 YC 072516 070516

                If Xtr(I) > XtrMax Then XtrMax = Xtr(I)
                If Xtr(I) < XtrMin Then XtrMin = Xtr(I)
                If Ytr(I) > YtrMax Then YtrMax = Ytr(I)
                If Ytr(I) < YtrMin Then YtrMin = Ytr(I)
            Next I
            xdim = XtrMax - XtrMin


            ydim = YtrMax - YtrMin

            'for edge case only
            'If Interior = False Then
            If modWorld.NXSymmetric = NWheels Then SymmSngl = True

        End If

        'reposition the gear at the given location along the longitudinal (y) edge
999:

        'For I = 1 To NWheels
        '    Ytr(I) = Ytr(I) - YCoord
        'Next I
        'For I = 1 To NWheels
        '    Xtr(I) = Xtr(I) + XCoord
        'Next I

        For I = 1 To NWheels
            If IsDisplayMovingLoad Then
                Ytr(I) = Ytr(I) - YCoordMoved
                Xtr(I) = Xtr(I) + XCoordMoved
            Else
                Ytr(I) = Ytr(I) - YCoord
                Xtr(I) = Xtr(I) + XCoord
            End If
        Next I


    End Sub

    Public Sub PlotGear()
        Dim I As Short
        Dim MaxWidthBy2, picAspect As Double
        Dim xbr, xtl, ytl, ybr As Double
        Dim YMin, XMin, XMax, YMax As Double
        Dim MaxHeightBy2, Rad As Double
        Dim Delta, Add_Renamed As Double
        Dim Add As Double

        Dim EqWheelArea, TireLoad, ContactLength As Double

        ' Gear axes are X +ve forward, Y +ve to the right.
        ' Gear axes on screen are X +ve upward, Y +ve to the right.
        ' Screen axes are X +ve to the right, Y +ve upward.
        ' Therefore, screen coordinates are written (y, x) in gear axes.
        ' Remember when drawing on screen and returning screen coordinates from mouse.

        picGear.Children.Clear()
        Call GearCG(XWheels, YWheels, NWheels, Xcg, Ycg)

        XMin = 1.0E+20 : XMax = -1.0E+20
        YMin = 1.0E+20 : YMax = -1.0E+20
        For I = 1 To NWheels
            If XWheels(I) < XMin Then XMin = XWheels(I)
            If XWheels(I) > XMax Then XMax = XWheels(I)
            If YWheels(I) < YMin Then YMin = YWheels(I)
            If YWheels(I) > YMax Then YMax = YWheels(I)
        Next I
        XMin = XMin - WheelRadius : XMax = XMax + WheelRadius
        YMin = YMin - WheelRadius : YMax = YMax + WheelRadius

        MaxHeightBy2 = 1.5 * (XMax - XMin) / 2 ' For maximum gear height in inches.
        MaxWidthBy2 = 1.2 * (YMax - YMin) / 2 ' For maximum gear width in inches.
        XCenter = (XMax + XMin) / 2
        YCenter = (YMax + YMin) / 2

        picAspect = picGear.ActualHeight / picGear.ActualWidth

        If MaxHeightBy2 / MaxWidthBy2 < picAspect Then
            xtl = XCenter + MaxWidthBy2 * picAspect : ytl = YCenter - MaxWidthBy2
            xbr = XCenter - MaxWidthBy2 * picAspect : ybr = YCenter + MaxWidthBy2
        Else
            xtl = XCenter + MaxHeightBy2 : ytl = YCenter - MaxHeightBy2 / picAspect
            xbr = XCenter - MaxHeightBy2 : ybr = YCenter + MaxHeightBy2 / picAspect
        End If

        'Don't do anydrawing unless the canvas attributes are accessible.
        If picGear.ActualHeight <= 0 Then Exit Sub

        'Prep for legacy scaling support
        topLeft = New Point(xtl, ytl)
        bottomRight = New Point(xbr, ybr)

        ' Global units are English units.

        GraphScaleX = picGear.ActualHeight / Math.Abs(xbr - xtl)
        GraphScaleY = picGear.ActualWidth / Math.Abs(ybr - ytl)

        'Dim gr As Graphics = CreateAutoRedrawGraphicsPictureBox(picGear)
        'Dim drawPen As New Pen(Color.Black, 1)
        'Dim drawPen2 As New Pen(picGear.BackColor)
        'Dim drawBrush As New SolidBrush(Color.Black)
        'Dim drawBrush2 As New SolidBrush(picGear.BackColor)
        'Dim drawFormat As New StringFormat()
        'Dim m As New Drawing2D.Matrix()
        'm.Translate(CSng((picGear.Width) / 2 - 3 - YCenter * GraphScaleY), CSng((picGear.Height) / 2 - 3 + XCenter * GraphScaleX))
        'm.Scale(-GraphScaleY, GraphScaleX)
        'm.Rotate(180)
        'gr.Transform = m
        'drawFormat.FormatFlags = StringFormatFlags.DirectionRightToLeft
        'gr.Clear(picGear.BackColor)

        'Dim blackPen As New Pen(Color.Black, 0.01)
        'Dim backPen As New Pen(picGear.BackColor, 0.01)

        '' Draw the rectangular load patch corresponding to each wheel.
        '' Shape of load patch is PCA rectangular equivalent area
        ''  cf Huang, Pavement Analysis and Design, 1993, p. 30

        TireLoad = ((CDbl(PcntOnMainGears / 100.0#) * GrossWeight) / NMainGears) * (CDbl(1.0# / NWheels))
        EqWheelArea = TireLoad / TirePressure
        ContactLength = System.Math.Sqrt(EqWheelArea / 0.5227)
        If PCARectangle = True Then
            WhlWidth = 0.6 * ContactLength 'square footprint
            WhlLength = 0.8712 * ContactLength 'square footprint
        Else
            WhlWidth = System.Math.Sqrt(EqWheelArea) 'square footprint
            WhlLength = System.Math.Sqrt(EqWheelArea) 'square footprint
        End If

        'Convert the WheelRadius to a "Canvas Size" number
        Dim drawnWheelRadius = ConvertFromLegacyPosition(New Point(topLeft.X + WheelRadius, topLeft.Y + WheelRadius), topLeft, bottomRight).X

        'Convert the WhlLength & WhlWidth to a "Canvas Size" number
        Dim drawnWhlLength = ConvertFromLegacyPosition(New Point(topLeft.X + WhlLength, topLeft.Y + WhlLength), topLeft, bottomRight).X
        Dim drawnWhlWidth = ConvertFromLegacyPosition(New Point(topLeft.X + WhlWidth, topLeft.Y + WhlWidth), topLeft, bottomRight).X

        For I = 1 To NWheels
            Dim wheel As New Rectangle()
            Dim mySolidColorBrush As New SolidColorBrush()
            mySolidColorBrush.Color = Colors.Black
            wheel.Fill = mySolidColorBrush
            wheel.Width = drawnWhlWidth ' drawnWheelRadius * 2 ' CSng(WhlLength)
            wheel.Height = drawnWhlLength ' drawnWheelRadius * 2 ' CSng(WhlWidth)
            Dim pos = ConvertFromLegacyPosition(New Point(XWheels(I), YWheels(I)), topLeft, bottomRight)
            Canvas.SetTop(wheel, pos.X - drawnWhlLength * 0.5) 'Canvas.SetTop(wheel, pos.X - drawnWheelRadius)
            Canvas.SetLeft(wheel, pos.Y - drawnWhlWidth * 0.5) 'Canvas.SetLeft(wheel, pos.Y - drawnWheelRadius)
            picGear.Children.Add(wheel)
            AddHandler wheel.MouseLeftButtonDown, AddressOf Rectangule_MouseLeftButtonDown
        Next I

        'Print the Grid in the canvas
        If XGridNPoints > 1 Then
            'Delta = (XGridMax - XGridOrigin) / (XGridNPoints - 1)
            Delta = (XGridMax - XGridOrigin) / Math.Round(XGridNPoints)
            'For I = 0 To XGridNPoints - 1
            For I = 0 To XGridNPoints
                Add = I * Delta
                'picGear.Line (YGridOrigin, XGridOrigin + Add)-(YGridMax, XGridOrigin + Add)
                Dim line1 As New Line()
                line1.Stroke = Brushes.Black
                Dim pos1 = ConvertFromLegacyPosition(New Point(XGridOrigin + Add, YGridOrigin), topLeft, bottomRight)
                Dim pos2 = ConvertFromLegacyPosition(New Point(XGridOrigin + Add, YGridMax), topLeft, bottomRight)
                line1.X1 = pos1.Y
                line1.X2 = pos2.Y
                line1.Y1 = pos1.X
                line1.Y2 = pos2.X
                line1.StrokeThickness = 1
                picGear.Children.Add(line1)
            Next I
        End If
        'Continues printing
        If YGridNPoints > 1 Then
            'Delta = (YGridMax - YGridOrigin) / (YGridNPoints - 1)
            Delta = (YGridMax - YGridOrigin) / Math.Round(YGridNPoints)
            'For I = 0 To YGridNPoints - 1
            For I = 0 To YGridNPoints
                Add = I * Delta
                'picGear.Line (YGridOrigin + Add, XGridOrigin)-(YGridOrigin + Add, XGridMax)
                Dim line2 As New Line()
                line2.Stroke = Brushes.Black
                Dim pos1 = ConvertFromLegacyPosition(New Point(XGridOrigin, YGridOrigin + Add), topLeft, bottomRight)
                Dim pos2 = ConvertFromLegacyPosition(New Point(XGridMax, YGridOrigin + Add), topLeft, bottomRight)
                line2.X1 = pos1.Y
                line2.X2 = pos2.Y
                line2.Y1 = pos1.X
                line2.Y2 = pos2.X
                line2.StrokeThickness = 1
                picGear.Children.Add(line2)
            Next I
        End If

        ' Draw the cg symbol with filled / unfilled successive quadrants.

        Rad = drawnWheelRadius / 3

        'Get the "new" CG location
        Dim posCG = ConvertFromLegacyPosition(New Point(Xcg, Ycg), topLeft, bottomRight)
        Dim stroke = If(NWheels = 1, picGear.Background, Brushes.Black)

        'Draw the "quadrants" for the CG image
        Dim quad1 As New Ellipse()
        quad1.Fill = picGear.Background
        quad1.Stroke = stroke
        quad1.Clip = New RectangleGeometry(New Rect(0, 0, Rad, Rad))
        quad1.Width = Rad * 2
        quad1.Height = Rad * 2
        Canvas.SetTop(quad1, posCG.X - Rad)
        Canvas.SetLeft(quad1, posCG.Y - Rad)
        picGear.Children.Add(quad1)

        Dim quad2 As New Ellipse()
        quad2.Fill = Brushes.Black
        quad2.Stroke = stroke
        quad2.Clip = New RectangleGeometry(New Rect(Rad, 0, Rad, Rad))
        quad2.Width = Rad * 2
        quad2.Height = Rad * 2
        Canvas.SetTop(quad2, posCG.X - Rad)
        Canvas.SetLeft(quad2, posCG.Y - Rad)
        picGear.Children.Add(quad2)

        Dim quad3 As New Ellipse()
        quad3.Fill = picGear.Background
        quad3.Stroke = stroke
        quad3.Clip = New RectangleGeometry(New Rect(Rad, Rad, Rad, Rad))
        quad3.Width = Rad * 2
        quad3.Height = Rad * 2
        Canvas.SetTop(quad3, posCG.X - Rad)
        Canvas.SetLeft(quad3, posCG.Y - Rad)
        picGear.Children.Add(quad3)

        Dim quad4 As New Ellipse()
        quad4.Fill = Brushes.Black
        quad4.Stroke = stroke
        quad4.Clip = New RectangleGeometry(New Rect(0, Rad, Rad, Rad))
        quad4.Width = Rad * 2
        quad4.Height = Rad * 2
        Canvas.SetTop(quad4, posCG.X - Rad)
        Canvas.SetLeft(quad4, posCG.Y - Rad)
        picGear.Children.Add(quad4)

        picGear.UpdateLayout()

        'If BtnSelectWheel.IsChecked = True Then

        '    If _closestCoordenates.X = 0 AndAlso _closestCoordenates.Y = 0 OrElse _closestCoordinatesSelectedByUser = False Then
        '        Dim ISelectedWheel As Integer

        '        Dim XTest = XWheels.Clone()
        '        Dim YTest = YWheels.Clone()

        '        FindStartWheel(XTest, YTest, ISelectedWheel)
        '        Dim pos = ConvertFromLegacyPosition(New Point(XWheels(ISelectedWheel), YWheels(ISelectedWheel)), topLeft, bottomRight)
        '        _closestCoordenates = New Point(pos.Y - drawnWheelRadius, pos.X - drawnWheelRadius)
        '        'set the last selected wheel
        '        _lastSelectedWheel = ISelectedWheel
        '    End If

        '    If _isAlreadySelected = False Then
        '        PrintLineInPicGear(_closestCoordenates.X + (drawnWheelRadius * 30 / 100), _closestCoordenates.Y + drawnWheelRadius, _closestCoordenates.X + drawnWheelRadius + (drawnWheelRadius * 70 / 100), _closestCoordenates.Y + drawnWheelRadius, Brushes.White, 2)
        '        PrintLineInPicGear(_closestCoordenates.X + drawnWheelRadius, _closestCoordenates.Y + (drawnWheelRadius * 30 / 100), _closestCoordenates.X + drawnWheelRadius, _closestCoordenates.Y + drawnWheelRadius + (drawnWheelRadius * 70 / 100), Brushes.White, 2)

        '        LabelXSelCoord.Content = String.Format("X Sel. = {0:F2} in", _closestCoordinatesLegacy.Y)
        '        LabelYSelCoord.Content = String.Format("Y Sel. = {0:F2} in", _closestCoordinatesLegacy.X)
        '        _isAlreadySelected = True
        '    Else
        '        Dim pointSelectedWheelLegacy = New Point(XWheels(_lastSelectedWheel), YWheels(_lastSelectedWheel))
        '        Dim pointSelectedWheelCanvas = ConvertFromLegacyPosition(pointSelectedWheelLegacy, topLeft, bottomRight)
        '        PrintLineInPicGear(pointSelectedWheelCanvas.Y + (drawnWheelRadius * 30 / 100) - drawnWheelRadius, pointSelectedWheelCanvas.X, pointSelectedWheelCanvas.Y + (drawnWheelRadius * 70 / 100), pointSelectedWheelCanvas.X, Brushes.White, 2)
        '        PrintLineInPicGear(pointSelectedWheelCanvas.Y, pointSelectedWheelCanvas.X + (drawnWheelRadius * 30 / 100) - drawnWheelRadius, pointSelectedWheelCanvas.Y, pointSelectedWheelCanvas.X + drawnWheelRadius + (drawnWheelRadius * 70 / 100) - drawnWheelRadius, Brushes.White, 2)

        '        LabelXSelCoord.Content = String.Format("X Sel. = {0:F2} in", pointSelectedWheelLegacy.Y)
        '        LabelYSelCoord.Content = String.Format("Y Sel. = {0:F2} in", pointSelectedWheelLegacy.X)
        '    End If
        'End If

        Call WriteParmGrid()
    End Sub

    'Private Sub PrintLineInPicGear(ByVal X1 As Double, ByVal Y1 As Double, ByVal X2 As Double, ByVal Y2 As Double, ByVal colorBrush As SolidColorBrush, ByVal strokeThickness As Integer)
    '    Dim line1 As New Line()
    '    line1.Stroke = colorBrush
    '    line1.X1 = X1
    '    line1.X2 = X2
    '    line1.Y1 = Y1
    '    line1.Y2 = Y2
    '    line1.StrokeThickness = strokeThickness
    '    picGear.Children.Add(line1)
    'End Sub

    'Private Sub UpdateAirplaneParameters()
    '    GridParams.Model.GrossWeight = Format(GrossWeight, "#,###,##0")
    '    GridParams.Model.GrossWeightOnGears = Format(PcntOnMainGears, "0.00")
    '    GridParams.Model.NumGears = Format(NMainGears, "0")
    '    GridParams.Model.WheelsOnGear = Format(NWheels, "0")
    '    GridParams.Model.TirePressure = Format(TirePressure, "0.0")
    'End Sub

    Private Sub picGear_MouseMove(sender As Object, e As MouseEventArgs) Handles picGear.MouseMove
        Dim canvasPosition = e.GetPosition(picGear)
        Dim pointerPosition = ConvertToLegacyPosition(New Point(canvasPosition.Y, canvasPosition.X), topLeft, bottomRight)
        LabelXCoord.Content = "X = " & String.Format("{0:F2}", pointerPosition.X) & " in"
        LabelYCoord.Content = "Y = " & String.Format("{0:F2}", pointerPosition.Y) & " in"
    End Sub
    Public Function ConvertToLegacyPosition(pos As Point, cnvTopLeft As Point, cnvBottomRight As Point) As Point
        Dim legacyWidth = Math.Abs(cnvTopLeft.Y - cnvBottomRight.Y)
        Dim legacyHeight = Math.Abs(cnvTopLeft.X - cnvBottomRight.X)

        Dim aspectRatioX = Decimal.Divide(pos.X, picGear.ActualWidth)
        Dim aspectRatioY = Decimal.Divide(pos.Y, picGear.ActualHeight)

        Dim offsetX = Decimal.Multiply(legacyWidth, aspectRatioX)
        Dim offsetY = Decimal.Multiply(legacyHeight, aspectRatioY)

        Dim x = Math.Round(If(cnvTopLeft.X > cnvBottomRight.X, cnvTopLeft.X - offsetX, cnvTopLeft.X + offsetX), 2)
        Dim y = Math.Round(If(cnvTopLeft.Y > cnvBottomRight.Y, cnvTopLeft.Y - offsetY, cnvTopLeft.Y + offsetY), 2)

        Return New Point(x, y)
    End Function

    Public Function ConvertToLegacyPositionOnPicSlab(pos As Point, cnvTopLeft As Point, cnvBottomRight As Point) As Point
        Dim legacyWidth = Math.Abs(cnvTopLeft.Y - cnvBottomRight.Y)
        Dim legacyHeight = Math.Abs(cnvTopLeft.X - cnvBottomRight.X)

        Dim aspectRatioX = Decimal.Divide(pos.X, picSlab.Width)
        Dim aspectRatioY = Decimal.Divide(pos.Y, picSlab.Height)

        Dim offsetX = Decimal.Multiply(legacyWidth, aspectRatioX)
        Dim offsetY = Decimal.Multiply(legacyHeight, aspectRatioY)

        Dim x = Math.Round(If(cnvTopLeft.X > cnvBottomRight.X, cnvTopLeft.X - offsetX, cnvTopLeft.X + offsetX), 2)
        Dim y = Math.Round(If(cnvTopLeft.Y > cnvBottomRight.Y, cnvTopLeft.Y - offsetY, cnvTopLeft.Y + offsetY), 2)

        Return New Point(x, y)
    End Function

    Private Sub btnAboutFEAFAA_Click(sender As Object, e As RoutedEventArgs) Handles btnAboutFEAFAA.Click
        Dim aboutWin As AboutBox = New AboutBox()
        aboutWin.Init()
        aboutWin.Show()
    End Sub

    Private Sub MainWindow_SizeChanged(sender As Object, e As SizeChangedEventArgs) Handles Me.SizeChanged
        If Me.IsInitializing = True Then
            Exit Sub
        End If

        Call PlotGear()
    End Sub
    ''' <summary>
    ''' Due to the significant plotting differences between a VB6 Picturebox
    ''' and a WPF Canvas, this conversion function is nessecary so that we do not have to
    ''' re-do all the legacy mathematics
    ''' </summary>
    Public Function ConvertFromLegacyPosition(pos As Point, cnvTopLeft As Point, cnvBottomRight As Point) As Point
        Dim legacyWidth = Math.Abs(cnvTopLeft.Y - cnvBottomRight.Y)
        Dim legacyHeight = Math.Abs(cnvTopLeft.X - cnvBottomRight.X)

        Dim legacyYDiff = Math.Abs(cnvTopLeft.Y - pos.Y)
        Dim legacyXDiff = Math.Abs(cnvTopLeft.X - pos.X)

        Dim newY = Math.Round(Decimal.Multiply(Decimal.Divide(legacyYDiff, legacyHeight), picGear.ActualHeight), 2)
        Dim newX = Math.Round(Decimal.Multiply(Decimal.Divide(legacyXDiff, legacyWidth), picGear.ActualWidth), 2)

        Return New Point(newX, newY)
    End Function

    Private Sub OptPCARectangle_Checked(sender As Object, e As RoutedEventArgs) Handles OptPCARectangle.Checked
        PCARectangle = True
        If Me.IsInitializing = True Then

        Else
            Call PlotGear()
        End If
    End Sub

    Private Sub OptSquare_Checked(sender As Object, e As RoutedEventArgs) Handles OptSquare.Checked
        PCARectangle = False

        If Me.IsInitializing = True Then

        Else
            Call PlotGear()
        End If
    End Sub

    Private Sub btnExit_Click(sender As Object, e As RoutedEventArgs) Handles btnExitT1.Click, btnExitT2.Click, btnExitT3.Click, btnExitT4.Click
        exitLoop = True
        Me.Close()
    End Sub

    Private Sub BtnAddACtoLibrary_Click(sender As Object, e As RoutedEventArgs) Handles BtnAddACtoLibrary.Click
        Dim DupName As Boolean
        Dim InsertName, S As String

        S = "Enter a name for the aircraft." & NL2
        S = S & "This name will be used as the title" & vbCrLf
        S = S & "saved in the external library."

        InsertName = InputBox(S, "Adding an Aircraft")
        If InsertName = "" Then Exit Sub

        Call InsertNewAircraft(InsertName, DupName)
        Call UpdateDataFromLibrary(LibIndex)

        If DupName Then Exit Sub

        Call WriteExternalFile()

        lstACGroupIndex = ListBoxAirplaneGroup.Items.Count - 1
        If lstACGroupIndex = ListBoxAirplaneGroup.SelectedIndex Then
            Call ListBoxAirplaneGroup_SelectionChanged(ListBoxAirplaneGroup, Nothing)
        Else
            ListBoxAirplaneGroup.SelectedIndex = lstACGroupIndex
        End If
    End Sub

    Private Sub BtnSaveACinLibrary_Checked(sender As Object, e As RoutedEventArgs)

    End Sub

    Private Sub BtnRemoveACfromLibrary_Click(sender As Object, e As RoutedEventArgs) Handles BtnRemoveACfromLibrary.Click
        Dim II, I, J As Short
        Dim S As String

        If ListBoxAirplaneGroup.SelectedIndex + 1 <> ExternalLibraryIndex Then
            S = JobTitle & " was selected" & vbCrLf
            S = S & "from the internal library." & NL2
            S = S & "Aircraft cannot be removed" & vbCrLf
            S = S & "from the internal library."
            Ret = MsgBox(S, 0, "Removing an Aircraft")
            Exit Sub
        End If

        S = "All data for " & JobTitle & vbCrLf
        S = S & "will be removed from the external library." & NL2
        S = S & "Once removed, the data cannot be restored." & NL2
        S = S & "Do you want to remove the data?"
        Ret = MsgBox(S, MsgBoxStyle.YesNo, "Removing an Aircraft")
        If Ret = MsgBoxResult.No Then Exit Sub


        ' Move all data down one place to current aircraft.
        For I = LibIndex To libNAC - 1

            II = I + 1
            libACName(I) = libACName(II)
            libGL(I) = libGL(II)
            libNMainGears(I) = libNMainGears(II)
            libPcntOnMainGears(I) = libPcntOnMainGears(II)
            libNTires(I) = libNTires(II)

            For J = 1 To libNTires(I)
                libTY(I, J) = libTY(II, J)
                libTX(I, J) = libTX(II, J)
            Next J

            libCP(I) = libCP(II)
            libXGridOrigin(I) = libXGridOrigin(II)
            libXGridMax(I) = libXGridMax(II)
            libXGridNPoints(I) = libXGridNPoints(II)
            libYGridOrigin(I) = libYGridOrigin(II)
            libYGridMax(I) = libYGridMax(II)
            libYGridNPoints(I) = libYGridNPoints(II)

        Next I

        IACAddedorRemoved = LibIndex

        libNAC = libNAC - 1
        Call UpdateDataFromLibrary(LibIndex)
        Call WriteExternalFile()

        Call ListBoxAirplaneGroup_SelectionChanged(ListBoxAirplaneGroup, Nothing)
    End Sub

    Private Sub BtnAddWheel_Click(sender As Object, e As RoutedEventArgs) Handles BtnAddWheel.Click
        If BtnMoveWheel.IsChecked Then
            BtnMoveWheel.IsChecked = False
        End If
        If BtnRemoveWheel.IsChecked Then
            BtnRemoveWheel.IsChecked = False
        End If
        If BtnSelectWheel.IsChecked Then
            BtnSelectWheel.Content = "Se_lect"
            BtnSelectWheel.IsChecked = False
        End If
        Operation = AddWheel
    End Sub

    Private Sub BtnRemoveWheel_Click(sender As Object, e As RoutedEventArgs) Handles BtnRemoveWheel.Click
        If BtnMoveWheel.IsChecked Then
            BtnMoveWheel.IsChecked = False
        End If
        If BtnSelectWheel.IsChecked Then
            BtnSelectWheel.Content = "Se_lect"
            BtnSelectWheel.IsChecked = False
        End If
        If BtnAddWheel.IsChecked Then
            BtnAddWheel.IsChecked = False
        End If
        If NWheels = 1 Then
            S = "The gear has only one wheel." & vbCrLf
            S = S & "The wheel cannot be removed."
            Ret = MsgBox(S, 0, "Removing a Wheel")
        Else
            Operation = RemoveWheel
        End If
    End Sub

    Private Sub BtnMoveWheel_Click(sender As Object, e As RoutedEventArgs) Handles BtnMoveWheel.Click
        If BtnSelectWheel.IsChecked Then
            BtnSelectWheel.Content = "Se_lect"
            BtnSelectWheel.IsChecked = False
        End If
        If BtnRemoveWheel.IsChecked Then
            BtnRemoveWheel.IsChecked = False
        End If
        If BtnAddWheel.IsChecked Then
            BtnAddWheel.IsChecked = False
        End If
        Operation = MoveWheel
    End Sub

    Private Sub BtnSelectWheel_Click(sender As Object, e As RoutedEventArgs) Handles BtnSelectWheel.Click
        If BtnMoveWheel.IsChecked Then
            BtnMoveWheel.IsChecked = False
        End If
        If BtnRemoveWheel.IsChecked Then
            BtnRemoveWheel.IsChecked = False
        End If
        If BtnAddWheel.IsChecked Then
            BtnAddWheel.IsChecked = False
        End If
        If BtnSelectWheel.Content <> "Dese_lect" Then
            LastOperation = Operation
            Operation = SelectAWheel
            BtnSelectWheel.Content = "Dese_lect"
        Else
            LastOperation = NoOperation
            Operation = NoOperation
            BtnSelectWheel.Content = "Se_lect"
            LabelXSelCoord.Content = ""
            LabelYSelCoord.Content = ""
            StartWheelIndex = 0
            Call PlotGear()
            BtnSelectWheel.IsChecked = False
        End If
        If BtnSelectWheel.IsChecked = True Then
            _isAlreadySelected = False
        Else
            LabelXSelCoord.Content = ""
            LabelYSelCoord.Content = ""
            _isAlreadySelected = True
            PlotGear()
        End If
    End Sub

    Private Sub TabStrip1_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles TabStrip1.SelectionChanged
        If m_IsInitializing_Tab Then Exit Sub 'VK: To simulate click event

        If TabStrip1.SelectedIndex = mintCurFrame Then Exit Sub ' No need to change frame.
        ' Otherwise, hide old frame, show new.
        ' Set mintCurFrame to new value.
        mintCurFrame = TabStrip1.SelectedIndex


        If TabStrip1.SelectedIndex = 0 Then 'Tab 1 Vehicle Selection
            Call VarAsg_fromControls()
            If chkCrackedBase.IsChecked Then modAutoMesh.CrackedBase = True
            If Not chkCrackedBase.IsChecked Then modAutoMesh.CrackedBase = False

            TabPage1_Enter() 'VK: added to simulate tabPage enter event

        ElseIf TabStrip1.SelectedIndex = 1 Then 'Tab 2 Pavement Structure
            sldIFParam.Value = IFParam
            Call DrawGear()
            If Winkler = True Then
                optWinkler.IsChecked = True
            Else
                optSubgrade.IsChecked = True
            End If

            Call VarInitControls()
            'txtYCoord.Text = CStr(YCoord)
            'txtXCoord.Text = Format(XCoord, "##0.00000") ' CStr(XCoord) VK: Modified to be formatted

            TabPage2_Enter() 'VK: added to simulate tabPage enter event
        ElseIf TabStrip1.SelectedIndex = 2 Or TabStrip1.SelectedIndex = 3 Then

            If cmbNSlabs.Text = "1" Then   ' no joint for 1 slab

                chkDowelX.IsEnabled = False
                chkDowelY.IsEnabled = False

                chkDowelX.IsChecked = False
                chkDowelY.IsChecked = False

                'Modified for NSlab=2 by YGC 100512
                'ElseIf
                'chkDowelX.Enabled = True 'suppressed by YGC 080612
                'chkDowelY.Enabled = True

            ElseIf cmbNSlabs.Text = "2" Then


                chkDowelX.IsEnabled = False
                chkDowelX.IsChecked = False

                'Modify ended for NSlab=2 by YGC 100512


            End If

            Call VarAsg_fromControls()
            Call VarInitControls()
            Call DrawJoint()

            If TabStrip1.SelectedIndex = 2 Then
                TabPage3_Enter()
            Else
                TabPage4_Enter()
            End If
        End If
    End Sub

    Sub DrawJoint()
        ' New subroutine to display variable slabs by YGC 060110
        Dim TireLoad As Double, EqWheelArea As Double, ContactLength As Double
        Dim Point1X, Point1Y, Point2X, Point2Y As Double
        Dim Point3X, Point3Y, Point4X, Point4Y As Double

        Dim NSlabsX As Integer, NSlabsY As Integer
        Dim SlabsDimensionX As Double, SlabsDimensionY As Double
        Dim SlabsEdgeX(4) As Double, SlabsEdgeY(4) As Double
        Dim SlabDspDimX As Double, SlabDspDimY As Double
        Dim Slab1OrgX As Double, Slab1OrgY As Double

        If NSlabs = 9 Then
            NSlabsX = 3
            NSlabsY = 3
        ElseIf NSlabs = 6 Then
            NSlabsX = 3
            NSlabsY = 2
        ElseIf NSlabs = 4 Then
            NSlabsX = 2
            NSlabsY = 2
        ElseIf NSlabs = 2 Then
            NSlabsX = 2
            NSlabsY = 1
        Else
            NSlabsX = 1
            NSlabsY = 1
        End If

        picJoint.Children.Clear()

        Dim lblY2 As Label = New Label()
        lblY2.Content = "Y"
        Canvas.SetLeft(lblY2, 10)
        Canvas.SetTop(lblY2, 10)
        picJoint.Children.Add(lblY2)

        Dim lblX2 As Label = New Label()
        lblX2.Content = "X"
        Canvas.SetLeft(lblX2, 219)
        Canvas.SetTop(lblX2, 181)
        picJoint.Children.Add(lblX2)

        '' Modified by YGC 060910
        SlabsDimensionX = NSlabsX * CSng(cmbDimX.Text)
        SlabsDimensionY = NSlabsY * CSng(cmbDimY.Text)

        If SlabsDimensionX > SlabsDimensionY Then
            SlabsLeft = 448
            SlabsRight = 2448
            SlabsTop = 1448 - (SlabsDimensionY / SlabsDimensionX) * 1000.0#
            SlabsBottom = 1448 + (SlabsDimensionY / SlabsDimensionX) * 1000.0#
        Else
            SlabsTop = 448
            SlabsBottom = 2448
            SlabsLeft = 1448 - (SlabsDimensionX / SlabsDimensionY) * 1000.0#
            SlabsRight = 1448 + (SlabsDimensionX / SlabsDimensionY) * 1000.0#
        End If

        SlabDspDimX = (SlabsRight - SlabsLeft) / NSlabsX
        SlabDspDimY = (SlabsBottom - SlabsTop) / NSlabsY
        '' Modify end by YGC 060910

        Dim sx, sy As Double
        sx = picJoint.Width / (SlabsRight + SlabsLeft) / 1.0
        sy = picJoint.Height / (SlabsBottom + SlabsTop) / 1.0

        'Canvas position
        Dim cnvTopLeft As Point = New Point(0, 0)
        Dim cnvBottomRight As Point = New Point(17, 21)

        For I = 1 To NSlabsX + 1
            SlabsEdgeY(I) = SlabsLeft + SlabDspDimX * (I - 1)
            Dim line1 As New Line()
            line1.Stroke = Brushes.Blue

            Dim pos1 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsEdgeY(I)), SlabsBottom), cnvTopLeft, cnvBottomRight)
            Dim pos2 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsEdgeY(I)), SlabsTop), cnvTopLeft, cnvBottomRight)
            line1.X1 = pos1.X
            line1.X2 = pos2.X
            line1.Y1 = pos1.Y
            line1.Y2 = pos2.Y
            line1.StrokeThickness = 0.5
            picJoint.Children.Add(line1)
        Next I

        For I = 1 To NSlabsY + 1
            SlabsEdgeX(I) = SlabsTop + SlabDspDimY * (I - 1)
            Dim line1 As New Line()
            line1.Stroke = Brushes.Blue

            Dim pos1 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsLeft, CSng(SlabsEdgeX(I))), cnvTopLeft, cnvBottomRight)
            Dim pos2 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsRight, CSng(SlabsEdgeX(I))), cnvTopLeft, cnvBottomRight)
            line1.X1 = pos1.X
            line1.X2 = pos2.X
            line1.Y1 = pos1.Y
            line1.Y2 = pos2.Y
            line1.StrokeThickness = 0.5
            picJoint.Children.Add(line1)
        Next I


        'Highlight the joint
        If chkDowelY.IsChecked Then
            For I = 2 To NSlabsX
                SlabsEdgeY(I) = SlabsLeft + SlabDspDimX * (I - 1)
                Dim line1 As New Line()
                line1.Stroke = Brushes.Navy

                Dim pos1 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsEdgeY(I)), SlabsBottom), cnvTopLeft, cnvBottomRight)
                Dim pos2 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsEdgeY(I)), SlabsTop), cnvTopLeft, cnvBottomRight)
                line1.X1 = pos1.X
                line1.X2 = pos2.X
                line1.Y1 = pos1.Y
                line1.Y2 = pos2.Y
                line1.StrokeThickness = 2
                picJoint.Children.Add(line1)
            Next I
        End If


        If chkDowelX.IsChecked Then
            For I = 2 To NSlabsY
                SlabsEdgeX(I) = SlabsTop + SlabDspDimY * (I - 1)
                Dim line1 As New Line()
                line1.Stroke = Brushes.Brown

                Dim pos1 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsLeft, CSng(SlabsEdgeX(I))), cnvTopLeft, cnvBottomRight)
                Dim pos2 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsRight, CSng(SlabsEdgeX(I))), cnvTopLeft, cnvBottomRight)
                line1.X1 = pos1.X
                line1.X2 = pos2.X
                line1.Y1 = pos1.Y
                line1.Y2 = pos2.Y
                line1.StrokeThickness = 2
                picJoint.Children.Add(line1)
            Next I
        End If


        'Highlight the Bounary Constrain
        If chkBoundaryY.IsChecked Then
            Dim line1 As New Line()
            line1.StrokeDashArray = New DoubleCollection() From {4}
            line1.Stroke = Brushes.Navy
            Dim pos11 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsLeft), SlabsBottom), cnvTopLeft, cnvBottomRight)
            Dim pos12 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsLeft), SlabsTop), cnvTopLeft, cnvBottomRight)
            line1.X1 = pos11.X
            line1.X2 = pos12.X
            line1.Y1 = pos11.Y
            line1.Y2 = pos12.Y
            line1.StrokeThickness = 2
            picJoint.Children.Add(line1)

            Dim line2 As New Line()
            line2.StrokeDashArray = New DoubleCollection() From {4}
            line2.Stroke = Brushes.Navy
            Dim pos21 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsRight), SlabsBottom), cnvTopLeft, cnvBottomRight)
            Dim pos22 = ConvertToLegacyPositionOnPicSlab(New Point(CSng(SlabsRight), SlabsTop), cnvTopLeft, cnvBottomRight)
            line2.X1 = pos21.X
            line2.X2 = pos22.X
            line2.Y1 = pos21.Y
            line2.Y2 = pos22.Y
            line2.StrokeThickness = 2
            picJoint.Children.Add(line2)
        End If


        If chkBoundaryX.IsChecked Then
            Dim line1 As New Line()
            line1.StrokeDashArray = New DoubleCollection() From {4}
            line1.Stroke = Brushes.Brown
            Dim pos11 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsLeft, CSng(SlabsTop)), cnvTopLeft, cnvBottomRight)
            Dim pos12 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsRight, CSng(SlabsTop)), cnvTopLeft, cnvBottomRight)
            line1.X1 = pos11.X
            line1.X2 = pos12.X
            line1.Y1 = pos11.Y
            line1.Y2 = pos12.Y
            line1.StrokeThickness = 2
            picJoint.Children.Add(line1)

            Dim line2 As New Line()
            line2.StrokeDashArray = New DoubleCollection() From {4}
            line2.Stroke = Brushes.Brown
            Dim pos21 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsLeft, CSng(SlabsBottom)), cnvTopLeft, cnvBottomRight)
            Dim pos22 = ConvertToLegacyPositionOnPicSlab(New Point(SlabsRight, CSng(SlabsBottom)), cnvTopLeft, cnvBottomRight)
            line2.X1 = pos21.X
            line2.X2 = pos22.X
            line2.Y1 = pos21.Y
            line2.Y2 = pos22.Y
            line2.StrokeThickness = 2
            picJoint.Children.Add(line2)
        End If

        ' Define the origin of XY on Slab 1 by YGC 060110
        If NSlabs > 1 Then
            Slab1OrgX = SlabsEdgeY(2)
        Else
            Slab1OrgX = SlabsEdgeY(1)
        End If

        If NSlabsY Mod 2 = 0 Then
            'Slab1OrgY = 1448 + SlabDspDimX / 2
            Slab1OrgY = 1448 + SlabDspDimY / 2   'corrected by YGC 100810
        Else
            Slab1OrgY = 1448
        End If
        ' Define end by YGC 060110

        ' Modified by YGC 060110
        Dim lineH As New Line()
        lineH.StrokeThickness = 0.5
        lineH.Stroke = Brushes.Black
        Dim posH1 = ConvertToLegacyPositionOnPicSlab(New Point(Slab1OrgX, Slab1OrgY), cnvTopLeft, cnvBottomRight)
        Dim posH2 = ConvertToLegacyPositionOnPicSlab(New Point(2895, Slab1OrgY), cnvTopLeft, cnvBottomRight)
        lineH.X1 = posH1.X
        lineH.X2 = posH2.X
        lineH.Y1 = posH1.Y
        lineH.Y2 = posH2.Y
        picJoint.Children.Add(lineH)

        Dim lineV As New Line()
        lineV.StrokeThickness = 0.5
        lineV.Stroke = Brushes.Black
        Dim posV1 = ConvertToLegacyPositionOnPicSlab(New Point(Slab1OrgX, Slab1OrgY), cnvTopLeft, cnvBottomRight)
        Dim posV2 = ConvertToLegacyPositionOnPicSlab(New Point(Slab1OrgX, 0), cnvTopLeft, cnvBottomRight)
        lineV.X1 = posV1.X
        lineV.X2 = posV2.X
        lineV.Y1 = posV1.Y
        lineV.Y2 = posV2.Y
        picJoint.Children.Add(lineV)

        Canvas.SetLeft(lblX2, 5 + (SlabsRight + 120) * sx)
        Canvas.SetTop(lblX2, -10 + (Slab1OrgY - 300) * sx)

        Canvas.SetLeft(lblY2, 5 + (Slab1OrgX + 120) * sy)
        Canvas.SetTop(lblY2, -10 + (SlabsTop - 300) * sy)
        ' Modify end by YGC 060110

        modAutoMesh.Interior = False

        TireLoad = ((CDbl(PcntOnMainGears / 100.0#) * GrossWeight) / NMainGears) * (CDbl(1.0# / NWheels))
        EqWheelArea = TireLoad / TirePressure
        ContactLength = Math.Sqrt(EqWheelArea / 0.5227)
        If PCARectangle = True Then
            WhlWidth = 0.6 * ContactLength 'for PCA rectangular footprint
            WhlLength = 0.8712 * ContactLength 'for PCA rectangular footprint
        Else
            WhlWidth = Math.Sqrt(EqWheelArea) 'for square footprint
            WhlLength = Math.Sqrt(EqWheelArea) 'for square footprint
        End If

        Call TirePositions()

        'Modified by YGC 073010
        'If XDimension > 28.5 Or YDimension > 28.5 Then ScaleFactor = 3.33 / NSlabsX Else: ScaleFactor = 6.67 / NSlabsX 'added by DRB 4/30/01, Modified by YGC 060110
        ' Note the ScaleFactor here is not appropriate to the slab size except for XDimension=30
        If SlabsDimensionX > SlabsDimensionY Then
            ScaleFactor = 2000 / (SlabsDimensionX * 12)
        Else
            ScaleFactor = 2000 / (SlabsDimensionY * 12)
        End If
        'Modify end


        'added to account for load poisition check by YGC 100810
        Dim PointXmax, PointXmin, PointYmax, PointYmin As Double

        For I = 1 To NWheels
            If GearAngle > 0 And GearAngle < 90.0# Then
                Angle1 = Math.PI / 2.0# - (-GearAngle * Math.PI / 180 + Math.Atan(modAutoMesh.Xfp / modAutoMesh.Yfp))
                Angle2 = Math.PI / 2.0# - (GearAngle * Math.PI / 180 + Math.Atan(modAutoMesh.Xfp / modAutoMesh.Yfp))
                Point1X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - modAutoMesh.Hypot * Math.Cos(Angle1))
                Point1Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + modAutoMesh.Hypot * Math.Sin(Angle1))
                Point2X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + modAutoMesh.Hypot * Math.Cos(Angle2))
                Point2Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + modAutoMesh.Hypot * Math.Sin(Angle2))
                Point3X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + modAutoMesh.Hypot * Math.Cos(Angle1))
                Point3Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - modAutoMesh.Hypot * Math.Sin(Angle1))
                Point4X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - modAutoMesh.Hypot * Math.Cos(Angle2))
                Point4Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - modAutoMesh.Hypot * Math.Sin(Angle2))
            ElseIf GearAngle = 0 Then


                Point1X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - 0.5 * WhlWidth)
                Point1Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + 0.5 * WhlLength)
                Point2X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + 0.5 * WhlWidth)
                Point2Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + 0.5 * WhlLength)
                Point3X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + 0.5 * WhlWidth)
                Point3Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - 0.5 * WhlLength)
                Point4X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - 0.5 * WhlWidth)
                Point4Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - 0.5 * WhlLength)
            Else
                Point1X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - 0.5 * WhlLength)
                Point1Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + 0.5 * WhlWidth)
                Point2X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + 0.5 * WhlLength)
                Point2Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) + 0.5 * WhlWidth)
                Point3X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) + 0.5 * WhlLength)
                Point3Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - 0.5 * WhlWidth)
                Point4X = Slab1OrgX + ScaleFactor * (modAutoMesh.Xtr(I) - 0.5 * WhlLength)
                Point4Y = Slab1OrgY + ScaleFactor * (modAutoMesh.Ytr(I) - 0.5 * WhlWidth)
            End If

            Dim line1 As New Line()
            line1.Stroke = Brushes.Black
            Dim pos11 = ConvertToLegacyPositionOnPicSlab(New Point(Point1X, Point1Y), cnvTopLeft, cnvBottomRight)
            Dim pos12 = ConvertToLegacyPositionOnPicSlab(New Point(Point2X, Point2Y), cnvTopLeft, cnvBottomRight)
            line1.X1 = pos11.X
            line1.X2 = pos12.X
            line1.Y1 = pos11.Y
            line1.Y2 = pos12.Y
            line1.StrokeThickness = 0.5
            picJoint.Children.Add(line1)

            Dim line2 As New Line()
            line2.Stroke = Brushes.Black
            Dim pos21 = ConvertToLegacyPositionOnPicSlab(New Point(Point2X, Point2Y), cnvTopLeft, cnvBottomRight)
            Dim pos22 = ConvertToLegacyPositionOnPicSlab(New Point(Point3X, Point3Y), cnvTopLeft, cnvBottomRight)
            line2.X1 = pos21.X
            line2.X2 = pos22.X
            line2.Y1 = pos21.Y
            line2.Y2 = pos22.Y
            line2.StrokeThickness = 0.5
            picJoint.Children.Add(line2)

            Dim line3 As New Line()
            line3.Stroke = Brushes.Black
            Dim pos31 = ConvertToLegacyPositionOnPicSlab(New Point(Point3X, Point3Y), cnvTopLeft, cnvBottomRight)
            Dim pos32 = ConvertToLegacyPositionOnPicSlab(New Point(Point4X, Point4Y), cnvTopLeft, cnvBottomRight)
            line3.X1 = pos31.X
            line3.X2 = pos32.X
            line3.Y1 = pos31.Y
            line3.Y2 = pos32.Y
            line3.StrokeThickness = 0.5
            picJoint.Children.Add(line3)

            Dim line4 As New Line()
            line4.Stroke = Brushes.Black
            Dim pos41 = ConvertToLegacyPositionOnPicSlab(New Point(Point4X, Point4Y), cnvTopLeft, cnvBottomRight)
            Dim pos42 = ConvertToLegacyPositionOnPicSlab(New Point(Point1X, Point1Y), cnvTopLeft, cnvBottomRight)
            line4.X1 = pos41.X
            line4.X2 = pos42.X
            line4.Y1 = pos41.Y
            line4.Y2 = pos42.Y
            line4.StrokeThickness = 0.5
            picJoint.Children.Add(line4)

            If I = 1 Then
                PointXmax = Point1X
                PointXmin = Point1X
                PointYmax = Point1Y
                PointYmin = Point1Y
            End If

            PointXmax = Math.Max(PointXmax, Math.Max(Math.Max(Point1X, Point2X), Math.Max(Point3X, Point4X)))
            PointXmin = Math.Min(PointXmin, Math.Min(Math.Min(Point1X, Point2X), Math.Min(Point3X, Point4X)))
            PointYmax = Math.Max(PointYmax, Math.Max(Math.Max(Point1Y, Point2Y), Math.Max(Point3Y, Point4Y)))
            PointYmin = Math.Min(PointYmin, Math.Min(Math.Min(Point1Y, Point2Y), Math.Min(Point3Y, Point4Y)))

        Next I
    End Sub
    Sub VarInitControls()
        'Events TabStrip1_Click	
        'TabPage1_Enter	TabPage2_Enter	TabPage3_Enter	TabPage4_Enter

        'Tab 2 Pavement Structure
        cmbNSlabs.Text = CStr(NSlabs)

        'If InteriorLoad Then
        '    optInterior.IsChecked = True
        'ElseIf EdgeLoad Then
        '    optEdge.IsChecked = True
        'End If

        cmbAngle.Text = CStr(GearAngle)

        'Tab 3 Joint Modeling
        txtJntStfX.Text = CStr(EqJStfX)
        txtJntStfY.Text = CStr(EqJStfY)

        txtEdgStfX.Text = CStr(EqEdgStfX)  'added for boundary(Edge) spring by YGC 110110
        txtEdgStfY.Text = CStr(EqEdgStfY)

    End Sub

    Private Sub BtnSaveACinLibrary_Click(sender As Object, e As RoutedEventArgs) Handles BtnSaveACinLibrary.Click
        Dim S As String

        If lstACGroupIndex + 1 <> ExternalLibraryIndex Then
            S = JobTitle & " was selected" & vbCrLf
            S = S & "from the internal library." & NL2
            S = S & "Data in the internal library" & vbCrLf
            S = S & "cannot be changed." & NL2
            S = S & "Do you want to add the data for the" & vbCrLf
            S = S & "current aircraft to the external library?"
            Ret = MsgBox(S, MsgBoxStyle.YesNo, "Saving Current Data")
            If Ret = MsgBoxResult.No Then
                Exit Sub
            Else
                BtnAddACtoLibrary_Click(New Object, Nothing)
                Exit Sub
            End If
        End If

        S = "The current data for " & JobTitle & vbCrLf
        S = S & "will replace the data in the external library." & NL2
        S = S & "Do you want to change the data for the" & vbCrLf
        S = S & "current aircraft in the external library?"
        Ret = MsgBox(S, MsgBoxStyle.YesNo, "Saving Current Data")
        If Ret = MsgBoxResult.No Then
            Exit Sub
        Else
            Call UpdateLibraryData(LibIndex)
            Call WriteExternalFile()
        End If
    End Sub
    Private Function Distance(ByVal mousePoint As Point, ByVal ellipsePoint As Point) As Integer
        Return Math.Sqrt((Math.Abs(If(mousePoint.X > ellipsePoint.X, mousePoint.X - ellipsePoint.X, ellipsePoint.X - mousePoint.X)) ^ 2) + (Math.Abs(If(mousePoint.Y > ellipsePoint.Y, mousePoint.Y - ellipsePoint.Y, ellipsePoint.Y - mousePoint.Y)) ^ 2))
    End Function

    Private _closestCoordenates As Point
    Private _isAlreadySelected As Boolean
    Private _closestCoordinatesLegacy As Point
    Private _closestCoordinatesSelectedByUser As Boolean
    Private Sub picGear_MouseLeftButtonDown(sender As Object, e As MouseButtonEventArgs) Handles picGear.MouseLeftButtonDown
        Dim Radius As Double
        If BtnAddWheel.IsChecked Then

            NWheels = NWheels + 1

            Dim canvasPosition = e.GetPosition(picGear)
            Dim wheelPositionToAdd = ConvertToLegacyPosition(New Point(canvasPosition.Y, canvasPosition.X), topLeft, bottomRight)

            YWheels(NWheels) = wheelPositionToAdd.Y
            XWheels(NWheels) = wheelPositionToAdd.X
            BtnAddWheel.IsChecked = False
            Call ResetOutputs()
            Call PlotGear()

        ElseIf BtnSelectWheel.IsChecked = True AndAlso Not _isAlreadySelected Then
            Dim pointerPosition = e.GetPosition(picGear)
            Dim pointerPositionLegacy = ConvertToLegacyPosition(New Point(pointerPosition.Y, pointerPosition.X), topLeft, bottomRight)
            Dim closest As Int32 = Int32.MaxValue
            For Each o As Object In picGear.Children
                If TypeOf o Is Rectangle Then
                    Dim rectanguleInCanvas As Rectangle = CType(o, Rectangle)
                    If rectanguleInCanvas.Clip Is Nothing Then
                        Dim yInCanvas = Canvas.GetTop(rectanguleInCanvas)
                        Dim xInCanvas = Canvas.GetLeft(rectanguleInCanvas)
                        Dim wheelWidthCanvas = rectanguleInCanvas.Width / 2
                        Dim wheelHeightCanvas = rectanguleInCanvas.Height / 2
                        Dim coordWheelInCanvas = ConvertToLegacyPosition(New Point(yInCanvas + wheelHeightCanvas, xInCanvas + wheelWidthCanvas), topLeft, bottomRight)
                        Dim distanceToPointer As Integer = Distance(pointerPositionLegacy, coordWheelInCanvas)
                        If distanceToPointer < closest Then
                            closest = distanceToPointer
                            _closestCoordinatesLegacy = coordWheelInCanvas
                            _closestCoordenates = New Point(xInCanvas, yInCanvas)
                            _closestCoordinatesSelectedByUser = True
                        End If
                    End If
                End If
            Next

            ''set the last selected wheel
            'For n As Integer = 1 To NWheels
            '    'TODO: Fix bug - on selection need to Round 
            '    If XWheels(n) = _closestCoordinatesLegacy.X AndAlso YWheels(n) = _closestCoordinatesLegacy.Y AndAlso _closestCoordinatesSelectedByUser Then
            '        _lastSelectedWheel = n
            '    End If
            'Next
            Call SelectWheel(XWheels, YWheels, _closestCoordinatesLegacy.X, _closestCoordinatesLegacy.Y, Radius, IWheelSelected)
            '***************************************
            _isAlreadySelected = True
            IWheelSelected = IWheelSelected '_lastSelectedWheel Modified by VK
            LastIWheel = IWheelSelected
            LastXP = XWheels(LastIWheel)
            LastYP = YWheels(LastIWheel)
            LabelXSelCoord.Content = "X Sel. = " & Format(LastXP, "#,##0.00") & "in"
            LabelYSelCoord.Content = "Y Sel. = " & Format(LastYP, "#,##0.00") & "in"
            StartWheelIndex = IWheelSelected
            LastOperation = Operation
            Operation = NoOperation
            '***************************************
            PlotGear()
        End If
    End Sub

    Private _selectedRectangule As Rectangle
    Private _selectedCoord As Point
    Private Sub Rectangule_MouseLeftButtonDown(ByVal sender As System.Object, ByVal e As System.Windows.Input.MouseEventArgs)
        Dim rectInCanvas = TryCast(sender, Rectangle)

        If BtnMoveWheel.IsChecked = True AndAlso rectInCanvas IsNot Nothing Then

            'Convert the WheelRadius to a "Canvas Size" number
            'Dim drawnWheelRadius = ConvertFromLegacyPosition(New Point(topLeft.X + WheelRadius, topLeft.Y + WheelRadius), topLeft, bottomRight).X
            Dim yInCanvas = Canvas.GetTop(rectInCanvas)
            Dim xInCanvas = Canvas.GetLeft(rectInCanvas)
            Dim wheelWidthCanvas = rectInCanvas.Width / 2
            Dim wheelHeightCanvas = rectInCanvas.Height / 2
            _selectedCoord = ConvertToLegacyPosition(New Point(yInCanvas + wheelHeightCanvas, xInCanvas + wheelWidthCanvas), topLeft, bottomRight)

            _selectedRectangule = rectInCanvas

            AddHandler picGear.MouseLeftButtonUp, AddressOf Rectangule_MouseLeftButtonUp
            AddHandler picGear.MouseMove, AddressOf Rectangule_MouseMove
        ElseIf BtnRemoveWheel.IsChecked = True AndAlso rectInCanvas IsNot Nothing Then
            If NWheels = 1 Then
                BtnRemoveWheel.IsChecked = False
                MessageBox.Show("The gear has only one wheel." & Environment.NewLine & "The wheel cannot be removed.", "Removing a Wheel", MessageBoxButton.OK, MessageBoxImage.Warning)
            Else
                Dim yInCanvas = Canvas.GetTop(rectInCanvas)
                Dim xInCanvas = Canvas.GetLeft(rectInCanvas)
                Dim wheelWidthCanvas = rectInCanvas.Width / 2
                Dim wheelHeightCanvas = rectInCanvas.Height / 2
                Dim selectedCoord = ConvertToLegacyPosition(New Point(yInCanvas + wheelHeightCanvas, xInCanvas + wheelWidthCanvas), topLeft, bottomRight)
                For I = 1 To NWheels
                    If Math.Round(XWheels(I), 2) = Math.Round(selectedCoord.X, 2) AndAlso Math.Round(YWheels(I), 2) = Math.Round(selectedCoord.Y, 2) Then
                        NWheels = NWheels - 1
                        For J = I To NWheels
                            XWheels(J) = XWheels(J + 1)
                            YWheels(J) = YWheels(J + 1)
                        Next J
                        Exit For
                    End If
                Next I

                BtnRemoveWheel.IsChecked = False
                Call ResetOutputs()
                Call PlotGear()
            End If
        End If
    End Sub
    Private Sub Rectangule_MouseLeftButtonUp(ByVal sender As System.Object, ByVal e As System.Windows.Input.MouseEventArgs)
        If _selectedRectangule IsNot Nothing AndAlso BtnMoveWheel.IsChecked Then
            For I = 1 To NWheels
                If Math.Round(XWheels(I), 2) = _selectedCoord.X AndAlso Math.Round(YWheels(I), 2) = _selectedCoord.Y Then
                    'If XWheels(I) = _selectedCoord.X AndAlso YWheels(I) = _selectedCoord.Y Then
                    Dim yInCanvas = Canvas.GetTop(_selectedRectangule)
                    Dim xInCanvas = Canvas.GetLeft(_selectedRectangule)
                    Dim wheelWidthCanvas = _selectedRectangule.Width / 2
                    Dim wheelHeightCanvas = _selectedRectangule.Height / 2
                    Dim finalPosition = ConvertToLegacyPosition(New Point(yInCanvas + wheelHeightCanvas, xInCanvas + wheelWidthCanvas), topLeft, bottomRight)
                    XWheels(I) = finalPosition.X
                    YWheels(I) = finalPosition.Y
                    Exit For
                End If
            Next I
            _selectedRectangule.Stroke = Nothing
            _selectedRectangule.Fill = Brushes.Black
            _selectedRectangule = Nothing
            _selectedCoord = Nothing
            BtnMoveWheel.IsChecked = False
            PlotGear()
        End If
    End Sub

    Private Sub Rectangule_MouseMove(sender As Object, e As MouseEventArgs)
        If _selectedRectangule IsNot Nothing AndAlso e.LeftButton = MouseButtonState.Pressed Then
            _selectedRectangule.Stroke = Brushes.Black
            _selectedRectangule.StrokeThickness = 1
            _selectedRectangule.Fill = Nothing
            Canvas.SetTop(_selectedRectangule, e.GetPosition(picGear).Y - _selectedRectangule.ActualHeight / 2)
            Canvas.SetLeft(_selectedRectangule, e.GetPosition(picGear).X - _selectedRectangule.ActualWidth / 2)
        End If
    End Sub

    Private Sub LabelXSelCoord_MouseLeftButtonDown(sender As Object, e As MouseButtonEventArgs) Handles LabelXSelCoord.MouseLeftButtonDown
        Dim S, SS As String
        Dim SVS As Double
        Dim CurrentValue, NewValue As Double
        Dim MinValue, MaxValue As Double

        If LastOperation <> SelectAWheel Then Exit Sub

        CurrentValue = XWheels(LastIWheel)
        MinValue = -1000
        MaxValue = 1000

        S = "The current longitudinal coordinate (X direction)" & vbCrLf
        S = S & "of the selected wheel is "
        S = S & Format(CurrentValue, "#,##0.00") & " inches." & NL2
        S = S & "Enter a new value in the range:"
        S = S & NL2 & Format(MinValue, "#,###,##0")
        S = S & " to " & Format(MaxValue, "#,###,##0") & "."
        S = S & NL2 & "Click Cancel at any time to retain the old value."
        SS = "Changing Selected Wheel X"

        If GetInputSingle(S, SS, SVS) Then

            NewValue = SVS

            '   Check to See if Value is within range.
            If NewValue < MinValue Or MaxValue < NewValue Then
                NewValue = CurrentValue
                S = "X coordinates cannot be less than "
                S = S & Format(MinValue, "0.000") & vbCrLf
                S = S & "or greater than "
                S = S & Format(MaxValue, "0.000") & "." & NL2
                S = S & "The old value has been retained."
                Ret = MsgBox(S, 0, "")
            End If

            XWheels(LastIWheel) = NewValue

        End If
        LastOperation = ChangeYCoordinate
        LabelXSelCoord.Content = ""
        LabelYSelCoord.Content = ""
        BtnSelectWheel_Click(BtnSelectWheel, Nothing)
        Call PlotGear()
    End Sub

    Private Sub LabelYSelCoord_MouseLeftButtonDown(sender As Object, e As MouseButtonEventArgs) Handles LabelYSelCoord.MouseLeftButtonDown
        Dim S, SS As String
        Dim SVS As Double
        Dim CurrentValue, NewValue As Double
        Dim MinValue, MaxValue As Double

        If LastOperation <> SelectAWheel Then Exit Sub

        CurrentValue = YWheels(LastIWheel)
        MinValue = -1000
        MaxValue = 1000

        S = "The current lateral coordinate (Y direction)" & vbCrLf
        S = S & "of the selected wheel is "
        S = S & Format(CurrentValue, "#,##0.00") & " inches." & NL2
        S = S & "Enter a new value in the range:"
        S = S & NL2 & Format(MinValue, "#,###,##0")
        S = S & " to " & Format(MaxValue, "#,###,##0") & "."
        S = S & NL2 & "Click Cancel at any time to retain the old value."
        SS = "Changing Selected Wheel Y"

        If GetInputSingle(S, SS, SVS) Then

            NewValue = SVS

            '   Check to See if Value is within range.
            If NewValue < MinValue Or MaxValue < NewValue Then
                NewValue = CurrentValue
                S = "Y coordinates cannot be less than "
                S = S & Format(MinValue, "0.000") & vbCrLf
                S = S & "or greater than "
                S = S & Format(MaxValue, "0.000") & "." & NL2
                S = S & "The old value has been retained."
                Ret = MsgBox(S, 0, "")
            End If

            YWheels(LastIWheel) = NewValue

        End If

        LastOperation = ChangeYCoordinate
        LabelXSelCoord.Content = ""
        LabelYSelCoord.Content = ""
        BtnSelectWheel_Click(BtnSelectWheel, Nothing)
        Call PlotGear()
    End Sub
    Public Shared Function GetRowIndex(dataGridCell As DataGridCell) As Integer
        ' Use reflection to get DataGridCell.RowDataItem property value.
        Dim rowDataItemProperty As Reflection.PropertyInfo = dataGridCell.[GetType]().GetProperty("RowDataItem", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)

        Dim dataGrid As DataGrid = GetDataGridFromChild(dataGridCell)

        Return dataGrid.Items.IndexOf(rowDataItemProperty.GetValue(dataGridCell, Nothing))
    End Function
    Public Shared Function GetDataGridFromChild(dataGridPart As DependencyObject) As DataGrid
        If VisualTreeHelper.GetParent(dataGridPart) Is Nothing Then
            Throw New NullReferenceException("Control is null.")
        End If
        If TypeOf VisualTreeHelper.GetParent(dataGridPart) Is DataGrid Then
            Return DirectCast(VisualTreeHelper.GetParent(dataGridPart), DataGrid)
        Else
            Return GetDataGridFromChild(VisualTreeHelper.GetParent(dataGridPart))
        End If
    End Function
    Private Sub grdLayerProps_MouseDoubleClick(sender As Object, e As MouseButtonEventArgs) Handles grdLayerProps.MouseDoubleClick
        If IsNothing(grdLayerProps.CurrentCell) OrElse IsNothing(grdLayerProps.CurrentCell.Column) Then
            Return
        End If
        Dim rowIndex, columnIndex As Integer
        Dim currentCell As DataGridCellInfo = grdLayerProps.CurrentCell()
        columnIndex = currentCell.Column.DisplayIndex()
        Dim cellContent = currentCell.Column.GetCellContent(currentCell.Item)
        Dim cell As DataGridCell = DirectCast(cellContent.Parent, DataGridCell)
        rowIndex = GetRowIndex(cell)

        Dim frmProperty As New PropertyWindow


        If columnIndex = 1 And rowIndex >= -1 Then 'for E Modulus

            frmProperty.lblEnterNewValue.Content = "Enter New E Modulus"
            If rowIndex = 0 Or (ChkOverlay.IsChecked = True And rowIndex = 1) Then 'for PCC slab
                frmProperty.lblRange.Text = "Allowable Values in the Range 500,000 psi to 8,000,000 psi"
                MinVal = 500000 : MaxVal = 8000000
                frmProperty.cmbEnterNewValue.Items.Add(Format(500000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(1000000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(2000000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(3000000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(4000000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(5000000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(6000000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(7000000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(8000000, "#,###,000"))

            ElseIf rowIndex = NumberOfLayers - 1 Then  'for subgrade

                frmProperty.lblRange.Text = "Allowable Values in the Range 3,000 psi to 4,000,000 psi"
                MinVal = 3000 : MaxVal = 4000000
                frmProperty.cmbEnterNewValue.Items.Add(Format(3000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(4500, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(15000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(20000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(30000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(50000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(100000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(200000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(4000000, "#,###,000"))

            Else

                frmProperty.lblRange.Text = "Allowable Values in the Range 3,000 psi to 4,000,000 psi"
                MinVal = 3000 : MaxVal = 4000000
                frmProperty.cmbEnterNewValue.Items.Add(Format(3000, "#,###,000")) 'added by YGC 110310
                frmProperty.cmbEnterNewValue.Items.Add(Format(5000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(10000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(15000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(20000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(30000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(75000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(100000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(200000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(500000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(1000000, "#,###,000"))
                frmProperty.cmbEnterNewValue.Items.Add(Format(4000000, "#,###,000"))

            End If

            frmProperty.cmbEnterNewValue.Text = grdLayerTable(rowIndex)(columnIndex)
            frmProperty.Show(rowIndex, columnIndex)


        ElseIf columnIndex = 2 And rowIndex >= -1 Then  'for Poisson's Ratio
            frmProperty.lblEnterNewValue.Content = "Enter New Poisson's Ratio"
            frmProperty.lblRange.Text = "Allowable Values in the Range 0.15 to 0.45"
            MinVal = 0.15 : MaxVal = 0.45

            frmProperty.cmbEnterNewValue.Items.Add(0.15)
            frmProperty.cmbEnterNewValue.Items.Add(0.2)
            frmProperty.cmbEnterNewValue.Items.Add(0.25)
            frmProperty.cmbEnterNewValue.Items.Add(0.3)
            frmProperty.cmbEnterNewValue.Items.Add(0.35)
            frmProperty.cmbEnterNewValue.Items.Add(0.4)
            frmProperty.cmbEnterNewValue.Items.Add(0.45)

            frmProperty.cmbEnterNewValue.Text = grdLayerTable(rowIndex)(columnIndex)
            frmProperty.Show(rowIndex, columnIndex)

        ElseIf columnIndex = 3 And rowIndex >= -1 Then  'for Layer Thickness

            'If GrdLayerProps.Text = "Infinite" Then
            If grdLayerTable(rowIndex)(columnIndex) = "Infinite" Then
                MsgBox("Infinite Subgrade Thickness - Cannot be Changed")
                GoTo 999
            End If

            frmProperty.lblEnterNewValue.Content = "Enter New Layer Thickness"
            If rowIndex = 0 Or (ChkOverlay.IsChecked = True And rowIndex = 1) Then 'for PCC slab
                frmProperty.lblRange.Text = "Allowable Values in the Range 4.0 in to 24.0 in"
                MinVal = 4.0# : MaxVal = 24.0#

                frmProperty.cmbEnterNewValue.Items.Add(4)
                frmProperty.cmbEnterNewValue.Items.Add(6)
                frmProperty.cmbEnterNewValue.Items.Add(8)
                frmProperty.cmbEnterNewValue.Items.Add(10)
                frmProperty.cmbEnterNewValue.Items.Add(12)
                frmProperty.cmbEnterNewValue.Items.Add(14)
                frmProperty.cmbEnterNewValue.Items.Add(16)
                frmProperty.cmbEnterNewValue.Items.Add(18)
                frmProperty.cmbEnterNewValue.Items.Add(24)

            Else

                frmProperty.lblRange.Text = "Allowable Values in the Range 4.0 in to 120.0 in"
                MinVal = 4.0# : MaxVal = 120.0#

                frmProperty.cmbEnterNewValue.Items.Add(4)
                frmProperty.cmbEnterNewValue.Items.Add(6)
                frmProperty.cmbEnterNewValue.Items.Add(8)
                frmProperty.cmbEnterNewValue.Items.Add(10)
                frmProperty.cmbEnterNewValue.Items.Add(12)
                frmProperty.cmbEnterNewValue.Items.Add(30)
                frmProperty.cmbEnterNewValue.Items.Add(60)
                frmProperty.cmbEnterNewValue.Items.Add(90)
                frmProperty.cmbEnterNewValue.Items.Add(120)

            End If

            frmProperty.cmbEnterNewValue.Text = grdLayerTable(rowIndex)(columnIndex)
            frmProperty.Show(rowIndex, columnIndex)

        End If
999:
    End Sub

    Public Sub btnAddALayer_Click(sender As Object, e As RoutedEventArgs) Handles btnAddALayer.Click
        Call VarAsg_fromControls()

        Dim row6 As DataRow
        row6 = grdLayerTable.NewRow

        If NumberOfLayers = 6 Then
        Else
            btnDeleteALayer.IsEnabled = True
            grdLayerTable.Rows.InsertAt(row6, NumberOfLayers - 1)
            NumberOfLayers = NumberOfLayers + 1

            Dim indx As Short
            If Overlay Then
                indx = NumberOfLayers - 3
            Else
                indx = NumberOfLayers - 2
            End If

            grdLayerTable(NumberOfLayers - 2)(0) = "Subbase " & CStr(indx)
            grdLayerTable(NumberOfLayers - 2)(1) = Format(EMod(indx), "#,###,##0")
            grdLayerTable(NumberOfLayers - 2)(2) = Format(PoissonsRatio(indx), "0.00")
            grdLayerTable(NumberOfLayers - 2)(3) = Format(LayerThickness(indx), "0.00")
        End If

        If NumberOfLayers = 6 Then
            btnAddALayer.IsEnabled = False
        End If
    End Sub

    Sub VarAsg_fromControls()

        Call TextInputCheck()

        If ErrorInput = True Then
            Exit Sub
        End If

        'Tab 2 Pavement Structure
        NSlabs = CShort(cmbNSlabs.Text)
        GearAngle = CSng(cmbAngle.Text)

        If ChkOverlay.IsChecked = False Then 'no PCC overlay present
            For I = 1 To NumberOfLayers - 1
                EMod(I) = grdLayerTable(I - 1)(1)
                PoissonsRatio(I) = grdLayerTable(I - 1)(2)
                LayerThickness(I) = grdLayerTable(I - 1)(3)
            Next I

            EMod(6) = grdLayerTable(I - 1)(1)
            PoissonsRatio(6) = grdLayerTable(I - 1)(2)

        Else 'PCC overlay present

            For I = 0 To NumberOfLayers - 2
                EMod(I) = grdLayerTable(I)(1)
                PoissonsRatio(I) = grdLayerTable(I)(2)
                LayerThickness(I) = grdLayerTable(I)(3)
            Next I

            EMod(6) = grdLayerTable(I)(1)
            PoissonsRatio(6) = grdLayerTable(I)(2)

        End If

        'Tab 3 Joint Modeling
        ' Assign dowel bar stiffness values only if checkboxes are checked
        If chkDowelX.IsChecked Then
            EqJStfX = CSng(txtJntStfX.Text)
        Else
            EqJStfX = 0.0  ' Set to zero when checkbox is unchecked
        End If
        
        If chkDowelY.IsChecked Then
            EqJStfY = CSng(txtJntStfY.Text)
        Else
            EqJStfY = 0.0  ' Set to zero when checkbox is unchecked
        End If
        'added for Boundary(Edge) spring by YGC 110110
        EqEdgStfX = CSng(txtEdgStfX.Text) : EqEdgStfY = CSng(txtEdgStfY.Text)

        gXcoord1 = CSng(txtXCoord.Text)
        gYcoord1 = CSng(txtYCoord.Text)

        gXcoord2 = CSng(txtXCoordMoved.Text)
        gYcoord2 = CSng(txtYCoordMoved.Text)
        NGearPositions = CSng(txtNGearPositons.Text)

    End Sub

    Public Sub btnDeleteALayer_Click(sender As Object, e As RoutedEventArgs) Handles btnDeleteALayer.Click
        If NumberOfLayers = 3 Or (NumberOfLayers = 4 And ChkOverlay.IsChecked) Then
        Else
            btnAddALayer.IsEnabled = True
            grdLayerTable.Rows.RemoveAt(NumberOfLayers - 2)
            NumberOfLayers = NumberOfLayers - 1
        End If

        If NumberOfLayers = 3 Or (NumberOfLayers = 4 And ChkOverlay.IsChecked) Then
            btnDeleteALayer.IsEnabled = False
        End If
    End Sub

    Private Sub cmbDimX_TextChanged(sender As Object, e As TextChangedEventArgs)
        If Me.IsInitializing = True Then
            Exit Sub
        End If

        If Not IsNumeric(sender.Text) Then
            MessageBox.Show("Please input numeric value")
            Exit Sub
        End If

        If NWheels = 0 Then Exit Sub

        Dim checkedRadioButton As New RadioButton
        checkedRadioButton.IsChecked = True

        If optInterior.IsChecked Then
            Call optInterior_Checked(checkedRadioButton, Nothing)
        End If

        'If Not optInterior.IsChecked Then
        If optEdge.IsChecked Then
            Call optEdge_Checked(checkedRadioButton, Nothing)
        End If

        If sender.Text <> "" And sender.Text <> "." Then
            Call DrawGear()
        End If
    End Sub

    Private Sub cmbDimX_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles cmbDimX.SelectionChanged
        'If Me.IsInitializing = True Then
        '    Exit Sub
        'End If

        'If NWheels = 0 Then Exit Sub

        'Dim checkedRadioButton As New RadioButton
        'checkedRadioButton.IsChecked = True

        'If optInterior.IsChecked Then
        '    Call optInterior_Checked(checkedRadioButton, Nothing)
        'End If

        'If Not optInterior.IsChecked Then
        '    Call optEdge_Checked(checkedRadioButton, Nothing)
        'End If

        'If sender.SelectedItem <> "" And sender.SelectedItem <> "." Then
        '    Call DrawGear()
        'End If
    End Sub

    Private Sub cmbDimY_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles cmbDimY.SelectionChanged
        'If Me.IsInitializing = True Then
        '    Exit Sub
        'End If

        'If NWheels = 0 Then Exit Sub

        'Dim checkedRadioButton As New RadioButton
        'checkedRadioButton.IsChecked = True

        'If optInterior.IsChecked Then
        '    Call optInterior_Checked(checkedRadioButton, Nothing)
        'End If

        'If Not optInterior.IsChecked Then
        '    Call optEdge_Checked(checkedRadioButton, Nothing)
        'End If

        'If sender.SelectedItem <> "" And sender.SelectedItem <> "." Then
        '    Call DrawGear()
        'End If
    End Sub

    Private Sub optInterior_Checked(sender As Object, e As RoutedEventArgs) Handles optInterior.Checked
        If Me.IsInitializing = True Then
            Exit Sub
        End If

        If sender.IsChecked Then

            InteriorLoad = True
            EdgeLoad = False

            ChkAllowSymmetry.IsEnabled = True

            cmbAngle.IsEnabled = True
            lblAngle.IsEnabled = True

            YCoord = 0.0#
            XCoord = CSng(cmbDimX.Text) * 12.0# / 2.0#
            txtXCoord.Text = Format(XCoord, "##0.00000")
            txtYCoord.Text = CStr(YCoord)

            Call PlotGear()
            Call DrawGear()

        End If
    End Sub

    Private Sub optEdge_Checked(sender As Object, e As RoutedEventArgs) Handles optEdge.Checked
        If Me.IsInitializing = True Then
            Exit Sub
        End If

        If sender.IsChecked Then

            InteriorLoad = False
            EdgeLoad = True

            ChkAllowSymmetry.IsEnabled = True

            YCoord = 0.0#
            XCoord = 0.0# - modAutoMesh.XtrMin + modAutoMesh.Xfp / 2.0#
            txtXCoord.Text = Format(XCoord, "##0.00000")
            txtYCoord.Text = CStr(YCoord)

            Call PlotGear()
            If cmbDimX.Text <> "" And cmbDimX.Text <> "." And
             cmbDimY.Text <> "" And cmbDimY.Text <> "." Then
                Call DrawGear()
            End If

        End If
    End Sub

    Private Sub TabPage1_Enter()
        Call VarInitControls()
        Call VarAsg_fromControls()
    End Sub

    Private Sub TabPage2_Enter()
        sldIFParam.Value = IFParam

        If Winkler = True Then
            optWinkler.IsChecked = True
        Else
            optSubgrade.IsChecked = True
        End If

        Call DrawGear()  'YGC 060110
        Call VarInitControls()

        txtXCoord.Text = gXcoord1
        txtYCoord.Text = gYcoord1

        txtXCoordMoved.Text = gXcoord2
        txtYCoordMoved.Text = gYcoord2
        txtNGearPositons.Text = NGearPositions
    End Sub

    Private Sub TabPage3_Enter()
        Call VarInitControls()
        Call DrawJoint()
    End Sub

    Private Sub TabPage4_Enter()
        Call VarAsg_fromControls()
        Call VarInitControls()
    End Sub

    Private Sub cmbNSlabs_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles cmbNSlabs.SelectionChanged
        If Me.IsInitializing = True Then
            Exit Sub
        End If

        NSlabs = CSng(cmbNSlabs.SelectedItem)

        Call DrawGear()

        'added to initiate Joint Modeling by YGC 080612 
        chkDowelX.IsEnabled = True
        chkDowelY.IsEnabled = True

        chkBoundaryX.IsEnabled = True
        chkBoundaryY.IsEnabled = True

        chkDowelX.IsChecked = True
        chkDowelY.IsChecked = True

        cmbDowelDiameterX.IsEnabled = True
        cmbDowelSpacingX.IsEnabled = True
        cmbJointOpeningX.IsEnabled = True
        optFreshX.IsEnabled = True
        optDrilledX.IsEnabled = True
        txtJntStfX.IsEnabled = True

        cmbDowelDiameterY.IsEnabled = True
        cmbDowelSpacingY.IsEnabled = True
        cmbJointOpeningY.IsEnabled = True
        optFreshY.IsEnabled = True
        optDrilledY.IsEnabled = True
        txtJntStfY.IsEnabled = True


        chkBoundaryX.IsChecked = True
        chkBoundaryY.IsChecked = True

        txtEdgStfX.IsEnabled = True
        txtEdgStfY.IsEnabled = True
    End Sub

    Private Sub cmbAngle_TextChanged(sender As Object, e As TextChangedEventArgs)
        If cmbAngle.Text = "" Or cmbAngle.Text = "." Then
            GearAngle = 0.0#
        Else
            GearAngle = CSng(cmbAngle.Text)
        End If


        If GearAngle = 0.0# Then GearParallel = True
        If GearAngle <> 0.0# Then GearParallel = False
        If GearAngle >= 0.0# And GearAngle <= 90.0# Then Call DrawGear()

        If NWheels = 0 Then Exit Sub

        Dim checkedRadioButton As New RadioButton
        checkedRadioButton.IsChecked = True

        If optInterior.IsChecked Then
            Call optInterior_Checked(checkedRadioButton, Nothing)
        End If

        'If Not optInterior.IsChecked Then
        If optEdge.IsChecked Then
            Call optEdge_Checked(checkedRadioButton, Nothing)
        End If

    End Sub
    Private Sub cmbAngle_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles cmbAngle.SelectionChanged
        'If cmbAngle.SelectedItem = "" Or cmbAngle.SelectedItem = "." Then
        '    GearAngle = 0.0#
        'Else
        '    GearAngle = CSng(cmbAngle.SelectedItem)
        'End If


        'If GearAngle = 0.0# Then GearParallel = True
        'If GearAngle <> 0.0# Then GearParallel = False
        'If GearAngle >= 0.0# And GearAngle <= 90.0# Then Call DrawGear()

        'If NWheels = 0 Then Exit Sub

        'Dim checkedRadioButton As New RadioButton
        'checkedRadioButton.IsChecked = True

        'If optInterior.IsChecked Then
        '    Call optInterior_Checked(checkedRadioButton, Nothing)
        'End If

        'If Not optInterior.IsChecked Then
        '    Call optEdge_Checked(checkedRadioButton, Nothing)
        'End If
    End Sub

    Private Sub txt_PreviewKeyDown(sender As Object, e As KeyEventArgs) Handles txtXCoord.PreviewKeyDown, txtYCoord.PreviewKeyDown, txtXCoordMoved.PreviewKeyDown, txtYCoordMoved.PreviewKeyDown,
        txtLETG.PreviewKeyDown, txtThermCoef.PreviewKeyDown, txtJntStfX.PreviewKeyDown, txtJntStfY.PreviewKeyDown, txtEdgStfX.PreviewKeyDown, txtEdgStfY.PreviewKeyDown
        Dim KeyAscii As String = e.Key

        'limit input to numbers, backspace(2), delete(32),minus(143), and decimal(144/88)
        e.Handled = Not ("D1D2D3D4D5D6D7D8D9D0NumPad1NumPad2NumPad3NumPad4NumPad5NumPad6NumPad7NumPad8NumPad9NumPad0".Contains(e.Key.ToString())) AndAlso KeyAscii <> 2 AndAlso KeyAscii <> 32 AndAlso KeyAscii <> 143 AndAlso KeyAscii <> 144 AndAlso KeyAscii <> 88

        'no second dot
        If sender.Text.Contains(".") Then
            If KeyAscii = 144 Or KeyAscii = 88 Then
                e.Handled = True
            End If
        End If

        'no second minus
        If sender.Text.Contains("-") Then
            If KeyAscii = 143 Then
                e.Handled = True
            End If
        End If

        'If Not IsNumeric(sender.Text) Then
        '    MessageBox.Show("Please input numeric value")
        'End If

        If txtXCoord.IsFocused Or txtYCoord.IsFocused Then
            optEdge.IsChecked = False
            optInterior.IsChecked = False
        End If

        'If KeyAscii = Key.Enter Then

        '    If IsNumeric(sender.text) Then
        '        XCoord = CSng(Application.win.txtXCoord.Text)
        '        YCoord = CSng(Application.win.txtYCoord.Text)
        '        XCoordMoved = CSng(Application.win.txtXCoordMoved.Text)
        '        YCoordMoved = CSng(Application.win.txtYCoordMoved.Text)
        '        VarAsg_fromControls()
        '        Call DrawGear()
        '    Else '"-" or "." only
        '        MessageBox.Show("Please input numeric value")
        '    End If

        'End If

    End Sub

    Private Sub txtXCoord_MouseDown(sender As Object, e As MouseButtonEventArgs) Handles txtXCoord.MouseDown

        If IsNumeric(txtXCoord.Text) Then
            MessageBox.Show("Please input numeric value")
            Exit Sub
        End If

        If IsNumeric(cmbAngle.Text) Then
            MessageBox.Show("Please input numeric value")
            Exit Sub
        End If

        XCoord = CSng(txtXCoord.Text)
        GearAngle = CSng(cmbAngle.Text)

        If GearAngle = 0.0# Then GearParallel = True
        If GearAngle <> 0.0# Then GearParallel = False
        Call DrawGear()

    End Sub

    Private Sub txt_LostFocus(sender As Object, e As RoutedEventArgs) Handles txtXCoord.LostFocus, txtYCoord.LostFocus, txtXCoordMoved.LostFocus, txtYCoordMoved.LostFocus,
        txtLETG.LostFocus, txtThermCoef.LostFocus, txtJntStfX.LostFocus, txtJntStfY.LostFocus, txtEdgStfX.LostFocus, txtEdgStfY.LostFocus

        'XCoord = CSng(txtXCoord.Text)
        'If Math.Abs(XCoord) > 0.001 Then LogicalXOffset = True
        'If Math.Abs(XCoord) <= 0.001 Then LogicalXOffset = False

        'Call VarAsg_fromControls()
        'Call DrawGear()

        Call TextInputCheck()

        If ErrorInput Then
            Exit Sub
        End If


        XCoord = CSng(Application.win.txtXCoord.Text)
        YCoord = CSng(Application.win.txtYCoord.Text)
        XCoordMoved = CSng(Application.win.txtXCoordMoved.Text)
        YCoordMoved = CSng(Application.win.txtYCoordMoved.Text)
        VarAsg_fromControls()
        Call DrawGear()

    End Sub

    Private Sub txtYCoord_MouseDown(sender As Object, e As MouseButtonEventArgs) Handles txtYCoord.MouseDown

        If IsNumeric(txtYCoord.Text) Then
            MessageBox.Show("Please input numeric value")
            Exit Sub
        End If

        If IsNumeric(cmbAngle.Text) Then
            MessageBox.Show("Please input numeric value")
            Exit Sub
        End If

        YCoord = CSng(txtYCoord.Text)
        GearAngle = CSng(cmbAngle.Text)
        If GearAngle = 0.0# Then GearParallel = True
        If GearAngle <> 0.0# Then GearParallel = False
        Call DrawGear()
    End Sub

    'Private Sub txtYCoord_LostFocus(sender As Object, e As RoutedEventArgs) Handles txtYCoord.LostFocus
    '    YCoord = CSng(txtYCoord.Text)
    '    If Math.Abs(YCoord) > 0.001 Then LogicalYOffset = True
    '    If Math.Abs(YCoord) <= 0.001 Then LogicalYOffset = False

    '    Call VarAsg_fromControls()
    '    Call DrawGear()
    'End Sub


    Private Sub cmb_PreviewKeyDown(sender As Object, e As KeyEventArgs) Handles cmbDimX.PreviewKeyDown, cmbDimY.PreviewKeyDown, cmbMesh.PreviewKeyDown, cmbFdMesh.PreviewKeyDown, cmbAngle.PreviewKeyDown,
        cmbDowelDiameterX.PreviewKeyDown, cmbDowelSpacingX.PreviewKeyDown, cmbJointOpeningX.PreviewKeyDown, cmbDowelDiameterY.PreviewKeyDown, cmbDowelSpacingY.PreviewKeyDown, cmbJointOpeningY.PreviewKeyDown
        Dim KeyAscii As String = e.Key

        'limit input to numbers, backspace(2), delete(32), decimal (144/88)
        e.Handled = Not ("D1D2D3D4D5D6D7D8D9D0NumPad1NumPad2NumPad3NumPad4NumPad5NumPad6NumPad7NumPad8NumPad9NumPad0".Contains(e.Key.ToString())) AndAlso KeyAscii <> 2 AndAlso KeyAscii <> 32 AndAlso KeyAscii <> 144 AndAlso KeyAscii <> 88

        'no second dot
        If sender.Text.Contains(".") Then
            If KeyAscii = 144 Or KeyAscii = 88 Then
                e.Handled = True
            End If
        End If

    End Sub

    Private Sub ChkOverlay_Checked(sender As Object, e As RoutedEventArgs) Handles ChkOverlay.Checked, ChkOverlay.Unchecked
        If Me.IsInitializing = True Then
            Exit Sub
        End If


        Dim overlayRow As DataRow
        overlayRow = grdLayerTable.NewRow


        If ChkOverlay.IsChecked Then
            Overlay = True

            If NumberOfLayers = 6 Then
                grdLayerTable.Rows.RemoveAt(NumberOfLayers - 2)
                NumberOfLayers = NumberOfLayers - 1
            End If


            If NumberOfLayers < 6 Then
                NumberOfLayers = NumberOfLayers + 1
            End If

            grdLayerTable.Rows.InsertAt(overlayRow, 0)

            grdLayerTable(0)(0) = "PCC Overlay"
            grdLayerTable(0)(1) = Format(EMod(0), "#,###,##0")
            grdLayerTable(0)(2) = Format(PoissonsRatio(0), "0.00")
            grdLayerTable(0)(3) = Format(LayerThickness(0), "0.00")

            grdLayerTable(1)(0) = "PCC Base Slab"
        Else
            Overlay = False '
            'If NumberOfLayers > 4 Then NumberOfLayers = NumberOfLayers - 1 '
            NumberOfLayers = NumberOfLayers - 1 '

            grdLayerTable.Rows.RemoveAt(0)
            'NumberOfLayers = NumberOfLayers - 1
            grdLayerTable(0)(0) = "PCC Slab"
        End If


        Call ELPGrid_PavementStructure()
    End Sub

    Private Sub chkTempLoad_Checked(sender As Object, e As RoutedEventArgs) Handles chkTempLoad.Checked, chkTempLoad.Unchecked
        If chkTempLoad.IsChecked Then

            'for INGRID 120413 by YC 111314
            'lblSurfTemp.IsEnabled = True
            'lblBotmTemp.IsEnabled = True
            lblLETG.IsEnabled = True
            'for INGRID 120413 by YC 111314 END

            lblThermCoef.IsEnabled = True
            lblCurlShape.IsEnabled = True

            'for INGRID 120413 by YC 111314
            'txtSurfTemp.IsEnabled = True
            'txtBotmTemp.IsEnabled = True
            txtLETG.IsEnabled = True
            'for INGRID 120413 by YC 111314 END

            txtThermCoef.IsEnabled = True
            cmbCurlShape.IsEnabled = True
        Else

            'for INGRID 120413 by YC 111314
            'lblSurfTemp.IsEnabled = False
            'lblBotmTemp.IsEnabled = False
            lblLETG.IsEnabled = False
            'for INGRID 120413 by YC 111314 END

            lblThermCoef.IsEnabled = False
            lblCurlShape.IsEnabled = False

            'for INGRID 120413 by YC 111314
            'txtSurfTemp.IsEnabled = False
            'txtBotmTemp.IsEnabled = False
            'for INGRID 120413 by YC 111314 END

            txtLETG.IsEnabled = False
            txtThermCoef.IsEnabled = False
            cmbCurlShape.IsEnabled = False
        End If
    End Sub

    Private Sub optSubgrade_Checked(sender As Object, e As RoutedEventArgs) Handles optSubgrade.Checked
        If sender.IsChecked Then
            If optSubgrade.IsChecked Then
                E4 = 26.0# * KSubgrade ^ 1.284
                Winkler = False
                InfiniteElement = True
            End If
        End If
    End Sub

    Private Sub optWinkler_Checked(sender As Object, e As RoutedEventArgs) Handles optWinkler.Checked
        If sender.IsChecked Then
            If optWinkler.IsChecked Then
                KSubgrade = (E4 / 26.0#) ^ 0.778816199
                Winkler = True
                InfiniteElement = False
            End If
        End If
    End Sub

    Private Sub OptHex_Checked(sender As Object, e As RoutedEventArgs) Handles OptHex.Checked
        If sender.IsChecked Then
            Modes = True
            ChkOverlay.IsEnabled = True
        End If
    End Sub

    Private Sub OptShell_Checked(sender As Object, e As RoutedEventArgs) Handles OptShell.Checked
        If sender.IsChecked Then
            If ChkOverlay.IsChecked Then
                ChkOverlay.IsChecked = False
                If NumberOfLayers > 4 Then NumberOfLayers = NumberOfLayers - 1
                Call ELPGrid_PavementStructure()
            End If
            Modes = False
            ChkOverlay.IsEnabled = False
            If Interior = False Then
                lblYOffset.IsEnabled = True
                txtYCoord.IsEnabled = True
            End If
        End If
    End Sub

    Private Sub chkSlabWt_Checked(sender As Object, e As RoutedEventArgs) Handles chkSlabWt.Checked, chkSlabWt.Unchecked
        If chkSlabWt.IsChecked Then SelfWeight = True
        If Not chkSlabWt.IsChecked Then SelfWeight = False
    End Sub

    Private Sub btnEleChk_Click(sender As Object, e As RoutedEventArgs) Handles btnEleChk.Click
        'added to check the total element number to avoid crach by YGC 080612

        Dim NEle, NFdEle As Integer
        Dim NTotEle As Integer

        btnEleChk.IsEnabled = False

        NEle = CInt(cmbMesh.Text)
        NFdEle = CInt(cmbFdMesh.Text)

        If Overlay = True Then
            NTotEle = NEle * NEle * NSlabs * 2 + NFdEle * NFdEle * (NumberOfLayers)
        Else
            NTotEle = NEle * NEle * NSlabs + NFdEle * NFdEle * (NumberOfLayers + 1)
        End If

        If NTotEle >= 100000 Then
            MsgBox("Too many element number. Please reduce it!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        Else
            MsgBox("Element number acceptable!", vbDefaultButton2, "OK!")
            TabPage3.IsEnabled = True
            TabPage4.IsEnabled = True
        End If


        btnEleChk.IsEnabled = True
    End Sub

    Private Sub btnPositionChk_Click(sender As Object, e As RoutedEventArgs) Handles btnPositionChk.Click
        btnPositionChk.IsEnabled = False

        XCoord = CSng(txtXCoord.Text)
        YCoord = CSng(txtYCoord.Text)

        GearAngle = CSng(cmbAngle.Text)
        If GearAngle = 0.0# Then GearParallel = True
        If GearAngle <> 0.0# Then GearParallel = False
        PositionChk = True


        'Moved from sub DrawGear by YGC 080612
        If PointXmax - PointXmin > SlabsRight - SlabsLeft Or PointYmax - PointYmin > SlabsBottom - SlabsTop Then
            MsgBox("Tire position out of slab domain, Please change the aircraft or increase slab number or slab dimension!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        ElseIf PointXmin - SlabsEdgeY(1) < -0.01 And PointYmin - SlabsEdgeX(1) < -0.01 Then
            MsgBox("Tire position out of slab domain, Please increase X-offset but decrease Y-offset!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        ElseIf PointXmin - SlabsEdgeY(1) < -0.01 And PointYmax - SlabsEdgeX(NSlabsY + 1) > 0.01 Then
            MsgBox("Tire position out of slab domain, Please increase X-offset and Y-offset!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        ElseIf PointXmax - SlabsEdgeY(NSlabsX + 1) > 0.01 And PointYmin - SlabsEdgeX(1) < -0.01 Then
            MsgBox("Tire position out of slab domain, Please decrease X-offset and Y-offset!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        ElseIf PointXmax - SlabsEdgeY(NSlabsX + 1) > 0.01 And PointYmax - SlabsEdgeX(NSlabsY + 1) > 0.01 Then
            MsgBox("Tire position out of slab domain, Please decrease X-offset but increase Y-offset!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        ElseIf PointXmin - SlabsEdgeY(1) < -0.01 Then
            MsgBox("Tire position out of slab domain, Please increase X-offset!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        ElseIf PointXmax - SlabsEdgeY(NSlabsX + 1) > 0.01 Then
            MsgBox("Tire position out of slab domain, Please decrease X-offset!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        ElseIf PointYmin - SlabsEdgeX(1) < -0.01 Then
            MsgBox("Tire position out of slab domain, Please decrease Y-offset!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        ElseIf PointYmax - SlabsEdgeX(NSlabsY + 1) > 0.01 Then
            MsgBox("Tire position out of slab domain, Please increase Y-offset!", vbCritical, "Warning!")
            TabPage3.IsEnabled = False
            TabPage4.IsEnabled = False
        Else
            MsgBox("Tire position acceptable!", vbDefaultButton2, "OK!")
            TabPage3.IsEnabled = True
            TabPage4.IsEnabled = True
        End If

        'Call DrawGear()
        'Move ended from sub DrawGear by YGC 080612

        PositionChk = False

        btnPositionChk.IsEnabled = True
    End Sub

    Private Sub chkDowelX_Checked(sender As Object, e As RoutedEventArgs) Handles chkDowelX.Checked, chkDowelX.Unchecked
        If chkDowelX.IsChecked Then

            lblDowelDiameterX.IsEnabled = True
            lblDowelSpacingX.IsEnabled = True
            lblJointOpeningX.IsEnabled = True

            cmbDowelDiameterX.IsEnabled = True
            cmbDowelSpacingX.IsEnabled = True
            cmbJointOpeningX.IsEnabled = True
            optFreshX.IsEnabled = True
            optDrilledX.IsEnabled = True
            txtJntStfX.IsEnabled = True
        Else

            lblDowelDiameterX.IsEnabled = False
            lblDowelSpacingX.IsEnabled = False
            lblJointOpeningX.IsEnabled = False

            cmbDowelDiameterX.IsEnabled = False
            cmbDowelSpacingX.IsEnabled = False
            cmbJointOpeningX.IsEnabled = False
            optFreshX.IsEnabled = False
            optDrilledX.IsEnabled = False
            txtJntStfX.IsEnabled = False
        End If
        Call DrawJoint()
    End Sub

    Private Sub chkDowelY_Checked(sender As Object, e As RoutedEventArgs) Handles chkDowelY.Checked, chkDowelY.Unchecked
        If chkDowelY.IsChecked Then

            lblDowelDiameterY.IsEnabled = True
            lblDowelSpacingY.IsEnabled = True
            lblJointOpeningY.IsEnabled = True

            cmbDowelDiameterY.IsEnabled = True
            cmbDowelSpacingY.IsEnabled = True
            cmbJointOpeningY.IsEnabled = True
            optFreshY.IsEnabled = True
            optDrilledY.IsEnabled = True
            txtJntStfY.IsEnabled = True
        Else

            lblDowelDiameterY.IsEnabled = False
            lblDowelSpacingY.IsEnabled = False
            lblJointOpeningY.IsEnabled = False

            cmbDowelDiameterY.IsEnabled = False
            cmbDowelSpacingY.IsEnabled = False
            cmbJointOpeningY.IsEnabled = False
            optFreshY.IsEnabled = False
            optDrilledY.IsEnabled = False
            txtJntStfY.IsEnabled = False
        End If

        Call DrawJoint()
    End Sub

    'VK: Event was changed to ComboBoxX_TextChanged to simulate the same behavior (WPF Conversion)
    Private Sub cmbDowelDiameterX_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles cmbDowelDiameterX.SelectionChanged, cmbDowelSpacingX.SelectionChanged, cmbJointOpeningX.SelectionChanged
        If Me.IsInitializing = True Then
            Exit Sub
        End If

        If sender.SelectedItem <> "" And sender.SelectedItem <> "." Then
            Call ComputeKJointX()
        End If
    End Sub
    Private Sub ComboBoxX_TextChanged(sender As Object, e As TextChangedEventArgs)
        If Me.IsInitializing = True Or Me.m_IsInitializing_Tab3 Then
            Exit Sub
        End If

        If sender.Text <> "" And sender.Text <> "." Then
            Call ComputeKJointX()
        End If
    End Sub
    Sub ComputeKJointX()
        Dim EDowel, ModulusOfDowelSupport, PRDowel As Double
        Dim DowelDiameter, DowelSpacing, JointOpening As Double
        Dim Beta, IDowel, DowelShearModulus As Double
        Dim TwelveCee, Phi, AsubZ, DCI, Kjoint As Double
        If optDrilledX.IsChecked Then
            ModulusOfDowelSupport = 5270000
        Else
            ModulusOfDowelSupport = 8290000
            '    ModulusOfDowelSupport = 1500000 'FAA-RD-79-4 Grinter Estimation ??? Lia
        End If
        EDowel = 29000000 'youngs modulus for mild steel
        PRDowel = 0.3 'Poissons ratio for mild steel
        DowelSpacing = CSng(cmbDowelSpacingX.Text)
        DowelDiameter = CSng(cmbDowelDiameterX.Text)
        JointOpening = CSng(cmbJointOpeningX.Text)
        IDowel = Math.PI * (DowelDiameter / 2.0#) ^ 4 / 4.0#
        Beta = ((ModulusOfDowelSupport * DowelDiameter) / (4 * EDowel * IDowel)) ^ 0.25
        DowelShearModulus = EDowel / (2.0# * (1 + PRDowel))
        AsubZ = 0.9 * (Math.PI * DowelDiameter ^ 2 / 4)
        Phi = (12.0# * EDowel * IDowel) / (DowelShearModulus * AsubZ * (JointOpening) ^ 2)
        DCI = (4.0# * Beta ^ 3 * EDowel * IDowel) / (2.0# + Beta * JointOpening)
        TwelveCee = (12.0# * EDowel * IDowel) / (JointOpening ^ 3 * (1.0# + Phi))
        Kjoint = (1.0# / DowelSpacing) * (1.0# / ((2.0# / DCI) + (1.0# / TwelveCee)))
        'KJoint = (1# / DowelSpacing) * (4# * Beta ^ 3 * EDowel * IDowel) / (2# + Beta * JointOpening + (1# / 3#) * Beta ^ 3 * JointOpening ^ 3 * (1# + Phi))
        'lblEJS.Caption = CStr(Kjoint) & " psi"
        txtJntStfX.Text = CStr(Kjoint)
        EqJStfX = Kjoint
    End Sub

    'VK: Event was changed to ComboBoxX_TextChanged to simulate the same behavior (WPF Conversion)
    Private Sub cmbDowelDiameterY_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles cmbDowelDiameterY.SelectionChanged, cmbDowelSpacingY.SelectionChanged, cmbJointOpeningY.SelectionChanged
        If Me.IsInitializing = True Then
            Exit Sub
        End If

        If sender.SelectedItem <> "" And sender.SelectedItem <> "." Then
            Call ComputeKJointY()
        End If
    End Sub

    Private Sub ComboBoxY_TextChanged(sender As Object, e As TextChangedEventArgs)
        If Me.IsInitializing = True Or m_IsInitializing_Tab3 Then
            Exit Sub
        End If

        'If Not (IsNothing(sender.SelectedItem)) And sender.Text <> "" And sender.Text <> "." Then
        If sender.Text <> "" And sender.Text <> "." Then
            Call ComputeKJointY()
        End If
    End Sub
    Sub ComputeKJointY()
        Dim ModulusOfDowelSupport As Double, EDowel As Double, PRDowel As Double
        Dim DowelSpacing As Double, DowelDiameter As Double, JointOpening As Double
        Dim IDowel As Double, Beta As Double, DowelShearModulus As Double
        Dim AsubZ As Double, Phi As Double, DCI As Double, TwelveCee As Double, Kjoint As Double
        If optDrilledY.IsChecked Then
            ModulusOfDowelSupport = 5270000
        Else
            ModulusOfDowelSupport = 8290000
            '    ModulusOfDowelSupport = 1500000 'FAA-RD-79-4 Grinter Estimation ??? Lia
        End If
        EDowel = 29000000 'youngs modulus for mild steel
        PRDowel = 0.3 'Poissons ratio for mild steel
        DowelSpacing = CSng(cmbDowelSpacingY.Text)
        DowelDiameter = CSng(cmbDowelDiameterY.Text)
        JointOpening = CSng(cmbJointOpeningY.Text)
        IDowel = Math.PI * (DowelDiameter / 2.0#) ^ 4 / 4.0#
        Beta = ((ModulusOfDowelSupport * DowelDiameter) / (4 * EDowel * IDowel)) ^ 0.25
        DowelShearModulus = EDowel / (2.0# * (1 + PRDowel))
        AsubZ = 0.9 * (Math.PI * DowelDiameter ^ 2 / 4)
        Phi = (12.0# * EDowel * IDowel) / (DowelShearModulus * AsubZ * (JointOpening) ^ 2)
        DCI = (4.0# * Beta ^ 3 * EDowel * IDowel) / (2.0# + Beta * JointOpening)
        TwelveCee = (12.0# * EDowel * IDowel) / (JointOpening ^ 3 * (1.0# + Phi))
        Kjoint = (1.0# / DowelSpacing) * (1.0# / ((2.0# / DCI) + (1.0# / TwelveCee)))
        'KJoint = (1# / DowelSpacing) * (4# * Beta ^ 3 * EDowel * IDowel) / (2# + Beta * JointOpening + (1# / 3#) * Beta ^ 3 * JointOpening ^ 3 * (1# + Phi))
        'lblEJS.Caption = CStr(Kjoint) & " psi"
        txtJntStfY.Text = CStr(Kjoint)
        EqJStfY = Kjoint

    End Sub

    'Private Sub txtJntStfX_LostFocus(sender As Object, e As RoutedEventArgs) Handles txtJntStfX.LostFocus, txtJntStfY.LostFocus
    '    If CSng(sender.Text) < 1000 Then sender.Text = CStr(1000)

    '    Call VarAsg_fromControls()
    '    Call DrawJoint()
    'End Sub

    Private Sub optFreshX_Checked(sender As Object, e As RoutedEventArgs) Handles optFreshX.Checked, optDrilledX.Checked
        Call ComputeKJointX()
    End Sub

    Private Sub optFreshY_Checked(sender As Object, e As RoutedEventArgs) Handles optFreshY.Checked, optDrilledY.Checked
        Call ComputeKJointY()
    End Sub

    Private Sub btnRestore_Click(sender As Object, e As RoutedEventArgs) Handles btnRestore.Click
        cmbDowelDiameterX.Text = "1.0"
        cmbDowelSpacingX.Text = "15"
        cmbJointOpeningX.Text = "0.375"
        optFreshX.IsChecked = True
        EqJStfX = 100000
        'lblEJS.Caption = "100000 psi"
        txtJntStfX.Text = "100000"
        txtEdgStfX.Text = "1000"   ' added for Boundary(Edge) spring by YGC 110110

        '' added for longitudinal joint by YGC 052810
        cmbDowelDiameterY.SelectedItem = "1.0"
        cmbDowelSpacingY.SelectedItem = "15"
        cmbJointOpeningY.SelectedItem = "0.375"
        optFreshX.IsChecked = True
        EqJStfY = 100000
        txtJntStfY.Text = "100000"
        txtEdgStfX.Text = "1000"  ' added for Boundary(Edge) spring by YGC 110110
        '' end add by YGC 052810
    End Sub

    Private Sub chkBoundaryX_Checked(sender As Object, e As RoutedEventArgs) Handles chkBoundaryX.Checked, chkBoundaryX.Unchecked
        If chkBoundaryX.IsChecked Then
            txtEdgStfX.IsEnabled = True
        Else
            txtEdgStfX.IsEnabled = False
        End If
        Call DrawJoint()
    End Sub

    Private Sub chkBoundaryY_Checked(sender As Object, e As RoutedEventArgs) Handles chkBoundaryY.Checked, chkBoundaryY.Unchecked
        If chkBoundaryY.IsChecked Then
            txtEdgStfY.IsEnabled = True
        Else
            txtEdgStfY.IsEnabled = False
        End If
        Call DrawJoint()
    End Sub

    Private Sub TabPage3_Loaded(sender As Object, e As RoutedEventArgs) Handles TabPage3.GotFocus
        m_IsInitializing_Tab3 = False
    End Sub

    Private Sub btnSaveJob_Click(sender As Object, e As RoutedEventArgs) Handles btnSaveJob.Click
        Call WriteJobFileXML()
        Me.Title = MainTitle & "        Job Name: " & JobName
    End Sub

    Private Sub btnRertrieveJob_Click(sender As Object, e As RoutedEventArgs) Handles btnRertrieveJob.Click
        Dim ret1 As Boolean

        Call ReadJobFileXML(ret1)

        If Not ret1 Then Exit Sub

        Me.Title = MainTitle & "        Job Name: " & JobName
        lblInfo.Content = ""
        'btnExit.BackColor = cmdSaveJob.BackColor

        btnMesh.IsEnabled = True : btnMesh.Background = btnSaveJob.Background
        btnAnalysis.IsEnabled = True : btnAnalysis.Background = btnSaveJob.Background
        btnNIKEPLOT.IsEnabled = True : btnNIKEPLOT.Background = btnSaveJob.Background
        btnMesh.Focus()
    End Sub

    Private Sub btnMesh_Click(sender As Object, e As RoutedEventArgs) Handles btnMesh.Click
        Dim FileExistsCheck As Boolean, sFile As String

        'sFile = My.Application.Info.DirectoryPath & "\" & txtFilNm.Text         ' YC 040620-1
        sFile = gJobFileFolder & "\" & JobName

        sFile = sFile & ".FEA.xml"  'v3.0 002 YC 052620

        FileExistsCheck = System.IO.File.Exists(sFile)

        'If FileExistsCheck Then       ' YC 040620-1

        Try
            Dim EFNo As Integer
            EFNo = FreeFile()
            FileOpen(EFNo, sFile, OpenMode.Input, , , 1024)
            FileClose(EFNo)
        Catch ex As Exception
            Dim msg1 As String

            'msg1 = "Access to nikein file is denied. " & Environment.NewLine & "Wait till INGRID or NIKE3D finish running."

            'msg1 = "Wait till job saved."   ' v3.0 002 YC 052620
            msg1 = "Save job before creating mesh."

            MsgBox(msg1, MsgBoxStyle.OkOnly, "'Create Mesh' button clicked.")
            Exit Sub
        End Try

        'End If    ' YC 040620-1

        Call InputCheck()  'added to check the input by YGC 080612

        If InputInfoChk = False Then : Exit Sub

        End If
        btnExitT4.Focus()
        btnRertrieveJob.IsEnabled = False : btnSaveJob.IsEnabled = False
        btnMesh.IsEnabled = False : btnAnalysis.IsEnabled = False : btnNIKEPLOT.IsEnabled = False : btnProcessStress.IsEnabled = False

        btnMesh.Background = Brushes.Aqua 'DirectCast(New BrushConverter().ConvertFrom("#80fffb"), SolidColorBrush) ' New SolidColorBrush(bc.ConvertFrom(&HFFFF80))
        btnAnalysis.Background = btnSaveJob.Background
        btnNIKEPLOT.Background = btnSaveJob.Background
        btnProcessStress.Background = btnSaveJob.Background
        btnExitT4.Background = btnSaveJob.Background
        btnExitT1.Background = btnSaveJob.Background
        btnExitT2.Background = btnSaveJob.Background
        btnExitT3.Background = btnSaveJob.Background

        Call VarAsg_fromControls()
        Call VarInitControls()

        'InteriorLoad = False
        InfiniteElement = True

        XScaleFactor = CSng(cmbDimX.Text) / 25.0#
        YScaleFactor = CSng(cmbDimY.Text) / 25.0#

        'assign structural layer properties
        For I = 0 To 6
            modAutoMesh.EMod(I) = EMod(I)
            modAutoMesh.PoissonsRatio(I) = PoissonsRatio(I)
            modAutoMesh.LayerThickness(I) = LayerThickness(I)
        Next I

        'FileNameString = Dir1.List(Dir1.ListIndex) & "\" & txtFilNm.Text


        'Call AMMain(Dir1.SelectedPath & "\" & txtFilNm.Text, InteriorLoad)  ' YC 040620-1
        Call AMMain(gJobFileFolder & "\" & txtFilNm.Text, InteriorLoad)


        If modAutoMesh.SymmErr = 1 Then GoTo 999
        If modAutoMesh.MeshErr = 1 Then MsgBox("Slab Too Small - Increase y-Dimension.") : Exit Sub
        If modAutoMesh.FileErr = 1 Then Exit Sub

        lblInfo.Content = "Mesh is being created."
        System.Windows.Forms.Application.DoEvents()
        System.Windows.Forms.Application.DoEvents()


        'Call IngridCall(Dir1.SelectedPath & "\" & txtFilNm.Text)   ' YC 040620
        Call FAAMESHCall()


startAgain:
        Dim do1 As Boolean
        Dim Ubound1, counter1 As Integer

        Do
            do1 = True
            Dim processes() As Process
            Dim instance As Process
            Dim process As New Process()
            processes = Process.GetProcesses

            do1 = False

            Ubound1 = UBound(processes)
            counter1 = 0

            For Each instance In processes
                counter1 = counter1 + 1
                If instance.ProcessName = "Ingrid3" Then
                    do1 = True
                    lblInfo.Content = "Mesh is being created."
                    System.Windows.Forms.Application.DoEvents()
                    System.Threading.Thread.Sleep(2000)
                    GoTo startAgain
                End If
                If counter1 > Ubound1 Then
                    Exit Do
                End If

            Next
            System.Windows.Forms.Application.DoEvents()
        Loop While do1 Or Not exitLoop Or counter1 > Ubound1

        btnMesh.Background = btnSaveJob.Background
        lblInfo.Content = "Mesh was created."

        btnAnalysis.Background = Brushes.Aqua 'New SolidColorBrush(bc.ConvertFrom(&HFFFF80))
        btnAnalysis.Focus()

        btnMesh.IsEnabled = True : btnAnalysis.IsEnabled = True : btnNIKEPLOT.IsEnabled = True : btnProcessStress.IsEnabled = True

999:
        btnRertrieveJob.IsEnabled = True : btnSaveJob.IsEnabled = True
    End Sub

    Sub GearLoads(ByRef p1 As par1)


        Dim NoXEls As Integer, NoXElsEven As Boolean
        Dim NoYEls As Integer, NoYElsEven As Boolean, NumberYDivisions As Integer, NumberYDivisionsEven As Boolean
        Dim NumberXDivisions As Integer, NumberXDivisionsEven As Boolean


        NoXElsEven = False
        NoYElsEven = False

        If GearParallel = True Then 'if gear oriented parallel to joint

            Xfp = WhlWidth
            Yfp = WhlLength
            XtrMin = 0.0#
            XtrMax = 0.0#
            YtrMax = 0.0#
            YtrMin = 0.0#
            For I = 1 To NWheels
                Xtr(I) = YWheels(I) - Ycg
                Ytr(I) = XWheels(I) - Xcg
                If Xtr(I) > XtrMax Then XtrMax = Xtr(I)
                If Xtr(I) < XtrMin Then XtrMin = Xtr(I)
                If Ytr(I) > YtrMax Then YtrMax = Ytr(I)
                If Ytr(I) < YtrMin Then YtrMin = Ytr(I)
            Next I
            xdim = XtrMax - XtrMin


            ydim = YtrMax - YtrMin

            If modWorld.NYSymmetric = NWheels Then SymmSngl = True

        ElseIf GearAngle = 90.0# Then 'If gear oriented perpendicular to joint

            Yfp = WhlWidth
            Xfp = WhlLength
            XtrMin = 0.0#
            XtrMax = 0.0#
            YtrMax = 0.0#
            YtrMin = 0.0#
            For I = 1 To NWheels
                Xtr(I) = XWheels(I) - Xcg
                Ytr(I) = YWheels(I) - Ycg

                Ytr(I) = -Ytr(I)    'corrected to consistent with angle 89.9 YC 072516 070516

                If Xtr(I) > XtrMax Then XtrMax = Xtr(I)
                If Xtr(I) < XtrMin Then XtrMin = Xtr(I)
                If Ytr(I) > YtrMax Then YtrMax = Ytr(I)
                If Ytr(I) < YtrMin Then YtrMin = Ytr(I)
            Next I
            xdim = XtrMax - XtrMin


            'If Interior = True Then 'for interior case only
            '    xdim = XtrMax
            '    If modWorld.NYSymmetric = NWheels And modWorld.NXSymmetric = NWheels Then SymmDbl = True
            '    If SymmDbl = False Then GoTo 999
            'End If

            ydim = YtrMax - YtrMin


            'If Interior = False Then 'for edge case only
            If modWorld.NXSymmetric = NWheels Then SymmSngl = True
            'For I = 1 To NWheels
            '    Xtr(I) = Xtr(I) - XtrMin + Xfp / 2#
            'Next I
            'End If
        Else
            SymmSngl = False
            SymmDbl = False
        End If

        If Not ChkAllowSymmetry.IsChecked Then
            SymmSngl = False
            SymmDbl = False
        End If

        'reposition the gear at the given location along the longitudinal (y) axis
        If GearAngle = 0.0# Or GearAngle = 90.0# Then
            For I = 1 To NWheels
                Ytr(I) = Ytr(I) - YCoord
            Next I
            If LogicalYOffset = True Then SymmSngl = False
            'reposition the gear at the given location along the transverse (y) axis
            For I = 1 To NWheels
                Xtr(I) = Xtr(I) + XCoord
            Next I
        End If
        'If  LogicalXOffset = True Then SymmSngl = False


        'set the mesh control parameters

        p1.c = (xdim + 30.0#) / XScaleFactor
40:     NoXEls = p1.c / Delta1X
        'check if NoXEls an even number

100:    If (-1) ^ NoXEls > 0 Then
            NoXElsEven = True
        Else
            NoXElsEven = False
        End If

        If NoXElsEven = True Then
            p1.c = (NoXEls + 2) * Delta1X
        Else
            p1.c = (NoXEls + 3) * Delta1X
        End If
        'If Interior = True And c = 150 Then
        '    NoXEls = NoXEls + 1
        '    GoTo 100
        'End If
        p1.d = CInt((NoXEls + 2) / 3.0#) * Delta0X
        p1.a = (CInt((NoXEls + 2) / 3.0#) + 1) * Delta0X

50:     p1.e = (ydim / 2.0# + 30.0#) / YScaleFactor
        If LogicalYOffset = True Then p1.e = 180.0# 'for repositioning the gear along the longitudinal edge of slab

150:    NoYEls = p1.e / Delta1Y
        If p1.e > 150 Then
            NumberYDivisions = 150.0# / Delta1Y 'this number should be an even integer divisible x 3
            'check if NumberYDivisions is an integer
            If Math.Abs((150.0# / Delta1Y) - NumberYDivisions) < 0.01 Then
            Else
                Delta1Y = 150.0# / NumberYDivisions
                Delta0Y = Delta1Y * 3.0#
                Delta2Y = Delta1Y / 2.0#
                Ygrid = Delta2Y
                GoTo 150
            End If
            'check if NumberYDivisions is even
            If (-1) ^ NumberYDivisions > 0 Then NumberYDivisionsEven = True
            If NumberYDivisionsEven = False Then
                Delta1Y = 150.0# / (NumberYDivisions + 3)
                Delta0Y = Delta1Y * 3.0#
                Delta2Y = Delta1Y / 2.0#
                Ygrid = Delta2Y
                GoTo 150
            End If
            'check if NumberYDivisions is divisible by 3
            If NumberYDivisions / 3 = Int(NumberYDivisions / 3) Then
            Else
                Delta1Y = 150.0# / (NumberYDivisions + 2)
                Delta0Y = Delta1Y * 3.0#
                Delta2Y = Delta1Y / 2.0#
                Ygrid = Delta2Y
                GoTo 150
            End If
        End If
        'check if NoYEls an even number
200:    If (-1) ^ NoYEls > 0 Then
            NoYElsEven = True
        Else
            NoYElsEven = False
        End If


        If NoYElsEven = True Then
            p1.e = (NoYEls + 2) * Delta1Y
        Else
            p1.e = (NoYEls + 3) * Delta1Y
        End If

        p1.f = CInt((NoYEls + 2) / 3.0#) * Delta0Y
        p1.b = (CInt((NoYEls + 2) / 3.0#) + 1) * Delta0Y
        If p1.f = 150.0# Then ydim = ydim + 10.0# : GoTo 50

        If p1.b >= 150 Then
            'check if NumberYDivisions is divisible by 6
            NumberYDivisions = 150.0# / Delta1Y
            If NumberYDivisions / 6 = Int(NumberYDivisions / 6) Then
            Else
                Delta1Y = 150.0# / (NumberYDivisions + 2)
                Delta0Y = Delta1Y * 3.0#
                Delta2Y = Delta1Y / 2.0#
                Ygrid = Delta2Y
                GoTo 150
            End If

            p1.a3 = p1.b + 0.02
            p1.b = 0.0#
            p1.c3 = p1.e + 0.02 : If p1.c3 < 150.0# Then ydim = ydim + 10.0# : GoTo 50
            p1.d3 = p1.f + 0.02
            p1.e = 150.0#
            p1.f = 150.0#
        End If

        If p1.a > 270 Then
            NumberXDivisions = 300.0# / Delta1X
            'check if NumberXDivisions is an integer
            If Math.Abs((300.0# / Delta1X) - NumberXDivisions) < 0.01 Then
            Else
                Delta1X = 150.0# / NumberXDivisions
                Delta0X = Delta1X * 3.0#
                Delta2X = Delta1X / 2.0#
                Xgrid = Delta2X
                GoTo 40
            End If
            'check if NumberXDivisions is even
            If (-1) ^ NumberXDivisions > 0 Then
                NumberXDivisionsEven = True
            Else
                NumberXDivisionsEven = False
            End If


            If NumberXDivisionsEven = False Then
                Delta1X = 300.0# / (NumberXDivisions + 3)
                Delta0X = Delta1X * 3.0#
                Delta2X = Delta1X / 2.0#
                Xgrid = Delta2X
                GoTo 40
            End If
            'check if NumberXDivisions is divisible by 6
            If NumberXDivisions / 6 = Int(NumberXDivisions / 6) Then
            Else
                Delta1X = 300.0# / (NumberXDivisions + 2)
                Delta0X = Delta1X * 3.0#
                Delta2X = Delta1X / 2.0#
                Xgrid = Delta2X
                GoTo 40
            End If

            p1.c7 = 300.02 + (30.0# / XScaleFactor)
            If p1.c + 0.02 > p1.c7 Then p1.c7 = p1.c + 0.02
            NoXEls = (p1.c7 - 300.02) / Delta1X
            'check if NoXEls an even number
            If (-1) ^ NoXEls > 0 Then NoXElsEven = True
            If NoXElsEven = True Then
                p1.c7 = 300.02 + (NoXEls + 2) * Delta1X
            Else
                p1.c7 = 300.02 + (NoXEls + 3) * Delta1X
            End If
            p1.d7 = 300.02 + CInt((NoXEls + 2) / 3.0#) * Delta0X
            p1.a7 = 300.02 + (CInt((NoXEls + 2) / 3.0#) + 1) * Delta0X
            p1.a = 0
        End If

        p1.a1 = 3.0# * Delta0X + 0.02
        p1.c1 = 2.0# * (Delta0X + Delta1X) + 0.02
        p1.d1 = 2.0# * Delta0X + 0.02

999:

    End Sub

    Sub InputCheck()

        InputInfoChk = True

        'added to check the input by YGC 080612

        'added to check the total element number to avoid crach by YGC 080612
        Dim NEle, NFdEle As Integer
        Dim NTotEle As Integer

        NEle = CInt(cmbMesh.Text)
        NFdEle = CInt(cmbFdMesh.Text)


        Dim NTotSlidingNode As Integer  ' v3.0 003 YC


        If Overlay = True Then
            NTotEle = NEle * NEle * NSlabs * 2 + NFdEle * NFdEle * (NumberOfLayers)


            NTotSlidingNode = (NEle + 1) * (NEle + 1) * NSlabs * 3 + (NFdEle + 1) * (NFdEle + 1)    ' v3.0 003 YC
        Else
            NTotEle = NEle * NEle * NSlabs + NFdEle * NFdEle * (NumberOfLayers + 1)


            NTotSlidingNode = (NEle + 1) * (NEle + 1) * NSlabs + (NFdEle + 1) * (NFdEle + 1)    ' v3.0 003 YC
        End If


        'v3.0 003 YC
        If NTotSlidingNode >= 48000 Then
            MsgBox("The number of sliding nodes exceeds 48,000. Please change the mesh.", vbCritical, "Warning!")

            InputInfoChk = False : Exit Sub
        End If
        'v3.0 003 YC END


        'If NTotEle >= 100000 Then
        'If NTotEle >= 17334 Then    'v3.0 002 YC 052620
        If NTotEle >= 48000 Then    'v3.0 003 YC

            'MsgBox("Too many element number. Please reduce it!", vbCritical, "Warning!") 'modified per DRB's comment by YGC 100512
            'MsgBox("The number of elements exceed 100,000. Please change the mesh.", vbCritical, "Warning!")   'v3.0 YC 052620
            'MsgBox("The number of elements exceed 17,334. Please change the mesh.", vbCritical, "Warning!")    'v3.0 003 YC
            MsgBox("The number of elements exceeds 48,000. Please change the mesh.", vbCritical, "Warning!")

            InputInfoChk = False : Exit Sub
        End If
        'add ended to check the total element number to avoid crach by YGC 080612

        'Moved from Sub DrawGear by YGC 080612
        If PointXmax - PointXmin > SlabsRight - SlabsLeft Or PointYmax - PointYmin > SlabsBottom - SlabsTop Then
            MsgBox("Tire position out of slab domain. Please change the aircraft or increase slab number or slab dimension!", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        ElseIf PointXmin - SlabsEdgeY(1) < -0.01 And PointYmin - SlabsEdgeX(1) < -0.01 Then
            MsgBox("Tire position out of slab domain. Please increase X-offset but decrease Y-offset!", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        ElseIf PointXmin - SlabsEdgeY(1) < -0.01 And PointYmax - SlabsEdgeX(NSlabsY + 1) > 0.01 Then
            MsgBox("Tire position out of slab domain. Please increase X-offset and Y-offset!", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        ElseIf PointXmax - SlabsEdgeY(NSlabsX + 1) > 0.01 And PointYmin - SlabsEdgeX(1) < -0.01 Then
            MsgBox("Tire position out of slab domain. Please decrease X-offset and Y-offset!", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        ElseIf PointXmax - SlabsEdgeY(NSlabsX + 1) > 0.01 And PointYmax - SlabsEdgeX(NSlabsY + 1) > 0.01 Then
            MsgBox("Tire position out of slab domain. Please decrease X-offset but increase Y-offset!", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        ElseIf PointXmin - SlabsEdgeY(1) < -0.01 Then
            MsgBox("Tire position out of slab domain. Please increase X-offset!", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        ElseIf PointXmax - SlabsEdgeY(NSlabsX + 1) > 0.01 Then
            MsgBox("Tire position out of slab domain. Please decrease X-offset!", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        ElseIf PointYmin - SlabsEdgeX(1) < -0.01 Then
            MsgBox("Tire position out of slab domain. Please decrease Y-offset!", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        ElseIf PointYmax - SlabsEdgeX(NSlabsY + 1) > 0.01 Then
            MsgBox("Tire position out of slab domain. Please increase Y-offset!", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        End If
        'Move ended from Sub DrawGear by YGC 080612

        'Modified for NSlab=2 by YGC 100512
        'If NSlabs > 1 Then
        '    If chkDowelX.Checked = False Then
        '        MsgBox("Please input X-Direction Dowel Bar Data", vbCritical, "Warning!")
        '        InputInfoChk = False : Exit Sub
        '    End If

        '    If chkDowelY.Checked = False Then
        '        MsgBox("Please input Y-Direction Dowel Bar Data", vbCritical, "Warning!")
        '        InputInfoChk = False : Exit Sub
        '    End If
        'End If

        ' Dowel bar validation - allow unchecked state for mesh generation
        ' If NSlabs > 2 Then
        '     If chkDowelX.IsChecked = False Then
        '         MsgBox("Please input X-Direction Dowel Bar Data", vbCritical, "Warning!")
        '         InputInfoChk = False : Exit Sub
        '     End If
        ' End If

        ' If NSlabs > 1 Then
        '     If chkDowelY.IsChecked = False Then
        '         MsgBox("Please input Y-Direction Dowel Bar Data", vbCritical, "Warning!")
        '         InputInfoChk = False : Exit Sub
        '     End If
        ' End If
        'Modify ended for NSlab=2 by YGC 100512


        If chkBoundaryX.IsChecked = False Then
            MsgBox("Please input X-Direction Spring Constraint", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        End If

        If chkBoundaryY.IsChecked = False Then
            MsgBox("Please input Y-Direction Spring Constraint", vbCritical, "Warning!")
            InputInfoChk = False : Exit Sub
        End If

    End Sub

    Private Sub btnAnalysis_Click(sender As Object, e As RoutedEventArgs) Handles btnAnalysis.Click


        GoTo labelNewProcedure 'YC 040620


        Dim FileExistsCheck As Boolean, sFile As String
        sFile = My.Application.Info.DirectoryPath & "\" & txtFilNm.Text
        FileExistsCheck = System.IO.File.Exists(sFile)

        If FileExistsCheck Then

            Try
                Dim EFNo As Integer
                EFNo = FreeFile()
                FileOpen(EFNo, sFile, OpenMode.Input, , , 1024)
                FileClose(EFNo)
            Catch ex As Exception
                Dim msg1 As String
                msg1 = "Access to nikein file is denied. " & Environment.NewLine & "Wait till INGRID or NIKE3D finish running."
                MsgBox(msg1, MsgBoxStyle.OkOnly, "'Run Analysis' button clicked.")
                Exit Sub
            End Try
        End If


        '=========================================================
        Dim do2 As Boolean
        Dim Ubound2, counter2 As Integer

        Do
            do2 = True
            Dim processes() As Process
            Dim instance As Process
            Dim process As New Process()
            processes = Process.GetProcesses

            do2 = False

            Ubound2 = UBound(processes)
            counter2 = 0

            For Each instance In processes
                counter2 = counter2 + 1
                If UCase(instance.ProcessName) = "NIKE3D" Then
                    do2 = True
                    System.Windows.Forms.Application.DoEvents()
                    MsgBox("Wait till NIKE3D finish running.", MsgBoxStyle.OkOnly, "'Run Analysis' button clicked.")
                    Exit Sub
                ElseIf instance.ProcessName = "NIKEPLOT" Then
                    do2 = True
                    System.Windows.Forms.Application.DoEvents()
                    MsgBox("Wait till NIKEPLOT finish running.", MsgBoxStyle.OkOnly, "'Run Analysis' button clicked.")
                    Exit Sub
                End If
                If counter2 > Ubound2 Then
                    Exit Do
                End If

            Next
            System.Windows.Forms.Application.DoEvents()
        Loop While do2 Or Not exitLoop Or counter2 > Ubound2
        '==========================================================


        btnExitT4.Focus()
        btnRertrieveJob.IsEnabled = False : btnSaveJob.IsEnabled = False
        btnMesh.IsEnabled = False : btnAnalysis.IsEnabled = False : btnNIKEPLOT.IsEnabled = False : btnProcessStress.IsEnabled = False
        btnAnalysis.Background = Brushes.Aqua ' System.Drawing.ColorTranslator.FromOle(&HFFFF80) 'cyan
        btnNIKEPLOT.Background = btnSaveJob.Background
        btnExitT4.Background = btnSaveJob.Background
        btnExitT1.Background = btnSaveJob.Background
        btnExitT2.Background = btnSaveJob.Background
        btnExitT3.Background = btnSaveJob.Background

        Dim fso As New Scripting.FileSystemObject
        Dim fil As Boolean
        Dim RetVal As Object
        'Microsoft Scripting has to be referenced

        System.Windows.Forms.Application.DoEvents()
        'Do
        System.Windows.Forms.Application.DoEvents()
        fil = fso.FileExists(My.Application.Info.DirectoryPath & "\" & "nikein")
        'Loop While Not fil

        If Not fil Then
            Dim msg1 As String
            msg1 = "File nikein is missing." & Environment.NewLine & "Click on button 'Create Mesh.'"
            MsgBox(msg1, MsgBoxStyle.OkOnly, "Message")

            btnRertrieveJob.IsEnabled = True : btnSaveJob.IsEnabled = True
            btnMesh.IsEnabled = True : btnMesh.Background = Brushes.Aqua ' System.Drawing.ColorTranslator.FromOle(&HFFFF80) 'cyan
            btnAnalysis.IsEnabled = False : btnAnalysis.ClearValue(Button.BackgroundProperty) 'System.Drawing.ColorTranslator.FromOle(&H8000000F) 'gray
            btnNIKEPLOT.IsEnabled = False : btnNIKEPLOT.ClearValue(Button.BackgroundProperty) 'System.Drawing.ColorTranslator.FromOle(&H8000000F) 'gray
            btnProcessStress.IsEnabled = False : btnProcessStress.ClearValue(Button.BackgroundProperty)
            btnMesh.Focus()

            Exit Sub
        End If


        System.Windows.Forms.Application.DoEvents()
        RetVal = Microsoft.VisualBasic.Shell(My.Application.Info.DirectoryPath & "\Library\" & "NIKE3D.exe", AppWinStyle.NormalFocus)

startAgain:
        Dim do1 As Boolean
        Dim Ubound1, counter1 As Integer

        Do
            do1 = True
            Dim processes() As Process
            Dim instance As Process
            Dim process As New Process()
            processes = Process.GetProcesses

            do1 = False

            Ubound1 = UBound(processes)
            counter1 = 0

            For Each instance In processes
                counter1 = counter1 + 1
                If UCase(instance.ProcessName) = "NIKE3D" Then
                    do1 = True
                    lblInfo.Content = "NIKE3D is running."
                    System.Windows.Forms.Application.DoEvents()
                    System.Threading.Thread.Sleep(3000)
                    GoTo startAgain
                End If
                If counter1 > Ubound1 Then
                    Exit Do
                End If

            Next
            System.Windows.Forms.Application.DoEvents()
        Loop While do1 Or Not exitLoop Or counter1 > Ubound1
        'System.Threading.Thread.Sleep(2000)

        lblInfo.Content = "NIKE3D finished running."


        ' YC 040620-1
labelNewProcedure:


        If lblInfo.Content <> "Mesh was created." Then
            Dim msg1 As String
            msg1 = "Wait till mesh created."
            MsgBox(msg1, MsgBoxStyle.OkOnly, "'Run Analysis' button clicked.")
            Exit Sub
        End If


        ' v3.0 005
        'Dim msg2 As String
        'msg2 = "If FEAFAA exits, please check file FAASR3d-Job Name.txt for error message!"
        'MsgBox(msg2, MsgBoxStyle.OkOnly, "'Run Analysis' button clicked.")
        ' v3.0 005 END



        lblInfo.Content = "FAASR3D is running."


        Try 'v3.0 002 YC 052620

            System.Windows.Forms.Application.DoEvents()
            Call FAASR3DCall()

            'lblInfo.Content = "FAASR3D finished running."
            lblInfo.Content = "FAASR3D run completed."
            ' YC 040620-1 END


            btnAnalysis.ClearValue(Button.BackgroundProperty) ' System.Drawing.ColorTranslator.FromOle(&H8000000F) 'gray
            btnNIKEPLOT.Background = Brushes.Aqua ' System.Drawing.ColorTranslator.FromOle(&HFFFF80) 'cyan
            btnProcessStress.Background = Brushes.Aqua

            btnMesh.IsEnabled = True : btnAnalysis.IsEnabled = True : btnNIKEPLOT.IsEnabled = True : btnProcessStress.IsEnabled = True
            btnNIKEPLOT.Focus()
            btnRertrieveJob.IsEnabled = True : btnSaveJob.IsEnabled = True


        Catch ex As Exception   'v3.0 002 YC 052620
            Dim msg1 As String
            msg1 = "FAASR3D run exited. " & Environment.NewLine & "Please save your job and report your probelm."
            'MsgBox(msg1, MsgBoxStyle.OkOnly, "'Run Analysis' button clicked.", MessageBoxImage.Warning)
            MessageBox.Show(msg1, "'Run Analysis' button clicked.", MessageBoxButton.OK, MessageBoxImage.Warning)
            btnAnalysis.ClearValue(Button.BackgroundProperty)
            lblInfo.Content = "FAASR3D run exited."
            Exit Sub
        End Try

    End Sub

    Private Sub btnNIKEPLOT_Click(sender As Object, e As RoutedEventArgs) Handles btnNIKEPLOT.Click
        '=========================================================
        Dim do2 As Boolean
        Dim Ubound2, counter2 As Integer

        Do
            do2 = True
            Dim processes() As Process
            Dim instance As Process
            Dim process As New Process()
            processes = process.GetProcesses

            do2 = False

            Ubound2 = UBound(processes)
            counter2 = 0

            For Each instance In processes
                counter2 = counter2 + 1
                If UCase(instance.ProcessName) = "NIKE3D" Then
                    do2 = True
                    System.Windows.Forms.Application.DoEvents()
                    MsgBox("Wait till NIKE3D finish running.", MsgBoxStyle.OkOnly, "'Run NIKEPLOT' button clicked.")
            Exit Sub
                ElseIf instance.ProcessName = "NIKEPLOT" Then
                    do2 = True
                    System.Windows.Forms.Application.DoEvents()
                    MsgBox("Wait till NIKEPLOT finish running.", MsgBoxStyle.OkOnly, "'Run NIKEPLOT' button clicked.")
                    Exit Sub
                End If
                If counter2 > Ubound2 Then
                    Exit Do
                End If

            Next
            System.Windows.Forms.Application.DoEvents()
        Loop While do2 Or Not exitLoop Or counter2 > Ubound2
        '==========================================================



        btnExitT4.Focus()
        btnRertrieveJob.IsEnabled = False : btnSaveJob.IsEnabled = False
        btnMesh.IsEnabled = False : btnAnalysis.IsEnabled = False : btnNIKEPLOT.IsEnabled = False : btnProcessStress.IsEnabled = False
        btnExitT4.Background = btnSaveJob.Background
        btnExitT1.Background = btnSaveJob.Background
        btnExitT2.Background = btnSaveJob.Background
        btnExitT3.Background = btnSaveJob.Background
        btnNIKEPLOT.Background = Brushes.Aqua ' System.Drawing.ColorTranslator.FromOle(&HFFFF80) 'cyan

        'Dim do1 As Boolean
        'Dim Ubound2, counter2 As Integer
        'do1 = True
        ''Do While do1
        'Dim processes() As Process
        'Dim instance As Process
        'Dim process As New Process()
        'processes = process.GetProcesses
        'Ubound2 = UBound(processes)
        'counter2 = 0

        'do1 = False
        'Do
        '    For Each instance In processes
        '        counter2 = counter2 + 1
        '        If instance.ProcessName = "NIKE3D" Then
        '            'MsgBox("Nike3D is Running", MsgBoxStyle.OkOnly, "msg")
        '            do1 = True
        '        End If
        '        If counter2 > Ubound2 Then
        '            Exit Do
        '        End If
        '    Next
        '    System.Windows.Forms.Application.DoEvents()
        'Loop While do1 Or Not exitLoop

        'If do1 Then
        '    Dim msg1 As String
        '    msg1 = "     Please, wait." & Environment.NewLine & "NIKE3D is still running."
        '    MsgBox(msg1, MsgBoxStyle.OkOnly, "Message")
        '    Exit Sub
        'End If
        ''Loop










        Dim fso As New Scripting.FileSystemObject
        Dim fil As Boolean
        Dim RetVal As Object
        'Microsoft Scripting has to be referenced

        fil = fso.FileExists(My.Application.Info.DirectoryPath & "\" & "n3dhsp")

        If Not fil Then
            Dim msg1 As String
            msg1 = "File n3dhsp is missing." & Environment.NewLine & "Click on button 'Run Analysis.'"
            MsgBox(msg1, MsgBoxStyle.OkOnly, "Message")

            btnRertrieveJob.IsEnabled = True : btnSaveJob.IsEnabled = True
            btnMesh.IsEnabled = False : btnMesh.ClearValue(Button.BackgroundProperty) 'gray
            btnAnalysis.IsEnabled = True : btnAnalysis.Background = Brushes.Aqua 'System.Drawing.ColorTranslator.FromOle(&HFFFF80) 'cyan
            btnNIKEPLOT.IsEnabled = False : btnNIKEPLOT.ClearValue(Button.BackgroundProperty) 'gray
            btnProcessStress.IsEnabled = False : btnProcessStress.ClearValue(Button.BackgroundProperty)
            btnAnalysis.Focus()

            Exit Sub
        End If

        System.Windows.Forms.Application.DoEvents()

        RetVal = Microsoft.VisualBasic.Shell(My.Application.Info.DirectoryPath & "\Library\" & "NIKEPLOT.exe", AppWinStyle.NormalFocus)

startAgain:
        Dim do1 As Boolean
        Dim Ubound1 As Integer
        Dim counter1 As Integer

        Do
            do1 = True
            Dim processes1() As Process
            Dim instance1 As Process
            Dim process1 As New Process()
            processes1 = Process.GetProcesses

            do1 = False


            Ubound1 = UBound(processes1)
            counter1 = 0

            For Each instance1 In processes1
                counter1 = counter1 + 1
                If instance1.ProcessName = "NIKEPLOT" Then
                    'Do something to or with this process or set a flag indicating that the desired process exists...
                    'MsgBox("Nike3D is Running", MsgBoxStyle.OkOnly, "msg")
                    do1 = True
                    lblInfo.Content = "NIKEPLOT is running."
                    System.Windows.Forms.Application.DoEvents()
                    System.Threading.Thread.Sleep(2000)
                    GoTo startAgain
                End If
                If counter1 > Ubound1 Then
                    Exit Do
                End If

            Next
            System.Windows.Forms.Application.DoEvents()
        Loop While do1 Or Not exitLoop Or counter1 > Ubound1
        'System.Threading.Thread.Sleep(2000)

        lblInfo.Content = "NIKEPLOT finished running."

        btnMesh.IsEnabled = True : btnAnalysis.IsEnabled = True : btnNIKEPLOT.IsEnabled = True : btnProcessStress.IsEnabled = True

        btnNIKEPLOT.Background = btnSaveJob.Background
        btnExitT4.Background = Brushes.Aqua ' System.Drawing.ColorTranslator.FromOle(&HFFFF80)
        btnExitT1.Background = Brushes.Aqua
        btnExitT2.Background = Brushes.Aqua
        btnExitT3.Background = Brushes.Aqua
        btnExitT4.Focus()
        btnRertrieveJob.IsEnabled = True : btnSaveJob.IsEnabled = True
    End Sub

    Private Sub ShowHelp()
        Dim startinfo = New ProcessStartInfo
        startinfo.WorkingDirectory = My.Application.Info.DirectoryPath + "\Library"  ' Environment.CurrentDirectory
        startinfo.FileName = "FEAFAA.chm"
        Dim proc As Process = Process.Start(startinfo)
    End Sub

    Private Sub btnHelpT1_Click(sender As Object, e As RoutedEventArgs) Handles btnHelpT1.Click, btnHelpT2.Click, btnHelpT3.Click, btnHelpT4.Click
        ShowHelp()
    End Sub

    Private Sub txtJobDirectory_MouseRightButtonUp(sender As Object, e As MouseButtonEventArgs) Handles txtJobDirectory.MouseRightButtonUp
        Dim oBrowse As FolderBrowser = New FolderBrowser(New Interop.WindowInteropHelper(Me).Handle)
        Dim NewWorkingDir As String

        Dim dlgFolderBrowser As Forms.FolderBrowserDialog = New Forms.FolderBrowserDialog
        oBrowse.Title = "Select a Job File Directory"
        oBrowse.NewUI = True
        oBrowse.ShowStatus = True

        NewWorkingDir = oBrowse.Browse(gJobFileFolder)
        If NewWorkingDir = "" Then Exit Sub
        gJobFileFolder = NewWorkingDir

        txtJobDirectory.Text = gJobFileFolder
    End Sub

    Private Sub btnProcessStress_Click(sender As Object, e As RoutedEventArgs) Handles btnProcessStress.Click

        ' YC 040620-1
        'If lblInfo.Content = "FAASR3D finished running." Then
        If lblInfo.Content = "FAASR3D run completed." Then
            NNd = Nd.Length - 1
            NBrckEle = BrickElement.Length - 1
            gPrintOutFolder = gJobFileFolder & "\PrintOut-" & JobName
        Else
            If Not lblInfo.Content = "Job printout folder is being selected." Then
                MsgBox("Please select a finished job printout folder, starting with ""PrintOut-"" in the left FEAFAA working directory.", MsgBoxStyle.OkOnly, "'Process Stress' button clicked.")
                lblInfo.Content = "Job printout folder is being selected."
                Dir1.IsEnabled = True
                fraFilNm.BorderBrush = Brushes.Red


                'btnProcessStress.Background = Brushes.Aqua
                Exit Sub
            End If

            gPrintOutFolder = Dir1.SelectedPath
            If Strings.Left(gPrintOutFolder, gJobFileFolder.Length + 10) <> gJobFileFolder & "\PrintOut-" Then
                MsgBox("FEAFAA job printout folder locates in FEAFAA working directory starting with ""PrintOut-"", please selected again!", MsgBoxStyle.OkOnly, "FEAFAA job printout folder")
                Exit Sub
            End If

            JobName = gPrintOutFolder.Remove(0, (gJobFileFolder & "\PrintOut-").Length)

            'Dim lPos As Integer = JobName.IndexOf(".")      'v3.0 002 YC 052620-v3.0 003
            'JobName = JobName.Remove(lPos, JobName.Length - lPos)

            Me.Title = MainTitle & "        Job Name: " & JobName

            ' check if input file exist
            Dim InputFile As String = gPrintOutFolder & "\input.txt"
            If Not System.IO.File.Exists(InputFile) Then
                MsgBox("FEAFAA job printout folder dost not have ""input"" file, please selected again!", MsgBoxStyle.OkOnly, "FEAFAA job printout folder")
            End If

            ' read in input file
            Dim readText() As String = IO.File.ReadAllLines(InputFile)  ' YC 040620-2
            Call readFAASR3DInputfile(readText)


            ' check if Output-Hexahedron Element files exist
            Dim Nstep = NAC
            For istep = 1 To Nstep
                Dim HexahedronStressFile As String = gPrintOutFolder & "\Output-Hexahedron Element-Step "
                HexahedronStressFile = HexahedronStressFile & istep & ".txt"

                If Not System.IO.File.Exists(HexahedronStressFile) Then
                    MsgBox("FEAFAA job printout folder dost not have ""Output-Hexahedron Element"" file, please selected again!", MsgBoxStyle.OkOnly, "FEAFAA job printout folder")
                    Exit Sub
                End If
            Next istep
        End If

        btnProcessStress.Background = btnSaveJob.Background
        fraFilNm.BorderBrush = btnSaveJob.Background
        Dir1.IsEnabled = False

        GoTo StressProcesslabel ' YC 040620
        ' YC 040620-1 END





        Dim fso As New Scripting.FileSystemObject
        Dim fil As Boolean

        fil = fso.FileExists(My.Application.Info.DirectoryPath & "\" & "n3dhsp")

        If Not fil Then
            Dim msg1 As String
            msg1 = "File n3dhsp is missing." & Environment.NewLine & "Click on button 'Run Analysis.'"
            MsgBox(msg1, MsgBoxStyle.OkOnly, "Message")

            btnMesh.IsEnabled = False : btnMesh.ClearValue(Button.BackgroundProperty) 'gray
            btnAnalysis.IsEnabled = True : btnAnalysis.Background = Brushes.Aqua 'System.Drawing.ColorTranslator.FromOle(&HFFFF80) 'cyan
            btnNIKEPLOT.IsEnabled = False : btnNIKEPLOT.ClearValue(Button.BackgroundProperty) 'gray
            btnProcessStress.IsEnabled = False : btnProcessStress.ClearValue(Button.BackgroundProperty) 'gray
            btnAnalysis.Focus()

            Exit Sub
        End If

        btnProcessStress.Background = btnSaveJob.Background


StressProcesslabel:  ' YC 040620

        Dim StressProcessWin As WindowStress = New WindowStress()
        StressProcessWin.ShowDialog()

        lblInfo.Content = ""

    End Sub

    Private Sub cmbLoadingType_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles cmbLoadingType.SelectionChanged


        If cmbLoadingType.SelectedIndex = 0 Then
            IsMovingLoad = False

            lblLdCase.IsEnabled = True
            optEdge.IsEnabled = True
            optInterior.IsEnabled = True
            'optEdge.IsChecked = True
            'optInterior.IsChecked = False

            lblPositionGear.Content = "Position of Gear Center:"

            lblPositionGearMoved.Visibility = Windows.Visibility.Hidden
            lblXOffsetMoved.Visibility = Windows.Visibility.Hidden
            lblYOffsetMoved.Visibility = Windows.Visibility.Hidden
            lblNGearPositons.Visibility = Windows.Visibility.Hidden
            'lblMovingSpeed.Visibility = Windows.Visibility.Hidden

            txtXCoordMoved.Visibility = Windows.Visibility.Hidden
            txtYCoordMoved.Visibility = Windows.Visibility.Hidden
            txtNGearPositons.Visibility = Windows.Visibility.Hidden
            'txtMovingSpeed.Visibility = Windows.Visibility.Hidden

        End If

        If cmbLoadingType.SelectedIndex = 1 Then
            IsMovingLoad = True

            lblLdCase.IsEnabled = False
            optEdge.IsEnabled = False
            optInterior.IsEnabled = False
            optEdge.IsChecked = False
            optInterior.IsChecked = False

            lblPositionGear.Content = "Start Position of Gear Center:"

            lblPositionGearMoved.Visibility = Windows.Visibility.Visible
            lblXOffsetMoved.Visibility = Windows.Visibility.Visible
            lblYOffsetMoved.Visibility = Windows.Visibility.Visible
            lblNGearPositons.Visibility = Windows.Visibility.Visible
            'lblMovingSpeed.Visibility = Windows.Visibility.Visible

            txtXCoordMoved.Visibility = Windows.Visibility.Visible
            txtYCoordMoved.Visibility = Windows.Visibility.Visible
            txtNGearPositons.Visibility = Windows.Visibility.Visible
            'txtMovingSpeed.Visibility = Windows.Visibility.Visible

            'NMovingStep = CSng(txtNGearPositons.Text)


        End If

        Call DrawGear()

    End Sub

    Sub TextInputCheck()

        If Not IsNumeric(txtXCoord.Text) Then
            MessageBox.Show("Please input numeric value for x-offset")
            ErrorInput = True
            TabStrip1.SelectedIndex = 1
            Exit Sub
        End If

        If Not IsNumeric(txtYCoord.Text) Then
            MessageBox.Show("Please input numeric value for y-offset")
            ErrorInput = True
            TabStrip1.SelectedIndex = 1
            Exit Sub
        End If

        If Not IsNumeric(txtXCoordMoved.Text) Then
            MessageBox.Show("Please input numeric value for end x-offset")
            ErrorInput = True
            TabStrip1.SelectedIndex = 1
            Exit Sub
        End If

        If Not IsNumeric(txtYCoordMoved.Text) Then
            MessageBox.Show("Please input numeric value for end y-offset")
            ErrorInput = True
            TabStrip1.SelectedIndex = 1
            Exit Sub
        End If

        If Not IsNumeric(txtLETG.Text) Then
            MessageBox.Show("Please input numeric value for ELTG")
            ErrorInput = True
            TabStrip1.SelectedIndex = 1
            Exit Sub
        End If

        If Not IsNumeric(txtThermCoef.Text) Then
            MessageBox.Show("Please input numeric value for thermal coefficient")
            ErrorInput = True
            TabStrip1.SelectedIndex = 1
            Exit Sub
        End If

        ' Only validate dowel bar stiffness if the corresponding checkboxes are checked
        If chkDowelX.IsChecked Then
            If Not IsNumeric(txtJntStfX.Text) Then
                MessageBox.Show("Please input numeric value for x-direction dowel bar")
                ErrorInput = True
                TabStrip1.SelectedIndex = 2
                Exit Sub
            End If
        End If

        If chkDowelY.IsChecked Then
            If Not IsNumeric(txtJntStfY.Text) Then
                MessageBox.Show("Please input numeric value for y-direction dowel bar")
                ErrorInput = True
                TabStrip1.SelectedIndex = 2
                Exit Sub
            End If
        End If

        If Not IsNumeric(txtEdgStfX.Text) Then
            MessageBox.Show("Please input numeric value for x-direction spring constraint")
            ErrorInput = True
            TabStrip1.SelectedIndex = 2
            Exit Sub
        End If

        If Not IsNumeric(txtEdgStfY.Text) Then
            MessageBox.Show("Please input numeric value for y-direction spring constraint")
            ErrorInput = True
            TabStrip1.SelectedIndex = 2
            Exit Sub
        End If

    End Sub


    Sub readFAASR3DInputfile(ByVal readTextall() As String)
        'to read in input file by YC 040620-2


        Dim mark1 = "* Interfaces[8], Number of symmetry planes[9], Disc. Element flag[10]"
        Dim mark2 = "* Max. allowable time step size[7]"



        'Dim mark3 = "* boundary conditions[15]"
        Dim mark4 = "* Buffer size[4], Output option[5], Data Storage Option[6]"
        'Dim mark5 = "*------------------ DISCRETE ELEMENT DECK ------------------*"
        'Dim mark6 = "*--------------------- SLIDING INTERFACE DEFINITIONS ---------------------*"
        'Dim mark7 = "*------------ CONCENTRATED NODAL LOADS --------------------------------*"


        'Dim markCard7 = "* Frequency shift[6], First Newmark parm.[7], Second Newmark  parm.[8]"    ' YC 121219
        'Dim markTemp = "*------------ TEMPERATURE PROFILE DECK --------------------------------*"

        Dim Mline = readTextall.Length

        'Dim Nline As Integer = 0
        Dim ReadLine, elements() As String
        Dim L, M, I, J As Integer
        'For I = 0 To Ifile
        '    M = Mline(I)
        '    Nline = Nline + M
        'Next I

        For I = 0 To Mline
            ReadLine = readTextall(I)

            If ReadLine = mark1 Then
                ReadLine = readTextall(I + 1)
                elements = ReadLine.Split(New Char() {" "c},
                                        StringSplitOptions.RemoveEmptyEntries)
                NMat = elements(1)
                NNd = elements(2)
                NBrckEle = elements(3)
                Erase elements
            End If
            ReDim Nd(NNd), BrickElement(NBrckEle)

            If ReadLine = mark2 Then
                ReadLine = readTextall(I + 1)
                elements = ReadLine.Split(New Char() {" "c},
                                        StringSplitOptions.RemoveEmptyEntries)
                NAC = elements(0)
            End If


            If ReadLine = mark4 Then
                L = I + 1 + 8 * NMat
                'Call matin(readTextall, L)
                'L = L + 8 * clsCom.nmmat + 1
                'ReadLine = readTextall(L)
                'If ReadLine = "    5    0" Then
                '    L = L + 5
                'Else
                '    L = L - 1
                'End If

                'Call Node(readTextall, L)
                For Ind = 1 To NNd
                    ReadLine = readTextall(L + Ind)
                    Dim modified As String = ReadLine.Insert(13, " ")
                    modified = modified.Insert(34, " ")
                    modified = modified.Insert(55, " ")
                    elements = modified.Split(New Char() {" "c},
                                            StringSplitOptions.RemoveEmptyEntries)
                    'Nd(Ind).X = elements(2) : Nd(Ind).Y = elements(3) : Nd(Ind).Z = elements(4)

                    Nd(Ind).X = Strings.Mid(ReadLine, 14, 20) : Nd(Ind).Y = Strings.Mid(ReadLine, 34, 20) : Nd(Ind).Z = Strings.Mid(ReadLine, 54, 20)
                Next Ind
                L = L + NNd

                'Call elemnh(readTextall, L)
                'L = L + clsCom.numelh
                For iEle = 1 To NBrckEle
                    ReadLine = readTextall(L + iEle)
                    elements = ReadLine.Split(New Char() {" "c},
                                            StringSplitOptions.RemoveEmptyEntries)
                    BrickElement(iEle).IdxMat = elements(1)

                    ReDim BrickElement(iEle).Node(8)
                    For J = 1 To 8
                        BrickElement(iEle).Node(J) = elements(J + 1)
                    Next J
                Next iEle

                Exit Sub

            End If

            'If ReadLine = mark5 Then
            '    ReadLine = readTextall(I + 2)
            '    elements = ReadLine.Split(New Char() {" "c},
            '                                    StringSplitOptions.RemoveEmptyEntries)
            '    clsCom.nmmtde = elements(0)
            '    clsCom.nmelde = elements(1)
            '    clsCom.nmmass = elements(2)
            '    ReDim clsCom.mtypde(clsCom.nmmtde), clsCom.cmde(9, clsCom.nmmtde), clsCom.ixde(3, clsCom.nmelde), clsCom.sclf(clsCom.nmelde)
            '    For J = 1 To clsCom.nmmtde
            '        ReadLine = readTextall(I + 2 * J + 1)
            '        elements = ReadLine.Split(New Char() {" "c},
            '                                        StringSplitOptions.RemoveEmptyEntries)
            '        clsCom.mtypde(J) = elements(1)
            '        ReadLine = readTextall(I + 2 * J + 2)
            '        elements = ReadLine.Split(New Char() {" "c},
            '                                        StringSplitOptions.RemoveEmptyEntries)
            '        clsCom.cmde(1, J) = elements(0)
            '        clsCom.cmde(2, J) = elements(1)
            '    Next J
            '    L = I + 2 * clsCom.nmmtde + 3
            '    For J = 1 To clsCom.nmelde
            '        ReadLine = readTextall(L + J - 1)
            '        elements = ReadLine.Split(New Char() {" "c},
            '                                        StringSplitOptions.RemoveEmptyEntries)
            '        If elements.Length < 5 Then
            '            clsCom.sclf(J) = 1.0
            '        Else
            '            clsCom.sclf(J) = elements(4)
            '        End If
            '        clsCom.ixde(1, J) = elements(1) : clsCom.ixde(2, J) = elements(2)
            '        clsCom.ixde(3, J) = elements(3)
            '    Next J
            'End If

            'If ReadLine = mark6 Then
            '    L = I + 1
            '    Call sillin(readTextall, L)
            '    Call slavin(readTextall, L)
            '    Call ldcvs(readTextall, L)
            'End If

            'If ReadLine = mark7 Then
            '    L = I + 1
            '    Call cnlds(readTextall, L)
            '    ReadLine = readTextall(L)
            '    elements = ReadLine.Split(New Char() {" "c},
            '                                    StringSplitOptions.RemoveEmptyEntries)
            'End If


            '' YC 121219
            'If (clsCom.itread <> 0 And ReadLine = markTemp) Then
            '    L = I + 1
            '    Call trder(readTextall, L)
            'End If
            '' YC 121219 END


        Next I

    End Sub

    Private Sub lblAnalysis_MouseUp(sender As Object, e As MouseButtonEventArgs) Handles lblAnalysis.MouseUp
        ' v3.0 005

        Dim msg2 As String
        msg2 = "If FEAFAA exits, please check file FAASR3d-Job Name.txt for error message!"
        MsgBox(msg2, MsgBoxStyle.OkOnly, "'Run Analysis' Information.")

    End Sub
End Class

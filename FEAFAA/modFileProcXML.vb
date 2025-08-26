Option Strict On
Option Explicit On

Imports Microsoft.Win32
Imports VB = Microsoft.VisualBasic
Imports System.Xml
Imports System.IO
Imports System.Threading

Imports System
Imports System.Globalization
Imports System.Security.Permissions

Module modFileProcXML

    Public JobName As String
    Dim WorkingDir As String
    Dim iGroup As Integer
    Dim iList As Integer

    Public Sub WriteJobFileXML()

        Dim I As Integer
        Dim cinfo, xmlFile As String
        Dim myStream As Stream = Nothing
        Dim saveFileDialog1 As New SaveFileDialog()

        Try

            cinfo = CultureInfo.CurrentCulture.Name
            'WorkingDir = Environment.CurrentDirectory & "\"

            If gJobFileFolder.Substring(gJobFileFolder.Length - 1, 1) = "\" Then
                WorkingDir = gJobFileFolder
            Else
                WorkingDir = gJobFileFolder & "\"
            End If


            'saveFileDialog1.InitialDirectory = Environment.CurrentDirectory & "\"
            saveFileDialog1.InitialDirectory = WorkingDir

            saveFileDialog1.Filter = "Job files (*.FEA.xml)|*.FEA.xml|All files (*.*)|*.*"
            saveFileDialog1.FilterIndex = 1
            saveFileDialog1.RestoreDirectory = True
            saveFileDialog1.DefaultExt = ".FEA.xml"


FileDialogShowlabel:    'v3.0 002 YC 052620

            If saveFileDialog1.ShowDialog() Then
                Dim lFileLength As Integer, lLastPos As Integer
                lLastPos = saveFileDialog1.FileName.LastIndexOf("\")
                lFileLength = saveFileDialog1.FileName.Length - lLastPos

                gJobFileFolder = saveFileDialog1.FileName.Remove(lLastPos, lFileLength)
                Application.win.txtJobDirectory.Text = gJobFileFolder

                xmlFile = saveFileDialog1.FileName
                'JobName = saveFileDialog1.FileName.Remove(0, (saveFileDialog1.InitialDirectory.Length))
                JobName = saveFileDialog1.FileName.Remove(0, lLastPos + 1)

                'v3.0 002 YC 052620
                'JobName = JobName.Remove(JobName.Length - 8, 8)


                'Dim lPos As Integer = JobName.IndexOf(".") 'v3.0 004
                Dim lPos As Integer = JobName.Length - 8


                Dim extn = JobName.Remove(0, lPos)
                If extn <> ".FEA.xml" Then
                    MessageBox.Show("Please input file name with no extention!", "File Name", MessageBoxButton.OK, MessageBoxImage.Warning)
                    GoTo FileDialogShowlabel
                End If
                'v3.0 002 YC 052620 END

                JobName = JobName.Remove(lPos, JobName.Length - lPos)


            Else
                Exit Sub
            End If

            iGroup = Application.win.ListBoxAirplaneGroup.SelectedIndex
            iList = Application.win.ListBoxLibraryAirplane.SelectedIndex

            Dim xw As New Xml.XmlTextWriter(xmlFile, System.Text.Encoding.UTF8)
            xw.WriteStartDocument()
            xw.WriteStartElement("FEAFAAJobInfo")

            '==========================================================================================

            xw.WriteStartElement("VehicleSelectionTab") 'Tab 1
            'xw.WriteElementString("VehicleName", JobTitle)
            xw.WriteElementString("VehicleGroup", Format(iGroup, "#0.00").ToString)
            xw.WriteElementString("VehicleIndex", Format(iList, "#0.00").ToString)

            xw.WriteElementString("GrossWeight", Format(GrossWeight, "#0.00").ToString)
            xw.WriteElementString("PcntOnMainGears", Format(PcntOnMainGears, "#0.00").ToString)
            xw.WriteElementString("NMainGears", Format(NMainGears, "#0").ToString)
            xw.WriteElementString("NWheels", Format(NWheels, "#0").ToString)
            xw.WriteElementString("TirePressure", Format(TirePressure, "#0.00").ToString)

            xw.WriteStartElement("WheelCoordinates")
            For I = 1 To NWheels
                xw.WriteElementString("X", Format(XWheels(I), "#0.00").ToString)
                xw.WriteElementString("Y", Format(YWheels(I), "#0.00").ToString)
            Next
            xw.WriteEndElement()

            xw.WriteElementString("PCARectangle", CStr(Application.win.PCARectangle))
            xw.WriteEndElement()

            '==========================================================================================

            xw.WriteStartElement("PavementStructureTab") 'Tab 2
            xw.WriteElementString("PCCOverlay", CStr(Application.win.ChkOverlay.IsChecked))
            xw.WriteElementString("NumberOfLayers", CStr(Application.win.NumberOfLayers))

            xw.WriteStartElement("LayerData")
            For I = 0 To Application.win.NumberOfLayers - 1
                xw.WriteElementString("LayerTypes", CStr(Application.win.grdLayerTable.Rows(I)(0)))
                xw.WriteElementString("EModulus", CStr(Application.win.grdLayerTable.Rows(I)(1)))
                xw.WriteElementString("PoissonsRatio", CStr(Application.win.grdLayerTable.Rows(I)(2)))
                xw.WriteElementString("Thickness", CStr(Application.win.grdLayerTable.Rows(I)(3)))
            Next
            xw.WriteEndElement()

            '--------------------------------------------------------------------------------
            xw.WriteStartElement("SlabMeshGroupBox")
            xw.WriteElementString("XDimension", CStr(Application.win.cmbDimX.Text))
            xw.WriteElementString("YDimension", CStr(Application.win.cmbDimY.Text))
            xw.WriteElementString("NumberOfElementsSlab", CStr(Application.win.cmbMesh.Text))
            xw.WriteElementString("NumberOfSlabs", CStr(Application.win.cmbNSlabs.SelectedItem))
            xw.WriteEndElement()

            xw.WriteStartElement("FoundationMeshGroupBox")
            xw.WriteElementString("NumberOfElementsFoundation", CStr(Application.win.cmbFdMesh.Text))
            xw.WriteEndElement()

            '--------------------------------------------------------------------------------


            xw.WriteStartElement("LoadingGroupBox")

            ' modified for moving load 012815

            'xw.WriteStartElement("SelectLoadCase")
            'xw.WriteElementString("InteriorLoad", CStr(Application.win.optInterior.IsChecked))
            'xw.WriteEndElement()

            'xw.WriteElementString("Angle", CStr(Application.win.cmbAngle.Text))

            'xw.WriteStartElement("PositonGear")
            'xw.WriteElementString("GearXoffset", Application.win.txtXCoord.Text)
            'xw.WriteElementString("GearYoffset", Application.win.txtYCoord.Text)
            'xw.WriteEndElement()

            xw.WriteElementString("LoadingType", Application.win.cmbLoadingType.Text)
            If Application.win.cmbLoadingType.Text = "Static Load" Then

                If Application.win.optInterior.IsChecked Then
                    xw.WriteElementString("LoadCase", "InteriorLoad")
                End If

                If Application.win.optEdge.IsChecked Then
                    xw.WriteElementString("LoadCase", "EdgeLoad")
                End If

                xw.WriteElementString("Angle", Application.win.cmbAngle.Text)

                xw.WriteStartElement("PositonGear")
                xw.WriteElementString("GearXoffset", Application.win.txtXCoord.Text)
                xw.WriteElementString("GearYoffset", Application.win.txtYCoord.Text)
                xw.WriteEndElement()
            End If

            If Application.win.cmbLoadingType.Text = "Moving Load" Then

                xw.WriteElementString("Angle", Application.win.cmbAngle.Text)

                xw.WriteStartElement("PositonGear")
                xw.WriteElementString("GearXoffset", Application.win.txtXCoord.Text)
                xw.WriteElementString("GearYoffset", Application.win.txtYCoord.Text)
                xw.WriteElementString("GearXoffsetMoved", Application.win.txtXCoordMoved.Text)
                xw.WriteElementString("GearYoffsetMoved", Application.win.txtYCoordMoved.Text)
                xw.WriteEndElement()

                xw.WriteElementString("NumberofGearPositions", Application.win.txtNGearPositons.Text)

            End If

            ' modified for moving load END YC 012815

            xw.WriteEndElement() 'LoadingGroupBox


            '--------------------------------------------------------------------------------
            xw.WriteStartElement("SlabTemperatureGroupBox")
            xw.WriteElementString("SlabTemperatureCheckBox", CStr(Application.win.chkTempLoad.IsChecked))

            'for INGRID 120413 by YC 111314
            'xw.WriteElementString("TopTemperature", Application.win.txtSurfTemp.Text)
            'xw.WriteElementString("BottomTemperature", Application.win.txtBotmTemp.Text)
            xw.WriteElementString("LETG", Application.win.txtLETG.Text)
            'for INGRID 120413 by YC 111314 END

            xw.WriteElementString("TermalCoefficient", Application.win.txtThermCoef.Text)
            xw.WriteElementString("SlabCurlingShape", CStr(Application.win.cmbCurlShape.SelectedItem))
            xw.WriteEndElement()

            xw.WriteEndElement()


            '==========================================================================================
            xw.WriteStartElement("JointModelingTab") 'Tab 3

            xw.WriteStartElement("XDirectionDowelBarDataGroupBox") 'Group Box
            xw.WriteElementString("XDirectionDowelBarDataCheckBox", CStr(Application.win.chkDowelX.IsChecked))
            xw.WriteElementString("XDowelBarDiameter", CStr(Application.win.cmbDowelDiameterX.Text))
            xw.WriteElementString("XDowelBarSpacing", CStr(Application.win.cmbDowelSpacingX.Text))
            xw.WriteElementString("XJointOpening", CStr(Application.win.cmbJointOpeningX.Text))

            xw.WriteStartElement("MethodOfDowerBarPlacement")
            xw.WriteElementString("XBarPlacedInFreshedConcrete", CStr(Application.win.optFreshX.IsChecked))
            xw.WriteEndElement()

            xw.WriteElementString("XEquivalentJointStiffness", Application.win.txtJntStfX.Text)
            xw.WriteEndElement()

            xw.WriteStartElement("XDirectionSpringConstraintGroupBox")
            xw.WriteElementString("XDirectionSpringConstraintCheckBox", CStr(Application.win.chkBoundaryX.IsChecked))
            xw.WriteElementString("XEquivalentBoundaryStiffness", Application.win.txtEdgStfX.Text)
            xw.WriteEndElement()

            '--------------------------------------------------------------------------------
            xw.WriteStartElement("YDirectionDowelBarDataGroupBox") 'Group Box
            xw.WriteElementString("YDirectionDowelBarDataCheckBox", CStr(Application.win.chkDowelY.IsChecked))
            xw.WriteElementString("YDowelBarDiameter", CStr(Application.win.cmbDowelDiameterY.Text))
            xw.WriteElementString("YDowelBarSpacing", CStr(Application.win.cmbDowelSpacingY.Text))
            xw.WriteElementString("YJointOpening", CStr(Application.win.cmbJointOpeningY.Text))

            xw.WriteStartElement("MethodOfDowerBarPlacement")
            xw.WriteElementString("YBarPlacedInFreshedConcrete", CStr(Application.win.optFreshY.IsChecked))
            xw.WriteEndElement()

            xw.WriteElementString("YEquivalentJointStiffness", Application.win.txtJntStfY.Text)
            xw.WriteEndElement()

            xw.WriteStartElement("YDirectionSpringConstraintGroupBox")
            xw.WriteElementString("YDirectionSpringConstraintCheckBox", CStr(Application.win.chkBoundaryY.IsChecked))
            xw.WriteElementString("YEquivalentBoundaryStiffness", Application.win.txtEdgStfY.Text)
            xw.WriteEndElement()

            xw.WriteEndElement() 'Joint Modeling Tab

            xw.WriteEndElement() 'FEAFAA job info
            xw.WriteEndDocument()
            xw.Close()


        Catch ex As Exception

            Dim txt As String
            txt = ex.Message
            txt = txt + Environment.NewLine + Environment.NewLine
            txt = txt + ex.StackTrace
            txt = txt + Environment.NewLine + Environment.NewLine
            MsgBox(txt)

        End Try



    End Sub

    Sub ReadJobFileXML(ByRef ret1 As Boolean)

        Dim myStream As Stream = Nothing
        Dim openFileDialog1 As New OpenFileDialog()
        Dim unknown As Integer

        Dim NWheels1, NMainGears1 As Short
        Dim GrossWeight1, TirePressure1, PcntOnMainGears1 As Double
        Dim XX(1), YY(1) As Double
        Dim JntStfX1, JntStfY1 As double

        Dim I, K As Integer, FileNameXML As String
        Dim ConvertToDot As Boolean

        If Thread.CurrentThread.CurrentCulture.NumberFormat.NumberDecimalSeparator.ToString = "." Then
            ConvertToDot = True
        Else
            ConvertToDot = False
        End If


        If gJobFileFolder.Substring(gJobFileFolder.Length - 1, 1) = "\" Then
            WorkingDir = gJobFileFolder
        Else
            WorkingDir = gJobFileFolder & "\"
        End If

        unknown = 0

        Try
            'openFileDialog1.InitialDirectory = Environment.CurrentDirectory & "\"
            openFileDialog1.InitialDirectory = gJobFileFolder
            openFileDialog1.Filter = "Job files (*.FEA.xml)|*.FEA.xml|All files (*.*)|*.*"
            openFileDialog1.FilterIndex = 1
            openFileDialog1.RestoreDirectory = True
            openFileDialog1.DefaultExt = ".FEA.xml"

            If openFileDialog1.ShowDialog() Then

                Dim lFileLength As Integer, lLastPos As Integer
                lLastPos = openFileDialog1.FileName.LastIndexOf("\")
                lFileLength = openFileDialog1.FileName.Length - lLastPos


                FileNameXML = openFileDialog1.FileName
                'JobName = openFileDialog1.SafeFileName
                'JobName = openFileDialog1.FileName.Remove(0, (openFileDialog1.InitialDirectory.Length)
                JobName = openFileDialog1.FileName.Remove(0, lLastPos + 1)

                JobName = JobName.Remove(JobName.Length - 8, 8) 'v3.0 002 YC 052620-v3.0 004
                'Dim lPos As Integer = JobName.IndexOf(".")
                'JobName = JobName.Remove(lPos, JobName.Length - lPos)

                gJobFileFolder = openFileDialog1.FileName.Remove(lLastPos, lFileLength)
                Application.win.txtJobDirectory.Text = gJobFileFolder

                ret1 = True

            Else
                ret1 = False
                Exit Sub
            End If


            Dim reader As XmlTextReader = New XmlTextReader(FileNameXML)
            Dim Element As String
            Element = ""

            I = -1
            Do While (reader.Read())
                Select Case reader.NodeType
                    Case XmlNodeType.Element 'Display beginning of element. (like GrossWt)
                        Debug.Write("<" + reader.Name)
                        Element = reader.Name

                        If reader.HasAttributes Then 'If attributes exist
                            While reader.MoveToNextAttribute()
                                'Display attribute name and value.
                                Debug.Write(" {0}='{1}' " & reader.Name & "   " & reader.Value)
                            End While
                        End If

                        Debug.WriteLine(">")
                    Case XmlNodeType.Text 'Display the text in each element. (like 25,000)
                        If Element = "SectionName" Then
                            'I += 1 : J = 0 : K = 0
                            'jobSectName(I) = reader.Value

                            'Vehicle Selection Tab
                        ElseIf Element = "VehicleGroup" Then

                            iGroup = CInt(reader.Value)

                        ElseIf Element = "VehicleIndex" Then

                            iList = CInt(reader.Value)

                        ElseIf Element = "GrossWeight" Then

                            If ConvertToDot Then
                                GrossWeight1 = CSng(reader.Value.Replace(",", "."))
                            Else
                                GrossWeight1 = CSng(reader.Value.Replace(".", ","))
                            End If

                        ElseIf Element = "PcntOnMainGears" Then

                            If ConvertToDot Then
                                PcntOnMainGears1 = CSng(reader.Value.Replace(",", "."))
                            Else
                                PcntOnMainGears1 = CSng(reader.Value.Replace(".", ","))
                            End If

                        ElseIf Element = "NMainGears" Then
                            NMainGears1 = CShort(reader.Value)

                        ElseIf Element = "NWheels" Then
                            NWheels1 = CShort(reader.Value)
                            ReDim XX(NWheels1)
                            ReDim YY(NWheels1)
                            K = 0

                        ElseIf Element = "X" Then
                            K = K + 1
                            XX(K) = CDbl(reader.Value)

                        ElseIf Element = "Y" Then
                            YY(K) = CDbl(reader.Value)


                        ElseIf Element = "TirePressure" Then

                            If ConvertToDot Then
                                TirePressure1 = CSng(reader.Value.Replace(",", "."))
                            Else
                                TirePressure1 = CSng(reader.Value.Replace(".", ","))
                            End If

                        ElseIf Element = "PCARectangle" Then

                            Application.win.PCARectangle = CBool(reader.Value)
                            If Application.win.PCARectangle Then
                                Application.win.OptPCARectangle.IsChecked = True
                            Else
                                Application.win.OptSquare.IsChecked = True
                            End If





                            'Pavement Structure Tab
                        ElseIf Element = "PCCOverlay" Then
                            Application.win.ChkOverlay.IsChecked = CBool(reader.Value)

                        ElseIf Element = "NumberOfLayers" Then
                            Dim oldNumber, newNumber As Integer
                            oldNumber = Application.win.NumberOfLayers
                            newNumber = CShort(reader.Value)

                            'Dim sender As System.Object
                            'Dim e As System.EventArgs
                            'vb.net sender object calling click event

                            If newNumber > oldNumber Then
                                Do While newNumber > oldNumber
                                    Call Application.win.btnAddALayer_Click(New System.Object, Nothing)
                                    oldNumber = oldNumber + 1
                                Loop
                            ElseIf newNumber < oldNumber Then

                                Do While newNumber < oldNumber
                                    Call Application.win.btnDeleteALayer_Click(New System.Object, Nothing)
                                    oldNumber = oldNumber - 1
                                Loop

                            End If



                            Application.win.NumberOfLayers = CShort(newNumber)

                        ElseIf Element = "LayerTypes" Then
                            I = I + 1
                            Application.win.grdLayerTable.Rows(I)(0) = reader.Value

                        ElseIf Element = "EModulus" Then
                            Application.win.grdLayerTable.Rows(I)(1) = reader.Value

                        ElseIf Element = "PoissonsRatio" Then
                            Application.win.grdLayerTable.Rows(I)(2) = reader.Value

                        ElseIf Element = "Thickness" Then
                            Application.win.grdLayerTable.Rows(I)(3) = reader.Value



                            'Slab Mesh and Foundation Mesh
                        ElseIf Element = "XDimension" Then
                            Application.win.cmbDimX.Text = reader.Value
                        ElseIf Element = "YDimension" Then
                            Application.win.cmbDimY.Text = reader.Value
                        ElseIf Element = "NumberOfElementsSlab" Then
                            Application.win.cmbMesh.Text = reader.Value
                        ElseIf Element = "NumberOfSlabs" Then
                            Application.win.cmbNSlabs.SelectedItem = reader.Value
                        ElseIf Element = "NumberOfElementsFoundation" Then
                            Application.win.cmbFdMesh.Text = reader.Value

                            'Loading

                            'ElseIf Element = "InteriorLoad" Then
                            '    'frmGear.optInterior.Checked = CBool(reader.Value)
                            '    Dim b1 As Boolean
                            '    b1 = CBool(reader.Value)
                            '    If b1 Then
                            '        Application.win.optInterior.IsChecked = True
                            '    Else
                            '        Application.win.optEdge.IsChecked = True
                            '    End If
                        ElseIf Element = "LoadCase" Then
                            LoadCase = reader.Value

                        ElseIf Element = "Angle" Then
                            'frmGear.cmbAngle.SelectedItem = reader.Value
                            Application.win.cmbAngle.Text = reader.Value

                            Application.win.GearAngle = CSng(reader.Value)  'v3.0 002 YC 052620

                        ElseIf Element = "GearXoffset" Then
                            gXcoord1 = CSng(reader.Value)
                        ElseIf Element = "GearYoffset" Then
                            gYcoord1 = CSng(reader.Value)


                        ElseIf Element = "LoadingType" Then
                            LoadingType = reader.Value
                        ElseIf Element = "GearXoffsetMoved" Then
                            gXcoord2 = CSng(reader.Value)
                        ElseIf Element = "GearYoffsetMoved" Then
                            gYcoord2 = CSng(reader.Value)
                        ElseIf Element = "NumberofGearPositions" Then
                            NGearPositions = CInt(reader.Value)


                            'Temperature
                        ElseIf Element = "SlabTemperatureCheckBox" Then
                            Application.win.chkTempLoad.IsChecked = CBool(reader.Value)

                            'for INGRID 120413 by YC 111314
                            'ElseIf Element = "TopTemperature" Then
                            '    Application.win.txtSurfTemp.Text = reader.Value
                            'ElseIf Element = "BottomTemperature" Then
                            '    Application.win.txtBotmTemp.Text = reader.Value
                        ElseIf Element = "LETG" Then
                            Application.win.txtLETG.Text = reader.Value
                            'for INGRID 120413 by YC 111314 END

                        ElseIf Element = "TermalCoefficient" Then
                            Application.win.txtThermCoef.Text = reader.Value
                        ElseIf Element = "SlabCurlingShape" Then
                            'frmGear.cmbCurlShape.SelectedItem = reader.Value
                            Application.win.cmbCurlShape.Text = reader.Value

                            If reader.Value = "Circular" Then Application.win.cmbCurlShape.Text = "Spherical" 'v3.0 002 YC 052620


                            'Tab 3
                            'Joint Modeling Tab
                        ElseIf Element = "XDirectionDowelBarDataCheckBox" Then
                            Application.win.chkDowelX.IsChecked = CBool(reader.Value)
                        ElseIf Element = "XDowelBarDiameter" Then
                            Application.win.cmbDowelDiameterX.Text = reader.Value
                        ElseIf Element = "XDowelBarSpacing" Then
                            Application.win.cmbDowelSpacingX.Text = reader.Value
                        ElseIf Element = "XJointOpening" Then
                            Application.win.cmbJointOpeningX.Text = reader.Value

                        ElseIf Element = "XBarPlacedInFreshedConcrete" Then
                            Dim b1 As Boolean
                            b1 = CBool(reader.Value)
                            If b1 Then
                                Application.win.optFreshX.IsChecked = True
                            Else
                                Application.win.optDrilledX.IsChecked = True
                            End If


                        ElseIf Element = "XEquivalentJointStiffness" Then
                            JntStfX1 = CSng(reader.Value)
                        ElseIf Element = "XDirectionSpringConstraintCheckBox" Then
                            Application.win.chkBoundaryX.IsChecked = CBool(reader.Value)
                        ElseIf Element = "XEquivalentBoundaryStiffness" Then
                            EqEdgStfX = CSng(reader.Value)

                            'Y direction
                        ElseIf Element = "YDirectionDowelBarDataCheckBox" Then
                            Application.win.chkDowelY.IsChecked = CBool(reader.Value)
                        ElseIf Element = "YDowelBarDiameter" Then
                            Application.win.cmbDowelDiameterY.Text = reader.Value
                        ElseIf Element = "YDowelBarSpacing" Then
                            Application.win.cmbDowelSpacingY.Text = reader.Value
                        ElseIf Element = "YJointOpening" Then
                            Application.win.cmbJointOpeningY.Text = reader.Value

                        ElseIf Element = "YBarPlacedInFreshedConcrete" Then
                            Dim b1 As Boolean
                            b1 = CBool(reader.Value)
                            If b1 Then
                                Application.win.optFreshY.IsChecked = True
                            Else
                                Application.win.optDrilledY.IsChecked = True
                            End If

                        ElseIf Element = "YEquivalentJointStiffness" Then
                            JntStfY1 = CSng(reader.Value)
                        ElseIf Element = "YDirectionSpringConstraintCheckBox" Then
                            Application.win.chkBoundaryY.IsChecked = CBool(reader.Value)
                        ElseIf Element = "YEquivalentBoundaryStiffness" Then
                            EqEdgStfY = CSng(reader.Value)

                            '=================================================================

                        ElseIf Element = "S" Then
                            S = reader.Value

                        ElseIf Element = "????" Then

                        Else
                            Dim txt As String
                            txt = "Unknown Element '" & Element & "'"
                            MsgBox(txt)
                            unknown = unknown + 1
                            If unknown > 8 Then
                                txt = "Too Many Unknown Elements."
                                MsgBox(txt)
                                reader.Close()
                                Exit Sub
                            End If

                        End If

                        Debug.WriteLine(reader.Value)

                    Case XmlNodeType.EndElement 'Display end of element.
                        Debug.Write("</" + reader.Name) : Debug.WriteLine(">")
                End Select

            Loop


            reader.Close()

            Application.win.optInterior.IsChecked = False   'v3.0 002 YC 052620
            Application.win.optEdge.IsChecked = False

            If LoadCase = "InteriorLoad" Then
                Application.win.optInterior.IsChecked = True
                Application.win.optEdge.IsChecked = False
            End If

            If LoadCase = "EdgeLoad" Then
                Application.win.optInterior.IsChecked = False
                Application.win.optEdge.IsChecked = True
            End If

            If LoadingType = "Moving Load" Then
                Application.win.optInterior.IsChecked = False
                Application.win.optEdge.IsChecked = False
            End If

            Application.win.XCoord = gXcoord1
            Application.win.YCoord = gYcoord1

            Application.win.XCoordMoved = gXcoord2
            Application.win.YCoordMoved = gYcoord2
            NMovingStep = NGearPositions

            EqJStfX = JntStfX1
            EqJStfY = JntStfY1

            'Tab 2 Pavement Structure
            Application.win.txtXCoord.Text = CStr(gXcoord1)
            Application.win.txtYCoord.Text = CStr(gYcoord1)

            Application.win.txtXCoordMoved.Text = CStr(gXcoord2)
            Application.win.txtYCoordMoved.Text = CStr(gYcoord2)
            Application.win.txtNGearPositons.Text = CStr(NGearPositions)

            Application.win.cmbLoadingType.Text = LoadingType

            'Tab 3 Joint Modeling
            Application.win.txtJntStfX.Text = CStr(JntStfX1)
            Application.win.txtJntStfY.Text = CStr(JntStfY1)

            Application.win.txtEdgStfX.Text = CStr(EqEdgStfX)
            Application.win.txtEdgStfY.Text = CStr(EqEdgStfY)



            LibIndex = LibACGroup(iGroup + 1) + CShort(iList) ' Library index
            Call UpdateDataFromLibrary(LibIndex)

            Application.win.ListBoxAirplaneGroup.SelectedIndex = iGroup
            Application.win.ListBoxLibraryAirplane.SelectedIndex = iList

            'Tab 1 Vehicle Selection
            GrossWeight = GrossWeight1
            PcntOnMainGears = PcntOnMainGears1
            NMainGears = NMainGears1
            NWheels = NWheels1
            TirePressure = TirePressure1


            For K = 1 To NWheels
                XWheels(K) = XX(K)
                YWheels(K) = YY(K)
            Next

            Call Application.win.PlotGear()

        Catch ex As Exception

            Dim txt As String
            txt = ex.Message
            txt = txt + Environment.NewLine + Environment.NewLine
            txt = txt + ex.StackTrace
            txt = txt + Environment.NewLine + Environment.NewLine
            MsgBox(txt)

        End Try

    End Sub

    'TODO (WPF): Commented out, please uncomment and replace frmGear references.
    ' Is this one even needed?
    Public Sub test()

        ''Dim sender As System.Object
        ''Dim e As System.EventArgs
        ''Call frmGear.cmdAddALayer_Click(sender, e)

        'Call frmGear.cmdDeleteALayer_Click(New System.Object, New System.EventArgs)
    End Sub

End Module

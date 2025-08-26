' added for moving load by YC 041015

Imports Microsoft.Win32
Imports System.Data.OleDb
Imports System.Data


Public Class WindowStress
    Public ResidualStressTable As New System.Data.DataTable
    Public NResidualStressRow, NResidualStressColumn As Integer
    Public rowselectedRS, columnselectedRS As Integer

    Public CoordFieldPoint(,) As double
    Dim NFieldPoint As Integer

    Public FieldPointTable As New System.Data.DataTable
    Dim colIndexFieldPoint As DataColumn
    Dim colxFieldPoint As DataColumn
    Dim colyFieldPoint As DataColumn
    Dim colzFieldPoint As DataColumn
    Public rowselectedFP, columnselectedFP As Integer

    Public LoadingStress(,,), TotalStress(,,) As Double, IsLoadingStress() As Boolean

    'Public NStep As Integer    ' YC 040620-2
    Dim NStep As Integer = NAC

    Public StressOutputTable As New System.Data.DataTable
    Dim colIndexStep As DataColumn
    Dim colStressXX As DataColumn
    Dim colStressYY As DataColumn
    Dim colStressZZ As DataColumn
    Dim colStressXY As DataColumn
    Dim colStressYZ As DataColumn
    Dim colStressXZ As DataColumn

    Public IsFpStressRun As Boolean = False, IsOutputStress As Boolean = False

    Dim gFieldPointFile As String '= gPrintOutFolder & "\Field Point.txt"    ' YC 040620-1
    Dim gFieldPointStressFile As String '= gPrintOutFolder & "\Stress.dat"

    Private Sub WindowStress_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded

        Me.Title = "    Job Name: " & JobName   ' YC 040620-1


        'txtFieldPointDirectory.Text = Environment.CurrentDirectory & "\field point.txt" ' YC 040620-1


        With grdFieldPoint
            colIndexFieldPoint = New DataColumn("IndexFieldPoint")
            colxFieldPoint = New DataColumn("xFieldPoint")
            colyFieldPoint = New DataColumn("yFieldPoint")
            colzFieldPoint = New DataColumn("zFieldPoint")

            FieldPointTable.Columns.Add(colIndexFieldPoint)
            FieldPointTable.Columns.Add(colxFieldPoint)
            FieldPointTable.Columns.Add(colyFieldPoint)
            FieldPointTable.Columns.Add(colzFieldPoint)
        End With

        grdFieldPoint.ItemsSource = FieldPointTable.AsDataView()
        NFieldPoint = FieldPointTable.Rows.Count
        rowselectedFP = -1


        With grdStressOutPut
            colIndexStep = New DataColumn("IdxStep")
            colStressXX = New DataColumn("StressXX")
            colStressYY = New DataColumn("StressYY")
            colStressZZ = New DataColumn("StressZZ")
            colStressXY = New DataColumn("StressXY")
            colStressYZ = New DataColumn("StressYZ")
            colStressXZ = New DataColumn("StressXZ")

            StressOutputTable.Columns.Add(colIndexStep)
            StressOutputTable.Columns.Add(colStressXX)
            StressOutputTable.Columns.Add(colStressYY)
            StressOutputTable.Columns.Add(colStressZZ)
            StressOutputTable.Columns.Add(colStressXY)
            StressOutputTable.Columns.Add(colStressYZ)
            StressOutputTable.Columns.Add(colStressXZ)
        End With

        grdStressOutPut.ItemsSource = StressOutputTable.AsDataView()
        rowselectedRS = -1

    End Sub


    Private Sub ChkResidualStress_Checked(sender As Object, e As RoutedEventArgs) Handles ChkResidualStress.Checked, ChkResidualStress.Unchecked

        Dim openFileDialog1 As New OpenFileDialog()
        Dim gResidualStressFileFolder As String

        If ChkResidualStress.IsChecked Then
            openFileDialog1.InitialDirectory = gJobFileFolder
            openFileDialog1.Filter = "Residual Stress File (*.xls;*.xlsx)|*.xls;*.xlsx|All files (*.*)|*.*"
            openFileDialog1.FilterIndex = 1
            openFileDialog1.RestoreDirectory = True
            openFileDialog1.DefaultExt = ".xls"

            If openFileDialog1.ShowDialog() Then

                Dim lFileLength As Integer, lLastPos As Integer
                lLastPos = openFileDialog1.FileName.LastIndexOf("\")
                lFileLength = openFileDialog1.FileName.Length - lLastPos

                gResidualStressFileFolder = openFileDialog1.FileName.Remove(lLastPos, lFileLength)
                txtResidualStressDirectory.Text = gResidualStressFileFolder
            Else
                ChkResidualStress.IsChecked = False     ' YC 040620-1
                Exit Sub
            End If

            Dim connection As New OleDbConnection()
            Dim command As New OleDbCommand()

            connection.ConnectionString = "Provider=Microsoft.ACE.OLEDB.12.0 ;Data Source=" & openFileDialog1.FileName & ";Extended Properties=""Excel 12.0 Xml;HDR=YES;"""
            command.CommandText = "select * from [Sheet1$]"
            command.Connection = connection

            Dim da As New OleDbDataAdapter(command)

            da.Fill(ResidualStressTable)
            connection.Close()

        Else
            ResidualStressTable.Clear()
            txtResidualStressDirectory.Clear()
        End If

        grdResidualStress.ItemsSource = ResidualStressTable.AsDataView()

        NResidualStressRow = ResidualStressTable.Rows.Count
        NResidualStressColumn = ResidualStressTable.Columns.Count

    End Sub

    Private Sub grdResidualStress_RequestBringIntoView(sender As Object, e As RequestBringIntoViewEventArgs) Handles grdResidualStress.RequestBringIntoView

        rowselectedRS = grdResidualStress.SelectedIndex

        If IsOutputStress Then
            If optLoadInducedStress.IsChecked Then
                optLoadInducedStress.IsChecked = False
                optLoadInducedStress.IsChecked = True
            End If
            If optTotalStress.IsChecked Then
                optTotalStress.IsChecked = False
                optTotalStress.IsChecked = True
            End If
        End If

    End Sub

    Private Sub grdResidualStress_MouseLeftButtonDown(sender As Object, e As MouseButtonEventArgs) Handles grdResidualStress.MouseLeftButtonDown

        If (Not IsDBNull(sender)) Then
            Dim grid As DataGrid = sender
            If (Not IsDBNull(grid)) And (Not IsDBNull(grid.SelectedItems)) And grid.SelectedItems.Count = 1 Then
                Dim dgr As DataGridRow = grid.ItemContainerGenerator.ContainerFromItem(grid.SelectedItem)
                If (dgr.IsMouseOver) Then
                    dgr.IsSelected = False
                End If
            End If
        End If

        rowselectedRS = -1

    End Sub


    Private Sub btnAddFieldPoint_Click(sender As Object, e As RoutedEventArgs) Handles btnAddFieldPoint.Click


        txtFieldPointDirectory.Text = "" ' YC 040620-1


        NFieldPoint = FieldPointTable.Rows.Count
        NFieldPoint = NFieldPoint + 1

        Dim rowi As DataRow
        rowi = FieldPointTable.NewRow

        If rowselectedFP = -1 Then
            FieldPointTable.Rows.Add(rowi)
            FieldPointTable(NFieldPoint - 1)(0) = NFieldPoint
        Else
            FieldPointTable.Rows.InsertAt(rowi, rowselectedFP + 1)

            Dim I As Integer
            For I = rowselectedFP + 1 To NFieldPoint
                FieldPointTable(I - 1)(0) = I
            Next I
        End If

        IsFpStressRun = False
        IsOutputStress = False
        'btnFpStress.Background = Brushes.Aqua
        optLoadInducedStress.IsChecked = False
        optTotalStress.IsChecked = False
        StressOutputTable.Clear()

    End Sub

    Private Sub btnRemoveFieldPoint_Click(sender As Object, e As RoutedEventArgs) Handles btnRemoveFieldPoint.Click


        txtFieldPointDirectory.Text = "" ' YC 040620-1


        NFieldPoint = FieldPointTable.Rows.Count
        NFieldPoint = NFieldPoint - 1

        If rowselectedFP = -1 Then
            MessageBox.Show("Please select a field point")
            Exit Sub
        Else
            FieldPointTable.Rows.RemoveAt(rowselectedFP)

            Dim I As Integer
            For I = rowselectedFP + 1 To NFieldPoint
                FieldPointTable(I - 1)(0) = I
            Next I

            rowselectedFP = grdFieldPoint.SelectedIndex
        End If

        IsFpStressRun = False
        IsOutputStress = False
        'btnFpStress.Background = Brushes.Aqua
        optLoadInducedStress.IsChecked = False
        optTotalStress.IsChecked = False
        StressOutputTable.Clear()

    End Sub

    Private Sub btnNewFieldPoint_Click(sender As Object, e As RoutedEventArgs) Handles btnNewFieldPoint.Click


        txtFieldPointDirectory.Text = "" ' YC 040620-1


        FieldPointTable.Clear()
        NFieldPoint = FieldPointTable.Rows.Count
        rowselectedFP = -1

        IsFpStressRun = False
        IsOutputStress = False
        'btnFpStress.Background = Brushes.Aqua
        optLoadInducedStress.IsChecked = False
        optTotalStress.IsChecked = False
        StressOutputTable.Clear()

    End Sub

    Private Sub btnOpenFieldPoint_Click(sender As Object, e As RoutedEventArgs) Handles btnOpenFieldPoint.Click

        FieldPointTable.Clear()
        rowselectedFP = -1

        Dim I As Integer
        Dim filexisted As Boolean

        Try


            ' 040620-1
            'filexisted = System.IO.File.Exists(My.Application.Info.DirectoryPath & "\" & "field point.txt") 

            'If Not filexisted Then
            '    MessageBox.Show("File does not exist, please input field point manually!", "Warning!", MessageBoxButton.OK, MessageBoxImage.Warning)
            '    Exit Sub
            'End If

            'FileOpen(11, "field point.txt", OpenMode.Input)  

            Dim openFileDialog1 As New OpenFileDialog()

            openFileDialog1.InitialDirectory = gPrintOutFolder
            openFileDialog1.Filter = "Field Point.txt; Field Point.dat|Field Point.txt; Field Point.dat|All files (*.*)|*.*"
            openFileDialog1.FilterIndex = 1
            openFileDialog1.RestoreDirectory = True
            openFileDialog1.DefaultExt = ".txt"

            If Not openFileDialog1.ShowDialog() Then
                Exit Sub
            End If

            gFieldPointFile = openFileDialog1.FileName
            txtFieldPointDirectory.Text = gFieldPointFile

            FileOpen(11, gFieldPointFile, OpenMode.Input)
            ' 040620-1 END






            FileSystem.Input(11, NFieldPoint)

            ReDim CoordFieldPoint(NFieldPoint, 3)

            For I = 1 To NFieldPoint
                FileSystem.Input(11, CoordFieldPoint(I, 1))
                FileSystem.Input(11, CoordFieldPoint(I, 2))
                FileSystem.Input(11, CoordFieldPoint(I, 3))
            Next I

            For I = 1 To NFieldPoint
                Dim rowi As DataRow
                rowi = FieldPointTable.NewRow
                FieldPointTable.Rows.Add(rowi)
                FieldPointTable(I - 1)(0) = I
                FieldPointTable(I - 1)(1) = CoordFieldPoint(I, 1)
                FieldPointTable(I - 1)(2) = CoordFieldPoint(I, 2)
                FieldPointTable(I - 1)(3) = CoordFieldPoint(I, 3)
            Next I

            FileClose(11)

            IsFpStressRun = False
            IsOutputStress = False
            'btnFpStress.Background = Brushes.Aqua
            optLoadInducedStress.IsChecked = False
            optTotalStress.IsChecked = False
            StressOutputTable.Clear()

        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try

    End Sub

    Private Sub btnSaveFieldPoint_Click(sender As Object, e As RoutedEventArgs) Handles btnSaveFieldPoint.Click

        Try


            ' 040620-1
            'FileOpen(11, "field point.txt", OpenMode.Output)  

            Dim saveFileDialog1 As New SaveFileDialog()

            saveFileDialog1.InitialDirectory = gPrintOutFolder
            saveFileDialog1.Filter = "Field Point.txt; Field Point.dat|Field Point.txt; Field Point.dat|All files (*.*)|*.*"
            saveFileDialog1.FilterIndex = 1
            saveFileDialog1.RestoreDirectory = True
            saveFileDialog1.DefaultExt = ".txt"

            If Not saveFileDialog1.ShowDialog() Then
                Exit Sub
            End If

            gFieldPointFile = saveFileDialog1.FileName
            txtFieldPointDirectory.Text = gFieldPointFile

            FileOpen(11, gFieldPointFile, OpenMode.Output)
            ' 040620-1


            PrintLine(11, NFieldPoint)

            Dim I As Integer
            For I = 1 To NFieldPoint
                PrintLine(11, FieldPointTable(I - 1)(1) & "," & FieldPointTable(I - 1)(2) & "," & FieldPointTable(I - 1)(3))
            Next I

            FileClose(11)


            IsFpStressRun = False
            IsOutputStress = False
            'btnFpStress.Background = Brushes.Aqua
            optLoadInducedStress.IsChecked = False
            optTotalStress.IsChecked = False
            StressOutputTable.Clear()

        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try

    End Sub

    Private Sub grdFieldPoint_CellEditEnding(sender As Object, e As DataGridCellEditEndingEventArgs) Handles grdFieldPoint.CellEditEnding

        Dim element As TextBox = e.EditingElement
        Dim text As String = element.Text

        columnselectedFP = e.Column.DisplayIndex()

        Dim OldValue As Double, OldValueExisted As Boolean
        If Not IsDBNull(FieldPointTable(rowselectedFP)(columnselectedFP)) Then
            OldValue = FieldPointTable(rowselectedFP)(columnselectedFP)
            OldValueExisted = True
        End If

        If IsNumeric(text) Then
            FieldPointTable(rowselectedFP)(columnselectedFP) = Convert.ToSingle(text)
        Else
            MessageBox.Show("Please input numeric value")
            FieldPointTable(rowselectedFP)(columnselectedFP) = DBNull.Value
            Exit Sub
        End If

        If OldValueExisted And Not OldValue = FieldPointTable(rowselectedFP)(columnselectedFP) Then
            IsFpStressRun = False
            IsOutputStress = False
            'btnFpStress.Background = Brushes.Aqua
            optLoadInducedStress.IsChecked = False
            optTotalStress.IsChecked = False
            StressOutputTable.Clear()
        End If


    End Sub

    Private Sub grdFieldPoint_MouseLeftButtonDown(sender As Object, e As MouseButtonEventArgs) Handles grdFieldPoint.MouseLeftButtonDown

        If (Not IsDBNull(sender)) Then
            Dim grid As DataGrid = sender
            If (Not IsDBNull(grid)) And (Not IsDBNull(grid.SelectedItems)) And grid.SelectedItems.Count = 1 Then
                Dim dgr As DataGridRow = grid.ItemContainerGenerator.ContainerFromItem(grid.SelectedItem)
                If (dgr.IsMouseOver) Then
                    dgr.IsSelected = False
                End If
            End If
        End If

        rowselectedFP = -1
    End Sub

    Private Sub grdFieldPoint_RequestBringIntoView(sender As Object, e As RequestBringIntoViewEventArgs) Handles grdFieldPoint.RequestBringIntoView

        rowselectedFP = grdFieldPoint.SelectedIndex

        If IsOutputStress Then
            If optLoadInducedStress.IsChecked Then
                optLoadInducedStress.IsChecked = False
                optLoadInducedStress.IsChecked = True
            End If
            If optTotalStress.IsChecked Then
                optTotalStress.IsChecked = False
                optTotalStress.IsChecked = True
            End If
        End If

    End Sub

    '    Private Sub btnNikeStress_Click(sender As Object, e As RoutedEventArgs) Handles btnFpStress.Click

    '        NFieldPoint = FieldPointTable.Rows.Count

    '        If NFieldPoint = 0 Then
    '            MessageBox.Show("Please input field point")
    '            Exit Sub
    '        End If

    '        Dim i As Integer
    '        For i = 1 To NFieldPoint
    '            If IsDBNull(FieldPointTable(i - 1)(1)) Then
    '                MessageBox.Show("Please input numeric value for field point #" & i & ", coordinate X")
    '                Exit Sub
    '            ElseIf IsDBNull(FieldPointTable(i - 1)(2)) Then
    '                MessageBox.Show("Please input numeric value for field point #" & i & ", coordinate Y")
    '                Exit Sub
    '            ElseIf IsDBNull(FieldPointTable(i - 1)(3)) Then
    '                MessageBox.Show("Please input numeric value for field point #" & i & ", coordinate Z")
    '                Exit Sub
    '            End If
    '        Next i

    '        Call SaveFieldPoint()

    '        GoTo newProcedurelabel  ' YC 040620-2


    '        Dim do2 As Boolean
    '        Dim Ubound2, counter2 As Integer

    '        Do
    '            do2 = True
    '            Dim processes() As Process
    '            Dim instance As Process
    '            Dim process As New Process()
    '            processes = Process.GetProcesses

    '            do2 = False

    '            Ubound2 = UBound(processes)
    '            counter2 = 0

    '            For Each instance In processes
    '                counter2 = counter2 + 1
    '                If UCase(instance.ProcessName) = "NIKEStress" Then
    '                    do2 = True
    '                    System.Windows.Forms.Application.DoEvents()
    '                    MsgBox("Wait till NIKEStress finish running.", MsgBoxStyle.OkOnly, "'Run NIKEStress' button clicked.")
    '                    Exit Sub
    '                End If
    '                If counter2 > Ubound2 Then
    '                    Exit Do
    '                End If

    '            Next
    '            System.Windows.Forms.Application.DoEvents()
    '        Loop While do2 Or Not exitLoop Or counter2 > Ubound2
    '        '==========================================================




    '        btnNewFieldPoint.IsEnabled = False
    '        btnOpenFieldPoint.IsEnabled = False
    '        btnAddFieldPoint.IsEnabled = False
    '        btnRemoveFieldPoint.IsEnabled = False



    '        Dim RetVal As Object
    '        'Microsoft Scripting has to be referenced

    '        System.Windows.Forms.Application.DoEvents()

    '        RetVal = Microsoft.VisualBasic.Shell(My.Application.Info.DirectoryPath & "\Library\" & "NIKEStress.exe", AppWinStyle.NormalNoFocus) ' YC 040620-2



    'startAgain:
    '        Dim do1 As Boolean
    '        Dim Ubound1 As Integer
    '        Dim counter1 As Integer

    '        Do
    '            do1 = True
    '            Dim processes1() As Process
    '            Dim instance1 As Process
    '            Dim process1 As New Process()
    '            processes1 = Process.GetProcesses

    '            do1 = False


    '            Ubound1 = UBound(processes1)
    '            counter1 = 0

    '            For Each instance1 In processes1
    '                counter1 = counter1 + 1
    '                If instance1.ProcessName = "NIKEStress" Then
    '                    do1 = True
    '                    System.Windows.Forms.Application.DoEvents()
    '                    System.Threading.Thread.Sleep(2000)
    '                    GoTo startAgain
    '                End If
    '                If counter1 > Ubound1 Then
    '                    Exit Do
    '                End If

    '            Next
    '            System.Windows.Forms.Application.DoEvents()
    '        Loop While do1 Or Not exitLoop Or counter1 > Ubound1
    '        'System.Threading.Thread.Sleep(2000)


    'newProcedurelabel:      ' YC 040620-2
    '        Call FiledPointStressCalcultion()

    '        btnNewFieldPoint.IsEnabled = True
    '        btnOpenFieldPoint.IsEnabled = True
    '        btnAddFieldPoint.IsEnabled = True
    '        btnRemoveFieldPoint.IsEnabled = True

    '        IsFpStressRun = True
    '        'btnFpStress.Background = btnAddFieldPoint.Background
    '        btnOutputStress.Background = Brushes.Aqua

    '        'Call ReadLoadingStress()   ' YC 040620-2

    '    End Sub


    Private Sub btnOutputStress_Click(sender As Object, e As RoutedEventArgs) Handles btnOutputStress.Click

        ' YC 040620-1
        NFieldPoint = FieldPointTable.Rows.Count

        If NFieldPoint = 0 Then
            MessageBox.Show("Please input field point")
            Exit Sub
        End If

        Dim i As Integer
        For i = 1 To NFieldPoint
            If IsDBNull(FieldPointTable(i - 1)(1)) Then
                MessageBox.Show("Please input numeric value for field point #" & i & ", coordinate X")
                Exit Sub
            ElseIf IsDBNull(FieldPointTable(i - 1)(2)) Then
                MessageBox.Show("Please input numeric value for field point #" & i & ", coordinate Y")
                Exit Sub
            ElseIf IsDBNull(FieldPointTable(i - 1)(3)) Then
                MessageBox.Show("Please input numeric value for field point #" & i & ", coordinate Z")
                Exit Sub
            End If
        Next i
        ' YC 040620-1 END


        If IsFpStressRun = False Then

            ' YC 040620-1
            'MessageBox.Show("Please run NIKEStress")
            'Exit Sub
            Call FiledPointStressCalcultion()
            ' YC 040620-1 END

        End If

        If rowselectedFP < 0 Then
            MessageBox.Show("Please select a field point")
            Exit Sub
        End If

        IsOutputStress = True
        btnOutputStress.Background = btnAddFieldPoint.Background

        optLoadInducedStress.IsChecked = False
        optTotalStress.IsChecked = False

        optLoadInducedStress.IsChecked = True

    End Sub


    Private Sub optLoadInducedStress_Checked(sender As Object, e As RoutedEventArgs) Handles optLoadInducedStress.Checked, optLoadInducedStress.Unchecked

        If sender.IsChecked Then

            ' YC 040620-1
            'If IsFpStressRun = False Then
            '    MessageBox.Show("Please run NIKEStress")
            '    optLoadInducedStress.IsChecked = False
            '    Exit Sub
            'End If

            If IsOutputStress = False Then
                MessageBox.Show("Please run Output Stress")
                optLoadInducedStress.IsChecked = False
                Exit Sub
            End If
            'If rowselectedFP < 0 Then
            '    MessageBox.Show("Please select a field point")
            '    optLoadInducedStress.IsChecked = False
            '    Exit Sub
            'End If
            ' YC 040620-1 END

            If IsLoadingStress(rowselectedFP + 1) = False Then
                MessageBox.Show("Field point is not in the pavement, no load-induced stress computed!", "Warning!", MessageBoxButton.OK, MessageBoxImage.Warning)
            End If

            optTotalStress.IsChecked = False
            StressOutputTable.Clear()

            For IStep = 1 To NStep

                Dim rowi As DataRow
                rowi = StressOutputTable.NewRow
                StressOutputTable.Rows.Add(rowi)

                StressOutputTable(IStep - 1)(0) = IStep

                For j = 1 To 6
                    'StressOutputTable(IStep - 1)(j) = LoadingStress(IStep, j, rowselectedFP + 1)        ' YC 040620-2-1
                    StressOutputTable(IStep - 1)(j) = Format(LoadingStress(IStep, j, rowselectedFP + 1), "F3")
                Next j

            Next IStep
        End If

    End Sub

    Private Sub optTotalStress_Checked(sender As Object, e As RoutedEventArgs) Handles optTotalStress.Checked

        If sender.IsChecked Then

            ' YC 040620-1
            'If IsFpStressRun = False Then
            '    MessageBox.Show("Please run NIKEStress")
            '    optTotalStress.IsChecked = False
            '    Exit Sub
            'End If

            If IsOutputStress = False Then
                MessageBox.Show("Please run Output Stress")
                optTotalStress.IsChecked = False
                Exit Sub
            End If
            'If rowselectedFP < 0 Then
            '    MessageBox.Show("Please select a field point")
            '    optTotalStress.IsChecked = False
            '    Exit Sub
            'End If
            ' YC 040620-1 END


            If IsLoadingStress(rowselectedFP + 1) = False Then
                MessageBox.Show("Field point is not in the pavement, no load-induced stress computed!", "Warning!", MessageBoxButton.OK, MessageBoxImage.Warning)
            End If

            If Not ChkResidualStress.IsChecked Then
                MessageBox.Show("Please import residual stress")
                optTotalStress.IsChecked = False
                Exit Sub
            End If

            If rowselectedRS < 0 Then
                MessageBox.Show("Please select a residual stress")
                optTotalStress.IsChecked = False
                Exit Sub
            End If

            optLoadInducedStress.IsChecked = False
            StressOutputTable.Clear()

            If ((FieldPointTable(rowselectedFP)(1) - ResidualStressTable(rowselectedRS)(0)) ^ 2 +
            (FieldPointTable(rowselectedFP)(2) - ResidualStressTable(rowselectedRS)(1)) ^ 2 +
            (FieldPointTable(rowselectedFP)(3) - ResidualStressTable(rowselectedRS)(2)) ^ 2) > 0.01 Then

                MessageBox.Show("Field point does not coincide with the residual stress location!", "Warning!", MessageBoxButton.OK, MessageBoxImage.Warning)

            End If


            ' YC 040620-1
            'Call ReadTotalStress()
            ReDim TotalStress(NStep, 6, NFieldPoint)

            For Istep = 1 To NStep
                TotalStress(Istep, 1, rowselectedFP + 1) = LoadingStress(Istep, 1, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(3) 'sig-xx
                TotalStress(Istep, 2, rowselectedFP + 1) = LoadingStress(Istep, 2, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(4) 'sig-yy
                TotalStress(Istep, 3, rowselectedFP + 1) = LoadingStress(Istep, 3, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(5) 'sig-zz
                TotalStress(Istep, 4, rowselectedFP + 1) = LoadingStress(Istep, 4, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(6) 'sig-xy
                TotalStress(Istep, 5, rowselectedFP + 1) = LoadingStress(Istep, 5, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(7) 'sig-yz
                TotalStress(Istep, 6, rowselectedFP + 1) = LoadingStress(Istep, 6, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(8) 'sig-xz
            Next
            ' YC 040620-1 END


            For IStep = 1 To NStep

                Dim rowi As DataRow
                rowi = StressOutputTable.NewRow
                StressOutputTable.Rows.Add(rowi)

                StressOutputTable(IStep - 1)(0) = IStep

                For j = 1 To 6
                    'StressOutputTable(IStep - 1)(j) = TotalStress(IStep, j, rowselectedFP + 1)     ' YC 040620-2-1
                    StressOutputTable(IStep - 1)(j) = Format(TotalStress(IStep, j, rowselectedFP + 1), "f3")
                Next j

            Next IStep

        End If

    End Sub


    Private Sub cmdExit_Click(sender As Object, e As RoutedEventArgs) Handles cmdExit.Click

        Me.Close()

    End Sub


    'Sub SaveFieldPoint()


    '    'FileOpen(11, "field point.txt", OpenMode.Output)   ' YC 040620-1
    '    FileOpen(11, gFieldPointFile, OpenMode.Output)


    '    PrintLine(11, NFieldPoint)

    '    Dim I As Integer
    '    For I = 1 To NFieldPoint
    '        PrintLine(11, FieldPointTable(I - 1)(1) & "," & FieldPointTable(I - 1)(2) & "," & FieldPointTable(I - 1)(3))
    '    Next I

    '    FileClose(11)
    'End Sub

    'Sub ReadLoadingStress()

    '    Dim fso As New Scripting.FileSystemObject
    '    Dim fil As Boolean


    '    'fil = fso.FileExists(My.Application.Info.DirectoryPath & "\" & "Stress Output.dat")    ' YC 040620-1
    '    fil = fso.FileExists(gFieldPointStressFile)


    '    If Not fil Then
    '        Dim msg1 As String
    '        msg1 = "File Stress Output.dat is missing." & Environment.NewLine & "Click on button 'Run NIKEStress'."
    '        MsgBox(msg1, MsgBoxStyle.OkOnly, "Message")

    '        Exit Sub
    '    End If

    '    FileOpen(12, "Stress Output.dat", OpenMode.Input)

    '    Dim IStep, IFieldPoint As Integer
    '    Dim StrLine As String

    '    StrLine = LineInput(12) '  " Number of Field Points:   
    '    StrLine = LineInput(12) '  " Number of Steps:   
    '    NStep = CInt(Right(StrLine, StrLine.Length - 17))
    '    StrLine = LineInput(12)

    '    ReDim LoadingStress(NStep, 6, NFieldPoint)
    '    ReDim IsLoadingStress(NFieldPoint)

    '    For IFieldPoint = 1 To NFieldPoint
    '        StrLine = LineInput(12) ' "at field point: 
    '        StrLine = LineInput(12)

    '        If StrLine = " ***************Stress Output*******************" Then 'field point in the pavement 
    '            IsLoadingStress(IFieldPoint) = True
    '            StrLine = LineInput(12) ' "Field point in PCC slab:"
    '            StrLine = LineInput(12) ' " Step#    sig-xx     sig-yy     sig-zz     sig-xy     sig-yz     sig-zx"

    '            For IStep = 1 To NStep
    '                FileSystem.Input(12, IStep)
    '                FileSystem.Input(12, LoadingStress(IStep, 1, IFieldPoint))
    '                FileSystem.Input(12, LoadingStress(IStep, 2, IFieldPoint))
    '                FileSystem.Input(12, LoadingStress(IStep, 3, IFieldPoint))
    '                FileSystem.Input(12, LoadingStress(IStep, 4, IFieldPoint))
    '                FileSystem.Input(12, LoadingStress(IStep, 5, IFieldPoint))
    '                FileSystem.Input(12, LoadingStress(IStep, 6, IFieldPoint))
    '            Next
    '        ElseIf StrLine = "This field point is not in the pavement" Then ' not in the paveemnt
    '            IsLoadingStress(IFieldPoint) = False
    '            For IStep = 1 To NStep
    '                LoadingStress(IStep, 1, IFieldPoint) = 0
    '                LoadingStress(IStep, 2, IFieldPoint) = 0
    '                LoadingStress(IStep, 3, IFieldPoint) = 0
    '                LoadingStress(IStep, 4, IFieldPoint) = 0
    '                LoadingStress(IStep, 5, IFieldPoint) = 0
    '                LoadingStress(IStep, 6, IFieldPoint) = 0
    '            Next
    '        End If

    '        StrLine = LineInput(12)
    '    Next

    '    FileClose(12)

    'End Sub

    'Sub ReadTotalStress()

    '    ReDim TotalStress(NStep, 6, NFieldPoint)

    '    For Istep = 1 To NStep
    '        TotalStress(Istep, 1, rowselectedFP + 1) = LoadingStress(Istep, 1, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(3) 'sig-xx
    '        TotalStress(Istep, 2, rowselectedFP + 1) = LoadingStress(Istep, 2, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(4) 'sig-yy
    '        TotalStress(Istep, 3, rowselectedFP + 1) = LoadingStress(Istep, 3, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(5) 'sig-zz
    '        TotalStress(Istep, 4, rowselectedFP + 1) = LoadingStress(Istep, 4, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(6) 'sig-xy
    '        TotalStress(Istep, 5, rowselectedFP + 1) = LoadingStress(Istep, 5, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(7) 'sig-yz
    '        TotalStress(Istep, 6, rowselectedFP + 1) = LoadingStress(Istep, 6, rowselectedFP + 1) + ResidualStressTable(rowselectedRS)(8) 'sig-xz
    '    Next

    'End Sub

    'If Not IsDBNull(ResidualStressTable(I)(J)) Then

    'My.Application.Info.DirectoryPath


    'Dim ResidualStressFileLines() As String
    'ResidualStressFileLines = System.IO.File.ReadAllLines(openFileDialog1.FileName)
    'iResidualStressLen = ResidualStressFileLines.Length



    'Try
    '    connection.Open()
    '    'command.ExecuteNonQuery()

    '    Dim reader As OleDbDataReader = command.ExecuteReader()

    '    While reader.Read()

    '        ReDim xResidual(3), sigResidual(2)

    '        xResidual(1) = Convert.ToSingle(reader.Item("x"))
    '        xResidual(2) = Convert.ToSingle(reader.Item("y"))
    '        xResidual(3) = Convert.ToSingle(reader.Item("z"))
    '        sigResidual(1) = Convert.ToSingle(reader.Item("sig-xx"))
    '        sigResidual(2) = Convert.ToSingle(reader.Item("sig-yy"))


    '    End While

    '    reader.Close()



    'Catch ex As Exception
    '    Console.WriteLine(ex.Message)
    'End Try


    'If (isManualEditCommit = False) Then
    '    isManualEditCommit = True
    '    grdFieldPoint.CommitEdit(DataGridEditingUnit.Cell, True)
    '    isManualEditCommit = False
    'End If

    'Dim rowView As DataRowView = e.Row.Item
    'rowBeingEdited = rowView


    Sub FiledPointStressCalcultion()
        ' YC 040620-2


        '**********************************************************   
        '**************** transfer node infromation ***************
        Dim x(3, NNd) As Double
        Dim ind As Integer
        For ind = 1 To NNd
            x(1, ind) = Nd(ind).X
            x(2, ind) = Nd(ind).Y
            x(3, ind) = Nd(ind).Z
        Next ind

        '**********************************************************   
        '************ transfer brick element infromation **********
        Dim NEle = NBrckEle
        Dim IdxEleNode(9, NEle) As Integer
        Dim iEle As Integer, j As Integer
        For iEle = 1 To NEle
            IdxEleNode(1, iEle) = BrickElement(iEle).IdxMat
            For j = 2 To 9
                IdxEleNode(j, iEle) = BrickElement(iEle).Node(j - 1)
            Next j
        Next iEle




        '**********************************************************   
        '***** read in "Output-Hexahedron Element-Step #" file ****
        Dim StressEleIP(9, 8, NEle, NStep) As Double

        Dim istep, ip As Integer
        Dim StrLine, elements() As String, L As Integer
        'Dim readText() As String
        For istep = 1 To NStep

            Dim HexahedronStressFile As String = gPrintOutFolder & "\Output-Hexahedron Element-Step "
            HexahedronStressFile = HexahedronStressFile & istep & ".txt"

            If System.IO.File.Exists(HexahedronStressFile) Then
                'FileOpen(200, HexahedronStressFile, OpenMode.Input)

                'StrLine = LineInput(200)
                'StrLine = LineInput(200) ' " Rigid Vehicle Pavement"
                'StrLine = LineInput(200) ' "                   FAASR3D (version 1.0 ) compiled 04/15/19"
                'StrLine = LineInput(200) ' " e l e m e n t   s t r e s s   c a l c u l a t i o n s  f o r   s t e p     # ( time = #.0000E+00 )"
                'StrLine = LineInput(200) ' " element  stress      sig-xx     sig-yy     sig-zz     sig-xy     sig-yz     sig-zx              yield"
                'StrLine = LineInput(200) ' " num/ipt   state                                                                       effsg     function" 

                Dim readText() As String = IO.File.ReadAllLines(HexahedronStressFile)

                'For iEle = 1 To NEle
                '    StrLine = LineInput(200) ' "iEle-iMat"
                'For L = 1 To readText.Length
                L = 0

readlinelabel:  StrLine = readText(L)

                If Strings.Mid(StrLine, 6, 1) = "-" Then

                    iEle = CInt(Strings.Left(StrLine, 5))

                    For ip = 1 To 9
                        L = L + 1
                        StrLine = readText(L)
                        elements = Strings.Right(StrLine, 88).Split(New Char() {" "c},
                                        StringSplitOptions.RemoveEmptyEntries)  ' to avoid "temp"

                        For j = 1 To 8
                            StressEleIP(ip, j, iEle, istep) = elements(j - 1)
                        Next j

                        'FileSystem.Input(200, ip)
                        'FileSystem.Input(200, StressEleIP(ip, 1, iEle, istep))  ' sig-xx
                        'FileSystem.Input(200, StressEleIP(ip, 2, iEle, istep))  ' sig-yy
                        'FileSystem.Input(200, StressEleIP(ip, 3, iEle, istep))  ' sig-zz
                        'FileSystem.Input(200, StressEleIP(ip, 4, iEle, istep))  ' sig-xy
                        'FileSystem.Input(200, StressEleIP(ip, 5, iEle, istep))  ' sig-yz
                        'FileSystem.Input(200, StressEleIP(ip, 6, iEle, istep))  ' sig-zx
                        'FileSystem.Input(200, StressEleIP(ip, 7, iEle, istep))  ' effsg
                        'FileSystem.Input(200, StressEleIP(ip, 8, iEle, istep))  ' yield function
                    Next ip

                    'Next iEle

                End If

                L = L + 1

                If L < readText.Length Then
                    GoTo readlinelabel
                End If
                'Next L

            End If

        Next istep



        ReDim LoadingStress(NStep, 6, NFieldPoint)  ' YC 040620-2-1
        ReDim IsLoadingStress(NFieldPoint)


        '**********************************************************   
        '******************** identify field point ****************
        Dim NFp = NFieldPoint
        Dim tol = 0.0001


        Dim Fpx(3, NFp), xmin(3), xmax(3), ksi(3) As Double
        Dim psiEleFp(8, 8) As Double
        'Dim IdxEleFp(8), NEleFpIMat(12) As Integer
        Dim IEleFp, NEleFp, IsInPavement As Integer


        Dim ifp, i, m As Integer

        Dim iFpStart = 1
162:    For ifp = iFpStart To NFp
            For j = 1 To 3
                Fpx(j, ifp) = FieldPointTable(ifp - 1)(j)
            Next j


            IEleFp = 0
            'IdxEleFp = 0
            'NEleFpIMat = 0

            'For imat = 1 To 12
            '    NEleFpIMat(imat) = 0
            'Next imat
            Dim IdxEleFp(8), NEleFpIMat(12) As Integer

            IsInPavement = 0  ' to identify if in the pavement or not 

            For m = 1 To NEle
                For i = 1 To 3
                    xmin(i) = x(i, IdxEleNode(2, m))
                    xmax(i) = x(i, IdxEleNode(2, m))
                    For j = 3 To 9
                        If (x(i, IdxEleNode(j, m)) < xmin(i)) Then
                            xmin(i) = x(i, IdxEleNode(j, m))
                        End If
                        If (x(i, IdxEleNode(j, m)) > xmax(i)) Then
                            xmax(i) = x(i, IdxEleNode(j, m))
                        End If
                    Next j
                Next i

                If (Fpx(1, ifp) >= xmin(1) - tol And Fpx(1, ifp) <= xmax(1) + tol And
                    Fpx(2, ifp) >= xmin(2) - tol And Fpx(2, ifp) <= xmax(2) + tol And
                    Fpx(3, ifp) >= xmin(3) - tol And Fpx(3, ifp) <= xmax(3) + tol) Then

                    IEleFp = IEleFp + 1
                    IdxEleFp(IEleFp) = m
                    NEleFp = IEleFp

                    For imat = 1 To 12
                        If (IdxEleNode(1, m) = imat) Then
                            NEleFpIMat(imat) = NEleFpIMat(imat) + 1
                        End If
                    Next imat

                    For i = 1 To 3
                        ksi(i) = (2 * Fpx(i, ifp) - (xmin(i) + xmax(i))) / (xmax(i) - xmin(i))
                    Next i

                    For i = 1 To 8
                        psiEleFp(i, IEleFp) = psi(i, ksi)
                    Next i

                    IsInPavement = 1

                    IsLoadingStress(ifp) = True ' YC 040620-2-1

                End If

            Next m


            If (IsInPavement = 0) Then
                '        Write(21, 9000)
                'write(21,*)   

                IsLoadingStress(ifp) = False    ' YC 040620-2-1

                iFpStart = ifp + 1
                GoTo 162
            End If
            '******************** identify field point END ************       
            '**********************************************************   



            '**********************************************************                          
            '******************** process stress **********************  
            Dim psiFptemp(1, 8), StressEleFpIPtemp(8, 6), psiAtemp(8, 8), psiAInvtemp(8, 8) As Double
            Dim StressEleFpNodetemp(8, 6), StressEleFptemp(1, 6) As Double

            Dim StressFp(6, 12, NStep) As Double

            For istep = 1 To NStep
                For IEleFp = 1 To NEleFp
                    m = IdxEleFp(IEleFp)

                    For i = 1 To 8
                        psiFptemp(1, i) = psiEleFp(i, IEleFp)
                    Next i

                    For ip = 1 To 8
                        For j = 1 To 6
                            StressEleFpIPtemp(ip, j) = StressEleIP(ip, j, m, istep)
                        Next j
                    Next ip

                    For ip = 1 To 8
                        For i = 1 To 8
                            psiAtemp(i, ip) = psiIP(i, ip)
                        Next i
                    Next ip

                    psiAInvtemp = Inverse(psiAtemp)

                    StressEleFpNodetemp = MatrixProduct(psiAInvtemp, StressEleFpIPtemp)

                    StressEleFptemp = MatrixProduct(psiFptemp, StressEleFpNodetemp)


                    For imat = 1 To 12
                        If (NEleFpIMat(imat) <> 0) Then
                            If (IdxEleNode(1, m) = imat) Then
                                For j = 1 To 6
                                    StressFp(j, imat, istep) = StressFp(j, imat, istep) + StressEleFptemp(1, j)
                                Next j
                            End If
                        End If
                    Next imat


                    If (IEleFp = NEleFp) Then
                        For imat = 1 To 12
                            If (NEleFpIMat(imat) <> 0) Then
                                For j = 1 To 6
                                    StressFp(j, imat, istep) = StressFp(j, imat, istep) / NEleFpIMat(imat)
                                Next j
                            End If
                        Next imat
                    End If


                Next IEleFp

            Next istep
            '******************** process stress END ******************        
            '**********************************************************                          



            '**********************************************************
            '******************** output result *********************** 
            For istep = 1 To NStep
                For imat = 1 To 12
                    If (imat = 1 Or imat = 12) Then 'only output stess in PCC Or overlay
                        If (NEleFpIMat(imat) <> 0) Then

                            For j = 1 To 6
                                LoadingStress(istep, j, ifp) = StressFp(j, imat, istep)
                            Next j

                            'If (imat.eq.12) Then
                            '    Write(21,*) 'Field point in overlay:'
                            'ElseIf (imat.eq.1) Then
                            '    Write(21,*) 'Field point in PCC slab:'
                            'ElseIf (imat.eq.2) Then
                            '    Write(21,*) 'Field point in base:'
                            'ElseIf (imat.eq.3) Then
                            '    Write(21,*) 'Field point in subbase 1:'
                            'ElseIf (imat.eq.4) Then
                            '    Write(21,*) 'Field point in subbase 2:'
                            'ElseIf (imat.eq.5) Then
                            '    Write(21,*) 'Field point in subbase 3:'
                            'Else
                            '    Write(21,*) 'Field point in subgrade:'
                            'End If

                            'Write(21,*) 'Step#    sig-xx     sig-yy     sig-zz     sig-xy
                            'c     sig - yz     sig-zx'

                        End If
                    End If
                Next imat
            Next istep
            '**********************************************************
            '******************** output result END********************


        Next ifp
    End Sub


    Function psi(ByRef i As Integer, ByRef ksi() As Double) As Double
        ' YC 040620-2

        If (i = 1) Then
            psi = (1 - ksi(1)) * (1 - ksi(2)) * (1 - ksi(3)) / 8
        ElseIf (i = 2) Then
            psi = (1 + ksi(1)) * (1 - ksi(2)) * (1 - ksi(3)) / 8
        ElseIf (i = 3) Then
            psi = (1 + ksi(1)) * (1 + ksi(2)) * (1 - ksi(3)) / 8
        ElseIf (i = 4) Then
            psi = (1 - ksi(1)) * (1 + ksi(2)) * (1 - ksi(3)) / 8
        ElseIf (i = 5) Then
            psi = (1 - ksi(1)) * (1 - ksi(2)) * (1 + ksi(3)) / 8
        ElseIf (i = 6) Then
            psi = (1 + ksi(1)) * (1 - ksi(2)) * (1 + ksi(3)) / 8
        ElseIf (i = 7) Then
            psi = (1 + ksi(1)) * (1 + ksi(2)) * (1 + ksi(3)) / 8
        ElseIf (i = 8) Then
            psi = (1 - ksi(1)) * (1 + ksi(2)) * (1 + ksi(3)) / 8
        End If

        Return psi

    End Function


    Function psiIP(ByRef i As Integer, ByRef ip As Integer) As Double
        ' YC 040620-2

        Dim ksi(3) As Double

        If (ip = 1) Then
            ksi = {0, -1 / 3 ^ 0.5, -1 / 3 ^ 0.5, -1 / 3 ^ 0.5}
        ElseIf (ip = 2) Then
            ksi = {0, 1 / 3 ^ 0.5, -1 / 3 ^ 0.5, -1 / 3 ^ 0.5}
        ElseIf (ip = 3) Then
            ksi = {0, 1 / 3 ^ 0.5, 1 / 3 ^ 0.5, -1 / 3 ^ 0.5}
        ElseIf (ip = 4) Then
            ksi = {0, -1 / 3 ^ 0.5, 1 / 3 ^ 0.5, -1 / 3 ^ 0.5}
        ElseIf (ip = 5) Then
            ksi = {0, -1 / 3 ^ 0.5, -1 / 3 ^ 0.5, 1 / 3 ^ 0.5}
        ElseIf (ip = 6) Then
            ksi = {0, 1 / 3 ^ 0.5, -1 / 3 ^ 0.5, 1 / 3 ^ 0.5}
        ElseIf (ip = 7) Then
            ksi = {0, 1 / 3 ^ 0.5, 1 / 3 ^ 0.5, 1 / 3 ^ 0.5}
        ElseIf (ip = 8) Then
            ksi = {0, -1 / 3 ^ 0.5, 1 / 3 ^ 0.5, 1 / 3 ^ 0.5}
        End If

        psiIp = psi(i, ksi)

        Return psiIP

    End Function


    Public Function Inverse(ByVal sourceMatrix(,) As Double) As Double(,)
        ' YC 040620-2

        ' ----- Build a new matrix that is the mathematical inverse
        '       of the supplied matrix. Multiplying a matrix and its
        '       inverse together will give the identity matrix.
        Dim eachCol As Integer
        Dim eachRow As Integer
        Dim rowsAndCols As Integer

        ' ----- Determine the size of each dimension of the matrix.
        '       Only square matrices can be inverted.
        If (UBound(sourceMatrix, 1) <> UBound(sourceMatrix, 2)) Then
            Throw New Exception("Matrix must be square.")
        End If
        Dim rank As Integer = UBound(sourceMatrix, 1)

        ' ----- Clone a copy of the matrix (not just a new reference).
        Dim workMatrix(,) As Double =
            CType(sourceMatrix.Clone, Double(,))

        ' ----- Variables used for backsolving.
        Dim destMatrix(rank, rank) As Double
        Dim rightHandSide(rank) As Double
        Dim solutions(rank) As Double
        Dim rowPivots(rank) As Integer
        Dim colPivots(rank) As Integer

        ' ----- Use LU decomposition to form a triangular matrix.
        workMatrix = FormLU(workMatrix, rowPivots, colPivots, rowsAndCols)

        ' ----- Backsolve the triangular matrix to get the inverted
        '       value for each position in the final matrix.

        'For eachCol = 0 To rank
        For eachCol = 1 To rank ' YC 040620-2

            rightHandSide(eachCol) = 1
            BackSolve(workMatrix, rightHandSide, solutions, rowPivots, colPivots)

            'For eachRow = 0 To rank    ' YC 040620-2
            For eachRow = 1 To rank

                destMatrix(eachRow, eachCol) = solutions(eachRow)
                rightHandSide(eachRow) = 0
            Next eachRow
        Next eachCol

        ' ----- Return the inverted matrix result.
        Return destMatrix
    End Function

    Private Function FormLU(ByVal sourceMatrix(,) As Double,
            ByRef rowPivots() As Integer, ByRef colPivots() As Integer,
            ByRef rowsAndCols As Integer) As Double(,)
        ' YC 040620-2

        ' ----- Perform an LU (lower and upper) decomposition of a matrix,
        '       a modified form of Gaussian elimination.
        Dim eachRow As Integer
        Dim eachCol As Integer
        Dim pivot As Integer
        Dim rowIndex As Integer
        Dim colIndex As Integer
        Dim bestRow As Integer
        Dim bestCol As Integer
        Dim rowToPivot As Integer
        Dim colToPivot As Integer
        Dim maxValue As Double
        Dim testValue As Double
        Dim oldMax As Double
        Const Deps As Double = 0.0000000000000001

        ' ----- Determine the size of the array.
        Dim rank As Integer = UBound(sourceMatrix, 1)
        Dim destMatrix(rank, rank) As Double
        Dim rowNorm(rank) As Double
        ReDim rowPivots(rank)
        ReDim colPivots(rank)

        ' ----- Make a copy of the array so we don't mess it up.
        Array.Copy(sourceMatrix, destMatrix, sourceMatrix.Length)

        ' ----- Initialize row and column pivot arrays.

        'For eachRow = 0 To rank ' YC 040620-2
        For eachRow = 1 To rank ' YC 040620-2
            rowPivots(eachRow) = eachRow
            colPivots(eachRow) = eachRow

            'For eachCol = 0 To rank ' YC 040620-2
            For eachCol = 1 To rank '

                rowNorm(eachRow) += Math.Abs(destMatrix(eachRow, eachCol))
            Next eachCol
            If (rowNorm(eachRow) = 0) Then
                Throw New Exception("Cannot invert a singular matrix.")
            End If
        Next eachRow

        ' ----- Use Gauss-Jordan elimination on the matrix rows.

        'For pivot = 0 To rank - 1  ' YC 040620-2
        For pivot = 1 To rank - 1

            maxValue = 0
            For eachRow = pivot To rank
                rowIndex = rowPivots(eachRow)
                For eachCol = pivot To rank
                    colIndex = colPivots(eachCol)
                    testValue = Math.Abs(destMatrix(rowIndex, colIndex)) _
                        / rowNorm(rowIndex)
                    If (testValue > maxValue) Then
                        maxValue = testValue
                        bestRow = eachRow
                        bestCol = eachCol
                    End If
                Next eachCol
            Next eachRow

            ' ----- Detect a singular, or very nearly singular, matrix.
            If (maxValue = 0) Then
                Throw New Exception("Singular matrix used for LU.")
            ElseIf (pivot > 1) Then     ' YC? 040620-2
                If (maxValue < (Deps * oldMax)) Then
                    Throw New Exception("Non-invertible matrix used for LU.")
                End If
            End If
            oldMax = maxValue

            ' ----- Swap row pivot values for the best row.
            If (rowPivots(pivot) <> rowPivots(bestRow)) Then
                rowsAndCols += 1
                Swap(rowPivots(pivot), rowPivots(bestRow))
            End If

            ' ----- Swap column pivot values for the best column.
            If (colPivots(pivot) <> colPivots(bestCol)) Then
                rowsAndCols += 1
                Swap(colPivots(pivot), colPivots(bestCol))
            End If

            ' ----- Work with the current pivot points.
            rowToPivot = rowPivots(pivot)
            colToPivot = colPivots(pivot)

            ' ----- Modify the remaining rows from the pivot points.
            For eachRow = (pivot + 1) To rank
                rowIndex = rowPivots(eachRow)
                destMatrix(rowIndex, colToPivot) =
                    -destMatrix(rowIndex, colToPivot) /
                    destMatrix(rowToPivot, colToPivot)
                For eachCol = (pivot + 1) To rank
                    colIndex = colPivots(eachCol)
                    destMatrix(rowIndex, colIndex) +=
                        destMatrix(rowIndex, colToPivot) *
                        destMatrix(rowToPivot, colIndex)
                Next eachCol
            Next eachRow
        Next pivot

        ' ----- Detect a non-invertible matrix.
        If (destMatrix(rowPivots(rank), colPivots(rank)) = 0) Then
            Throw New Exception("Non-invertible matrix used for LU.")
        ElseIf (Math.Abs(destMatrix(rowPivots(rank), colPivots(rank))) /
                rowNorm(rowPivots(rank))) < (Deps * oldMax) Then
            Throw New Exception("Non-invertible matrix used for LU.")
        End If

        ' ----- Success. Return the LU triangular matrix.
        Return destMatrix
    End Function

    Private Sub Swap(ByRef firstValue As Integer, ByRef secondValue As Integer)
        ' YC 040620-2

        ' ----- Reverse the values of two reference integers.
        Dim holdValue As Integer
        holdValue = firstValue
        firstValue = secondValue
        secondValue = holdValue
    End Sub

    Private Sub BackSolve(ByVal sourceMatrix(,) As Double,
            ByVal rightHandSide() As Double, ByVal solutions() As Double,
            ByRef rowPivots() As Integer, ByRef colPivots() As Integer)
        ' YC 040620-2

        ' ----- Solve an upper-right-triangle matrix.
        Dim pivot As Integer
        Dim rowToPivot As Integer
        Dim colToPivot As Integer
        Dim eachRow As Integer
        Dim eachCol As Integer
        Dim rank As Integer = UBound(sourceMatrix, 1)

        ' ----- Work through all pivot points. This section builds
        '       the "B" in the AX=B formula.

        'For pivot = 0 To (rank - 1)    ' YC 040620-2
        For pivot = 1 To (rank - 1)

            colToPivot = colPivots(pivot)
            For eachRow = (pivot + 1) To rank
                rowToPivot = rowPivots(eachRow)
                rightHandSide(rowToPivot) +=
                    sourceMatrix(rowToPivot, colToPivot) _
                    * rightHandSide(rowPivots(pivot))
            Next eachRow
        Next pivot

        ' ----- Now solve for each X using the general formula
        '       x(i) = (b(i) - summation(a(i,j)x(j)))/a(i,i)

        'For eachRow = rank To 0 Step -1    ' YC 040620-2
        For eachRow = rank To 1 Step -1

            colToPivot = colPivots(eachRow)
            rowToPivot = rowPivots(eachRow)
            solutions(colToPivot) = rightHandSide(rowToPivot)
            For eachCol = (eachRow + 1) To rank
                solutions(colToPivot) -=
                    sourceMatrix(rowToPivot, colPivots(eachCol)) _
                    * solutions(colPivots(eachCol))
            Next eachCol
            solutions(colToPivot) /= sourceMatrix(rowToPivot, colToPivot)
        Next eachRow
    End Sub


    Private Shared Function MatrixProduct(ByVal matrixA(,) As Double, ByVal matrixB(,) As Double) As Double(,)
        ' YC 040620-2

        Dim aRows As Integer = UBound(matrixA, 1)
        Dim aCols As Integer = UBound(matrixA, 2)
        Dim bRows As Integer = UBound(matrixB, 1)
        Dim bCols As Integer = UBound(matrixB, 2)
        If aCols <> bRows Then
            Throw New Exception("Non-conformable matrices in MatrixProduct")
        End If

        'Dim result(,) As Double = MatrixCreate(aRows, bCols)
        Dim result(aRows, bCols) As Double

        'For i As Integer = 0 To aRows  ' each row of A     ' YC 040620-2
        '    For j As Integer = 0 To bCols ' each col of B
        '        For k As Integer = 0 To aCols ' could use k < bRows
        For i As Integer = 1 To aRows  ' each row of A
            For j As Integer = 1 To bCols ' each col of B
                For k As Integer = 1 To aCols ' could use k < bRows
                    result(i, j) += matrixA(i, k) * matrixB(k, j)
                Next k
            Next j
        Next i

        'Parallel.For(0, aRows, Sub(i)
        '						   For j As Integer = 0 To bCols - 1 ' each col of B
        '							   For k As Integer = 0 To aCols - 1 ' could use k < bRows
        '								   result(i)(j) += matrixA(i)(k) * matrixB(k)(j)
        '							   Next k
        '						   Next j
        '					   End Sub);

        Return result
    End Function

    'Private Shared Function MatrixCreate(ByVal rows As Integer, ByVal cols As Integer) As Double(,)
    '    ' allocates/creates a matrix initialized to all 0.0. assume rows and cols > 0
    '    ' do error checking here
    '    Dim result(rows - 1)() As Double
    '    For i As Integer = 0 To rows - 1
    '        result(i) = New Double(cols - 1) {}
    '    Next i

    '    'For i As Integer = 0 To rows - 1
    '    '	For j As Integer = 0 To cols - 1
    '    '		result(i)(j) = 0.0 ' explicit initialization needed in some languages
    '    '	Next j
    '    'Next i

    '    Return result
    'End Function


End Class

Public Class DataTable
    Private Shared _model As New DataTableModel()

    Public Property Model As DataTableModel
        Get
            Return _model
        End Get
        Set(value As DataTableModel)

        End Set
    End Property

    Public Sub UpdateDisplay()

        
    End Sub

    Private Sub Button_Click(sender As Object, e As RoutedEventArgs) Handles ButtonGrossWeight.MouseDoubleClick, ButtonGrossWeightOnGears.MouseDoubleClick, ButtonNumGears.MouseDoubleClick, ButtonTireP.MouseDoubleClick
        Dim ValueChanged As Boolean
        If sender Is ButtonGrossWeight Then
            Call ChangeGrossWeight(ValueChanged)
        ElseIf sender Is ButtonGrossWeightOnGears Then
            Call ChangePcntOnMainGears(ValueChanged)
        ElseIf sender Is ButtonNumGears Then
            Call ChangeNMainGears(ValueChanged)
        ElseIf sender Is ButtonTireP Then
            Call ChangeTirePressure(ValueChanged)
        End If

        If ValueChanged Then
            Application.win.StartWheelIndex = 0
            Call Application.win.PlotGear()
        End If
        WriteParmGrid()
    End Sub
End Class

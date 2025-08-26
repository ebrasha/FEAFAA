Public Class PropertyWindow
    Dim e_row As Integer
    Dim e_column As Integer
    Public Overloads Sub Show(ByVal row As Integer, ByVal col As Integer)
        e_row = row
        e_column = col
        MyBase.Show()
    End Sub

    Private Sub cmbEnterNewValue_KeyDown(sender As Object, e As KeyEventArgs) Handles cmbEnterNewValue.KeyDown
        Dim KeyAscii As String = e.Key

        'limit input to numbers, back(2), delete(32) decimal (144/88)
        e.Handled = Not ("D1D2D3D4D5D6D7D8D9D0NumPad1NumPad2NumPad3NumPad4NumPad5NumPad6NumPad7NumPad8NumPad9NumPad0".Contains(e.Key.ToString())) AndAlso KeyAscii <> 2 AndAlso KeyAscii <> 32 AndAlso KeyAscii <> 144 AndAlso KeyAscii <> 88

        'no second dot
        If sender.Text.Contains(".") Then
            If KeyAscii = 144 Or KeyAscii = 88 Then
                e.Handled = True
            End If
        End If

    End Sub

    Private Sub cmdCancel_Click(sender As Object, e As RoutedEventArgs) Handles cmdCancel.Click
        Me.Close()
    End Sub

    Private Sub cmdOK_Click(sender As Object, e As RoutedEventArgs) Handles cmdOK.Click
        If e_column = 1 Then
            Application.win.grdLayerTable.Rows(e_row)(e_column) = Format(CSng(cmbEnterNewValue.Text), "###,###,#00")
        Else
            Application.win.grdLayerTable.Rows(e_row)(e_column) = cmbEnterNewValue.Text
        End If
        Me.Close()
    End Sub

    Private Sub cmbEnterNewValue_LostFocus(sender As Object, e As RoutedEventArgs) Handles cmbEnterNewValue.LostFocus
        If CSng(cmbEnterNewValue.Text) < Application.win.MinVal Then cmbEnterNewValue.Text = CStr(Application.win.MinVal)
        If CSng(cmbEnterNewValue.Text) > Application.win.MaxVal Then cmbEnterNewValue.Text = CStr(Application.win.MaxVal)
    End Sub
End Class

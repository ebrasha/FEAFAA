Class Application

    ' Application-level events, such as Startup, Exit, and DispatcherUnhandledException
    ' can be handled in this file.
    Public Shared win As MainWindow

    Private Sub Application_Startup(ByVal sender As System.Object, ByVal e As System.Windows.StartupEventArgs)
        win = New MainWindow()
        'win.Init()
        win.Show()
    End Sub
End Class

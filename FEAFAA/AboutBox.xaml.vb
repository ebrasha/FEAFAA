Public Class AboutBox
    Private ppalWin As MainWindow = Application.win
    Private Sub btn_OK_Click(sender As Object, e As RoutedEventArgs) Handles btn_OK.Click
        Me.Close()
    End Sub

    Public Sub Init()
        Left = ppalWin.Left + (ppalWin.Width - Width) \ 2
        Top = ppalWin.Top + (ppalWin.Height - Height) \ 2

        S = "The computer program FEAFAA is a 3D finite element pavement analysis tool."
        S = S & " FEAFAA calculates responses for rigid airport pavements only."
        S = S & " This program was prepared by the FAA for its R&D use only,"
        S = S & " and it is not sanctioned as a standard, or as part of a standard."
        S = S & " FEAFAA 4.0 makes use of programs FAASR3D, a Visual Basic library that performs"
        S = S & " 3D finite element computations for rigid pavement systems. FAASR3D replaces the"
        S = S & " FAA-modified version of Fortran finite element program NIKE3D that was used in"
        S = S & " previous FEAFAA versions."
        S = S & " This version is customized for civil engineering and construction machinery applications."


        lblCredits.Text = S

        S = "Questions and comments regarding the program should be directed to  "& vbCrLf
        S = S & "Reprogrammed Software for Civil Engineering by Ebrahim Shafiei (EbraSha), "& vbCrLf
        S = S & "Email: Prof.Shafiei@Gmail.com, " & vbCrLf
        S = S & "GitHub: https://github.com/EbraSha" & vbCrLf
        S = S & "Telegram: @ProfShafiei" & vbCrLf

        LblComments.Text = S
    End Sub
End Class

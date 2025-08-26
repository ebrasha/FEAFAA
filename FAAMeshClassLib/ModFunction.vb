Module ModFunction

    Public Function LPad(ByRef N As Integer, ByRef SS As String) As String
        ' Adds leading spaces to variant string SS to make it N characters long.
        ' Used to format output to a file. #### characters in a Format function
        ' do not force spaces like QuickBasic.
        ' Typically, SS = Format(XX, "0.00")
        Dim ITemp As Short
        ITemp = CShort(Len(SS))
        If ITemp > N Then N = ITemp ' Length = Len if Len > N
        LPad = Space(N - ITemp) & SS
    End Function

    Public Function RPad(ByRef N As Integer, ByRef SS As String) As String
        ' Adds leading spaces to variant string SS to make it N characters long.
        ' Used to format output to a file. #### characters in a Format function
        ' do not force spaces like QuickBasic.
        ' Typically, SS = Format(XX, "0.00")
        Dim ITemp As Short
        ITemp = CShort(Len(SS))
        If ITemp > N Then N = ITemp ' Length = Len if Len > N
        RPad = SS & Space(N - ITemp)
    End Function

    Sub MakeDecimalPeriod(ByRef SN As String)

        Dim IP As Integer

        IP = InStr(SN, ",")
        If IP <> 0 Then
            Mid(SN, IP) = "."
        End If

    End Sub

End Module

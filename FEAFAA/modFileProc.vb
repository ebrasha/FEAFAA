Option Strict Off
Option Explicit On
Module modFileProcedures
	
    Public Const NUMBOXES As Short = 5
	Public Const SAVEFILE As Short = 1
	Public Const LOADFILE As Short = 2
	Public Const REPLACEFILE As Short = 1
	Public Const READFILE As Short = 2
	Public Const ADDTOFILE As Short = 3
	Public Const RANDOMFILE As Short = 4
	Public Const BINARYFILE As Short = 5
	
	' Define a data type to hold a record:
	' Define global variables to hold the file number and record number
	' of the current data file.
	' Default file name to show in dialog boxes.
	Public Const Err_DeviceUnavailable As Short = 68
	Public Const Err_DiskNotReady As Short = 71
	Public Const Err_FileAlreadyExists As Short = 58
	Public Const Err_TooManyFiles As Short = 67
	Public Const Err_RenameAcrossDisks As Short = 74
	Public Const Err_Path_FileAccessError As Short = 75
	Public Const Err_DeviceIO As Short = 57
	Public Const Err_DiskFull As Short = 61
	Public Const Err_BadFileName As Short = 64
	Public Const Err_BadFileNameOrNumber As Short = 52
	Public Const Err_FileNotFound As Short = 53
	Public Const Err_PathDoesNotExist As Short = 76
	Public Const Err_BadFileMode As Short = 54
	Public Const Err_FileAlreadyOpen As Short = 55
	Public Const Err_InputPastEndOfFile As Short = 62
	Public Const MB_EXCLAIM As Short = 48
	Public Const MB_STOP As Short = 16
	
    Public Const ExternalAircraftFileName As String = "Library\AircraftLibrary"
	Public Const ExternalFileFormat As String = "Format1"
	
	Public IACAddedorRemoved As Short
	
    Sub InsertNewAircraft(ByRef InsertName As String, ByRef DupName As Boolean)
        ' Inserts a new section in the current job in memory.
        ' The insertion point is in ASCII order on InsertName$.
        ' Following this routine with WriteJobFile ensures that
        ' the sections in the job file will always be sorted and list
        ' box order will show ASCII order without setting .Sorted = True.

        Dim II, I, J As Short
        Dim IAC, IStart As Short
        Dim S As String

        ' Find insertion point
        DupName = False
        libNAC = libNAC + 1

        libACName(libNAC) = "ZZZZZZZZZZZZZZZZZZ" ' End marker.
        IStart = LibACGroup(ExternalLibraryIndex)
        For I = IStart To libNAC
            If StrComp(InsertName, libACName(I), 1) <= 0 Then
                If StrComp(InsertName, libACName(I), 1) = 0 Then
                    S = "Name " & InsertName & " already exists." & NL2
                    S = S & "No action will be taken."
                    Ret = MsgBox(S, 0, "Inserting Current Aircraft")
                    libNAC = libNAC - 1
                    DupName = True
                    Exit Sub
                End If
                IAC = I
                Exit For
            End If
        Next I

        ' Move all data from insertion point up one place.
        For I = libNAC - 1 To IAC Step -1

            II = I + 1
            libACName(II) = libACName(I)
            libGL(II) = libGL(I)
            libNMainGears(II) = libNMainGears(I)
            libPcntOnMainGears(II) = libPcntOnMainGears(I)
            libNTires(II) = libNTires(I)

            For J = 1 To libNTires(I)
                libTY(II, J) = libTY(I, J)
                libTX(II, J) = libTX(I, J)
            Next J

            libCP(II) = libCP(I)
            libCoverages(II) = libCoverages(I)
            libXGridOrigin(II) = libXGridOrigin(I)
            libXGridMax(II) = libXGridMax(I)
            libXGridNPoints(II) = libXGridNPoints(I)
            libYGridOrigin(II) = libYGridOrigin(I)
            libYGridMax(II) = libYGridMax(I)
            libYGridNPoints(II) = libYGridNPoints(I)

        Next I

        ' Put current section data at insertion point.

        JobTitle = InsertName
        Call UpdateLibraryData(IAC)
        IACAddedorRemoved = IAC

    End Sub
	
    Function FileErrors(ByRef errVal As Short, ByRef FileName As String) As Short
        ' Return Value  Meaning     Return Value    Meaning
        ' 0             Resume      2               Unrecoverable error
        ' 1             Resume Next 3               Unrecognized error
        Dim MsgType As Short
        Dim Response As Short
        Dim Msg As String
        MsgType = MB_EXCLAIM
        Select Case errVal
            Case Err_DeviceUnavailable ' Error #68
                Msg = "The disk appears to be unavailable."
                MsgType = MB_EXCLAIM + 5
            Case Err_DiskNotReady ' Error #71
                Msg = "The disk is not ready."
            Case Err_DeviceIO
                Msg = "The disk is full."
            Case Err_BadFileName, Err_BadFileNameOrNumber ' Errors #64 & 52
                Msg = FileName & " path or file name is illegal."
            Case Err_PathDoesNotExist ' Error #76
                Msg = "The path for " & FileName & " doesn't exist."
            Case Err_BadFileMode ' Error #54
                Msg = "Can't open " & FileName & " for that type of access."
            Case Err_FileAlreadyOpen ' Error #55
                Msg = FileName & " is already open."
            Case Err_InputPastEndOfFile ' Error #62
                Msg = FileName & " has a nonstandard end-of-file marker,"
                Msg = Msg & "or an attempt was made to read beyond "
                Msg = Msg & "the end-of-file marker."
            Case Err_FileNotFound
                Msg = "Cannot find " & FileName & "."
            Case Else
                FileErrors = 3
                Msg = "Unknown file error"
                Exit Function
        End Select
        Response = MsgBox(Msg, MsgType, "File Error")
        Select Case Response
            Case 4 ' Retry button.
                FileErrors = 0
            Case 5 ' Ignore button.
                FileErrors = 1
            Case 1, 2, 3 ' Ok and Cancel buttons.
                FileErrors = 2
            Case Else
                FileErrors = 3
        End Select
    End Function
	
	Sub MakeDecimalPeriod(ByRef SN As String)
		
		Dim IP As Integer
		
		Do 
			IP = InStr(SN, ",")
			If IP <> 0 Then
				Mid(SN, IP) = "."
			Else
				Exit Do
			End If
		Loop 
		
	End Sub

    Sub ReadExternalFile(ByRef IA As Short)

        Dim J, I, IErr As Short
        Dim S, SS As String
        Dim FileName, FileFormat As String
        Dim EFNo As Short
        On Error GoTo RExternalFileError

        ExtFilePath = System.AppDomain.CurrentDomain.BaseDirectory() 'ik2020.04.21
        FileName = ExtFilePath & ExternalAircraftFileName & ".Ext"
        'MsgBox(FileName, MsgBoxStyle.AbortRetryIgnore, "Place1")
        EFNo = FreeFile()
        FileOpen(EFNo, FileName, OpenMode.Input, , , 1024)

        If EOF(EFNo) Then
            S = "File " & FileName & " is empty or does not exist." & NL2
            S = S & "No action will be taken now."
            Exit Sub
        End If

        FileFormat = LineInput(EFNo)

        Do
            If IA + 1 > MaxLibAC Then
                SS = Format(MaxLibAC, "0")
                S = "The maximum number of aircraft allowed is " & SS & "." & vbCrLf
                S = S & "The external library file exceeds this value." & vbCrLf
                S = S & "No more aircraft will be loaded."
                Ret = MsgBox(S, 0, "Too Many Sections")
                Exit Do
            End If
            If EOF(EFNo) Then Exit Do

            IA = IA + 1
            Units = LineInput(EFNo)
            libACName(IA) = LineInput(EFNo)
            FileSystem.Input(EFNo, libGL(IA))
            FileSystem.Input(EFNo, libNMainGears(IA))
            FileSystem.Input(EFNo, libPcntOnMainGears(IA))
            FileSystem.Input(EFNo, libNTires(IA))
            For I = 1 To libNTires(IA)
                FileSystem.Input(EFNo, libTY(IA, I))
                FileSystem.Input(EFNo, libTX(IA, I))
            Next I
            FileSystem.Input(EFNo, libCP(IA))
            FileSystem.Input(EFNo, libXGridOrigin(IA))
            FileSystem.Input(EFNo, libXGridMax(IA))
            FileSystem.Input(EFNo, libXGridNPoints(IA))
            FileSystem.Input(EFNo, libYGridOrigin(IA))
            FileSystem.Input(EFNo, libYGridMax(IA))
            FileSystem.Input(EFNo, libYGridNPoints(IA))
            libAlpha(IA) = 0
            libCoverages(IA) = StandardCoverages

        Loop
        FileClose(EFNo)
        Exit Sub

RExternalFileError:
        IErr = Err.Number
        If IErr = 62 Then
            IA = IA - 1
            Exit Sub
        End If
        Ret = FileErrors(IErr, FileName)
        If Ret = 0 Then Resume
        If Ret = 3 Then
            S = "Error # " & Format(IErr, "0") & " occurred reading" & vbCrLf
            S = S & "file " & FileName & "." & NL2
            S = S & "It may have been caused by a corrupted file or by" & vbCrLf
            S = S & "a file with incorrect format. Please check the file." & NL2
            S = S & "If the error occurred during startup, try loading" & vbCrLf
            S = S & "another job or create a new job. Repair or delete" & vbCrLf
            S = S & "the bad job file as soon as possible."
            Ret = MsgBox(S, 0, "File Error")
        End If
        FileClose(EFNo)
        Exit Sub

    End Sub

    Sub WriteExternalFile()

        Dim J, IA, IErr As Short
        Dim S As String
        Dim FileName As String
        Dim EFNo, I As Short
        On Error GoTo WExternalFileError

        FileName = ExtFilePath & ExternalAircraftFileName & ".Ext"
        EFNo = FreeFile()
        FileOpen(EFNo, FileName, OpenMode.Output, , , 1024)

        PrintLine(EFNo, ExternalFileFormat)

        For IA = LibACGroup(ExternalLibraryIndex) To libNAC

            PrintLine(EFNo, Units)
            PrintLine(EFNo, libACName(IA))
            S = LPad(12, Format(libGL(IA), "0"))
            Call MakeDecimalPeriod(S)
            PrintLine(EFNo, S)
            S = LPad(12, Format(libNMainGears(IA), "0"))
            Call MakeDecimalPeriod(S)
            PrintLine(EFNo, S)
            S = LPad(12, Format(libPcntOnMainGears(IA), "0.000"))
            Call MakeDecimalPeriod(S)
            PrintLine(EFNo, S)
            S = LPad(12, Format(libNTires(IA), "0"))
            Call MakeDecimalPeriod(S)
            PrintLine(EFNo, S)
            For I = 1 To libNTires(IA)
                S = LPad(12, Format(libTY(IA, I), "0.000"))
                S = S & LPad(12, Format(libTX(IA, I), "0.000"))
                Call MakeDecimalPeriod(S)
                PrintLine(EFNo, S)
            Next I
            S = LPad(12, Format(libCP(IA), "0.000"))
            Call MakeDecimalPeriod(S)
            PrintLine(EFNo, S)
            S = LPad(12, Format(libXGridOrigin(IA), "0.000"))
            S = S & LPad(12, Format(libXGridMax(IA), "0.000"))
            S = S & LPad(12, Format(libXGridNPoints(IA), "0"))
            Call MakeDecimalPeriod(S)
            PrintLine(EFNo, S)
            S = LPad(12, Format(libYGridOrigin(IA), "0.000"))
            S = S & LPad(12, Format(libYGridMax(IA), "0.000"))
            S = S & LPad(12, Format(libYGridNPoints(IA), "0"))
            Call MakeDecimalPeriod(S)
            PrintLine(EFNo, S)

        Next IA
        FileClose(EFNo)
        Exit Sub

WExternalFileError:

        IErr = Err.Number
        Ret = FileErrors(IErr, FileName)
        If Ret = 0 Then Resume
        If Ret = 3 Then
            S = "Error # " & Format(IErr, "0") & " occurred writing" & vbCrLf
            S = S & "file " & FileName & "."
            Ret = MsgBox(S, 0, "File Error")
        End If
        FileClose(EFNo)
        Exit Sub

    End Sub

    Public Sub UpdateLibraryData(ByRef IAC As Short)

        Dim Nx, J, Ny As Short

        libACName(IAC) = JobTitle
        libGL(IAC) = GrossWeight
        libNMainGears(IAC) = NMainGears
        libPcntOnMainGears(IAC) = PcntOnMainGears
        libNTires(IAC) = NWheels

        For J = 1 To libNTires(IAC)
            libTY(IAC, J) = XWheels(J)
            libTX(IAC, J) = YWheels(J)
        Next J

        If libXGridNPoints(IAC) <> 0 And libXGridNPoints(IAC) <> 0 Then
            Nx = XGridNPoints : Ny = YGridNPoints
        Else
            Nx = 0 : Ny = 0
        End If

        libCP(IAC) = TirePressure
        libCoverages(IAC) = Coverages
        libXGridOrigin(IAC) = XGridOrigin
        libXGridMax(IAC) = XGridMax
        libXGridNPoints(IAC) = Nx
        libYGridOrigin(IAC) = YGridOrigin
        libYGridMax(IAC) = YGridMax
        libYGridNPoints(IAC) = Ny

    End Sub

    Public Sub UpdateDataFromLibrary(ByRef IAC As Short)

        Dim Nx, J, Ny As Short

        JobTitle = libACName(IAC)
        GrossWeight = libGL(IAC)
        NMainGears = libNMainGears(IAC)
        PcntOnMainGears = libPcntOnMainGears(IAC)
        NWheels = libNTires(IAC)

        For J = 1 To libNTires(IAC)
            XWheels(J) = libTY(IAC, J)
            YWheels(J) = libTX(IAC, J)
        Next J

        If libXGridNPoints(IAC) <> 0 And libXGridNPoints(IAC) <> 0 Then
            Nx = XGridNPoints : Ny = YGridNPoints
        Else
            Nx = 0 : Ny = 0
        End If

        TirePressure = libCP(IAC)
        Coverages = libCoverages(IAC)
        XGridOrigin = libXGridOrigin(IAC)
        XGridMax = libXGridMax(IAC)
        XGridNPoints = libXGridNPoints(IAC)
        YGridOrigin = libYGridOrigin(IAC)
        YGridMax = libYGridMax(IAC)
        YGridNPoints = libYGridNPoints(IAC)

    End Sub

End Module


'http://www.simple-talk.com/dotnet/visual-studio/updates-to-setup-projects/


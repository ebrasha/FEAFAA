Option Strict Off
Option Explicit On
Module modGearEdit
	
	Public IWheelSelected As Short
    Public Const CoordResolution As Short = 1
	
	Public Operation, LastOperation As Short
	Public LastXP, LastYP As Double
	Public LastIWheel As Short
	
	Public Const NoOperation As Short = 0
	Public Const MoveWheel As Short = 1
	Public Const AddWheel As Short = 2
	Public Const RemoveWheel As Short = 3
	Public Const SelectAWheel As Short = 4
	Public Const ChangeXCoordinate As Short = 5
	Public Const ChangeYCoordinate As Short = 6
	
	Public Const kPaTopsi As Double = 0.1450377438
	Public Const cmToin As Double = 0.3937008
	Public Const kgTolb As Double = 2.2046225
	
	Public ChangeDataRet As Short
	
	Public Const MaxSectAC As Short = 20
	Public Const MaxLibGroups As Short = 10
	Public Const MaxLibAC As Short = 200 ' Total number of aircraft in library.
	Public Const MaxNEval As Short = 8 ' Maximum number of evaluation points
	Public Const MaxNTires As Short = 24 ' Maximum number of tires on eval. gear.
	Public Const MaxNTTrack As Short = 10 ' Maximum number of gear tracks (for CDF).
	

	Public lstLibFileIndex As Short ' See frmParameters.lstAircraft
	Public lstACGroupIndex As Short ' See frmParameters.lstAircraft
	Public lstAircraftIndex As Short ' See frmParameters.lstAircraft
	
	Public ILibACGroup As Short
	Public NLibACGroups As Short
	Public LibACGroup(MaxLibGroups) As Short
	Public LibACGroupName(MaxLibGroups) As String
	
	Public libNAC As Short ' Number of aircraft in library list.
	Public NBelly As Short
	Public Const BellyExt As String = " Belly"
	Public libACName(MaxLibAC) As String
	Public libGL(MaxLibAC) As double
	Public libNMainGears(MaxLibAC) As double
	Public libPcntOnMainGears(MaxLibAC) As double
	Public libMGpcnt(MaxLibAC) As double
	Public libNTires(MaxLibAC) As Short
	Public libTX(MaxLibAC, MaxNTires) As double
	Public libTY(MaxLibAC, MaxNTires) As double
	Public libCP(MaxLibAC) As double
	Public libNEVPTS(MaxLibAC) As Short
	Public libEVPTX(MaxLibAC, MaxNEval) As double
	Public libEVPTY(MaxLibAC, MaxNEval) As double
	Public libGear(MaxLibAC) As String
	Public libNTTrack(MaxLibAC) As Short
	Public libIGear(MaxLibAC) As Short
	Public libTT(MaxLibAC) As double
	Public libTS(MaxLibAC) As double
	Public libTG(MaxLibAC) As double
	Public libB(MaxLibAC) As double
	Public libXAC(MaxLibAC, MaxNTTrack) As double
	Public libAlpha(MaxLibAC) As Double
	Public libCoverages(MaxLibAC) As Double
	Public libXGridOrigin(MaxLibAC) As Double
	Public libYGridOrigin(MaxLibAC) As Double
	Public libXGridMax(MaxLibAC) As Double
	Public libYGridMax(MaxLibAC) As Double
	Public libXGridNPoints(MaxLibAC) As Double
	Public libYGridNPoints(MaxLibAC) As Double
	
	Public LibIndex As Short ' Library index for load index (link).
	Public LI As Short ' Temporary alias for LibIndex(I)
	
	Public NAC As Short ' Number of aircraft in current section.
	Public ACName(MaxSectAC) As String
	Public GL(MaxSectAC) As double ' Gross aircraft load for design.
	Public WT(MaxSectAC) As double
	Public TW(MaxSectAC) As double
	
	Public Const MinGLFraction As double = 0.1
	Public Const MaxGLFraction As double = 10
	
	Public Sub SelectWheel(ByRef XW() As Double, ByRef YW() As Double, ByRef X As Double, ByRef Y As Double, ByRef MinNorm As Double, ByRef ISelected As Short)
		
		Dim I As Short
		Dim Norm As Double
		
		MinNorm = 1E+20
		For I = 1 To NWheels
			Norm = System.Math.Sqrt((XW(I) - X) ^ 2 + (YW(I) - Y) ^ 2)
			If Norm < MinNorm Then
				MinNorm = Norm
				ISelected = I
			End If
		Next I
		
	End Sub
	
    Public Sub WriteParmGrid()

        Application.win.GridParams.Model.GrossWeight = Format(GrossWeight, "#,###,##0")
        Application.win.GridParams.Model.GrossWeightOnGears = Format(PcntOnMainGears, "0.00")
        Application.win.GridParams.Model.NumGears = Format(NMainGears, "0")
        Application.win.GridParams.Model.WheelsOnGear = Format(NWheels, "0")
        Application.win.GridParams.Model.TirePressure = Format(TirePressure, "0.0")

        'System.Windows.Forms.Application.DoEvents()		
	End Sub

    Public Sub ChangeGrossWeight(ByRef ValueChanged As Boolean)

        Dim S, SS As String
        Dim SVS, LibValue As Double
        Dim CurrentValue, NewValue As Double
        Dim MinValue, MaxValue As Double

        LibIndex = LibACGroup(lstACGroupIndex + 1) + Application.win.ListBoxLibraryAirplane.SelectedIndex ' frmGear.lstLibFile.SelectedIndex ' Library index

        CurrentValue = GrossWeight
        LibValue = libGL(LibIndex) ' From library file.
        MinValue = MinGLFraction * LibValue
        MaxValue = MaxGLFraction * LibValue

        S = "The default value of gross load for" & vbCrLf
        S = S & "this aircraft is " & Format(LibValue, "#,###,##0") & " lbs." & NL2
        S = S & "Enter a new value in the range:"
        S = S & NL2 & Format(MinValue, "#,###,##0")
        S = S & " to " & Format(MaxValue, "#,###,##0") & " lbs."
        S = S & NL2 & "Click Cancel at any time to retain the old value."
        SS = "Changing Aircraft Gross Load"

        ValueChanged = GetInputSingle(S, SS, SVS)

        If ValueChanged Then

            NewValue = SVS

            '   Check to See if value is within range.
            If NewValue < MinValue Or MaxValue < NewValue Then
                NewValue = CurrentValue
                S = "Gross load cannot be less than "
                S = S & Format(MinGLFraction, "0.00")
                S = S & " x " & Format(LibValue, "#,###,###") & "." & vbCrLf
                S = S & "or greater than "
                S = S & Format(MaxGLFraction, "0.00")
                S = S & " x " & Format(LibValue, "#,###,###") & "." & NL2
                S = S & "The default value of gross load has been selected."
                Ret = MsgBox(S, 0, "")
                ValueChanged = False
            End If

            GrossWeight = NewValue

        End If

    End Sub

    Public Sub ChangePcntOnMainGears(ByRef ValueChanged As Boolean)

        Dim S, SS As String
        Dim SVS, LibValue As Double
        Dim CurrentValue, NewValue As Double
        Dim MinValue, MaxValue As Double

        LibIndex = LibACGroup(lstACGroupIndex + 1) + Application.win.ListBoxLibraryAirplane.SelectedIndex ' frmGear.lstLibFile.SelectedIndex ' Library index

        CurrentValue = PcntOnMainGears
        LibValue = libPcntOnMainGears(LibIndex) ' From library file.
        MinValue = 5
        MaxValue = 100

        S = "The current value of percent gross weight on" & vbCrLf
        S = S & "all of the main gears for this aircraft is "
        S = S & Format(CurrentValue, "#,###,##0.00") & "." & vbCrLf & vbCrLf
        S = S & "Enter a new value in the range:"
        S = S & NL2 & Format(MinValue, "#,###,##0.00")
        S = S & " to " & Format(MaxValue, "#,###,##0.00") & "."
        S = S & NL2 & "Click Cancel at any time to retain the old value."
        SS = "Changing Percent on Main Gears"

        ValueChanged = GetInputSingle(S, SS, SVS)

        If ValueChanged Then

            NewValue = SVS

            '   Check to See if value is within range.
            If NewValue < MinValue Or MaxValue < NewValue Then
                NewValue = CurrentValue
                S = "Percent gross weight cannot be less than "
                S = S & Format(MinValue, "0.00") & vbCrLf
                S = S & "or greater than "
                S = S & Format(MaxValue, "0.00") & "." & NL2
                S = S & "The old value has been retained."
                'UPGRADE_WARNING: Couldn't resolve default property of object Ret. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
                Ret = MsgBox(S, 0, "")
                ValueChanged = False
            End If

            PcntOnMainGears = NewValue

        End If

    End Sub

    Public Sub ChangeTirePressure(ByRef ValueChanged As Boolean)

        Dim S, SS As String
        Dim SVS, LibValue As Double
        Dim CurrentValue, NewValue As Double
        Dim MinValue, MaxValue As Double

        LibIndex = LibACGroup(lstACGroupIndex + 1) + Application.win.ListBoxLibraryAirplane.SelectedIndex 'frmGear.lstLibFile.SelectedIndex ' Library index

        CurrentValue = TirePressure
        LibValue = libCP(LibIndex) ' From library file.
        MinValue = 50
        MaxValue = 400

        S = "The current value of tire pressure for" & vbCrLf
        S = S & "this aircraft is "
        S = S & Format(CurrentValue, "#,###,##0.00") & " psi." & vbCrLf & vbCrLf
        S = S & "Enter a new value in the range:"
        S = S & NL2 & Format(MinValue, "#,###,##0")
        S = S & " to " & Format(MaxValue, "#,###,##0") & "."
        S = S & NL2 & "Click Cancel at any time to retain the old value."
        SS = "Changing Tire Pressure"

        ValueChanged = GetInputSingle(S, SS, SVS)
        If ValueChanged Then

            NewValue = SVS

            '   Check to See if value is within range.
            If NewValue < MinValue Or MaxValue < NewValue Then
                NewValue = CurrentValue
                S = "Tire pressure cannot be less than "
                S = S & Format(MinValue, "0.00") & vbCrLf
                S = S & "or greater than "
                S = S & Format(MaxValue, "0.00") & "." & NL2
                S = S & "The old value has been retained."
                'UPGRADE_WARNING: Couldn't resolve default property of object Ret. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
                Ret = MsgBox(S, 0, "")
                ValueChanged = False
            End If

            TirePressure = NewValue

        End If

    End Sub

     Public Sub ChangeNMainGears(ByRef ValueChanged As Boolean)

        Dim S, SS As String
        Dim SVS, LibValue As Double
        Dim CurrentValue, NewValue As Double
        Dim MinValue, MaxValue As Double

        '  If lstACGroupIndex + 1 <> ExternalLibraryIndex Then
        '    S$ = "The current aircraft is in the internal library." & NL2
        '    S$ = S$ & "The number of main gears cannot be" & vbCrLf
        '    S$ = S$ & "changed for aircraft in the internal library."
        '    Ret = MsgBox(S$, 0, "Changing Number of Main Gears")
        '    Exit Sub
        '  End If

        LibIndex = LibACGroup(lstACGroupIndex + 1) + Application.win.ListBoxLibraryAirplane.SelectedIndex ' frmGear.lstLibFile.SelectedIndex ' Library index

        CurrentValue = NMainGears
        LibValue = libNMainGears(LibIndex) ' From library file.
        MinValue = 1
        MaxValue = 16

        S = "The current number of main" & vbCrLf
        S = S & "gears for this aircraft is "
        S = S & Format(CurrentValue, "#,###,##0") & "." & vbCrLf & vbCrLf
        S = S & "Enter a new value in the range:"
        S = S & NL2 & Format(MinValue, "#,###,##0")
        S = S & " to " & Format(MaxValue, "#,###,##0") & "."
        S = S & NL2 & "Click Cancel at any time to retain the old value."
        SS = "Changing Number of Main Gears"

        ValueChanged = GetInputSingle(S, SS, SVS)
        If ValueChanged Then

            NewValue = SVS

            '   Check to See if value is within range.
            If NewValue < MinValue Or MaxValue < NewValue Then
                NewValue = CurrentValue
                S = "The number of main gears cannot be less than "
                S = S & Format(MinValue, "0.00") & vbCrLf
                S = S & "or greater than "
                S = S & Format(MaxValue, "0.00") & "." & NL2
                S = S & "The old value has been retained."
                Ret = MsgBox(S, 0, "")
                ValueChanged = False
            End If

            NMainGears = NewValue

        End If

    End Sub
	

    Public Sub ResetOutputs()
		
        Application.win.StartWheelIndex = 0 ' Don't plot cross for rigid origin.
        Application.win.BtnSelectWheel.Content = "Se_lect"
        Application.win.BtnSelectWheel.IsChecked = False
        Application.win.LabelXSelCoord.Content = ""
        Application.win.LabelYSelCoord.Content = ""

        'CType(frmGear.Controls("cmdSelectWheel"), Object).Text = "Se&lect"
        'CType(frmGear.Controls("lblXSelected"), Object).Text = ""
        '      CType(frmGear.Controls("lblYSelected"), Object).Text = ""

        Operation = NoOperation
        LastOperation = NoOperation
		
	End Sub
	
	
	

End Module
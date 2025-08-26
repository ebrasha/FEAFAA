Option Strict Off
Option Explicit On
Module modWorld

    Declare Function WinHelp Lib "user32" Alias "WinHelpA" (ByVal hwnd As Integer, ByVal lpHelpFile As String, ByVal wCommand As Integer, ByVal dwData As Integer) As Integer

    Public gJobFileFolder As String
    Public exitLoop As Boolean = False
	Public ExtFilePath, FileName, FilePath, Units, FileFormat As String
    Public FirstFileRead As Boolean, ExternalLibraryIndex As Short
    Public Ret As Microsoft.VisualBasic.MsgBoxResult
    Public S As String
	Public Const NL2 As String = vbCrLf & vbCrLf

    Public Const StandardCoverages As Double = 10000
    Public Coverages As Double

    Public JobTitle As String, NWheels As Short
	Public Const NSubs As Short = 4
	Public Const NMaxWheels As Short = 56
	Public XWheels(NMaxWheels) As Double
	Public YWheels(NMaxWheels) As Double
	Public XGridMax, XGridOrigin, XGridNPoints As Double
	Public YGridMax, YGridOrigin, YGridNPoints As Double
	Public GrossWeight, TirePressure As Double
	Public PcntOnMainGears, NMainGears As Double

    Public WheelRadius, InputAlpha As Double

    Public GrossWeightRow, PcntOnMainGearsRow, NMainGearsRow As Short
    Public NWheelsRow, TirePressureRow As Short

    'Tab 2 Pavement Structure
    Public gXcoord1, gYcoord1 As double
    Public gXcoord2, gYcoord2 As double, NGearPositions As Integer, LoadingType, LoadCase As String

    'Tab 3 Joint Modeling
    Public EqJStfX, EqJStfY, EqEdgStfX, EqEdgStfY As double


    Public Const XINCH As Double = 2.54 ' inches to cm
    Public Const XPRES As Double = 145.0377438 ' Mpa to psi
    Public Const XPOUND As Double = 2.2046225 ' kg to lb
	
    Public NXSymmetric, NYSymmetric As Short

	'UPGRADE_ISSUE: Declaring a parameter 'As Any' is not supported. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="FAE78A8D-8978-4FD4-8208-5B7324A8F795"'
    'ikawa 999 Declare Function OSWinHelp Lib "USER32"  Alias "WinHelpA"(ByVal hwnd As Integer, ByVal HelpFile As String, ByVal wCommand As Short, ByRef dwData As Any) As Short
	


    Function GetInputSingle(ByRef Prompt As String, ByRef Title As String, ByRef SVS As Double) As Boolean
        ' Get variant input from an InputBox and check for valid number.
        ' Returns SVS as a double (precision) number.
        ' Returns True if valid number. Allows commas and periods.
        Dim SV As Object, S As String
        SV = InputBox(Prompt, Title)
        If SV = "" Then
            GetInputSingle = False
        ElseIf Not IsNumeric(SV) Then
            S = SV & " was entered." & vbCrLf & "It is not a valid number."
            S = S & vbCrLf & vbCrLf & "Please retry."
            Ret = MsgBox(S, 0, "Incorrect Data Entry")
            GetInputSingle = False
        Else
            SVS = SV ' Get rid of string elements (commas).
            GetInputSingle = True
        End If
    End Function
	

    Public Function StripDirPath(ByRef S As String) As String

        Dim IP, I, L As Integer

        IP = 0
        L = Len(S)
        For I = 1 To L
            If Mid(S, I, 1) = "\" Then IP = I
        Next I

        StripDirPath = Right(S, L - IP)

    End Function

	Public Function LPad(ByRef N As Object, ByRef SS As Object) As String

        ' Adds leading spaces to variant string SS to make it N characters long.
		' Used to format output to a file. #### characters in a Format function
		' do not force spaces like QuickBasic.
		' Typically, SS = Format(XX, "0.00")
		Dim ITemp As Short
		ITemp = Len(SS)
        If N - ITemp < 1 Then N = ITemp + 1
        LPad = Space(N - ITemp) & SS

	End Function

	
	Public Function ALOG10(ByRef X As Double) As Double
		Const Log10BaseE As Double = 2.30258509299405
		ALOG10 = System.Math.Log(X) / Log10BaseE
	End Function
	

	Public Sub GearCG(ByRef XW() As Double, ByRef YW() As Double, ByRef N As Short, ByRef Xcg As Double, ByRef Ycg As Double)
		
		Dim I, J As Short
        Dim XRcg() As Double, YRcg() As Double
        Dim IXRcg() As Double, IYRcg() As Double
        Dim GridInterval As Double
		
        ReDim XRcg(N) : ReDim YRcg(N) : ReDim IXRcg(N) : ReDim IYRcg(N)
		
		WheelRadius = GrossWeight * PcntOnMainGears / (100 * NMainGears * NWheels)
        WheelRadius = System.Math.Sqrt(WheelRadius / TirePressure / Math.PI)
		
		Xcg = 0 : Ycg = 0
		For I = 1 To N
			Xcg = Xcg + XW(I)
			Ycg = Ycg + YW(I)
		Next I
		Xcg = Xcg / N
		Ycg = Ycg / N
		
		If libXGridNPoints(LibIndex) <> 0 And libYGridNPoints(LibIndex) <> 0 Then
			Exit Sub
		End If
		
		For I = 1 To N
			XRcg(I) = XW(I) - Xcg
			YRcg(I) = YW(I) - Ycg
			IXRcg(I) = Fix(XRcg(I)) ' Test symmetry to the nearest inch.
			IYRcg(I) = Fix(YRcg(I))
		Next I
		
		NXSymmetric = 0 : NYSymmetric = 0
		For I = 1 To N
			For J = 1 To N
				' Test symmetry about the X axis.
				If IXRcg(I) = IXRcg(J) And IYRcg(I) = -IYRcg(J) And I <> J Then
					NXSymmetric = NXSymmetric + 1
				End If
				' Test symmetry about the Y axis.
				If IYRcg(I) = IYRcg(J) And IXRcg(I) = -IXRcg(J) And I <> J Then
					NYSymmetric = NYSymmetric + 1
				End If
			Next J
			' Wheels on an axis are symmetric but not found above.
			If IYRcg(I) = 0 Then NXSymmetric = NXSymmetric + 1
			If IXRcg(I) = 0 Then NYSymmetric = NYSymmetric + 1
		Next I
		
		XGridOrigin = 1E+35 : XGridMax = -1E+35
		YGridOrigin = 1E+35 : YGridMax = -1E+35
		If NXSymmetric = N And NYSymmetric = N Then ' Symmetric about both axes.
			For I = 1 To N
				If XRcg(I) < XGridOrigin Then XGridOrigin = XRcg(I)
				If YRcg(I) < YGridOrigin Then YGridOrigin = YRcg(I)
			Next I
			XGridOrigin = XGridOrigin + Xcg
			XGridMax = Xcg
			YGridOrigin = YGridOrigin + Ycg
			YGridMax = Ycg
		ElseIf NXSymmetric = N And NYSymmetric < N Then  ' Symmetric about X axis.
			For I = 1 To N
				If XRcg(I) < XGridOrigin Then XGridOrigin = XRcg(I)
				If YRcg(I) < YGridOrigin Then YGridOrigin = YRcg(I)
				If XRcg(I) > XGridMax Then XGridMax = XRcg(I)
			Next I
			XGridOrigin = XGridOrigin + Xcg
			XGridMax = XGridMax + Xcg
			YGridOrigin = YGridOrigin + Ycg
			YGridMax = Ycg
		ElseIf NXSymmetric < N And NYSymmetric = N Then  ' Symmetric about Y axis.
			For I = 1 To N
				If XRcg(I) < XGridOrigin Then XGridOrigin = XRcg(I)
				If YRcg(I) < YGridOrigin Then YGridOrigin = YRcg(I)
				If YRcg(I) > YGridMax Then YGridMax = YRcg(I)
			Next I
			XGridOrigin = XGridOrigin + Xcg
			XGridMax = Xcg
			YGridOrigin = YGridOrigin + Ycg
			YGridMax = YGridMax + Ycg
		ElseIf NXSymmetric < N And NYSymmetric < N Then  ' No symmetry.
			For I = 1 To N
				If XRcg(I) < XGridOrigin Then XGridOrigin = XRcg(I)
				If XRcg(I) > XGridMax Then XGridMax = XRcg(I)
				If YRcg(I) < YGridOrigin Then YGridOrigin = YRcg(I)
				If YRcg(I) > YGridMax Then YGridMax = YRcg(I)
			Next I
			XGridOrigin = XGridOrigin + Xcg
			XGridMax = XGridMax + Xcg
			YGridOrigin = YGridOrigin + Ycg
			YGridMax = YGridMax + Ycg
		Else ' Two tires in the same place.
			'    Error recovery here
		End If
		
		GridInterval = WheelRadius / 3
		XGridNPoints = CShort((XGridMax - XGridOrigin) / GridInterval)
		YGridNPoints = CShort((YGridMax - YGridOrigin) / GridInterval)
		I = 3
		If XGridNPoints < I Then XGridNPoints = I
		If YGridNPoints < I Then YGridNPoints = I
		I = 35
		If XGridNPoints > I Then XGridNPoints = I
		If YGridNPoints > I Then YGridNPoints = I
		
    End Sub



    ' TODO (WPF): Is this needed? Or is it for the old WinForms picturebox stuff that will be replaced anyway by Bill's gear editor?

    'Public Function CreateAutoRedrawGraphicsPictureBox(ByVal oPic As PictureBox) As Graphics

    '    If oPic.Image Is Nothing Then
    '        oPic.Image = New Bitmap(oPic.ClientRectangle.Width, oPic.ClientRectangle.Height)
    '    End If
    '    CreateAutoRedrawGraphicsPictureBox = Graphics.FromImage(oPic.Image)
    '    oPic.Invalidate()

    'End Function

End Module


'http://visualbasic.about.com/od/learnvbnet/a/eventhandler.htm
'http://visualbasic.about.com/od/usingvbnet/a/lbhbug01.htm
'http://www.vbdotnetheaven.com/UploadFile/rajeshvs/StructuresInVbDotNet04192005235119PM/StructuresInVbDotNet.aspx
'http://visualbasic.about.com/od/quicktips/qt/vardeclare.htm

'http://dotnetref.blogspot.com/2008/05/finding-my-documents-folder-in-vbnet.html


'ProcessesRunning vb 2008 'check if program is running vb.net 'time delay in vb.net 
'http://vbcity.com/forums/t/39321.aspx
'http://www.dreamincode.net/forums/topic/84480-check-if-a-program-is-running/
'http://vbnetsample.blogspot.com/2007/08/start-and-kill-process.html

'http://msdn.microsoft.com/en-us/library/ms684139(v=vs.85).aspx
'http://www.freevbcode.com/ShowCode.Asp?ID=5879


'Graphics for Visual Basic 6.0 Users
'http://msdn.microsoft.com/en-us/library/9dtfzwyx(VS.90).aspx
'http://msdn.microsoft.com/en-us/library/aa265091(VS.60).aspx
'Convert VB6 "Drawing Styles" to .Net : VB.Net
'Microsoft Visual Basic: XOR DrawMode



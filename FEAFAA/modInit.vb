Module modInit

    Public Sub InitACLib()

        ' IG = aircraft group number.
        ' IA = aircraft number.
        ' NBelly = number of dual-wheel belly gear aircraft, like MD-11.
        ' The aircraft data is accessed by aircraft number, e.g.
        ' GL(I) = libGL(LibIndex(I))
        ' where I = index of aircraft within the current list
        ' and LibIndex(I) = IA
        ' Aircraft numbers within a group can be found using:
        ' StartGroupNo = LibACGroup(J):  EndGroupNo = LibACGroup(J + 1) - 1
        ' IA = StartGroupNo + IOffset - 1
        ' If IA > EndGroupNo Then NotInCurrentGroup
        ' where J = current group number = ILibACGroup
        ' IOffset = offset into current group.
        ' libNAC = number of aircraft in the library.
        ' NLibACGroups = number of aircraft groups in the library.

        ' GL = aircraft gross load (lb) (typically taxi weight).
        ' TX, TY = wheel coordinates on gear (in), TX = lateral, TY = forward.
        ' CP = contained (inflation) pressure (psi).
        ' NEVPTS = number of stress evaluation points.
        ' EVPTX, EVPTY = coordinates of stress evaluation points (in),
        '                X = lateral, Y = forward.
        ' GEAR$ = military gear type designation.
        ' IGEAR = LEDNEW gear type designation.
        ' TT, TS, TG, B = gear dimensions (definitions vary with gear type).
        ' NTTRACK = number of wheel tracks.
        ' XAC = lateral coordinates of the wheel tracks.

        Dim IA, I, IG As Short
        Dim c, M As double ' Linear equation.
        Dim Temp1, Temp, Temp2 As double

        IA = 1
        IG = 1
        LibACGroup(IG) = IA : LibACGroupName(IG) = "Generic"

        libACName(IA) = "Dual Whl-18"
        libGL(IA) = 18000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 101.5!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-50"
        libGL(IA) = 50000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-75"
        libGL(IA) = 75000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-100"
        libGL(IA) = 100000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-125"
        libGL(IA) = 125000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-150"
        libGL(IA) = 150000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-175"
        libGL(IA) = 175000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-200"
        libGL(IA) = 200000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-250"
        libGL(IA) = 250000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-300"
        libGL(IA) = 300000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Whl-400"
        libGL(IA) = 400000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 75.0!
        libGear(IA) = "D"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-50"
        libGL(IA) = 50000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 180.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-75"
        libGL(IA) = 75000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 180.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-100"
        libGL(IA) = 100000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 180.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-125"
        libGL(IA) = 125000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 180.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-150"
        libGL(IA) = 150000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 180.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-175"
        libGL(IA) = 175000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 180.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-200"
        libGL(IA) = 200000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 180.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-250"
        libGL(IA) = 250000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 180.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-300"
        libGL(IA) = 300000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 180.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 26.0! : libTS(IA) = 229.0!
        libTG(IA) = 0.0! : libB(IA) = 46.0!
        IA = IA + 1

        libACName(IA) = "Dual Tan-400"
        libGL(IA) = 400000.0! : libMGpcnt(IA) = 0.475
        libCP(IA) = 200.0!
        libGear(IA) = "F"
        libIGear(IA) = 3
        libTT(IA) = 30.0! : libTS(IA) = 220.0!
        libTG(IA) = 0.0! : libB(IA) = 55.0!
        IA = IA + 1

        ' Set the number of aircraft groups to 1 (only Generic)
        NLibACGroups = IG

        libNAC = IA
        NBelly = 0

        ' Set derived data where possible.

        For IA = 1 To libNAC

            If libGear(IA) = "A" Or libGear(IA) = "B" Then

                libNTires(IA) = 1
                libTX(IA, 1) = 0.0! : libTY(IA, 1) = 0.0!
                libTS(IA) = 0.0! : libTG(IA) = 0.0!
                libB(IA) = 0.0!
                If libGear(IA) = "A" Then ' 1 wheel, 1 gear.
                    libNMainGears(IA) = 1
                    libPcntOnMainGears(IA) = libMGpcnt(IA) * 100
                    libNTTrack(IA) = 1
                    libXAC(IA, 1) = 0.0!
                Else ' 1 wheel, 2 gears.
                    libNMainGears(IA) = 2
                    libPcntOnMainGears(IA) = libMGpcnt(IA) * 2 * 100
                    libNTTrack(IA) = 2
                    libXAC(IA, 1) = -libTT(IA) * 0.5
                    libXAC(IA, 2) = -libXAC(IA, 1)
                End If
                libNEVPTS(IA) = 1
                libEVPTX(IA, 1) = 0.0! : libEVPTY(IA, 1) = 0.0!

            ElseIf libGear(IA) = "D" Then  ' Dual-wheel, 2 gears.

                libNTires(IA) = 2
                libTX(IA, 1) = -libTT(IA) * 0.5 : libTY(IA, 1) = 0.0!
                libTX(IA, 2) = -libTX(IA, 1) : libTY(IA, 2) = 0.0!
                libTG(IA) = 0.0! : libB(IA) = 0.0!
                libNMainGears(IA) = 2
                libPcntOnMainGears(IA) = libMGpcnt(IA) * 2 * 100
                libNTTrack(IA) = 4
                libXAC(IA, 1) = -libTS(IA) * 0.5 - libTT(IA)
                libXAC(IA, 2) = -libTS(IA) * 0.5
                libXAC(IA, 3) = -libXAC(IA, 2)
                libXAC(IA, 4) = -libXAC(IA, 1)
                libNEVPTS(IA) = 4
                libEVPTX(IA, 1) = libTT(IA) * 0.5
                libEVPTX(IA, 2) = libTT(IA) * 0.375
                libEVPTX(IA, 3) = libTT(IA) * 0.25
                libEVPTX(IA, 4) = 0.0!
                For I = 1 To libNEVPTS(IA)
                    libEVPTY(IA, I) = 0.0!
                Next I

                '      Dual-tandem, 2 gears; dual-tandem, 2 gears + 1 dual belly gear.
                '      See after the current If block for belly gear data.
            ElseIf libGear(IA) = "F" Or libGear(IA) = "H" Then

                libNTires(IA) = 4
                libTX(IA, 1) = -libTT(IA) * 0.5 : libTY(IA, 1) = 0.0!
                libTX(IA, 2) = libTT(IA) * 0.5 : libTY(IA, 2) = 0.0!
                libTX(IA, 3) = libTX(IA, 1) : libTY(IA, 3) = libB(IA)
                libTX(IA, 4) = libTX(IA, 2) : libTY(IA, 4) = libB(IA)
                libNMainGears(IA) = 2
                libPcntOnMainGears(IA) = libMGpcnt(IA) * 2 * 100
                libNTTrack(IA) = 4
                libXAC(IA, 1) = -libTS(IA) * 0.5 - libTT(IA)
                libXAC(IA, 2) = -libTS(IA) * 0.5
                libXAC(IA, 3) = -libXAC(IA, 2)
                libXAC(IA, 4) = -libXAC(IA, 1)
                libNEVPTS(IA) = 8
                Temp = libTT(IA) * 0.5
                c = 0.5604 * libTT(IA) - 0.2637 * libB(IA)
                If c < 0.0! Then c = 0.0!
                M = -c / Temp
                libEVPTX(IA, 1) = Temp
                libEVPTY(IA, 1) = 0.0!
                libEVPTX(IA, 2) = Temp * 0.8
                libEVPTY(IA, 2) = c + M * libEVPTX(IA, 2)
                libEVPTX(IA, 3) = Temp * 0.6
                libEVPTY(IA, 3) = c + M * libEVPTX(IA, 3)
                libEVPTX(IA, 4) = Temp * 0.4
                libEVPTY(IA, 4) = c + M * libEVPTX(IA, 4)
                libEVPTX(IA, 5) = Temp * 0.2
                libEVPTY(IA, 5) = c + M * libEVPTX(IA, 5)
                libEVPTX(IA, 6) = 0.0!
                libEVPTY(IA, 6) = c
                libEVPTX(IA, 7) = 0.0!
                libEVPTY(IA, 7) = (libB(IA) * 0.5 - c) * 0.5 + c
                libEVPTX(IA, 8) = 0.0!
                libEVPTY(IA, 8) = libB(IA) * 0.5

            ElseIf libGear(IA) = "J" Then  ' 747 configuration.

                libNTires(IA) = 4
                libTX(IA, 1) = -libTT(IA) * 0.5 : libTY(IA, 1) = 0.0!
                libTX(IA, 2) = libTT(IA) * 0.5 : libTY(IA, 2) = 0.0!
                libTX(IA, 3) = libTX(IA, 1) : libTY(IA, 3) = libB(IA)
                libTX(IA, 4) = libTX(IA, 2) : libTY(IA, 4) = libB(IA)
                libNMainGears(IA) = 1
                libPcntOnMainGears(IA) = libMGpcnt(IA) * 1 * 100
                libNEVPTS(IA) = 6
                Temp = libTT(IA)
                libEVPTX(IA, 1) = Temp * 0.5 : libEVPTY(IA, 1) = 0.0!
                libEVPTX(IA, 2) = Temp * 0.4 : libEVPTY(IA, 2) = 0.0!
                libEVPTX(IA, 3) = Temp * 0.3 : libEVPTY(IA, 3) = 0.0!
                libEVPTX(IA, 4) = Temp * 0.2 : libEVPTY(IA, 4) = 0.0!
                libEVPTX(IA, 5) = Temp * 0.1 : libEVPTY(IA, 5) = 0.0!
                libEVPTX(IA, 6) = 0.0! : libEVPTY(IA, 6) = 0.0!
                libNTTrack(IA) = 4
                libXAC(IA, 1) = -libTS(IA) * 0.5 - libTT(IA)
                libXAC(IA, 2) = -libTS(IA) * 0.5
                libXAC(IA, 3) = -libXAC(IA, 2)
                libXAC(IA, 4) = -libXAC(IA, 1)
                'End added by drb
                'Lia 06/12/01
            ElseIf libGear(IA) = "T" Then  ' A380-800 Belly 6 wheel configuration.

                libNTires(IA) = 6
                Temp = libTT(IA) * 0.5 : Temp1 = libB(IA) : Temp2 = libTG(IA) * 0.5

                libTX(IA, 1) = -Temp : libTY(IA, 1) = -Temp1
                libTX(IA, 2) = Temp : libTY(IA, 2) = -Temp1
                libTX(IA, 3) = -Temp2 : libTY(IA, 3) = 0.0#
                libTX(IA, 4) = Temp2 : libTY(IA, 4) = 0.0#
                libTX(IA, 5) = -Temp : libTY(IA, 5) = Temp1
                libTX(IA, 6) = Temp : libTY(IA, 6) = Temp1
                libNMainGears(IA) = 2
                libPcntOnMainGears(IA) = libMGpcnt(IA) * 2 * 100
                libNEVPTS(IA) = 6
                Temp2 = (libTS(IA) - libTT(IA)) * 0.5
                libEVPTX(IA, 1) = Temp * 0.5 : libEVPTY(IA, 1) = 0.0!
                libEVPTX(IA, 2) = Temp * 0.4 : libEVPTY(IA, 2) = 0.0!
                libEVPTX(IA, 3) = Temp * 0.3 : libEVPTY(IA, 3) = 0.0!
                libEVPTX(IA, 4) = Temp * 0.2 : libEVPTY(IA, 4) = 0.0!
                libEVPTX(IA, 5) = Temp * 0.1 : libEVPTY(IA, 5) = 0.0!
                libEVPTX(IA, 6) = 0.0! : libEVPTY(IA, 6) = 0.0!
                libNTTrack(IA) = 4
                libXAC(IA, 1) = -libTS(IA) * 0.5 - libTT(IA)
                libXAC(IA, 2) = -libTS(IA) * 0.5
                libXAC(IA, 3) = -libXAC(IA, 2)
                libXAC(IA, 4) = -libXAC(IA, 1)
                'Lia 06/12/01 end

            End If

            If libGear(IA) = "H" Then ' Set belly gear data.
                NBelly = NBelly + 1
                I = libNAC + NBelly
                libACName(I) = libACName(IA) & BellyExt
                libGL(I) = libGL(IA)
                libMGpcnt(I) = 0.95 - 2.0! * libMGpcnt(IA)
                If libACName(IA) = "DC-10-30" Or libACName(IA) = "KC-10" Then
                    libCP(I) = 153.0!
                ElseIf libACName(IA) = "MD-11" Then
                    libCP(I) = 180.0!
                Else
                    libCP(I) = libCP(IA) * 2.0! * libMGpcnt(I) / libMGpcnt(IA)
                End If
                libGear(I) = "HB"
                libIGear(I) = 2 ' Same as single-wheel, two gears, for coverage to pass.
                libTT(I) = libTG(IA) : libTS(I) = 0.0!
                libTG(IA) = 0.0!
                libTG(I) = 0.0! : libB(I) = 0.0!
                libNTires(I) = 2
                libTX(I, 1) = -libTT(I) * 0.5 : libTY(I, 1) = 0.0!
                libTX(I, 2) = -libTX(I, 1) : libTY(I, 2) = 0.0!
                libNTTrack(I) = 2
                libXAC(I, 1) = -libTT(I) * 0.5
                libXAC(I, 2) = libTT(I) * 0.5
                libNEVPTS(I) = 4
                libEVPTX(I, 1) = libTT(I) * 0.5 : libEVPTY(I, 1) = 0.0!
                libEVPTX(I, 2) = libTT(I) * 0.375 : libEVPTY(I, 2) = 0.0!
                libEVPTX(I, 3) = libTT(I) * 0.25 : libEVPTY(I, 3) = 0.0!
                libEVPTX(I, 4) = 0.0! : libEVPTY(I, 4) = 0.0!
            End If

            libXGridOrigin(IA) = 0
            libXGridMax(IA) = 0
            libXGridNPoints(IA) = 0
            libAlpha(IA) = 0
            libCoverages(IA) = StandardCoverages

        Next IA

    End Sub

End Module

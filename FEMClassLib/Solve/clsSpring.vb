'This file contains all the methods of spring.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to compute residual and stiffness for discrete elements
    ''' </summary>
    ''' <param name="mtypde"></param>
    ''' <param name="cmde"></param>
    ''' <param name="ixde"></param>
    ''' <param name="sclf"></param>
    ''' <param name="dehv"></param>
    ''' <param name="nmelde"></param>
    ''' <param name="x"></param>
    ''' <param name="u"></param>
    ''' <param name="ui"></param>
    ''' <param name="idp"></param>
    ''' <param name="r"></param>
    ''' <param name="npc"></param>
    ''' <param name="p"></param>
    ''' <param name="icnt2"></param>
    ''' <param name="idir"></param>
    ''' <param name="lc"></param>
    ''' <param name="xmg"></param>
    ''' <param name="rf"></param>
    ''' <param name="diag"></param>
    Public Sub spring(ByRef mtypde() As Integer, ByRef cmde(,) As Double, ByRef ixde(,) As Integer,
                      ByRef sclf() As Double, ByRef dehv(,) As Double, ByRef nmelde As Integer,
                      ByRef x(,) As Double, ByRef u() As Double, ByRef ui() As Double, ByRef idp(,) As Integer,
                     ByRef r() As Double, ByRef npc() As Integer, ByRef p() As Double,
                      ByRef icnt2 As Integer, ByRef idir() As Integer, ByRef lc() As Integer,
                      ByRef xmg() As Double, ByRef rf() As Double, ByRef diag(,) As Double)

        Dim rl(6), xc(6), rlm(6), length, loc, npoint, dfdu, dldt, dfdldt, d2d1 As Double ' QW 12-12-2018-
        Dim mt, node1, node2, ierr, iflag, lmax As Integer
        Dim ux1, uy1, uz1, ux2, uy2, uz2, vx1, vy1, vz1, vx2, vy2, vz2 As Double
        Dim ax1, ay1, az1, ax2, ay2, az2, ax, ay, az As Double
        Dim dx, dy, dz, ux, uy, uz, vx, vy, vz As Double
        Dim du, nx, ny, nz, da, dv, dui As Double
        Dim coeff, force, forcx, forcy, forcz, stfnes As Double
        Dim ftr, aksitr, fyn, fcurs, sdu, aksin, fyc, dup, df As Double
        Dim xmag, dyt, dyc, fyt, dlu, dl1, fy0 As Double
        Dim lstif As Boolean

        Dim Msg1 = "entering spring.F"

        Dim zero = 0.0
        Dim one = 1.0
        'TODO common/bk12/ has dt. But in this file it is dt1. Assumed it is dt. Check.
        Dim dti = 1.0 / (dt + 1.0E-20)
        Dim numwds = 0

        Dim mx, n, i, k As Integer 'YC 092018

        Dim lm(6) As Integer, s_1(21) As Double

        'For i = 0 To nmelde - 1    ' YC 102418
        For i = 1 To nmelde

            'mx = ixde(2, i) ' YC 102418
            mx = ixde(3, i)
            mt = mtypde(mx)

            'node1 = ixde(0, i)      ' YC 102418
            'node2 = ixde(1, i)
            node1 = ixde(1, i)
            node2 = ixde(2, i)

            'TODO - Check
            '      call unpkid(lm(1),idp(1,node1),1)
            'Call clsInput.unpkid(lm, idp, 0, node1, 1) 'YC 092018
            'Call objNik3d.unpkid(lm, idp, 0, node1 - 1, 1)  ' YC? 102418
            'Call objComsub.ArrayExtract1Dfrom2D(idp, 0, node1, lm, 3)       ' QW 12-12-2018-
            Call objComsub.ArrayExtract1Dfrom2D(idp, 1 - 1, node1 - 1, lm, 3)      ' QW 9-6-2019
            'TODO - Check
            '      call unpkid(lm(4),idp(1,node2),1)
            'Call clsInput.unpkid(lm.Skip(3).ToArray(), idp, 0, node2, 1)
            'Call objNik3d.unpkid(lm.Skip(3).ToArray(), idp, 0, node2 - 1, 1)   ' YC? 102418
            'Call objComsub.ArrayExtract1Dfrom2D(idp, 0, node2, lm.Skip(3).ToArray(), 3)     ' QW 12-12-2018-
            Dim lm_4(3) As Integer
            Call objComsub.ArrayExtract1Dfrom2D(idp, 1 - 1, node2 - 1, lm_4, 3)
            Call objComsub.ArrayInsert1Dto1D(lm_4, 3, lm, 4 - 1)                            ' QW 9-6-2019
            ' YC 102418
            'xc(0) = x(0, ixde(0, i))
            'xc(1) = x(1, ixde(0, i))
            'xc(2) = x(2, ixde(0, i))
            'xc(3) = x(0, ixde(1, i))
            'xc(4) = x(1, ixde(1, i))
            'xc(5) = x(2, ixde(1, i))
            'is_nodes(0, i) = ixde(0, i)
            'is_nodes(1, i) = ixde(1, i)
            xc(1) = x(1, ixde(1, i))
            xc(2) = x(2, ixde(1, i))
            xc(3) = x(3, ixde(1, i))
            xc(4) = x(1, ixde(2, i))
            xc(5) = x(2, ixde(2, i))
            xc(6) = x(3, ixde(2, i))
            is_nodes(1, i) = ixde(1, i)
            is_nodes(2, i) = ixde(2, i)
            ' YC 102418 END

            ux1 = 0.0 : uy1 = 0.0 : uz1 = 0.0 : ux2 = 0.0 : uy2 = 0.0 : uz2 = 0.0
            vx1 = 0.0 : vy1 = 0.0 : vz1 = 0.0 : vx2 = 0.0 : vy2 = 0.0 : vz2 = 0.0
            ax1 = 0.0 : ay1 = 0.0 : az1 = 0.0 : ax2 = 0.0 : ay2 = 0.0 : az2 = 0.0

            'For k = 0 To 5     ' YC 102418
            For k = 1 To 6
                is_dofs(k, i) = lm(k)
            Next

            'If lm(0) <> 0 Then     ' YC 102418
            If lm(1) <> 0 Then

                'xc(0) = xc(0) + u(lm(0))       ' YC 102418
                'ux1 = ui(lm(0))
                xc(1) = xc(1) + u(lm(1))
                ux1 = ui(lm(1))

            End If

            'If lm(1) <> 0 Then ' YC 102418
            If lm(2) <> 0 Then

                'xc(1) = xc(1) + u(lm(1))       ' YC 102418
                'uy1 = ui(lm(1))
                xc(2) = xc(2) + u(lm(2))
                uy1 = ui(lm(2))

            End If

            'If lm(2) <> 0 Then ' YC 102418
            If lm(3) <> 0 Then

                'xc(2) = xc(2) + u(lm(2))       ' YC 102418
                'uz1 = ui(lm(2))
                xc(3) = xc(3) + u(lm(3))
                uz1 = ui(lm(3))

            End If

            'If lm(3) <> 0 Then ' YC 102418
            If lm(4) <> 0 Then

                'xc(3) = xc(3) + u(lm(3))       ' YC 102418
                'ux2 = ui(lm(3))
                xc(4) = xc(4) + u(lm(4))
                ux2 = ui(lm(4))

            End If


            'If lm(4) <> 0 Then      ' YC 102418
            If lm(5) <> 0 Then

                'xc(4) = xc(4) + u(lm(4))       ' YC 102418
                'uy2 = ui(lm(4))
                xc(5) = xc(5) + u(lm(5))
                uy2 = ui(lm(5))

            End If

            'If lm(5) <> 0 Then    ' YC 102418
            If lm(6) <> 0 Then

                'xc(5) = xc(5) + u(lm(5))        ' YC 102418
                'uz2 = ui(lm(5))
                xc(6) = xc(6) + u(lm(6))
                uz2 = ui(lm(6))

            End If

            ' YC 102418
            'dx = xc(3) - xc(0)
            'dy = xc(4) - xc(1)
            'dz = xc(5) - xc(2)
            dx = xc(4) - xc(1)
            dy = xc(5) - xc(2)
            dz = xc(6) - xc(3)
            ' YC 102418 END

            ux = ux2 - ux1
            uy = uy2 - uy1
            uz = uz2 - uz1
            vx = vx2 - vx1
            vy = vy2 - vy1
            vz = vz2 - vz1
            ax = ax2 - ax1
            ay = ay2 - ay1
            az = az2 - az1

            length = Math.Sqrt(Math.Pow(dx, 2) + Math.Pow(dy, 2) + Math.Pow(dz, 2))

            'dehv(10, i) = length          ' YC 102418
            dehv(11, i) = length
            du = length - dehv(7, i)

            nx = dx / (length + 1.0E-20)
            ny = dy / (length + 1.0E-20)
            nz = dz / (length + 1.0E-20)

            da = nx * ax + ny * ay + nz * az
            dv = nx * vx + ny * vy + nz * vz
            dui = nx * ux + ny * uy + nz * uz

            Select Case mt
                Case 1

                    'force = dehv(9, i) + sclf(i) * cmde(0, mx) * du         ' YC 102418
                    force = dehv(10, i) + sclf(i) * cmde(1, mx) * du

                    'If cmde(1, mx) > 0.5 And cmde(1, mx) < 1.5 Then  ' YC 102418
                    If cmde(2, mx) > 0.5 And cmde(2, mx) < 1.5 Then

                        'forcx = sclf(i) * cmde(0, mx) * ux      ' YC 102418
                        forcx = sclf(i) * cmde(1, mx) * ux     ' YC 052019-1-110419
                        'forcx = dehv(10, i) + sclf(i) * cmde(1, mx) * ux   ' QW 11-07-2019

                        force = forcx
                        forcy = 0.0
                        forcz = 0.0
                        nx = 1.0
                        ny = 0.0
                        nz = 0.0
                    End If

                    'If cmde(1, mx) > 1.5 And cmde(1, mx) < 2.5 Then      ' YC 102418
                    If cmde(2, mx) > 1.5 And cmde(2, mx) < 2.5 Then

                        forcx = 0.0

                        'forcy = sclf(i) * cmde(0, mx) * uy    ' YC 102418
                        forcy = sclf(i) * cmde(1, mx) * uy     ' YC 052019-1-110419
                        'forcy = dehv(10, i) + sclf(i) * cmde(1, mx) * uy    ' QW 11-07-2019

                        force = forcy
                        forcz = 0.0
                        nx = 0.0
                        ny = 1.0
                        nz = 0.0
                    End If

                    'If cmde(1, mx) > 2.5 And cmde(1, mx) < 3.5 Then     ' YC 102418
                    If cmde(2, mx) > 2.5 And cmde(2, mx) < 3.5 Then
                        forcx = 0.0
                        forcy = 0.0

                        'forcz = sclf(i) * cmde(0, mx) * uz   ' YC 102418
                        forcz = sclf(i) * cmde(1, mx) * uz     ' YC 052019-1-110419
                        'forcz = dehv(10, i) + sclf(i) * cmde(1, mx) * uz    ' QW 11-07-2019

                        force = forcz
                        nx = 0.0
                        ny = 0.0
                        nz = 1.0
                    End If

                    'If cmde(1, mx) > -0.5 And cmde(1, mx) < 0.5 Then     ' YC 102418
                    If cmde(2, mx) > -0.5 And cmde(2, mx) < 0.5 Then
                        forcx = nx * force
                        forcy = ny * force
                        forcz = nz * force
                    End If

                    ' YC 102418
                    'dehv(9, i) = force
                    'dehv(6, i) = length
                    'stfnes = sclf(i) * cmde(0, mx)
                    dehv(10, i) = force
                    dehv(7, i) = length
                    stfnes = sclf(i) * cmde(1, mx)
                    ' YC 102418 END
                    GoTo 90

                Case 2

                    'coeff = -sclf(i) * cmde(0, mx)     ' YC 102418
                    coeff = -sclf(i) * cmde(1, mx)

                    force = coeff * (a4 * dv + a5 * da)
                    If iphase > 2 Then force = force - coeff * a1 * dui
                    forcx = nx * force
                    forcy = ny * force
                    forcz = nz * force

                    ' YC 102418
                    'dehv(9, i) = force
                    'dehv(6, i) = length
                    'stfnes = sclf(i) * cmde(0, mx) * a1
                    dehv(10, i) = force
                    dehv(7, i) = length
                    stfnes = sclf(i) * cmde(1, mx) * a1
                    ' YC 102418 END

                    GoTo 90

                Case 3
                    '   ... bilinear plasticity ...

                    ' YC 102418
                    'ftr = cmde(0, mx) * du + dehv(9, i) / sclf(i)
                    'aksitr = Math.Abs(ftr - dehv(4, i))
                    'fyn = cmde(2, mx) + dehv(3, i) * cmde(1, mx)
                    ftr = cmde(1, mx) * du + dehv(10, i) / sclf(i)
                    aksitr = Math.Abs(ftr - dehv(5, i))
                    fyn = cmde(3, mx) + dehv(4, i) * cmde(2, mx)
                    ' YC 102418 END

                    If aksitr <= fyn Then
                        fcurs = ftr * sclf(i)

                        'stfnes = cmde(0, mx)         ' YC 102418
                        stfnes = cmde(1, mx)
                    Else

                        'sdu = clsInput.sign(one, du)  'YC 092018
                        sdu = objComsub.sign(one, du)

                        'aksin = Math.Abs(dehv(9, i) - dehv(4, i))    ' YC 102418
                        aksin = Math.Abs(dehv(10, i) - dehv(5, i))

                        fyc = Math.Max(fyn, aksin)

                        ' YC 102418
                        'dup = (aksitr - fyc) / cmde(0, mx)
                        'df = (fyc - aksin + cmde(1, mx) * dup) * sdu
                        'dehv(3, i) = dehv(3, i) + cmde(3, mx) * dup
                        'dehv(4, i) = dehv(4, i) + (1.0 - cmde(3, mx)) * cmde(1, mx) * dup * sdu
                        'fcurs = dehv(9, i) + df * sclf(i)
                        'stfnes = Math.Abs((Math.Abs(fcurs) - Math.Abs(dehv(9, i) / sclf(i))) / du)
                        dup = (aksitr - fyc) / cmde(1, mx)
                        df = (fyc - aksin + cmde(2, mx) * dup) * sdu
                        dehv(4, i) = dehv(4, i) + cmde(4, mx) * dup
                        dehv(5, i) = dehv(5, i) + (1.0 - cmde(4, mx)) * cmde(2, mx) * dup * sdu
                        fcurs = dehv(10, i) + df * sclf(i)
                        stfnes = Math.Abs((Math.Abs(fcurs) - Math.Abs(dehv(10, i) / sclf(i))) / du)
                        ' YC 102418 END

                    End If

                    forcx = nx * fcurs
                    forcy = ny * fcurs
                    forcz = nz * fcurs

                    'dehv(9, i) = fcurs        ' YC 102418
                    'dehv(6, i) = length
                    dehv(10, i) = fcurs
                    dehv(7, i) = length

                    GoTo 90

                Case 4
                    ierr = 0
                    xmag = 1.0

                    'n = clsInput.nint(cmde(0, mx))  'YC 092018, YC 102418
                    n = objComsub.nint(cmde(1, mx))

                    loc = npc(n)
                    npoint = (npc(n + 1) - loc) / 2
                    force = 0.0

                    'du = length - dehv(0, i)         ' YC 102418
                    du = length - dehv(1, i)

                    'Call itrpde(p.Skip(loc).ToArray(), du, npoint, force, dfdu, sclf.Skip(i).ToArray())        ' YC 102418
                    Call itrpde(p.Skip(loc - 1).ToArray(), du, npoint, force, dfdu, sclf.Skip(i).ToArray())

                    forcx = nx * force
                    forcy = ny * force
                    forcz = nz * force
                    stfnes = dfdu

                    'dehv(9, i) = force         ' YC 102418
                    'dehv(6, i) = length
                    dehv(10, i) = force
                    dehv(7, i) = length

                    GoTo 90

                Case 5
                    ierr = 0
                    xmag = 1.0

                    'n = clsInput.nint(cmde(0, mx)) 'YC 092018, YC 102418
                    n = objComsub.nint(cmde(1, mx))

                    loc = npc(n)
                    npoint = (npc(n + 1) - loc) / 2
                    dldt = du * dti
                    force = 0.0

                    'Call itrpde(p.Skip(loc).ToArray, dldt, npoint, force, dfdldt, sclf.Skip(i).ToArray)        ' YC 102418
                    Call itrpde(p.Skip(loc - 1).ToArray, dldt, npoint, force, dfdldt, sclf.Skip(i).ToArray)

                    forcx = nx * force
                    forcy = ny * force
                    forcz = nz * force

                    'dehv(9, i) = force     ' YC 102418
                    'dehv(6, i) = length
                    dehv(10, i) = force
                    dehv(7, i) = length

                    GoTo 90

                Case 6
                    '     general nonlinear springs

                    'iflag = clsInput.nint(dehv(0, i))  'YC 092018, YC 102418
                    iflag = objComsub.nint(dehv(1, i))

                    'dyt = dehv(1, i)       ' YC 102418
                    'dyc = dehv(2, i)
                    dyt = dehv(2, i)
                    dyc = dehv(3, i)

                    If iflag <> 0 Or du > dyt Or du < dyc Then GoTo 62

                    'n = clsInput.nint(cmde(0, mx)) 'YC 092018, YC 102418
                    n = objComsub.nint(cmde(1, mx))

                    loc = npc(n)
                    npoint = (npc(n + 1) - loc) / 2

                    'Call itrpd2(p.Skip(loc).ToArray(), du, npoint, force, sclf.Skip(i).ToArray(), 1, dehv(3, i), d2d1) ' YC 102418
                    Call itrpd2(p.Skip(loc - 1).ToArray(), du, npoint, force, sclf.Skip(i - 1).ToArray(), 1, dehv(4, i), d2d1)

                    'dehv(9, i) = force ' YC 102418
                    dehv(10, i) = force

                    stfnes = d2d1
                    GoTo 80

62:                 If du < dyt Then GoTo 64

                    ' ...tension yielding

                    'n = clsInput.nint(cmde(0, mx)) 'YC 092018, YC 102418
                    n = objComsub.nint(cmde(1, mx))

                    loc = npc(n)
                    npoint = (npc(n + 1) - loc) / 2

                    'Call itrpd2(p.Skip(loc).ToArray(), du, npoint, force, sclf.Skip(i).ToArray(), 1, dehv(3, i), d2d1)  ' YC 102418
                    Call itrpd2(p.Skip(loc - 1).ToArray(), du, npoint, force, sclf.Skip(i - 1).ToArray(), 1, dehv(4, i), d2d1)


                    stfnes = d2d1

                    'dehv(1, i) = du     ' YC 102418
                    'dehv(7, i) = force
                    'dehv(9, i) = force
                    'dehv(0, i) = 1.0
                    dehv(2, i) = du
                    dehv(8, i) = force
                    dehv(10, i) = force
                    dehv(1, i) = 1.0

                    GoTo 80

64:                 If du > dyc Then GoTo 66

                    ' ...compression yielding

                    'n = clsInput.nint(cmde(0, mx))  'YC 092018, YC 102418
                    n = objComsub.nint(cmde(1, mx))

                    loc = npc(n)
                    npoint = (npc(n + 1) - loc) / 2

                    'Call itrpd2(p.Skip(loc).ToArray(), du, npoint, force, sclf.Skip(i).ToArray(), 1, dehv(4, i), d2d1)   ' YC 102418
                    Call itrpd2(p.Skip(loc - 1).ToArray(), du, npoint, force, sclf.Skip(i - 1).ToArray(), 1, dehv(4, i), d2d1)


                    stfnes = d2d1

                    'dehv(2, i) = du        ' YC 102418
                    'dehv(8, i) = force
                    'dehv(9, i) = force
                    'dehv(0, i) = 3.0
                    dehv(3, i) = du
                    dehv(9, i) = force
                    dehv(10, i) = force
                    dehv(1, i) = 3.0

                    GoTo 80

66:                 If iflag <> 2 Then GoTo 68

                    ' ...loading along elastic region of unloading curve

                    'n = clsInput.nint(cmde(1, mx))  'YC? 092018, YC 102418
                    n = objComsub.nint(cmde(2, mx))

                    loc = npc(n)
                    npoint = (npc(n + 1) - loc) / 2

                    'Call itrpd2(p.Skip(loc).ToArray(), du, npoint, force, sclf.Skip(i).ToArray(), 1, dehv(5, i), d2d1)  ' YC 102418
                    Call itrpd2(p.Skip(loc - 1).ToArray(), du, npoint, force, sclf.Skip(i - 1).ToArray(), 1, dehv(6, i), d2d1)

                    'dehv(9, i) = force ' YC 102418
                    dehv(10, i) = force

                    stfnes = d2d1
                    GoTo 80

68:                 If iflag <> 1 Then GoTo 70

                    ' ...transition to unloading curve from tensile yielding

                    'fyt = dehv(7, i)     ' YC 102418
                    fyt = dehv(8, i)

                    'n = clsInput.nint(cmde(1, mx)) 'YC 092018, YC 102418
                    n = objComsub.nint(cmde(2, mx))

                    loc = npc(n)
                    npoint = (npc(n + 1) - loc) / 2

                    'Call itrpd2(p.Skip(loc).ToArray(), dl1, npoint, fyt, sclf.Skip(i).ToArray(), 2, zero, d2d1)     ' YC 102418
                    Call itrpd2(p.Skip(loc - 1).ToArray(), dl1, npoint, fyt, sclf.Skip(i - 1).ToArray(), 2, zero, d2d1)


                    dlu = dyt - dl1

                    'Call itrpd2(p.Skip(loc).ToArray(), du, npoint, force, sclf.Skip(i).ToArray(), 1, dlu, d2d1)        ' YC 102418
                    Call itrpd2(p.Skip(loc - 1).ToArray(), du, npoint, force, sclf.Skip(i - 1).ToArray(), 1, dlu, d2d1)

                    stfnes = d2d1

                    'fy0 = sclf(i) * (cmde(3, mx) - cmde(4, mx))      ' YC 102418
                    'fyc = fyt - fy0 - cmde(2, mx) * (-fy0 + 2.0 * fyt)
                    fy0 = sclf(i) * (cmde(4, mx) - cmde(5, mx))
                    fyc = fyt - fy0 - cmde(3, mx) * (-fy0 + 2.0 * fyt)

                    'Call itrpd2(p.Skip(loc).ToArray(), dyc, npoint, fyc, sclf.Skip(i).ToArray(), 2, dlu, d2d1)     ' YC 102418
                    Call itrpd2(p.Skip(loc - 1).ToArray(), dyc, npoint, fyc, sclf.Skip(i - 1).ToArray(), 2, dlu, d2d1)


                    'n = clsInput.nint(cmde(0, mx))  'YC 092018, YC 102418
                    n = objComsub.nint(cmde(1, mx))

                    loc = npc(n)
                    npoint = (npc(n + 1) - loc) / 2

                    'Call itrpd2(p.Skip(loc).ToArray(), dl1, npoint, fyc, sclf.Skip(i).ToArray(), 2, zero, d2d1)   ' YC 102418
                    Call itrpd2(p.Skip(loc - 1).ToArray(), dl1, npoint, fyc, sclf.Skip(i - 1).ToArray(), 2, zero, d2d1)

                    'dehv(0, i) = 2.0         ' YC 102418
                    'dehv(2, i) = dyc
                    'dehv(4, i) = dyc - dl1
                    'dehv(5, i) = dlu
                    'dehv(8, i) = fyc
                    'dehv(9, i) = force
                    dehv(1, i) = 2.0
                    dehv(3, i) = dyc
                    dehv(5, i) = dyc - dl1
                    dehv(6, i) = dlu
                    dehv(9, i) = fyc
                    dehv(10, i) = force

                    GoTo 80

            End Select

            ' ...transition to unloading curve from compressive yielding

            '70:         fyc = dehv(8, i)     ' YC 102418
70:         fyc = dehv(9, i)

            'n = clsInput.nint(cmde(1, mx))  'YC 092018,YC 102418
            n = objComsub.nint(cmde(2, mx))

            loc = npc(n)
            npoint = (npc(n + 1) - loc) / 2

            'Call itrpd2(p.Skip(loc).ToArray(), dl1, npoint, fyc, sclf.Skip(i).ToArray(), 2, zero, d2d1)    ' YC 102418
            Call itrpd2(p.Skip(loc - 1).ToArray(), dl1, npoint, fyc, sclf.Skip(i - 1).ToArray(), 2, zero, d2d1)


            dlu = dyc - dl1

            'Call itrpd2(p.Skip(loc).ToArray(), du, npoint, force, sclf.Skip(i).ToArray(), 1, dlu, d2d1)     ' YC 102418
            Call itrpd2(p.Skip(loc - 1).ToArray(), du, npoint, force, sclf.Skip(i - 1).ToArray(), 1, dlu, d2d1)


            stfnes = d2d1

            'fy0 = sclf(i) * (cmde(3, mx) - cmde(4, mx))   ' YC 102418
            'fyt = fyc + fy0 - cmde(2, mx) * (fy0 + 2.0 * fyc)
            fy0 = sclf(i) * (cmde(4, mx) - cmde(5, mx))
            fyt = fyc + fy0 - cmde(3, mx) * (fy0 + 2.0 * fyc)

            'Call itrpd2(p.Skip(loc).ToArray(), dyt, npoint, fyt, sclf.Skip(i).ToArray(), 2, dlu, d2d1)     ' YC 102418
            Call itrpd2(p.Skip(loc - 1).ToArray(), dyt, npoint, fyt, sclf.Skip(i - 1).ToArray(), 2, dlu, d2d1)

            'n = clsInput.nint(cmde(0, mx)) 'YC 092018,  YC 102418
            n = objComsub.nint(cmde(1, mx))

            loc = npc(n)
            npoint = (npc(n + 1) - loc) / 2

            'Call itrpd2(p.Skip(loc).ToArray(), dl1, npoint, fyt, sclf.Skip(i).ToArray(), 2, zero, d2d1)     ' YC 102418
            Call itrpd2(p.Skip(loc - 1).ToArray(), dl1, npoint, fyt, sclf.Skip(i - 1).ToArray(), 2, zero, d2d1)


            '            dehv(0, i) = 2.0                   ' YC 102418
            '            dehv(1, i) = dyt
            '            dehv(3, i) = dyt - dl1
            '            dehv(5, i) = dlu
            '            dehv(7, i) = fyt
            '            dehv(9, i) = force
            dehv(1, i) = 2.0
            dehv(2, i) = dyt
            dehv(4, i) = dyt - dl1
            dehv(6, i) = dlu
            dehv(8, i) = fyt
            dehv(10, i) = force

80:         forcx = nx * force
            forcy = ny * force
            forcz = nz * force

            '90:         rl(0) = -forcx            ' YC 102418 
            '            rl(1) = -forcy
            '            rl(2) = -forcz
            '            rl(3) = forcx
            '            rl(4) = forcy
            '            rl(5) = forcz
90:         rl(1) = -forcx
            rl(2) = -forcy
            rl(3) = -forcz
            rl(4) = forcx
            rl(5) = forcy
            rl(6) = forcz

            'For j = 0 To 5     ' YC 102418 
            For j = 1 To 6
                rlm(j) = 0
            Next

            ' YC 102418 
            ''TODO - original variable s was defined as a 2d array in common/bk31/
            ''used s_1 instead in common/rocspr/ CHECK
            's_1(0) = stfnes * nx * nx
            's_1(1) = stfnes * nx * ny
            's_1(2) = stfnes * ny * ny
            's_1(3) = stfnes * nx * nz
            's_1(4) = stfnes * ny * nz
            's_1(5) = stfnes * nz * nz
            's_1(6) = -s_1(0)
            's_1(7) = -s_1(1)
            's_1(8) = -s_1(3)
            's_1(9) = s_1(0)
            's_1(10) = -s_1(1)
            's_1(11) = -s_1(2)
            's_1(12) = -s_1(4)
            's_1(13) = s_1(1)
            's_1(14) = s_1(2)
            's_1(15) = -s_1(3)
            's_1(16) = -s_1(4)
            's_1(17) = -s_1(5)
            's_1(18) = s_1(3)
            's_1(19) = s_1(4)
            's_1(20) = s_1(5)
            s_1(1) = stfnes * nx * nx
            s_1(2) = stfnes * nx * ny
            s_1(3) = stfnes * ny * ny
            s_1(4) = stfnes * nx * nz
            s_1(5) = stfnes * ny * nz
            s_1(6) = stfnes * nz * nz
            s_1(7) = -s_1(1)
            s_1(8) = -s_1(2)
            s_1(9) = -s_1(4)        ' QW 12-12-2018-
            s_1(10) = s_1(1)
            s_1(11) = -s_1(2)
            s_1(12) = -s_1(3)
            s_1(13) = -s_1(5)       ' QW 12-12-2018-
            s_1(14) = s_1(2)
            s_1(15) = s_1(3)
            s_1(16) = -s_1(4)
            s_1(17) = -s_1(5)
            s_1(18) = -s_1(6)
            s_1(19) = s_1(4)
            s_1(20) = s_1(5)
            s_1(21) = s_1(6)
            ' YC 102418  END

            ' add terms to stiffness diagonal

            If idodiag = 1 Then

                ' YC 102418
                'diag(0, ixde(0, i)) = diag(0, ixde(0, i)) + s_1(0)
                'diag(1, ixde(0, i)) = diag(1, ixde(0, i)) + s_1(1)
                'diag(2, ixde(0, i)) = diag(2, ixde(0, i)) + s_1(2)
                'diag(3, ixde(0, i)) = diag(3, ixde(0, i)) + s_1(3)
                'diag(4, ixde(0, i)) = diag(4, ixde(0, i)) + s_1(4)
                'diag(5, ixde(0, i)) = diag(5, ixde(0, i)) + s_1(5)


                'diag(0, ixde(1, i)) = diag(0, ixde(1, i)) + s_1(9)
                'diag(1, ixde(1, i)) = diag(1, ixde(1, i)) + s_1(13)
                'diag(2, ixde(1, i)) = diag(2, ixde(1, i)) + s_1(14)
                'diag(3, ixde(1, i)) = diag(3, ixde(1, i)) + s_1(18)
                'diag(4, ixde(1, i)) = diag(4, ixde(1, i)) + s_1(19)
                'diag(5, ixde(1, i)) = diag(5, ixde(1, i)) + s_1(20)


                diag(1, ixde(1, i)) = diag(1, ixde(1, i)) + s_1(1)
                diag(2, ixde(1, i)) = diag(2, ixde(1, i)) + s_1(2)
                diag(3, ixde(1, i)) = diag(3, ixde(1, i)) + s_1(3)
                diag(4, ixde(1, i)) = diag(4, ixde(1, i)) + s_1(4)
                diag(5, ixde(1, i)) = diag(5, ixde(1, i)) + s_1(5)
                diag(6, ixde(1, i)) = diag(6, ixde(1, i)) + s_1(6)


                diag(1, ixde(2, i)) = diag(1, ixde(2, i)) + s_1(10)
                diag(2, ixde(2, i)) = diag(2, ixde(2, i)) + s_1(14)
                diag(3, ixde(2, i)) = diag(3, ixde(2, i)) + s_1(15)
                diag(4, ixde(2, i)) = diag(4, ixde(2, i)) + s_1(19)
                diag(5, ixde(2, i)) = diag(5, ixde(2, i)) + s_1(20)
                diag(6, ixde(2, i)) = diag(6, ixde(2, i)) + s_1(21)
                ' YC 102418 END

                'For k = 0 To 20     ' YC 102418 
                For k = 1 To 21
                    spring_stiff(k, i) = s_1(k)
                Next

            End If

            Call dispde(icnt2, fval, rl, rlm, s_1, lm, idir, lc, xmg, rf)

            'lmax = lm(0) ' YC 102418 
            lmax = lm(1)

            'For l = 1 To 5      ' YC 102418 
            For l = 2 To 6
                lmax = Math.Max(lmax, lm(l))
            Next

            If lmax > neql Then
                lstif = (iphase <> 3 And iref = 0)
                Continue For
            End If

            'For l = 0 To 5  ' YC 102418 
            For l = 1 To 6
                If (lm(l) <> 0) Then r(lm(l)) = r(lm(l)) - rl(l)
            Next

        Next

        idodiag = 0

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine spring(mtypde,cmde,ixde,sclf,dehv,nmelde,x,u,ui,idp,r,
'     &                  npc,p,icnt2,idir,lc,xmg,rf,diag)
'c
'
'      implicit double precision (a-h,o-z)                             
'
'c
'c===> module to compute residual and stiffness for discrete elements
'c
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/bk12/dtx0,dt1,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
'      common/bk20/kpri,nstep,ite,ilimit,iref
'
'      double precision length,nx,ny,nz
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'c      
'c NKC 8/31/99
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/fissl1/melemt,nnns,n2g,llls
'      common/elcnts/numelf
'      common/rocspr/lm(6),s(21)
'      logical lelstf,lstif
'      common/elstfb/lelstf
'      common/bkneq/neql
'      
'c NKC 9/14/99
'      common spring_stiff(21,10000),is_dofs(6,10000),is_nodes(2,10000)
'      common/nkcgar/idodiag
'
'      dimension mtypde(*),cmde(8,*),ixde(3,*),sclf(*),dehv(11,*),u(*),
'     &ui(*),idp(6,*),x(3,*),r(*),npc(*),p(*),rl(6),
'     &idir(*),lc(*),xmg(*),rf(*),xc(6),rlm(6),
'     &diag(21,*)
'
'c
'c NKC 8/9/99
'c      write(6,*) 'entering spring.F'
'    
'      zero = 0.0
'      one = 1.0
'      dti=1./(dt1+1.e-20)
'      numwds=0
'c      nwxrb=0
'      do 200 i=1,nmelde
'      mx=ixde(3,i)
'      mt=mtypde(mx)
'      node1=ixde(1,i)
'      node2=ixde(2,i)
'      call unpkid(lm(1),idp(1,node1),1)
'      call unpkid(lm(4),idp(1,node2),1)
'      xc(1)=x(1,ixde(1,i))
'      xc(2)=x(2,ixde(1,i))
'      xc(3)=x(3,ixde(1,i))
'      xc(4)=x(1,ixde(2,i))
'      xc(5)=x(2,ixde(2,i))
'      xc(6)=x(3,ixde(2,i))
'      is_nodes(1,i)=ixde(1,i)
'      is_nodes(2,i)=ixde(2,i)
'      ux1=0.0
'      uy1=0.0
'      uz1=0.0
'      ux2=0.0
'      uy2=0.0
'      uz2=0.0
'      vx1=0.0
'      vy1=0.0
'      vz1=0.0
'      vx2=0.0
'      vy2=0.0
'      vz2=0.0
'      ax1=0.0
'      ay1=0.0
'      az1=0.0
'      ax2=0.0
'      ay2=0.0
'      az2=0.0
'      do k=1, 6
'        is_dofs(k,i)=lm(k)
'      enddo
'
'      if (lm(1).ne.0) then
'        xc(1)=xc(1)+u(lm(1))
'        ux1=ui(lm(1))
'
'      endif
'      if (lm(2).ne.0) then
'        xc(2)=xc(2)+u(lm(2))
'        uy1=ui(lm(2))
'
'      endif
'      if (lm(3).ne.0) then
'        xc(3)=xc(3)+u(lm(3))
'        uz1=ui(lm(3))
'
'      endif
'      if (lm(4).ne.0) then
'        xc(4)=xc(4)+u(lm(4))
'        ux2=ui(lm(4))
'
'      endif
'      if (lm(5).ne.0) then
'        xc(5)=xc(5)+u(lm(5))
'        uy2=ui(lm(5))
'
'      endif
'      if (lm(6).ne.0) then
'        xc(6)=xc(6)+u(lm(6))
'        uz2=ui(lm(6))
'
'      endif
'      dx=xc(4)-xc(1)
'      dy=xc(5)-xc(2)
'      dz=xc(6)-xc(3)
'      ux=ux2-ux1
'      uy=uy2-uy1
'      uz=uz2-uz1
'      vx=vx2-vx1
'      vy=vy2-vy1
'      vz=vz2-vz1
'      ax=ax2-ax1
'      ay=ay2-ay1
'      az=az2-az1
'      length=sqrt(dx**2+dy**2+dz**2)
'      dehv(11,i)=length
'      du=length-dehv(7,i)
'      nx=dx/(length+1.e-20)
'      ny=dy/(length+1.e-20)
'      nz=dz/(length+1.e-20)
'      da=nx*ax+ny*ay+nz*az
'      dv=nx*vx+ny*vy+nz*vz
'      dui=nx*ux+ny*uy+nz*uz
'      go to (10,20,30,40,50,60),mt
'   10 force=dehv(10,i)+sclf(i)*cmde(1,mx)*du
'c NKC 8/30/99
'      if(cmde(2,mx) .gt. 0.5 .and. cmde(2,mx) .lt. 1.5) then
'        forcx=sclf(i)*cmde(1,mx)*ux
'        force=forcx
'        forcy=0.0
'        forcz=0.0
'        nx=1.0
'        ny=0.0
'        nz=0.0
'      endif
'      if(cmde(2,mx) .gt. 1.5 .and. cmde(2,mx) .lt. 2.5) then
'        forcx=0.0
'        forcy=sclf(i)*cmde(1,mx)*uy
'        force=forcy
'        forcz=0.0
'        nx=0.0
'        ny=1.0
'        nz=0.0
'      endif
'      if(cmde(2,mx) .gt. 2.5 .and. cmde(2,mx) .lt. 3.5) then
'        forcx=0.0
'        forcy=0.0
'        forcz=sclf(i)*cmde(1,mx)*uz
'        force=forcz
'        nx=0.0
'        ny=0.0
'        nz=1.0
'      endif
'      if(cmde(2,mx) .gt. -0.5 .and. cmde(2,mx) .lt. 0.5) then
'        forcx=nx*force
'        forcy=ny*force
'        forcz=nz*force
'      endif
'      dehv(10,i)=force
'      dehv(7,i)=length
'      stfnes=sclf(i)*cmde(1,mx)
'      go to 90
'   20 coeff=-sclf(i)*cmde(1,mx)
'      force=coeff*(a4*dv+a5*da)
'      if (iphase.gt.2) force=force-coeff*a1*dui
'      forcx=nx*force
'      forcy=ny*force
'      forcz=nz*force
'      dehv(10,i)=force
'      dehv(7,i)=length
'      stfnes=sclf(i)*cmde(1,mx)*a1
'      go to 90
'c
'c   ... bilinear plasticity ...
'   30 ftr=cmde(1,mx)*du+dehv(10,i)/sclf(i)
'      aksitr=abs(ftr-dehv(5,i))
'      fyn=cmde(3,mx)+dehv(4,i)*cmde(2,mx)
'c
'      if (aksitr.le.fyn) then
'        fcurs=ftr*sclf(i)
'        stfnes=cmde(1,mx)
'      else
'
'        sdu=sign(one,du)
'        aksin=abs(dehv(10,i)-dehv(5,i))
'        fyc=max(fyn,aksin)
'        dup=(aksitr-fyc)/cmde(1,mx)
'        df=(fyc-aksin+cmde(2,mx)*dup)*sdu
'        dehv(4,i)=dehv(4,i)+cmde(4,mx)*dup
'        dehv(5,i)=dehv(5,i)+(1.-cmde(4,mx))*cmde(2,mx)*dup*sdu
'        fcurs=dehv(10,i)+df*sclf(i)
'c       stfnes=cmde(2,mx)
'        stfnes=abs( ( abs(fcurs) - abs(dehv(10,i)/sclf(i)) )/du )
'      endif
'c
'      forcx=nx*fcurs
'      forcy=ny*fcurs
'      forcz=nz*fcurs
'      dehv(10,i)=fcurs
'      dehv(7,i)=length
'      go to 90
'c
'   40 ierr=0
'      xmag=1.0
'      n   =nint(cmde(1,mx))
'      loc =npc(n)
'      npoint=(npc(n+1)-loc)/2
'      force =0.0
'      du=length-dehv(1,i)
'      call itrpde(p(loc),du,npoint,force,dfdu,sclf(i))
'      forcx=nx*force
'      forcy=ny*force
'      forcz=nz*force
'      stfnes=dfdu
'      dehv(10,i)=force
'      dehv(7,i)=length
'      go to 90
'   50 ierr=0
'      xmag=1.0
'      n   =nint(cmde(1,mx))
'      loc =npc(n)
'      npoint=(npc(n+1)-loc)/2
'c     dldt  =dti*du/(.5*(length+dehv(7,i)))
'      dldt=du*dti
'      force =0.0
'      call itrpde(p(loc),dldt,npoint,force,dfdldt,sclf(i))
'      forcx=nx*force
'      forcy=ny*force
'      forcz=nz*force
'      dehv(10,i)=force
'      dehv(7,i)=length
'      go to 90
'c
'c     general nonlinear springs
'c
'   60 iflag=nint(dehv(1,i))
'      dyt  =dehv(2,i)
'      dyc  =dehv(3,i)
'      if (iflag.ne.0.or.du.gt.dyt.or.du.lt.dyc) go to 62
'      n=nint(cmde(1,mx))
'      loc=npc(n)
'      npoint=(npc(n+1)-loc)/2
'      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dehv(4,i),d2d1)
'      dehv(10,i)=force
'      stfnes = d2d1
'      go to 80
'   62 if (du.lt.dyt) go to 64
'
'c ...tension yielding
'      n=nint(cmde(1,mx))
'      loc=npc(n)
'      npoint=(npc(n+1)-loc)/2
'      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dehv(4,i),d2d1)
'      stfnes = d2d1
'      dehv(2,i)=du
'      dehv(8,i)=force
'      dehv(10,i)=force
'      dehv(1,i)=1.0
'      go to 80
'   64 if (du.gt.dyc) go to 66
'
'c ...compression yielding
'      n=nint(cmde(1,mx))
'      loc=npc(n)
'      npoint=(npc(n+1)-loc)/2
'      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dehv(5,i),d2d1)
'      stfnes = d2d1
'      dehv(3,i)=du
'      dehv(9,i)=force
'      dehv(10,i)=force
'      dehv(1,i)=3.0
'      go to 80
'   66 if (iflag.ne.2) go to 68
'
'c ...loading along elastic region of unloading curve
'      n=nint(cmde(2,mx))
'      loc=npc(n)
'      npoint=(npc(n+1)-loc)/2
'      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dehv(6,i),d2d1)
'      dehv(10,i)=force
'      stfnes = d2d1
'      go to 80
'   68 if (iflag.ne.1) go to 70
'
'c ...transition to unloading curve from tensile yielding
'      fyt=dehv(8,i)
'      n=nint(cmde(2,mx))
'      loc=npc(n)
'      npoint=(npc(n+1)-loc)/2
'      call itrpd2(p(loc),dl1,npoint,fyt,sclf(i),2,zero,d2d1)
'      dlu=dyt-dl1
'      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dlu,d2d1)
'      stfnes = d2d1
'      fy0=sclf(i)*(cmde(4,mx)-cmde(5,mx))
'      fyc=fyt-fy0-cmde(3,mx)*(-fy0+2.*fyt)
'      call itrpd2(p(loc),dyc,npoint,fyc,sclf(i),2,dlu,d2d1)
'      n=nint(cmde(1,mx))
'      loc=npc(n)
'      npoint=(npc(n+1)-loc)/2
'      call itrpd2(p(loc),dl1,npoint,fyc,sclf(i),2,zero,d2d1)
'      dehv(1,i)=2.
'      dehv(3,i)=dyc
'      dehv(5,i)=dyc-dl1
'      dehv(6,i)=dlu
'      dehv(9,i)=fyc
'      dehv(10,i)=force
'      go to 80
'
'c ...transition to unloading curve from compressive yielding
'   70 fyc=dehv(9,i)
'      n=nint(cmde(2,mx))
'      loc=npc(n)
'      npoint=(npc(n+1)-loc)/2
'      call itrpd2(p(loc),dl1,npoint,fyc,sclf(i),2,zero,d2d1)
'      dlu=dyc-dl1
'      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dlu,d2d1)
'      stfnes = d2d1
'      fy0=sclf(i)*(cmde(4,mx)-cmde(5,mx))
'      fyt=fyc+fy0-cmde(3,mx)*(fy0+2.*fyc)
'      call itrpd2(p(loc),dyt,npoint,fyt,sclf(i),2,dlu,d2d1)
'      n=nint(cmde(1,mx))
'      loc=npc(n)
'      npoint=(npc(n+1)-loc)/2
'      call itrpd2(p(loc),dl1,npoint,fyt,sclf(i),2,zero,d2d1)
'      dehv(1,i)=2.0
'      dehv(2,i)=dyt
'      dehv(4,i)=dyt-dl1
'      dehv(6,i)=dlu
'      dehv(8,i)=fyt
'      dehv(10,i)=force
'   80 forcx=nx*force
'      forcy=ny*force
'      forcz=nz*force
'   90 rl(1)=-forcx
'      rl(2)=-forcy
'      rl(3)=-forcz
'      rl(4)=forcx
'      rl(5)=forcy
'      rl(6)=forcz
'      do j=1,6
'       rlm(j)=0
'      enddo
'
'      s(1) =stfnes*nx*nx
'      s(2) =stfnes*nx*ny
'      s(3) =stfnes*ny*ny
'      s(4) =stfnes*nx*nz
'      s(5) =stfnes*ny*nz
'      s(6) =stfnes*nz*nz
'      s(7) =-s(1)
'      s(8) =-s(2)
'      s(9) =-s(4)
'      s(10)= s(1)
'      s(11)=-s(2)
'      s(12)=-s(3)
'      s(13)=-s(5)
'      s(14)= s(2)
'      s(15)= s(3)
'      s(16)=-s(4)
'      s(17)=-s(5)
'      s(18)=-s(6)
'      s(19)= s(4)
'      s(20)= s(5)
'      s(21)= s(6)
'c NKC 9/14/99, add terms to stiffness diagonal
'
'       if(idodiag .eq. 1) then
'c 
'        diag(1,ixde(1,i))=diag(1,ixde(1,i))+s(1)
'        diag(2,ixde(1,i))=diag(2,ixde(1,i))+s(2)
'        diag(3,ixde(1,i))=diag(3,ixde(1,i))+s(3)
'        diag(4,ixde(1,i))=diag(4,ixde(1,i))+s(4)
'        diag(5,ixde(1,i))=diag(5,ixde(1,i))+s(5)
'        diag(6,ixde(1,i))=diag(6,ixde(1,i))+s(6)
'
'
'        diag(1,ixde(2,i))=diag(1,ixde(2,i))+s(10)
'        diag(2,ixde(2,i))=diag(2,ixde(2,i))+s(14)
'        diag(3,ixde(2,i))=diag(3,ixde(2,i))+s(15)
'        diag(4,ixde(2,i))=diag(4,ixde(2,i))+s(19)
'        diag(5,ixde(2,i))=diag(5,ixde(2,i))+s(20)
'        diag(6,ixde(2,i))=diag(6,ixde(2,i))+s(21)
'
'        do k=1, 21
'          spring_stiff(k,i)=s(k)
'        enddo
'
'      endif
'
'      call dispde(icnt2,fval,rl,rlm,s,lm,idir,lc,xmg,rf)
'c
'      lmax=lm(1)
'      do 100 l=2,6
'      lmax=max(lmax,lm(l))
'  100 continue
'c
'      if (lmax.gt.neql) then
'        lstif=(iphase.ne.3 .and. iref.eq.0)
'
'        goto 200
'      endif
'c
'        do 110 l=1,6
'        if (lm(l).ne.0) r(lm(l))=r(lm(l))-rl(l)
'c       
'  110   continue
'c
'
'  200 continue
'c
'
'      idodiag=0
'      
'710   format(i6,a2,6(i10,a2))       
'810   format(2(i5,a2),f12.5)
'820   format(i6,a2,27(e12.5,a2))
'830   format(i6,a2,6(i10,a2),21(e12.5,a2))      
'      return
'      end

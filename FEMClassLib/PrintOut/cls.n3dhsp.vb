Partial Public Class clsPrintOut
    Sub n3dhsp(ByVal ifg As Integer, ByVal icase As Integer, ByVal path As String)

        Dim i, j, k, l, m As Integer

        'calculate number of nodes used in the hexahederon elements
        Dim nh0 As Integer = 0
        For kh = 1 To numnp
            For ih = 1 To numelh
                For jh = 1 To 8
                    If (ixh(jh, ih) = kh) Then GoTo 49 ' QW 12-12-2018-
                Next jh
            Next ih
            GoTo 51
49:         nh0 = nh0 + 1
51:
        Next kh
        Dim nh As Integer
        nh = nh0
        'calculate number of nodes used in the hexahederon elements END


        Dim cnl(numnp, nlcur * 3) As Double 'load at each node
        For i = 1 To nload
            cnl(nod(i), idirn(i) + (ncur(i) - 1) * 3) = fac(i)
        Next i

        Dim tnl(numnp) As Double ' temperature at each node (needs to consolidated to modMAIN ?)

        Dim ns1(nmelde), ns2(nmelde), ms(nmelde) As Integer 'discrete (spring) element
        For i = 1 To nmelde
            ns1(i) = ixde(1, i)
            ns2(i) = ixde(2, i)
            ms(i) = ixde(3, i)
        Next i


        Dim moe(numelh, 8), nem(numelh), npd(numelh), npe(numelh) As Integer 'element related

        Dim ls(numelh, 4, 2, numsv), ln(2, numsv) As Integer 'sliding lement related

        Dim fm(nmmtde), fs(nmelde), rs(nmelde, ntime), ds(nmelde) As Double ' discrete element related


        For i = 1 To numelh
            npd(i) = 0
            npe(i) = 0
            For j = 1 To 8
                moe(i, j) = ixe(j + 1, i)           ' QW 12-12-2018-
            Next j

        Next i
        'Call Check2D(ixe, 9, numelh)
        Dim ip, jp, kn, np As Integer
        ip = 1
        jp = 1
92:     npd(jp) = ip
        npe(jp) = 2
        i = 2
93:     For j = 1 To numelh
            If (npe(j) = i) Then
                For k = 1 To 8
                    For l = 1 To numelh
                        For m = 1 To 8
                            If ((moe(j, k) = moe(l, m)) And (j <> l) And (npe(l) <> 1)) Then
                                kn = i + 1
                                npe(l) = kn
                                npd(l) = ip
                                GoTo 94
                            End If
                        Next m
94:
                    Next l
                Next k
                npe(j) = 1
            End If
        Next j

        If (i = kn) Then GoTo 95
        i = kn
        GoTo 93
95:     For i = 1 To numelh
            If (npd(i) = 0) Then
                jp = i
                ip = ip + 1
                GoTo 92
            End If
        Next i

        '	calculate the number of parts
        np = ip
        Dim mee(np), ie As Integer
        For ip = 1 To np
            ie = 0
            For i = 1 To numelh
                If (npd(i) = ip) Then ie = ie + 1
            Next i
            mee(ip) = ie
        Next ip

        '	creat data file for Tecplot
        Call tecplot(numnp, numelh, cnl, tnl, nob, moe, nlcur, np, mee, nmelde, ns1, ns2, ms, path)

        'Call tecstress(ccc, nnd, nne, nf, nob, x, y, z, snl, moe, st, ln, ls, ifg, nnt, nem, npd, np, Me, nh, ns, ns1, ns2, ms, fs, ds, rs)
        Call tecstress(numnp, numelh, moe, ntime, np, mee, path)

    End Sub
End Class




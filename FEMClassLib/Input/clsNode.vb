
Partial Public Class clsInput

    Sub Node()
        Dim i, k, kk, kn, kn0, num, numn, iflag, jflag, bcd, bcs As Integer
        Dim xmin, xmax, ymin, ymax, zmin, zmax, xnum, dx, dy, dz As Double
        xmin = 1.0E+20 : xmax = -1.0E+20 : ymin = 1.0E+20 : ymax = -1.0E+20 : zmin = 1.0E+20 : zmax = -1.0E+20
        Dim nold As Integer = 0
        Dim idp(,) As Integer = clsCom.idp
        Dim nob() As Integer = clsCom.nob
        Dim x(,) As Double = clsCom.x
        'ReDim idp(6, clsCom.numnp), x(3, clsCom.numnp), nob(clsCom.numnp)
        For J = 1 To clsCom.numnp
            kn = kn0
            Dim bcc As Integer = nob(J)
            Dim bcr As Integer = 7
            If xmin > x(1, J) Then xmin = x(1, J)
            If xmax < x(1, J) Then xmax = x(1, J)
            If ymin > x(2, J) Then ymin = x(2, J)
            If ymax < x(2, J) Then ymax = x(2, J)
            If zmin > x(3, J) Then zmin = x(3, J)
            If zmax < x(3, J) Then zmax = x(3, J)

            If bcc >= 0 Then iflag = 0
            If bcc < 0 Then iflag = 1
            If bcc <> bcd Then iflag = 1
            If bcc < 0 Then bcc = -bcc
            If bcr >= 0 Then jflag = 0
            If bcr < 0 Then jflag = 1
            If bcr <> bcs Then jflag = 1
            If bcr < 0 Then bcr = -bcr

            If (bcc = 1) Then idp(1, J) = 1
            If (bcc = 2) Then idp(2, J) = 1
            If (bcc = 3) Then idp(3, J) = 1
            If (bcc = 4) Then idp(1, J) = 1
            If (bcc = 4) Then idp(2, J) = 1
            If (bcc = 5) Then idp(2, J) = 1
            If (bcc = 5) Then idp(3, J) = 1
            If (bcc = 6) Then idp(1, J) = 1
            If (bcc = 6) Then idp(3, J) = 1
            If (bcc = 7) Then idp(1, J) = 1
            If (bcc = 7) Then idp(2, J) = 1
            If (bcc = 7) Then idp(3, J) = 1
            idp(4, J) = 1
            idp(5, J) = 1
            idp(6, J) = 1

            If kn = 0 Then kn = 1
            If nold = 0 Then GoTo 50
            num = (J - nold) / kn
            numn = num - 1
            If numn < 1 Then GoTo 50
            xnum = num
            dx = (x(1, J) - x(1, nold)) / xnum
            dy = (x(2, J) - x(2, nold)) / xnum
            dz = (x(3, J) - x(3, nold)) / xnum
            k = nold
            For i = 1 To numn
                kk = k
                k = k + kn
                x(1, k) = x(1, kk) + dx
                x(2, k) = x(2, kk) + dy
                x(3, k) = x(3, kk) + dz
                If iflag <> 1 Then GoTo 27
                idp(1, k) = 0
                idp(2, k) = 0
                idp(3, k) = 0
27:             If jflag <> 1 Then GoTo 29
                idp(4, k) = 0
                idp(5, k) = 0
                idp(6, k) = 0
29:         Next i

50:         nold = J
            bcd = bcc
            bcs = bcr
        Next J
        clsCom.x = x
        clsCom.idp = idp
    End Sub

End Class

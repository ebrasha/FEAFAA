
Partial Public Class clsInput

    Sub slavin()
        ' Combine islavin and slavin
        Dim i, j, js, jm, nrts, nrtm, nsn, nmn, nty, nn As Integer
        ReDim nsf(2, numsv)
        js = 0 : jm = 0
        ifl = 0 : nsntl = 0 : nmntl = 0
        For i = 1 To clsCom.numsv
            nrts = iparm(1, i)
            nrtm = iparm(2, i)
            nsn = iparm(3, i)
            nmn = iparm(4, i)
            nty = iparm(5, i)


            ' v3.0 003 YC
            Dim irects2(4, nrts) As Integer
            'Call readsg(irects2, nrts, readTextAll, L)
            For j = 1 To nrts
                For k = 1 To 4
                    irects2(k, j) = irects(k, js + j)
                Next
            Next
            'If (nrtm <> 0) Then
            Dim irectm2(4, nrtm) As Integer
            'Call readsg(irectm2, nrtm, readTextAll, L)
            For j = 1 To nrtm
                For k = 1 To 4
                    irectm2(k, j) = irectm(k, jm + j)
                Next
            Next
            ' v3.0 003 YC END


            nn = 4 * Math.Max(nrts, nrtm)
            Dim mnn(nn) As Integer

            'Call countn(irects, mnn, nsn, nrts)    ' v3.0 003 YC
            Call countn(irects2, mnn, nsn, nrts)

            For j = 1 To nn
                mnn(j) = 0
            Next j

            'Call countn(irectm, mnn, nmn, nrtm) ' v3.0 003 YC
            Call countn(irectm2, mnn, nmn, nrtm)

            iparm(3, i) = nsn
            iparm(4, i) = nmn
            nsf(1, i) = nsn
            nsf(2, i) = nmn

            If (fric(1, i) ^ 2 + fric(2, i) ^ 2 + fric(3, i) ^ 2) <> 0 Then
                ifl = ifl + nsn + nmn
            End If

            If ifd(i) <> 0 Then nifd = nifd + nsn + nmn
            nsmmax = Math.Max(nsn, nmn)
            nsntl = nsntl + nsn
            nmntl = nmntl + nmn
            js = js + nrts
            jm = jm + nrtm
        Next i
        'Call Check2D(irects, 4, nrttls)
    End Sub
   
End Class

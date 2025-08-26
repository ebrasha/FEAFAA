'This file contains all the methods of blkmax.f
Partial Public Class clsInput

    ''' <summary>
    ''' To compute bulk modulus Of Each material and accumulate the largest bulk modulus in mesh
    ''' (Array starts from 0 index, so while calling this check for proper mt value)
    ''' </summary>
    ''' <param name="matype"></param>
    ''' <param name="mt">Array starts from 0 index, so use proper mt value</param>
    ''' <param name="prop"></param>
    ''' <param name="bulkmx"></param>
    ''' <param name="ym"></param>
    ''' <param name="pr"></param>
    Public Sub blkmax(ByRef matype As Integer(), mt As Integer, ByRef prop2 As Double(,),
                      ByRef bulkmx As Double, ByRef ym As Double, ByRef pr As Double)
        Dim prop(48 * clsCom.nmmat) As Double
        Dim lc = 48 * (mt - 1) + 1
        Dim mtype = matype(mt)
        For I = 1 To 48
            prop(lc + I - 1) = prop2(I, mt)
        Next I
        '   NKC
        If mtype = 1 OrElse mtype = 56 OrElse mtype = 57 OrElse mtype = 58 Then
            ym = prop(lc)
            pr = prop(lc + 1)
        ElseIf mtype = 2 Then
            ym = prop(lc + 10)
            pr = prop(lc + 11)
        ElseIf mtype = 3 Then
            ym = prop(lc)
            pr = prop(lc + 1)
        ElseIf mtype = 4 Then
            ym = prop(lc + 8)
            pr = prop(lc + 16)
        ElseIf mtype = 5 Then
            ym = 3 * prop(lc + 1)
            pr = 0.0
        ElseIf mtype = 6 Then
            ym = 3 * prop(lc)
            pr = 0.0
        ElseIf mtype = 7 Then
            ym = prop(lc + 10)
            pr = prop(lc + 11)
        ElseIf mtype = 8 Then
            ym = 3 * prop(lc + 16)
            pr = 0.0
        ElseIf mtype = 9 Then
            ym = prop(lc)
            pr = prop(lc + 1)
        ElseIf mtype = 10 Then
            ym = prop(lc + 8)
            pr = prop(lc + 16)
        ElseIf mtype = 11 Then
            ym = prop(lc + 16)
            pr = prop(lc)
        ElseIf mtype = 12 Then
            Dim xk = prop(lc + 32)
            Dim gg = prop(lc + 8) / prop(lc)
            ym = 9 * gg * xk / (3 * xk + gg)
            pr = (3 * xk - 2 * gg) / (6 * xk + 2 * gg)
        ElseIf mtype = 13 Then
            ym = prop(lc + 10)
            pr = prop(lc + 11)
        ElseIf mtype = 14 Then
            ym = prop(lc)
            pr = prop(lc + 8)
        ElseIf mtype = 15 Then
            Dim gg = 2 * (prop(lc) + prop(lc + 8))
            pr = prop(lc + 16)
            ym = 2 * gg * (1 + pr)
        ElseIf mtype = 16 Then
            ym = prop(lc + 8)
            pr = prop(lc + 16)
        ElseIf mtype = 17 Then
            ym = 1.0
            pr = 0.3
        ElseIf mtype = 18 Then
            Dim gg = 2 * (prop(lc) + prop(lc + 1))
            Dim xk = prop(lc + 8)
            ym = 9 * gg * xk / (3 * xk + gg)
            pr = (3 * xk - 2 * gg) / (6 * xk + 2 * gg)
        ElseIf mtype = 19 Then
            ym = prop(lc)
            pr = prop(lc + 1)
        ElseIf mtype = 20 Then
            ym = prop(lc)
            pr = prop(lc + 8)
        ElseIf mtype = 23 Then
            ym = 3 * prop(lc + 20)
            pr = 0.0
        ElseIf mtype = 35 Then
            ym = prop(lc)
            pr = prop(lc + 8)
        End If

        Dim blkm As Double = 0
        If pr = 0.5 Then
            blkm = ym * 10000000000.0 '1E10
        Else
            blkm = ym / (3 * (1 - 2 * pr))
        End If

        bulkmx = Math.Max(blkm, bulkmx)
    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine blkmax(matype, mt, prop, bulkmx, ym, pr)
'c

'implicit Double precision (a-h, o - z)                                    dp

'c
'c ==>= module to compute bulk modulus of each material
'c     And accumulate the largest bulk modulus in mesh
'c
'      dimension matype(*), prop(*)
'c

'!ik099 1111111111
'      Integer ik01
'      If (ik01.eq.0) Then
'write(88,*) 'blkmax'
'ik01 = ik01 + 1
'End If

'lc = 48 * (mt - 1) + 1
'mtype = matype(mt)
'c
'c NKC
'If (mtype.eq.1 .Or. mtype.eq.56 .Or.
'     &    mtype.eq.57 .Or. mtype.eq.58) then
'         ym = prop(lc)
'pr = prop(lc + 1)
'c
'ElseIf (mtype.eq.2) Then
'ym = prop(lc + 10)
'pr = prop(lc + 11)
'c
'ElseIf (mtype.eq.3) Then
'ym = prop(lc)
'pr = prop(lc + 1)
'c
'ElseIf (mtype.eq.4) Then
'ym = prop(lc + 8)
'pr = prop(lc + 16)
'c
'ElseIf (mtype.eq.5) Then
'ym = 3.* prop(lc + 1)
'pr = 0.0
'c
'ElseIf (mtype.eq.6) Then
'ym = 3.* prop(lc)
'pr = 0.0
'c
'ElseIf (mtype.eq.7) Then
'ym = prop(lc + 10)
'pr = prop(lc + 11)
'c
'ElseIf (mtype.eq.8) Then
'ym = 3.* prop(lc + 16)
'pr = 0.0
'c
'ElseIf (mtype.eq.9) Then
'ym = prop(lc)
'pr = prop(lc + 1)
'c
'ElseIf (mtype.eq.10) Then
'ym = prop(lc + 8)
'pr = prop(lc + 16)
'c
'ElseIf (mtype.eq.11) Then
'ym = prop(lc + 16)
'pr = prop(lc)
'c
'ElseIf (mtype.eq.12) Then
'xk = prop(lc + 32)
'gg = prop(lc + 8) / prop(lc)
'ym = 9.* gg * xk / (3.* xk + gg)
'pr = (3.* xk - 2.* gg) / (6.* xk + 2.* gg)
'c
'ElseIf (mtype.eq.13) Then
'ym = prop(lc + 10)
'pr = prop(lc + 11)
'c
'ElseIf (mtype.eq.14) Then
'ym = prop(lc)
'pr = prop(lc + 8)
'c
'ElseIf (mtype.eq.15) Then
'gg = 2.* (prop(lc) + prop(lc + 8))
'pr = prop(lc + 16)
'ym = 2.* gg * (1.+ pr)
'c
'ElseIf (mtype.eq.16) Then
'ym = prop(lc + 8)
'pr = prop(lc + 16)
'c
'ElseIf (mtype.eq.17) Then
'ym = 1.0
'pr = 0.3
'c
'ElseIf (mtype.eq.18) Then
'gg = 2.* (prop(lc) + prop(lc + 1))
'xk = prop(lc + 8)
'ym = 9.* gg * xk / (3.* xk + gg)
'pr = (3.* xk - 2.* gg) / (6.* xk + 2.* gg)
'c
'ElseIf (mtype.eq.19) Then
'ym = prop(lc)
'pr = prop(lc + 1)
'c
'ElseIf (mtype.eq.20) Then
'ym = prop(lc)
'pr = prop(lc + 8)
'c
'ElseIf (mtype.eq.23) Then
'ym = 3.* prop(lc + 20)
'pr = 0.0
'c
'ElseIf (mtype.eq.35) Then
'ym = prop(lc)
'pr = prop(lc + 8)
'c
'End If
'If (pr.eq.0.5) Then
'blkm = ym * 1.e10
'Else
'blkm = ym / (3.* (1.- 2.* pr))
'End If
'bulkmx = max(blkm, bulkmx)
'Return
'End

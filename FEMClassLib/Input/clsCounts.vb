'This file contains all the methods of counts.f
Partial Public Class clsInput

    'Public Sub counts(ByRef irect As Integer(), ByRef mnn As Integer(), ByRef nseg As Integer(), nrt As Integer, nn As Integer)
    'This overload method takes 1D array and call actual sub with 2D array
    'Dim irect_n(4, nrt) As Integer
    ' For ind1 = 0 To 3
    '    Try
    '       irect_n(ind1, 0) = irects(ind1)
    '  Catch ex As Exception
    '     irect_n(ind1, 0) = 0
    'End Try
    'Next
    ' counts(irect_n, mnn, nseg, nrt, nn)
    ' End Sub


    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="irect"></param>
    ''' <param name="mnn"></param>
    ''' <param name="nseg"></param>
    ''' <param name="nrt"></param>
    ''' <param name="nn"></param>
    Public Sub counts(ByRef irect As Integer(,), ByRef mnn As Integer(), ByRef nseg As Integer(), nrt As Integer, nn As Integer)
        Dim ind1, ind2, ind3 As Integer
        If (nrt = 0) Then Return
        For ind1 = 1 To nrt
            For ind2 = 1 To 4
                If irect(ind2, ind1) = 0 Then Continue For
                For ind3 = 1 To nn
                    Dim il = ind3
                    If mnn(ind3) = irect(ind2, ind1) Then
                        nseg(il) = nseg(il) + 1
                    End If
                Next ind3
            Next ind2
        Next ind1
    End Sub
End Class


'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine counts(irect, mnn, nseg, nrt, nn)
'c

'implicit Double precision (a-h, o - z)                                    dp

'c
'c ==>= module to
'c
'      dimension irect(4,*), mnn(*), nseg(*)
'c

'!ik099 1111111111
'      Integer ik01
'      If (ik01.eq.0) Then
'write(88,*) 'counts'
'ik01 = ik01 + 1
'End If

'If (nrt.eq.0) return
'      Do 70 i=1,nrt
'      Do 60 j=1,4
'      If (irect(j, i).eq.0) go to 60
'      Do 30 k=1,nn
'      il = k
'30 if(mnn(k).eq.irect(j,i)) go to 40
'   40 nseg(il)=nseg(il)+1
'   60 continue
'   70 continue
'c
'Return
'End

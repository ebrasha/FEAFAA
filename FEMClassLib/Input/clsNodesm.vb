'This file contains all the methods of nodesm.f
Partial Public Class clsInput

    ''' <summary>
    ''' module to
    ''' </summary>
    ''' <param name="nsv"></param>
    ''' <param name="msr"></param>
    ''' <param name="irects"></param>
    ''' <param name="irectm"></param>
    ''' <param name="iparm"></param>
    Public Sub nodesm(ByRef nsv As Integer(), ByRef msr As Integer(), ByRef irects As Integer(,),
                      ByRef irectm As Integer(,), ByRef iparm(,) As Integer, ByVal ind As Integer)
        Dim i, j, nrts, nrtm, nsn, nmn, nty, k, jnrts, jnrtm, jnsn, jnmn, nmn1, imn, ip1 As Integer
        nrts = iparm(1, ind)
        nrtm = iparm(2, ind)
        nsn = iparm(3, ind)
        nmn = iparm(4, ind)
        nty = iparm(5, ind)
        jnrts = 0 : jnrtm = 0 : jnsn = 0 : jnmn = 0
        If ind > 1 Then

            ' YC 121219-4
            'jnrts = iparm(1, ind - 1) : jnrtm = iparm(2, ind - 1)
            'jnsn = iparm(3, ind - 1) : jnmn = iparm(4, ind - 1)
            For i = 1 To ind - 1
                jnrts = jnrts + iparm(1, i)
                jnrtm = jnrtm + iparm(2, i)
                jnsn = jnsn + iparm(3, i)
                jnmn = jnmn + iparm(4, i)
            Next
            ' YC 121219-4 END


        End If
        Dim irecs(4, nrts), irecm(4, nrtm) As Integer
        For j = 1 To nrts
            For k = 1 To 4
                irecs(k, j) = irects(k, jnrts + j)
            Next
        Next
        For j = 1 To nrtm
            For k = 1 To 4
                irecm(k, j) = irectm(k, jnrtm + j)
            Next
        Next
        Dim nsvs(nsn), msrm(nmn) As Integer
        countn(irecs, nsvs, k, nrts)
        countn(irecm, msrm, k, nrtm)
        For j = 1 To nsn
            nsv(jnsn + j) = nsvs(j)
        Next
        For j = 1 To nmn
            msr(jnmn + j) = msrm(j)
        Next

        If nty <> 2 AndAlso nty <> 7 Then Return ' check for tied s.s.
        nmn1 = nmn - 1
        For i = 1 To nmn1
            imn = msr(i)
            ip1 = i + 1
            For j = ip1 To nmn
                If imn < msr(j) Then Continue For
                msr(i) = msr(j)
                msr(j) = imn
                imn = msr(i)
            Next
        Next
    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine nodesm(nsv, msr, irects, irectm, iparm)
'c

'implicit Double precision (a-h, o - z)                                    dp

'c
'c ==>= module to
'c

'common/ bk37 / numsv, nrtm, nrts, nmn, nsn, nty, nst, mst, noco, n
'      dimension nsv(*), msr(*), irects(4,*), irectm(4,*), iparm(*)
'c

'!ik099 1111111111
'      Integer ik01
'      If (ik01.eq.0) Then
'write(88,*) 'nodesm'
'ik01 = ik01 + 1
'End If

'nrts = iparm(1)
'nrtm = iparm(2)
'nsn = iparm(3)
'nmn = iparm(4)
'nty = iparm(5)
'c
'Call countn(irects, nsv, k, nrts)
'Call countn(irectm, msr, k, nrtm)
'If (nty.ne.2.And.nty.ne.7) return    ! check for tied s.s.
'      nmn1 = nmn - 1
'Do 20 i=1,nmn1
'      imn = msr(i)
'ip1 = i + 1
'Do 10 j=ip1,nmn
'      If (imn.lt.msr(j)) go To 10
'      msr(i) = msr(j)
'msr(j) = imn
'imn = msr(i)
'10 continue
'   20 continue
'c
'Return
'End

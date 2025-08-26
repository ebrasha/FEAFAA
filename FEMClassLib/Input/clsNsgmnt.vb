'This file contains all the methods of nsgmnt.f

Partial Public Class clsInput

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="iparm"></param>
    ''' <param name="irects"></param>
    ''' <param name="irectm"></param>
    ''' <param name="nsv"></param>
    ''' <param name="msr"></param>
    ''' <param name="nsegs"></param>
    ''' <param name="nsegm"></param>
    ''' <param name="nstl"></param>
    ''' <param name="nmtl"></param>
    Public Sub nsgmnt(ByRef iparm(,) As Integer, ByRef irects(,) As Integer, ByRef irectm(,) As Integer,
                    ByRef nsv() As Integer, ByRef msr() As Integer, ByRef nsegs() As Integer,
                    ByRef nsegm() As Integer, ind As Integer)

        Dim nrts, nrtm, nsn, nmn, nst, mst, i, j, k, jnrts, jnrtm, jnsn, jnmn As Integer
        nrts = iparm(1, ind)
        nrtm = iparm(2, ind)
        nsn = iparm(3, ind)
        nmn = iparm(4, ind)
        nst = 0
        mst = 0
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



        Dim irecs(4, nrts), irecm(4, nrtm), nsvs(nsn), msrm(nmn), nses(nsn + 1), nsem(nmn + 1) As Integer
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
        For j = 1 To nsn
            nsvs(j) = nsv(jnsn + j)
        Next
        For j = 1 To nmn
            msrm(j) = msr(jnmn + j)
        Next

        For i = 1 To nsn
            nses(i + 1) = 0
        Next
        For i = 1 To nmn
            nsem(i + 1) = 0
        Next

        nses(1) = 1
        nsem(1) = 1
        Dim nseg() As Integer
        ReDim nseg(nses.Length - 2)
        '.Copy(nses, 2, nseg, 1, nseg.Length)
        Call counts(irecs, nsvs, nseg, nrts, nsn)
        Array.Copy(nseg, 1, nses, 2, nseg.Length - 1)
        ReDim nseg(nsem.Length - 2)
        'Array.Copy(nsem, 2, nseg, 1, nseg.Length)
        Call counts(irecm, msrm, nseg, nrtm, nmn)
        Array.Copy(nseg, 1, nsem, 2, nseg.Length - 1)

        For i = 1 To nsn
            nst = nst + nses(i + 1)
            nstl = nstl + nses(i + 1)
            nses(i + 1) = nses(i) + nses(i + 1)
        Next

        For i = 1 To nmn
            mst = mst + nsem(i + 1)
            nmtl = nmtl + nsem(i + 1)
            nsem(i + 1) = nsem(i) + nsem(i + 1)
        Next
        If ind > 1 Then

            'jnsn = jnsn + 1 : jnmn = jnmn + 1   ' YC 121219-4
            jnsn = jnsn + ind - 1 : jnmn = jnmn + ind - 1

        End If
        For j = 1 To nsn + 1
            nsegs(jnsn + j) = nses(j)
        Next
        For j = 1 To nmn + 1
            nsegm(jnmn + j) = nsem(j)
        Next
        iparm(6, ind) = nst
        iparm(7, ind) = mst

    End Sub
End Class

'  ref org fortran code

'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine nsgmnt(iparm,irects,irectm,nsv,msr,nsegs,nsegm,nstl,
'     1                    nmtl)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to
'c
'      
'      common/bk37/numsv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
'      dimension iparm(*),irects(*),irectm(*),nsv(*),msr(*),nsegs(*),
'     1          nsegm(*)
'c
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'nsgmnt'
'      ik01 = ik01 + 1
'      end if
'
'      nrts=iparm(1)
'      nrtm=iparm(2)
'      nsn =iparm(3)
'      nmn =iparm(4)
'      nst =0
'      mst =0
'c
'      do 10 i=1,nsn
'   10 nsegs(i+1)=0
'      do 20 i=1,nmn
'   20 nsegm(i+1)=0
'      nsegs(1)=1
'      nsegm(1)=1
'c
'      call counts(irects,nsv,nsegs(2),nrts,nsn)
'      call counts(irectm,msr,nsegm(2),nrtm,nmn)
'c
'      do 30 i=1,nsn
'      nst = nst+nsegs(i+1)
'      nstl=nstl+nsegs(i+1)
'   30 nsegs(i+1)=nsegs(i)+nsegs(i+1)
'      do 40 i=1,nmn
'      mst = mst+nsegm(i+1)
'      nmtl=nmtl+nsegm(i+1)
'   40 nsegm(i+1)=nsegm(i)+nsegm(i+1)
'c
'      iparm(6)=nst
'      iparm(7)=mst
'c
'      return
'      end
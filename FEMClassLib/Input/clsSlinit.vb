'This file contains all the methods of slinit.f
Partial Public Class clsInput
    ''' <summary>
    ''' drive slide surface initialization
    ''' </summary>
    ''' <param name="iparm"></param>
    ''' <param name="irects"></param>
    ''' <param name="irectm"></param>
    ''' <param name="nsv"></param>
    ''' <param name="msr"></param>
    ''' <param name="nsegs"></param>
    ''' <param name="nsegm"></param>
    ''' <param name="lnsv"></param>
    ''' <param name="lmsr"></param>
    ''' <param name="ilocs"></param>
    ''' <param name="ilocm"></param>
    ''' <param name="stfs"></param>
    ''' <param name="stfm"></param>
    ''' <param name="irtls"></param>
    ''' <param name="irtlm"></param>
    ''' <param name="mtype"></param>
    ''' <param name="x"></param>
    ''' <param name="bh"></param>
    ''' <param name="cst"></param>
    ''' <param name="rdc"></param>
    ''' <param name="pend"></param>
    Public Sub slinit(ByRef iparm(,) As Integer, ByRef irects(,) As Integer, ByRef irectm(,) As Integer, ByRef nsv() As Integer,
                      ByRef msr() As Integer, ByRef nsegs() As Integer, ByRef nsegm() As Integer, ByRef lnsv() As Integer,
                      ByRef lmsr() As Integer, ByRef ilocs() As Integer, ByRef ilocm() As Integer,
                      ByRef stfs() As Double, ByRef stfm() As Double, ByRef irtls() As Integer, ByRef irtlm() As Integer,
                      ByRef mtype() As Integer, ByRef xx1() As Double, ByRef xx2() As Double, ByRef xx3() As Double,
                      ByRef x(,) As Double, ByRef ibh(,) As Integer, ByRef cst(,) As Double,
                      ByRef rdc(,) As Double, ByRef pend() As Double, ByVal det As Double, ByRef emodl2 As Double,
                      ByRef ixx() As Integer, ByRef xs As Double, ByRef ys As Double, ByRef zs As Double, ByRef amx As Double, ByRef amy As Double, ByRef amz As Double,
                      ByRef vn() As Double, ByRef llc As Integer, ByRef lresrt As Integer, ByRef resltl As Double, ByRef result As Double,
                      ByRef nrtm As Integer, ByRef nrts As Integer, ByRef nmn As Integer, ByRef nsn As Integer, ByRef nty As Integer, ByRef nst As Integer, ByRef mst As Integer, ByRef hh() As Double,
                      ByRef xx112 As Double, ByRef xx134 As Double, ByRef xx212 As Double, ByRef xx234 As Double, ByRef xx312 As Double, ByRef xx334 As Double, ByRef xx112p As Double, ByRef xx134p As Double, ByRef xx212p As Double, ByRef xx234p As Double, ByRef xx312p As Double, ByRef xx334p As Double, ByRef xx114 As Double, ByRef xx123 As Double, ByRef xx214 As Double, ByRef xx223 As Double, ByRef xx314 As Double, xx323 As Double, ByRef x1423 As Double, ByRef y1423 As Double, ByRef z1423 As Double, ByRef x1423p As Double, ByRef y1423p As Double, ByRef z1423p As Double)

        Dim k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, I, J As Integer ' QW 8-2018
        Dim irect(,) As Integer
        Dim sb = New Text.StringBuilder()
        sb.AppendLine(" SLIDE SURFACE INITIALIZATION data")
        sb.AppendLine(Space(1) + New String("=", 60))
        sb.AppendLine("    interface    side    max. relocation distance   node")

        Dim msg = sb.ToString()
        Dim ipass As Integer
        Dim ioff As Boolean
        Dim dxs, bulks, stfss, dxm, bulkm, stfsm, stfmin, stfmax As Double
        k1 = 1 : k2 = 1 : k3 = 1 : k4 = 1 : k5 = 1 : k6 = 1 : k7 = 1 : k8 = 1 : k9 = 1 : k10 = 1 : k11 = 1
        'If bNIKE3D Then
        'Console.WriteLine(msg)
        'End If

        For n = 1 To numsv
            nrts = iparm(1, n)
            nrtm = iparm(2, n)
            nsn = iparm(3, n)
            nmn = iparm(4, n)
            nty = iparm(5, n)
            nst = iparm(6, n)
            mst = iparm(7, n)

            ' ipass = 1 * sign(1, nty) 'check For Single pass
            ipass = 1 * Math.Sign(nty) ' QW 8-2018
            nty = Math.Abs(nty)

            If nty = 8 Then             'type 3 with shell thickness
                nty = 3
            End If

            ioff = False
            If nty = 7 Then
                nty = 2
                iparm(5, n) = 2
                ioff = True
            End If

            If nty = 10 Then  'dont do facet node reordering
                nty = 3
                iparm(5, n) = ipass * 3
            End If

            ReDim irect(4, nrttls)
            For I = 1 To 4
                For J = 1 To nrttls - k1 + 1
                    irect(I, J) = irects(I, k1 + J - 1)
                Next
            Next
            Dim nseg(), lc(), mns(), mms(), iloc() As Integer
            ReDim nseg(nsegs.Length - k2), lc(lnsv.Length - k3), mns(nsv.Length - k4), mms(msr.Length - k5), iloc(ilocs.Length - k4)
            Array.Copy(nsegs, k2, nseg, 1, nsegs.Length - k2)
            Array.Copy(lnsv, k3, lc, 1, lnsv.Length - k3)
            Array.Copy(nsv, k4, mns, 1, nsv.Length - k4)
            Array.Copy(msr, k5, mms, 1, msr.Length - k5)
            Array.Copy(ilocs, k4, iloc, 1, ilocs.Length - k4)
            Call slavi0(x, irect, nseg, lc, mns, mms, iloc, nmn, nsn, nrts, n)
            'Call slavi0(x, irect, nsegs.Skip(k2).ToArray(), lnsv.Skip(k3).ToArray(), nsv.Skip(k4).ToArray(), msr.Skip(k5).ToArray(), ilocs.Skip(k4).ToArray(), nmn, nsn, nrts, n)
            Array.Copy(nseg, 1, nsegs, k2, nsegs.Length - k2)
            Array.Copy(lc, 1, lnsv, k3, lnsv.Length - k3)
            Array.Copy(mns, 1, nsv, k4, nsv.Length - k4)
            Array.Copy(mms, 1, msr, k5, msr.Length - k5)
            Array.Copy(iloc, 1, ilocs, k4, ilocs.Length - k4)
            ReDim irect(4, nrttlm)
            For I = 1 To 4
                For J = 1 To nrttlm - k6 + 1
                    irect(I, J) = irectm(I, k6 + J - 1)
                Next
            Next
            ReDim nseg(nsegm.Length - k7), lc(lmsr.Length - k8), mns(nsv.Length - k4), mms(msr.Length - k5), iloc(ilocm.Length - k5)
            Array.Copy(nsegm, k7, nseg, 1, nsegm.Length - k7)
            Array.Copy(lmsr, k8, lc, 1, lmsr.Length - k8)
            Array.Copy(nsv, k4, mns, 1, nsv.Length - k4)
            Array.Copy(msr, k5, mms, 1, msr.Length - k5)
            Array.Copy(ilocm, k5, iloc, 1, ilocm.Length - k5)
            Call slavi0(x, irect, nseg, lc, mms, mns, iloc, nsn, nmn, nrtm, n)
            'Call slavi0(x, irect, nsegm.Skip(k7).ToArray(), lmsr.Skip(k8).ToArray(), msr.Skip(k5).ToArray(), nsv.Skip(k4).ToArray(), ilocm.Skip(k5).ToArray(), nsn, nmn, nrtm, n)
            Array.Copy(nseg, 1, nsegm, k7, nsegm.Length - k7)
            Array.Copy(lc, 1, lmsr, k8, lmsr.Length - k8)
            Array.Copy(mns, 1, nsv, k4, nsv.Length - k4)
            Array.Copy(mms, 1, msr, k5, msr.Length - k5)
            Array.Copy(iloc, 1, ilocm, k5, ilocm.Length - k5)

            Dim stf() As Double
            ReDim irect(4, nrttls)
            For I = 1 To 4
                For J = 1 To nrttls - k1 + 1
                    irect(I, J) = irects(I, k1 + J - 1)
                Next
            Next
            ReDim stf(stfs.Length - k9)
            Array.Copy(stfs, k9, stf, 1, stfs.Length - k9)
            Call stiffs(x, irect, stf, ibh, mtype, numelh, nrts, stfsf(n), dxs, bulks, stfss, nty, nsn, rdc, n, xx1, xx2, xx3, det, emodl2)
            'Call stiffs(x, irect, stfs.Skip(k9).ToArray(), ibh, mtype, numelh, nrts, stfsf(n), dxs, bulks, stfss, nty, nsn, rdc, n, xx1, xx2, xx3, det, emodl2)
            Array.Copy(stf, 1, stfs, k9, stfs.Length - k9)

            If nmn <> 0 Then
                ReDim irect(4, nrttlm)
                For I = 1 To 4
                    For J = 1 To nrttlm - k6 + 1
                        irect(I, J) = irectm(I, k6 + J - 1)
                    Next
                Next
                ReDim stf(stfm.Length - k10)
                Array.Copy(stfm, k10, stf, 1, stfm.Length - k10)
                Call stiffs(x, irect, stf, ibh, mtype, numelh, nrtm, stfsf(n), dxm, bulkm, stfsm, nty, nmn, rdc, n, xx1, xx2, xx3, det, emodl2)
                ' Call stiffs(x, irect, stfm.Skip(k10).ToArray, ibh, mtype, numelh, nrtm, stfsf(n), dxm, bulkm, stfsm, nty, nmn, rdc, n, xx1, xx2, xx3, det, emodl2)
                Array.Copy(stf, 1, stfm, k10, stfm.Length - k10)
            End If

            stfmin = Math.Min(stfss, stfsm)
            stfmax = Math.Max(stfss, stfsm)

            Dim avgs = 0.5 * (stfmin + Math.Min(stfmax, 9 * stfmin))
            If nty = 4 Then
                Dim msg1 = "ik02 call sscnv in sub slinit"
                GoTo 10
            End If
            ReDim stf(stfs.Length - k9)
            Array.Copy(stfs, k9, stf, 1, stfs.Length - k9)
            Call modstf(stf, nrts, stfss, avgs, 1, n)
            'Call modstf(stfs.Skip(k9).ToArray, nrts, stfss, avgs, 1, n)
            Array.Copy(stf, 1, stfs, k9, stfs.Length - k9)
            ReDim stf(stfm.Length - k10)
            Array.Copy(stfm, k10, stf, 1, stfm.Length - k10)
            Call modstf(stf, nrtm, stfsm, avgs, 2, n)
            'Call modstf(stfm.Skip(k10).ToArray, nrtm, stfsm, avgs, 2, n)
            Array.Copy(stf, 1, stfm, k10, stfm.Length - k10)

            Dim irtl() As Integer
            ReDim irect(4, nrttlm)
            For I = 1 To 4
                For J = 1 To nrttlm - k6 + 1
                    irect(I, J) = irectm(I, k6 + J - 1)
                Next
            Next
            ReDim lc(lmsr.Length - k8), mns(nsv.Length - k4), mms(msr.Length - k5), iloc(ilocs.Length - k4), irtl(irtlm.Length - k4), nseg(nsegm.Length - k7)
            Array.Copy(nsegm, k7, nseg, 1, nsegm.Length - k7)
            Array.Copy(lmsr, k8, lc, 1, lmsr.Length - k8)
            Array.Copy(nsv, k4, mns, 1, nsv.Length - k4)
            Array.Copy(msr, k5, mms, 1, msr.Length - k5)
            Array.Copy(ilocs, k4, iloc, 1, ilocs.Length - k4)
            Array.Copy(irtlm, k4, irtl, 1, irtlm.Length - k4)
            Call slavi1(x, irect, lc, mms, mns, iloc, irtl, nseg, nsn, nmn, n, llc, lresrt, resltl, result)
            'Call slavi1(x, irect, lmsr.Skip(k8).ToArray, msr.Skip(k5).ToArray, nsv.Skip(k4).ToArray, ilocs.Skip(k4).ToArray, irtlm.Skip(k4).ToArray, nsegm.Skip(k7).ToArray, nsn, nmn, n, llc, lresrt, resltl, result)
            Array.Copy(nseg, 1, nsegm, k7, nsegm.Length - k7)
            Array.Copy(lc, 1, lmsr, k8, lmsr.Length - k8)
            Array.Copy(mns, 1, nsv, k4, nsv.Length - k4)
            Array.Copy(mms, 1, msr, k5, msr.Length - k5)
            Array.Copy(iloc, 1, ilocs, k4, ilocs.Length - k4)
            Array.Copy(irtl, 1, irtlm, k4, irtlm.Length - k4)

            ReDim irect(4, nrttls)
            For I = 1 To 4
                For J = 1 To nrttls - k1 + 1
                    irect(I, J) = irects(I, k1 + J - 1)
                Next
            Next
            ReDim lc(lnsv.Length - k3), mns(nsv.Length - k4), mms(msr.Length - k5), iloc(ilocm.Length - k5), irtl(irtls.Length - k5), nseg(nsegs.Length - k2)
            Array.Copy(nsegs, k2, nseg, 1, nsegs.Length - k2)
            Array.Copy(lnsv, k3, lc, 1, lnsv.Length - k3)
            Array.Copy(nsv, k4, mns, 1, nsv.Length - k4)
            Array.Copy(msr, k5, mms, 1, msr.Length - k5)
            Array.Copy(ilocm, k5, iloc, 1, ilocm.Length - k5)
            Array.Copy(irtls, k5, irtl, 1, irtls.Length - k5)
            Call slavi1(x, irect, lc, mns, mms, iloc, irtl, nseg, nmn, nsn, n, llc, lresrt, resltl, result)
            'Call slavi1(x, irect, lnsv.Skip(k3).ToArray, nsv.Skip(k4).ToArray, msr.Skip(k5).ToArray, ilocm.Skip(k5).ToArray, irtls.Skip(k5).ToArray, nsegs.Skip(k2).ToArray, nmn, nsn, n, llc, lresrt, resltl, result)
            Array.Copy(nseg, 1, nsegs, k2, nsegs.Length - k2)
            Array.Copy(lc, 1, lnsv, k3, lnsv.Length - k3)
            Array.Copy(mns, 1, nsv, k4, nsv.Length - k4)
            Array.Copy(mms, 1, msr, k5, msr.Length - k5)
            Array.Copy(iloc, 1, ilocm, k5, ilocm.Length - k5)
            Array.Copy(irtl, 1, irtls, k5, irtls.Length - k5)

            ReDim irect(4, nrttlm)
            For I = 1 To 4
                For J = 1 To nrttlm - k6 + 1
                    irect(I, J) = irectm(I, k6 + J - 1)
                Next
            Next
            ReDim lc(lmsr.Length - k8), mns(nsv.Length - k4), mms(msr.Length - k5), iloc(ilocs.Length - k4), irtl(irtlm.Length - k4), stf(stfm.Length - k10)
            Array.Copy(stfm, k10, stf, 1, stfm.Length - k10)
            Array.Copy(lmsr, k8, lc, 1, lmsr.Length - k8)
            Array.Copy(nsv, k4, mns, 1, nsv.Length - k4)
            Array.Copy(msr, k5, mms, 1, msr.Length - k5)
            Array.Copy(ilocs, k4, iloc, 1, ilocs.Length - k4)
            Array.Copy(irtlm, k4, irtl, 1, irtlm.Length - k4)
            Call slavi2(x, irect, lc, mms, mns, iloc, irtl, stf, nsn, nmn, nty,
                        cst, n, pend(n), 1, ioff, xx1, xx2, xx3, ixx, xs, ys, zs, amx, amy, amz, vn, hh,
                        xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)
            'Call slavi2(x, irect, lmsr.Skip(k8).ToArray, msr.Skip(k5).ToArray, nsv.Skip(k4).ToArray, ilocs.Skip(k4).ToArray, irtlm.Skip(k4).ToArray, stfm.Skip(k10).ToArray, nsn, nmn, nty,
            '          cst, n, pend(n), 1, ioff, xx1, xx2, xx3, ixx, xs, ys, zs, amx, amy, amz, vn, h,
            '         xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)
            Array.Copy(stf, 1, stfm, k10, stfm.Length - k10)
            Array.Copy(lc, 1, lmsr, k8, lmsr.Length - k8)
            Array.Copy(mns, 1, nsv, k4, nsv.Length - k4)
            Array.Copy(mms, 1, msr, k5, msr.Length - k5)
            Array.Copy(iloc, 1, ilocs, k4, ilocs.Length - k4)
            Array.Copy(irtl, 1, irtlm, k4, irtlm.Length - k4)

            If nty > 3 AndAlso ipass = 1 Then
                ReDim irect(4, nrttls)
                For I = 1 To 4
                    For J = 1 To nrttls - k1 + 1
                        irect(I, J) = irects(I, k1 + J - 1)
                    Next
                Next
                ReDim lc(lnsv.Length - k3), mns(nsv.Length - k4), mms(msr.Length - k5), iloc(ilocm.Length - k5), irtl(irtls.Length - k5), stf(stfs.Length - k9)
                Array.Copy(stfs, k9, stf, 1, stfs.Length - k9)
                Array.Copy(lnsv, k3, lc, 1, lnsv.Length - k3)
                Array.Copy(nsv, k4, mns, 1, nsv.Length - k4)
                Array.Copy(msr, k5, mms, 1, msr.Length - k5)
                Array.Copy(ilocm, k5, iloc, 1, ilocm.Length - k5)
                Array.Copy(irtls, k5, irtl, 1, irtls.Length - k5)
                Call slavi2(x, irect, lc, mns, mms, iloc, irtl, stf, nmn, nsn, nty,
                            cst, n, pend(n), 2, ioff, xx1, xx2, xx3, ixx, xs, ys, zs, amx, amy, amz, vn, hh,
                            xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)
                'Call slavi2(x, irect, lnsv.Skip(k3).ToArray, nsv.Skip(k4).ToArray, msr.Skip(k5).ToArray, ilocm.Skip(k5).ToArray, irtls.Skip(k5).ToArray, stfs.Skip(k9).ToArray, nmn, nsn, nty,
                '            cst, n, pend(n), 2, ioff, xx1, xx2, xx3, ixx, xs, ys, zs, amx, amy, amz, vn, h,
                '            xx112, xx134, xx212, xx234, xx312, xx334, xx112p, xx134p, xx212p, xx234p, xx312p, xx334p, xx114, xx123, xx214, xx223, xx314, xx323, x1423, y1423, z1423, x1423p, y1423p, z1423p)
                Array.Copy(stf, 1, stfs, k9, stfs.Length - k9)
                Array.Copy(lc, 1, lnsv, k3, lnsv.Length - k3)
                Array.Copy(mns, 1, nsv, k4, nsv.Length - k4)
                Array.Copy(mms, 1, msr, k5, msr.Length - k5)
                Array.Copy(iloc, 1, ilocm, k5, ilocm.Length - k5)
                Array.Copy(irtl, 1, irtls, k5, irtls.Length - k5)
            End If

10:         k1 = k1 + nrts
            k2 = k2 + 1 + nsn
            k3 = k3 + nst
            k4 = k4 + nsn
            k5 = k5 + nmn
            k6 = k6 + nrtm
            k7 = k7 + 1 + nmn
            k8 = k8 + mst
            k9 = k9 + nrts
            k10 = k10 + nrtm
            k11 = k11 + nsn + nmn
        Next

    End Sub
End Class
'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine slinit(
'     1iparm,irects,irectm,nsv,msr,nsegs,nsegm,lnsv,lmsr,ilocs,ilocm,
'c     2stfs,stfm,irtls,irtlm,mtype,cm,thicks,x,xo,bh,bs,cst,chrlen,ethik,
'c     &rdc,ngap,pend,irbacm,islr,irbn)
'     2stfs,stfm,irtls,irtlm,mtype,x,bh,cst,
'     &rdc,pend)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to drive slide surface initialization
'c
'	common/block02/bNIKE3D !ikawa 04/14/03
'	logical bNIKE3D
'
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh
'      common/bk310/stsf(1)
'c      logical ithick,ioff,inoreord
'      common/bk37/numsv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
'      
'c
'      dimension
'     1iparm(7,1),irects(1),irectm(1),nsv(1),msr(1),nsegs(1),            vax
'     2nsegm(*),lnsv(*),lmsr(*),ilocs(*),ilocm(*),stfs(*),stfm(*),
'c     3irtls(*),irtlm(*),mtype(*),cm(*),thicks(*),x(*),xo(*),bh(*),bs(*),
'     3irtls(*),irtlm(*),mtype(*),x(*),bh(*),
'c     4cst(3,1),chrlen(1),ethik(1),rdc(*),ngap(*),pend(*),irbacm(*)
'     4cst(3,1),rdc(*),pend(*)
'c     5islr(*),irbn(*)
'
'      !ikawa data k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11/1,1,1,1,1,1,1,1,1,1,1/
'      common/slinit_k1/k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11
'c
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'slinit'
'      ik01 = ik01 + 1
'      end if
'
'      write(lutty,100)
'
'	if (bNIKE3D) then
'        write(*,100)
'	end if
'
'      do 20 n=1,numsv
'      nrts=iparm(1,n)
'      nrtm=iparm(2,n)
'      nsn =iparm(3,n)
'      nmn =iparm(4,n)
'      nty =iparm(5,n)
'      nst =iparm(6,n)
'      mst =iparm(7,n)
'
'      ipass = 1*isign(1,nty)        ! check for single pass
'      nty = abs(nty)
'
'c      ithick=.false.                ! ithick shell thickness flag
'c      if(nty.eq.4) ithick=.true.
'      if(nty.eq.8) then             ! type 3 with shell thickness
'       nty=3
'c      ithick=.true.
'      endif
'       ioff=.false.
'      if(nty.eq.7) then
'       nty=2
'       iparm(5,n)=2
'       ioff=.true.
'      endif
'c       inoreord = .false.
'      if(nty.eq.10) then            ! dont do facet node reordering
'c       inoreord = .true.
'       nty=3
'       iparm(5,n)=ipass*3
'      endif
'c
'c      if(nnrbt.ne.0.and.islr(n).ne.0) then
'!       call inrbacc(irbacm,nsv(k4),msr(k5),irbn,x,n,islr,
'!     &                                          stfs(k9),stsf(n))
'c        write(10,*) 'ik02 call inrbacc in sub slinit'
'c       go to 10
'c      endif
'      call slavi0(x,irects(k1),nsegs(k2),lnsv(k3),nsv(k4),msr(k5),
'     & ilocs(k4),nmn,nsn,nrts)
'      call slavi0(x,irectm(k6),nsegm(k7),lmsr(k8),msr(k5),nsv(k4),
'     & ilocm(k5),nsn,nmn,nrtm)
'c      call stiffs(xo,irects(k1),stfs(k9) ,bh,bs,cm,mtype,thicks,
'c     & numelh,numels,nrts,stsf(n),dxs,bulks,stfss,
'c     & chrlen(n),ethik(k11),ithick,nty,nsn,nsv(k4),rdc,inoreord)
'c      if (nmn.ne.0)
'c     &call stiffs(xo,irectm(k6),stfm(k10),bh,bs,cm,mtype,thicks,
'c     & numelh,numels,nrtm,stsf(n),dxm,bulkm,stfsm,
'c     & chrlen(n),ethik(k11+nsn),ithick,nty,nmn,msr(k5),rdc,inoreord)
'c      call stiffs(xo,irects(k1),stfs(k9),bh,mtype,
'      call stiffs(x,irects(k1),stfs(k9),bh,mtype,
'     & numelh,nrts,stsf(n),dxs,bulks,stfss,
'     & nty,nsn,nsv(k4),rdc)
'      if (nmn.ne.0)
'c     &call stiffs(xo,irectm(k6),stfm(k10),bh,mtype,
'     &call stiffs(x,irectm(k6),stfm(k10),bh,mtype,
'     & numelh,nrtm,stsf(n),dxm,bulkm,stfsm,
'     & nty,nmn,msr(k5),rdc)
'      stfmin=min(stfss,stfsm)
'      stfmax=max(stfss,stfsm)
'      avgs  =.50*(stfmin+min(stfmax,9.*stfmin))
'      if (nty.eq.4) then
'!      call sscnv(irects(k1),nrts,nsv(k4),nsn)
'       write(10,*) 'ik02 call sscnv in sub slinit'
'      go to 10
'      endif
'      call modstf(stfs(k9) ,nrts,stfss,avgs,1,n)
'      call modstf(stfm(k10),nrtm,stfsm,avgs,2,n)
'      call slavi1(x,irectm(k6),lmsr(k8),msr(k5),nsv(k4),
'     & ilocs(k4),irtlm(k4),nsegm(k7),nsn,nmn,n)
'      call slavi1(x,irects(k1),lnsv(k3),nsv(k4),msr(k5),
'     & ilocm(k5),irtls(k5),nsegs(k2),nmn,nsn,n)
'c      call slavi2(x,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4),
'c     & irtlm(k4),stfm(k10),nsn,nmn,nty,cst(1,k4),n,ngap(n),pend(n),
'c     & ethik(k11),ethik(k11+nsn),1,ioff)
'      call slavi2(x,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4),
'     & irtlm(k4),stfm(k10),nsn,nmn,nty,cst(1,k4),n,pend(n),
'     & 1,ioff)
'      if(nty.ge.3.and.ipass.eq.1)
'c     &call slavi2(x,irects(k1),lnsv(k3),nsv(k4),msr(k5),ilocm(k5),
'c     & irtls(k5),stfs(k9),nmn,nsn,nty,cst(1,k4),n,ngap(n),pend(n),
'c     & ethik(k11+nsn),ethik(k11),2,ioff)
'     &call slavi2(x,irects(k1),lnsv(k3),nsv(k4),msr(k5),ilocm(k5),
'     & irtls(k5),stfs(k9),nmn,nsn,nty,cst(1,k4),n,pend(n),
'     & 2,ioff)
'c
'   10 k1=k1+4*nrts
'      k2=k2+1+nsn
'      k3=k3+nst
'      k4=k4+nsn
'      k5=k5+nmn
'      k6=k6+4*nrtm
'      k7=k7+1+nmn
'      k8=k8+mst
'      k9=k9+nrts
'      k10=k10+nrtm
'      k11=k11+nsn+nmn
'   20 continue
'      return
'  100 format(//' SLIDE SURFACE INITIALIZATION data'/1x,60('=')/
'     &'    interface    side    max. relocation distance   node')
'      end
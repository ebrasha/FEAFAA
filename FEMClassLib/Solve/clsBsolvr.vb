'This file contains all the methods of bsolvr.f
Partial Public Class clsSolve

    
    Public bNIKE3D As Boolean

    'Public hxdata(), sfdata() As Double

    Private ldgen As Boolean    'by YC 102418

    Dim ampcgus(), ampcgp(), ampcgz(), ampcgap() As Double
    ''' <summary>
    ''' drive linear equation solvers
    ''' </summary>
    ''' <param name="c"></param>
    ''' <param name="iopt"></param>
    ''' <param name="_is"></param>
    Public Sub bsolvr(ByRef c() As Double, ByRef iopt As Integer, ByRef _is As Integer)
        ' Call CheckName2("bsolvr", iopt)
        'Dim ldgen = False   'YC 092018

        'Dim ampcgus(54000), ampcgp(54000), ampcgz(54000), ampcgap(54000) As Double   'YC 092018
        ReDim Preserve ampcgus(neql), ampcgp(neql), ampcgz(neql), ampcgap(neql)


        If Not bNIKE3D Then
            If StopFEDFAA = -1 Then Return
        End If

        'Call objNik3d.timing(cpuio, cpuip, _is, 2)  'YC 092018
        Call objComsub.timing(cpuio, cpuip, _is, 2)

        'c-----------------------------------------------------------------------
        'c     linear solver data is appended to other data in blank common
        'c-----------------------------------------------------------------------

        If iopt = 1 Then
            'c....... determine profile structure for global matrix
        ElseIf iopt = 2 Then
            'c....... perform assembly/factorization (direct) or counterparts (iter)
            melemt = 0

            'TODO - Missing Sub
            Call invert(abdg, hxdata1, hxdata2, sfdata1, sfdata2)   ' QW 12-12-2018-

            'Call Check2D(abdg, 6, numnp, istep)

        End If

        If iopt >= 2 Then
            'c....... solve for unknown vector
            'TODO - Missing Sub
            Call precon(c, ampcgus, ampcgp, ampcgz, ampcgap, abdg, idp, hxdata1, hxdata2, sfdata1, sfdata2, iopt) ' QW 12-12-2018-
            ibkflg = 1
        End If

        'Call objNik3d.timing(cpuio, cpuip, _is, 3)  'YC 092018
        Call objComsub.timing(cpuio, cpuip, _is, 3)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine bsolvr(c,iopt,is)
'c
'
'      implicit double precision (a-h,o-z)                             
'
'c
'c===> module to drive linear equation solvers
'c
'	common/block02/bNIKE3D, bNike3dMsg
'	logical bNIKE3D, bNike3dMsg
'
'	common/block03/StopFEDFAA, FEDFAAStopped	
'      INTEGER(4) StopFEDFAA, FEDFAAStopped	
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      common/double/iprec
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,
'     &            nipmx,nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx  ebe
'      common/bk01/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n14a
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09
'      common/ebye5/mbsize,nhxblk,ihxblk(750),nsnode,nsfblk,isfblk(750)
'      common/bk26/ntlen,ntlenf
'      common/bk30/cpuio(72),cpuip(72)   
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'c      
'      common/incomp/incflg,ibkflg,stepls,stepsv      
'      common/fissl1/melemt,nnns,n2g,llls
'      common/elcnts/numelf
'      logical ldgen
'      common/ebye1/itrprt,itrlmt,tollin,ldgen
'      common/ebye2a/ mpdiag,mpelhx,mpelsf
'      common/ebye2b/ mpcgus, mpcgp, mpcgz,mpcgap                      
'      common/bkneq/neql
'      common/iread/idp(54000),x(27000) 
'      common/bdg/abdg(189000)                                 ! QW 11-11-2015
'      common/hx/hxdata(10000000),sfdata(1000000),sftemp(10000)    ! QW 11-11-2015
'      
'      dimension ampcgus(54001),ampcgp(54001),ampcgz(54001),
'     $ ampcgap(54000)
'c      
'      dimension c(*)
'
'c 
'	if (not (bNike3d)) then !ikawa 09/22/03
'      IF( StopFEDFAA .eq. -1 ) return
'	end if
'
'      call timing (cpuio,cpuip,is,2)
'c
'c-----------------------------------------------------------------------
'c     linear solver data is appended to other data in blank common
'c-----------------------------------------------------------------------
'c
'      if (iopt.eq.1) then
'c
'c....... determine profile structure for global matrix
'c
'      else if (iopt.eq.2) then
'c
'c....... perform assembly/factorization (direct) or counterparts (iter)
'c
'         melemt=0
'   
'            call invert(abdg,hxdata,sfdata)      
'
'      endif
'c
'      if (iopt.ge.2) then
'c
'c....... solve for unknown vector
'
'            call precon(c,ampcgus,ampcgp,ampcgz,ampcgap,     ! QW 11-11-2015
'     &                    abdg,abdg,idp,                  
'     &                    hxdata,sfdata)
'
'         ibkflg = 1                                                  
'      endif
'      call timing (cpuio,cpuip,is,3)
'
'c 
'      return
'c                 
'      end

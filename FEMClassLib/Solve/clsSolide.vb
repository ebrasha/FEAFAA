'This file contains all the methods of solide.f
Partial Public Class clsSolve


    ' added by YC 092018
    Public n5, n6, iphase, incflg, nelpg As Integer

    Public ixh(,), matype(), ia() As Integer

    Public den(), sic(,,), dic(,), usic(,), dusic(,), ric(,) As Double ' QW 12-12-2018-


    'Private ric(,) As Double
    ' added by YC 092018 END



    ''' <summary>
    ''' enter hexahedral continuum element overlay
    ''' </summary>
    Public Sub solide()

        'Call objNik3d.timing(cpuio, cpuip, 14, 2)  'YC 092018
        Call objComsub.timing(cpuio, cpuip, 14, 2)


        Dim m1 = n2
        Dim m2 = n3
        Dim icnt2 = 0
        If iphase < 3 Then GoTo 10
        icnt2 = 1
        If iphase = 4 Then iphase = 2
        m1 = n6
        m2 = n5
        GoTo 20

10:     If incflg = 1 Then
            'TODO - check 1D-2D array in fe3dic

            'by YC 092018
            'Call fe3dic(un2, un3, un4, ia, x, matype, den, fval, idp, abdg, hxdata, icnt2,
            '              tvc2, sic, dic, usic, dusic, ric, numnp, nmmat) 
            'Call Check1D(un3, neql, istep)
            'Call Checka22(usic, dusic, 100, istep)
            Call fe3dic(un2, un3, un4, ixh, xyz, matype, den, fval, idp, abdg, hxdata1, hxdata2, icnt2,
              tvc2, sic, dic, usic, dusic, ric, numnp, nmmat)
            'by YC 092018 END

        Else
            'TODO - Missing Sub
            Call fe3dh(un2, un3, un4, ia, xyz, matype, den, fval, idp, abdg, hxdata1, hxdata2, icnt2, numnp, nmmat)

        End If
        GoTo 30

20:     If incflg = 1 Then
            'TODO - check 1D-2D array in fe3dic

            'Call fe3dic(un6, un5, un4, ia, x, matype, den, fval, idp, abdg, hxdata, icnt2,
            '             'tvc2, sic, dic, usic, dusic, ric, numnp, nmmat)  'by YC 092018
            'Call Checka22(usic, dusic, 100, istep)
            Call fe3dic(un6, un5, un4, ixh, xyz, matype, den, fval, idp, abdg, hxdata1, hxdata2, icnt2,
             tvc2, sic, dic, usic, dusic, ric, numnp, nmmat)

        Else
            'TODO - Missing Sub
            Call fe3dh(un6, un5, un4, ia, xyz, matype, den, fval, idp, abdg, hxdata1, hxdata2, icnt2, numnp, nmmat)
        End If
30:     Call objComsub.timing(cpuio, cpuip, 14, 3)

        Call objComsub.timing(cpuio, cpuip, 14, 3)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine solide(*)
'c
'      implicit double precision (a-h,o-z)                            
'c
'c===> module to enter hexahedral continuum element overlay
'c
'      common/bk00/negb1,negb2,negb3,nm0,nm1,nm2,nm3,nipmx,
'     & nmmat,nm1a,nm1b,nm1c,nipmx1,nconsx
'      common/bk01/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n14a
'      common/bk05/n000,n001,nh01,nh04,nh05,nh06,nh07,nh08,nh09
'      common/bk13/ft,nprint,nnel,mpri,locstr,jpri,numelh
'      common/bk10/iphase,nelgp,imass,lhex(9)   
'      common/bk26/ntlen,ntlenf
'      common/bk30/cpuio(72),cpuip(72)
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/incomp/incflg,ibkflg,stepls,stepsv
'      common/ebye2a/ mpdiag,mpelhx,mpelsf
'c      
'      common/in3/ia(81000),ihv(9001),numelg(2)              ! QW 11-11-2015
'      common/hx/hxdata(10000000),sfdata(1000000),sftemp(10000)         ! QW 11-11-2015
'      common/bkneq/neql
'      common/bk16/nlcur,nptst,nload,nptm
'      common/iread/idp(54000),x(27000)                                ! QW 11-11-2015
'      common/ini/npc(25),p(480),cmde(8,24),ixde(3,500),sclf(500),
'     & dehv(11,500),dehva(11,500),fval(48),mtypde(6)                  ! QW 11-11-2015
'      common/bdg/abdg(189000)
'c      
'      common/aa/anegb1(7508928),anegb2(7508928)
'      common/nib/un2(54000),un3(54000),un4(54000),un5(54000),
'     & un6(54000),tvc2(54000)
'      common/readm/matype(25),den(25),prop(48,25),csprop(24,25),
'     & trefm(25),tmecc(25),rdc(4,25),aux(48)                           ! QW 11-11-2015
'      common/nh/sic(297,9000),dic(9,9000),usic(9,9000),dusic(9,9000),  ! QW 11-11-2015
'     & ric(9,9000)                                        
'c      
'      call timing(cpuio,cpuip,14,2)
'      m1=n2
'      m2=n3
'      icnt2=0
'      if (iphase.lt.3) go to 10
'      icnt2=1
'      if (iphase.eq.4) iphase=2
'      m1=n6
'      m2=n5
'      goto 20                         ! QW 11-11-2015
'c
'   10 if (incflg.eq.1) then
'
'      call fe3dic(un2,un3,un4,ia,x,     
'     1               matype,den,fval,idp,        
'     2               abdg,hxdata,icnt2,               
'     3               tvc2,sic,dic,usic,dusic,ric,
'     4               numnp,nmmat,*991)               
'      else                                                            
'
'      call fe3dh (un2,un3,un4,ia,x,
'     1               matype,den,fval,idp,
'     2               abdg,hxdata,icnt2,numnp,nmmat,*991)
'
'      endif
'      goto 30
'c
'20    if (incflg.eq.1) then                                            
'      
'      call fe3dic(un6,un5,un4,ia,x,     
'     1               matype,den,fval,idp,        
'     2               abdg,hxdata,icnt2,               
'     3               tvc2,sic,dic,usic,dusic,ric,
'     4               numnp,nmmat,*991)               
'      else                                                             
'      call fe3dh (un6,un5,un4,ia,x,
'     1               matype,den,fval,idp,
'     2               abdg,hxdata,icnt2,numnp,nmmat,*991)  
'      endif                                                             
'30    call timing(cpuio,cpuip,14,3)
'      return
'c
'  991 call timing(cpuio,cpuip,14,3)
'      return 1
'      end

'This file contains all the methods of dscrte.f
Partial Public Class clsSolve

    
    Public nmmass As Integer
    Public mtypde(), ixde(,) As Integer, cmde(,), sclf() As Double



    ''' <summary>
    ''' to drive discrete element routine
    ''' </summary>
    ''' <param name="id"></param>
    ''' <param name="u"></param>
    ''' <param name="ui"></param>
    ''' <param name="r"></param>
    ''' <param name="x"></param>
    Public Sub dscrte(ByRef id(,) As Integer, ByRef u() As Double, ByRef ui() As Double,
                      ByRef r() As Double, ByRef x(,) As Double)


        If nmelde > 0 Then
            icnt2 = 1
            If lelstf Then icnt2 = 0

            'Call blkcpy(dehva, dehv, 11 * nmelde)  ' YC 102418
            Call blkcpy2D(dehva, dehv, 11, nmelde)

            nnns = 6
            llls = 27

            'YC? 092018
            'Dim x_copy(2, nmelde - 1), abdg_copy(,) As Double
            'Dim id_copy(,) As Integer

            'TODO set data from x to x_copy. x not modified in spring and hence no need to copy back
            'TODO set data from id to id_copy. 
            'TODO set data from abdg to abdg_copy. 

            'Call spring(mtypde, cmde, ixde, sclf, dehv, nmelde, x_copy, u, ui, id_copy, r, npc, p, icnt2, idid, lcd, amad, rf, abdg_copy)

            '
            'TODO set data from abdg to abdg_copy. 
            'TODO set data from abdg_copy to abdg

            'Dim abdg_2D(21, numnp) As Double
            'Call objComsub.ArrayConvert1Dto2D(abdg, abdg_2D, 21, numnp)

            'Call spring(mtypde, cmde, ixde, sclf, dehv, nmelde, x, u, ui, id, r, npc, p, icnt2, idid, lcd, amad, rf, abdg_2D)
            Call spring(mtypde, cmde, ixde, sclf, dehv, nmelde, x, u, ui, id, r, npc, p, icnt2, idid, lcd, amad, rf, abdg)
            'Call objComsub.ArrayConvert2Dto1D(abdg_2D, 21, numnp, abdg)

            'YC? 092018 END



        End If

        If nmmass > 0 Then
            nnns = 3
            llls = 9

            Dim msg = "ik02 call addmss in sub dscrte"
        End If

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine dscrte(id,u,ui,r,x)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to drive discrete element routine
'c
'      common/bk01/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n14a
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e,
'     & n20f,n20g
'      common/bk10/iphase,nelgp,imass,lhex(9)
'      common/b10a/inpsd,nmmtde,nmelde,nmmass
'      common/b10b/kd01,kd02,kd03,kd04,kd05,kd06
'      
'      common/bk26/ntlen,ntlenf
'      common/ebye2a/ mpdiag,mpelhx,mpelsf
'
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/fissl1/melemt,nnns,n2g,llls
'      logical lelstf
'      common/elstfb/lelstf
'      common/bk16/nlcur,nptst,nload,nptm
'      common/bk35/numdc,numudc,nrcc
'      common/ini/npc(25),p(480),cmde(8,24),ixde(3,500),sclf(500),
'     & dehv(11,500),dehva(11,500),fval(48),mtypde(6)                 ! QW 11-11-2015
'      common/ibc/noded(1000),idid(1000),lcd(1000),amad(1000),
'     & idflad(1000),rf(1000),rfa(1000)
'      common/bdg/abdg(189000)
'c      
'c      
'      dimension id(*),u(*),ui(*),r(*),x(*)
'
'c NKC 8/9/99
'c      write(6,*) 'entering dscrte.F'
'
'      if (nmelde.gt.0) then
'      icnt2=1
'      if(lelstf) icnt2=0
'
'      call blkcpy(dehva,dehv,11*nmelde)                    ! QW 11-11-2015
'      nnns=6
'      llls=27
'
'      call spring(mtypde,cmde,ixde,sclf,dehv,nmelde,x,u,  
'     &            ui,id,r,npc,p,   
'     &            icnt2,idid,lcd,amad,rf,abdg)
'
'      endif
'      if (nmmass.gt.0) then
'      nnns=3
'      llls=9
'
'      write(10,*) 'ik02 call addmss in sub dscrte'
'	endif
'
'c NKC 8/9/99
'c      write(6,*) 'leaving1 dscrte.F'
'
'      return
'      end

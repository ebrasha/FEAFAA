'This file contains all the methods of dotprd.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to compute dot product, ignoring dof's with essential bc's
    ''' </summary>
    ''' <param name="v1"></param>
    ''' <param name="v2"></param>
    Public Function dotprd(ByRef v1() As Double, ByRef v2() As Double) As Double
        If numudc <> 0 Then
            'TODO
            '	write(10,*) 'ik02 call predot 1 in sub dotprd'
        End If
        dotprd = fdot(v1, v2, neql)
        If numudc <> 0 Then
            'TODO
            '      write(10,*) 'ik02 call predot 2 in sub dotprd'
        End If
    End Function
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      function dotprd  (v1,v2)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to compute dot product, ignoring dof's with essential bc's
'c
'      common/bk03/ntotal,n15,n16,n17,n18,n19,n20a,n20b,n20c,n20d,n20e,
'     & n20f,n20g
'      common/bk34/dn1,dn2,nwebuf,ntime,mthsol,numnp,imem
'      common/bk35/numdc,numudc,nrcc
'      common/bkneq/neql
'      dimension v1(*),v2(*)
'      
'      if(numudc.ne.0) then 
'	write(10,*) 'ik02 call predot 1 in sub dotprd'
'      end if
'	dotprd =fdot(v1,v2,neql)
'      if(numudc.ne.0) then
'      write(10,*) 'ik02 call predot 2 in sub dotprd'
'      end if
'	return
'      end

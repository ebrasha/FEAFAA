'This file contains all the methods of cndres.f
Partial Public Class clsSolve


    ''' <summary>
    ''' to condense residual to compatible degrees of freedom
    ''' </summary>
    ''' <param name="r"></param>
    ''' <param name="rxic"></param>
    ''' <param name="sic"></param>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    Public Sub cndres(ByRef r(,) As Double, ByRef rxic(,) As Double, ByRef sic(,,) As Double,
                      ByRef lft As Integer, ByRef llt As Integer)



        If ilu = 1 Then

            '   ... perform forward-backward to compute incompatible dof
            'Create copy of sic


            Dim ind0 = sic.GetUpperBound(0) - 24
            Dim ind1 = sic.GetUpperBound(1)
            Dim ind2 = sic.GetUpperBound(2)
            Dim sic_copy(ind0, ind1, ind2) As Double
            'set values of sic to copy of sic
            For ii0 = 0 To ind0
                For ii1 = 0 To ind1
                    For ii2 = 0 To ind2
                        sic_copy(ii0, ii1, ii2) = sic(ii0 + 24, ii1, ii2)
                    Next
                Next
            Next

            Call lufwbv(sic_copy, 33, 9, 9, rxic, 64, lft, llt)

            'set values of sic copy to sic
            For ii0 = 0 To ind0
                For ii1 = 0 To ind1
                    For ii2 = 0 To ind2
                        sic(ii0 + 24, ii1, ii2) = sic_copy(ii0, ii1, ii2)
                    Next
                Next
            Next

        Else

            '.... backsubstitution of incompatible dof

            'For j = 8 To 0 Step -1     ' YC 102418
            For j = 9 To 1 Step -1
                For lv = lft To llt
                    rxic(lv, j) = rxic(lv, j) / sic(j + 24, j, lv)
                Next

                'For i = 0 To j     ' YC 102418
                For i = 1 To j - 1
                    For lv = lft To llt
                        rxic(lv, i) = rxic(lv, i) - sic(i + 24, j, lv) * rxic(lv, j)
                    Next
                Next
            Next

            'c.... forward reduction of incompatible dof

            'For j = 0 To 8  ' YC 102418
            For j = 1 To 8
                For lv = lft To llt
                    rxic(lv, j) = rxic(lv, j) / sic(j + 24, j, lv)
                Next

                'For i = j + 1 To 8     ' YC 102418
                For i = j + 1 To 9
                    For lv = lft To llt
                        rxic(lv, i) = rxic(lv, i) - sic(j + 24, i, lv) * rxic(lv, j)
                    Next
                Next
            Next
        End If

        '.... condense compatible dof

        'For i = 0 To 23        ' YC 102418
        '    For j = 0 To 8
        For i = 1 To 24
            For j = 1 To 9
                For lv = lft To llt
                    r(lv, i) = r(lv, i) - sic(i, j, lv) * rxic(lv, j)
                Next
            Next
        Next


    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine cndres(r,rxic,sic,lft,llt)
'c
'      implicit double precision (a-h,o-z)                             
'c
'c===> module to condense residual to compatible degrees of freedom
'c
'      dimension r(64,1),rxic(64,1),sic(33,9,*)
'      !ikawa data ilu/1/
'	common/cndres_data1/ilu
'c
'      if(ilu.eq.1) then
'c
'c   ... perform forward-backward to compute incompatible dof
'c
'      call lufwbv(sic(25,1,1),33,9,9,rxic,64,lft,llt)
'c
'      else
'c
'c.... backsubstitution of incompatible dof
'c
'      do 400 j=9,1,-1
'         do 100 lv=lft,llt
'  100    rxic(lv,j) = rxic(lv,j)/sic(j+24,j,lv)
'         do 300 i=1,j-1
'         do 200 lv=lft,llt
'  200    rxic(lv,i) = rxic(lv,i) - sic(i+24,j,lv)*rxic(lv,j)
'  300    continue
'  400 continue
'c
'c.... forward reduction of incompatible dof
'c
'      do 800 j=1,9
'         do 500 lv=lft,llt
'  500    rxic(lv,j) = rxic(lv,j)/sic(j+24,j,lv)
'         do 700 i=j+1,9
'         do 600 lv=lft,llt
'  600    rxic(lv,i) = rxic(lv,i) - sic(j+24,i,lv)*rxic(lv,j)
'  700    continue
'  800 continue
'c
'      endif
'c
'c.... condense compatible dof
'c
'      do 1100 i=1,24
'         do 1000 j=1,9
'         do 900 lv=lft,llt
'  900    r(lv,i) = r(lv,i) - sic(i,j,lv)*rxic(lv,j)
' 1000    continue
' 1100 continue
'c
'      return
'      end

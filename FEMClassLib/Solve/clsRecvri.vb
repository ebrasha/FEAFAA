'This file contains all the methods of recvri.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to recover displacement increments for incompatible dof
    ''' </summary>
    ''' <param name="delus"></param>
    ''' <param name="sic"></param>
    ''' <param name="dusic"></param>
    ''' <param name="ric"></param>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    Public Sub recvri(ByRef delus(,) As Double, sic0(,,) As Double, dusic0(,) As Double,
                      ric0(,) As Double, lft As Integer, llt As Integer)

        'c.... compute coupling from compatible dof

        Dim i As Integer ' YC 102418
        'Call Check2D(delus, llt, 24, istep)
        'For i = 0 To 8     ' YC 102418
        '    For lv = lft - 1 To llt - 1
        For i = 1 To 9
            For lv = lft To llt
                dusic0(i, lv) = ric0(i, lv)
            Next

            'For j = 0 To 23        ' YC 102418
            '    For lv = lft - 1 To llt - 1
            For j = 1 To 24
                For lv = lft To llt
                    dusic0(i, lv) = dusic0(i, lv) - sic0(j, i, lv) * delus(lv, j)
                Next
            Next
        Next
        'Call Check2D(dusic0, 9, llt, istep)
        If ilu = 1 Then GoTo 5000

        'c.... backsubstitution of incompatible dof

        'For j = 8 To 0 Step -1   ' YC 102418
        '    For lv = lft - 1 To llt - 1
        For j = 9 To 1 Step -1
            For lv = lft To llt
                dusic0(j, lv) = dusic0(j, lv) / sic0(j + 24, j, lv)
            Next

            'For i = 0 To j
            '    For lv = lft - 1 To llt - 1
            For i = 1 To j - 1
                For lv = lft To llt
                    dusic0(i, lv) = dusic0(i, lv) - sic0(i + 24, j, lv) * dusic0(j, lv)
                Next
            Next
        Next

        'c.... forward reduction of incompatible dof

        'For j = 0 To 8   ' YC 102418
        '    For lv = lft - 1 To llt - 1
        For j = 1 To 9
            For lv = lft To llt
                dusic0(j, lv) = dusic0(j, lv) / sic0(j + 24, j, lv)
            Next

            'For i = j + 1 To 8     ' YC 102418
            '    For lv = lft - 1 To llt - 1
            For i = j + 1 To 9
                For lv = lft To llt
                    dusic0(i, lv) = dusic0(i, lv) - sic0(j + 24, i, lv) * dusic0(j, lv)
                Next
            Next
        Next

        Return

5000:

        'c   ... perform forward-backward to compute incompatible dof
        'TODO - set dim
        Dim II, JJ, KK As Integer
        Dim sic_copy(9, 9, llt) As Double
        For II = 1 To 9
            For JJ = 1 To 9
                For KK = lft To llt
                    sic_copy(II, JJ, KK) = sic0(II + 24, JJ, KK)
                Next
            Next
        Next
        Call lufwbk(sic_copy, 33, 9, 9, dusic0, 9, lft, llt)
        'TODO - copy sic_copy to sic

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine recvri(delus,sic,dusic,ric,lft,llt)
'c
'
'      implicit double precision (a-h,o-z)                         
'
'c
'c===> module to recover displacement increments for incompatible dof
'c
'      dimension delus(64,1),sic(33,9,*),dusic(9,*),ric(9,*)
'      !ikawa data ilu/1/
'	common/recvri_data1/ilu
'c
'c.... compute coupling from compatible dof
'c
'      do 300 i=1,9
'         do 50 lv=lft,llt
'   50    dusic(i,lv) = ric(i,lv)
'         do 200 j=1,24
'         do 100 lv=lft,llt
'  100    dusic(i,lv) = dusic(i,lv) - sic(j,i,lv)*delus(lv,j)
'  200    continue
'  300 continue
'c
'      if(ilu.eq.1) goto 5000
'c
'c.... backsubstitution of incompatible dof
'c
'      do 700 j=9,1,-1
'         do 400 lv=lft,llt
'  400    dusic(j,lv) = dusic(j,lv)/sic(j+24,j,lv)
'         do 600 i=1,j-1
'         do 500 lv=lft,llt
'  500    dusic(i,lv) = dusic(i,lv) - sic(i+24,j,lv)*dusic(j,lv)
'  600    continue
'  700 continue
'c
'c.... forward reduction of incompatible dof
'c
'      do 1100 j=1,9
'         do 800 lv=lft,llt
'  800    dusic(j,lv) = dusic(j,lv)/sic(j+24,j,lv)
'         do 1000 i=j+1,9
'         do 900 lv=lft,llt
'  900    dusic(i,lv) = dusic(i,lv) - sic(j+24,i,lv)*dusic(j,lv)
' 1000    continue
' 1100 continue
'c
'      return
'c
' 5000 continue
'c
'c   ... perform forward-backward to compute incompatible dof
'c
'      call lufwbk(sic(25,1,1),33,9,9,dusic,9,lft,llt)
'      return
'      end

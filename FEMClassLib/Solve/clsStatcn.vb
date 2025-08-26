'This file contains all the methods of statcn.f
Partial Public Class clsSolve

    Public ilu As Integer = 1  ' common/cndres_data1/i000   =i000 in Nike3D.f 'YC? 092018


    ''' <summary>
    ''' to perform static condensation/partial factorization of incompatible degrees of freedom for hexahedral element
    ''' </summary>
    ''' <param name="s"></param>
    ''' <param name="ekic"></param>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    Public Sub statcn(ByRef s(,) As Double, ByRef ekic(,,) As Double, ByRef lft As Integer, ByRef llt As Integer)

        'Dim temp(63), temp2(63), aj(8, 63) As Double       ' YC 102418
        Dim temp(64), temp2(64), aj(9, 64) As Double

        Dim ij As Integer

        Dim i, j, k As Integer      ' YC 102418



        If ilu = 1 Then GoTo 5000

        For lv = lft To llt

            'ekic(32, 8, lv) = Math.Sqrt(ekic(32, 8, lv))    ' YC 102418
            'temp(lv) = 1.0 / ekic(32, 8, lv)
            ekic(33, 9, lv) = Math.Sqrt(ekic(33, 9, lv))
            temp(lv) = 1.0 / ekic(33, 9, lv)
        Next

        'For i = 0 To 31     ' YC 102418
        For i = 1 To 32

            For lv = lft To llt

                'ekic(i, 8, lv) = temp(lv) * ekic(i, 8, lv)  ' YC 102418
                ekic(i, 9, lv) = temp(lv) * ekic(i, 9, lv)
            Next
        Next

        'For j = 7 To 0 Step -1  ' YC 102418
        For j = 8 To 1 Step -1
            '....... reduce diagonal coefficient
            For lv = lft To llt
                temp(lv) = 0.0
            Next

            'For l = j + 1 To 8 ' YC 102418
            For l = j + 1 To 9
                For lv = lft To llt

                    'temp(lv) = temp(lv) + ekic(j + 23, l, lv) * ekic(j + 23, l, lv)    ' YC 102418
                    temp(lv) = temp(lv) + ekic(j + 24, l, lv) * ekic(j + 24, l, lv)
                Next
            Next

            For lv = lft To llt

                'ekic(j + 23, j, lv) = Math.Sqrt(ekic(j + 23, j, lv) - temp(lv))    ' YC 102418
                'temp2(lv) = 1.0 / ekic(j + 23, j, lv)
                ekic(j + 24, j, lv) = Math.Sqrt(ekic(j + 24, j, lv) - temp(lv))
                temp2(lv) = 1.0 / ekic(j + 24, j, lv)
            Next
            '.......... eventually just store reciprocal of diagonal entries

            '....... reduce remaining coefficients in column

            'For i = j + 22 To 0 Step -1  ' YC 102418
            For i = j + 23 To 1 Step -1
                For lv = lft To llt
                    temp(lv) = 0.0
                Next

                'For l = j + 1 To 8     ' YC 102418
                For l = j + 1 To 9
                    For lv = lft To llt

                        'temp(lv) = temp(lv) + ekic(i, l, lv) * ekic(j + 23, l, lv)     ' YC 102418
                        temp(lv) = temp(lv) + ekic(i, l, lv) * ekic(j + 24, l, lv)
                    Next
                Next

                For lv = lft To llt
                    ekic(i, j, lv) = (ekic(i, j, lv) - temp(lv)) * temp2(lv)
                Next

            Next

        Next

        '-----------------------------------------------------------------------
        '     compute Schur complement for quadrant k(cc)
        '-----------------------------------------------------------------------

        Dim jj = 300

        'For j = 23 To 0 Step -1     ' YC 102418
        For j = 24 To 1 Step -1
            '....... reduce diagonal coefficient

            For lv = lft To llt
                temp(lv) = 0.0
            Next

            'For l = 0 To 8   ' YC 102418
            For l = 1 To 9
                For lv = lft To llt
                    temp(lv) = temp(lv) + ekic(j, l, lv) * ekic(j, l, lv)
                Next
            Next

            For lv = lft To llt
                s(jj, lv) = s(jj, lv) - temp(lv)
            Next

            '....... reduce remaining coefficients in column

            ij = jj

            'For i = j To 0 Step -1    ' YC 102418
            For i = j - 1 To 1 Step -1
                ij = ij - 1
                For lv = lft To llt
                    temp(lv) = 0.0
                Next

                'For l = 0 To 8    ' YC 102418
                For l = 1 To 9
                    For lv = lft To llt
                        temp(lv) = temp(lv) + ekic(i, l, lv) * ekic(j, l, lv)
                    Next
                Next

                For lv = lft To llt
                    s(ij, lv) = s(ij, lv) - temp(lv)
                Next

            Next
            jj = jj - j
        Next
        Return

5000:
        '   ... perform LU decomposition of Kii ...

        'Call Check3D(ekic, 33, 9, 9)
        'Create copy of ekic
        Dim ind0 = ekic.GetUpperBound(0) - 24
        Dim ind1 = ekic.GetUpperBound(1)
        Dim ind2 = ekic.GetUpperBound(2)
        Dim ekic_copy(ind0, ind1, ind2) As Double
        'set values of ekic to copy of ekic
        For ii0 = 1 To ind0
            For ii1 = 1 To ind1
                For ii2 = 1 To ind2
                    ekic_copy(ii0, ii1, ii2) = ekic(ii0 + 24, ii1, ii2)
                Next
            Next
        Next

        Call ludcmp(ekic_copy, 33, 9, 9, lft, llt)

        'set values of ekic copy to ekic
        'For ii0 = 24 To ind0
        For ii0 = 1 To ind0                    ' QW 12-12-2018
            For ii1 = 1 To ind1
                For ii2 = 1 To ind2
                    ekic(ii0 + 24, ii1, ii2) = ekic_copy(ii0, ii1, ii2)
                Next
            Next
        Next
        'Call Check3D(ekic, 33, 9, 9)

        '   ... successive forward-backward substitutions develop
        '   ... K(hat) = Kcc - Kci*(Kii)-1*Kic for global solver

        '   ... loop over columns of Kcc ...

        'For j = 0 To 23    ' YC 102418
        For j = 1 To 24

            'For k = 0 To 8   ' YC 102418
            For k = 1 To 9
                For l = lft To llt
                    aj(k, l) = ekic(j, k, l)
                Next
            Next


            'set values of ekic to copy of ekic
            For ii0 = 1 To ind0                 ' QW 12-12-2018
                For ii1 = 1 To ind1
                    For ii2 = 1 To ind2
                        ekic_copy(ii0, ii1, ii2) = ekic(ii0 + 24, ii1, ii2)
                    Next
                Next
            Next

            Call lufwbk(ekic_copy, 33, 9, 9, aj, 9, lft, llt)

            'set values of ekic copy to ekic
            'For ii0 = 24 To ind0
            For ii0 = 1 To ind0                ' QW 12-12-2018
                For ii1 = 1 To ind1
                    For ii2 = 1 To ind2
                        ekic(ii0 + 24, ii1, ii2) = ekic_copy(ii0, ii1, ii2)
                    Next
                Next
            Next

            ' Call Check3D(ekic, 33, 9, 9)


            'c   ... loop over rows of Kcc (upper triangle only) ...

            'For i = 0 To j - 1     ' YC 102418
            For i = 1 To j
                ij = i + j * (j - 1) / 2

                'For k = 0 To 8     ' YC 102418
                For k = 1 To 9
                    For l = lft To llt
                        s(ij, l) = s(ij, l) - ekic(i, k, l) * aj(k, l)
                    Next
                Next

            Next
        Next
        'Call Check2DT(s, 300, 9)

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine statcn(s,ekic,lft,llt)
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c
'c===> module to perform static condensation/partial factorization
'c===> of incompatible degrees of freedom for hexahedral element
'c
'      dimension s(324,*),ekic(33,9,*),temp(64),temp2(64)
'c
'c-----------------------------------------------------------------------
'c     find U U-transpose factorization of k(ii) and k(ci) quadrants
'c-----------------------------------------------------------------------
'c
'      dimension aj(9,64)
'	common/statcn_data1/ilu
'c
'
'      if (ilu.eq.1) goto 5000
'c
'      do 100 lv=lft,llt
'      ekic(33,9,lv) = sqrt(ekic(33,9,lv))
'  100 temp(lv) = 1.0/ekic(33,9,lv)
'c
'      do 300 i=1,32
'      do 200 lv=lft,llt
'  200 ekic(i,9,lv) = temp(lv)*ekic(i,9,lv)
'  300 continue
'c
'      do 1300 j=8,1,-1
'c
'c....... reduce diagonal coefficient
'c
'         do 400 lv=lft,llt
'  400    temp(lv) = 0.0
'         do 600 l=j+1,9
'         do 500 lv=lft,llt
'  500    temp(lv) = temp(lv) + ekic(j+24,l,lv)*ekic(j+24,l,lv)
'  600    continue
'c
'         do 700 lv=lft,llt
'         ekic(j+24,j,lv) = sqrt(ekic(j+24,j,lv) - temp(lv))
'  700    temp2(lv) = 1.0/ekic(j+24,j,lv)
'c.......... eventually just store reciprocal of diagonal entries
'c
'c....... reduce remaining coefficients in column
'c
'         do 1200 i=j+23,1,-1
'c
'         do 800 lv=lft,llt
'  800    temp(lv) = 0.0
'         do 1000 l=j+1,9
'         do 900 lv=lft,llt
'  900    temp(lv) = temp(lv) + ekic(i,l,lv)*ekic(j+24,l,lv)
' 1000    continue
'c
'         do 1100 lv=lft,llt
' 1100    ekic(i,j,lv) = (ekic(i,j,lv) - temp(lv))*temp2(lv)
'c
' 1200    continue
'c
' 1300 continue
'c
'c-----------------------------------------------------------------------
'c     compute Schur complement for quadrant k(cc)
'c-----------------------------------------------------------------------
'c
'      jj = 300
'      do 2300 j=24,1,-1
'c
'c....... reduce diagonal coefficient
'c
'         do 1400 lv=lft,llt
' 1400    temp(lv) = 0.0
'         do 1600 l=1,9
'         do 1500 lv=lft,llt
' 1500    temp(lv) = temp(lv) + ekic(j,l,lv)*ekic(j,l,lv)
' 1600    continue
'c
'         do 1700 lv=lft,llt
' 1700    s(jj,lv) = s(jj,lv) - temp(lv)
'c
'c....... reduce remaining coefficients in column
'c
'         ij = jj
'         do 2200 i=j-1,1,-1
'         ij = ij - 1
'         do 1800 lv=lft,llt
' 1800    temp(lv) = 0.0
'         do 2000 l=1,9
'         do 1900 lv=lft,llt
' 1900    temp(lv) = temp(lv) + ekic(i,l,lv)*ekic(j,l,lv)
' 2000    continue
'c
'         do 2100 lv=lft,llt
' 2100    s(ij,lv) = s(ij,lv) - temp(lv)
'c
' 2200    continue
'         jj = jj - j
' 2300 continue
'      return
'c
' 5000 continue
'c   ... perform LU decomposition of Kii ...
'      call ludcmp(ekic(25,1,1),33,9,9,lft,llt)
'c
'c   ... successive forward-backward substitutions develop
'c   ... K(hat) = Kcc - Kci*(Kii)-1*Kic for global solver
'c
'c   ... loop over columns of Kcc ...
'      do 5060 j=1,24
'c
'      do 5020 k=1,9
'      do 5010 l=lft,llt
'      aj(k,l)=ekic(j,k,l)
' 5010 continue
' 5020 continue
'c
'      call lufwbk(ekic(25,1,1),33,9,9,aj,9,lft,llt)
'c
'c   ... loop over rows of Kcc (upper triangle only) ...
'      do 5050 i=1,j
'      ij=i+j*(j-1)/2
'c
'      do 5040 k=1,9
'      do 5030 l=lft,llt
'      s(ij,l)=s(ij,l)-ekic(i,k,l)*aj(k,l)
' 5030 continue
' 5040 continue
'c
' 5050 continue
' 5060 continue
'      return
'      end

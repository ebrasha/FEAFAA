'This file contains all the methods of moveii.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to move factored k(ii) quadrant to global storage
    ''' </summary>
    ''' <param name="sic"></param>
    ''' <param name="ekic"></param>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    Public Sub moveii(ByRef sic(,,) As Double, ByRef ekic(,,) As Double, ByRef lft As Integer, ByRef llt As Integer)

        Dim i As Integer ' YC 102418


        'For i = 24 To 32   ' YC 102418
        '    For j = 0 To 8
        For i = 25 To 33
            For j = 1 To 9
                For lv = lft To llt
                    sic(i, j, lv) = ekic(i, j, lv)
                Next
            Next
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine moveii(sic,ekic,lft,llt)
'c
'
'      implicit double precision (a-h,o-z)                             
'
'c
'c===> module to move factored k(ii) quadrant to global storage
'c
'      dimension sic(33,9,*),ekic(33,9,*)
'c
'
'      do 200 i=25,33
'      do 200 j=1,9
'      do 100 lv=lft,llt
'  100 sic(i,j,lv) = ekic(i,j,lv)
'  200 continue
'c
'      return
'      end

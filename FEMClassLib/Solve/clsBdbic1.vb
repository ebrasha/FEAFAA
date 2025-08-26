'This file contains all the methods of bdbic1.f
Partial Public Class clsSolve


    'Public irow(9), icol(9), igrad(9) As Integer     



    ''' <summary>
    ''' compute upper profile coefficients for the compatible-incompatible quadrant of the hexahedral stiffness matrix
    ''' </summary>
    ''' <param name="ps"></param>
    ''' <param name="pi"></param>
    ''' <param name="d"></param>
    ''' <param name="ekic"></param>
    ''' <param name="nmel"></param>
    Public Sub bdbic1(ByRef ps(,,) As Double, ByRef pi(,,) As Double, ByRef d(,,) As Double,
                      ByRef ekic(,,) As Double, ByRef nmel As Integer)

        Dim jrow, krow, jcol, kcol, jgrad, kgrad, jbig, kbig As Integer
        Dim i As Integer ' YC 092018



        'For jcnt = 0 To 8      ' YC 102418
        '    For kcnt = 0 To 8
        For jcnt = 1 To 9
            For kcnt = 1 To 9
                jrow = irow(jcnt)
                krow = irow(kcnt)
                jcol = icol(jcnt)
                kcol = icol(kcnt)
                jgrad = igrad(jcnt)
                kgrad = igrad(kcnt)

                'For jsmall = 0 To 2    ' YC 102418
                '    For ksmall = 0 To 7
                For jsmall = 1 To 3
                    For ksmall = 1 To 8

                        'jbig = jsmall * 3 + jcol - 1                               ' YC 102418
                        'kbig = ksmall * 3 + kcol - 1
                        jbig = (jsmall - 1) * 3 + jcol
                        kbig = (ksmall - 1) * 3 + kcol
                        For i = lft To llt
                            ekic(kbig, jbig, i) = ekic(kbig, jbig, i) + pi(i, jsmall, jgrad) * d(jrow, krow, i) * ps(i, ksmall, kgrad)
                        Next

                    Next
                Next
            Next
        Next

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine bdbic1(ps,pi,d,ekic,nmel)
'c
'
'      implicit double precision (a-h,o-z)                           
'
'c
'c===> module to compute upper profile coefficients for the compatible-
'c     incompatible quadrant of the hexahedral stiffness matrix
'c
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect53/irow(9),icol(9),igrad(9)
'      dimension ps(nmel,8,3),pi(nmel,3,3),d(6,6,*),ekic(33,9,*)
'c
'
'      do 10 jcnt=1,9
'      do 10 kcnt=1,9
'      jrow=irow(jcnt)
'      krow=irow(kcnt)
'      jcol=icol(jcnt)
'      kcol=icol(kcnt)
'      jgrad=igrad(jcnt)
'      kgrad=igrad(kcnt)
'      do 20 jsmall=1,3
'      do 20 ksmall=1,8
'      jbig=(jsmall-1)*3+jcol
'      kbig=(ksmall-1)*3+kcol
'      do 30 i=lft,llt
'      ekic(kbig,jbig,i)=ekic(kbig,jbig,i)
'     &   +pi(i,jsmall,jgrad)*d(jrow,krow,i)*ps(i,ksmall,kgrad)
'   30 continue
'   20 continue
'   10 continue
'c
'      return
'      end

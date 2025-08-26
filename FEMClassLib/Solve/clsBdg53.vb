'This file contains all the methods of bdg53.f
Partial Public Class clsSolve

    ''' <summary>
    ''' to assemble 5 node, 15 dof element into block diagonal
    ''' </summary>
    ''' <param name="diag"></param>
    ''' <param name="s"></param>
    ''' <param name="ien"></param>
    Public Sub bdg53(ByRef diag(,) As Double, ByRef s() As Double, ByRef ien() As Integer)



        ' YC 102418
        'diag(0, ien(0)) = diag(0, ien(0)) + s(0)
        'diag(1, ien(0)) = diag(1, ien(0)) + s(1)
        'diag(2, ien(0)) = diag(2, ien(0)) + s(2)
        'diag(3, ien(0)) = diag(3, ien(0)) + s(3)
        'diag(4, ien(0)) = diag(4, ien(0)) + s(4)
        'diag(5, ien(0)) = diag(5, ien(0)) + s(5)

        'diag(0, ien(1)) = diag(0, ien(1)) + s(9)
        'diag(1, ien(1)) = diag(1, ien(1)) + s(13)
        'diag(2, ien(1)) = diag(2, ien(1)) + s(14)
        'diag(3, ien(1)) = diag(3, ien(1)) + s(18)
        'diag(4, ien(1)) = diag(4, ien(1)) + s(19)
        'diag(5, ien(1)) = diag(5, ien(1)) + s(20)

        'diag(0, ien(2)) = diag(0, ien(2)) + s(27)
        'diag(1, ien(2)) = diag(1, ien(2)) + s(34)
        'diag(2, ien(2)) = diag(2, ien(2)) + s(35)
        'diag(3, ien(2)) = diag(3, ien(2)) + s(42)
        'diag(4, ien(2)) = diag(4, ien(2)) + s(43)
        'diag(5, ien(2)) = diag(5, ien(2)) + s(44)

        'diag(0, ien(3)) = diag(0, ien(3)) + s(54)
        'diag(1, ien(3)) = diag(1, ien(3)) + s(64)
        'diag(2, ien(3)) = diag(2, ien(3)) + s(65)
        'diag(3, ien(3)) = diag(3, ien(3)) + s(75)
        'diag(4, ien(3)) = diag(4, ien(3)) + s(76)
        'diag(5, ien(3)) = diag(5, ien(3)) + s(77)

        'diag(0, ien(4)) = diag(0, ien(4)) + s(90)
        'diag(1, ien(4)) = diag(1, ien(4)) + s(103)
        'diag(2, ien(4)) = diag(2, ien(4)) + s(104)
        'diag(3, ien(4)) = diag(3, ien(4)) + s(117)
        'diag(4, ien(4)) = diag(4, ien(4)) + s(118)
        'diag(5, ien(4)) = diag(5, ien(4)) + s(119)
        diag(1, ien(1)) = diag(1, ien(1)) + s(1)
        diag(2, ien(1)) = diag(2, ien(1)) + s(2)
        diag(3, ien(1)) = diag(3, ien(1)) + s(3)
        diag(4, ien(1)) = diag(4, ien(1)) + s(4)
        diag(5, ien(1)) = diag(5, ien(1)) + s(5)
        diag(6, ien(1)) = diag(6, ien(1)) + s(6)

        diag(1, ien(2)) = diag(1, ien(2)) + s(10)
        diag(2, ien(2)) = diag(2, ien(2)) + s(14)
        diag(3, ien(2)) = diag(3, ien(2)) + s(15)
        diag(4, ien(2)) = diag(4, ien(2)) + s(19)
        diag(5, ien(2)) = diag(5, ien(2)) + s(20)
        diag(6, ien(2)) = diag(6, ien(2)) + s(21)

        diag(1, ien(3)) = diag(1, ien(3)) + s(28)
        diag(2, ien(3)) = diag(2, ien(3)) + s(35)
        diag(3, ien(3)) = diag(3, ien(3)) + s(36)
        diag(4, ien(3)) = diag(4, ien(3)) + s(43)
        diag(5, ien(3)) = diag(5, ien(3)) + s(44)
        diag(6, ien(3)) = diag(6, ien(3)) + s(45)

        diag(1, ien(4)) = diag(1, ien(4)) + s(55)
        diag(2, ien(4)) = diag(2, ien(4)) + s(65)
        diag(3, ien(4)) = diag(3, ien(4)) + s(66)
        diag(4, ien(4)) = diag(4, ien(4)) + s(76)
        diag(5, ien(4)) = diag(5, ien(4)) + s(77)
        diag(6, ien(4)) = diag(6, ien(4)) + s(78)

        diag(1, ien(5)) = diag(1, ien(5)) + s(91)
        diag(2, ien(5)) = diag(2, ien(5)) + s(104)
        diag(3, ien(5)) = diag(3, ien(5)) + s(105)
        diag(4, ien(5)) = diag(4, ien(5)) + s(118)
        diag(5, ien(5)) = diag(5, ien(5)) + s(119)
        diag(6, ien(5)) = diag(6, ien(5)) + s(120)
        ' YC 102418 END


    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine bdg53(diag,s,ien)                                    
'c
'
'      implicit double precision (a-h,o-z)                             
'c                                                                     
'c===> module to assemble 5 node, 15 dof element into block diagonal   
'c                                                                     
'      dimension diag(21,*),s(120),ien(*)                              
'c                                                                     
'
'         diag(1,ien(1)) = diag(1,ien(1)) + s(  1)                     
'         diag(2,ien(1)) = diag(2,ien(1)) + s(  2)                     
'         diag(3,ien(1)) = diag(3,ien(1)) + s(  3)                     
'         diag(4,ien(1)) = diag(4,ien(1)) + s(  4)                     
'         diag(5,ien(1)) = diag(5,ien(1)) + s(  5)                     
'         diag(6,ien(1)) = diag(6,ien(1)) + s(  6)                     
'c                                                                     
'         diag(1,ien(2)) = diag(1,ien(2)) + s( 10)                     
'         diag(2,ien(2)) = diag(2,ien(2)) + s( 14)                     
'         diag(3,ien(2)) = diag(3,ien(2)) + s( 15)                     
'         diag(4,ien(2)) = diag(4,ien(2)) + s( 19)                     
'         diag(5,ien(2)) = diag(5,ien(2)) + s( 20)                     
'         diag(6,ien(2)) = diag(6,ien(2)) + s( 21)                     
'c                                                                     
'         diag(1,ien(3)) = diag(1,ien(3)) + s( 28)                     
'         diag(2,ien(3)) = diag(2,ien(3)) + s( 35)                     
'         diag(3,ien(3)) = diag(3,ien(3)) + s( 36)                     
'         diag(4,ien(3)) = diag(4,ien(3)) + s( 43)                     
'         diag(5,ien(3)) = diag(5,ien(3)) + s( 44)                     
'         diag(6,ien(3)) = diag(6,ien(3)) + s( 45)                     
'c                                                                     
'         diag(1,ien(4)) = diag(1,ien(4)) + s( 55)                     
'         diag(2,ien(4)) = diag(2,ien(4)) + s( 65)                     
'         diag(3,ien(4)) = diag(3,ien(4)) + s( 66)                     
'         diag(4,ien(4)) = diag(4,ien(4)) + s( 76)                     
'         diag(5,ien(4)) = diag(5,ien(4)) + s( 77)                     
'         diag(6,ien(4)) = diag(6,ien(4)) + s( 78)                     
'c                                                                     
'         diag(1,ien(5)) = diag(1,ien(5)) + s( 91)                     
'         diag(2,ien(5)) = diag(2,ien(5)) + s(104)                     
'         diag(3,ien(5)) = diag(3,ien(5)) + s(105)                     
'         diag(4,ien(5)) = diag(4,ien(5)) + s(118)                     
'         diag(5,ien(5)) = diag(5,ien(5)) + s(119)                     
'         diag(6,ien(5)) = diag(6,ien(5)) + s(120)                     
'c                                                                     
'      return                                                          
'      end                                                             

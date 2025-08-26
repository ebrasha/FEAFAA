'This file contains all the methods of symm33.f
Partial Public Class clsSolve


    Private t1(64), t2(64), t3(64), t7(64), t8(64), t9(64) As Double    ' YC 092018

    ''' <summary>
    ''' to block-diagonal transform symmetric 3 x 3 el submatrix
    ''' </summary>
    ''' <param name="lft"></param>
    ''' <param name="llt"></param>
    ''' <param name="s"></param>
    ''' <param name="dgj"></param>
    Public Sub symm33(ByRef lft As Integer, ByRef llt As Integer, ByRef s(,) As Double, ByRef dgj(,) As Double)

        ' YC 102418
        'For l = lft - 1 To llt - 1
        '    t1(l) = dgj(l, 0) * s(l, 0)
        '    t2(l) = dgj(l, 0) * s(l, 1)
        '    t3(l) = dgj(l, 0) * s(l, 3)
        'Next

        'For l = lft - 1 To llt - 1
        '    t4(l) = dgj(l, 2) * (s(l, 1) - t1(l) * dgj(l, 1))
        '    t5(l) = dgj(l, 2) * (s(l, 2) - t2(l) * dgj(l, 1))
        '    t6(l) = dgj(l, 2) * (s(l, 4) - t3(l) * dgj(l, 1))
        'Next

        'For l = lft - 1 To llt - 1
        '    t7(l) = dgj(l, 5) * (s(l, 3) - t1(l) * dgj(l, 3) - t4(l) * dgj(l, 4))
        '    t8(l) = dgj(l, 5) * (s(l, 4) - t2(l) * dgj(l, 3) - t5(l) * dgj(l, 4))
        '    t9(l) = dgj(l, 5) * (s(l, 5) - t3(l) * dgj(l, 3) - t6(l) * dgj(l, 4))
        'Next

        'For l = lft - 1 To llt - 1
        '    s(l, 0) = dgj(l, 0) * t1(l)
        '    s(l, 1) = dgj(l, 0) * t4(l)
        '    s(l, 3) = dgj(l, 0) * t7(l)
        'Next

        'For l = lft - 1 To llt - 1
        '    s(l, 2) = dgj(l, 2) * (t5(l) - s(l, 1) * dgj(l, 1))
        '    s(l, 4) = dgj(l, 2) * (t8(l) - s(l, 3) * dgj(l, 1))
        'Next

        'For l = lft - 1 To llt - 1
        '    s(l, 5) = dgj(l, 5) * (t9(l) - s(l, 3) * dgj(l, 3) - s(l, 4) * dgj(l, 4))
        'Next
        For l = lft To llt
            t1(l) = dgj(l, 1) * s(l, 1)
            t2(l) = dgj(l, 1) * s(l, 2)
            t3(l) = dgj(l, 1) * s(l, 4)
        Next

        For l = lft To llt
            t4(l) = dgj(l, 3) * (s(l, 2) - t1(l) * dgj(l, 2))
            t5(l) = dgj(l, 3) * (s(l, 3) - t2(l) * dgj(l, 2))
            t6(l) = dgj(l, 3) * (s(l, 5) - t3(l) * dgj(l, 2))
        Next

        For l = lft To llt
            t7(l) = dgj(l, 6) * (s(l, 4) - t1(l) * dgj(l, 4) - t4(l) * dgj(l, 5))
            t8(l) = dgj(l, 6) * (s(l, 5) - t2(l) * dgj(l, 4) - t5(l) * dgj(l, 5))
            t9(l) = dgj(l, 6) * (s(l, 6) - t3(l) * dgj(l, 4) - t6(l) * dgj(l, 5))
        Next

        For l = lft To llt
            s(l, 1) = dgj(l, 1) * t1(l)
            s(l, 2) = dgj(l, 1) * t4(l)
            s(l, 4) = dgj(l, 1) * t7(l)
        Next

        For l = lft To llt
            s(l, 3) = dgj(l, 3) * (t5(l) - s(l, 2) * dgj(l, 2))
            s(l, 5) = dgj(l, 3) * (t8(l) - s(l, 4) * dgj(l, 2))
        Next

        For l = lft To llt
            s(l, 6) = dgj(l, 6) * (t9(l) - s(l, 4) * dgj(l, 4) - s(l, 5) * dgj(l, 5))
        Next
        ' YC 102418 END

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine symm33(lft,llt,s,dgj)                                  
'c
'
'      implicit double precision (a-h,o-z)                              
'
'c                                                                       
'c===> module to block-diagonal transform symmetric 3 x 3 el submatrix   
'c                                                                       
'      dimension s(65,6),dgj(65,6)                                       
'      common /vect2/ t1(64),t2(64),t3(64),t4(64),t5(64),t6(64),         
'     &               t7(64),t8(64),t9(64)                               
'c                                                                       
'      do 100 l=lft,llt                                                  
'         t1(l) = dgj(l,1)*s(l,1)                                        
'         t2(l) = dgj(l,1)*s(l,2)                                        
'         t3(l) = dgj(l,1)*s(l,4)                                        
'  100 continue                                                          
'c                                                                       
'      do 200 l=lft,llt                                                  
'         t4(l) = dgj(l,3)*(s(l,2) - t1(l)*dgj(l,2))                     
'         t5(l) = dgj(l,3)*(s(l,3) - t2(l)*dgj(l,2))                     
'         t6(l) = dgj(l,3)*(s(l,5) - t3(l)*dgj(l,2))                     
'  200 continue                                                          
'c                                                                       
'      do 300 l=lft,llt                                                  
'         t7(l) = dgj(l,6)*(s(l,4) - t1(l)*dgj(l,4) - t4(l)*dgj(l,5))    
'         t8(l) = dgj(l,6)*(s(l,5) - t2(l)*dgj(l,4) - t5(l)*dgj(l,5))    
'         t9(l) = dgj(l,6)*(s(l,6) - t3(l)*dgj(l,4) - t6(l)*dgj(l,5))    
'  300 continue                                                          
'c                                                                       
'      do 400 l=lft,llt                                                  
'         s(l,1) = dgj(l,1)*t1(l)                                        
'         s(l,2) = dgj(l,1)*t4(l)                                        
'         s(l,4) = dgj(l,1)*t7(l)                                        
'  400 continue                                                          
'c                                                                       
'      do 500 l=lft,llt                                                  
'         s(l,3) = dgj(l,3)*(t5(l) - s(l,2)*dgj(l,2))                    
'         s(l,5) = dgj(l,3)*(t8(l) - s(l,4)*dgj(l,2))                    
'  500 continue                                                          
'c                                                                       
'      do 600 l=lft,llt                                                  
'         s(l,6) = dgj(l,6)*(t9(l) - s(l,4)*dgj(l,4)                     
'     &          - s(l,5)*dgj(l,5))                                      
'  600 continue                                                          
'c                                                                       
'      return                                                            
'      end                                                               

'This file contains all the methods Of unpci.f
Partial Public Class clsSolve


    Private mxt(64) As Integer          


    Public Sub unpci(ByRef ixp(,) As Integer, ind1 As Integer, ind2 As Integer)

        Dim i As Integer ' YC 102418

        For i = lft To llt

            ' YC 102418
            'mxt(i) = ixp(0 + ind1, i + ind2)
            'ix1(i) = ixp(1 + ind1, i + ind2)
            'ix2(i) = ixp(2 + ind1, i + ind2)
            'ix3(i) = ixp(3 + ind1, i + ind2)
            'ix4(i) = ixp(4 + ind1, i + ind2)
            'ix5(i) = ixp(5 + ind1, i + ind2)
            'ix6(i) = ixp(6 + ind1, i + ind2)
            'ix7(i) = ixp(7 + ind1, i + ind2)
            'ix8(i) = ixp(8 + ind1, i + ind2)
            mxt(i) = ixp(1 + ind1, i + ind2)
            ix1(i) = ixp(2 + ind1, i + ind2)
            ix2(i) = ixp(3 + ind1, i + ind2)
            ix3(i) = ixp(4 + ind1, i + ind2)
            ix4(i) = ixp(5 + ind1, i + ind2)
            ix5(i) = ixp(6 + ind1, i + ind2)
            ix6(i) = ixp(7 + ind1, i + ind2)
            ix7(i) = ixp(8 + ind1, i + ind2)
            ix8(i) = ixp(9 + ind1, i + ind2)
            ' YC 102418 END

        Next

        matp = mxt 'from common/vect1/, added by YC 102418 

    End Sub
    ''' <summary>
    ''' unpack connection data
    ''' </summary>
    ''' <param name="ixp"></param>
    ''' 

    'suppressed by YC 102418
    'Public Sub unpci(ByRef ixp(,) As Integer)
    '    For i = lft To llt
    '        mxt(i) = ixp(0, i)
    '        ix1(i) = ixp(1, i)
    '        ix2(i) = ixp(2, i)
    '        ix3(i) = ixp(3, i)
    '        ix4(i) = ixp(4, i)
    '        ix5(i) = ixp(5, i)
    '        ix6(i) = ixp(6, i)
    '        ix7(i) = ixp(7, i)
    '        ix8(i) = ixp(8, i)
    '    Next
    'End Sub
    'suppressed by YC 102418


End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine unpci  (ixp)                                          
'c
'      implicit double precision (a-h,o-z)                               
'c
'c     unpack connection data
'c
'      common/vect1/                                                    
'     1 rx1(64),ry1(64),rz1(64),rx2(64),ry2(64),rz2(64),                
'     2 rx3(64),ry3(64),rz3(64),rx4(64),ry4(64),rz4(64),                
'     3 rx5(64),ry5(64),rz5(64),rx6(64),ry6(64),rz6(64),                
'     4 rx7(64),ry7(64),rz7(64),rx8(64),ry8(64),rz8(64),                
'     5 mtype(64),mxt(64)                                               
'      common/vect4/                                                    
'     1 ix1(64),ix2(64),ix3(64),ix4(64),ix5(64),ix6(64),ix7(64),ix8(64),
'     & dum4(16,64)                                                     
'      common/vect13/                                                   
'     1 kka(64),kkb(64),kkc(64),kk1(64),kk2(64),kk3(64),dum13(3456)     
'      common/range/mft,mlt,lft,llt,nftm1                               
'      dimension  ixp(9,*)                                              
'c
'      do 10 i=lft,llt                                                  
'      mxt(i)=ixp(1,i)                                                  
'      ix1(i)=ixp(2,i)                                                  
'      ix2(i)=ixp(3,i)                                                  
'      ix3(i)=ixp(4,i)                                                  
'      ix4(i)=ixp(5,i)                                                  
'      ix5(i)=ixp(6,i)                                                  
'      ix6(i)=ixp(7,i)                                                  
'      ix7(i)=ixp(8,i)                                                  
'   10 ix8(i)=ixp(9,i)                                                  
'c
'      return                                                           
'      end                                                              

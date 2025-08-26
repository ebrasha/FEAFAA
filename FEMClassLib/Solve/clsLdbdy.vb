'This file contains all the methods of ldbdy.f
Partial Public Class clsSolve

    
    Public nthpy, nthpz, nthpx, nthsx, nthsy, nthsz As Integer
    Public gax, gay, gaz, slx, sly, slz As Double

    Private gx1(64), gy1(64), gz1(64), gx2(64), gy2(64), gz2(64),
     gx3(64), gy3(64), gz3(64), gx4(64), gy4(64), gz4(64),
     gx5(64), gy5(64), gz5(64), gx6(64), gy6(64), gz6(64),
     gx7(64), gy7(64), gz7(64), gx8(64), gy8(64), gz8(64) As Double  ' common/vect6 g11 to g38 in stfic.f




    ''' <summary>
    ''' to add body force loads to hexahedral element residual
    ''' </summary>
    ''' <param name="fval"></param>
    Public Sub ldbdy(ByRef fval() As Double)

        Dim facx, facy, facz, facsx, facsy, facsz As Double
        Dim i As Integer
        If nthpy + nthpz + nthpx + nthsx + nthsy + nthsz = 0 Then Return

        If nthpy <> 0 Then
            facy = fval(nthpy) * gay
            For i = lft To llt

                ' YC 102418
                'ry1(i) = ry1(i) + facy * ym(0, i)
                'ry2(i) = ry2(i) + facy * ym(1, i)
                'ry3(i) = ry3(i) + facy * ym(2, i)
                'ry4(i) = ry4(i) + facy * ym(3, i)
                'ry5(i) = ry5(i) + facy * ym(4, i)
                'ry6(i) = ry6(i) + facy * ym(5, i)
                'ry7(i) = ry7(i) + facy * ym(6, i)
                'ry8(i) = ry8(i) + facy * ym(7, i)
                ry1(i) = ry1(i) + facy * ym(1, i)
                ry2(i) = ry2(i) + facy * ym(2, i)
                ry3(i) = ry3(i) + facy * ym(3, i)
                ry4(i) = ry4(i) + facy * ym(4, i)
                ry5(i) = ry5(i) + facy * ym(5, i)
                ry6(i) = ry6(i) + facy * ym(6, i)
                ry7(i) = ry7(i) + facy * ym(7, i)
                ry8(i) = ry8(i) + facy * ym(8, i)
                ' YC 102418 END

            Next
        End If

        If nthpz <> 0 Then
            '	Assume body force always exists for each time step, set fval(nthpz)=1.     
            facz = gaz
            For i = lft To llt

                ' YC 102418
                'rz1(i) = rz1(i) + facz * ym(0, i)
                'rz2(i) = rz2(i) + facz * ym(1, i)
                'rz3(i) = rz3(i) + facz * ym(2, i)
                'rz4(i) = rz4(i) + facz * ym(3, i)
                'rz5(i) = rz5(i) + facz * ym(4, i)
                'rz6(i) = rz6(i) + facz * ym(5, i)
                'rz7(i) = rz7(i) + facz * ym(6, i)
                'rz8(i) = rz8(i) + facz * ym(7, i)
                rz1(i) = rz1(i) + facz * ym(1, i)
                rz2(i) = rz2(i) + facz * ym(2, i)
                rz3(i) = rz3(i) + facz * ym(3, i)
                rz4(i) = rz4(i) + facz * ym(4, i)
                rz5(i) = rz5(i) + facz * ym(5, i)
                rz6(i) = rz6(i) + facz * ym(6, i)
                rz7(i) = rz7(i) + facz * ym(7, i)
                rz8(i) = rz8(i) + facz * ym(8, i)
                ' YC 102418 END

            Next
        End If

        If nthpx <> 0 Then
            facx = fval(nthpx) * gax
            For i = lft To llt

                ' YC 102418
                'rx1(i) = rx1(i) + facx * ym(0, i)
                'rx2(i) = rx2(i) + facx * ym(1, i)
                'rx3(i) = rx3(i) + facx * ym(2, i)
                'rx4(i) = rx4(i) + facx * ym(3, i)
                'rx5(i) = rx5(i) + facx * ym(4, i)
                'rx6(i) = rx6(i) + facx * ym(5, i)
                'rx7(i) = rx7(i) + facx * ym(6, i)
                'rx8(i) = rx8(i) + facx * ym(7, i)
                rx1(i) = rx1(i) + facx * ym(1, i)
                rx2(i) = rx2(i) + facx * ym(2, i)
                rx3(i) = rx3(i) + facx * ym(3, i)
                rx4(i) = rx4(i) + facx * ym(4, i)
                rx5(i) = rx5(i) + facx * ym(5, i)
                rx6(i) = rx6(i) + facx * ym(6, i)
                rx7(i) = rx7(i) + facx * ym(7, i)
                rx8(i) = rx8(i) + facx * ym(8, i)
                ' YC 102418 END

            Next
        End If

        If nthsx <> 0 Then
            facsx = Math.Pow((fval(nthsx) * slx), 2)
            If facsx <> 0 Then
                For i = lft To llt

                    ' YC 102418 END
                    'ry1(i) = ry1(i) - facsx * gy1(i) * ym(0, i)
                    'ry2(i) = ry2(i) - facsx * gy2(i) * ym(1, i)
                    'ry3(i) = ry3(i) - facsx * gy3(i) * ym(2, i)
                    'ry4(i) = ry4(i) - facsx * gy4(i) * ym(3, i)
                    'ry5(i) = ry5(i) - facsx * gy5(i) * ym(4, i)
                    'ry6(i) = ry6(i) - facsx * gy6(i) * ym(5, i)
                    'ry7(i) = ry7(i) - facsx * gy7(i) * ym(6, i)
                    'ry8(i) = ry8(i) - facsx * gy8(i) * ym(7, i)

                    'rz1(i) = rz1(i) - facsx * gz1(i) * ym(0, i)
                    'rz2(i) = rz2(i) - facsx * gz2(i) * ym(1, i)
                    'rz3(i) = rz3(i) - facsx * gz3(i) * ym(2, i)
                    'rz4(i) = rz4(i) - facsx * gz4(i) * ym(3, i)
                    'rz5(i) = rz5(i) - facsx * gz5(i) * ym(4, i)
                    'rz6(i) = rz6(i) - facsx * gz6(i) * ym(5, i)
                    'rz7(i) = rz7(i) - facsx * gz7(i) * ym(6, i)
                    'rz8(i) = rz8(i) - facsx * gz8(i) * ym(7, i)
                    ry1(i) = ry1(i) - facsx * gy1(i) * ym(1, i)
                    ry2(i) = ry2(i) - facsx * gy2(i) * ym(2, i)
                    ry3(i) = ry3(i) - facsx * gy3(i) * ym(3, i)
                    ry4(i) = ry4(i) - facsx * gy4(i) * ym(4, i)
                    ry5(i) = ry5(i) - facsx * gy5(i) * ym(5, i)
                    ry6(i) = ry6(i) - facsx * gy6(i) * ym(6, i)
                    ry7(i) = ry7(i) - facsx * gy7(i) * ym(7, i)
                    ry8(i) = ry8(i) - facsx * gy8(i) * ym(8, i)
                    rz1(i) = rz1(i) - facsx * gz1(i) * ym(1, i)
                    rz2(i) = rz2(i) - facsx * gz2(i) * ym(2, i)
                    rz3(i) = rz3(i) - facsx * gz3(i) * ym(3, i)
                    rz4(i) = rz4(i) - facsx * gz4(i) * ym(4, i)
                    rz5(i) = rz5(i) - facsx * gz5(i) * ym(5, i)
                    rz6(i) = rz6(i) - facsx * gz6(i) * ym(6, i)
                    rz7(i) = rz7(i) - facsx * gz7(i) * ym(7, i)
                    rz8(i) = rz8(i) - facsx * gz8(i) * ym(8, i)
                    ' YC 102418 END

                Next
            End If
        End If

        If nthsy <> 0 Then
            facsy = Math.Pow((fval(nthsy) * sly), 2)
            If facsy <> 0 Then
                For i = lft To llt

                    ' YC 102418
                    'rx1(i) = rx1(i) - facsy * gx1(i) * ym(0, i)
                    'rx2(i) = rx2(i) - facsy * gx2(i) * ym(1, i)
                    'rx3(i) = rx3(i) - facsy * gx3(i) * ym(2, i)
                    'rx4(i) = rx4(i) - facsy * gx4(i) * ym(3, i)
                    'rx5(i) = rx5(i) - facsy * gx5(i) * ym(4, i)
                    'rx6(i) = rx6(i) - facsy * gx6(i) * ym(5, i)
                    'rx7(i) = rx7(i) - facsy * gx7(i) * ym(6, i)
                    'rx8(i) = rx8(i) - facsy * gx8(i) * ym(7, i)

                    'rz1(i) = rz1(i) - facsy * gz1(i) * ym(0, i)
                    'rz2(i) = rz2(i) - facsy * gz2(i) * ym(1, i)
                    'rz3(i) = rz3(i) - facsy * gz3(i) * ym(2, i)
                    'rz4(i) = rz4(i) - facsy * gz4(i) * ym(3, i)
                    'rz5(i) = rz5(i) - facsy * gz5(i) * ym(4, i)
                    'rz6(i) = rz6(i) - facsy * gz6(i) * ym(5, i)
                    'rz7(i) = rz7(i) - facsy * gz7(i) * ym(6, i)
                    'rz8(i) = rz8(i) - facsy * gz8(i) * ym(7, i)
                    rx1(i) = rx1(i) - facsy * gx1(i) * ym(1, i)
                    rx2(i) = rx2(i) - facsy * gx2(i) * ym(2, i)
                    rx3(i) = rx3(i) - facsy * gx3(i) * ym(3, i)
                    rx4(i) = rx4(i) - facsy * gx4(i) * ym(4, i)
                    rx5(i) = rx5(i) - facsy * gx5(i) * ym(5, i)
                    rx6(i) = rx6(i) - facsy * gx6(i) * ym(6, i)
                    rx7(i) = rx7(i) - facsy * gx7(i) * ym(7, i)
                    rx8(i) = rx8(i) - facsy * gx8(i) * ym(8, i)
                    rz1(i) = rz1(i) - facsy * gz1(i) * ym(1, i)
                    rz2(i) = rz2(i) - facsy * gz2(i) * ym(2, i)
                    rz3(i) = rz3(i) - facsy * gz3(i) * ym(3, i)
                    rz4(i) = rz4(i) - facsy * gz4(i) * ym(4, i)
                    rz5(i) = rz5(i) - facsy * gz5(i) * ym(5, i)
                    rz6(i) = rz6(i) - facsy * gz6(i) * ym(6, i)
                    rz7(i) = rz7(i) - facsy * gz7(i) * ym(7, i)
                    rz8(i) = rz8(i) - facsy * gz8(i) * ym(8, i)
                    ' YC 102418 END

                Next
            End If
        End If

        If nthsz <> 0 Then
            facsz = Math.Pow((fval(nthsz) * slz), 2)
            If facsz <> 0 Then
                For i = lft To llt

                    ' YC 102418
                    'rx1(i) = rx1(i) - facsz * gx1(i) * ym(0, i)
                    'rx2(i) = rx2(i) - facsz * gx2(i) * ym(1, i)
                    'rx3(i) = rx3(i) - facsz * gx3(i) * ym(2, i)
                    'rx4(i) = rx4(i) - facsz * gx4(i) * ym(3, i)
                    'rx5(i) = rx5(i) - facsz * gx5(i) * ym(4, i)
                    'rx6(i) = rx6(i) - facsz * gx6(i) * ym(5, i)
                    'rx7(i) = rx7(i) - facsz * gx7(i) * ym(6, i)
                    'rx8(i) = rx8(i) - facsz * gx8(i) * ym(7, i)

                    'ry1(i) = ry1(i) - facsz * gy1(i) * ym(0, i)
                    'ry2(i) = ry2(i) - facsz * gy2(i) * ym(1, i)
                    'ry3(i) = ry3(i) - facsz * gy3(i) * ym(2, i)
                    'ry4(i) = ry4(i) - facsz * gy4(i) * ym(3, i)
                    'ry5(i) = ry5(i) - facsz * gy5(i) * ym(4, i)
                    'ry6(i) = ry6(i) - facsz * gy6(i) * ym(5, i)
                    'ry7(i) = ry7(i) - facsz * gy7(i) * ym(6, i)
                    'ry8(i) = ry8(i) - facsz * gy8(i) * ym(7, i)
                    rx1(i) = rx1(i) - facsz * gx1(i) * ym(1, i)
                    rx2(i) = rx2(i) - facsz * gx2(i) * ym(2, i)
                    rx3(i) = rx3(i) - facsz * gx3(i) * ym(3, i)
                    rx4(i) = rx4(i) - facsz * gx4(i) * ym(4, i)
                    rx5(i) = rx5(i) - facsz * gx5(i) * ym(5, i)
                    rx6(i) = rx6(i) - facsz * gx6(i) * ym(6, i)
                    rx7(i) = rx7(i) - facsz * gx7(i) * ym(7, i)
                    rx8(i) = rx8(i) - facsz * gx8(i) * ym(8, i)
                    ry1(i) = ry1(i) - facsz * gy1(i) * ym(1, i)
                    ry2(i) = ry2(i) - facsz * gy2(i) * ym(2, i)
                    ry3(i) = ry3(i) - facsz * gy3(i) * ym(3, i)
                    ry4(i) = ry4(i) - facsz * gy4(i) * ym(4, i)
                    ry5(i) = ry5(i) - facsz * gy5(i) * ym(5, i)
                    ry6(i) = ry6(i) - facsz * gy6(i) * ym(6, i)
                    ry7(i) = ry7(i) - facsz * gy7(i) * ym(7, i)
                    ry8(i) = ry8(i) - facsz * gy8(i) * ym(8, i)
                    ' YC 102418 END

                Next
            End If
        End If

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine ldbdy (fval)
'c
'
'      implicit double precision (a-h,o-z)                             
'c
'c===> module to add body force loads to hexahedral element residual
'c
'c      
'      common/bk16/nlcur,nptst,nload,nptm
'      common/bk17/nthpx,nthpy,nthpz,nthsx,nthsy,nthsz,iacflg
'      common/bk18/gax,gay,gaz,slx,sly,slz
'      common/range/mft,mlt,lft,llt,nftm1
'      common/vect1/
'     1 rx1(64),ry1(64),rz1(64),rx2(64),ry2(64),rz2(64),
'     2 rx3(64),ry3(64),rz3(64),rx4(64),ry4(64),rz4(64),
'     3 rx5(64),ry5(64),rz5(64),rx6(64),ry6(64),rz6(64),
'     4 rx7(64),ry7(64),rz7(64),rx8(64),ry8(64),rz8(64),
'     5 mtype(64),matp(64)
'      common/vect1alpha/
'     1 rmx1(64),rmy1(64),rmz1(64),rmx2(64),rmy2(64),rmz2(64),
'     2 rmx3(64),rmy3(64),rmz3(64),rmx4(64),rmy4(64),rmz4(64),
'     3 rmx5(64),rmy5(64),rmz5(64),rmx6(64),rmy6(64),rmz6(64),
'     4 rmx7(64),rmy7(64),rmz7(64),rmx8(64),rmy8(64),rmz8(64)
'      common/vect6/
'     1 gx1(64),gy1(64),gz1(64),gx2(64),gy2(64),gz2(64),
'     2 gx3(64),gy3(64),gz3(64),gx4(64),gy4(64),gz4(64),
'     3 gx5(64),gy5(64),gz5(64),gx6(64),gy6(64),gz6(64),
'     4 gx7(64),gy7(64),gz7(64),gx8(64),gy8(64),gz8(64),
'     5 h11(64),h21(64),h31(64),h12(64),h22(64),h32(64),
'     6 h13(64),h23(64),h33(64),h14(64),h24(64),h34(64),
'     7 h15(64),h25(64),h35(64),h16(64),h26(64),h36(64),
'     8 h17(64),h27(64),h37(64),h18(64),h28(64),h38(64),
'     9 f11(64),f21(64),f31(64),f12(64),f22(64),f32(64),
'     & f13(64),f23(64),f33(64),f14(64),f24(64),f34(64),
'     & f15(64),f25(64),f35(64),f16(64),f26(64),f36(64),
'     & f17(64),f27(64),f37(64),f18(64),f28(64),f38(64)
'      common/vect9/scale(24,64),ym(8,64),ro(64),rov(64),tmecc(64)
'c
'      dimension fval(*)
'c
'
'      if(nthpy+nthpz+nthpx+nthsx+nthsy+nthsz.eq.0) return
'c
'      if (nthpy.ne.0) then
'      facy=fval(nthpy)*gay
'      do 10 i=lft,llt
'      ry1(i)=ry1(i)+facy*ym(1,i)
'      ry2(i)=ry2(i)+facy*ym(2,i)
'      ry3(i)=ry3(i)+facy*ym(3,i)
'      ry4(i)=ry4(i)+facy*ym(4,i)
'      ry5(i)=ry5(i)+facy*ym(5,i)
'      ry6(i)=ry6(i)+facy*ym(6,i)
'      ry7(i)=ry7(i)+facy*ym(7,i)
'      ry8(i)=ry8(i)+facy*ym(8,i)
'   10 continue
'      endif
'      if(nthpz.ne.0) then
'c
'c	------------------------Changed by Qiang 02-22-2012 
'c	Assume body force always exists for each time step, set fval(nthpz)=1.     
'c	facz=fval(nthpz)*gaz
'	facz=gaz
'c	---------------------------------------------------
'      do 20 i=lft,llt				
'      rz1(i)=rz1(i)+facz*ym(1,i)
'      rz2(i)=rz2(i)+facz*ym(2,i)
'      rz3(i)=rz3(i)+facz*ym(3,i)
'      rz4(i)=rz4(i)+facz*ym(4,i)
'      rz5(i)=rz5(i)+facz*ym(5,i)
'      rz6(i)=rz6(i)+facz*ym(6,i)
'      rz7(i)=rz7(i)+facz*ym(7,i)
'      rz8(i)=rz8(i)+facz*ym(8,i)
'   20 continue
'      endif
'      if(nthpx.ne.0) then
'      facx=fval(nthpx)*gax
'      do 30 i=lft,llt
'      rx1(i)=rx1(i)+facx*ym(1,i)
'      rx2(i)=rx2(i)+facx*ym(2,i)
'      rx3(i)=rx3(i)+facx*ym(3,i)
'      rx4(i)=rx4(i)+facx*ym(4,i)
'      rx5(i)=rx5(i)+facx*ym(5,i)
'      rx6(i)=rx6(i)+facx*ym(6,i)
'      rx7(i)=rx7(i)+facx*ym(7,i)
'      rx8(i)=rx8(i)+facx*ym(8,i)
'   30 continue
'      endif
'      if(nthsx.ne.0) then
'      facsx=(fval(nthsx)*slx)**2
'      if(facsx.ne.0.) then
'      do 40 i=lft,llt
'      ry1(i)=ry1(i)-facsx*gy1(i)*ym(1,i)
'      ry2(i)=ry2(i)-facsx*gy2(i)*ym(2,i)
'      ry3(i)=ry3(i)-facsx*gy3(i)*ym(3,i)
'      ry4(i)=ry4(i)-facsx*gy4(i)*ym(4,i)
'      ry5(i)=ry5(i)-facsx*gy5(i)*ym(5,i)
'      ry6(i)=ry6(i)-facsx*gy6(i)*ym(6,i)
'      ry7(i)=ry7(i)-facsx*gy7(i)*ym(7,i)
'      ry8(i)=ry8(i)-facsx*gy8(i)*ym(8,i)
'      rz1(i)=rz1(i)-facsx*gz1(i)*ym(1,i)
'      rz2(i)=rz2(i)-facsx*gz2(i)*ym(2,i)
'      rz3(i)=rz3(i)-facsx*gz3(i)*ym(3,i)
'      rz4(i)=rz4(i)-facsx*gz4(i)*ym(4,i)
'      rz5(i)=rz5(i)-facsx*gz5(i)*ym(5,i)
'      rz6(i)=rz6(i)-facsx*gz6(i)*ym(6,i)
'      rz7(i)=rz7(i)-facsx*gz7(i)*ym(7,i)
'      rz8(i)=rz8(i)-facsx*gz8(i)*ym(8,i)
'   40 continue
'      endif
'      endif
'      if(nthsy.ne.0) then
'      facsy=(fval(nthsy)*sly)**2
'      if (facsy.ne.0.) then
'      do 50 i=lft,llt
'      rx1(i)=rx1(i)-facsy*gx1(i)*ym(1,i)
'      rx2(i)=rx2(i)-facsy*gx2(i)*ym(2,i)
'      rx3(i)=rx3(i)-facsy*gx3(i)*ym(3,i)
'      rx4(i)=rx4(i)-facsy*gx4(i)*ym(4,i)
'      rx5(i)=rx5(i)-facsy*gx5(i)*ym(5,i)
'      rx6(i)=rx6(i)-facsy*gx6(i)*ym(6,i)
'      rx7(i)=rx7(i)-facsy*gx7(i)*ym(7,i)
'      rx8(i)=rx8(i)-facsy*gx8(i)*ym(8,i)
'      rz1(i)=rz1(i)-facsy*gz1(i)*ym(1,i)
'      rz2(i)=rz2(i)-facsy*gz2(i)*ym(2,i)
'      rz3(i)=rz3(i)-facsy*gz3(i)*ym(3,i)
'      rz4(i)=rz4(i)-facsy*gz4(i)*ym(4,i)
'      rz5(i)=rz5(i)-facsy*gz5(i)*ym(5,i)
'      rz6(i)=rz6(i)-facsy*gz6(i)*ym(6,i)
'      rz7(i)=rz7(i)-facsy*gz7(i)*ym(7,i)
'      rz8(i)=rz8(i)-facsy*gz8(i)*ym(8,i)
'   50 continue
'      endif
'      endif
'      if(nthsz.ne.0) then
'      facsz=(fval(nthsz)*slz)**2
'      if(facsz.ne.0.) then
'      do 60 i=lft,llt
'      rx1(i)=rx1(i)-facsz*gx1(i)*ym(1,i)
'      rx2(i)=rx2(i)-facsz*gx2(i)*ym(2,i)
'      rx3(i)=rx3(i)-facsz*gx3(i)*ym(3,i)
'      rx4(i)=rx4(i)-facsz*gx4(i)*ym(4,i)
'      rx5(i)=rx5(i)-facsz*gx5(i)*ym(5,i)
'      rx6(i)=rx6(i)-facsz*gx6(i)*ym(6,i)
'      rx7(i)=rx7(i)-facsz*gx7(i)*ym(7,i)
'      rx8(i)=rx8(i)-facsz*gx8(i)*ym(8,i)
'      ry1(i)=ry1(i)-facsz*gy1(i)*ym(1,i)
'      ry2(i)=ry2(i)-facsz*gy2(i)*ym(2,i)
'      ry3(i)=ry3(i)-facsz*gy3(i)*ym(3,i)
'      ry4(i)=ry4(i)-facsz*gy4(i)*ym(4,i)
'      ry5(i)=ry5(i)-facsz*gy5(i)*ym(5,i)
'      ry6(i)=ry6(i)-facsz*gy6(i)*ym(6,i)
'      ry7(i)=ry7(i)-facsz*gy7(i)*ym(7,i)
'      ry8(i)=ry8(i)-facsz*gy8(i)*ym(8,i)
'   60 continue
'      endif
'      endif
'
'      ! remove the else break out inertias for alpha method. QW 02-22-2017
'    
'      return
'      end

'This file contains all the methods of bdecay.f
Partial Public Class clsSolve

    Public Sub bdecay(ByRef r As Double, ByRef s As Double, ByRef t As Double,
                      ByRef ro As Double, ByRef dc() As Double, ByRef i As Integer)

        Select Case CInt(ro)

            Case 1
                Dim f2367 = 0.25 * (f12(i) + f13(i) + f16(i) + f17(i))
                Dim f1458 = 0.25 * (f11(i) + f14(i) + f15(i) + f18(i))
                Dim decay = 0.1 * Math.Log(Math.Abs((f2367 - f1458) / f1458) - 0.09) + 1
                ' Dim rr = f1458 - xl(1) + 0.5 * (f2367 - f1458) * (1 + r)   ' QW 12-12-2018-
                Dim rr = f1458 - xl(2) + 0.5 * (f2367 - f1458) * (1 + r)

                ' YC 102418
                'dc(0) = decay * Math.Pow(((f11(i) - xl(1)) / rr), 2)
                'dc(1) = decay * Math.Pow(((f12(i) - xl(1)) / rr), 2)
                'dc(2) = decay * Math.Pow(((f13(i) - xl(1)) / rr), 2)
                'dc(3) = decay * Math.Pow(((f14(i) - xl(1)) / rr), 2)
                'dc(4) = decay * Math.Pow(((f15(i) - xl(1)) / rr), 2)
                'dc(5) = decay * Math.Pow(((f16(i) - xl(1)) / rr), 2)
                'dc(6) = decay * Math.Pow(((f17(i) - xl(1)) / rr), 2)
                'dc(7) = decay * Math.Pow(((f18(i) - xl(1)) / rr), 2)
                dc(1) = decay * Math.Pow(((f11(i) - xl(2)) / rr), 2)
                dc(2) = decay * Math.Pow(((f12(i) - xl(2)) / rr), 2)
                dc(3) = decay * Math.Pow(((f13(i) - xl(2)) / rr), 2)
                dc(4) = decay * Math.Pow(((f14(i) - xl(2)) / rr), 2)
                dc(5) = decay * Math.Pow(((f15(i) - xl(2)) / rr), 2)
                dc(6) = decay * Math.Pow(((f16(i) - xl(2)) / rr), 2)
                dc(7) = decay * Math.Pow(((f17(i) - xl(2)) / rr), 2)
                dc(8) = decay * Math.Pow(((f18(i) - xl(2)) / rr), 2)
                ' YC 102418 END

            Case 2
                Dim f3478 = 0.25 * (f23(i) + f24(i) + f27(i) + f28(i))
                Dim f1256 = 0.25 * (f21(i) + f22(i) + f25(i) + f26(i))
                Dim decay = 0.1 * Math.Log(Math.Abs((f3478 - f1256) / f1256) - 0.09) + 1
                'Dim rr = f1256 - xl(3) + 0.5 * (f3478 - f1256) * (1 + s)
                Dim rr = f1256 - xl(4) + 0.5 * (f3478 - f1256) * (1 + s)

                ' YC 102418
                'dc(0) = decay * Math.Pow(((f21(i) - xl(3)) / rr), 2)
                'dc(1) = decay * Math.Pow(((f22(i) - xl(3)) / rr), 2)
                'dc(2) = decay * Math.Pow(((f23(i) - xl(3)) / rr), 2)
                'dc(3) = decay * Math.Pow(((f24(i) - xl(3)) / rr), 2)
                'dc(4) = decay * Math.Pow(((f25(i) - xl(3)) / rr), 2)
                'dc(5) = decay * Math.Pow(((f26(i) - xl(3)) / rr), 2)
                'dc(6) = decay * Math.Pow(((f27(i) - xl(3)) / rr), 2)
                'dc(7) = decay * Math.Pow(((f28(i) - xl(3)) / rr), 2)
                dc(1) = decay * Math.Pow(((f21(i) - xl(4)) / rr), 2)
                dc(2) = decay * Math.Pow(((f22(i) - xl(4)) / rr), 2)
                dc(3) = decay * Math.Pow(((f23(i) - xl(4)) / rr), 2)
                dc(4) = decay * Math.Pow(((f24(i) - xl(4)) / rr), 2)
                dc(5) = decay * Math.Pow(((f25(i) - xl(4)) / rr), 2)
                dc(6) = decay * Math.Pow(((f26(i) - xl(4)) / rr), 2)
                dc(7) = decay * Math.Pow(((f27(i) - xl(4)) / rr), 2)
                dc(8) = decay * Math.Pow(((f28(i) - xl(4)) / rr), 2)
                ' YC 102418 END

            Case 3
                Dim f1234 = 0.25 * (f31(i) + f32(i) + f33(i) + f34(i))
                Dim f5678 = 0.25 * (f35(i) + f36(i) + f37(i) + f38(i))
                Dim decay = 0.1 * Math.Log(Math.Abs((f5678 - f1234) / f1234) - 0.09) + 1
                'Dim rr = f1234 - xl(5) + 0.5 * (f5678 - f1234) * (1 + t)
                Dim rr = f1234 - xl(6) + 0.5 * (f5678 - f1234) * (1 + t)

                ' YC 102418
                'dc(0) = decay * Math.Pow(((f31(i) - xl(5)) / rr), 2)
                'dc(1) = decay * Math.Pow(((f32(i) - xl(5)) / rr), 2)
                'dc(2) = decay * Math.Pow(((f33(i) - xl(5)) / rr), 2)
                'dc(3) = decay * Math.Pow(((f34(i) - xl(5)) / rr), 2)
                'dc(4) = decay * Math.Pow(((f35(i) - xl(5)) / rr), 2)
                'dc(5) = decay * Math.Pow(((f36(i) - xl(5)) / rr), 2)
                'dc(6) = decay * Math.Pow(((f37(i) - xl(5)) / rr), 2)
                'dc(7) = decay * Math.Pow(((f38(i) - xl(5)) / rr), 2)
                dc(1) = decay * Math.Pow(((f31(i) - xl(6)) / rr), 2)
                dc(2) = decay * Math.Pow(((f32(i) - xl(6)) / rr), 2)
                dc(3) = decay * Math.Pow(((f33(i) - xl(6)) / rr), 2)
                dc(4) = decay * Math.Pow(((f34(i) - xl(6)) / rr), 2)
                dc(5) = decay * Math.Pow(((f35(i) - xl(6)) / rr), 2)
                dc(6) = decay * Math.Pow(((f36(i) - xl(6)) / rr), 2)
                dc(7) = decay * Math.Pow(((f37(i) - xl(6)) / rr), 2)
                dc(8) = decay * Math.Pow(((f38(i) - xl(6)) / rr), 2)
                ' YC 102418 END

            Case 4
                Dim f2367 = 0.25 * (f12(i) + f13(i) + f16(i) + f17(i))
                Dim f1458 = 0.25 * (f11(i) + f14(i) + f15(i) + f18(i))
                Dim decay = 0.1 * Math.Log(Math.Abs((f1458 - f2367) / f2367) - 0.09) + 1
                'Dim rr = f2367 - xl(0) + 0.5 * (f1458 - f2367) * (1 + r)
                Dim rr = f2367 - xl(1) + 0.5 * (f1458 - f2367) * (1 + r)

                ' YC 102418
                'dc(0) = decay * Math.Pow(((f11(i) - xl(0)) / rr), 2)
                'dc(1) = decay * Math.Pow(((f12(i) - xl(0)) / rr), 2)
                'dc(2) = decay * Math.Pow(((f13(i) - xl(0)) / rr), 2)
                'dc(3) = decay * Math.Pow(((f14(i) - xl(0)) / rr), 2)
                'dc(4) = decay * Math.Pow(((f15(i) - xl(0)) / rr), 2)
                'dc(5) = decay * Math.Pow(((f16(i) - xl(0)) / rr), 2)
                'dc(6) = decay * Math.Pow(((f17(i) - xl(0)) / rr), 2)
                'dc(7) = decay * Math.Pow(((f18(i) - xl(0)) / rr), 2)
                dc(1) = decay * Math.Pow(((f11(i) - xl(1)) / rr), 2)
                dc(2) = decay * Math.Pow(((f12(i) - xl(1)) / rr), 2)
                dc(3) = decay * Math.Pow(((f13(i) - xl(1)) / rr), 2)
                dc(4) = decay * Math.Pow(((f14(i) - xl(1)) / rr), 2)
                dc(5) = decay * Math.Pow(((f15(i) - xl(1)) / rr), 2)
                dc(6) = decay * Math.Pow(((f16(i) - xl(1)) / rr), 2)
                dc(7) = decay * Math.Pow(((f17(i) - xl(1)) / rr), 2)
                dc(8) = decay * Math.Pow(((f18(i) - xl(1)) / rr), 2)
                ' YC 102418 END

            Case 5
                Dim f3478 = 0.25 * (f23(i) + f24(i) + f27(i) + f28(i))
                Dim f1256 = 0.25 * (f21(i) + f22(i) + f25(i) + f26(i))
                Dim decay = 0.1 * Math.Log(Math.Abs((f1256 - f3478) / f3478) - 0.09) + 1
                'Dim rr = f3478 - xl(2) + 0.5 * (f1256 - f3478) * (1 + s)
                Dim rr = f3478 - xl(3) + 0.5 * (f1256 - f3478) * (1 + s)

                ' YC 102418
                'dc(0) = decay * Math.Pow(((f21(i) - xl(2)) / rr), 2)
                'dc(1) = decay * Math.Pow(((f22(i) - xl(2)) / rr), 2)
                'dc(2) = decay * Math.Pow(((f23(i) - xl(2)) / rr), 2)
                'dc(3) = decay * Math.Pow(((f24(i) - xl(2)) / rr), 2)
                'dc(4) = decay * Math.Pow(((f25(i) - xl(2)) / rr), 2)
                'dc(5) = decay * Math.Pow(((f26(i) - xl(2)) / rr), 2)
                'dc(6) = decay * Math.Pow(((f27(i) - xl(2)) / rr), 2)
                'dc(7) = decay * Math.Pow(((f28(i) - xl(2)) / rr), 2)
                dc(1) = decay * Math.Pow(((f21(i) - xl(3)) / rr), 2)
                dc(2) = decay * Math.Pow(((f22(i) - xl(3)) / rr), 2)
                dc(3) = decay * Math.Pow(((f23(i) - xl(3)) / rr), 2)
                dc(4) = decay * Math.Pow(((f24(i) - xl(3)) / rr), 2)
                dc(5) = decay * Math.Pow(((f25(i) - xl(3)) / rr), 2)
                dc(6) = decay * Math.Pow(((f26(i) - xl(3)) / rr), 2)
                dc(7) = decay * Math.Pow(((f27(i) - xl(3)) / rr), 2)
                dc(8) = decay * Math.Pow(((f28(i) - xl(3)) / rr), 2)
                ' YC 102418 END

            Case 6
                Dim f1234 = 0.25 * (f31(i) + f32(i) + f33(i) + f34(i))
                Dim f5678 = 0.25 * (f35(i) + f36(i) + f37(i) + f38(i))
                Dim decay = 0.1 * Math.Log(Math.Abs((f1234 - f5678) / f5678) - 0.09) + 1
                'Dim rr = f5678 - xl(4) + 0.5 * (f1234 - f5678) * (1 + t)
                Dim rr = f5678 - xl(5) + 0.5 * (f1234 - f5678) * (1 + t)

                ' YC 102418
                'dc(0) = decay * Math.Pow(((f31(i) - xl(4)) / rr), 2)
                'dc(1) = decay * Math.Pow(((f32(i) - xl(4)) / rr), 2)
                'dc(2) = decay * Math.Pow(((f33(i) - xl(4)) / rr), 2)
                'dc(3) = decay * Math.Pow(((f34(i) - xl(4)) / rr), 2)
                'dc(4) = decay * Math.Pow(((f35(i) - xl(4)) / rr), 2)
                'dc(5) = decay * Math.Pow(((f36(i) - xl(4)) / rr), 2)
                'dc(6) = decay * Math.Pow(((f37(i) - xl(4)) / rr), 2)
                'dc(7) = decay * Math.Pow(((f38(i) - xl(4)) / rr), 2)
                dc(1) = decay * Math.Pow(((f31(i) - xl(5)) / rr), 2)
                dc(2) = decay * Math.Pow(((f32(i) - xl(5)) / rr), 2)
                dc(3) = decay * Math.Pow(((f33(i) - xl(5)) / rr), 2)
                dc(4) = decay * Math.Pow(((f34(i) - xl(5)) / rr), 2)
                dc(5) = decay * Math.Pow(((f35(i) - xl(5)) / rr), 2)
                dc(6) = decay * Math.Pow(((f36(i) - xl(5)) / rr), 2)
                dc(7) = decay * Math.Pow(((f37(i) - xl(5)) / rr), 2)
                dc(8) = decay * Math.Pow(((f38(i) - xl(5)) / rr), 2)
                ' YC 102418 END

        End Select

    End Sub
End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'c     Qiang develop this subroutine at 03-09-2010.
'      subroutine bdecay(r,s,t,ro,dc,i)
'c
'      implicit double precision (a-h,o-z)                             
'c
'c===> module to adjust the decay of shape function in the infinite direction
'c
'      common/range/mft,mlt,lft,llt,nftm1
'	common/vect6/
'     1 g11(64),g21(64),g31(64),g12(64),g22(64),g32(64),
'     2 g13(64),g23(64),g33(64),g14(64),g24(64),g34(64),
'     3 g15(64),g25(64),g35(64),g16(64),g26(64),g36(64),
'     4 g17(64),g27(64),g37(64),g18(64),g28(64),g38(64),
'     5 h11(64),h21(64),h31(64),h12(64),h22(64),h32(64),
'     6 h13(64),h23(64),h33(64),h14(64),h24(64),h34(64),
'     7 h15(64),h25(64),h35(64),h16(64),h26(64),h36(64),
'     8 h17(64),h27(64),h37(64),h18(64),h28(64),h38(64),
'     9 f11(64),f21(64),f31(64),f12(64),f22(64),f32(64),
'     & f13(64),f23(64),f33(64),f14(64),f24(64),f34(64),
'     & f15(64),f25(64),f35(64),f16(64),f26(64),f36(64),
'     & f17(64),f27(64),f37(64),f18(64),f28(64),f38(64)
'c
'	common /rload/xl						
'	dimension dc(1),xl(6)
'c    
'	select case (int(ro))
'		case(:1)
'			
'			f2367=0.25*(f12(i)+f13(i)+f16(i)+f17(i))
'			f1458=0.25*(f11(i)+f14(i)+f15(i)+f18(i))
'			decay=0.1*log(abs((f2367-f1458)/f1458)-0.09)+1.
'			rr=f1458-xl(2)+0.5*(f2367-f1458)*(1.+r)
'			dc(1)=decay*((f11(i)-xl(2))/rr)**2.
'			dc(2)=decay*((f12(i)-xl(2))/rr)**2.
'			dc(3)=decay*((f13(i)-xl(2))/rr)**2.
'			dc(4)=decay*((f14(i)-xl(2))/rr)**2.
'			dc(5)=decay*((f15(i)-xl(2))/rr)**2.
'			dc(6)=decay*((f16(i)-xl(2))/rr)**2.
'			dc(7)=decay*((f17(i)-xl(2))/rr)**2.
'			dc(8)=decay*((f18(i)-xl(2))/rr)**2.	
'		case(2)
'			
'			f3478=0.25*(f23(i)+f24(i)+f27(i)+f28(i))
'			f1256=0.25*(f21(i)+f22(i)+f25(i)+f26(i))
'			decay=0.1*log(abs((f3478-f1256)/f1256)-0.09)+1.
'			rr=f1256-xl(4)+0.5*(f3478-f1256)*(1.+s)
'			dc(1)=decay*((f21(i)-xl(4))/rr)**2.
'			dc(2)=decay*((f22(i)-xl(4))/rr)**2.
'			dc(3)=decay*((f23(i)-xl(4))/rr)**2.
'			dc(4)=decay*((f24(i)-xl(4))/rr)**2.
'			dc(5)=decay*((f25(i)-xl(4))/rr)**2.
'			dc(6)=decay*((f26(i)-xl(4))/rr)**2.
'			dc(7)=decay*((f27(i)-xl(4))/rr)**2.
'			dc(8)=decay*((f28(i)-xl(4))/rr)**2.			
'		case(3)
'			
'			f1234=0.25*(f31(i)+f32(i)+f33(i)+f34(i))
'			f5678=0.25*(f35(i)+f36(i)+f37(i)+f38(i))
'			decay=0.1*log(abs((f5678-f1234)/f1234)-0.09)+1.
'			rr=f1234-xl(6)+0.5*(f5678-f1234)*(1.+t)
'			dc(1)=decay*((f31(i)-xl(6))/rr)**2.
'			dc(2)=decay*((f32(i)-xl(6))/rr)**2.
'			dc(3)=decay*((f33(i)-xl(6))/rr)**2.
'			dc(4)=decay*((f34(i)-xl(6))/rr)**2.
'			dc(5)=decay*((f35(i)-xl(6))/rr)**2.
'			dc(6)=decay*((f36(i)-xl(6))/rr)**2.
'			dc(7)=decay*((f37(i)-xl(6))/rr)**2.
'			dc(8)=decay*((f38(i)-xl(6))/rr)**2.
'		case(4)
'			
'			f2367=0.25*(f12(i)+f13(i)+f16(i)+f17(i))
'			f1458=0.25*(f11(i)+f14(i)+f15(i)+f18(i))
'			decay=0.1*log(abs((f1458-f2367)/f2367)-0.09)+1.
'			rr=f2367-xl(1)+0.5*(f1458-f2367)*(1.+r)
'			dc(1)=decay*((f11(i)-xl(1))/rr)**2.
'			dc(2)=decay*((f12(i)-xl(1))/rr)**2.
'			dc(3)=decay*((f13(i)-xl(1))/rr)**2.
'			dc(4)=decay*((f14(i)-xl(1))/rr)**2.
'			dc(5)=decay*((f15(i)-xl(1))/rr)**2.
'			dc(6)=decay*((f16(i)-xl(1))/rr)**2.
'			dc(7)=decay*((f17(i)-xl(1))/rr)**2.
'			dc(8)=decay*((f18(i)-xl(1))/rr)**2.
'			
'		case(5)
'			
'			f3478=0.25*(f23(i)+f24(i)+f27(i)+f28(i))
'			f1256=0.25*(f21(i)+f22(i)+f25(i)+f26(i))
'			decay=0.1*log(abs((f1256-f3478)/f3478)-0.09)+1.
'			rr=f3478-xl(3)+0.5*(f1256-f3478)*(1.+s)
'			dc(1)=decay*((f21(i)-xl(3))/rr)**2.
'			dc(2)=decay*((f22(i)-xl(3))/rr)**2.
'			dc(3)=decay*((f23(i)-xl(3))/rr)**2.
'			dc(4)=decay*((f24(i)-xl(3))/rr)**2.
'			dc(5)=decay*((f25(i)-xl(3))/rr)**2.
'			dc(6)=decay*((f26(i)-xl(3))/rr)**2.
'			dc(7)=decay*((f27(i)-xl(3))/rr)**2.
'			dc(8)=decay*((f28(i)-xl(3))/rr)**2.
'			
'		case(6:)
'			
'			f1234=0.25*(f31(i)+f32(i)+f33(i)+f34(i))
'			f5678=0.25*(f35(i)+f36(i)+f37(i)+f38(i))
'			decay=0.1*log(abs((f1234-f5678)/f5678)-0.09)+1.
'			rr=f5678-xl(5)+0.5*(f1234-f5678)*(1.+t)
'			dc(1)=decay*((f31(i)-xl(5))/rr)**2.
'			dc(2)=decay*((f32(i)-xl(5))/rr)**2.
'			dc(3)=decay*((f33(i)-xl(5))/rr)**2.
'			dc(4)=decay*((f34(i)-xl(5))/rr)**2.
'			dc(5)=decay*((f35(i)-xl(5))/rr)**2.
'			dc(6)=decay*((f36(i)-xl(5))/rr)**2.
'			dc(7)=decay*((f37(i)-xl(5))/rr)**2.
'			dc(8)=decay*((f38(i)-xl(5))/rr)**2.
'			
'	end select	
'c
'	return 
'	end

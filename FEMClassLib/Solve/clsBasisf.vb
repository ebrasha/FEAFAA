
'This file contains all the methods of basisf.f
Partial Public Class clsSolve

    ''' <summary>
    ''' load values of local trilinear shape ftn derivatives for current integration point
    ''' </summary>
    ''' <param name="p1"></param>
    ''' <param name="p2"></param>
    ''' <param name="p3"></param>
    ''' <param name="ipt"></param>
    ''' <param name="mattype"></param>
    ''' <param name="ipart"></param>
    ''' <param name="ro"></param>
    ''' <param name="ie"></param>
    Public Sub basisf(ByRef p1() As Double, ByRef p2() As Double, ByRef p3() As Double,
                      ByRef ipt As Integer, ByRef mattype As Integer, ByRef ipart As Integer,
                      ByRef ro As Double, ByRef ie As Integer)


        'Dim nr(7), ns(7), nt(7), dc(7) As Double           ' YC 102418
        Dim nr(8), ns(8), nt(8), dc(8) As Double

        nr(1) = -1 : ns(1) = -1 : nt(1) = -1
        nr(2) = 1 : ns(2) = -1 : nt(2) = -1
        nr(3) = 1 : ns(3) = 1 : nt(3) = -1
        nr(4) = -1 : ns(4) = 1 : nt(4) = -1
        nr(5) = -1 : ns(5) = -1 : nt(5) = 1
        nr(6) = 1 : ns(6) = -1 : nt(6) = 1
        nr(7) = 1 : ns(7) = 1 : nt(7) = 1
        nr(8) = -1 : ns(8) = 1 : nt(8) = 1



        Dim r, s, t As Double
        Dim ir, _is, it As Integer

        If mattype = 56 Then
            Dim co = ro
            ro = ro * 100000000.0 + 0.1

            Dim rootthree = 1.0 / Math.Sqrt(3.0)
            r = nr(ipt) * rootthree
            s = ns(ipt) * rootthree
            t = nt(ipt) * rootthree

            'c		decay function
            'TODO - Missing Sub
            Call bdecay(r, s, t, ro, dc, ie)

            '		mapping function

            If ipart = 1 Then
                Select Case (Int(ro))
                    Case 1

                        'For i = 0 To 7      ' YC 102418
                        For i = 1 To 8      
                            _is = ns(i) : it = nt(i)
                            'If (i = 1 OrElse i = 2 OrElse i = 5 OrElse i = 6) Then
                            If (i = 2 OrElse i = 3 OrElse i = 6 OrElse i = 7) Then  ' QW 12-12-2018-
                                p1(i) = dc(i) * dmfm1(-1, _is, it, r, s, t)
                                p2(i) = dc(i) * dmfm2(-1, _is, it, r, t)
                                p3(i) = dc(i) * dmfm2(-1, it, _is, r, s)
                            Else
                                p1(i) = dc(i) * dmfn1(-1, _is, it, r, s, t)
                                p2(i) = dc(i) * dmfn2(-1, _is, it, r, t)
                                p3(i) = dc(i) * dmfn2(-1, it, _is, r, s)
                            End If
                        Next
                    Case 2

                        'For i = 0 To 7 ' YC 102418
                        For i = 1 To 8
                            ir = nr(i) : it = nt(i)
                            ' If (i = 2 OrElse i = 3 OrElse i = 6 OrElse i = 7) Then  'QW 12-12-2018-
                            If (i = 3 OrElse i = 4 OrElse i = 7 OrElse i = 8) Then
                                p1(i) = dc(i) * dmfm2(-1, ir, it, s, t)
                                p2(i) = dc(i) * dmfm1(-1, ir, it, s, r, t)
                                p3(i) = dc(i) * dmfm2(-1, it, ir, s, r)
                            Else
                                p1(i) = dc(i) * dmfn2(-1, ir, it, s, t)
                                p2(i) = dc(i) * dmfn1(-1, ir, it, s, r, t)
                                p3(i) = dc(i) * dmfn2(-1, it, ir, s, r)
                            End If
                        Next
                    Case 3

                        'For i = 0 To 7 ' YC 102418
                        For i = 1 To 8
                            ir = nr(i) : _is = ns(i)
                            'If (i = 4 OrElse i = 5 OrElse i = 6 OrElse i = 7) Then
                            If (i = 5 OrElse i = 6 OrElse i = 7 OrElse i = 8) Then
                                p1(i) = dc(i) * dmfm2(-1, ir, _is, t, s)
                                p2(i) = dc(i) * dmfm2(-1, _is, ir, t, r)
                                p3(i) = dc(i) * dmfm1(-1, _is, ir, t, s, r)
                            Else
                                p1(i) = dc(i) * dmfn2(-1, ir, _is, t, s)
                                p2(i) = dc(i) * dmfn2(-1, _is, ir, t, r)
                                p3(i) = dc(i) * dmfn1(-1, _is, ir, t, s, r)
                            End If
                        Next
                    Case 4

                        'For i = 0 To 7 ' YC 102418
                        For i = 1 To 8
                            _is = ns(i) : it = nt(i)
                            'If (i = 0 OrElse i = 3 OrElse i = 4 OrElse i = 7) Then
                            If (i = 1 OrElse i = 4 OrElse i = 5 OrElse i = 8) Then
                                p1(i) = dc(i) * dmfm1(1, _is, it, r, s, t)
                                p2(i) = dc(i) * dmfm2(1, _is, it, r, t)
                                p3(i) = dc(i) * dmfm2(1, it, _is, r, s)
                            Else
                                p1(i) = dc(i) * dmfn1(1, _is, it, r, s, t)
                                p2(i) = dc(i) * dmfn2(1, _is, it, r, t)
                                p3(i) = dc(i) * dmfn2(1, it, _is, r, s)
                            End If
                        Next
                    Case 5

                        'For i = 0 To 7 ' YC 102418
                        For i = 1 To 8
                            ir = nr(i) : it = nt(i)
                            'If (i = 0 OrElse i = 1 OrElse i = 4 OrElse i = 5) Then
                            If (i = 1 OrElse i = 2 OrElse i = 5 OrElse i = 6) Then
                                p1(i) = dc(i) * dmfm2(1, ir, it, s, t)
                                p2(i) = dc(i) * dmfm1(1, ir, it, s, r, t)
                                p3(i) = dc(i) * dmfm2(1, it, ir, s, r)
                            Else
                                p1(i) = dc(i) * dmfn2(1, ir, it, s, t)
                                p2(i) = dc(i) * dmfn1(1, ir, it, s, r, t)
                                p3(i) = dc(i) * dmfn2(1, it, ir, s, r)
                            End If
                        Next
                    Case 6

                        'For i = 0 To 7 ' YC 102418
                        For i = 1 To 8
                            ir = nr(i) : _is = ns(i)
                            ' If (i = 0 OrElse i = 1 OrElse i = 2 OrElse i = 3) Then
                            If (i = 1 OrElse i = 2 OrElse i = 3 OrElse i = 4) Then
                                p1(i) = dc(i) * dmfm2(1, ir, _is, t, s)
                                p2(i) = dc(i) * dmfm2(1, _is, ir, t, r)
                                p3(i) = dc(i) * dmfm1(1, _is, ir, t, s, r)
                            Else
                                p1(i) = dc(i) * dmfn2(1, ir, _is, t, s)
                                p2(i) = dc(i) * dmfn2(1, _is, ir, t, r)
                                p3(i) = dc(i) * dmfn1(1, _is, ir, t, s, r)
                            End If
                        Next
                        '			
                End Select

                '		shape function

            ElseIf ipart = 2 Then

                'For i = 0 To 7 ' YC 102418
                For i = 1 To 8
                    p1(i) = dc(i) * dsf(nr(i), ns(i), nt(i), s, t)
                    p2(i) = dc(i) * dsf(ns(i), nr(i), nt(i), r, t)
                    p3(i) = dc(i) * dsf(nt(i), nr(i), ns(i), r, s)
                Next
            End If

            ro = co

        Else

            'For i = 0 To 7 ' YC 102418
            For i = 1 To 8
                p1(i) = pr(i, ipt)
                p2(i) = ps(i, ipt)
                p3(i) = pt(i, ipt)
            Next
        End If


    End Sub

    ''' <summary>
    ''' derivative of mapping function of middle point with respect to r
    ''' </summary>
    ''' <param name="ir"></param>
    ''' <param name="_is"></param>
    ''' <param name="it"></param>
    ''' <param name="r"></param>
    ''' <param name="s"></param>
    ''' <param name="t"></param>
    ''' <returns></returns>
    Public Function dmfm1(ir As Integer, _is As Double, it As Integer, r As Double, s As Double, t As Double) As Double
        dmfm1 = -0.5 * ir * (1 + _is * s) * (1 + it * t) / Math.Pow((1 + ir * r), 2)
    End Function

    ''' <summary>
    ''' derivative of mapping function of corner point with respect to r
    ''' </summary>
    ''' <param name="ir"></param>
    ''' <param name="_is"></param>
    ''' <param name="it"></param>
    ''' <param name="r"></param>
    ''' <param name="s"></param>
    ''' <param name="t"></param>
    ''' <returns></returns>
    Public Function dmfn1(ir As Integer, _is As Double, it As Integer, r As Double, s As Double, t As Double) As Double
        dmfn1 = 0.5 * ir * (1 + _is * s) * (1 + it * t) / Math.Pow((1 + ir * r), 2)
    End Function

    ''' <summary>
    ''' derivative of mapping function of middle point with respect to s
    ''' </summary>
    ''' <param name="ir"></param>
    ''' <param name="_is"></param>
    ''' <param name="it"></param>
    ''' <param name="r"></param>
    ''' <param name="t"></param>
    ''' <returns></returns>
    Public Function dmfm2(ir As Integer, _is As Double, it As Integer, r As Double, t As Double) As Double
        dmfm2 = 0.25 * _is * (1 - ir * r) * (1 + it * t) / (1 + ir * r)
    End Function

    ''' <summary>
    ''' derivative of mapping function of corner point with respect to s
    ''' </summary>
    ''' <param name="ir"></param>
    ''' <param name="_is"></param>
    ''' <param name="it"></param>
    ''' <param name="r"></param>
    ''' <param name="t"></param>
    ''' <returns></returns>
    Public Function dmfn2(ir As Integer, _is As Double, it As Integer, r As Double, t As Double) As Double
        dmfn2 = 0.5 * ir * _is * r * (1.0 + it * t) / (1 + ir * r)
    End Function

    ''' <summary>
    ''' derivative of shape function with respect to r
    ''' </summary>
    ''' <param name="ir"></param>
    ''' <param name="_is"></param>
    ''' <param name="it"></param>
    ''' <param name="s"></param>
    ''' <param name="t"></param>
    ''' <returns></returns>
    Public Function dsf(ir As Integer, _is As Double, it As Integer, s As Double, t As Double) As Double
        dsf = 0.125 * ir * (1.0 + _is * s) * (1.0 + it * t)
    End Function

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'c NKC added mattype parameter. 
'c	Qiang rewrote this subroutine to modify the mapping and shape functions. 03-09-2010.
'      subroutine basisf(p1,p2,p3,ipt,mattype,ipart,ro,ie)
'c
'      implicit double precision (a-h,o-z)                            
'c
'c===> module to load values of local trilinear shape ftn derivatives
'c     for current integration point
'c
'      common/bk32/h(8,9),pr(8,9),ps(8,9),pt(8,9)
'      dimension nr(8),ns(8),nt(8),dc(8),p1(*),p2(*),p3(*)
'
'c	NKC  added infinite shape funcitons for material type 56
'c
'	nr(1)=-1;ns(1)=-1;nt(1)=-1
'	nr(2)=1;ns(2)=-1;nt(2)=-1
'	nr(3)=1;ns(3)=1;nt(3)=-1
'	nr(4)=-1;ns(4)=1;nt(4)=-1
'	nr(5)=-1;ns(5)=-1;nt(5)=1
'	nr(6)=1;ns(6)=-1;nt(6)=1
'	nr(7)=1;ns(7)=1;nt(7)=1
'	nr(8)=-1;ns(8)=1;nt(8)=1
'c
'      if(mattype.eq.56) then
'		co=ro											
'		ro=ro*100000000.+0.1
'c
'		rootthree=1./sqrt(3.0)
'		r=nr(ipt)*rootthree
'		s=ns(ipt)*rootthree
'		t=nt(ipt)*rootthree
'c
'c		decay function
'c
'        	call bdecay(r,s,t,ro,dc,ie)
'c
'c		mapping function
'c
'	  if(ipart.eq.1) then
'		select case (int(ro))
'			case(:1)	
'			do i=1,8
'				is=ns(i);it=nt(i)
'				if(i==2.or.i==3.or.i==6.or.i==7) then
'					p1(i)=dc(i)*dmfm1(-1,is,it,r,s,t)
'					p2(i)=dc(i)*dmfm2(-1,is,it,r,t)
'					p3(i)=dc(i)*dmfm2(-1,it,is,r,s)
'				else
'					p1(i)=dc(i)*dmfn1(-1,is,it,r,s,t)
'					p2(i)=dc(i)*dmfn2(-1,is,it,r,t)
'					p3(i)=dc(i)*dmfn2(-1,it,is,r,s)
'				end if
'			end do		
'			case(2)
'			do i=1,8
'				ir=nr(i);it=nt(i)
'				if(i==3.or.i==4.or.i==7.or.i==8) then
'					p1(i)=dc(i)*dmfm2(-1,ir,it,s,t)
'					p2(i)=dc(i)*dmfm1(-1,ir,it,s,r,t)
'					p3(i)=dc(i)*dmfm2(-1,it,ir,s,r)
'				else
'					p1(i)=dc(i)*dmfn2(-1,ir,it,s,t)
'					p2(i)=dc(i)*dmfn1(-1,ir,it,s,r,t)
'					p3(i)=dc(i)*dmfn2(-1,it,ir,s,r)
'				end if
'			end do
'			case(3)
'			do i=1,8
'				ir=nr(i);is=ns(i)
'				if(i==5.or.i==6.or.i==7.or.i==8) then
'					p1(i)=dc(i)*dmfm2(-1,ir,is,t,s)
'					p2(i)=dc(i)*dmfm2(-1,is,ir,t,r)
'					p3(i)=dc(i)*dmfm1(-1,is,ir,t,s,r)
'				else
'					p1(i)=dc(i)*dmfn2(-1,ir,is,t,s)
'					p2(i)=dc(i)*dmfn2(-1,is,ir,t,r)
'					p3(i)=dc(i)*dmfn1(-1,is,ir,t,s,r)
'				end if
'			end do
'			case(4)
'			do i=1,8
'				is=ns(i);it=nt(i)
'				if(i==1.or.i==4.or.i==5.or.i==8) then
'					p1(i)=dc(i)*dmfm1(1,is,it,r,s,t)
'					p2(i)=dc(i)*dmfm2(1,is,it,r,t)
'					p3(i)=dc(i)*dmfm2(1,it,is,r,s)
'				else
'					p1(i)=dc(i)*dmfn1(1,is,it,r,s,t)
'					p2(i)=dc(i)*dmfn2(1,is,it,r,t)
'					p3(i)=dc(i)*dmfn2(1,it,is,r,s)
'				end if
'			end do
'			case(5)
'			do i=1,8
'				ir=nr(i);it=nt(i)
'				if(i==1.or.i==2.or.i==5.or.i==6) then
'					p1(i)=dc(i)*dmfm2(1,ir,it,s,t)
'					p2(i)=dc(i)*dmfm1(1,ir,it,s,r,t)
'					p3(i)=dc(i)*dmfm2(1,it,ir,s,r)
'				else
'					p1(i)=dc(i)*dmfn2(1,ir,it,s,t)
'					p2(i)=dc(i)*dmfn1(1,ir,it,s,r,t)
'					p3(i)=dc(i)*dmfn2(1,it,ir,s,r)
'				end if
'			end do
'			case(6:)			
'			do i=1,8	
'				ir=nr(i);is=ns(i)
'				if(i==1.or.i==2.or.i==3.or.i==4) then
'					p1(i)=dc(i)*dmfm2(1,ir,is,t,s)
'					p2(i)=dc(i)*dmfm2(1,is,ir,t,r)
'					p3(i)=dc(i)*dmfm1(1,is,ir,t,s,r)
'				else
'					p1(i)=dc(i)*dmfn2(1,ir,is,t,s)
'					p2(i)=dc(i)*dmfn2(1,is,ir,t,r)
'					p3(i)=dc(i)*dmfn1(1,is,ir,t,s,r)
'				end if
'			end do
'			
'		end select
'c
'c		shape function
'c
'          else if(ipart.eq.2) then		
'c
'			do i=1,8
'				p1(i)=dc(i)*dsf(nr(i),ns(i),nt(i),s,t)
'				p2(i)=dc(i)*dsf(ns(i),nr(i),nt(i),r,t)
'				p3(i)=dc(i)*dsf(nt(i),nr(i),ns(i),r,s)
'			end do
'        endif
'c
'		ro=co										
'c
'      else
'        do 20 i=1,8
'        p1(i)=pr(i,ipt)
'        p2(i)=ps(i,ipt)
'   20   p3(i)=pt(i,ipt)
'      endif
'c
'      return
'      end
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'	function dmfm1(ir,is,it,r,s,t)
'c
'      implicit double precision (a-h,o-z)
'c
'c     derivative of mapping function of middle point with respect to r
'c
'	dmfm1=-0.5*ir*(1.+is*s)*(1.+it*t)/(1+ir*r)**2.
'      return
'      end
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      function dmfn1(ir,is,it,r,s,t)
'c
'      implicit double precision (a-h,o-z)
'c
'c     derivative of mapping function of corner point with respect to r
'c
'	dmfn1=0.5*ir*(1.+is*s)*(1.+it*t)/(1+ir*r)**2.
'      return
'      end
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'
'      function dmfm2(ir,is,it,r,t)
'c
'      implicit double precision (a-h,o-z)
'c
'c     derivative of mapping function of middle point with respect to s
'c
'	dmfm2=0.25*is*(1.-ir*r)*(1.+it*t)/(1+ir*r)
'      return
'      end
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      function dmfn2(ir,is,it,r,t)
'c
'      implicit double precision (a-h,o-z)
'c
'c     derivative of mapping function of corner point with respect to s
'c
'	dmfn2=0.5*ir*is*r*(1.+it*t)/(1+ir*r)
'      return
'      end
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      function dsf(ir,is,it,s,t)
'c
'      implicit double precision (a-h,o-z)
'c
'c     derivative of shape function with respect to r
'c
'	dsf=0.125*ir*(1.+is*s)*(1.+it*t)
'
'      return
'      end

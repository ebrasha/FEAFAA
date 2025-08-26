'This file contains all the methods of fcalc.f

'Partial Public Class clsOthers  ' YC 102418-041519
Partial Public Class clsSolve

    ''' <summary>
    ''' to compute tangential friction force on slidesurface facet
    ''' </summary>
    ''' <param name="fni"></param>
    ''' <param name="fxi"></param>
    ''' <param name="fyi"></param>
    ''' <param name="fzi"></param>
    ''' <param name="fric"></param>
    ''' <param name="fdat"></param>
    ''' <param name="iseg"></param>
    ''' <param name="l"></param>
    ''' <param name="ss"></param>
    ''' <param name="tt"></param>
    ''' <param name="e1"></param>
    ''' <param name="e2"></param>
    ''' <param name="e3"></param>
    ''' <param name="amx"></param>
    ''' <param name="amy"></param>
    ''' <param name="amz"></param>
    ''' <param name="irect"></param>
    ''' <param name="dt2"></param>
    ''' <param name="stft"></param>
    ''' <param name="x"></param>
    ''' <param name="dx"></param>
    ''' <param name="dy"></param>
    ''' <param name="dz"></param>
    ''' <param name="fmax"></param>
    ''' <param name="fmag"></param>
    Public Sub fcalc(ByRef fni As Double, ByRef fxi As Double, ByRef fyi As Double,
                     ByRef fzi As Double, ByRef fric() As Double, ByRef fdat() As Double,
                     ByRef iseg() As Integer, ByRef l As Integer, ByRef ss As Double,
                     ByRef tt As Double, ByRef e1 As Double, ByRef e2 As Double,
                     ByRef e3 As Double, ByRef amx As Double, ByRef amy As Double,
                     ByRef amz As Double, ByRef irect(,) As Integer, ByRef dt2 As Double,
                     ByRef stft As Double, ByRef x(,) As Double, ByRef dx As Double, ByRef dy As Double,
                     ByRef dz As Double, ByRef fmax As Double, ByRef fmag As Double)

        'Dim jj, nn1, nn2, nn3, nn4, ds, vel, proj, sclf As Integer ' YC 102418-041519
        Dim jj, nn1, nn2, nn3, nn4 As Integer

        Dim h1, h2, h3, h4 As Double
        Dim one = 1

        Dim tp, tm, sp, sm, ds, vel, proj, sclf As Double ' YC 102418-041519

        If fric(3) < 0 Then stft = 100 * stft
        jj = iseg(1)                 ' n-1 segment cidp#
        If jj <> 0 Then GoTo 10
        iseg(2) = l                  ' store current n segment cidp#
        fdat(6) = ss                 ' store current n isoparm coords
        fdat(7) = tt
        dx = 0.0
        dy = 0.0
        dz = 0.0
        Return
10:     tp = 0.25 * (1.0 + fdat(2))       ' shape ftn vals for n-1 isoparm coords
        tm = 0.25 * (1.0 - fdat(2))
        sp = 1.0 + fdat(1)
        sm = 1.0 - fdat(1)
        h1 = tm * sm
        h2 = tm * sp
        h3 = tp * sp
        h4 = tp * sm
        nn1 = irect(1, jj)
        nn2 = irect(2, jj)
        nn3 = irect(3, jj)
        nn4 = irect(4, jj)

        'c   ... dx = projection amx at time n -  projection at time n-1
        dx = amx - h1 * x(1, nn1) - h2 * x(1, nn2) - h3 * x(1, nn3) - h4 * x(1, nn4)
        dy = amy - h1 * x(2, nn1) - h2 * x(2, nn2) - h3 * x(2, nn3) - h4 * x(2, nn4)
        dz = amz - h1 * x(3, nn1) - h2 * x(3, nn2) - h3 * x(3, nn3) - h4 * x(3, nn4)

        'c   ... incremental velocity along curve
        ds = Math.Sqrt(Math.Pow(dx, 2) + Math.Pow(dy, 2) + Math.Pow(dz, 2))
        vel = ds / Math.Max(1.0E-20 * one, dt2)

        'c   ... calc f(n) (unprojected)
        'c.....mod by drb 12/02
        fdat(8) = fdat(3) + stft * dx
        fdat(9) = fdat(4) + stft * dy
        fdat(10) = fdat(5) + stft * dz
        'c.....end mod
        'c   ... In the following project velocities and friction onto facet where
        'c        the surface normal is given by n = (e1,e2,e3)
        'c       project n-1 friction onto facet i.e. f(n-1) = f(n-1) - (f(n-1)*n) n
        'c       project vel onto facet               vel    = vel - (vel*n) n
        'c        this projected velocity actually underpredicts curvlinear vel
        'c       project n   friction  f(n) = f(n-1) + k vel - (f(n-1) + k vel)*n n

        proj = fdat(8) * e1 + fdat(9) * e2 + fdat(10) * e3
        fdat(8) = fdat(8) - proj * e1
        fdat(9) = fdat(9) - proj * e2
        fdat(10) = fdat(10) - proj * e3

        'c  ... Determine which friction coefficient to use
        If fric(3) <= 0 And fric(2) <> 0 Then   ' Newer method
            fmax = -(fric(2) + (fric(1) - fric(2)) * Math.Exp(fric(3) * vel)) * fni
        Else                    ' Older Dyna3d method
            fmax = -(fric(2) + (fric(1) - fric(2)) * Math.Exp(-fric(3) * vel)) * fni
        End If
        fmag = Math.Sqrt(Math.Pow(fdat(8), 2) + Math.Pow(fdat(9), 2) + Math.Pow(fdat(10), 2))
        '
        'c  ... Check if trial friction gt max if so scale
        'c.....mod by drb 12/02 - this suppresses the slide phase
        GoTo 20
        'c.....end mod
        sclf = fmax / fmag
        fdat(8) = sclf * fdat(8)
        fdat(9) = sclf * fdat(9)
        fdat(10) = sclf * fdat(10)

        'c  ... Add friction force to normal
20:     fxi = fxi + fdat(8)
        fyi = fyi + fdat(9)
        fzi = fzi + fdat(10)

        'c  ... Save current seg cidp# and isoparm coords (update in fupdt if converged)
        iseg(2) = l
        fdat(6) = ss
        fdat(7) = tt

    End Sub
End Class

'  ref org fortran code 
'cidp# 1 "fcalc.F"
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine fcalc(fni,fxi,fyi,fzi,fric,fdat,iseg,l,ss,tt,
'     1                 e1,e2,e3,amx,amy,amz,irect,dt2,stft,x,
'     1                 dx,dy,dz,fmax,fmag)
'c
'
'      implicit double precision (a-h,o-z)                             
'c
'c===> module to compute tangential friction force on slidesurface facet
'c
'      dimension fric(*),fdat(*),irect(4,*),x(3,*),iseg(*)
'c
'      one = 1
'      if(fric(3).lt.0) stft=100*stft
'      jj=iseg(1)                 ! n-1 segment cidp#
'      if (jj.ne.0) go to 10
'      iseg(2)=l                  ! store current n segment cidp#
'      fdat(6)=ss                 ! store current n isoparm coords
'      fdat(7)=tt
'      dx=0.
'      dy=0.
'      dz=0.
'      return
'   10 tp=.25*(1.0+fdat(2))       ! shape ftn vals for n-1 isoparm coords
'      tm=.25*(1.0-fdat(2))
'      sp=1.0+fdat(1)
'      sm=1.0-fdat(1)
'      h1=tm*sm
'      h2=tm*sp
'      h3=tp*sp
'      h4=tp*sm
'      nn1=irect(1,jj)
'      nn2=irect(2,jj)
'      nn3=irect(3,jj)
'      nn4=irect(4,jj)
'
'c   ... dx = projection amx at time n -  projection at time n-1
'      dx=amx-h1*x(1,nn1)-h2*x(1,nn2)-h3*x(1,nn3)-h4*x(1,nn4)
'      dy=amy-h1*x(2,nn1)-h2*x(2,nn2)-h3*x(2,nn3)-h4*x(2,nn4)
'      dz=amz-h1*x(3,nn1)-h2*x(3,nn2)-h3*x(3,nn3)-h4*x(3,nn4)
'
'c   ... incremental velocity along curve
'      ds =sqrt(dx**2+dy**2+dz**2)
'      vel=ds/max(1.e-20*one,dt2)
'
'c   ... calc f(n) (unprojected)
'c.....mod by drb 12/02
'		fdat(8)=fdat(3)+stft*dx
'		fdat(9)=fdat(4)+stft*dy
'		fdat(10)=fdat(5)+stft*dz
'c.....end mod
'c   ... In the following project velocities and friction onto facet where
'c        the surface normal is given by n = (e1,e2,e3)
'c       project n-1 friction onto facet i.e. f(n-1) = f(n-1) - (f(n-1)*n) n
'c       project vel onto facet               vel    = vel - (vel*n) n
'c        this projected velocity actually underpredicts curvlinear vel
'c       project n   friction  f(n) = f(n-1) + k vel - (f(n-1) + k vel)*n n
'
'      proj=fdat(8)*e1+fdat(9)*e2+fdat(10)*e3
'      fdat(8)=fdat(8)-proj*e1
'      fdat(9)=fdat(9)-proj*e2
'      fdat(10)=fdat(10)-proj*e3
'
'c  ... Determine which friction coefficient to use
'      if(fric(3).le.0.and.fric(2).ne.0) then   ! Newer method
'       fmax= -( fric(2) + (fric(1)-fric(2))*exp(fric(3)*vel) )*fni
'      else                    ! Older Dyna3d method
'       fmax= -( fric(2) + (fric(1)-fric(2))*exp(-fric(3)*vel) )*fni
'      endif
'      fmag=sqrt(fdat(8)**2+fdat(9)**2+fdat(10)**2)
'
'c  ... Check if trial friction gt max if so scale
'c.....mod by drb 12/02 - this suppresses the slide phase
'	goto 20
'c.....end mod
'      sclf=fmax/fmag
'      fdat(8)=sclf*fdat(8)
'      fdat(9)=sclf*fdat(9)
'      fdat(10)=sclf*fdat(10)
'
'c  ... Add friction force to normal
'   20 fxi=fxi+fdat(8)
'      fyi=fyi+fdat(9)
'      fzi=fzi+fdat(10)
'
'c  ... Save current seg cidp# and isoparm coords (update in fupdt if converged)
'      iseg(2)=l
'      fdat(6)=ss
'      fdat(7)=tt
'      return
'      end

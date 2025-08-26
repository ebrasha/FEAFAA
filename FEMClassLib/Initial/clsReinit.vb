Partial Public Class clsInitial

    ' From the class of clsCom 

    Public Sub Reinit()
        ReDim gass(3, 9), gps(4), gpt(4), irow(9), icol(9), igrad(9)
        gass(1, 1) = -0.5773502691896
        gass(2, 1) = -0.5773502691896
        gass(3, 1) = -0.5773502691896
        gass(1, 2) = 0.5773502691896
        gass(2, 2) = -0.5773502691896
        gass(3, 2) = -0.5773502691896
        gass(1, 3) = 0.5773502691896
        gass(2, 3) = 0.5773502691896
        gass(3, 3) = -0.5773502691896

        gass(1, 4) = -0.5773502691896
        gass(2, 4) = 0.5773502691896
        gass(3, 4) = -0.5773502691896
        gass(1, 5) = -0.5773502691896
        gass(2, 5) = -0.5773502691896
        gass(3, 5) = 0.5773502691896
        gass(1, 6) = 0.5773502691896
        gass(2, 6) = -0.5773502691896
        gass(3, 6) = 0.5773502691896

        gass(1, 7) = 0.5773502691896
        gass(2, 7) = 0.5773502691896
        gass(3, 7) = 0.5773502691896
        gass(1, 8) = -0.5773502691896
        gass(2, 8) = 0.5773502691896
        gass(3, 8) = 0.5773502691896
        gass(1, 9) = 0.0
        gass(2, 9) = 0.0
        gass(3, 9) = 0.0

        gps(1) = 0.5773502691896
        gps(2) = -0.5773502691896
        gps(3) = -0.5773502691896
        gps(4) = 0.5773502691896

        gpt(1) = 0.5773502691896
        gpt(2) = 0.5773502691896
        gpt(3) = -0.5773502691896
        gpt(4) = -0.5773502691896

        irow(1) = 1
        irow(2) = 2
        irow(3) = 3
        irow(4) = 4
        irow(5) = 4
        irow(6) = 5
        irow(7) = 5
        irow(8) = 6
        irow(9) = 6

        icol(1) = 1
        icol(2) = 2
        icol(3) = 3
        icol(4) = 1
        icol(5) = 2
        icol(6) = 2
        icol(7) = 3
        icol(8) = 1
        icol(9) = 3

        igrad(1) = 1
        igrad(2) = 2
        igrad(3) = 3
        igrad(4) = 2
        igrad(5) = 1
        igrad(6) = 3
        igrad(7) = 2
        igrad(8) = 3
        igrad(9) = 1

        extime = -1 : nstep = -1 : mthsol = 1 : riksf = 0 : lcspf = 0 : kpri = 1
        alpha = 1 : ptime = 0 : tnew = 0 : iprec = 2
        dtx0 = dt
        nprm(1) = 24
        nprm(2) = (nprm(1) * (nprm(1) + 1)) / 2
        nprm(3) = 0
        nprm(4) = 96
        nprm(5) = 48
        nprm(6) = 12
        nprm(8) = nprm(1) + nprm(2)
        ' control card #10
        itrlmt = 9999 : tollin = 0.001
        ' zero displacement boundary conditions  
        numdc = 0 : numudc = 0 : numpc = 0
        idid(numdc) = 0 : lcd(numdc) = 0 : amad(numdc) = 0 : idflad(numdc) = 0 : rf(numdc) = 0
        For i = 0 To 8
            xn(i, numpc) = 0 : yn(i, numpc) = 0 : zn(i, numpc) = 0
            pmult(i, numpc) = 0 : strt(numpc) = 0 : nodep(i, numpc) = 0
        Next
        ' body force
        nthpz = 1 : gax = 0 : gay = 0 : gaz = 1

    End Sub
   
End Class

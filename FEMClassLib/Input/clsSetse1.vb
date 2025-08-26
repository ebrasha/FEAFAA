'This file contains all the methods of setse1.f
Partial Public Class clsInput

    Public Sub setse1(ByRef prop() As Double, ByRef mt As Integer)
        Dim prop_n(4, 4) As Double
        For ind1 = 1 To 4
            Try
                prop_n(ind1, 1) = prop(ind1)
            Catch ex As Exception
                prop_n(ind1, 1) = 0
            End Try
        Next
        Call setse1(prop_n, mt)
    End Sub
    ''' <summary>
    ''' initialize elastic material constants
    ''' </summary>
    ''' <param name="prop"></param>
    ''' <param name="mt"></param>
    Public Sub setse1(ByRef prop(,) As Double, ByRef mt As Integer)
        Dim sb = New Text.StringBuilder()
        sb.AppendLine("*******************************************************")
        sb.AppendLine("*                   - WARNING -                       *")
        sb.AppendLine("*   Poisson\'s ratio for material #',{0},' is near 0.5,    *")
        sb.AppendLine("*  a condition likely to generate numerical errors.   *")
        sb.AppendLine("*******************************************************")
        Dim q1, q2, q3 As Double

        If Math.Abs(1 - 2 * prop(2, 1)) < 0.0000000001 Then
            Console.WriteLine(String.Format(sb.ToString(), mt.ToString("000")))
            '        write(luo,100) mt  'What to do?
        End If
        q1 = prop(1, 1) * prop(1, 0) / ((1 + prop(2, 1)) * (1 - 2.0 * prop(2, 1)))
        q2 = prop(1, 1) * 0.5 / (1 + prop(2, 1))
        q3 = q1 + 2.0 * q2
        prop(1, 5) = prop(1, 2)
        prop(2, 5) = prop(2, 1)
        For i = 0 To 3
            For j = 0 To 3
                prop(i, j) = 0.0
            Next
        Next
        prop(1, 1) = q3
        prop(2, 2) = q3
        prop(3, 3) = q3
        prop(4, 4) = q2
        prop(1, 2) = q1
        prop(1, 3) = q1
        prop(2, 1) = q1
        prop(2, 3) = q1
        prop(3, 1) = q1
        prop(3, 2) = q1
    End Sub
End Class


'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine setse1(prop,mt)
'c
'
'      implicit double precision (a-h,o-z)                                    dp
'
'c
'c===> module to initialize elastic material constants
'c
'      common/lunits/lutty,lui,luo,lud,lur,lut,lug,luf,lust,
'     &              lus1,lus2,lus5,lus6,lus11,lus12,luebe
'      dimension prop(4,*)
'c
'
'!ik099 1111111111
'      integer ik01
'      if (ik01.eq.0) then
'       write(88,*) 'setse1'
'      ik01 = ik01 + 1
'      end if
'
'      if ( abs(1.-2.*prop(2,1)) .lt. 1.e-10) then
'        write(lutty,100) mt
'        write(luo,100) mt
'      endif
'      q1=prop(1,1)*prop(2,1)/((1.+prop(2,1))*(1.-2.0*prop(2,1)))
'      q2=prop(1,1)*0.5/(1.+prop(2,1))
'      q3=q1+2.0*q2
'      prop(1,5)=prop(1,1)
'      prop(2,5)=prop(2,1)
'      do 10 i=1,4
'      do 10 j=1,4
'   10 prop(i,j)=0.0
'      prop(1,1)=q3
'      prop(2,2)=q3
'      prop(3,3)=q3
'      prop(4,4)=q2
'      prop(1,2)=q1
'      prop(1,3)=q1
'      prop(2,1)=q1
'      prop(2,3)=q1
'      prop(3,1)=q1
'      prop(3,2)=q1
'      return
'  100 format(/
'     &' *******************************************************',/
'     &' *                   - WARNING -                       *',/
'     &' *   Poisson"s ratio for material #',i3,' is near 0.5,    *',/
'     &' *  a condition likely to generate numerical errors.   *',/
'     &' *******************************************************')
'      end
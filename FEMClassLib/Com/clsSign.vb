Partial Public Class clsCom

    'Common Fortran Library functions not in VB
    Public Function sign(x As Double, y As Double) As Double
        Return If(y >= 0, Math.Abs(x), -Math.Abs(x))
    End Function

    Public Function sign(x As Integer, y As Integer) As Integer
        Return If(y >= 0, Math.Abs(x), -Math.Abs(x))
    End Function

    Public Function nint(x As Double) As Integer  'YC? 092018
        Return If(x >= 0, Int(x + 0.5), Int(x - 0.5))
    End Function
End Class

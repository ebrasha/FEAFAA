Imports System
Imports System.Globalization
Imports System.Windows
Imports System.Windows.Data
Imports System.Windows.Media.Imaging
'Namespace FEAFAA
<ValueConversion(GetType(String), GetType(Boolean))> _
Public Class HeaderToImageConverter
    Implements IValueConverter
    Public Shared Instance As New HeaderToImageConverter()

    Public Function Convert(value As Object, targetType As Type, parameter As Object, culture As CultureInfo) As Object Implements IValueConverter.Convert
        Try
            If TryCast(value, String).Contains("\") Then
                Dim uri As New Uri("pack://application:,,,/Images/diskdrive.png")
                Dim source As New BitmapImage(uri)
                Return source
                'Return My.Resources.diskdrive
            Else
                Dim uri As New Uri("pack://application:,,,/Images/folder.png")
                Dim source As New BitmapImage(uri)
                Return source
            End If
        Catch ex As Exception
            Return New BitmapImage()
        End Try
    End Function
    Public Function ConvertBack(value As Object, targetType As Type, parameter As Object, culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotSupportedException("Cannot convert back")
    End Function
End Class
'End Namespace



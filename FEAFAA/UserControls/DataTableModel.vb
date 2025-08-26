Imports System.ComponentModel
Public Class DataTableModel
    Implements INotifyPropertyChanged

    Private _GrossWeight As String
    Private _GrossWeightOnGears As String
    Private _NumGears As String
    Private _WheelsOnGear As String
    Private _TirePressure As String

    Public Property GrossWeight() As String
        Get
            Return _GrossWeight
        End Get
        Set(ByVal value As String)
            _GrossWeight = value
            ' Call OnPropertyChanged whenever the property is updated
            OnPropertyChanged("GrossWeight")
        End Set
    End Property
    Public Property GrossWeightOnGears() As String
        Get
            Return _GrossWeightOnGears
        End Get
        Set(ByVal value As String)
            _GrossWeightOnGears = value
            ' Call OnPropertyChanged whenever the property is updated
            OnPropertyChanged("GrossWeightOnGears")
        End Set
    End Property
    Public Property NumGears() As String
        Get
            Return _NumGears
        End Get
        Set(ByVal value As String)
            _NumGears = value
            ' Call OnPropertyChanged whenever the property is updated
            OnPropertyChanged("NumGears")
        End Set
    End Property
    Public Property WheelsOnGear() As String
        Get
            Return _WheelsOnGear
        End Get
        Set(ByVal value As String)
            _WheelsOnGear = value
            ' Call OnPropertyChanged whenever the property is updated
            OnPropertyChanged("WheelsOnGear")
        End Set
    End Property
    Public Property TirePressure() As String
        Get
            Return _TirePressure
        End Get
        Set(ByVal value As String)
            _TirePressure = value
            ' Call OnPropertyChanged whenever the property is updated
            OnPropertyChanged("TirePressure")
        End Set
    End Property
  

    ' Declare the event 
    Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged

    ' Create the OnPropertyChanged method to raise the event 
    Protected Sub OnPropertyChanged(ByVal name As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(name))
    End Sub

End Class

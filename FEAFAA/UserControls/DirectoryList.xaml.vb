Imports System.IO
Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text
Imports System.Windows
Imports System.Windows.Controls
Imports System.Windows.Data
Imports System.Windows.Documents
Imports System.Windows.Input
Imports System.Windows.Media
Imports System.Windows.Media.Imaging
Imports System.Windows.Navigation
Imports System.Windows.Shapes
'Namespace FEAFAA.Controls
Public Class DirectoryList

    Public dummyNode As Object = Nothing
    Public Property SelectedImagePath() As String
        Get
            Return m_SelectedImagePath
        End Get
        Set(value As String)
            m_SelectedImagePath = value
        End Set
    End Property
    Public Property SelectedPath() As String
        Get
            Dim tree As TreeView = DirectCast(Me.DirectoryList.foldersItem, TreeView)
            Dim temp As TreeViewItem = DirectCast(tree.SelectedItem, TreeViewItem)
            Return IIf(IsNothing(temp.Tag), String.Empty, temp.Tag.ToString())
        End Get
        Set(value As String)
            Dim tree As TreeView = DirectCast(Me.DirectoryList.foldersItem, TreeView)
            Dim temp As Object = tree
            Dim selectedNode As TreeViewItem = Nothing
            Dim paths As IEnumerable(Of String) = value.Split("\")
            For Each path As String In paths
                For Each item As TreeViewItem In temp.Items
                    If (Not IsNothing(item.Header) AndAlso item.Header.ToString().Replace("\", "") = path) Then
                        selectedNode = item
                        selectedNode.IsSelected = True
                        selectedNode.IsExpanded = True
                        Exit For
                    End If
                Next
                temp = selectedNode
            Next
            'If (Not IsNothing(selectedNode)) Then
            '    selectedNode.IsSelected = True
            'End If
        End Set
    End Property
    Private m_SelectedImagePath As String
    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        For Each s As String In Directory.GetLogicalDrives()
            Dim item As New TreeViewItem()
            item.Header = s
            item.Tag = s
            item.FontWeight = FontWeights.Normal
            item.Items.Add(dummyNode)
            AddHandler item.Expanded, AddressOf folder_Expanded
            foldersItem.Items.Add(item)
        Next
    End Sub
    Private Sub folder_Expanded(sender As Object, e As RoutedEventArgs)
        Dim item As TreeViewItem = DirectCast(sender, TreeViewItem)
        If item.Items.Count = 1 AndAlso IsNothing(item.Items(0)) Then
            item.Items.Clear()
            Try
                For Each s As String In Directory.GetDirectories(item.Tag.ToString())
                    Dim subitem As New TreeViewItem()
                    subitem.Header = s.Substring(s.LastIndexOf("\") + 1)
                    subitem.Tag = s
                    subitem.FontWeight = FontWeights.Normal
                    subitem.Items.Add(dummyNode)
                    AddHandler subitem.Expanded, AddressOf folder_Expanded
                    item.Items.Add(subitem)
                Next
            Catch generatedExceptionName As Exception
            End Try
        End If
    End Sub
    Private Sub foldersItem_SelectedItemChanged(sender As Object, e As RoutedPropertyChangedEventArgs(Of Object))
        Dim tree As TreeView = DirectCast(sender, TreeView)
        Dim temp As TreeViewItem = DirectCast(tree.SelectedItem, TreeViewItem)

        If temp Is Nothing Then
            Return
        End If
        SelectedImagePath = ""
        Dim temp1 As String = ""
        Dim temp2 As String = ""
        While True
            temp1 = temp.Header.ToString()
            If temp1.Contains("\") Then
                temp2 = ""
            End If
            SelectedImagePath = temp1 & temp2 & SelectedImagePath
            If temp.Parent.[GetType]().Equals(GetType(TreeView)) Then
                Exit While
            End If
            temp = DirectCast(temp.Parent, TreeViewItem)
            temp2 = "\"
        End While
        'show user selected path
        'MessageBox.Show(SelectedImagePath)
    End Sub
End Class
'End Namespace

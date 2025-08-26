'This file contains all the methods of unpkid.f
Partial Public Class clsCom


    ''' <summary>
    ''' unpack Integer data 6 Or 9 temrs
    ''' </summary>
    ''' <param name="idu"></param>
    ''' <param name="idp"></param>
    ''' <param name="itp"></param>
    Public Sub unpkid(ByRef idu() As Integer, ByRef idp() As Integer, itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp)
        For ind1 = 1 To n
            idu(ind1) = idp(ind1)
        Next
    End Sub

    ''' <summary>
    ''' overloaded to handle 2D array
    ''' </summary>
    Public Sub unpkid(ByRef idu(,) As Integer, iduInd1 As Integer, iduInd2 As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idu(ind1 + iduInd1 - 1, iduInd2) = idp(ind1 + idpInd1 - 1, idpInd2)
        Next
    End Sub
    Public Sub unpkid(ByRef idu As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        ' idu = idp(ind1 + idpInd1, idpInd2)
        idu = idp(idpInd1, idpInd2)
    End Sub
    Public Sub unpkid(ByRef idu() As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idu(ind1) = idp(ind1 + idpInd1 - 1, idpInd2)
        Next
    End Sub

    ''' <summary>
    ''' pack Integer data 6 Or 9 terms
    ''' </summary>
    ''' <param name="idu"></param>
    ''' <param name="idp"></param>
    ''' <param name="itp"></param>
    Public Sub packid(ByRef idu() As Integer, ByRef idp() As Integer, itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idp(ind1) = idu(ind1)
        Next
    End Sub

    ''' <summary>
    ''' overloaded to handle 2D array
    ''' </summary>
    Public Sub packid(ByRef idu(,) As Integer, iduInd1 As Integer, iduInd2 As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idp(ind1 + idpInd1 - 1, idpInd2) = idu(ind1 + iduInd1 - 1, iduInd2)
        Next
    End Sub

    Public Sub packid(ByRef idu As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        ' idp(ind1 + idpInd1, idpInd2) = idu
        idp(idpInd1, idpInd2) = idu
    End Sub

    Public Sub packid(ByRef idu() As Integer,
                      ByRef idp(,) As Integer, idpInd1 As Integer, idpInd2 As Integer,
                      itp As Integer)
        Dim nit = New Integer() {6, 9}
        Dim n = nit(itp - 1)
        For ind1 = 1 To n
            idp(ind1 + idpInd1 - 1, idpInd2) = idu(ind1)
        Next
    End Sub

End Class

'  ref org fortran code 
'c
'c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'c
'      subroutine unpkid(idu, idp, itp)                                    vax
'c

'implicit Double precision (a-h, o - z)                                    dp

'c
'c     unpack Integer data 6 Or 9 temrs
'c
'      dimension idu(*), nit(2), idp(*)                                    vax
'      data nit/6,9/                                                     vax
'c

'!ik099 1111111111
'      Integer ik01
'      If (ik01.eq.0) Then
'write(88,*) 'unpkid'
'ik01 = ik01 + 1
'End If

'n = nit(itp)                                                        vax
'      Do 10 i=1,n                                                       vax
'   10 idu(i)=idp(i)                                                     vax
'      Return vax
'      entry packid(idu, idp, itp)                                         vax
'c
'c     pack Integer data 6 Or 9 terms
'c
'n = nit(itp)                                                        vax
'      Do 20 i=1,n                                                       vax
'   20 idp(i)=idu(i)                                                     vax
'      Return vax
'End                                                               vax

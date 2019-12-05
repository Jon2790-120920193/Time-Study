Imports System.IO
Imports System.Linq

Public Class Form1
    'Statistical Place Holders
    Dim OfaceOperations As Integer
    Dim UfaceOperations As Integer
    Dim VfaceOperations As Integer
    Dim HfaceOperations As Integer
    Dim OTime As Double
    Dim VTime As Double
    Dim UTime As Double
    Dim HTime As Double
    Dim OLength As Double
    Dim VLength As Double
    Dim ULength As Double
    Dim HLength As Double
    Dim ScribeMarks As Integer
    Dim AKcuts As Integer
    Dim IKcuts As Integer
    Dim Holes As Integer
    Dim Slots As Integer

    'Index related place holders
    Public Index As Double
    Public NestStart As Double
    Public XList As New List(Of Double)

    'Variables to be set based on research and user settings
    Public AvgIndexSpeed As Double = 100
    Public AvgTotalScanTime As Double = 15
    Public Scanspeed As Double = 20
    Public ApproachDistance As Double = 30
    Public PathCorrectionScanD As Double = 550
    Public NormalEnveloppeLength As Double = 550
    Public BevelEnveloppeLength As Double = 125
    Public HeightCorrectionScanD As Double = 100

    'Target .dt file place holders
    Public Path As String = ""
    Public ComArgPath() As String = Environment.GetCommandLineArgs()

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.AllowDrop = True
    End Sub

    Private Sub Form1_DragDrop(sender As System.Object, e As System.Windows.Forms.DragEventArgs) Handles Me.DragDrop
        Dim files() As String = e.Data.GetData(DataFormats.FileDrop)
        For Each Paths In files
            Path = Paths

        Next
        ListBox1.Items.Clear()
        ListBox2.Items.Clear()
        TextBox1.Text = ""
        SimDraw()
    End Sub

    Private Sub Form1_DragEnter(sender As Object, e As DragEventArgs) Handles MyBase.DragEnter
        If e.Data.GetDataPresent(DataFormats.FileDrop) Then
            e.Effect = DragDropEffects.Copy
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        ListBox1.Items.Clear()
        ListBox2.Items.Clear()
        TextBox1.Text = ""

        Dim fd As OpenFileDialog = New OpenFileDialog()

        fd.Title = "Open File Dialog"
        fd.InitialDirectory = "C:\"
        fd.Filter = "All files (*.dt)|*.dt"
        fd.FilterIndex = 2
        fd.RestoreDirectory = True

        If fd.ShowDialog() = DialogResult.OK Then
            Path = fd.FileName
            SimDraw()
        End If




    End Sub

    Public Function GetValue(ByVal Line As String, ByVal c As Integer)
        Dim a = File.ReadAllLines(Path)
        For i = 0 To a.Length - 1
            If a(i).StartsWith(Line) Then
                Return a(i + c)
            End If
        Next

        Return Nothing
    End Function

    Public Sub SimDraw()
        Try
            Dim Time As Double
            Dim InProcess As Boolean = False
            Dim Parts As New List(Of Part)


            'Nest profile parameters
            Dim power As Integer = GetValue("HYPERTHERM WEB SETTINGS", 1)
            Dim AKLead As Double = GetValue("PARTSPACING", 1) / 2
            Dim WebThick As Double = GetValue("END_LIST", 7)
            Dim FlangeThick As Double = GetValue("END_LIST", 6)
            Dim Heights As Double = GetValue("END_LIST", 5)
            Dim Widths As Double = GetValue("END_LIST", 4)
            Dim ProfileType As String = GetValue("END_LIST", 3)
            Dim NestLength As Double = GetValue("END_LIST", 2)

            'Web Settings
            Dim wcutSpeed As Integer = GetValue("HYPERTHERM WEB SETTINGS", 9)
            Dim wpierceDelay As Integer = GetValue("HYPERTHERM WEB SETTINGS", 11) / 1000

            'Flange Settings
            Dim fcutSpeed As Double = GetValue("HYPERTHERM FLANGE SETTINGS", 9)
            Dim fpierceDelay As Double = GetValue("HYPERTHERM FLANGE SETTINGS", 11) / 1000

            'Marking Settings
            Dim mcutSpeed As Double = GetValue("HYPERTHERM MARKING SETTINGS", 9)

            Dim a = File.ReadAllLines(Path)

            TextBox4.Text = "Nest Length = " + NestLength.ToString
            TextBox5.Text = "Profile Type = " + ProfileType.ToString


            For i = 0 To a.Length - 1
                Dim operationcount As Integer = 0
                If a(i) = "PART" Then
                    Dim newPart = New Part(a(i + 1), a(i + 2))
                    Parts.Add(newPart)
                    operationcount = operationcount + 1
                ElseIf a(i) = "CUTOFF_AK_O" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = fcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDCUTOFF" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next

                    ListBox2.Items.Add("*************************** AK O *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next


                    ListBox2.Items.Add("*************************** END *******************************")
                    ListBox2.Items.Add(" ")

                    l = l + 2 * AKLead
                    t = t + (2 * AKLead / cutSpeed) + fpierceDelay

                    Parts.ElementAt(operationcount).AddOperation(New Operation("AK", "O", l, t, xmin, xmax))

                ElseIf a(i) = "CUTOFF_IK_O" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = fcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDCUTOFF" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next




                    ListBox2.Items.Add("*************************** IK O *******************************")
                    Dim Coordinates As New List(Of LineSegment)


                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next


                    ListBox2.Items.Add("*************************** END *******************************")
                    ListBox2.Items.Add(" ")

                    t = t + fpierceDelay

                    Parts.ElementAt(operationcount).AddOperation(New Operation("IK", "O", l, t, xmin, xmax))

                ElseIf a(i) = "CUTOFF_AK_V" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = wcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDCUTOFF" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next

                    ListBox2.Items.Add("*************************** AK V *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next
                    ListBox2.Items.Add("*************************** END *******************************")
                    ListBox2.Items.Add(" ")

                    l = l + 2 * AKLead
                    t = t + (2 * AKLead / cutSpeed) + wpierceDelay

                    Parts.ElementAt(operationcount).AddOperation(New Operation("AK", "V", l, t, xmin, xmax))

                ElseIf a(i) = "CUTOFF_IK_V" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = wcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDCUTOFF" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next


                    ListBox2.Items.Add("*************************** IK V *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next
                    ListBox2.Items.Add("*************************** END *******************************")
                    ListBox2.Items.Add(" ")

                    t = t + wpierceDelay

                    Parts.ElementAt(operationcount).AddOperation(New Operation("IK", "V", l, t, xmin, xmax))

                ElseIf a(i) = "CUTOFF_AK_U" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = fcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDCUTOFF" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next


                    ListBox2.Items.Add("*************************** AK U *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next

                    l = l + 2 * AKLead
                    t = t + (2 * AKLead / cutSpeed) + fpierceDelay

                    ListBox2.Items.Add("*************************** END *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("AK", "U", l, t, xmin, xmax))

                ElseIf a(i) = "CUTOFF_IK_U" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = fcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDCUTOFF" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next


                    ListBox2.Items.Add("*************************** IK U *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next

                    t = t + fpierceDelay

                    ListBox2.Items.Add("*************************** END *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("IK", "U", l, t, xmin, xmax))


                ElseIf a(i) = "CUTOFF_AK_H" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = wcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDCUTOFF" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next


                    ListBox2.Items.Add("*************************** AK H *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next

                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")

                    l = l + 2 * AKLead
                    t = t + (2 * AKLead / cutSpeed) + wpierceDelay

                    Parts.ElementAt(operationcount).AddOperation(New Operation("AK", "H", l, t, xmin, xmax))

                ElseIf a(i) = "CUTOFF_IK_H" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = wcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDCUTOFF" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next


                    ListBox2.Items.Add("*************************** IK H *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next


                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")

                    t = t + wpierceDelay

                    Parts.ElementAt(operationcount).AddOperation(New Operation("IK", "H", l, t, xmin, xmax))


                ElseIf a(i) = "PIECEMARK_O" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = mcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDPIECEMARK" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next


                    ListBox2.Items.Add("*************************** Mark O *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next


                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Mark", "O", l, t, xmin, xmax))

                ElseIf a(i) = "PIECEMARK_V" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = mcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDPIECEMARK" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next


                    ListBox2.Items.Add("*************************** Mark V *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next


                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Mark", "V", l, t, xmin, xmax))

                ElseIf a(i) = "PIECEMARK_U" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = mcutSpeed



                    For j = i To a.Length - 1
                        If a(j) = "ENDPIECEMARK" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next


                    ListBox2.Items.Add("*************************** Mark U *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next


                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Mark", "U", l, t, xmin, xmax))

                ElseIf a(i) = "PIECEMARK_H" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim x As Double
                    Dim y As Double
                    Dim x2 As Double
                    Dim y2 As Double
                    Dim l As Double
                    Dim t As Double
                    Dim cutSpeed As Double = mcutSpeed


                    For j = i To a.Length - 1
                        If a(j) = "ENDPIECEMARK" Then
                            Exit For
                        End If
                        If a(j) = "POINT" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1))
                            Point.Add(segment)
                            XList.Add(segment.X)
                        ElseIf a(j) = "ARC" And Not a(j + 1) = "0" Then
                            Dim segment = New Points(a(j + 1), True)
                            Point.Add(segment)
                        End If

                        i = j
                    Next


                    ListBox2.Items.Add("*************************** Mark H *******************************")
                    Dim Coordinates As New List(Of LineSegment)

                    Dim xmin As Double
                    Dim xmax As Double
                    Dim first As Boolean = True
                    For Each p As Points In Point
                        If Point.IndexOf(p) < Point.Count - 1 Then
                            Coordinates.Add(New LineSegment(p))
                            Coordinates.ElementAt(Point.IndexOf(p)).Addpoint(Point.ElementAt(Point.IndexOf(p) + 1))
                        End If

                        If p.IsArc = False Then
                            If first = True Then
                                xmin = p.X
                                xmax = p.X
                                first = False
                            Else
                                If p.X < xmin Then
                                    xmin = p.X
                                ElseIf p.X > xmax Then
                                    xmax = p.X
                                End If
                            End If
                        End If
                    Next

                    Dim arcfound As Boolean = False
                    For Each line As LineSegment In Coordinates
                        If line.P1.IsArc = True Then
                            arcfound = True
                            l = l + ArcLength(line.P1.Diameter, line.P1.Sweep)
                            t = t + ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed
                            ListBox2.Items.Add("Arc | Length= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(ArcLength(line.P1.Diameter, line.P1.Sweep) / cutSpeed, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((cutSpeed), 2).ToString + "mm/s | Diameter = " + Math.Round(line.P1.Diameter, 2).ToString + "mm | Sweep = " + Math.Round(line.P1.Sweep).ToString + " degrees")
                        ElseIf line.P2.IsArc = True Then

                        Else

                            Dim ncutspeed As Double = cutSpeed
                            If line.P1.CutSpeedPercentage = line.P2.CutSpeedPercentage Then
                                ncutspeed = cutSpeed * line.P1.CutSpeedPercentage
                            End If

                            x = line.P1.X
                            y = line.P1.Y
                            x2 = line.P2.X
                            y2 = line.P2.Y
                            l = l + LineLength(x, y, x2, y2)
                            t = t + LineLength(x, y, x2, y2) / ncutspeed

                            ListBox2.Items.Add("Line | Length= " + Math.Round(LineLength(x, y, x2, y2), 2).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(LineLength(x, y, x2, y2) / (ncutspeed), 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round((ncutspeed), 2).ToString + "mm/s | (x, x2) = (" + Math.Round(x, 2).ToString + "," + Math.Round(x2, 2).ToString + ") | (y, y2) = (" + Math.Round(y).ToString + "," + Math.Round(y2).ToString + ")")
                        End If
                    Next


                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Mark", "H", l, t, xmin, xmax))


                ElseIf a(i) = "SLOT_O" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim slots As New Slot(a(i + 3))
                    Dim l As Double = slots.GetSlotPerimeter()
                    Dim cutSpeed As Double = fcutSpeed
                    Dim xmin As Double = slots.GetXmin
                    Dim xmax As Double = slots.GetXmax

                    Dim t As Double = l / cutSpeed
                    i = i + 3


                    ListBox2.Items.Add("*************************** SLOT O *******************************")

                    ListBox2.Items.Add("Line | Length= " + Math.Round(l).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(t, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round(cutSpeed, 2).ToString + "mm/s | Diameter = " + Math.Round(slots.Diameter, 2).ToString + "mm | Length = " + Math.Round(slots.Length).ToString)

                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Slot", "O", l, t, xmin, xmax))


                ElseIf a(i) = "SLOT_V" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim slots As New Slot(a(i + 3))
                    Dim l As Double = slots.GetSlotPerimeter()
                    Dim cutSpeed As Double = wcutSpeed
                    Dim xmin As Double = slots.GetXmin
                    Dim xmax As Double = slots.GetXmax

                    Dim t As Double = l / cutSpeed
                    i = i + 3

                    ListBox2.Items.Add("*************************** SLOT V *******************************")

                    ListBox2.Items.Add("Line | Length= " + Math.Round(l).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(t, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round(cutSpeed, 2).ToString + "mm/s | Diameter = " + Math.Round(slots.Diameter, 2).ToString + "mm | Length = " + Math.Round(slots.Length).ToString)

                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Slot", "V", l, t, xmin, xmax))


                ElseIf a(i) = "SLOT_U" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim slots As New Slot(a(i + 3))
                    Dim l As Double = slots.GetSlotPerimeter()
                    Dim cutSpeed As Double = fcutSpeed
                    Dim xmin As Double = slots.GetXmin
                    Dim xmax As Double = slots.GetXmax

                    Dim t As Double = l / cutSpeed
                    i = i + 3


                    ListBox2.Items.Add("*************************** SLOT U *******************************")

                    ListBox2.Items.Add("Line | Length= " + Math.Round(l).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(t, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round(cutSpeed, 2).ToString + "mm/s | Diameter = " + Math.Round(slots.Diameter, 2).ToString + "mm | Length = " + Math.Round(slots.Length).ToString + "mm")

                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Slot", "U", l, t, xmin, xmax))

                ElseIf a(i) = "SLOT_H" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim slots As New Slot(a(i + 3))
                    Dim l As Double = slots.GetSlotPerimeter()
                    Dim cutSpeed As Double = wcutSpeed
                    Dim xmin As Double = slots.GetXmin
                    Dim xmax As Double = slots.GetXmax

                    Dim t As Double = l / cutSpeed
                    i = i + 3


                    ListBox2.Items.Add("*************************** SLOT H *******************************")

                    ListBox2.Items.Add("Line | Length= " + Math.Round(l).ToString + "mm | Total Length= " + Math.Round(l, 2).ToString + "mm | Time= " + Math.Round(t, 2).ToString + "s | Total Time = " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round(cutSpeed, 2).ToString + "mm/s | Diameter = " + Math.Round(slots.Diameter, 2).ToString + "mm | Length = " + Math.Round(slots.Length).ToString + "mm")

                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Slot", "H", l, t, xmin, xmax))



                ElseIf a(i) = "HOLE_O" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim holes As New Hole(a(i + 7))
                    Dim d As Double = holes.Circumference
                    Dim cutSpeed As Double = fcutSpeed
                    Dim xmin As Double = holes.GetXmin
                    Dim xmax As Double = holes.GetXmax

                    Dim t As Double = d / cutSpeed
                    i = i + 7


                    ListBox2.Items.Add("*************************** HOLE O *******************************")

                    ListBox2.Items.Add("Line | Circumference= " + Math.Round(d).ToString + "mm | Time= " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round(cutSpeed, 2).ToString + "mm/s | Diameter = " + Math.Round(holes.Circumference, 2).ToString + "mm")

                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Hole", "O", d, t, xmin, xmax))


                ElseIf a(i) = "HOLE_V" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim holes As New Hole(a(i + 7))
                    Dim d As Double = holes.Circumference
                    Dim cutSpeed As Double = wcutSpeed
                    Dim xmin As Double = holes.GetXmin
                    Dim xmax As Double = holes.GetXmax

                    Dim t As Double = d / cutSpeed
                    i = i + 7


                    ListBox2.Items.Add("*************************** HOLE V *******************************")

                    ListBox2.Items.Add("Line | Circumference= " + Math.Round(d).ToString + "mm | Time= " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round(cutSpeed, 2).ToString + "mm/s | Diameter = " + Math.Round(holes.Circumference, 2).ToString + "mm")

                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Hole", "V", d, t, xmin, xmax))

                ElseIf a(i) = "HOLE_U" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim holes As New Hole(a(i + 7))
                    Dim d As Double = holes.Circumference
                    Dim cutSpeed As Double = fcutSpeed
                    Dim xmin As Double = holes.GetXmin
                    Dim xmax As Double = holes.GetXmax

                    Dim t As Double = d / cutSpeed
                    i = i + 7


                    ListBox2.Items.Add("*************************** HOLE U *******************************")

                    ListBox2.Items.Add("Line | Circumference= " + Math.Round(d).ToString + "mm | Time= " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round(cutSpeed, 2).ToString + "mm/s | Diameter = " + Math.Round(holes.Circumference, 2).ToString + "mm")

                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Hole", "U", d, t, xmin, xmax))

                ElseIf a(i) = "HOLE_H" Then
                    Dim Point As New List(Of Points)
                    Dim pointcount As Integer = 0
                    Dim holes As New Hole(a(i + 7))
                    Dim d As Double = holes.Circumference
                    Dim cutSpeed As Double = fcutSpeed
                    Dim xmin As Double = holes.GetXmin
                    Dim xmax As Double = holes.GetXmax

                    Dim t As Double = d / cutSpeed
                    i = i + 7


                    ListBox2.Items.Add("*************************** HOLE H *******************************")

                    ListBox2.Items.Add("Line | Circumference= " + Math.Round(d).ToString + "mm | Time= " + Math.Round(t, 2).ToString + "s | Speed= " + Math.Round(cutSpeed, 2).ToString + "mm/s | Diameter = " + Math.Round(holes.Circumference, 2).ToString + "mm")

                    ListBox2.Items.Add("*************************** NEW OPERATION *******************************")
                    ListBox2.Items.Add(" ")
                    Parts.ElementAt(operationcount).AddOperation(New Operation("Hole", "H", d, t, xmin, xmax))

                End If


            Next
            For Each X As Double In XList
                Try
                    If X < XList.ElementAt(X + 1) And X < NestStart Then
                        NestStart = X
                    End If
                Catch ex As Exception
                End Try
            Next
            TextBox6.Text = "Start of Nest at: " + NestStart.ToString + "mm"

            Dim XPminList As New List(Of IndexHelper)
            Dim XPmaxList As New List(Of IndexHelper)

            Parts.ElementAt(0).operationList.Sort(Function(x, y) x.Xmin.CompareTo(y.Xmin))

            For Each Op As Operation In Parts.ElementAt(0).GetOperations
                ListBox1.Items.Add(Op.Face + " " + Op.Type + " | Time= " + Math.Round(Op.Time, 3).ToString + "s | Length " + Math.Round(Op.Length, 3).ToString + "mm")
                'Add cut time from each operation
                Time = Time + Op.Time

                'Storing of X values for Index calculations in next loop

                'Considering removing these list and instead using loop of Parts.ElementAt(0) to conduct
                'Face changing and height scan loop.

                'Xmin List
                XPmaxList.Add(New IndexHelper(Op.Face, Op.Xmin))
                'Xmax List
                XPminList.Add(New IndexHelper(Op.Face, Op.Xmax))
                'Maybe for the future...
                Statify(Op)
            Next


            'Log face changing times and numbers
            'Considering using Parts.ElementAt(0).operationList in previous loop (For Each Op...)
            'Will need to check that the operationlist is being sorted, if not, a more complex strategy may be needed.
            Dim IndexTime As Double = 0
            Dim Indexnumber As Integer = 0
            Dim HeightTorchScans As Integer = 0
            Dim HeightTorchScanTime As Double = 0

            Dim Hmin As Double
            Dim Hmax As Double = XPminList.First.X + HeightCorrectionScanD


            For Each xmax As IndexHelper In XPmaxList
                Dim Imin As Double = NestStart - NormalEnveloppeLength / 2
                Dim Imax As Double = NestStart + NormalEnveloppeLength / 2
                Hmin = xmax.X

                If xmax.X >= Imax Then
                    NestStart = xmax.X
                    Time = Time + AvgTotalScanTime
                    IndexTime = IndexTime + AvgTotalScanTime
                    Indexnumber = Indexnumber + 1
                    TextBox7.Text = "Index Time = " + Math.Round(IndexTime).ToString + "s | Index Count = " + Indexnumber.ToString
                Else
                    'To Be worked. Will need to figure out how to log sub height index to different face changes + face changes
                    If Hmin >= Hmax Then
                        Hmax = Hmin + HeightCorrectionScanD
                        Time = Time + (ApproachDistance / Scanspeed)
                        HeightTorchScans = HeightTorchScans + 1
                        HeightTorchScanTime = HeightTorchScanTime + (ApproachDistance / Scanspeed)
                        TextBox8.Text = "Height Scans = " + HeightTorchScans.ToString + " | Time = " + HeightTorchScanTime.ToString
                    End If
                End If

            Next

            ListBox2.Items.Add("*************************** END *******************************")

            Time = Time + (AvgIndexSpeed / NestLength)
            TextBox3.Text = "Average Total Index Time: " + (AvgIndexSpeed / NestLength).ToString


            TextBox1.Text = "Time = " + Math.Round(Time, 2).ToString + "s"
            Dim WholeTime As Double = Math.Truncate(Time / 60)
            TextBox2.Text = "Time = " + WholeTime.ToString + "min " + Math.Ceiling((Time / 60 - WholeTime) * 60).ToString + "s"

        Catch ex As Exception
            MsgBox("There was a problem with the nest import, please verify that the .dt file opens correctly in SteelPRO Director")
            MsgBox(ex.ToString)
        End Try


    End Sub

    Public Sub Statify(ByVal Op As Operation)
        Select Case Op.Face
            Case "O"
                OfaceOperations = OfaceOperations + 1
            Case "V"
                VfaceOperations = VfaceOperations + 1
            Case "U"
                UfaceOperations = UfaceOperations + 1
            Case "H"
                HfaceOperations = HfaceOperations + 1
        End Select
    End Sub

    Public Function ArcLength(ByVal diameter As Double, ByVal sweep As Double) As Double
        Return ((sweep / 360) * Math.PI * diameter)
    End Function

    Public Function LineLength(ByVal x1 As Double, ByVal y1 As Double, ByVal x2 As Double, ByVal y2 As Double) As Double
        Return (Math.Sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2))
    End Function


End Class

Public Class IndexHelper
    Public _x As Double
    Public _f As String
    Public Sub New(ByVal face As String, ByVal x As Double)
        _f = face
        _x = x
    End Sub

    Public Property Face As String
        Get
            Return _f
        End Get
        Set(value As String)
            _f = value
        End Set
    End Property

    Public Property X As Double
        Get
            Return _x
        End Get
        Set(value As Double)
            _x = value
        End Set
    End Property
End Class

Public Class Part

    Public Property Name() As String
        Get
            Return _MyPart
        End Get
        Set(value As String)
            _MyPart = value
        End Set
    End Property

    Public Property Length() As Double
        Get
            Return _MyLength
        End Get
        Set(value As Double)
            _MyLength = value
        End Set
    End Property

    Friend _MyPart As String
    Friend _MyLength As Double

    Public Sub New(ByVal Name As String, ByVal Length As String)
        _MyPart = Name
        _MyLength = CDbl(Length)
    End Sub

    Public operationList As New List(Of Operation)
    Public Sub AddOperation(ByVal op As Operation)
        operationList.Add(op)
    End Sub

    Public Function GetOperations()
        Return operationList
    End Function
End Class

Public Class Hole
    Friend _D As Double
    Public Property Circumference() As Double
        Get
            Return _D
        End Get
        Set(value As Double)
            _D = value
        End Set
    End Property

    Friend _xposition As Double

    Public Function GetXmin()
        Return _xposition - _D / 2
    End Function

    Public Function GetXmax()
        Return _xposition + _D / 2
    End Function

    Public Sub New(ByVal InputLine As String)
        Dim result As String() = InputLine.Split(New String() {" "}, StringSplitOptions.None)
        _D = Math.Abs(CDbl(result(2)) * Math.PI)
        _xposition = result(0)
    End Sub

End Class

Public Class Slot
    Friend _diameter
    Public Property Diameter() As Double
        Get
            Return _diameter
        End Get
        Set(value As Double)
            _diameter = value
        End Set
    End Property

    Friend _length
    Public Property Length() As Double
        Get
            Return _length
        End Get
        Set(value As Double)
            _length = value
        End Set
    End Property

    Friend XPosition As Double

    Public Function GetXmin()
        Return XPosition - _diameter / 2
    End Function

    Public Function GetXmax()
        Return XPosition + _diameter / 2 + _length
    End Function

    Public Function GetSlotPerimeter() As Double
        Return (2 * _length) + (_diameter * Math.PI)
    End Function

    Public Sub New(ByVal InputLine As String)
        Dim result As String() = InputLine.Split(New String() {" "}, StringSplitOptions.None)
        _length = CDbl(result(3))
        _diameter = CDbl(result(2))
        XPosition = CDbl(result(0))
    End Sub
End Class

Public Class Points

    Friend _curved As Boolean
    Public Property IsArc() As Boolean
        Get
            Return _curved
        End Get
        Set(value As Boolean)
            _curved = value
        End Set
    End Property

    Friend _d As Double
    Public Property Diameter() As Double
        Get
            Return _d
        End Get
        Set(value As Double)
            _d = value
        End Set
    End Property

    Friend _s As Double
    Public Property Sweep() As Double
        Get
            Return _s
        End Get
        Set(value As Double)
            _s = value
        End Set
    End Property

    Friend _X As Double
    Public Property X() As Double
        Get
            Return _X
        End Get
        Set(value As Double)
            _X = value
        End Set
    End Property

    Friend _Y As Double
    Public Property Y() As Double
        Get
            Return _Y
        End Get
        Set(value As Double)
            _Y = value
        End Set
    End Property

    Friend _CutSP As Double
    Public Property CutSpeedPercentage() As Double
        Get
            Return _CutSP
        End Get
        Set(value As Double)
            _CutSP = value
        End Set
    End Property

    Public Sub New(ByVal InputLine As String, Optional ByVal Arc As Boolean = False)
        Dim result As String() = InputLine.Split(New String() {" "}, StringSplitOptions.None)

        If Arc = False Then
            _curved = False
            _X = CDbl(result(0))
            _Y = CDbl(result(1))
            _CutSP = CDbl(result(4))
        Else
            _curved = True
            _s = Math.Abs(CDbl(result(3)))
            _d = Math.Abs(CDbl(result(4)))
        End If
    End Sub
End Class

Public Class Operation
    Friend _type As String
    Public Property Type() As String
        Get
            Return _type
        End Get
        Set(value As String)
            _type = value
        End Set
    End Property

    Friend _face As String
    Public Property Face() As String
        Get
            Return _face
        End Get
        Set(value As String)
            _face = value
        End Set
    End Property

    Friend _length As Double
    Public Property Length() As Double
        Get
            Return _length
        End Get
        Set(value As Double)
            _length = value
        End Set
    End Property

    Friend _time As Double
    Public Property Time() As Double
        Get
            Return _time
        End Get
        Set(value As Double)
            _time = value
        End Set
    End Property

    Friend _xmin As Double
    Public Property Xmin() As Double
        Get
            Return _xmin
        End Get
        Set(value As Double)
            _xmin = value
        End Set
    End Property

    Friend _xmax As Double
    Public Property Xmax() As Double
        Get
            Return _xmax
        End Get
        Set(value As Double)
            _xmax = value
        End Set
    End Property

    Public Sub New(ByVal Type As String, ByVal Face As String, ByVal Length As Double, ByVal Time As Double, ByVal Xmin As Double, ByVal Xmax As Double)
        _type = Type
        _face = Face
        _length = Length
        _time = Time
        _xmin = Xmin
        _xmax = Xmax
    End Sub
End Class

Public Class LineSegment

    Friend _p As Points
    Friend _p2 As Points
    Friend ispopulated As Boolean

    Public Sub New(ByVal point As Points)
        _p = point
        ispopulated = True
    End Sub

    Public Sub Addpoint(ByVal point As Points)
        _p2 = point
    End Sub

    Public Property P1() As Points
        Get
            Return _p
        End Get
        Set(value As Points)
            _p = value
        End Set
    End Property

    Public Property P2() As Points
        Get
            Return _p2
        End Get
        Set(value As Points)
            _p2 = value
        End Set
    End Property

    Public Property IsPop() As Boolean
        Get
            Return ispopulated
        End Get
        Set(value As Boolean)
            ispopulated = True
        End Set
    End Property
End Class

Public Class PieceStatistic
    Friend _face As String
    Friend _type As String
    Friend _count As Double
    Friend _time As Double



    Public Sub New(ByVal op As Operation)

    End Sub
End Class

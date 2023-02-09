Rem                     Tunnelbreaker (TB 1.0)
Rem                   Richtiger Dateiname:  TB~8.bas
Rem              Zeilen (nach letzteer Berechnung):  1100

Rem             sichtbare Teleporter immer Farbe 11
Rem             sichtabre Munitionslager immer Farbe 10
Rem             sichtabre Punktelinien immer Farbe 13
Rem             sichtabre Wegschalter immer Farbe 8


DECLARE SUB highscore ()
DECLARE SUB pfadanlegen ()
DECLARE SUB apohighs ()
DECLARE SUB highscoreliste ()
DECLARE SUB rechte ()
DECLARE SUB Mapeditor ()
DECLARE SUB laden ()
DECLARE SUB level4 ()
DECLARE SUB inhaltsperre ()
DECLARE SUB beamnow ()
DECLARE SUB beam ()
DECLARE SUB bild ()
DECLARE SUB slvz ()
DECLARE SUB lvz ()
DECLARE SUB HILFE ()
DECLARE SUB BONUS ()
DECLARE SUB muni ()
DECLARE SUB pause ()
DECLARE SUB bombe ()
DECLARE SUB schuss ()
DECLARE SUB lg ()
DECLARE SUB level3 ()
DECLARE SUB block1 ()
DECLARE SUB start ()
DECLARE SUB Level2 ()
DECLARE SUB musik ()
DECLARE SUB level1 ()
Dim Shared beam2%
Dim Shared num%
Dim Shared x1
Dim Shared y1
Dim Shared x2
Dim Shared y2
Dim Shared z!
Dim Shared x3
Dim Shared modus
Dim Shared y3
Dim Shared n1%, n2%, n3%, n4%, n5%
Dim Shared m1%, m2%, m3%, m4%, m5%
Dim Shared s%
Dim Shared ap1$, ap2$, ap3$, ap4$, ap5$
Dim Shared np1$, np2$, np3$, np4$, np5$
Dim Shared platz1!, platz2!, platz3!, platz4!, platz5!
Dim Shared apoca1!, apoca2!, apoca3!, apoca4!, apoca5!
Dim Shared v1
Dim Shared levelll%
Dim Shared farbcode!
Dim Shared h1
Dim Shared v2
Dim Shared h2
Dim Shared v
Dim Shared h
Dim Shared inhaltsvz%
Dim Shared x4
Dim Shared y4
Dim Shared x5
Dim Shared y5
Dim Shared x6
Dim Shared y6
Dim Shared sperre$
Dim Shared bom%
Dim Shared punkte%
Dim Shared punkte2%
Dim Shared F%
Dim Shared f3%
Dim Shared zzz!
Dim Shared bz!
Dim Shared cl%
Dim Shared cl2%
Dim Shared xbeam
Dim Shared sound$
Dim Shared ort$
Dim Shared ort2$
Call rechte
Call pfadanlegen
bz! = 0
zzz! = 0
cl% = 13
cl2% = 13
F% = 9 'Blockfarbe
f3% = 11
f31% = 11
mapladen = 0
sperre$ = "ja" 'Sperre: Schuss kann (/nicht) verwendet werden
punkte% = 0
s% = 0

Cls
Screen 12
Print "Ton an?"
Print
Input "", sound$
Cls
Color 4
Print "                                HAUPTMENö       "
d = Timer + 1: Do While Timer < d: Loop
Hauptmenu:
View Print
Color 2
Print
Print
Print "Modus wÑhlen:"
Print
Print " 1. Gegen die Zeit"
Print " 2. Ans Ziel kommen - mit allen Mitteln"
Print " 3. Level selbst Zeichnen"
Print " 4. Level laden"
Print " 5. Highscoreliste"
Do
    taste$ = InKey$
    If taste$ = "1" Then modus = 1
    If taste$ = "2" Then modus = 2
    If taste$ = "3" Then
        Call Mapeditor
        GoTo Hauptmenu
    End If
    If taste$ = "4" Then mapladen = 1
    If taste$ = "5" Then
        Call highscoreliste
        GoTo Hauptmenu
    End If
Loop Until taste$ <> ""
If modus = 1 Then
    sperre$ = "ja"
    Print
    Color 4
    Print "Zeitmodus aktiviert!"
End If
If modus = 2 Then
    sperre$ = "nein"
    s% = 1
    Color 4
    Print
    Print "Apokalypsemodus aktiviert!"
End If
Cls
Color 8
Locate 30, 50
Print "(ESC um Intro zu Åberspringen)"
Sleep 5
Cls
If sound$ = "ja" Then Call musik
anfang2:
s% = 1
punkte2% = 0
cl% = 13
punkte% = 0
taste$ = InKey$
Screen 12
If mapladen = 1 Then GoTo inhalt
Color 2
Print "Levelwahl mit jeweiliger Nummer!"
Color 3
Print "Zur öbersicht mit allen Tasten au·er denen, die ein Level starten (1,2,3,...)"
Do
    taste$ = InKey$
    If taste$ = "1" Then
        level$ = "level1"
        num% = 1
    End If
    If taste$ = "2" Then
        level$ = "level2"
        num% = 2
    End If
    If taste$ = "3" Then
        level$ = "level3"
        num% = 3
    End If
    If taste$ <> "1" And taste$ <> "2" And taste$ <> "3" Then level$ = "l"
Loop Until taste$ <> ""
z! = .07

inhalt:
Call muni
Call start
If level$ = "level1" Then
    num% = 1
    Call level1
End If
If level$ = "level2" Then
    num% = 2
    Call Level2
End If
If level$ = "level3" Then
    num% = 3
    Call level3
End If
If level$ = "l" Then
    Call slvz
    Call lvz
End If
xbeam = 0
If mapladen = 1 Then
    num% = 999
    Call laden
    Call start
End If
Rem  Steuerung - Level 1
steuerung:
runde% = 1
x1 = 15
x2 = 25
y1 = 320
y2 = 310
Call block1
Call BONUS
If num% < 0 Then Call start
Do
    taste$ = InKey$
    If taste$ = "1" Then a! = 1
    If taste$ = "2" Then a! = 2
    If taste$ = Chr$(0) + ";" Then
        Call HILFE
        GoTo anfang2
    End If
Loop Until taste$ = "1" Or taste$ = "2"

steuerung2:
Line (x1, y1)-(x2, y2), F%, BF
Timer On
zzz! = Timer
Do
    taste2$ = InKey$
    If taste2$ <> "" Then taste$ = taste2$

    y1 = y1: x1 = x1: x2 = x1 + 10: y2 = y1 - 10: x3 = x2: x4 = x3: y3 = y2: y4 = y3: y5 = y4: x5 = x4
    beam2% = 0
    If taste$ = "p" Then Call pause
    If taste$ = Chr$(0) + "M" Then 'rechts
        Do
            taste$ = InKey$
            GoSub ziele
            Call inhaltsperre
            Call muni
            Call beam: If xbeam = 100 And taste$ = "x" Then Call beamnow
            x3 = x2 + 2
            x4 = x3 + 1
            x5 = x4 - 1
            x6 = x5 - 1
            y4 = y1 - 5
            y5 = y1 + 1
            y6 = y1 - 11
            If s% > 0 Then
                v1 = 1
                h1 = 6
                v2 = 0
                h2 = -6
                v = 1
                h = 0
                If taste$ = Chr$(32) Then Call schuss
            End If
            If s% > 1 And taste$ = "b" Then Call bombe
            If taste$ = Chr$(0) + ";" Then Call HILFE
            If taste$ = "p" Then Call pause
            x1 = x1 + 2
            x2 = x2 + 2
            Line (x1 - a!, y1)-(x2 - a!, y2), 0, BF
            Line (x1, y1)-(x2, y2), F%, BF
            d = Timer + z!: Do While d > Timer: Loop
            If taste$ = Chr$(27) Then End
            If taste$ = "n" Then GoTo anfang2
            If taste$ = "+" Then z! = z! - .01
            If taste$ = "-" Then z! = z! + .01
            If z! < .005 Then z! = .005
            If z! > .2 Then z! = .2
            z! = z!
            If taste$ = Chr$(13) Then z! = .0005
            If Point(x1 - 1, y1 + 1) = 6 Or Point(x1 - 1, y1 + 1) = 4 Then GoTo ende
            If Point(x2 + 1, y2 - 1) = 6 Or Point(x2 + 1, y2 - 1) = 4 Then GoTo ende
            If Point(x1 - 1, y1 - 11) = 6 Or Point(x1 - 1, y1 - 11) = 4 Then GoTo ende
            If Point(x2 + 1, y2 + 11) = 6 Or Point(x2 + 1, y2 + 11) = 4 Then GoTo ende
        Loop Until taste$ = Chr$(0) + "P" Or taste$ = Chr$(0) + "K" Or taste$ = Chr$(0) + "H"
    End If
    y1 = y1: x1 = x1: x2 = x1 + 10: y2 = y1 - 10: x3 = x2: x4 = x3: y3 = y2: y4 = y3: y5 = y4: x5 = x4
    beam2% = 0
    If taste$ = Chr$(0) + "P" Then 'runter
        Do
            taste$ = InKey$
            Call muni
            Call inhaltsperre
            Call beam: If xbeam = 100 And taste$ = "x" Then Call beamnow
            GoSub ziele
            y3 = y1 + 2
            y4 = y3 + 1
            y5 = y4 - 1
            y6 = y5 - 1
            x4 = x1 + 5
            x5 = x1 - 1
            x6 = x1 + 11
            If taste$ = "p" Then Call pause
            If s% > 0 Then
                v1 = 6
                h1 = 1
                h2 = 0
                v2 = -6
                v = 0
                h = 1
                If taste$ = Chr$(32) Then Call schuss
            End If
            If s% > 1 And taste$ = "b" Then Call bombe
            If taste$ = Chr$(0) + ";" Then Call HILFE
            y1 = y1 + 2
            y2 = y2 + 2
            Line (x1, y1 - a!)-(x2, y2 - a!), 0, BF
            Line (x1, y1)-(x2, y2), F%, BF
            d = Timer + z!: Do While d > Timer: Loop
            If taste$ = Chr$(27) Then End
            If taste$ = "n" Then GoTo anfang2
            If taste$ = "+" Then z! = z! - .01
            If taste$ = "-" Then z! = z! + .01
            If z! < .005 Then z! = .005
            If z! > .2 Then z! = .2
            z! = z!
            If taste$ = Chr$(13) Then z! = .0005
            If Point(x1 - 1, y1 + 1) = 6 Or Point(x1 - 1, y1 + 1) = 4 Then GoTo ende
            If Point(x2 + 1, y2 - 1) = 6 Or Point(x2 + 1, y2 - 1) = 4 Then GoTo ende
            If Point(x1 - 1, y1 - 11) = 6 Or Point(x1 - 1, y1 - 11) = 4 Then GoTo ende
            If Point(x2 + 1, y2 + 11) = 6 Or Point(x2 + 1, y2 + 11) = 4 Then GoTo ende
        Loop Until taste$ = Chr$(0) + "M" Or taste$ = Chr$(0) + "K" Or taste$ = Chr$(0) + "H"
    End If
    y1 = y1: x1 = x1: x2 = x1 + 10: y2 = y1 - 10: x3 = x2: x4 = x3: y3 = y2: y4 = y3: y5 = y4: x5 = x4
    beam2% = 0
    If taste$ = Chr$(0) + "K" Then 'links
        Do
            taste$ = InKey$
            Call muni
            Call beam: If xbeam = 100 And taste$ = "x" Then Call beamnow
            GoSub ziele
            x3 = x1 - 2
            x4 = x3 - 1
            x5 = x4 + 1
            x6 = x5 + 1
            y4 = y1 - 5
            y5 = y1 + 1
            y6 = y1 - 11
            If s% > 0 Then
                v1 = -1
                h1 = -6
                v2 = 0
                h2 = 6
                v = -1
                h = 0
                If taste$ = Chr$(32) Then Call schuss
            End If
            Call inhaltsperre
            If taste$ = Chr$(0) + ";" Then Call HILFE
            If s% > 1 And taste$ = "b" Then Call bombe
            If taste$ = "p" Then Call pause
            x1 = x1 - 2
            x2 = x2 - 2
            Line (x1 + a!, y1)-(x2 + a!, y2), 0, BF
            Line (x1, y1)-(x2, y2), F%, BF
            d = Timer + z!: Do While d > Timer: Loop
            If taste$ = Chr$(27) Then End
            If taste$ = "n" Then GoTo anfang2
            If taste$ = "+" Then z! = z! - .01
            If taste$ = "-" Then z! = z! + .01
            If z! < .005 Then z! = .005
            If z! > .2 Then z! = .2
            z! = z!
            If taste$ = Chr$(13) Then z! = .0005
            If Point(x1 - 1, y1 + 1) = 6 Or Point(x1 - 1, y1 + 1) = 4 Then GoTo ende
            If Point(x2 + 1, y2 - 1) = 6 Or Point(x2 + 1, y2 - 1) = 4 Then GoTo ende
            If Point(x1 - 1, y1 - 11) = 6 Or Point(x1 - 1, y1 - 11) = 4 Then GoTo ende
            If Point(x2 + 1, y2 + 11) = 6 Or Point(x2 + 1, y2 + 11) = 4 Then GoTo ende
        Loop Until taste$ = Chr$(0) + "P" Or taste$ = Chr$(0) + "M" Or taste$ = Chr$(0) + "H"
    End If
    y1 = y1: x1 = x1: x2 = x1 + 10: y2 = y1 - 10: x3 = x2: x4 = x3: y3 = y2: y4 = y3: y5 = y4: x5 = x4
    beam2% = 0
    If taste$ = Chr$(0) + "H" Then 'hoch
        Do
            taste$ = InKey$
            GoSub ziele
            Call beam: If xbeam = 100 And taste$ = "x" Then Call beamnow
            Call inhaltsperre
            Call muni
            y3 = y2 - 2
            y4 = y3 - 1
            y5 = y4 + 1
            y6 = y5 + 2
            x4 = x1 + 5
            x5 = x1 - 1
            x6 = x1 + 11
            If s% > 0 Then
                v1 = -6
                h1 = -1
                v2 = 6
                h2 = 0
                v = 0
                h = -1
                If taste$ = Chr$(32) Then Call schuss
            End If
            If taste$ = Chr$(0) + ";" Then Call HILFE
            If s% > 1 And taste$ = "b" Then Call bombe
            If taste$ = "p" Then Call pause
            y1 = y1 - 2
            y2 = y2 - 2
            Line (x1, y1 + a!)-(x2, y2 + a!), 0, BF
            Line (x1, y1)-(x2, y2), F%, BF
            d = Timer + z!: Do While d > Timer: Loop
            If taste$ = Chr$(27) Then End
            If taste$ = "n" Then GoTo anfang2
            If taste$ = "+" Then z! = z! - .01
            If taste$ = "-" Then z! = z! + .01
            If z! < .005 Then z! = .005
            If z! > .2 Then z! = .2
            z! = z!
            If taste$ = Chr$(13) Then z! = .0005
            If Point(x1 - 1, y1 + 1) = 6 Or Point(x1 - 1, y1 + 1) = 4 Then GoTo ende
            If Point(x2 + 1, y2 - 1) = 6 Or Point(x2 + 1, y2 - 1) = 4 Then GoTo ende
            If Point(x1 - 1, y1 - 11) = 6 Or Point(x1 - 1, y1 - 11) = 4 Then GoTo ende
            If Point(x2 + 1, y2 + 11) = 6 Or Point(x2 + 1, y2 + 11) = 4 Then GoTo ende
        Loop Until taste$ = Chr$(0) + "P" Or taste$ = Chr$(0) + "K" Or taste$ = Chr$(0) + "M"
    End If
 
    If taste$ = Chr$(27) Then End
    If taste$ = "n" Then GoTo anfang2
    Call muni
Loop
End
         
                                                     
Rem                     Subroutinen

ziele:
taste2$ = InKey$
If taste2$ <> "" Then taste$ = taste2$


Rem                     ZIELE / GADGES
Call BONUS

If Point(10, 331) = 0 Then 'Level 1
    levelll% = 1
    If x2 > 550 And y2 < 40 Then
        x1 = 15: x2 = 25: y1 = 320: y2 = 310
        Call lg
        Call start
        Call block1
        Call Level2
        GoTo steuerung
    End If
End If

If Point(10, 331) = 2 Then 'Level 2
    levelll% = 2
    If x2 > 500 And y1 < 31 Then 'Falle
        Line (480, 29)-(485, 10), 4, BF
        Line (540, 29)-(535, 10), 4, BF
    End If
 
    If x2 > 610 And y2 < 70 And y2 > 60 Then
        Line (x1, y1)-(x2, y2), 0, BF
        x1 = 150: y1 = 60: x2 = 160: y2 = 50 'Teleport (Ziel)
        x1 = x1: x2 = x2: y1 = y1: y2 = y2
        Line (599, 70)-(570, 60), 0, BF
        GoTo steuerung2
    End If

    If Point(x5, y5) = 11 Then
        Line (x1, y1)-(x2, y2), 0, BF 'Teleport Weg 2
        x1 = 90: x2 = 100: y1 = 190: y2 = 180
        GoTo steuerung2
    End If

    If x2 > 550 And y2 < 40 Then 'Ziel
        x1 = 15: x2 = 25: y1 = 320: y2 = 310
        Call lg
        Call start
        Call block1
        Call level3
        GoTo steuerung
    End If
End If

If Point(10, 331) = 2 + 4 Then 'Inhaltsverzeichnis
    If Point(x4, y4) = 1 Then
        Line (x1, y1)-(x2, y2), 0, BF
        num% = 1
        Call bild
        level$ = "level1"
        GoTo inhalt
    End If
    If Point(x4, y4) = 2 Then
        Line (x1, y1)-(x2, y2), 0, BF
        num% = 2
        Call bild
        level$ = "level2"
        GoTo inhalt
    End If
    If Point(x4, y4) = 3 Then
        Line (x1, y1)-(x2, y2), 0, BF
        num% = 3
        Call bild
        level$ = "level3"
        GoTo inhalt
    End If
End If

If Point(10, 331) = 3 Then 'Level 3
    If runde% = 1 Then
        If f3% = 3 Then f3% = 11
        If f31% = 11 Then f31% = 3
        runde% = 0
    End If
    If runde% = 2 Then
        If f3% = 11 Then f3% = 3
        If f31% = 3 Then f31% = 11
        runde% = 1
    End If
    If runde% = 0 Then runde% = 2

    Line (15, 195)-(25, 205), f3%, BF 'Teleport links
    Line (335, 195)-(325, 205), f31%, BF 'Teleport rechts
    If Point(x4, y4) = 3 Then 'Teleport 1
        Line (x1, y1)-(x2, y2), 0, BF
        x1 = 20: x2 = 30: y1 = 30: y2 = 20
        runde% = 3
        GoTo steuerung2
    End If
    If Point(x4, y4) = 11 Then 'Teleport 2
        Line (x1, y1)-(x2, y2), 0, BF
        x1 = 610: x2 = 620: y1 = 315: y2 = 305
        runde% = 3
        GoTo steuerung2
    End If
    If Point(x4, y4) = 8 Then Line (600, 230)-(629, 240), 0, BF 'Wegschaltung
    If Point(x4, y4) = 10 Then s% = 2
End If

If Point(10, 331) = 4 Then 'Level 4
    If x1 < 30 And y1 > 260 And y1 < 280 Then
    End If
End If
Return

If num% < 0 Then 'Map Creator
    If Point(x4, y4) = 15 Then Call lg
End If



ende:
View Print 23 To 30
If sound$ = "nein" Then GoTo 448
Play "f <  a < c"
448 Color 4
Cls 2
Print
Print "Crash!"
d = Timer + 3: Do While d > Timer: Loop
Print "Tja, leider verloren. Aber veruschs doch nochmal. Neustart mit 'N', beenden mit ESC."
Color 15
Line (x1, y1)-(x2, y2), 0, BF
Do
    taste$ = InKey$
    If taste$ = "n" Then GoTo anfang2
    If taste$ = Chr$(27) Then End
Loop

Sub apohighs
    Close
    d = Timer + 1: Do While Timer < d: Loop
    Open "c:\TB\score2.txt" For Input Access Read Write As #4
    Input #4, apoca1!, apoca2!, apoca3!, apoca4!, apoca5!
    Input #4, ap1$, ap2$, ap3$, ap4$, ap5$
    Input #4, n1%, n2%, n3%, n4%, n5%
    If bz! < apoca1! Or apoca1! = 0 Then
        n5% = n4%
        n4% = n3%
        n3% = n2%
        n2% = n1%
        n1% = num%
        apoca5! = apoca4!
        apoca4! = apoca3!
        apoca3! = apoca2!
        apoca2! = apoca1!
        apoca1! = bz!
        Cls
        ap5$ = ap4$
        ap4$ = ap3$
        ap3$ = ap2$
        ap2$ = ap1$
        Input "Name: ", ap1$
        GoTo hierher
    End If
    If bz! < apoca2! And bz! > apoca1! Or apoca2! = 0 Then
        n5% = n4%
        n4% = n3%
        n3% = n2%
        n2% = num%
        apoca5! = apoca4!
        apoca4! = apoca3!
        apoca3! = apoca2!
        apoca2! = bz!
        ap5$ = ap4$
        ap4$ = ap3$
        ap3$ = ap2$
        Cls
        Input "Name: ", ap2$
        GoTo hierher
    End If
    If bz! < apoca3! And bz! > apoca2! Or apoca3! = 0 Then
        n5% = n4%
        n4% = n3%
        n3% = num%
        apoca5! = apoca4!
        apoca4! = apoca3!
        ap5$ = ap4$
        ap4$ = ap3$
        apoca3! = bz!
        Cls
        Input "Name: ", ap3$
        GoTo hierher
    End If
    If bz! < apoca4! And bz! > apoca3! Or apoca4! = 0 Then
        n5% = n4%
        n4% = num%
        apoca5! = apoca4!
        ap5$ = ap4$
        apoca4! = bz!
        Cls
        Input "Name: ", ap4$
        GoTo hierher
    End If
    If bz! < apoca5! And bz! > apoca4! Or apoca5! = 0 Then
        n5% = num%
        apoca5! = bz!
        Cls
        Input "Name: ", ap5$
        GoTo hierher
    End If
    hierher:
    Close #4
    Open "c:\TB\score2.txt" For Output As #3
    Write #3, apoca1!
    Write #3, apoca2!
    Write #3, apoca3!
    Write #3, apoca4!
    Write #3, apoca5!
    Write #3, ap1$
    Write #3, ap2$
    Write #3, ap3$
    Write #3, ap4$
    Write #3, ap5$
    Write #3, n1%, n2%, n3%, n4%, n5%
    Close #3
End Sub

Sub beam
    If inhaltsvz% = 1 Then GoTo 99
    If sperre$ <> "ja" Then
        taste$ = InKey$
        beam2% = beam2% + 1
        If beam2% > 39 And xbeam < 100 Then
            Line (343, 403)-(343 + xbeam, 413), 5, BF
            xbeam = xbeam + 1
        End If
        If xbeam > 100 Then xbeam = 100
        If xbeam = 100 Then Line (343, 403)-(343 + xbeam, 413), 13, BF
    End If
99 End Sub

Sub beamnow
    Line (x1, y1)-(x2, y2), 0, BF
    x1 = x1 + 40 * v
    x2 = x2 + 40 * v
    y1 = y1 + 40 * h
    y2 = y2 + 40 * h
    Line (x1, y1)-(x2, y2), F%, BF
    Line (343, 403)-(443, 413), 0, BF
    xbeam = 0
End Sub

Sub bild
    View Print
    Cls
    Screen 12
    Color 2
    Print "                           Teleport zu Level"; num%; "aktiviert"
    inhaltsvz% = 0
    r% = 5
    Do
        r% = r% + 1
        co% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
        Circle (320, 240), r%, co%
        d = Timer + .005: Do While Timer < d: Loop
        r% = r% + 1
        co% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
        Circle (320, 240), r%, co%
        d = Timer + .005: Do While Timer < d: Loop
        r% = r% + 1
        co% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
        Circle (320, 240), r%, co%
        d = Timer + .005: Do While Timer < d: Loop
    Loop Until r% > 180
    Do
        r% = r% - 1
        co% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
        Circle (320, 240), r%, co%
        d = Timer + .001: Do While Timer < d: Loop
        Circle (320, 240), r% + 15, 0
    Loop Until r% = 10
    xx = 320
    yy = 240
    xx2 = xx
    yy2 = yy
    Do
        xx = xx - 1
        xx2 = xx2 + 1
        yy = yy - 1
        yy2 = yy2 + 1
        co% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
        If Rnd + Rnd + Rnd > 2 Then co% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
        Line (320, 240)-(xx2, 240), co%
        Line (320, 240)-(xx, 240), co%
        Line (320, 240)-(320, yy), co%
        Line (320, 240)-(320, yy2), co%
        d = Timer + .001: Do While Timer < d: Loop
    Loop Until xx = 0

    xx = 320
    yy = 240
    xx2 = xx
    yy2 = yy
    co% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
    Do
        xx = xx - 1
        xx2 = xx2 + 1
        yy = yy - 1
        yy2 = yy2 + 1
        Line (320, 240)-(xx2, 0), co%, BF
        Line (320, 240)-(xx, 0), co%, BF
        Line (320, 240)-(xx, 480), co%, BF
        Line (320, 240)-(xx2, 480), co%, BF
        Line (320, 240)-(0, yy), co%, BF
        Line (320, 240)-(0, yy2), co%, BF
        Line (320, 240)-(640, yy), co%, BF
        Line (320, 240)-(640, yy2), co%, BF
        d = Timer + .01: Do While Timer < d: Loop
    Loop Until xx = 315
    co1% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
    co2% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
    co% = co1% - co2%
    If co% < 1 Then co% = co2% + co1%
    If co% > 14 Then co% = co2% - co1%
    If co% < 1 Or co% > 14 Then co% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
    Do
        xx = xx - 5
        xx2 = xx2 + 5
        yy = yy - 5
        yy2 = yy2 + 5
        Line (320, 240)-(xx2, 0), co%, BF
        Line (320, 240)-(xx, 0), co%, BF
        Line (320, 240)-(xx, 480), co%, BF
        Line (320, 240)-(xx2, 480), co%, BF
        Line (320, 240)-(0, yy), co%, BF
        Line (320, 240)-(0, yy2), co%, BF
        Line (320, 240)-(640, yy), co%, BF
        Line (320, 240)-(640, yy2), co%, BF
    Loop Until xx < 0
    r% = 0
    Do
        r% = r% + 1
        Circle (320, 240), r%, 0
    Loop Until r% > 400
    d = Timer + 3: Do While Timer < d: Loop
    zeit = Timer
End Sub

Sub block1
    punkte2% = 0
    Rem Block
    Line (x1, y1)-(x2, y2), F%, BF

    Rem             Rahmen
    Screen 12
    taste$ = InKey$
    Line (0, 0)-(640, 10), 4, BF
    Line (0, 0)-(10, 325), 4, BF
    Line (0, 330)-(630, 325), 4, BF
    Line (640, 330)-(630, 10), 4, BF

End Sub

Sub bombe
    Do
        x4 = x4 + v
        y4 = y4 + h
        Circle (x4, y4), 2, 12
        Paint (x4, y4), 12
        d = Timer + .003: Do While d > Timer: Loop
        Paint (x4, y4), 0
    Loop Until Point(x4 + (4 * v), y4 + (4 * h)) <> 0
    r% = 2
    For i = 1 To 28
        r% = r% + 1
        Circle (x4, y4), r%, 12
        d = Timer + .005: Do While Timer < d: Loop
        Circle (x4, y4), r% - 3, 0
    Next i
    Paint (x4, y4), 0, 12
    r% = 1
    For i = 1 To 30
        r% = r% + 1
        Circle (x4, y4), r%, 0
        Line (x4 - r% / 1.5, y4 - r% / 1.5)-(x4 + r% / 1.5, y4 + r% / 1.5), 0, BF
    Next i
    s% = s% - 2
End Sub

Sub BONUS
    If sperre$ <> "ja" Then
        If Point(10, 331) = 0 Then 'Level 1
            Line (270, 40)-(270, 90), cl%
            Line (130, 170)-(170, 170), cl%
            Line (500, 280)-(500, 300), cl%
            Line (270, 100)-(270, 130), cl%
            Line (580, 180)-(580, 205), cl%
            If punkte2% = 3 Then cl% = 5
        End If
 
        If Point(10, 331) = 2 Then 'Level 2
            Line (510, 250)-(510, 230), cl%
            Line (200, 210)-(200, 225), cl%
            Line (260, 40)-(260, 70), cl%
            Line (180, 150)-(180, 170), cl%
            Line (240, 260)-(240, 280), cl%
            If punkte2% = 3 Then cl% = 5
        End If
 
        If Point(10, 331) = 3 Then 'Level 3
            Line (100, 10)-(100, 150), cl%
            Line (400, 160)-(430, 160), cl%
            Line (500, 290)-(500, 270), cl%
            Line (500, 180)-(480, 200), cl%
            If punkte2% = 3 Then cl% = 5
        End If

        If Point(x4, y4) = 13 Or Point(x3, y3) = 13 Or Point(x5, y5) = 13 Then
            punkte% = punkte% + 1
            punkte2% = punkte2% + 1
        End If
        If punkte% > 7 Then
            s% = s% + 1
            punkte% = punkte% - 8
        End If
    End If
End Sub

Sub highscore
    Cls
    d = Timer + 1: Do While Timer < d: Loop
    Open "c:\TB\score.txt" For Input As #3
    Input #3, platz1!, platz2!, platz3!, platz4!, platz5!
    Input #3, np1$, np2$, np3$, np4$, np5$
    Input #3, m1%, m2%, m3%, m4%, m5%
    If bz! < platz1! Or platz1! = 0 Then
        m5% = m4%
        m4% = m3%
        m3% = m2%
        m2% = m1%
        m1% = num%
        platz5! = platz4!
        platz4! = platz3!
        platz3! = platz2!
        platz2! = platz1!
        platz1! = bz!
        np5$ = np4$
        np4$ = np3$
        np3$ = np2$
        np2$ = np1$
        Input "Name: ", np1$
        GoTo hierher2
    End If
    If bz! < platz2! And bz! > platz1! Or platz2! = 0 Then
        m5% = m4%
        m4% = m3%
        m3% = m2%
        m2% = num%
        np5$ = np4$
        np4$ = np3$
        np3$ = np2$
        platz5! = platz4!
        platz4! = platz3!
        platz3! = platz2!
        platz2! = bz!
        Input "Name: ", np2$
        GoTo hierher2
    End If
    If bz! < platz3! And bz! > platz2! Or platz3! = 0 Then
        m5% = m4%
        m4% = m3%
        m3% = num%
        platz5! = platz4!
        platz4! = platz3!
        np5$ = np4$
        np4$ = np3$
        platz3! = bz!
        Input "Name: ", np3$
        GoTo hierher2
    End If
    If bz! < platz4! And bz! > platz3! Or platz4! = 0 Then
        m5% = m4%
        m4% = num%
        platz5! = platz4!
        np5$ = np4$
        platz4! = bz!
        Input "Name: ", np4$
        GoTo hierher2
    End If
    If bz! < platz5! And bz! > platz4! Or platz5! = 0 Then
        m5% = num%
        platz5! = bz!
        Input "Name: ", np5$
        GoTo hierher2
    End If
    hierher2:
    Close
    Open "c:\TB\score.txt" For Output Access Read Write As #5
    Write #5, platz1!, platz2!, platz3!, platz4!, platz5!
    Write #5, np1$, np2$, np3$, np4$, np5$
    Write #5, m1%, m2%, m3%, m4%, m5%
    Close
End Sub

Sub highscoreliste
    Cls
    Screen 1
    Screen 12
    Color 14
    Locate 3, 29
    Print "…ÕÕÕÕÕÕÕÕÕÕÕÕÕª"
    Locate 4, 29
    Print "∫  Highscores ∫"
    Locate 5, 29
    Print "»ÕÕÕÕÕÕÕÕÕÕÕÕÕº"
    ahl:
    View Print 10 To 30
    Cls 2
    Open "c:\TB\score.txt" For Input Access Read As #2
    Input #2, platz1!, platz2!, platz3!, platz4!, platz5!
    Input #2, np1$, np2$, np3$, np4$, np5$
    Input #2, m1%, m2%, m3%, m4%, m5%
    View Print 10 To 30
    Locate 11
    Color 14
    Print "Rangliste fÅr 'Gegen die Zeit' Modus:"
    Color 2
    Print
    Print
    Print "     1. Platz     Level"; m1%, platz1!; "sec.", np1$
    Print "     2. Platz     Level"; m2%, platz2!; "sec.", np2$
    Print "     3. Platz     Level"; m3%, platz3!; "sec.", np3$
    Print "     4. Platz     Level"; m4%, platz4!; "sec.", np4$
    Print "     5. Platz     Level"; m5%, platz5!; "sec.", np5$
    Close #2
    Locate 29, 35
    Color 8
    Print "Anderer Modus mit Links order Rechts wÑhlen"
    Locate 30, 35
    Print "(mit ESC zurÅck)"
    Do
        taste$ = InKey$
        If taste$ = Chr$(0) + "K" Then GoSub links
        If taste$ = Chr$(0) + "M" Then GoSub links
        If taste$ = Chr$(27) Then GoTo endehl
    Loop
    Cls

    links:
    Close
    View Print 10 To 30
    Cls 2
    Open "c:\TB\score2.txt" For Input Access Read As #9
    Input #9, apoca1!, apoca2!, apoca3!, apoca4!, apoca5!
    Input #9, ap1$, ap2$, ap3$, ap4$, ap5$
    Input #9, n1%, n2%, n3%, n4%, n5%
    View Print 10 To 30
    Color 14
    Locate 11
    Print "Rangliste fÅr 'Apokalypse' Modus:"
    Print
    Color 2
    Print
    Print "     1. Platz     Level"; n1%, apoca1!; "sec.", ap1$
    Print "     2. Platz     Level"; n2%, apoca2!; "sec.", ap2$
    Print "     3. Platz     Level"; n3%, apoca3!; "sec.", ap3$
    Print "     4. Platz     Level"; n4%, apoca4!; "sec.", ap4$
    Print "     5. Platz     Level"; n5%, apoca5!; "sec.", ap5$
    Close #9
    Locate 29, 35
    Color 8
    Print "Anderer Modus mit Links order Rechts wÑhlen"
    Locate 30, 35
    Print "(mit ESC zurÅck)"
    Do
        taste$ = InKey$
        If taste$ = Chr$(0) + "K" Then GoTo ahl
        If taste$ = Chr$(0) + "M" Then GoTo ahl
        If taste$ = Chr$(27) Then GoTo endehl
    Loop
    Return
    endehl:
    Cls
End Sub

Sub HILFE
    Rem                             Anleitung fÅr Tunnelbreaker (TB)

    Screen 12
    Cls
    anfanghilfe:
    Color 9
    Print "                                 Anleitung zu TB"
    Print
    Color 14
    Print "Streuern:    Pfeiltasten"
    Print
    Print "Beenden:     ESC"
    Print
    Print "Neustart:    N"
    Print
    Print "Pause:       P (Block wird grau)"
    Print
    Print "Schie·en:    LEER (Nur wenn Schuss verfÅgbar -> siehe kurz Anleitung)"
    Print
    Print "Tempo:       + (erhîhen)  - (verringern) -> funktioniert NICHT am Nummernblock"
    Print
    Print "Modus:       1 (Schweif)  2 (normal)"
    Print
    Print "Turbo:       Durch gedrÅckt halten von ENTER erhîht sich die Geschwindigkeit"
    Print "             auf eine Art punktueller Warp -> kleinere Schub-stî·e"
    Print
    Print "Bombe:       Bombe fliegt gerade aus, bis sie auf ein Hindernis stî·t und"
    Print "             explodiert. (Benîtigt 2 Schuss)"
    Print
    Print "Teleport:    Wenn deine Energie voll geladen ist (pink), dann kannst du dich"
    Print "             mit X Åber eine Entfernung von 40 Pixel beamen!  "
    Print
    Print
    Color 15
    Print "                                     Weiter mit ENTER"
    Do: taste$ = InKey$: Loop Until taste$ = Chr$(13)
    Color 2
    Print
    Print
    Print
    Print "Schuss/Bombe:        Falls durch richtigen Code oder sammeln von Boxen die"
    Print "                     Munition aufgeladen wird (max. 1) kînnen Hindernisse"
    Print "                     in einer Reichweite von 30 Pixel (Block hat je 10"
    Print "                     Pixel) beseitigt werden -> Block hÑlt an und das Hindernis"
    Print "                     wird durchbohrt/gesprengt."
    Print
    Print "Tempo:               Der Block kann durch halten der entsprechenden Taste bis zu"
    Print "                     einem gewissen Grad beschleunigt oder gebremst werden."
    Print
    Print "Schweifmodus:        Block bewegt sich und hinterlÑsst an befahrenen Stellen"
    Print "                     eine blau-schwarze Linie."
    Print
    Print "Normalmodus:         Block bewegt sich ohne hinterlassen von Markierungen."
    Print
    Print "Teleport:            Deine Energie lÑdt sich immer dann auf, wenn du dich Åber "
    Print "                     lÑngere Strecken gerade aus bewegst. Bei 100 Punkten ist    "
    Print "                     die Energie vollstÑndig geladen -> Beamen ist mîglich"
    Color 3
    Print
    Print "Feldmakierungen:     Es gibt Feldmakierungen fÅr jeden Zweck:  "
    Color 7
    Print "                           TÅrkies:    Teleporter (Ziel unbekant)"
    Print "                           GrÅn:       Munitionsbonus"
    Print "                           Pink:       Bonuslinien (bringen Punkte)"
    Print
    Print
    Color 15
    Print "Mit ENTER zum Anfang zurÅck"
    Color 4
    Print "Mit ESC zum Spiel zurÅck"
    Print
    Do
        taste$ = InKey$
        If taste$ = Chr$(13) Then GoTo anfanghilfe
    Loop Until taste$ = Chr$(27)
    Call start
    If levelll% = 1 Then Call level1
    If levelll% = 2 Then Call Level2
    If levelll% = 3 Then Call level3
    Call block1
    Call pause
End Sub

Sub inhaltsperre
    If inhaltsvz% = 1 Then Line (0, 400)-(640, 480), 0, BF
End Sub

Sub Koordinate
    koordinate1:
    Screen 12
    Cls
    Color 15
    Input "x-Achse (Pixel): ", achse1!
    Print
    Input "y-Achse (Pixel): ", achse2!
    Print
    Print "Farbe:"
    Print "Blau, GrÅn, Rot, Wei·, Braun, Gelb"
    Print
    Input "", farbe$
    If farbe$ = "blau" Then farbe% = 1
    If farbe$ = "grÅn" Then farbe% = 2
    If farbe$ = "rot" Then farbe% = 4
    If farbe$ = "wei·" Then farbe% = 15
    If farbe$ = "braun" Then farbe% = 6
    If farbe$ = "gelb" Then farbe% = 14

    Cls
    x = 0
    y1 = 0
    y2 = 480

    Do
        Line (x, y1)-(x, y2), farbe%
        x = x + achse1!
        Line (x, y1)-(x, y2), farbe%
    Loop Until x > 640

    x1 = 0
    x2 = 640
    y = 0

    Do
        Line (x1, y)-(x2, y), farbe%
        y = y + achse2!
        Line (x1, y)-(x2, y), farbe%
    Loop Until y > 480

    Do
        taste$ = InKey$
    Loop Until taste$ = Chr$(27)

    Color 4
    Print "Noch einmal?"
    Print
    Print "(J / N)"
    Do
        taste$ = InKey$
        If taste$ = "j" Then GoTo koordinate1
    Loop Until taste$ = "n"

End Sub

Sub laden
    Color 2
    Cls
    Input "Dateiname (keine Sonderzeichen!): ", ort2$
    ort1$ = "c:\TB\Maps\"
    ort3$ = ".txt"
    ort$ = ort1$ + ort2$ + ort3$
    Open ort$ For Input As #1
    x5 = 0: y5 = 0
    Cls
    View Print 26 To 30
    Print "laden..."
    Do
        x5 = x5 + 1
        If x5 = 640 Then
            x5 = 0
            y5 = y5 + 1
        End If
        Input #1, farbcode!
        PSet (x5, y5), farbcode!
    Loop Until y5 = 332
    ort2$ = ""
    Close #1
    Cls 2
End Sub

Sub level1
    Line (61, 325)-(70, 250), 6, BF
    Line (10, 220)-(100, 210), 6, BF
    Line (120, 305)-(130, 180), 6, BF
    Line (120, 305)-(320, 295), 6, BF
    Line (130, 180)-(40, 170), 6, BF
    Line (40, 180)-(50, 120), 6, BF
    Line (320, 305)-(330, 280), 6, BF
    Line (320, 280)-(600, 270), 6, BF
    Line (355, 325)-(365, 300), 6, BF
    Line (390, 300)-(400, 280), 6, BF
    Line (425, 325)-(435, 300), 6, BF
    Line (460, 300)-(470, 280), 6, BF
    Line (495, 325)-(505, 300), 6, BF
    Line (530, 300)-(540, 280), 6, BF
    Line (565, 325)-(575, 300), 6, BF
    Line (630, 250)-(550, 240), 6, BF
    Line (630, 180)-(550, 170), 6, BF
    Line (630, 110)-(550, 100), 6, BF
    Line (530, 215)-(600, 205), 6, BF
    Line (530, 145)-(600, 135), 6, BF
    Line (530, 280)-(520, 135), 6, BF
    Line (550, 100)-(500, 110), 6, BF
    Line (500, 100)-(490, 240), 6, BF
    Line (500, 240)-(300, 250), 6, BF
    Line (280, 280)-(270, 220), 6, BF
    Line (280, 280)-(320, 270), 6, BF
    Line (270, 220)-(450, 210), 6, BF
    Line (450, 220)-(460, 80), 6, BF
    Line (450, 80)-(600, 70), 6, BF
    Line (600, 80)-(610, 40), 6, BF
    Line (600, 50)-(550, 40), 6, BF
    Line (560, 40)-(550, 30), 6, BF
    Line (40, 120)-(50, 30), 6, BF
    Line (40, 30)-(400, 40), 6, BF
    Line (390, 40)-(400, 150), 6, BF
    Line (420, 10)-(430, 180), 6, BF
    Line (400, 150)-(340, 160), 6, BF
    Line (430, 180)-(320, 190), 6, BF
    Line (310, 190)-(320, 100), 6, BF
    Line (320, 100)-(170, 90), 6, BF
    Line (170, 90)-(180, 250), 6, BF
    Line (260, 280)-(270, 130), 6, BF
    Line (210, 80)-(220, 70), 6, BF
    Line (140, 100)-(150, 110), 6, BF
    Line (160, 50)-(170, 60), 6, BF
    Line (280, 70)-(270, 60), 6, BF
    Line (90, 70)-(100, 80), 6, BF
    Line (110, 120)-(120, 130), 6, BF
    Line (160, 150)-(150, 160), 6, BF
    Line (150, 210)-(140, 200), 6, BF
    Line (250, 150)-(260, 160), 6, BF
    Line (230, 180)-(220, 190), 6, BF
    Line (210, 110)-(220, 120), 6, BF
    Line (190, 160)-(200, 150), 6, BF
    Line (200, 230)-(210, 240), 6, BF
    Line (230, 270)-(240, 280), 6, BF
End Sub

Sub Level2
    Rem                     Pfad 1
    Line (0, 331)-(640, 331), 2 'level marke
    Line (60, 325)-(70, 250), 6, BF
    Line (10, 220)-(100, 210), 6, BF
    Line (90, 250)-(100, 300), 6, BF
    Line (100, 300)-(200, 290), 6, BF
    Line (200, 300)-(210, 260), 6, BF '1. knick
    Line (200, 260)-(260, 250), 6, BF '...
    Line (270, 250)-(260, 300), 6, BF '...
    Line (230, 325)-(240, 280), 6, BF '...
    Line (270, 300)-(370, 290), 6, BF
    Line (370, 300)-(380, 260), 6, BF '2. knick
    Line (370, 260)-(430, 250), 6, BF '...
    Line (430, 250)-(440, 300), 6, BF '...
    Line (400, 325)-(410, 280), 6, BF '...
    Line (440, 300)-(610, 290), 6, BF
    Line (600, 290)-(590, 200), 6, BF 'rechts unten (eck)
    Line (400, 200)-(600, 190), 6, BF
    Line (400, 190)-(410, 150), 6, BF
    Line (410, 150)-(40, 140), 6, BF
    Line (430, 150)-(630, 160), 6, BF
    Line (430, 150)-(440, 110), 6, BF
    Line (440, 110)-(40, 100), 6, BF
    Line (570, 190)-(560, 180), 6, BF 'pfadblockade
    Line (540, 160)-(530, 170), 6, BF
    Line (500, 190)-(510, 180), 6, BF
    Line (470, 160)-(480, 170), 6, BF
    Line (430, 160)-(440, 170), 6, BF
    Line (80, 125)-(410, 125), 6 'pfadblockade ende

    Rem             Pfad 2
    Line (60, 210)-(120, 220), 6, BF
    Line (120, 220)-(130, 150), 6, BF 'SpÑter Teleport zu 230,110
    Line (90, 250)-(150, 240), 6, BF
    Line (150, 250)-(160, 170), 6, BF
    Line (150, 170)-(200, 180), 6, BF '
    Line (230, 150)-(220, 200), 6, BF '
    Line (230, 200)-(180, 210), 6, BF 'Schlaufen
    Line (160, 225)-(250, 235), 6, BF '
    Line (250, 235)-(260, 180), 6, BF 'gewirbel
    Line (250, 180)-(370, 170), 6, BF '
    Line (400, 200)-(280, 210), 6, BF '
    Line (260, 225)-(420, 235), 6, BF '
    Line (420, 235)-(430, 220), 6, BF '
    Line (420, 220)-(570, 230), 6, BF '
    Line (570, 220)-(560, 260), 6, BF 'rechts unten
    Line (570, 260)-(500, 270), 6, BF
    Line (500, 250)-(510, 260), 6, BF
    Line (540, 240)-(550, 250), 11, BF 'Teleporter

    Line (10, 70)-(100, 80), 6, BF '2 in 1 weg
    Line (120, 40)-(130, 80), 6, BF 'falscher weg
    Line (130, 40)-(30, 30), 6, BF '...
    Line (130, 40)-(540, 30), 6, BF '...
    Line (120, 80)-(460, 70), 6, BF 'richtiger weg
    Line (460, 70)-(470, 130), 6, BF
    Line (470, 130)-(600, 120), 6, BF
    Line (600, 130)-(610, 60), 6, BF
    Line (610, 60)-(540, 70), 6, BF
    Line (540, 100)-(530, 30), 6, BF
End Sub

Sub level3
    Line (0, 331)-(640, 331), 3 'level marke
    Line (10, 270)-(80, 280), 6, BF
    Line (80, 270)-(90, 300), 6, BF
    Line (110, 330)-(120, 250), 6, BF
    Line (120, 250)-(30, 240), 6, BF
    Line (10, 220)-(140, 210), 6, BF
    Line (140, 210)-(150, 300), 6, BF
    Line (170, 330)-(180, 210), 6, BF
    Line (10, 190)-(340, 180), 6, BF 'Weggablung
    Line (180, 210)-(340, 220), 6, BF
    Line (340, 220)-(350, 180), 6, BF
    Line (15, 195)-(25, 205), 11, BF 'Teleport links
    Line (335, 195)-(325, 205), 11, BF 'Teleport rechts
    Line (10, 40)-(300, 50), 6, BF 'links oben             Pfad 1
    Line (300, 40)-(310, 60), 6, BF
    Line (310, 60)-(180, 70), 6, BF
    Line (330, 10)-(340, 90), 6, BF
    Line (340, 90)-(200, 100), 6, BF
    Line (180, 60)-(170, 120), 6, BF
    Line (170, 120)-(360, 130), 6, BF 'oben mitte
    Line (360, 130)-(370, 30), 6, BF
    Line (390, 10)-(400, 150), 6, BF
    Line (400, 150)-(40, 160), 6, BF
    Line (40, 70)-(30, 160), 6, BF
    Line (60, 90)-(70, 80), 6, BF 'Blockfeld beginnt (40,70)-(160,160)
    Line (80, 110)-(90, 120), 6, BF
    Line (120, 70)-(130, 60), 6, BF
    Line (140, 100)-(150, 110), 6, BF
    Line (100, 90)-(110, 100), 6, BF
    Line (50, 140)-(60, 130), 6, BF
    Line (130, 130)-(140, 140), 6, BF
    Line (95, 70)-(85, 80), 6, BF 'Blockfeld ende
    Line (430, 150)-(630, 170), 6, BF 'Pfad 1 -> riesen Blockade
    Line (550, 30)-(560, 40), 6, BF 'Zielsperre
    Line (550, 40)-(630, 50), 6, BF '    "
    Line (420, 70)-(430, 80), 6, BF 'Riesenblockade (400,0)-(630,150)
    Line (500, 20)-(510, 30), 6, BF
    Line (600, 110)-(610, 120), 6, BF
    Line (450, 90)-(460, 100), 6, BF
    Line (410, 40)-(420, 30), 6, BF
    Line (510, 90)-(520, 100), 6, BF
    Line (470, 110)-(480, 120), 6, BF
    Line (490, 50)-(500, 60), 6, BF
    Line (560, 70)-(570, 80), 6, BF
    Line (580, 100)-(590, 110), 6, BF
    Line (530, 60)-(540, 70), 6, BF
    Line (540, 100)-(550, 110), 6, BF
    Line (560, 90)-(570, 100), 6, BF
    Line (440, 60)-(450, 70), 6, BF
    Line (430, 50)-(440, 40), 6, BF
    Line (510, 80)-(500, 70), 6, BF
    Line (495, 45)-(485, 35), 6, BF
    Line (470, 80)-(480, 90), 6, BF
    Line (460, 30)-(470, 40), 6, BF
    Line (500, 150)-(490, 140), 6, BF
    Line (530, 120)-(540, 130), 6, BF
    Line (560, 130)-(570, 140), 6, BF
    Line (590, 80)-(600, 70), 6, BF 'Riesenblockade ende
    Line (630, 300)-(420, 290), 6, BF 'rechts unten            Pfad 2
    Line (430, 290)-(350, 280), 6, BF
    Line (400, 325)-(380, 310), 6, BF
    Line (360, 300)-(200, 290), 6, BF
    Line (180, 270)-(300, 260), 6, BF
    Line (300, 270)-(310, 240), 6, BF 'Mitte unter Teleport2
    Line (300, 230)-(630, 240), 6, BF
    Line (350, 262)-(630, 257), 6, BF
    Line (460, 260)-(630, 270), 6, BF
    If sperre$ <> "ja" Then Line (620, 275)-(610, 285), 10, BF 'Munistionslager
    Line (490, 275)-(480, 285), 8, BF 'Weg schalter
    Line (610, 230)-(300, 220), 6, BF
    Line (610, 220)-(350, 180), 6
    Line (630, 200)-(430, 170), 6
End Sub

Sub lg
    If sperre$ = "ja" Then
        bz! = Timer - zzz!
        Call highscore
    End If
    If modus = 2 Then
        bz! = Timer - zzz!
        Call apohighs
    End If
    Cls
    punkte% = punkte% + 3
    lg:
    View Print
    Cls
    Color 5
    Print
    Print
    Print "                             Herzlichen GlÅckwunsch!!!"
    Print
    Print
    Print "                                Level"; num%; "beendet"
    Print
    Print
    Print "                              Benîtigte zeit: "; bz!
    Print
    Print
    If sperre$ <> "ja" Then
        Color 7
        Print "                               Aktuelle Bonuspunkte:"; punkte%
        Print "                                 Aktuelle Munition:"; s%
        Print "                      (alle 8 Bonuspunkte bekommst du 1 Schuss)"
        Print
    End If
    Color 8
    Print
    Print
    Print
    Print "                                 Highsocre (F1)"
    Print "                                 weiter mit ENTER"
    Do: taste$ = InKey$:
        If taste$ = Chr$(0) + ";" Then
            Call highscoreliste
            GoTo lg
        End If
    Loop Until taste$ = Chr$(13)
    If num% = 999 Then Exit Sub
    If num% = 3 Then
        Cls
        Screen 12
        View Print
        Color 12
        Locate 5
        Print "Tut mir Leid. Aber es gibt keine weiteren vorgefertigten Level."
        Print
        Print "Erstelle dir eigene Level im HauptmenÅ, Kategorie 3!"
        Locate 28, 55
        Color 2
        Print "ceated by Timo B."
        Sleep
        End
    End If
    E% = 6
    Do
        E% = E% - 1
        Cls
        Color 14
        Print
        Print
        Print "                              Level"; num% + 1; "startet in"
        Color 4
        Print
        Print
        Print "                                     "; E%
        Print
        Print
        Color 2
        If E% < 2 Then Print "                                   Viel Spa·"
        d = Timer + 1: Do While Timer < d: Loop
    Loop Until E% = 0
    num% = num% + 1
    zeit! = Timer
    punkte2% = 0
    xbeam = 0
End Sub

Sub lvz
    Line (0, 331)-(640, 331), 2 + 4 'levelmarke
    Line (500, 15)-(629, 50), 0, BF
    Line (100, 200)-(110, 210), 1, BF
    Line (150, 200)-(160, 210), 2, BF
    Line (200, 200)-(210, 210), 3, BF
    num% = 9
End Sub

Sub Mapeditor
    Rem                           Mapeditor   v1.0     by Timo B.
    Cls
    Print "Ist der Speicherpfad angelegt (J/N)? - Wenn nein, wird er automatisch "
    Print "angelegt."
    anfangeditor:
    g% = 1
    pixel% = 6
    pixel2% = 0
    pixel3% = 4
    prozent! = 100 / 331
    Do
        taste$ = InKey$
        If taste$ = "n" Then GoSub pfad
    Loop Until taste$ = "j"
    Screen 12
    Cls
    Color 2
    View Print 23 To 30
    Print "Gespeicherte Datei bearbeiten (J/N)?"
    Do
        taste$ = InKey$
        If taste$ = "n" Then GoTo anfangeditor2
    Loop Until taste$ = "j"
    GoSub bearbeiten
    x1 = 11: x2 = x1 + 10: y1 = 240: y2 = y1 + 10
    GoTo anfangeditor3

    anfangeditor2:
    Cls
    x1 = 11: x2 = x1 + 10: y1 = 240: y2 = y1 + 10
    g% = 1
    pixel% = 6
    pixel2% = 0
    pixel3% = 4
    Line (10, 260)-(120, 270), pixel%, BF
    Line (120, 330)-(130, 260), pixel%, BF
    Line (550, 10)-(560, 40), pixel%, BF
    Line (550, 50)-(630, 40), pixel%, BF
    Line (0, 0)-(640, 10), pixel3%, BF
    Line (0, 0)-(10, 325), pixel3%, BF
    Line (0, 330)-(630, 325), pixel3%, BF
    Line (640, 330)-(630, 10), pixel3%, BF
    Line (0, 331)-(640, 331), 15

    anfangeditor3:
    View Print 23 To 30
    Print
    Print "LEER:     Radierer / Neuen Punkt anfahren    POS1:       Cursor hervorheben"
    Print "ENTER:    Neu ansetzen (per Koordinaten)     + oder - :  Cursor-Geschwindigkeit"
    Print "N:        Neustart"
    Locate 30, 60
    Print "by Timo B."

    subanfang:
    round% = 0
    a% = 0
    Do
        Locate 27
        Print "Position des Cursors: "; x1; ", "; y1
        Locate 28
        Print "Geschw.: "; g%; " Pixel pro Sprung (-> Taste)"
        x2 = x1 + 10: y2 = y1 + 10
        taste$ = InKey$
        If taste$ = Chr$(0) + "G" Then
            Line (x1, y1)-(x2, y2), 3, B
            Sleep
        End If
        Line (x1, y1)-(x2, y2), pixel%, BF
        If taste$ = "+" Then g% = g% + 1
        If taste$ = "-" Then g% = g% - 1
        If g% < 1 Then g% = 1
        If g% > 10 Then g% = 10
        If taste$ = Chr$(0) + "M" Then 'rechts
            x1 = x1 + g%
            x3 = x1 + g%
            y3 = y1 + 5
        End If
        If taste$ = Chr$(0) + "K" Then 'links
            x1 = x1 - g%
            x3 = x1 - g%
            y3 = y1 + 5
        End If
        If taste$ = Chr$(0) + "H" Then 'hoch
            y1 = y1 - g%
            x3 = x1 + 5
            y3 = y1 - g%
        End If
        If taste$ = Chr$(0) + "P" Then 'runter
            y1 = y1 + g%
            x3 = x1 + 5
            y3 = y1 + g%
        End If
        If taste$ = "n" Then GoTo anfangeditor2
        If taste$ = Chr$(32) Then GoSub radierer
        If taste$ = Chr$(13) Then GoSub neu
        If taste$ = Chr$(27) Then GoSub endME
    Loop

    neu:
    View Print 23 To 30
    Cls 2
    Line (x1, y1)-(x2, y2), pixel2%, BF
    Print "Neue Koordinaten"
    Input "x-Achse: ", x1
    Input "y-Achse: ", y1
    Cls 2
    GoTo anfangeditor3
    Return

    radierer:
    Do
        Locate 27
        Print "Position des Cursors: "; x1; ", "; y1
        Locate 28
        Print "Geschw.: "; g%; " Pixel pro Sprung (-> Taste)"
        x2 = x1 + 10: y2 = y1 + 10
        Line (x1, y1)-(x2, y2), pixel%, BF
        Line (x1, y1)-(x2, y2), 3, B
        taste$ = InKey$
        If taste$ = "+" Then g% = g% + 1
        If taste$ = "-" Then g% = g% - 1
        If g% < 1 Then g% = 1
        If g% > 10 Then g% = 10
        If taste$ = Chr$(0) + "M" Then 'rechts
            Line (x1, y1)-(x2, y2), pixel2%, BF
            x1 = x1 + g%
            x3 = x1 + g%
            y3 = y1 + 5
        End If
        If taste$ = Chr$(0) + "K" Then 'links
            Line (x1, y1)-(x2, y2), pixel2%, BF
            x1 = x1 - g%
            x3 = x1 - g%
            y3 = y1 + 5
        End If
        If taste$ = Chr$(0) + "H" Then 'hoch
            Line (x1, y1)-(x2, y2), pixel2%, BF
            y1 = y1 - g%
            x3 = x1 + 5
            y3 = y1 - g%
        End If
        If taste$ = Chr$(0) + "P" Then 'runter
            Line (x1, y1)-(x2, y2), pixel2%, BF
            y1 = y1 + g%
            x3 = x1 + 5
            y3 = y1 + g%
        End If
        If taste$ = Chr$(13) Then GoSub neu
        If taste$ = Chr$(27) Then GoSub endME
        If taste$ = "n" Then GoTo anfangeditor2
    Loop Until taste$ = Chr$(32)
    Return

    endME:
    Line (x1, y1)-(x2, y2), 0, BF
    View Print 23 To 30
    Cls 2
    Print
    Print "Speichern (J / N)?"
    Print "ESC um abzubrechen."
    Do
        taste$ = InKey$
        If taste$ = "j" Then
            Cls 2
            GoTo save
        End If
        If taste$ = "n" Then End
    Loop Until taste$ = Chr$(27)
    GoTo anfangeditor3
    Return

    save:
    Line (x1, y2)-(x2, y2), 0, BF
    Input "Dateiname (keine Sonderzeichen): ", ort2$
    ort1$ = "c:\TB\Maps\"
    ort3$ = ".txt"
    ort$ = ort1$ + ort2$ + ort3$
    Open ort$ For Output As #1
    x5 = 0: y5 = 0
    Locate 26
    Print "speichern..."
    Do
        x5 = x5 + 1
        If x5 = 640 Then
            x5 = 0
            y5 = y5 + 1
        End If
        farbcode! = Point(x5, y5)
        Write #1, farbcode!
    Loop Until y5 = 332
    Close #1
    ort2$ = ""
    Locate 27
    Print "Fortfahren (J/N)? Mit ESC beenden."
    Do
        taste$ = InKey$
        If taste$ = "j" Then
            Cls 2
            GoTo anfangeditor3
        End If
        If taste$ = "n" Then GoTo anfangeditor
    Loop Until taste$ = Chr$(27)
    GoTo 0

    bearbeiten:
    View Print 23 To 30
    Cls 2
    Input "Dateiname (keine Sonderzeichen!): ", ort2$
    ort1$ = "c:\TB\Maps\"
    ort3$ = ".txt"
    ort$ = ort1$ + ort2$ + ort3$
    Open ort$ For Input As #1
    x5 = 0: y5 = 0
    Cls
    Locate 27
    Print "laden..."
    Do
        x5 = x5 + 1
        If x5 = 640 Then
            x5 = 0
            y5 = y5 + 1
        End If
        Input #1, farbcode!
        PSet (x5, y5), farbcode!
    Loop Until y5 = 332
    ort2$ = ""
    Close #1
    Cls 2
    View Print 23 To 30
    Input "GrafikschÑden (falls vorhanden) beheben?   ", antwort$
    If antwort$ = "ja" Then GoSub antigrafikbug
    Cls 2
    Return

    pfad:
    MkDir "c:\TB"
    MkDir "c:\TB\Maps"
    Return


    antigrafikbug:
    x5 = 11: y5 = 11
    Do
        x5 = x5 + 1
        x6 = x5 - 1
        farbee! = Point(x5, y5)
        If farbee! <> 0 And farbee! <> 6 Then
            farbe% = Point(x6, y5)
            farbe% = 6 'evtl. umsetllung
            Line (x5, y5)-(x5, y5), farbe%
        End If
        If x5 = 629 Then
            x5 = 11
            y5 = y5 + 1
        End If
    Loop Until y5 = 325
    Return

0 End Sub

Sub muni
    If inhaltsvz% = 1 Then GoTo 999
    If sperre$ <> "ja" Then
        If Point(x4, y4) = 10 Then s% = 2
        If s% = 2 Then
            F% = 1
            Line (95, 403)-(97, 413), 8, BF
            Line (100, 403)-(102, 413), 8, BF
        End If
        If s% = 1 Then
            Line (95, 403)-(97, 413), 8, BF
            Line (100, 403)-(102, 413), 0, BF
        End If
        If s% = 0 Then
            Line (95, 403)-(97, 413), 0, BF
            Line (100, 403)-(102, 413), 0, BF
        End If
        If s% < 2 Then F% = 9
    End If
999 End Sub

Sub musik
    1 Screen 12
    Cls
    ESC = 27
    Print
    Print
    Print
    ccc% = Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd + Rnd
    Color ccc%
    If ccc% = 8 Or ccc% = 7 Then GoTo 1
    Print
    If InKey$ = Chr$(ESC) Then GoTo endsub
    Print "                         Wilkommen zu Tunnelbreaker "
    Play "g g g > c c c <  a f c4"
    If InKey$ = Chr$(ESC) Then GoTo endsub
    Play "g g g > c c c d < b g"
    If InKey$ = Chr$(ESC) Then GoTo endsub
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print "                                                                 Version 1.0"
    Print "                                                                 by Timo B."
    If InKey$ = Chr$(ESC) Then GoTo endsub
    Play "b- b- b- > e- e- e- d < b- f"
    If InKey$ = Chr$(ESC) Then GoTo endsub
    Play "e- e- e- a- a- a- g > e4 < c"
    endsub:
End Sub

Sub pause
    Timer Stop
    x1 = x1: x2 = x2: y2 = y2: y1 = y1
    Line (x1, y1)-(x2, y2), 8, BF
    Do
        taste3$ = InKey$
    Loop Until taste3$ = Chr$(27) Or taste3$ = "p"
    Timer On
End Sub

Sub pfadanlegen
    Cls
    Screen 12
    taste$ = InKey$
    Locate 10
    Color 15
    Print "Sind die benîtigten Dateien richtig angelegt?"
    Print "Wenn nein, drÅcke    um sie ALLE anlegen zu lassen (alte Daten werden gelîscht)!"
    Locate 11, 19
    Color 2
    Print "F1"
    Color 15
    Print "Sind die Dateien angelegt, drÅcke     um fortzufahren."
    Locate 12, 35
    Color 2
    Print "ESC"
    Color 4
    q% = 21
    Do
        q% = q% - 1
        taste$ = InKey$
        If taste$ = Chr$(0) + ";" Then GoTo anlegen
        Locate 25, 35
        Print q%
        d = Timer + 1: Do While Timer < d: Loop
    Loop Until q% = 0 Or taste$ = Chr$(27)
    Exit Sub

    anlegen:
    MkDir "c:\TB"
    MkDir "c:\TB\Maps"
    Open "c:\TB\score.txt" For Output As #1
    Write #1, platz1!, platz2!, platz3!, platz4!, platz5!
    Write #1, np1$, np2$, np3$, np4$, np5$
    Close #1
    Open "c:\TB\score2.txt" For Output As #2
    Write #2, apoca1!
    Write #2, apoca2!
    Write #2, apoca3!
    Write #2, apoca4!
    Write #2, apoca5!
    Write #2, ap1$
    Write #2, ap2$
    Write #2, ap3$
    Write #2, ap4$
    Write #2, ap5$
    Close #2
End Sub

Sub rausgeschnitten
    Line (500, 10)-(500, 30), 2 'Marke fÅr Falle
    Line (610, 65)-(630, 65), 3 'Marke fÅr Teleport

    If x1 = 520 And y1 > 250 And y1 < 270 Then punkte% = punkte% + 1 'Level2
    If x1 = 200 And y1 < 225 And y1 > 210 Then punkte% = punkte% + 1
    If x1 = 260 And y1 > 40 And y1 < 70 Then punkte% = punkte% + 1
    If x1 = 180 And y1 < 160 And y1 > 130 Then punkte% = punkte% + 1
    If x1 = 240 And y1 < 280 And y1 > 260 Then punkte% = punkte% + 1

    If x1 = 270 And y1 < 90 And y1 > 40 Then punkte% = punkte% + 1 'Level1
    If x1 > 130 And x1 < 170 And y1 = 170 Then punkte% = punkte% + 1
    If x1 = 270 And y1 < 130 And y1 > 100 Then punkte% = punkte% + 1
    If x1 = 500 And y1 > 280 And y1 < 300 Then punkte% = punkte% + 1
    If x1 = 580 And y1 > 170 And y1 < 200 Then punkte% = punkte% + 1

    Rem             SUB bombe
    bom% = 5
    For i = 1 To 25
        bom% = bom% + 1
        Circle (x1 + 5, y1 - 5), bom%, 12
        d = Timer + .005: Do While Timer < d: Loop
        Circle (x1 + 5, y1 - 5), bom% - 4, 0
    Next i
    bom% = 26
    For i = 1 To 4
        bom% = bom% + 1
        Circle (x1 + 5, y1 - 5), bom%, 0
        d = Timer + .005: Do While Timer < d: Loop
    Next i
    Paint (x1 + 5, y1 - 5), bom%, 0
    s% = 0
    Line (95, 403)-(97, 413), 0, BF
    Line (100, 403)-(102, 413), 0, BF
    Rem             end sub

End Sub

Sub rechte
    Color 15
    View Print 10 To 20
    Print "Dieses Spiel ist Urheberrechtlich geschÅtzt."
    Print "Alle Rechte obligen den unten genannten Personen (inklusive Copyright)."
    Print "Bei VervielfÑltigung des Spiels oder bei Verletzung der Urheberrechte,"
    Print "ist mit rechtlichen Folgen seitens des Herstellers zu rechnen."
    Print
    Print
    Print
    Color 4
    Print "Hersteller:                                        Timo B."
    Print "Mit freundlicher UnterstÅtzung von:                Johannes R."
    Sleep 20
End Sub

Sub schuss
    If sperre$ = "ja" Or inhaltsvz% = 1 Then GoTo subend
    If s% > 2 Then s% = 2
    For i = 1 To 30
        Line (x4 + v1, y4 + h1)-(x4 + v2, y4 + h2), 0, BF
        x4 = x4 + v
        y4 = y4 + h
        Line (x4 + v1, y4 + h1)-(x4 + v2, y4 + h2), 12, BF
        d = Timer + .005: Do While d > Timer: Loop
        Line (x4 + v1, y4 + h1)-(x4 + v2, y4 + h2), 12, BF
    Next i
    Line (x4 + v1, y4 + h1)-(x4 + v2, y4 + h2), 0, BF
    s% = s% - 1
    subend:
End Sub

Sub slvz
    Cls
    Screen 12
    Color 15
    Print
    Print
    Print "    LEVEL:"
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print "             I    II    III    "
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Print "Starte mit '2' und bewege dich auf einen der Teleporter hin"
    inhaltsvz% = 1
End Sub

Sub start
    If num% = 999 Then GoTo starta
    Screen 12
    Cls
    starta:
    Color 15
    View Print
    Locate 2, 72
    Print "Ziel"
    View Print 22 To 30
    Color 15
    Print "                                  LEVEL "; num%
    Color 14
    Print "Beenden:    ESC              Hilfe:        F1 "
    Print "Neustart:   N                Turbo:        ENTER (gedrÅckt halten)"
    Print "Pause:      P (graue Farbe)  Bombe:        B (benîtigt 2 Schuss)"
    Color 8
    If sperre$ <> "ja" Then
        Print "Schuss:                      Energie:"
    End If
    Print
    Color 2
    Print "Starte nun mit '1' oder '2'!!!                     Tunnelbreaker by Timo B."
    View Print
End Sub

Sub Stoppuhr
    Cls
    Print "1. Durchlauf: ",
    Input "Zeit in Sekunden: ", zeit!
    x1 = 0
    x2 = 0
    y1 = 320
    y2 = 310
    Timer Stop
    timer1! = Timer
    Timer On
    Do
        taste$ = InKey$
        If taste$ = Chr$(27) Then End
        x1 = x1 + 1
        x2 = x2 + 1
        Line (x1, y1)-(x2, y2), 9, BF
        d = Timer + zeit!: Do While d > Timer: Loop
    Loop Until x2 = 640
    Timer Stop
    timer2! = Timer
    Timer On
    Time! = timer2! - timer1!
    PpS! = 640 / Time!
    Print
    Print "Zeit: ", Time!
    Print
    Print "Pixel pro Sekunde: ", PpS!
End Sub

Sub xxxxxxxx
    Open "c:\TB\score2.txt" For Output As #4
    apoca1! = 40
    apoca2! = 40
    apoca3! = 40
    apoca4! = 40
    apoca5! = 40
    Write #4, apoca2!
    Write #4, apoca3!
    Write #4, apoca4!
    Write #4, apoca5!
    Write #4, apoca1!
    Close #4
    Do
        taste$ = InKey$
    Loop Until taste$ = Chr$(13)
    Open "c:\TB\score2.txt" For Input As #4
    Print apoca1!, apoca2!, apoca3!, apoca4!, apoca5!
    Close #4
End Sub


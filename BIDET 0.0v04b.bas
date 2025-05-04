' BIDET
' BIDET = Bhsdfa's IDE Turbo
' This is still work in progress, don't use it with your code without a backup!
' Everything here is subject to change.
' Version: 0.0v04b

' https://qb64phoenix.com/qb64wiki/index.php?title=_ACOS&action=edit

DO
   _LIMIT 5
LOOP UNTIL _SCREENEXISTS

SCREEN _NEWIMAGE(1280, 680, 32)
DIM SHARED CodeLayer AS LONG
CodeLayer = _NEWIMAGE(1280, 680, 32)

CONST PI = 3.14159265359: CONST PIDIV180 = PI / 180
'Create
DIM SHARED DarkAlphaSprite AS LONG
DarkAlphaSprite = _NEWIMAGE(1, 1, 32): _DEST DarkAlphaSprite
_CLEARCOLOR _RGB32(0, 0, 0), DarkAlphaSprite: LINE (0, 0)-(1, 1), _RGBA32(0, 0, 0, 45), BF
_DEST 0
delay = 2
'OPTION _EXPLICIT
'$DYNAMIC
$COLOR:32
TYPE Lin
   VText AS STRING
   IText AS STRING
   Indent AS _UNSIGNED INTEGER
   ILength AS _UNSIGNED LONG
   VLength AS _UNSIGNED LONG
END TYPE
TYPE Mouse
   x AS _UNSIGNED LONG
   y AS _UNSIGNED LONG
   click1 AS _BYTE
   click2 AS _BYTE
   click3 AS _BYTE
   scroll AS _BYTE
   LinX AS _UNSIGNED LONG
   LinY AS _UNSIGNED LONG
   hassel AS _BYTE
END TYPE
TYPE Keywords
   word AS STRING
   col AS _UNSIGNED LONG
END TYPE
TYPE Secs
   start AS _UNSIGNED LONG
   endr AS _UNSIGNED LONG
   col AS _UNSIGNED INTEGER
END TYPE
TYPE GUI
   x1 AS DOUBLE
   x2 AS DOUBLE
   y1 AS DOUBLE
   y2 AS DOUBLE
   title AS STRING
   TextID AS _UNSIGNED LONG
   RGB AS _UNSIGNED LONG
   Selected AS _BYTE
   Clicked AS _BYTE
   Hover AS _BYTE
   IMGHANDLE AS _UNSIGNED LONG
END TYPE
TYPE Particles
   X AS DOUBLE
   Y AS DOUBLE
   Xm AS DOUBLE
   Ym AS DOUBLE
   Rot AS DOUBLE
   RotM AS DOUBLE
   Image AS LONG
   Size AS DOUBLE
   Exists AS _BYTE
   AnimStyle AS _UNSIGNED _BYTE
   AnimID AS _UNSIGNED _BYTE
END TYPE
TYPE Selection
   LinX1 AS _UNSIGNED LONG
   LinY1 AS _UNSIGNED LONG
   LinX2 AS _UNSIGNED LONG
   LinY2 AS _UNSIGNED LONG
   x1 AS DOUBLE
   x2 AS DOUBLE
   y1 AS DOUBLE
   y2 AS DOUBLE
   Side AS _BYTE
   Exists AS _BYTE
   Text AS STRING
END TYPE
TYPE Config
   CANIM_Erase AS _UNSIGNED _BYTE
   Gravity AS DOUBLE
   FixCursorToScreen AS _BYTE
END TYPE

'Max variables
DIM SHARED MaxKeywords AS _UNSIGNED INTEGER
MaxKeywords = 1024

' Dimmers
DIM SHARED Language AS STRING
DIM SHARED LanguageIDs(64) AS _UNSIGNED LONG
DIM SHARED Sel AS Selection
DIM SHARED Selies(64) AS Selection
DIM SHARED Mouse AS Mouse
DIM SHARED Keywords(MaxKeywords) AS Keywords
DIM SHARED GUI(16) AS GUI
DIM SHARED Cfg AS Config
DIM SHARED HColors(16) AS LONG
LoadHighlightColors

FOR i = 1 TO MaxKeywords
   Keywords(i).word = "": Keywords(i).col = 0
NEXT
'Load keywords
OPEN "BIDET/key.words" FOR INPUT AS #1
i = 0
DO
   i = i + 1
   LINE INPUT #1, Keywords(i).word
   Keywords(i).col = 1
LOOP UNTIL EOF(1)
CLOSE #1



DIM SHARED SubOutput1 AS DOUBLE
DIM SHARED LinCamX AS _UNSIGNED LONG, LinCamY AS _UNSIGNED LONG
DIM SHARED LinCamX2 AS DOUBLE, LinCamY2 AS DOUBLE
DIM SHARED CamXSmo AS DOUBLE, CamYSmo AS DOUBLE
DIM SHARED LinEditX AS _UNSIGNED LONG, LinEditY AS _UNSIGNED LONG
DIM SHARED LastLine AS _UNSIGNED LONG: LastLine = 1
DIM SHARED LastGUI AS _UNSIGNED LONG: LastGUI = 8
DIM SHARED LinesOnScreenY AS _UNSIGNED INTEGER, LinesOnScreenX AS _UNSIGNED INTEGER
DIM SHARED Lin(LastLine) AS Lin
DIM SHARED FontSizeX
DIM SHARED FontSizeY
DIM SHARED KeyP AS STRING


'Related to Dim
DIM SHARED MaxParticles AS _UNSIGNED LONG: MaxParticles = 99999

'MISC stuff:
Cfg.Gravity = 3.5
Cfg.FixCursorToScreen = 0

'Animations:
Cfg.CANIM_Erase = 2

' Decorative.
DIM SHARED Part(MaxParticles) AS Particles

Font = _LOADFONT("BIDET/Fonts/KongText.ttf", 15, "monospace")
_FONT Font, 0
_FONT Font, CodeLayer


FontSizeX = _PRINTWIDTH("ABC") / 3
FontSizeY = _FONTHEIGHT(Font)
DIM SHARED Delay AS _UNSIGNED LONG
LinesOnScreenY = FIX(_HEIGHT / FontSizeY) - 1
LinesOnScreenX = FIX(_WIDTH / FontSizeX) - 1
CONST CSC = "§"
_DELAY 0.5

DO
   CLS , _RGB(0, 10, 45): _LIMIT 5 + (_WINDOWHASFOCUS * -70): IF Delay > 0 THEN Delay = Delay - 1
   'IF _KEYDOWN(117) THEN Cfg.CANIM_Erase = 1
   'IF _KEYDOWN(105) THEN Cfg.CANIM_Erase = 2
   'Mouse related shenanigans.
   Mouse.scroll = 0
   DO WHILE _MOUSEINPUT
      Mouse.x = _MOUSEX: Mouse.y = _MOUSEY
      Mouse.click1 = _MOUSEBUTTON(1): Mouse.click2 = _MOUSEBUTTON(2): Mouse.click3 = _MOUSEBUTTON(3)
      IF _MOUSEWHEEL <> 0 THEN Mouse.scroll = _MOUSEWHEEL
   LOOP
   IF KeyP <> "" THEN _KEYCLEAR
   KeyP = INKEY$
   GetCursorOnText
   LinCamX2 = LinCamX2 + ((LinCamX + 0.01) - LinCamX2) / 30
   LinCamY2 = LinCamY2 + ((LinCamY + 0.01) - LinCamY2) / 30

   IF _KEYDOWN(100306) AND _KEYDOWN(111) AND Delay = 0 THEN Delay = 20: LoadFromFile ' CTRL + O = Load from file.
   IF _KEYDOWN(100306) AND _KEYDOWN(115) AND Delay = 0 THEN Delay = 20: SaveToFile ' CTRL + S = Save to file
   'Mouse scrolling
   IF Mouse.scroll <> 0 THEN HandleMouseScroll: AdjustEditLin



   '  AnimationLogic
   IF KeyP <> "" THEN ExtraKeys: GenerateVText Lin(LinEditY)
   IF KeyP <> "" THEN WriteToLin LinEditY, KeyP: GenerateVText Lin(LinEditY)


   ' Render on screen.
   CLS , , CodeLayer
   RenderLines
   _PUTIMAGE (0, 0), CodeLayer, 0
   ParticleSUB
   ' PrintWithColor _WIDTH - _PRINTWIDTH("LinCamX: " + STR$(LinCamX)), 2 * FontSizeY, ("LinCamX: " + STR$(LinCamX)), 0
   ' PrintWithColor _WIDTH - _PRINTWIDTH("LinCamY: " + STR$(LinCamY)), 3 * FontSizeY, ("LinCamY: " + STR$(LinCamY)), 0
   ' a1$ = LEFT$(STR$(LinCamX2), LEN(STR$(INT(LinCamX2))) + 3)
   ' a2$ = LEFT$(STR$(LinCamY2), LEN(STR$(INT(LinCamY2))) + 3)
   ' PrintWithColor _WIDTH - _PRINTWIDTH("LinCamX2: " + a1$), 4 * FontSizeY, ("LinCamX2: " + a1$), 0
   ' PrintWithColor _WIDTH - _PRINTWIDTH("LinCamY2: " + a2$), 5 * FontSizeY, ("LinCamY2: " + a2$), 0

   _DISPLAY
LOOP

'$INCLUDE:'externalfuncs/BIDET/alaska.bi'

SUB GetCursorOnText
   IF Mouse.click1 THEN
      LinEditX = FIX((Mouse.x + (FontSizeX / 2)) / FontSizeX) + LinCamX
      LinEditY = FIX(Mouse.y / FontSizeY) + LinCamY
      IF Mouse.hassel = -1 THEN
         IF LinEditX <> Mouse.LinX OR LinEditY <> Mouse.LinY THEN
            Sel.Exists = -1


            Sel.LinX1 = Mouse.LinX: Sel.LinY1 = Mouse.LinY
            Sel.LinX2 = LinEditX: Sel.LinY2 = LinEditY
            IF Sel.LinY1 > Sel.LinY2 THEN SWAP Sel.LinX1, Sel.LinX2
            IF Sel.LinY2 < Sel.LinY1 THEN SWAP Sel.LinY1, Sel.LinY2


            IF Sel.LinX2 < Sel.LinX1 THEN IF Sel.LinY1 = Sel.LinY2 THEN SWAP Sel.LinX1, Sel.LinX2

         END IF
      ELSE
         Mouse.LinX = LinEditX: Mouse.LinY = LinEditY: Mouse.hassel = -1
      END IF
      IF LinEditX = Mouse.LinX AND LinEditY = Mouse.LinY THEN Sel.Exists = 0
   ELSE
      Mouse.LinX = 0: Mouse.LinY = 0: Mouse.hassel = 0:
   END IF

   IF Sel.LinY2 > LastLine THEN Sel.LinY2 = LastLine
   RenderSelected
END SUB

SUB DeleteSelection
   IF _KEYDOWN(100304) THEN ShowIntervals = 1
   DIM AllSize AS _UNSIGNED LONG
   IF Sel.LinY1 <> Sel.LinY2 THEN
      FOR i = Sel.LinY1 + 1 TO Sel.LinY2 - 1
         AllSize = AllSize + (Lin(i).ILength)
      NEXT
      AllSize = AllSize + ((Lin(Sel.LinY1).ILength) - Sel.LinX1)
      AllSize = AllSize + (Sel.LinX2)
      AllSize = AllSize + (Sel.LinY2 - Sel.LinY1)
   ELSE
      AllSize = Sel.LinX2 - Sel.LinX1
   END IF
   X = Sel.LinX2 - 1: Y = Sel.LinY2
   DO
      DeleteLetterBackspace Y, X
      X = X - 1: IF X = -1 THEN Y = Y - 1: X = Lin(Y).ILength:
      Sel.LinX2 = X + 1
      Sel.LinY2 = Y
      AllSize = AllSize - 1
      'Debugging code:
      IF ShowIntervals THEN _LIMIT 9 + (_KEYDOWN(8) * -16): GenerateVText Lin(Y): CLS , , 0: CLS , , CodeLayer: RenderLines: _PUTIMAGE (0, 0), CodeLayer, 0: RenderSelected: _DISPLAY
   LOOP WHILE AllSize > 0
   LinEditX = X: LinEditY = Y
   FOR Y = Sel.LinY1 TO Sel.LinY2
      IF Y > LastLine THEN EXIT FOR
      GenerateVText Lin(Y)
   NEXT

   Sel.Exists = 0
   Sel.LinY1 = 0
   Sel.LinY2 = 0
   Sel.LinX1 = 0
   Sel.LinX2 = 0
END SUB


SUB DeleteLetterBackspace (Ycur AS _UNSIGNED LONG, Xcur AS _UNSIGNED LONG)
   IF Ycur > LastLine THEN EXIT SUB
   IF Xcur > 0 THEN
      IF Cfg.CANIM_Erase <> 0 THEN ANIM_Erase Xcur, Ycur
      Lin(Ycur).IText = LEFT$(Lin(Ycur).IText, Xcur - 1) + RIGHT$(Lin(Ycur).IText, LEN(Lin(Ycur).IText) - Xcur)
      Xcur = Xcur - 1
   ELSE
      Lin(Ycur - 1).IText = Lin(Ycur - 1).IText + Lin(Ycur).IText
      DeleteLine Ycur: GoToLine Ycur - 1: Xcur = LEN(Lin(Ycur).IText)
   END IF
END SUB

SUB DeleteLetterDelete (Ycur AS _UNSIGNED LONG, Xcur AS _UNSIGNED LONG)
   IF Ycur > LastLine - 1 THEN EXIT SUB
   IF Xcur < LEN(Lin(Ycur).IText) THEN
      IF Cfg.CANIM_Erase <> 0 THEN ANIM_Erase Xcur + 1, Ycur
      Lin(Ycur).IText = LEFT$(Lin(Ycur).IText, Xcur) + RIGHT$(Lin(Ycur).IText, (LEN(Lin(Ycur).IText) - Xcur) - 1)
   ELSE
      Lin(Ycur).IText = Lin(Ycur).IText + Lin(Ycur + 1).IText
      DeleteLine Ycur + 1
   END IF
END SUB

FUNCTION ConvertToHardwareImg (handle AS _UNSIGNED LONG)

END FUNCTION

SUB RenderSelected
   FOR y = Sel.LinY1 TO Sel.LinY2
      IF Sel.LinY1 = Sel.LinY2 THEN LINE (ETSX(Sel.LinX1), ETSY(y))-(ETSX(Sel.LinX2), ETSY(y) + FontSizeY - 1), _RGBA32(255, 255, 255, 64), BF
      IF Sel.LinY1 <> Sel.LinY2 THEN
         IF y = Sel.LinY2 THEN LINE (ETSX(0), ETSY(y))-(ETSX(Sel.LinX2), ETSY(y) + FontSizeY - 1), _RGBA32(255, 255, 255, 64), BF: GOTO EndFor
         IF y = Sel.LinY1 THEN LINE (ETSX(Sel.LinX1), ETSY(y))-(ETSX(LEN(Lin(y).IText) + 1), ETSY(y) + FontSizeY - 1), _RGBA32(255, 255, 255, 64), BF: GOTO EndFor
         LINE (ETSX(0), ETSY(y))-(ETSX(LEN(Lin(y).IText) + 1), ETSY(y) + FontSizeY - 1), _RGBA32(255, 255, 255, 64), BF
         EndFor:
      END IF
   NEXT
END SUB

SUB AnimationLogic
   IF LEN(KeyP) > 1 THEN KeyD$ = RIGHT$(KeyP, 1)
   'Erasing Animation.
   IF KeyP = CHR$(8) OR KeyD$ = "S" THEN
      IF KeyD$ = "S" THEN S = 1
      IF MID$(Lin(LinEditY).IText, LinEditX + S, 1) = " " THEN EXIT SUB
      X = (FontSizeX * ((LinEditX + S) - LinCamX))
      Y = (FontSizeY * (LinEditY - LinCamY))
      YM = -(20 + (INT(RND * 20) * 2))
      IF S = 0 THEN XM = ((INT(RND * 20) + 2) * 2)
      IF S = 1 THEN XM = -((INT(RND * 20) + 2) * 2)
      SpawnParticle 1, Cfg.CANIM_Erase, X + FontSizeX / 2, Y + FontSizeY / 2, 1.2, XM, YM, 0, INT(RND * 16) - 8, 0
      Place = _INSTRREV(LinEditX, Lin(LinEditY).VText, "§")
      ColorNum$ = MID$(Lin(LinEditY).VText, Place + 1, 1)
      IF SubOutput1 >= 0 THEN PartLetterFromScreen Part(SubOutput1), X, Y, FontSizeX, FontSizeY, CodeLayer
   END IF
END SUB

SUB ANIM_Erase (XCur AS _UNSIGNED LONG, YCur AS _UNSIGNED LONG)
   X = (FontSizeX * ((XCur) - LinCamX2))
   Y = (FontSizeY * (YCur - LinCamY2))
   YM = -(20 + (INT(RND * 20) * 2))
   XM = ((INT(RND * 20) - 10) * 2)
   SpawnParticle 1, Cfg.CANIM_Erase, X + FontSizeX / 2, Y + FontSizeY / 2, 1.2, XM, YM, 0, INT(RND * 16) - 8, 0
   IF SubOutput1 >= 0 THEN PartLetterFromScreen Part(SubOutput1), X, Y, FontSizeX, FontSizeY, CodeLayer
END SUB

SUB ParticleSUB
   FOR i = 0 TO MaxParticles
      IF Part(i).Exists = 1 THEN
         RotoZoom Part(i).X, Part(i).Y, Part(i).Image, Part(i).Size, Part(i).Rot
         ParticlePhys Part(i)
      END IF
   NEXT
END SUB

SUB PartLetterFromScreen (Part AS Particles, X AS _UNSIGNED LONG, Y AS _UNSIGNED LONG, SizeX AS _UNSIGNED INTEGER, SizeY AS _UNSIGNED INTEGER, Handle AS LONG)
   IF Part.Image <> 0 THEN _FREEIMAGE Part.Image
   Part.Image = _NEWIMAGE(SizeX, SizeY, 32)
   _PUTIMAGE (0, 0), Handle, Part.Image, (X, Y)-(X + SizeX, Y + SizeY)
   _PUTIMAGE (0, 0)-(SizeX, SizeY), DarkAlphaSprite, Part.Image
   _CLEARCOLOR _RGB32(0, 0, 0), Part.Image
END SUB

SUB ParticlePhys (Part AS Particles)
   IF Part.Y > _HEIGHT + 15 THEN KillParticle Part
   IF Part.X > _WIDTH + 100 THEN KillParticle Part
   IF Part.Y < -100 THEN KillParticle Part
   IF Part.X < -100 THEN KillParticle Part
   SELECT CASE Part.AnimStyle

      CASE 1
         SELECT CASE Part.AnimID
            CASE 1: ParticlePhysFall Part
            CASE 2: ParticlePhysMouse Part
         END SELECT


      CASE 2

   END SELECT


END SUB

SUB ParticlePhysMouse (Part AS Particles)
   DIM dist AS DOUBLE: DIM dist2 AS DOUBLE: DIM dist3 AS DOUBLE
   Part.X = Part.X + Part.Xm / 20: Part.Y = Part.Y + Part.Ym / 20
   dx = Part.X - Mouse.x: dy = Part.Y - Mouse.y
   Part.Rot = ATan2(dy, dx) ' Angle in radians
   Part.Rot = (Part.Rot * 180 / PI) + 90 + (INT(RND * 10) - 5)
   IF Part.Rot > 180 THEN Part.Rot = Part.Rot - 179.9

   dist = Distance(Part.X, Part.Y, Mouse.x, Mouse.y) / 20: IF dist > 50 THEN dist = 50
   IF dist < 6 THEN Part.Xm = Part.Xm / 1.002: Part.Ym = Part.Ym / 1.002
   IF dist < 5 THEN Part.Xm = Part.Xm / 1.009: Part.Ym = Part.Ym / 1.009
   IF dist < 4 THEN Part.Xm = Part.Xm / 1.01: Part.Ym = Part.Ym / 1.01
   Part.Size = dist * 1.3: IF Part.Size > 1.5 THEN Part.Size = 1.5
   IF Part.Size < 0.5 THEN KillParticle Part: EXIT SUB

   IF dist < 25 THEN
      Part.Xm = (Part.Xm / 1.001) + SIN(Part.Rot * PIDIV180) * (15 / dist): Part.Ym = (Part.Ym / 1.001) + -COS(Part.Rot * PIDIV180) * (15 / dist)
   END IF
END SUB



SUB GeneratePartLetter (Part AS Particles, Text AS STRING, ColorNum AS STRING)
   IF Part.Image <> 0 THEN _FREEIMAGE Part.Image
   Part.Image = _NEWIMAGE(FontSizeX, FontSizeY, 32)
   _PRINTSTRING (0, 0), Text, Part.Image
   _CLEARCOLOR _RGB32(0, 0, 0), Part.Image
END SUB

SUB SpawnParticle (AnimStyle AS _UNSIGNED _BYTE, AnimID AS _UNSIGNED _BYTE, X AS DOUBLE, Y AS DOUBLE, size AS DOUBLE, XM AS DOUBLE, YM AS DOUBLE, Rot AS DOUBLE, RotM AS DOUBLE, Image AS LONG)
   IF X > _WIDTH THEN EXIT SUB
   IF Y > _WIDTH THEN EXIT SUB
   IF X < 0 THEN EXIT SUB
   IF Y < 0 THEN EXIT SUB
   FOR i = 0 TO MaxParticles
      IF Part(i).Exists = 0 THEN
         Part(i).X = X: Part(i).Y = Y: Part(i).Xm = XM: Part(i).Ym = YM
         Part(i).Rot = Rot: Part(i).RotM = RotM: Part(i).Image = Image: Part(i).Exists = 1
         Part(i).AnimStyle = AnimStyle: Part(i).AnimID = AnimID
         Part(i).Size = size
         SubOutput1 = i
         EXIT FOR
      END IF
   NEXT
END SUB


SUB RenderGUI
   DIM i AS _UNSIGNED LONG
   FOR i = 0 TO 10
   NEXT
END SUB

SUB CreateNewGUIObj (x AS _UNSIGNED LONG, y AS _UNSIGNED LONG, TextID AS _UNSIGNED LONG, Size AS LONG)
   ' if Size = -1 then Size = _Printwidth
END SUB

SUB DeleteGUIObj

END SUB

FUNCTION Distance (x1, y1, x2, y2)
   Distance = 1
   Dist = SQR(((x1 - x2) ^ 2) + ((y1 - y2) ^ 2))
   Distance = Dist
END FUNCTION

SUB HandleMouseScroll
   IF NOT _KEYDOWN(100304) THEN
      FOR i = 1 TO 6
         IF Mouse.scroll < 0 AND LinCamY = 0 THEN EXIT FOR
         LinCamY = LinCamY + Mouse.scroll
      NEXT
   END IF
   IF _KEYDOWN(100304) THEN
      FOR i = 1 TO 12
         IF Mouse.scroll < 0 AND LinCamX = 0 THEN EXIT FOR
         LinCamX = LinCamX + Mouse.scroll
      NEXT
   END IF
END SUB

SUB RenderLines
   LINE (ETSX(0), ETSY(LinEditY))-(_WIDTH, ETSY(LinEditY) + FontSizeY), _RGBA(0, 64, 128, 160), BF ' Change Color.
   FOR i = FIX(LinCamY2) TO INT(LinCamY2) + LinesOnScreenY + 1

      IF i < LastLine THEN PrintWithColor -(LinCamX2 * FontSizeX), ETSY(i), Lin(i).VText, CodeLayer ' PRINT Lin(i).VText
   NEXT
   ' EditX/Y
   LINE (ETSX(LinEditX + 1), ETSY(LinEditY) + (FontSizeY / 1.15))-(ETSX(LinEditX + 2), ETSY(LinEditY) + FontSizeY), _RGBA(255, 255, 255, 128), BF
   ' Mouse X/Y
   LINE (ETSX(Mouse.LinX), ETSY(Mouse.LinY) + (FontSizeY / 1.15))-(ETSX(Mouse.LinX + 1), ETSY(Mouse.LinY) + FontSizeY), _RGBA(255, 0, 255, 128), BF

END SUB

SUB AdjustEditLin
   IF Cfg.FixCursorToScreen = 0 THEN EXIT SUB
   IF LinEditY < LinCamY THEN LinEditY = LinCamY
   IF LinEditY > LinCamY + LinesOnScreenY THEN LinEditY = LinCamY + LinesOnScreenY
   IF LinEditX < LinCamX THEN LinEditX = LinCamX
   IF LinEditX > LinCamX + LinesOnScreenX THEN LinEditX = LinCamX + LinesOnScreenX
END SUB

SUB AdjustCamLin
   IF LinEditY < LinCamY THEN LinCamY = LinEditY
   IF LinEditY > LinCamY + LinesOnScreenY THEN LinCamY = LinEditY - LinesOnScreenY
   IF LinEditX > LinCamX + LinesOnScreenX THEN LinCamX = LinEditX - LinesOnScreenX
   IF LinEditX < LinCamX THEN LinCamX = LinEditX
END SUB


SUB GenerateVText (Lin AS Lin)
   Lin.VText = "§0 " + Lin.IText + " §0"
   DIM Sec(200) AS Secs
   'Special Cases
   Comment = INSTR(0, Lin.VText, "'")
   IF Comment > 0 THEN
      Lin.VText = _TRIM$(LEFT$(Lin.VText, Comment - 1) + "§5" + RIGHT$(Lin.VText, (LEN(Lin.VText) - Comment + 1)))
   ELSE
      Comment = LEN(Lin.VText) + 1
   END IF
   '     KEYWORD CHECKING:
   FOR i = 1 TO MaxKeywords
      ist = 0
      IF Keywords(i).word = "" THEN EXIT FOR
      DO
         ist = INSTR(ist, UCASE$(Lin.VText), " " + _TRIM$(UCASE$(Keywords(i).word)) + " ")
         ' Stop if cases
         IF ist > Comment THEN EXIT DO
         ist2 = 0
         IF ist > 0 THEN
            ist = ist + 1
            Lin.VText = _TRIM$(LEFT$(Lin.VText, ist - 1) + "§" + _TRIM$(STR$(Keywords(i).col)) + RIGHT$(Lin.VText, (LEN(Lin.VText) - ist + 1)))
            Lin.VText = LEFT$(Lin.VText, ist + LEN(Keywords(i).word) + 1) + "§0" + RIGHT$(Lin.VText, (LEN(Lin.VText) - (ist + LEN(Keywords(i).word) + 1)))
         END IF
         ist2 = INSTR(ist + 2, UCASE$(Lin.VText), " " + _TRIM$(UCASE$(Keywords(i).word)) + " ")
      LOOP WHILE ist2 <> 0

   NEXT
   Lin.VLength = LEN(Lin.VText)
   Lin.ILength = LEN(Lin.IText)
END SUB

SUB PrintWithColor (X AS LONG, Y AS _UNSIGNED LONG, Text AS STRING, Handle AS LONG)
   Last = 0
   OldHandle = _DEST
   _DEST Handle
   IF INSTR(0, Text, CSC) THEN
      DO
         First = INSTR(Last, Text, CSC)
         Last = INSTR(First + 1, Text, CSC): IF Last = 0 THEN Last = LEN(Text) + 1
         bytes = Last - First
         COLOR ColorFromSymbol(MID$(Text, First + 1, 1)), _RGBA32(0, 0, 0, 0)
         _PRINTSTRING (X + lens, Y), MID$(Text, First + 2, bytes - 2), Handle
         lens = lens + ((bytes - 2) * FontSizeX)
         '    quit = quit + 1: IF quit > 2048 THEN EXIT DO 'prevent sub getting stuck somehow.
      LOOP WHILE Last < LEN(Text)
   ELSE
      COLOR _RGB32(255, 255, 255), _RGB32(0, 0, 0)
      _PRINTSTRING (X, Y), Text
   END IF
   _DEST OldHandle
END SUB


SUB ExtraKeys
   IF LEN(KeyP) > 1 THEN KeyD$ = RIGHT$(KeyP, 1)
   'Move TCursor
   MovedCursor = 0
   IF NOT _KEYDOWN(100306) THEN
      IF KeyD$ = "H" AND LinEditY > 0 THEN GoToLine LinEditY - 1: MovedCursor = 1 ' Up Arrow
      IF KeyD$ = "P" AND LinEditY < LastLine THEN GoToLine LinEditY + 1: MovedCursor = 1 ' Down Arrow
      IF KeyD$ = "K" AND LinEditX > 0 THEN LinEditX = LinEditX - 1: MovedCursor = 1 ' Left Arrow
      IF KeyD$ = "M" THEN LinEditX = LinEditX + 1: MovedCursor = 1 ' Right Arrow
      IF KeyD$ = "G" THEN LinEditX = 0: MovedCursor = 1 ' Home
      IF KeyD$ = "O" THEN LinEditX = LEN(Lin(LinEditY).IText): MovedCursor = 1 ' End
   END IF

   IF MovedCursor = 1 THEN AdjustCamLin


   '  IF _KEYDOWN(100306) THEN
   '     IF KeyD$ = "H" THEN LinCamY = LinCamY - 1: KeyP = "" ' Up Arrow
   '     IF KeyD$ = "P" THEN LinCamY = LinCamY + 1: KeyP = "" ' Down Arrow
   '     IF KeyD$ = "K" THEN LinCamX = LinCamX - 1: KeyP = "" ' Left Arrow
   '     IF KeyD$ = "M" THEN LinCamX = LinCamX + 1: KeyP = "" ' Right Arrow
   '  END IF

   'BacksSpace
   IF KeyP = CHR$(8) THEN
      KeyP = ""
      IF Sel.Exists THEN
         DeleteSelection
      ELSE
         DeleteLetterBackspace LinEditY, LinEditX
      END IF
   END IF

   'Delete
   IF KeyD$ = "S" THEN
      KeyP = ""
      IF Sel.Exists THEN
         DeleteSelection
      ELSE
         DeleteLetterDelete LinEditY, LinEditX
      END IF
   END IF

   'Enter
   IF KeyP = CHR$(13) THEN NewLine: KeyP = "": LinEditX = 0


   IF KeyD$ <> "" THEN KeyP = ""
END SUB

SUB NewLine
   LastLine = LastLine + 1
   REDIM _PRESERVE Lin(LastLine + 20) AS Lin


   FOR i = LastLine - 1 TO LinEditY + 1 STEP -1
      Lin(i + 1).IText = Lin(i).IText: Lin(i).IText = " "
      Lin(i + 1).VText = Lin(i).VText: Lin(i).VText = " "
   NEXT
   Lin(LinEditY + 1).IText = MID$(Lin(LinEditY).IText, LinEditX + 1, LEN(Lin(LinEditY).IText) - (LinEditX))
   Lin(LinEditY).IText = LEFT$(Lin(LinEditY).IText, LinEditX)
   GenerateVText Lin(LinEditY)
   GenerateVText Lin(LinEditY + 1)
   GoToLine LinEditY + 1
END SUB

SUB DeleteLine (ID AS _UNSIGNED LONG)
   FOR i = ID TO LastLine - 1
      Lin(i).IText = Lin(i + 1).IText: Lin(i + 1).IText = "' Deleted Line, bug?"
      Lin(i).VText = Lin(i + 1).VText: Lin(i + 1).VText = "' Deleted Line, bug?"
      Lin(i).VLength = LEN(Lin(i).VText)
      Lin(i).ILength = LEN(Lin(i).IText)

   NEXT
   LastLine = LastLine - 1
   REDIM _PRESERVE Lin(LastLine + 20) AS Lin
END SUB



SUB WriteToLin (id AS _UNSIGNED LONG, text AS STRING)

   IF LEN(Lin(id).IText) < LinEditX THEN
      FOR i = 1 TO LinEditX - LEN(Lin(id).IText)
         Lin(id).IText = Lin(id).IText + " "
      NEXT
   END IF
   Lin(id).IText = LEFT$(Lin(id).IText, LinEditX) + text + RIGHT$(Lin(id).IText, LEN(Lin(id).IText) - LinEditX)
   LinEditX = LinEditX + 1

END SUB

SUB LoadFromFile
   DIM Iterations AS _UNSIGNED LONG
   Iterations = 0
   INPUT "FileName: ", FileName$
   OPEN FileName$ FOR INPUT AS #1
   DO WHILE NOT EOF(1)
      LINE INPUT #1, Lin(Iterations).IText
      Iterations = Iterations + 1

      IF Iterations = LastLine THEN LastLine = LastLine + 1000: REDIM _PRESERVE Lin(LastLine) AS Lin
   LOOP
   LastLine = Iterations: REDIM _PRESERVE Lin(LastLine + 20) AS Lin
   CLOSE #1
   FOR o = 0 TO LastLine
      GenerateVText Lin(o)
   NEXT
END SUB

SUB SaveToFile
   INPUT "Save File Name: ", FileName$
   OPEN FileName$ FOR OUTPUT AS #2
   FOR i = 0 TO LastLine - 2
      PRINT #2, Lin(i).IText
   NEXT
   CLOSE #2
END SUB











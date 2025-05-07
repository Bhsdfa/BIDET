' BIDET
' BIDET = Bhsdfa's IDE Turbo
' This is still work in progress, don't use it with your code without a backup!
' Everything here is subject to change.
' Version: 0.0v04b

' https://qb64phoenix.com/qb64wiki/index.php?title=_ACOS&action=edit

DO
   _LIMIT 15
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
   xm AS LONG
   ym AS LONG

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
   exists AS _BYTE
   clickable AS _BYTE
   IMGHANDLE AS _UNSIGNED LONG
   openwindow AS STRING
END TYPE
TYPE Windows
   x1 AS DOUBLE
   x2 AS DOUBLE
   y1 AS DOUBLE
   y2 AS DOUBLE

   x1o AS DOUBLE
   x2o AS DOUBLE
   y1o AS DOUBLE
   y2o AS DOUBLE

   x1b AS DOUBLE
   x2b AS DOUBLE
   y1b AS DOUBLE
   y2b AS DOUBLE

   vtitle AS STRING
   ititle AS STRING
   TitID AS _UNSIGNED LONG
   hovered AS _UNSIGNED _BYTE
   State AS _BYTE
   AnimTime AS _UNSIGNED INTEGER
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
DIM SHARED LanguageIDs(128) AS _UNSIGNED LONG
DIM SHARED Sel AS Selection
DIM SHARED Selies(64) AS Selection
DIM SHARED Mouse AS Mouse
DIM SHARED Keywords(MaxKeywords) AS Keywords
DIM SHARED Cfg AS Config
DIM SHARED HColors(16) AS LONG

LoadHighlightColors

FOR i = 1 TO MaxKeywords
   Keywords(i).word = "": Keywords(i).col = 0
NEXT

'Load keywords
IF _FILEEXISTS("BIDET/key.words") THEN
   OPEN "BIDET/key.words" FOR INPUT AS #1
   i = 0
   DO
      i = i + 1
      LINE INPUT #1, Keywords(i).word
      Keywords(i).col = 1
   LOOP UNTIL EOF(1)
   CLOSE #1
ELSE
   _MESSAGEBOX "Missing FIle.", ("File '" + "BIDET/key.words" + "' wasn't found!"), "error"
   SYSTEM
END IF

'Required
DIM SHARED LangTXT(1) AS STRING
DIM SHARED DefaultFont
DIM SHARED WindowFocused AS _UNSIGNED LONG

DIM SHARED SubOutput1 AS DOUBLE
DIM SHARED LinCamX AS _UNSIGNED LONG, LinCamY AS _UNSIGNED LONG
DIM SHARED LinCamX2 AS DOUBLE, LinCamY2 AS DOUBLE
DIM SHARED LinEditX AS _UNSIGNED LONG, LinEditY AS _UNSIGNED LONG
DIM SHARED LastLine AS _UNSIGNED LONG: LastLine = 5

DIM SHARED LinesOnScreenY AS _UNSIGNED INTEGER, LinesOnScreenX AS _UNSIGNED INTEGER
DIM SHARED Lin(LastLine) AS Lin
DIM SHARED FontSizeX
DIM SHARED FontSizeY
DIM SHARED KeyP AS STRING
'Debug
DIM SHARED IDE_DEBUG AS _BYTE
DIM SHARED Deb_LiveParts AS _UNSIGNED INTEGER
DIM SHARED Deb_LiveWindows AS _UNSIGNED INTEGER
'Related to Dim
DIM SHARED MaxParticles AS _UNSIGNED LONG: MaxParticles = 99999
DIM SHARED LastGUI AS _UNSIGNED LONG: LastGUI = 0
DIM SHARED MaxGUI AS _UNSIGNED LONG: MaxGUI = 16
DIM SHARED LastWindows AS _UNSIGNED LONG: LastWindows = 1
DIM SHARED MaxWindows AS _UNSIGNED LONG: MaxWindows = 16

DIM SHARED GUI(MaxGUI) AS GUI
DIM SHARED Windows(MaxWindows) AS Windows



'MISC stuff:
Cfg.Gravity = 3.5
Cfg.FixCursorToScreen = 0

'Animations:
Cfg.CANIM_Erase = 2

' Decorative.
DIM SHARED Part(MaxParticles) AS Particles

DefaultFont = _LOADFONT("BIDET/Fonts/ComicMono-Bold.ttf", 19, "MONOSPACE,UNICODE")
_FONT DefaultFont, 0
_FONT DefaultFont, CodeLayer


FontSizeX = _PRINTWIDTH("ABC") / 3
FontSizeY = _FONTHEIGHT(DefaultFont)
DIM SHARED Delay AS _UNSIGNED LONG
LinesOnScreenY = FIX(_HEIGHT / FontSizeY) - 1
LinesOnScreenX = FIX(_WIDTH / FontSizeX) - 1
CONST CSC = "§"
_DELAY 0.5
LoadLanguage "English"
CreateNewGUIObj 0, 0, (Wrd(4, 2)), -1, -1, "file" 'file
CreateNewGUIObj SubOutput1 + (FontSizeX * 2), 0, (Wrd(5, 2)), -1, -1, "edit"
CreateNewGUIObj SubOutput1 + (FontSizeX * 2), 0, (Wrd(6, 2)), -1, -1, "view"
CreateNewGUIObj SubOutput1 + (FontSizeX * 2), 0, (Wrd(7, 2)), -1, -1, "search"
CreateNewGUIObj SubOutput1 + (FontSizeX * 2), 0, (Wrd(8, 2)), -1, -1, "run"
CreateNewGUIObj SubOutput1 + (FontSizeX * 2), 0, (Wrd(9, 2)), -1, -1, "debug"
CreateNewGUIObj SubOutput1 + (FontSizeX * 2), 0, (Wrd(10, 2)), -1, -1, "options"
CreateNewGUIObj SubOutput1 + (FontSizeX * 2), 0, (Wrd(10, 2)), -1, -1, "tools"
DO
   CLS , _RGB(0, 10, 45): _LIMIT 5 + (_WINDOWHASFOCUS * -70): IF Delay > 0 THEN Delay = Delay - 1
   'Mouse related shenanigans.
   Mouse.scroll = 0
   Mouse.xm = Mouse.x: Mouse.ym = Mouse.y
   DO WHILE _MOUSEINPUT
      Mouse.x = _MOUSEX: Mouse.y = _MOUSEY
      Mouse.click1 = _MOUSEBUTTON(1): Mouse.click2 = _MOUSEBUTTON(2): Mouse.click3 = _MOUSEBUTTON(3)
      IF _MOUSEWHEEL <> 0 THEN Mouse.scroll = _MOUSEWHEEL
   LOOP
   Mouse.xm = Mouse.xm - Mouse.x: Mouse.ym = Mouse.ym - Mouse.y
   IF KeyP <> "" THEN _KEYCLEAR
   KeyP = INKEY$
   GUILogic
   WindowLogic

   GetCursorOnText ' Responsable for text selection AND clicking.
   LinCamX2 = LinCamX2 + ((LinCamX + 0.01) - LinCamX2) / 10
   LinCamY2 = LinCamY2 + ((LinCamY + 0.01) - LinCamY2) / 10

   'Mouse scrolling
   IF Mouse.scroll <> 0 THEN HandleMouseScroll: AdjustEditLin
   ShortCuts 'Things like 'CTRL + O' to open files.

   '  AnimationLogic
   IF KeyP <> "" THEN ExtraKeys: GenerateVText Lin(LinEditY)
   IF KeyP <> "" THEN WriteToLin LinEditY, KeyP: GenerateVText Lin(LinEditY)

   ' Render on screen.
   CLS , , CodeLayer
   RenderLines
   _PUTIMAGE (0, 0), CodeLayer, 0

   ParticleSUB
   RenderGUI
   IDEDEBUG
   _DISPLAY
LOOP

SUB GUILogic
   FOR i = 0 TO MaxGUI
      IF GUI(i).exists THEN

         IF Mouse.click1 AND Delay = 0 AND CheckIfBounds(GUI(i).x1, GUI(i).y1, GUI(i).x2, GUI(i).y2, Mouse.x, Mouse.y) THEN GUIClicked GUI(i): Delay = 10
      END IF


   NEXT
END SUB

SUB GUIClicked (GUI AS GUI)
   IF GUI.openwindow <> "" THEN
      NewWindow GUI.x1, GUI.y1, GUI.x2, GUI.y2, (_WIDTH / 2) - 200, (_HEIGHT / 2) - 150, (_WIDTH / 2) + 200, (_HEIGHT / 2) + 150, GUI.openwindow
   END IF
   Mouse.click1 = 0
END SUB

FUNCTION CheckIfBounds (x1 AS LONG, y1 AS LONG, x2 AS LONG, y2 AS LONG, x3 AS LONG, y3 AS LONG)
   CheckIfBounds = 0
   IF x3 > x1 THEN: IF x3 < x2 THEN: IF y3 > y1 THEN: IF y3 < y2 THEN
               CheckIfBounds = -1
   END IF: END IF: END IF: END IF
END FUNCTION

SUB NewWindow (x1 AS DOUBLE, y1 AS DOUBLE, x2 AS DOUBLE, y2 AS DOUBLE, x1b AS DOUBLE, y1b AS DOUBLE, x2b AS DOUBLE, y2b AS DOUBLE, ititle AS STRING)
   DIM i AS _UNSIGNED LONG
   i = LastWindows
   Deb_LiveWindows = Deb_LiveWindows + 1
   IF LastWindows < MaxWindows THEN
      Windows(i).x1 = x1: Windows(i).x1o = x1
      Windows(i).x2 = x2: Windows(i).x2o = x2
      Windows(i).y1 = y1: Windows(i).y1o = y1
      Windows(i).y2 = y2: Windows(i).y2o = y2:
      Windows(i).x1b = x1b: Windows(i).x2b = x2b
      Windows(i).y1b = y1b: Windows(i).y2b = y2b
      Windows(i).ititle = ititle: Windows(i).State = 1
      Windows(i).IMGHANDLE = _NEWIMAGE(x2b - x1b, y2b - y1b, 32)
      SELECT CASE LCASE$(ititle)
         CASE "options": Windows(i).vtitle = Wrd$(10, 2)
      END SELECT

      LastWindows = LastWindows + 1
   END IF
END SUB

SUB KillWIndow (win AS Windows)
   Deb_LiveWindows = Deb_LiveWindows - 1
   SWAP win.x1b, win.x1o
   SWAP win.x2b, win.x2o
   SWAP win.y1b, win.y1o
   SWAP win.y2b, win.y2o
   win.ititle = ""
   win.State = -1
   win.vtitle = ""
END SUB


SUB WindowLogic

   FOR i = 1 TO MaxWindows
      IF Windows(i).State <> 0 THEN
         RenderWindowsOutside Windows(i)
         IF Windows(i).State = 1 OR Windows(i).State = -1 THEN WindowsAnim Windows(i)


      END IF
   NEXT
   IF Windows(WindowFocused).State = 2 THEN WindowLiveLogic Windows(WindowFocused)
   WindowFocused = 0
   FOR o = MaxWindows TO 1 STEP -1
      IF Windows(o).State = 2 AND CheckIfBounds(Windows(o).x1, Windows(o).y1, Windows(o).x2, Windows(o).y2, Mouse.x, Mouse.y) THEN WindowFocused = o: EXIT FOR
   NEXT

END SUB

SUB WindowLiveLogic (Win AS Windows)
   LINE (Win.x1, Win.y1)-(Win.x2, Win.y2), _RGBA(255, 0, 0, 128), BF
   DIM Lmx AS LONG: DIM Lmy AS LONG
   SizeX = Win.x2 - Win.x1
   SizeY = Win.y2 - Win.y2
   Lmx = Mouse.x - Win.x1
   Lmy = Mouse.y - Win.y1
   IF Mouse.click3 THEN
      Mouse.click1 = 0
      Mouse.click2 = 0
      Mouse.click3 = 0
      Win.x1 = Win.x1 - Mouse.xm
      Win.x2 = Win.x2 - Mouse.xm
      Win.y1 = Win.y1 - Mouse.ym
      Win.y2 = Win.y2 - Mouse.ym
   END IF

   IF Lmx > SizeX - FontSizeY AND Lmy < FontSizeY AND Mouse.click1 THEN KillWIndow Win


   'Rendering
   _DEST Win.IMGHANDLE
   CLS
   LINE (0, 0)-(_WIDTH, FontSizeY), _RGB32(255, 255, 0), BF
   LINE (SizeX - FontSizeY, 0)-(SizeX, FontSizeY), _RGB32(255, 0, 0), BF
   PRINT "x1: "; Win.x1
   PRINT "y1: "; Win.y1
   PRINT "x2: "; Win.x2
   PRINT "y2: "; Win.y2
   PRINT "Lmx: "; Lmx
   PRINT "Lmy: "; Lmy
   _DEST 0
   Mouse.click1 = 0
   Mouse.click2 = 0
   Mouse.click3 = 0
END SUB

SUB WindowsAnim (Win AS Windows)
   Win.AnimTime = Win.AnimTime + 2
   IF Win.AnimTime > 60 THEN Win.AnimTime = 60
   Win.x1 = Win.x1 + (Win.x1b - Win.x1) / (10 / (Win.AnimTime / 40))
   Win.x2 = Win.x2 + (Win.x2b - Win.x2) / (10 / (Win.AnimTime / 40))
   Win.y1 = Win.y1 + (Win.y1b - Win.y1) / (10 / (Win.AnimTime / 40))
   Win.y2 = Win.y2 + (Win.y2b - Win.y2) / (10 / (Win.AnimTime / 40))
   distx1 = ABS(Win.x1 - Win.x1b): distx2 = ABS(Win.x2 - Win.x2b)
   disty1 = ABS(Win.y1 - Win.y1b): disty2 = ABS(Win.y2 - Win.y2b)
   dist = distx1 + distx2 + disty1 + disty2

   IF dist < 6 THEN
      Win.x1 = Win.x1b: Win.x2 = Win.x2b
      Win.y1 = Win.y1b: Win.y2 = Win.y2b
      Win.State = Win.State + 1: Win.AnimTime = 0
   END IF
   IF Win.State = 0 THEN _FREEIMAGE Win.IMGHANDLE: LastWindows = LastWindows - 1
END SUB

SUB RenderWindowsOutside (Win AS Windows)

   _PUTIMAGE (Win.x1, Win.y1)-(Win.x2, Win.y2), Win.IMGHANDLE, 0
   LINE (Win.x1, Win.y1)-(Win.x2, Win.y2), _RGBA32(255, 255, 255, 80), BF
END SUB

SUB IDEDEBUG
   IF IDE_DEBUG = 0 THEN EXIT SUB
   LINE (0, _HEIGHT)-(_WIDTH, _HEIGHT - (4 * FontSizeY)), _RGB(0, 0, 0), BF
   PrintWithColor 0, _HEIGHT - (4 * FontSizeY), ("§5LinCamX = §3" + STR$(LinCamX)), 0
   PrintWithColor 0, _HEIGHT - (3 * FontSizeY), ("§5LinCamy = §3" + STR$(LinCamY)), 0
   PrintWithColor 0, _HEIGHT - (2 * FontSizeY), ("§5LinCamX2 = §3" + Trunc(LinCamX2, 3)), 0
   PrintWithColor 0, _HEIGHT - (1 * FontSizeY), ("§5LinCamy2 = §3" + Trunc(LinCamY2, 3)), 0
   Size = SubOutput1 * FontSizeX
   PrintWithColor Size, _HEIGHT - (4 * FontSizeY), ("§5LiveParts = §3" + STR$(Deb_LiveParts)), 0
   PrintWithColor Size, _HEIGHT - (3 * FontSizeY), ("§5LiveWinds = §3" + STR$(Deb_LiveWindows)), 0
   PrintWithColor Size, _HEIGHT - (2 * FontSizeY), ("§5Lines = §3" + STR$(LastLine)), 0
END SUB

SUB ShortCuts
   IF _KEYDOWN(100304) AND _KEYDOWN(15616) AND Delay = 0 THEN Delay = 15: IDE_DEBUG = IDE_DEBUG + 1: IF IDE_DEBUG = 2 THEN IDE_DEBUG = 0 ' SHIFT + F3 to activate IDE debug.
   IF _KEYDOWN(100306) AND _KEYDOWN(111) AND Delay = 0 THEN Delay = 10: INPUT "Filename: ", FilePath$: LoadFromFile FilePath$ ' CTRL + O = Load from file.
   IF _KEYDOWN(100306) AND _KEYDOWN(115) AND Delay = 0 THEN Delay = 10: SaveToFile ' CTRL + S = Save to file
END SUB

FUNCTION Trunc$ (Value AS DOUBLE, After AS _UNSIGNED INTEGER)
   Trunc = LEFT$(STR$(Value), LEN(STR$(INT(Value))) + After)
END FUNCTION

SUB ShortKeys
   'Scrolling text with PAGE UP/DOWN
   IF _KEYDOWN(18688) THEN LinCamY = ChangeCam(LinCamY, -LinesOnScreenY) 'Page Up
   IF _KEYDOWN(20736) THEN LinCamY = ChangeCam(LinCamY, LinesOnScreenY * 20) ' Page Down
END SUB

FUNCTION ChangeCam~& (Cam AS _UNSIGNED LONG, Value AS LONG)
   DIM I64 AS _INTEGER64
   I64 = Cam + Value
   IF I64 <= 0 THEN ChangeCam~& = 0: Value = 0: PRINT "I64 = "; I64: _DISPLAY: _DELAY 2: EXIT FUNCTION
   ChangeCam~& = Cam + Value
END FUNCTION

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
   DIM temp AS _UNSIGNED LONG
   IF Part.Image <> 0 THEN _FREEIMAGE Part.Image
   Part.Image = _NEWIMAGE(SizeX, SizeY, 32)
   _PUTIMAGE (0, 0), Handle, Part.Image, (X, Y)-(X + SizeX, Y + SizeY)
   _PUTIMAGE (0, 0)-(SizeX, SizeY), DarkAlphaSprite, Part.Image
   _CLEARCOLOR _RGB32(0, 0, 0), Part.Image

   temp = _COPYIMAGE(Part.Image, 33)
   _FREEIMAGE Part.Image
   Part.Image = temp
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
         Deb_LiveParts = Deb_LiveParts + 1
         EXIT FOR
      END IF
   NEXT
END SUB


SUB RenderGUI
   DIM i AS _UNSIGNED LONG
   FOR i = 0 TO 10
      LINE (GUI(i).x1, GUI(i).y1)-(GUI(i).x2, GUI(i).y2), _RGB32(0, 0, 0), BF
      _PUTIMAGE (GUI(i).x1, GUI(i).y1)-(GUI(i).x2, GUI(i).y2), GUI(i).IMGHANDLE
   NEXT
END SUB

SUB CreateNewGUIObj (x AS _UNSIGNED LONG, y AS _UNSIGNED LONG, Text AS STRING, SizeX AS LONG, SizeY AS LONG, WName AS STRING)
   DIM Temp AS _UNSIGNED LONG
   IF LastGUI < MaxGUI THEN
      GUI(LastGUI).exists = -1
      GUI(LastGUI).openwindow = WName
      GUI(LastGUI).x1 = x
      GUI(LastGUI).y1 = y
      IF SizeX = -1 THEN SizeX = (LEN(Text)) * _FONTWIDTH(DefaultFont)
      IF SizeY = -1 THEN SizeY = _FONTHEIGHT(DefaultFont)
      GUI(LastGUI).IMGHANDLE = _NEWIMAGE(SizeX, SizeY, 32)
      _FONT DefaultFont, GUI(LastGUI).IMGHANDLE
      GUI(LastGUI).x2 = GUI(LastGUI).x1 + SizeX
      GUI(LastGUI).y2 = GUI(LastGUI).y1 + SizeY
      PrintWithColor 0, 0, Text, GUI(LastGUI).IMGHANDLE



      Temp = _COPYIMAGE(GUI(LastGUI).IMGHANDLE, 33)
      _FREEIMAGE GUI(LastGUI).IMGHANDLE
      GUI(LastGUI).IMGHANDLE = Temp


      LastGUI = LastGUI + 1
   END IF

   SubOutput1 = GUI(LastGUI - 1).x2

END SUB

SUB LoadLanguage (Lang AS STRING)
   OPEN ("BIDET/Languages/" + Lang + ".lang") FOR INPUT AS #9
   DIM i AS _UNSIGNED LONG
   DO
      REDIM _PRESERVE LangTXT(i) AS STRING
      LINE INPUT #9, LangTXT(i)
      i = i + 1
   LOOP WHILE NOT EOF(9)
   CLOSE #9
END SUB

FUNCTION Wrd$ (ID AS _UNSIGNED LONG, stat AS _UNSIGNED _BYTE)
   ID = ID - 1
   DIM Res AS STRING
   IF ID < UBOUND(LangTXT) THEN
      Res = LangTXT(ID)
   ELSE
      Res = "§6#NOTXT#"
   END IF
   SELECT CASE stat
      CASE 0
         Wrd$ = LCASE$(Res)
      CASE 1
         Wrd$ = UCASE$(Res)
      CASE 2
         Wrd$ = (UCASE$(LEFT$(Res, 1)) + LCASE$(RIGHT$(Res, LEN(Res) - 1)))
   END SELECT
END FUNCTION


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
   DIM i AS _UNSIGNED LONG
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
      LOOP WHILE Last < LEN(Text)
   ELSE
      COLOR _RGB32(255, 255, 255), _RGB32(0, 0, 0)
      _PRINTSTRING (X, Y), Text
   END IF
   _DEST OldHandle
   SubOutput1 = LEN(Text)
END SUB


SUB ExtraKeys
   IF LEN(KeyP) > 1 THEN KeyD$ = RIGHT$(KeyP, 1)
   'Move TCursor
   MovedCursor = 0
   IF NOT _KEYDOWN(100306) THEN
      IF KeyD$ = "H" AND LinEditY > 1 THEN GoToLine LinEditY - 1: MovedCursor = 1 ' Up Arrow
      IF KeyD$ = "P" AND LinEditY < LastLine THEN GoToLine LinEditY + 1: MovedCursor = 1 ' Down Arrow
      IF KeyD$ = "K" AND LinEditX > 0 THEN LinEditX = LinEditX - 1: MovedCursor = 1 ' Left Arrow
      IF KeyD$ = "M" THEN LinEditX = LinEditX + 1: MovedCursor = 1 ' Right Arrow
      IF KeyD$ = "G" THEN LinEditX = 0: MovedCursor = 1 ' Home
      IF KeyD$ = "O" THEN LinEditX = LEN(Lin(LinEditY).IText): MovedCursor = 1 ' End
   END IF
   ShortKeys
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

SUB LoadFromFile (FilePath AS STRING)
   DIM Iterations AS _UNSIGNED LONG
   Iterations = 1

   IF _FILEEXISTS(FilePath) THEN

      OPEN FilePath FOR INPUT AS #1
      DO WHILE NOT EOF(1)
         LINE INPUT #1, Lin(Iterations).IText
         Iterations = Iterations + 1

         IF Iterations = LastLine THEN LastLine = LastLine + 2: REDIM _PRESERVE Lin(LastLine) AS Lin
      LOOP
      LastLine = Iterations: REDIM _PRESERVE Lin(LastLine + 20) AS Lin
      CLOSE #1
      FOR o = 0 TO LastLine
         GenerateVText Lin(o)
      NEXT

   ELSE
      PRINT ("File '" + FilePath + "' not found!!!")
      _DISPLAY
      BEEP
      _DELAY 1
   END IF

END SUB

SUB SaveToFile
   INPUT "Save File Name: ", FileName$
   OPEN FileName$ FOR OUTPUT AS #2
   FOR i = 0 TO LastLine - 2
      PRINT #2, Lin(i).IText
   NEXT
   CLOSE #2
END SUB









SUB ParticlePhysFall (Part AS Particles)
   Part.X = Part.X + Part.Xm / 10
   Part.Y = Part.Y + Part.Ym / 10
   Part.Rot = Part.Rot + Part.RotM
   Part.Xm = Part.Xm / 1.01
   Part.Ym = Part.Ym / 1.01
   Part.Ym = Part.Ym + Cfg.Gravity
END SUB

FUNCTION ATan2 (y AS SINGLE, x AS SINGLE)
   DIM AtanResult AS SINGLE
   IF x = 0 THEN
      IF y > 0 THEN
         AtanResult = PI / 2
      ELSEIF y < 0 THEN
         AtanResult = -PI / 2
      ELSE
         AtanResult = 0
      END IF
   ELSE
      AtanResult = ATN(y / x)
      IF x < 0 THEN
         IF y >= 0 THEN AtanResult = AtanResult + PI
      ELSE AtanResult = AtanResult - PI
      END IF
   END IF
   ATan2 = AtanResult
END FUNCTION

SUB KillParticle (Part AS Particles)
   Part.X = 0: Part.Y = 0: Part.Xm = 0: Part.Ym = 0: Part.Rot = 0
   Part.Exists = 0: Part.RotM = 0
   IF Part.Image <> 0 THEN _FREEIMAGE Part.Image
END SUB

FUNCTION ColorFromSymbol~& (Text AS STRING)
   ColorFromSymbol~& = _RGB32(255, 255, 255) ' Default
   ColorFromSymbol~& = HColors(VAL(Text))
END FUNCTION

SUB GoToLine (id AS _UNSIGNED LONG)
   IF id < LastLine THEN
      LinEditY = id
    END IF
END SUB

SUB RotoZoom (X AS LONG, Y AS LONG, Image AS LONG, Scale AS SINGLE, Rotation AS SINGLE)
   DIM px(3) AS SINGLE: DIM py(3) AS SINGLE
   W& = _WIDTH(Image&): H& = _HEIGHT(Image&)
   px(0) = -W& / 2: py(0) = -H& / 2: px(1) = -W& / 2: py(1) = H& / 2
   px(2) = W& / 2: py(2) = H& / 2: px(3) = W& / 2: py(3) = -H& / 2
   sinr! = SIN(-Rotation / 57.2957795131): cosr! = COS(-Rotation / 57.2957795131)
   FOR i& = 0 TO 3
      x2& = (px(i&) * cosr! + sinr! * py(i&)) * Scale + X: y2& = (py(i&) * cosr! - px(i&) * sinr!) * Scale + Y
      px(i&) = x2&: py(i&) = y2&
   NEXT
   _MAPTRIANGLE (0, 0)-(0, H& - 1)-(W& - 1, H& - 1), Image& TO(px(0), py(0))-(px(1), py(1))-(px(2), py(2))
   _MAPTRIANGLE (0, 0)-(W& - 1, 0)-(W& - 1, H& - 1), Image& TO(px(0), py(0))-(px(3), py(3))-(px(2), py(2))
END SUB

SUB LoadHighlightColors
   OPEN "BIDET/highlight colors.ini" FOR INPUT AS #7
   DO WHILE NOT EOF(7)
      LINE INPUT #7, Liner$
      start = INSTR(0, Liner$, "=") + 1
      ends = INSTR(0, Liner$, "'")
      bytes = ABS(start - ends)
      Var$ = _TRIM$(LEFT$(Liner$, start - 2))
      Com$ = _TRIM$(MID$(Liner$, start, bytes))
      HColors(VAL(Var$)) = VAL("&HFF" + Com$)
   LOOP
   CLOSE #7
END SUB

FUNCTION ETSX (x AS _UNSIGNED LONG)
   ETSX = FontSizeX * (x - LinCamX2)
END FUNCTION
FUNCTION ETSY (y AS _UNSIGNED LONG)
   ETSY = FontSizeY * (y - LinCamY2)
END FUNCTION
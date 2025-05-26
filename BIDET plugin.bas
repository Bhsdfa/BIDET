
' Thanks to Fellippe Heitor!
' https://github.com/FellippeHeitor/tcpip-experiments
' Tamo junto mano

' This is made to work WITH BIDET.
DIM SHARED MainImage AS _UNSIGNED LONG
MainImage = _NEWIMAGE(400, 300, 32)
SCREEN MainImage
TIMER ON

DIM SHARED Bhost AS SINGLE

'Settings:
' NoFocus{0|1} (Runs even without focus)
' Resizable{0|1} (Allows window to resize)
' Title{} (Window title)
' Color{} (Window title color)
'
DIM Col AS _UNSIGNED LONG
Col = _RGB32(28, 155, 238)
LogInBIDET ("Title{RandomShit},Color{" + _TRIM$(STR$(Col)) + "},NoFocus,Resizable")


DO
   IF _EXIT THEN
      b$ = "BYE!>Dawg"
      Send Bhost, b$
      SYSTEM
   END IF

   _LIMIT 30
   'LINE (_WIDTH / 2 - 128, _HEIGHT / 2 - 100)-(_WIDTH / 2 + 128, _HEIGHT / 2 + 100), _RGB32(255, 255, 255), BF
   LINE (0, 0)-(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 7), BF
   LINE (INT(RND * 400), INT(RND * 400))-(INT(RND * 400), INT(RND * 400)), _RGB32(255, 255, INT(RND * 255))
   LINE (INT(RND * 400), INT(RND * 400))-(INT(RND * 400), INT(RND * 400)), _RGB32(255, 255, INT(RND * 255))
   _PRINTSTRING (0, 0), STR$(TIMER)
   CommandHandler
   IMGToBIDET 0







LOOP

SUB CommandHandler
   GET #Bhost, , incomingData$
   stream$ = stream$ + incomingData$

   DO WHILE INSTR(stream$, "<END>")
      thisData$ = LEFT$(stream$, INSTR(stream$, "<END>") - 1)
      stream$ = MID$(stream$, INSTR(stream$, "<END>") + 5)
      thisCommand$ = LEFT$(thisData$, INSTR(thisData$, ">") - 1)
      IF thisCommand$ = "" THEN
         thisCommand$ = thisData$
      ELSE
         thisData$ = MID$(thisData$, LEN(thisCommand$) + 2)
      END IF

      SELECT CASE thisCommand$
         CASE "BYE!"
            BEEP
            SYSTEM
         CASE "CLICK"
            FOR i = 1 TO 30
               CIRCLE (CVI(LEFT$(thisData$, 2)), CVI(RIGHT$(thisData$, 2))), i, _RGB32(255, 255, 255)
            NEXT
         CASE "PING"
            ping = TIMER
            b$ = "PONG>"
            Send Bhost, b$
      END SELECT
   LOOP

END SUB

SUB IMGToBIDET (Handle AS _UNSIGNED LONG)
   myCanvas& = _COPYIMAGE(Handle)
   DIM imgMem AS _MEM
   imgMem = _MEMIMAGE(myCanvas&)
   b$ = SPACE$(imgMem.SIZE)
   _MEMGET imgMem, imgMem.OFFSET, b$
   _MEMFREE imgMem
   _FREEIMAGE myCanvas&

   IF prevImage$ <> b$ THEN
      prevImage$ = b$
      b$ = "IMAGE>" + b$
      Send Bhost, b$
   END IF

END SUB
SUB Send (channel, __theData$)
   theData$ = __theData$ + "<END>"
   PUT #channel, , theData$
END SUB

SUB LogInBIDET (Settings AS STRING)
   DIM Checksum AS STRING
   Checksum = _MD5$(Settings)
   PRINT "Looking for host..."
   Bhost = _OPENCLIENT("TCP/IP:63451:localhost")
   IF Bhost = 0 THEN SYSTEM
   Da$ = "HELLO> " + _TRIM$(Settings) + "<END>"
   PUT #Bhost, , Da$

END SUB





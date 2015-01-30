DECLARE SUB tex.load (fsrc$)
'dos mouse type
TYPE DosMouse
x AS LONG
y AS LONG
lb AS INTEGER
rb AS INTEGER
oldX AS LONG
oldY AS LONG
oldLb AS INTEGER
oldRb AS INTEGER
mode AS INTEGER
attr AS INTEGER
oldMype AS INTEGER
oldAttr AS INTEGER
virtattr AS INTEGER
END TYPE

'function declarations
DECLARE SUB ms.load ()
DECLARE SUB ms.init ()
DECLARE SUB ms.show ()
DECLARE SUB ms.start2 ()
DECLARE SUB ms.show2 ()
DECLARE SUB ms.end2 ()
DECLARE SUB ms.start3 ()
DECLARE SUB ms.show3 ()
DECLARE SUB ms.writeat2 (char%, attr%)
DECLARE SUB ms.writeat3 (char%, attr%)
DECLARE SUB ms.hide ()
DECLARE SUB ms.range (x1%, y1%, x2%, y2%)
DECLARE SUB ms.put (x&, y&)
DECLARE SUB ms.status ()
DECLARE SUB ms.relativestatus ()
DECLARE SUB ms.start2 ()
DECLARE FUNCTION dat.datum% (fl1$, pos1&)
DECLARE SUB dat.loaddata (fl1$, pos1&, pos2&, segment&)
DECLARE SUB dat.loadpic (fl1$)
DECLARE SUB makecopy ()
DECLARE SUB savedat (fl1$)
DECLARE FUNCTION lastIndexOf% (s$, f$)
DECLARE FUNCTION fl.name$ (fsrc$)
DECLARE FUNCTION fl.full$ (fsrc$)

'Draw a Screen 0 slided Image = "aaaa.dat"
'bsave 4000 bytes
'xpos,ypos,char,attr
'if(xpos=255) then new slide


'handle errors
ON ERROR GOTO err.handler
DIM SHARED Err$, MouseMod$, Mouse AS DosMouse
Mouse.mode = 1
Mouse.attr = 1
Mouse.virtattr = 0


'start
SCREEN 0
ms.init
ms.hide
ms.put 0, 0
tex.load "aaaa.dat"

mouse.start2
Jerry.MouseType = 1
Jerry.mouseattrib = 1
DO
        k$ = ""
        WHILE k$ = ""
        mouse.show2
        k$ = INKEY$
        IF (k$ = CHR$(27)) THEN EXIT DO
        attr% = attr2% * 128 + attr3% * 16 + attr1%
        IF (Jerry.left = 1) THEN mouse.writeat2 Jerry.MouseType, Jerry.mouseattrib
        IF (Jerry.right = 1) THEN mouse.writeat2 0, 0
        IF (Jerry.left = 1 AND Jerry.right = 1) THEN CLS
        WEND
       
SELECT CASE k$
'Change main colour + (Ctrl+Z)
CASE CHR$(26)
a1% = Jerry.mouseattrib AND &HF
a1% = a1% + 1
IF a1% > 15 THEN a1% = 0
Jerry.mouseattrib = (Jerry.mouseattrib AND &HF0) OR a1%
'Change main colour - (Ctrl+X)
CASE CHR$(24)
a1% = Jerry.mouseattrib AND &HF
a1% = a1% - 1
IF (a1% < 0) THEN a1% = 15
Jerry.mouseattrib = (Jerry.mouseattrib AND &HF0) OR a1%
'Change blink colour + (Ctrl+C)
CASE CHR$(3)
a1% = Jerry.mouseattrib AND &H80
a1% = a1% + 1
IF (a1% > 128) THEN a1% = 0
Jerry.mouseattrib = (Jerry.mouseattrib AND &H7F) OR a1%
'Change back colour +  (Ctrl+V)
CASE CHR$(22)
a1% = (Jerry.mouseattrib AND &H70) \ 16
a1% = a1% + 1
IF (a1% > 7) THEN a1% = 0
Jerry.mouseattrib = (Jerry.mouseattrib AND &H8F) OR (a1% * 16)
'Change back colour -  (Ctrl+B)
CASE CHR$(2)
a1% = (Jerry.mouseattrib AND &H70) \ 16
a1% = a1% - 1
IF (a1% < 0) THEN a1% = 7
Jerry.mouseattrib = (Jerry.mouseattrib AND &H8F) OR (a1% * 16)
'Change char +  (Ctrl+Q)
CASE CHR$(17)
a1% = Jerry.MouseType
a1% = a1% + 1
IF a1% > 255 THEN a1% = 0
Jerry.MouseType = a1%
'Change char -  (Ctrl+W)
CASE CHR$(23)
a1% = Jerry.MouseType
a1% = a1% - 1
IF a1% < 0 THEN a1% = 255
Jerry.MouseType = a1%
'Copy char     (Ctrl+E)
CASE CHR$(5)
DEF SEG = &HB800
Jerry.MouseType = PEEK(5000)
DEF SEG
'Copy attrib  (Ctrl+R)
CASE CHR$(18)
DEF SEG = &HB800
Jerry.mouseattrib = PEEK(5001)
DEF SEG
'Copy char and attrib  (Ctrl+T)
CASE CHR$(20)
DEF SEG = &HB800
Jerry.MouseType = PEEK(5000)
Jerry.mouseattrib = PEEK(5001)
DEF SEG
'Display location  (Ctrl+Y)
CASE CHR$(25)
DEF SEG = &HB800
FOR lv1& = 0 TO 159
POKE 4000 + lv1&, PEEK(lv1&)
NEXT
DEF SEG
LOCATE 1, 1
PRINT "Location:"; Jerry.xpos, Jerry.ypos
k$ = INPUT$(1)
DEF SEG = &HB800
FOR lv1& = 0 TO 159
POKE lv1&, PEEK(4000 + lv1&)
NEXT
DEF SEG
'Save slide in save.dat  (Ctrl+P)
CASE CHR$(16)
save% = save% + 1
mouse.end2
savedat "save.dat"
makecopy
DEF SEG = &HB800
FOR lv1& = 0 TO 159
POKE 4000 + lv1&, PEEK(lv1&)
NEXT
DEF SEG
LOCATE 1, 1
PRINT "New slide"; save%
k$ = INPUT$(1)
DEF SEG = &HB800
FOR lv1& = 0 TO 159
POKE lv1&, PEEK(4000 + lv1&)
NEXT
DEF SEG
'Write char on press
CASE ELSE
mouse.writeat2 ASC(k$), Jerry.mouseattrib
Jerry.xpos = Jerry.xpos + 1
IF (Jerry.xpos > 79) THEN Jerry.xpos = 79
mouse.put Jerry.xpos, Jerry.ypos
END SELECT
LOOP

mouse.end2

'Save
IF (save% = 0 AND saveused% = 0) THEN
OPEN "O", #1, "aaaa.dat"
CLOSE #1
DEF SEG = &HB800
OPEN "B", #1, "aaaa.dat"
FOR i& = 0 TO 3999
k$ = CHR$(PEEK(i&))
PUT #1, i& + 1, k$
NEXT
CLOSE #1
DEF SEG
END IF


err.handler:
RESUME NEXT

FUNCTION dat.datum% (fl1$, pos1&)
SHARED file AS filestring
fr% = FREEFILE
OPEN "B", #fr%, fl1$
SEEK #fr%, pos1&
file.byte = INPUT$(1, #fr%)
CLOSE #fr%
dat.datum% = ASC(file.byte)
END FUNCTION

SUB dat.loaddata (fl1$, pos1&, pos2&, segment&)
SHARED file AS filestring
DEF SEG = segment&
fr% = FREEFILE
OPEN "B", #fr%, fl1$
IF (pos2& > LOF(fr%)) THEN pos2& = LOF(fr%)
posp& = pos1&
sz& = pos2& - pos1& + 3
POKE 0, (sz& AND &HFF00) \ &H100
POKE 1, sz& MOD 256
mem1& = 2
DO UNTIL posp& > pos2&
SEEK #fr%, posp&
posp& = posp& + 1
file.byte = INPUT$(1, #fr%)
POKE mem1&, ASC(file.byte)
mem1& = mem1& + 1
LOOP
CLOSE #fr%
DEF SEG
END SUB

SUB dat.loadpic (fl1$)
SHARED save%
fr% = FREEFILE
OPEN "B", #fr%, fl1$
length& = LOF(fr%)
pos1& = 1
DO
SEEK #fr%, pos1&
IF (pos1& + 4 > length&) THEN EXIT DO
SEEK #fr%, pos1&
k$ = INPUT$(4, #fr%)
IF (LEFT$(k$, 1) = CHR$(255)) THEN
        pos1& = pos1& + 1
        SOUND 21000, 5
        save% = save% + 1
ELSE
        x% = ASC(LEFT$(k$, 1))
        y% = ASC(MID$(k$, 2, 1))
        c% = ASC(MID$(k$, 3, 1))
        a% = ASC(RIGHT$(k$, 1))
        DEF SEG = &HB800
        POKE (x% + y% * 80) * 2, c%
        POKE (x% + y% * 80) * 2 + 1, a%
        DEF SEG
        pos1& = pos1& + 4
END IF
LOOP UNTIL pos1& > length&
CLOSE #fr%
END SUB

FUNCTION fl.full$ (fsrc$, ext$)

fl.full$ = fl.name$(fsrc$) + ext$
END FUNCTION

FUNCTION fl.name$ (fsrc$)

dot% = lastIndexOf%(fsrc$, ".")
IF dot% = -1 THEN fl.name$ = fsrc$ ELSE fl.name$ = LEFT$(fsrc$, LEN(fsrc$) - dot%)
END FUNCTION

FUNCTION lastIndexOf% (s$, f$)

sLen% = LEN(s$)
fLen% = LEN(f$)
FOR i% = 1 TO sLen% - fLen% + 1
t$ = MID$(s$, i%, fLen%)
IF t$ = f$ THEN EXIT FOR
NEXT

IF t$ = f$ THEN lastIndexOf% = i% ELSE lastIndexOf% = -1
END FUNCTION

SUB makecopy
DEF SEG = &HB800
mem1& = 0
DO
POKE 5002 + mem1&, PEEK(mem1&)
mem1& = mem1& + 1
LOOP UNTIL mem1& > 4000
DEF SEG
END SUB

SUB ms.end2
SHARED Jerry AS Mouse
mem1& = (Jerry.xpos + Jerry.ypos * 80) * 2
DEF SEG = &HB800
POKE mem1&, PEEK(5000)
POKE mem1& + 1, PEEK(5001)
DEF SEG
END SUB

SUB ms.hide
SHARED Err$, MouseMod$

DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 22
CALL absolute(func&)
DEF SEG

END SUB

SUB ms.init
SHARED Err$, MouseMod$

'call init routine
IF LEN(MouseMod$) = 0 THEN ms.load
DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$)
CALL absolute(func&)

'check result
DEF SEG = &H100
IF (PEEK(0) = 255 AND PEEK(1) = 255) THEN a1% = 1
DEF SEG
IF a1% = 0 THEN GOTO mouse.init.err
EXIT SUB

mouse.init.err:
Err$ = "Mouse not installed"
ERROR 2
END SUB

SUB ms.load
SHARED Err$, MouseMod$

'open mouse helper asm
f% = FREEFILE
OPEN "B", #f%, "mouse.dll"

'verify if exists
length& = LOF(f%)
IF length& = 0 THEN GOTO mouse.load.err

'load as module
MouseMod$ = INPUT$(LOF(f%), #f%)
CLOSE #f%
EXIT SUB

mouse.load.err:
Err$ = "Mouse module - mouse.dll - cannot be found"
ERROR 1
END SUB

SUB ms.put (x&, y&)
SHARED Err$, MouseMod$

x& = x& * 8
y& = y& * 8
DEF SEG = &H101
POKE 0, x& MOD 256
POKE 1, x& \ 256
POKE 2, y& MOD 256
POKE 3, y& \ 256
DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 66
CALL absolute(func&)
DEF SEG

END SUB

SUB ms.range (x1%, y1%, x2%, y2%)
SHARED Err$, MouseMod$

DEF SEG = &H101
POKE 1, 0   'x1% MOD 256
POKE 0, 0   ' x1% \ 256
POKE 3, 200 'x2% MOD 256
POKE 2, 0   'x2% \ 256
POKE 5, 0   'y1% MOD 256
POKE 4, 0   'y1% \ 256
POKE 7, 100 'y2% MOD 256
POKE 6, 0   'y2% \ 256

DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 28
CALL absolute(func&)
DEF SEG

END SUB

SUB ms.relativestatus
SHARED Jerry AS Mouse
SHARED Mouse$
DEF SEG = VARSEG(Mouse$)
mem1& = SADD(Mouse$) + 117
CALL absolute(mem1&)
DEF SEG = &H100
a1% = PEEK(0)
Jerry.left = a1% AND 1
Jerry.right = (a1% AND 2) \ 2
a1& = PEEK(2)
a2& = PEEK(3)
Jerry.xpos = a2& * 256 + a1&
IF (Jerry.xpos AND &H8000 = &H8000) THEN Jerry.xpos = -1 * (NOT (Jerry.xpos) + 1)
Jerry.xpos = Jerry.xpos \ 2
a1& = PEEK(4)
a2& = PEEK(5)
Jerry.ypos = a2& * 256 + a1&
IF (Jerry.ypos AND &H8000 = &H8000) THEN Jerry.ypos = -1 * (NOT (Jerry.ypos) + 1)
DEF SEG
END SUB

SUB ms.show
SHARED Err$, MouseMod$

DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 16
CALL absolute(func&)
DEF SEG

END SUB

SUB ms.show2
SHARED Jerry AS Mouse, k$
mouse.status
IF (Jerry.virtualattrib > 0) THEN
IF (Jerry.oldleft <> Jerry.left OR Jerry.oldright <> Jerry.right) THEN
at% = Jerry.virtualattrib
wr% = at% AND &HF
IF (Jerry.left = 1) THEN wr% = (wr% * 2) AND &HF
wr1% = at% AND &HF0
IF (Jerry.right = 1) THEN wr1% = (wr1% * 2) AND &HF0
wr% = wr1% + wr%
IF (Jerry.oldxpos = Jerry.xpos AND Jerry.oldypos = Jerry.ypos) THEN
DEF SEG = &HB800
mem1& = (Jerry.ypos * 80 + Jerry.xpos) * 2 + 1
POKE mem1&, wr%
DEF SEG
END IF
Jerry.oldleft = Jerry.left
Jerry.oldright = Jerry.right
Jerry.mouseattrib = wr%
END IF
END IF
IF (Jerry.oldxpos <> Jerry.xpos OR Jerry.oldypos <> Jerry.ypos OR Jerry.oldmouseattrib <> Jerry.mouseattrib OR Jerry.oldmousetype <> Jerry.MouseType) THEN
DEF SEG = &HB800
mem1& = (Jerry.oldypos * 80 + Jerry.oldxpos) * 2
POKE mem1&, PEEK(5000)
POKE mem1& + 1, PEEK(5001)
mem1& = (Jerry.ypos * 80 + Jerry.xpos) * 2
POKE 5000, PEEK(mem1&)
POKE 5001, PEEK(mem1& + 1)
POKE mem1&, Jerry.MouseType
POKE mem1& + 1, Jerry.mouseattrib
Jerry.oldypos = Jerry.ypos
Jerry.oldxpos = Jerry.xpos
Jerry.oldmouseattrib = Jerry.mouseattrib
Jerry.oldmousetype = Jerry.MouseType
DEF SEG
END IF
END SUB

SUB ms.show3
SHARED Jerry AS Mouse
mouse.status
IF (Jerry.oldxpos <> Jerry.xpos OR Jerry.oldypos <> Jerry.ypos) THEN
DEF SEG = &HB800
mem1& = (Jerry.oldypos * 80 + Jerry.oldxpos) * 2
POKE mem1&, Jerry.MouseType
POKE mem1& + 1, NOT (Jerry.mouseattrib)
mem1& = (Jerry.ypos * 80 + Jerry.xpos) * 2
Jerry.MouseType = PEEK(mem1&)
Jerry.mouseattrib = NOT (PEEK(mem1& + 1))
POKE mem1&, Jerry.MouseType
POKE mem1& + 1, Jerry.mouseattrib
Jerry.oldypos = Jerry.ypos
Jerry.oldxpos = Jerry.xpos
DEF SEG
END IF
END SUB

SUB ms.start2
SHARED Jerry AS Mouse
mem1& = (Jerry.xpos + Jerry.ypos * 80) * 2
DEF SEG = &HB800
POKE 5000, PEEK(mem1&)
POKE 5001, PEEK(mem1& + 1)
DEF SEG
END SUB

SUB ms.start3
SHARED Jerry AS Mouse
DEF SEG = &HB800
mem1& = (Jerry.oldypos * 80 + Jerry.oldxpos) * 2
Jerry.MouseType = PEEK(mem1&)
Jerry.mouseattrib = NOT (PEEK(mem1& + 1))
DEF SEG
END SUB

SUB ms.status
SHARED Err$, MouseMod$, Mouse AS DosMouse

DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 89
CALL absolute(func&)

DEF SEG = &H100
Mouse.lb = PEEK(0) AND 1
Mouse.rb = (PEEK(0) \ 2) AND 1
Mouse.x = (PEEK(3) * 256 + PEEK(2)) \ 8
Mouse.y = (PEEK(5) * 256 + PEEK(4)) \ 8
DEF SEG

END SUB

SUB ms.writeat2 (char%, attr%)
SHARED Jerry AS Mouse
DEF SEG = &HB800
POKE 5000, char%
POKE 5001, attr%
DEF SEG
END SUB

SUB ms.writeat3 (char%, attr%)
SHARED Jerry AS Mouse
Jerry.MouseType = char%
Jerry.mouseattrib = NOT (attr%)
END SUB

SUB savedat (fl1$)
fr% = FREEFILE
OPEN "B", #fr%, fl1$
pos1& = LOF(fr%) + 1
a1$ = CHR$(255)
PUT #fr%, pos1&, a1$
pos1& = pos1& + 1
DEF SEG = &HB800
mem1& = 0
DO
IF (PEEK(mem1&) <> PEEK(5002 + mem1&) OR PEEK(mem1& + 1) <> PEEK(5003 + mem1&)) THEN
yy% = mem1& \ 160
xx% = (mem1& - yy% * 160) \ 2
ch% = PEEK(mem1&)
at% = PEEK(mem1& + 1)
a1$ = CHR$(xx%)
PUT #fr%, pos1&, a1$
pos1& = pos1& + 1
a1$ = CHR$(yy%)
PUT #fr%, pos1&, a1$
pos1& = pos1& + 1
a1$ = CHR$(ch%)
PUT #fr%, pos1&, a1$
pos1& = pos1& + 1
a1$ = CHR$(at%)
PUT #fr%, pos1&, a1$
pos1& = pos1& + 1
END IF
mem1& = mem1& + 2
LOOP UNTIL mem1& > 4000
DEF SEG
CLOSE #fr%
END SUB

SUB tex.load (fsrc$)

'get data from file
length& = 4000
f% = FREEFILE
OPEN "b", #f%, fsrc$
chars$ = INPUT$(length&, #1)
CLOSE #f%

'write data to screen
DEF SEG = &HB800
FOR i& = 0 TO length& - 1
POKE i&, ASC(MID$(chars$, i&, 1))
NEXT
DEF SEG

END SUB

SUB tex.save (fdst$)

'load data from screen
length& = 4000
DEF SEG = &HB800
FOR i& = 0 TO length& - 1
chars$ = chars$ + CHR$(PEEK(i&))
NEXT
DEF SEG

'save data to file
f% = FREEFILE
OPEN "B", #f%, fdst$
PUT #1, , chars$
CLOSE #f%

END SUB


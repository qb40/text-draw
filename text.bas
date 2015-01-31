'dos mouse type
TYPE DosMouse
x AS LONG
y AS LONG
lb AS INTEGER
rb AS INTEGER
END TYPE

'screen attr type
TYPE ScreenAttr
fg AS INTEGER
bg AS INTEGER
bk AS INTEGER
END TYPE


'function declarations
DECLARE SUB ms.statabs ()
DECLARE SUB ms.absinfo ()
DECLARE SUB scr.get (x%, y%, char%, attr%)
DECLARE SUB ms.getat (char%, attr%)
DECLARE SUB scr.put (x%, y%, char%, attr%)
DECLARE FUNCTION scr.addr& (x%, y%)
DECLARE SUB ms.move (dx%, dy%)
DECLARE SUB ms.pos (x&, y&)
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
DECLARE SUB ms.getabs ()
DECLARE SUB ms.getrel ()
DECLARE SUB ms.start2 ()
DECLARE FUNCTION dat.datum% (fl1$, pos1&)
DECLARE SUB dat.loaddata (fl1$, pos1&, pos2&, segment&)
DECLARE SUB dat.loadpic (fl1$)
DECLARE SUB makecopy ()
DECLARE SUB savedat (fl1$)
DECLARE SUB tex.load (fsrc$)
DECLARE FUNCTION fl.name$ (fsrc$)
DECLARE FUNCTION fl.full$ (fsrc$)
DECLARE FUNCTION lastIndexOf% (s$, f$)
DECLARE SUB ms.get (char%, attr AS ScreenAttr)
DECLARE SUB tex.save (fdst$)


'handle errors
ON ERROR GOTO err.handler
DIM SHARED Err$, MouseMod$, Mouse AS DosMouse
DIM char%, attr AS ScreenAttr


'input keys
CONST kbkspc = 8, kenter = 13, ktab = 9, kesc = 27
CONST kleft = 75, kright = 77, kup = 72, kdown = 80
CONST kuplt = 71, kuprt = 73, kdnlt = 79, kdnrt = 81
CONST kins = 82, khome = 73, kpgup = 71, kdel = 83, kend = 81, kpgdn = 79
CONST kf1 = 59, kf2 = 60, kf3 = 61, kf4 = 62, kf5 = 63, kf6 = 64, kf7 = 65, kf8 = 66, kf9 = 67, kf10 = 68, kf11 = 133, kf12 = 134


'start
CLS
COLOR 15
PRINT "Text Draw"
COLOR 7
PRINT "---------"
PRINT

'file details
COLOR 14
INPUT "File name"; fsrc$
fsrc$ = fl.full(fsrc$, ".tex")

'load screen
CLS
ms.init
ms.hide
ms.pos 0, 0
tex.load fsrc$
ms.show

'prepare
ms.start2
Mouse.Mode = 1
Mouse.attr = 1
Mouse.virtattr = 0


'main loop
DO

'get key, handle mouse
k$ = ""
WHILE k$ = ""
k$ = INKEY$
IF Mouse.lb = 1 THEN ms.put char%, attr
IF Mouse.rb = 1 THEN ms.get char%, attr
IF Mouse.lb = 1 AND Mouse.rb = 1 THEN
char% = 0
attr.fg = 0
attr.bg = 0
attr.bk = 0
END IF
WEND
       
SELECT CASE k$

CASE CHR$(27)
SYSTEM

CASE CHR$(0) + CHR$(kup)
ms.move 0, -1

CASE CHR$(0) + CHR$(kdown)
ms.move 0, 1

char CHR$(0) + CHR$(kleft)
ms.move -1, 0

CASE CHR$(0) + CHR$(kright)
ms.move 1, 0

CASE CHR$(0) + CHR$(kins)
attr.fg = (attr.fg + 1) MOD 16

CASE CHR$(0) + CHR$(kdel)
attr.fg = (attr.fg + 15) MOD 16

CASE CHR$(0) + CHR$(khome)
attr.bg = (attr.bg + 1) MOD 8

CASE CHR$(0) + CHR$(kend)
attr.bg = (attr.bg + 7) MOD 8

CASE CHR$(0) + CHR$(kpgup)
attr.bk = (attr.bk + 1) MOD 2

CASE CHR$(0) + CHR$(kpgdn)
attr.bk = (attr.bk + 1) MOD 2

CASE CHR$(0) + CHR$(kf1)


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

'save
tex.save fsrc$

err.handler:
RESUME NEXT

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

SUB ms.draw (char%, attr%)
SHARED Err$, MouseMod$, Mouse AS DosMouse, k$

ms.get char0%, attr0%
scr.put Mouse.x, Mouse.y, char0%, attr0%
ms.statabs
scr.get Mouse.x, Mouse.y, char0%, attr0%
ms.put char0%, attr0%
scr.put Mouse.x, Mouse.y, char%, attr%

END SUB

SUB ms.get (char%, attr%)
SHARED Err$, MouseMod$, Mouse AS DosMouse

DEF SEG = &HB800
char% = PEEK(5000)
attr% = PEEK(5001)
DEF SEG

END SUB

SUB ms.hide
SHARED Err$, MouseMod$, Mouse AS DosMouse

DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 22
CALL absolute(func&)
DEF SEG

END SUB

SUB ms.init
SHARED Err$, MouseMod$, Mouse AS DosMouse

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
SHARED Err$, MouseMod$, Mouse AS DosMouse

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

SUB ms.move (dx%, dy%)
SHARED Err$, MouseMod$, Mouse AS DosMouse

Mouse.x = Mouse.x + dx%
Mouse.y = Mouse.y + dy% + (Mouse.x \ 80)
Mouse.x = (Mouse.x + 80) MOD 80
Mouse.y = (Mouse.y + 25) MOD 25
ms.pos Mouse.x, Mouse.y

END SUB

SUB ms.pos (x&, y&)
SHARED Err$, MouseMod$, Mouse AS DosMouse

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

SUB ms.put (char%, attr%)
SHARED Err$, MouseMod$, Mouse AS DosMouse

DEF SEG = &HB800
POKE 5000, char%
POKE 5001, attr%
DEF SEG

END SUB

SUB ms.range (x1%, y1%, x2%, y2%)
SHARED Err$, MouseMod$, Mouse AS DosMouse

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

SUB ms.show
SHARED Err$, MouseMod$, Mouse AS DosMouse

DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 16
CALL absolute(func&)
DEF SEG

END SUB

SUB ms.statabs
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

SUB ms.statrel
SHARED Err$, MouseMod$, Mouse AS DosMouse

DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 117
CALL absolute(func&)

DEF SEG = &H100
a1% = PEEK(0)
Mouse.lb = PEEK(0) AND 1
Mouse.rb = (PEEK(0) \ 2) AND 1
Mouse.x = CVI(CHR$(PEEK(2)) + CHR$(PEEK(3))) \ 2
Mouse.y = CVI(CHR$(PEEK(4)) + CHR$(PEEK(5)))
DEF SEG

END SUB

FUNCTION scr.addr& (x%, y%)

scr.addr& = ((y% * 80 + x%) * 2)
END FUNCTION

SUB scr.get (x%, y%, char%, attr%)

ptr& = scr.addr&(x%, y%)
DEF SEG = &HB800
char% = PEEK(ptr&)
attr% = PEEK(ptr& + 1)
DEF SEG

END SUB

SUB scr.put (x%, y%, char%, attr%)

ptr& = scr.addr(x%, y%)
DEF SEG = &HB800
POKE ptr&, char%
POKE ptr& + 1, attr%
DEF SEG

END SUB

SUB tex.help

COLOR 15
PRINT "Help"
COLOR 7
PRINT "----"
PRINT
COLOR 14
PRINT "F1 - Help"
PRINT "F5 - Save"
PRINT "MOUSE, ARROW KEYS = Move Cursor"
PRINT "MOUSE-LEFT = Draw"
PRINT "MOUSE-RIGHT = Copy"
PRINT "MOUSE-(LEFT+RIGHT) = Erase Mode"
PRINT "INSERT, DELETE = Change Character Color"
PRINT "HOME, END = Change Background Color"
PRINT "PAGE-UP, PAGE-DOWN = Change Character Blink"

END SUB

SUB tex.load (fsrc$)

'get data from file
length& = 4000
f% = FREEFILE
OPEN "b", #f%, fsrc$
IF LOF(f%) < length& THEN GOTO text.load.end
chars$ = INPUT$(length&, #1)
CLOSE #f%

'write data to screen
DEF SEG = &HB800
FOR i& = 0 TO length& - 1
POKE i&, ASC(MID$(chars$, i&, 1))
NEXT
DEF SEG

tex.load.end:
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


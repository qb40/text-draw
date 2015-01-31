'dos mouse type
TYPE DosMouse
x AS INTEGER
y AS INTEGER
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
DECLARE SUB att.set (attr AS ScreenAttr, attv%)
DECLARE FUNCTION att.get% (attr AS ScreenAttr)
DECLARE FUNCTION fl.name$ (fsrc$)
DECLARE FUNCTION fl.full$ (fsrc$, ext$)
DECLARE FUNCTION lastIndexOf% (s$, f$)
DECLARE SUB ms.init ()
DECLARE SUB ms.load ()
DECLARE SUB ms.show ()
DECLARE SUB ms.hide ()
DECLARE SUB ms.statabs ()
DECLARE SUB ms.statrel ()
DECLARE SUB ms.pos (x%, y%)
DECLARE SUB ms.move (dx%, dy%)
DECLARE SUB ms.get (char%, attr%)
DECLARE SUB ms.put (char%, attr%)
DECLARE SUB ms.draw (char%, attr%)
DECLARE SUB ms.range (x1%, y1%, x2%, y2%)
DECLARE SUB scr.get (x%, y%, char%, attr%)
DECLARE SUB scr.put (x%, y%, char%, attr%)
DECLARE FUNCTION scr.addr& (x%, y%)
DECLARE SUB tex.load (fsrc$)
DECLARE SUB tex.save (fdst$)
DECLARE SUB tex.help ()


'handle errors
ON ERROR GOTO err.handler
DIM SHARED err$, MouseMod$, Mouse AS DosMouse
DIM attr AS ScreenAttr


'input keys
CONST kbkspc = 8, kenter = 13, ktab = 9, kesc = 27
CONST kleft = 75, kright = 77, kup = 72, kdown = 80
CONST kuplt = 71, kuprt = 73, kdnlt = 79, kdnrt = 81
CONST kins = 82, kpgup = 73, khome = 71, kdel = 83, kpgdn = 81, kend = 79
CONST kf1 = 59, kf2 = 60, kf3 = 61, kf4 = 62, kf5 = 63, kf6 = 64, kf7 = 65, kf8 = 66, kf9 = 67, kf10 = 68, kf11 = 133, kf12 = 134


'start
CLS
COLOR 15
PRINT "Text Draw"
COLOR 7
PRINT "---------"
PRINT

'init mouse
ms.init
ms.hide

'file details
COLOR 14
INPUT "File name"; fsrc$
fsrc$ = fl.full(fsrc$, ".tex")

'help
CLS
tex.help
k$ = INPUT$(1)

'load screen
CLS
tex.load fsrc$
scr.get 0, 0, byte%, attv%
ms.put byte%, attv%
byte% = 32
attr.bg = 7
attv% = att.get%(attr)

'main loop
DO

'handle mouse
k$ = ""
WHILE k$ = ""
k$ = INKEY$
ms.draw byte%, attv%
IF Mouse.lb = 1 THEN ms.put byte%, attv%
IF Mouse.rb = 1 THEN ms.get byte%, attv%
IF Mouse.lb = 1 AND Mouse.rb = 1 THEN
byte% = 32
attv% = 7
END IF
WEND
      
att.set attr, attv%
SELECT CASE k$

CASE CHR$(27)
EXIT DO

CASE CHR$(0) + CHR$(kup)
ms.move 0, -1

CASE CHR$(0) + CHR$(kdown)
ms.move 0, 1

CASE CHR$(0) + CHR$(kleft)
ms.move -1, 0

CASE CHR$(0) + CHR$(kright)
ms.move 1, 0

CASE CHR$(0) + CHR$(kins)
byte% = (byte% + 1) MOD 256

CASE CHR$(0) + CHR$(kdel)
byte% = (byte% + 255) MOD 256

CASE CHR$(0) + CHR$(khome)
attr.fg = (attr.fg + 1) MOD 16

CASE CHR$(0) + CHR$(kend)
attr.fg = (attr.fg + 15) MOD 16

CASE CHR$(0) + CHR$(kpgup)
attr.bg = (attr.bg + 1) MOD 8

CASE CHR$(0) + CHR$(kpgdn)
attr.bg = (attr.bg + 7) MOD 8

CASE CHR$(0) + CHR$(kf3)
attr.bk = (attr.bk + 1) MOD 2

CASE CHR$(0) + CHR$(kf4)
attr.bk = (attr.bk + 1) MOD 2

CASE CHR$(0) + CHR$(kf1)
tex.save "text.tmp"
CLS
tex.help
k$ = INPUT$(1)
CLS
tex.load "text.tmp"
KILL "text.tmp"

CASE CHR$(0) + CHR$(kf5)
tex.save fsrc$

CASE ELSE
byte% = ASC(k$)
ms.put byte%, attv%
ms.move 1, 0

END SELECT
attv% = att.get(attr)
ms.draw byte%, attv%

LOOP

err.handler:
CLS
COLOR 12
PRINT err$
PRINT
COLOR 7
SYSTEM

FUNCTION att.get% (attr AS ScreenAttr)

att.get% = attr.bk * 128 + attr.bg * 16 + attr.fg
END FUNCTION

SUB att.set (attr AS ScreenAttr, attv%)

attr.bk = (attv% \ 128) AND 1
attr.bg = (attv% \ 16) AND 7
attr.fg = attv% AND 15

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

SUB ms.draw (byte%, attr%)
SHARED err$, MouseMod$, Mouse AS DosMouse, k$

oldx% = Mouse.x
oldY% = Mouse.y
ms.statabs
IF oldx% = Mouse.x AND oldY% = Mouse.y THEN GOTO ms.draw.draw
ms.get char0%, attr0%
scr.put oldx%, oldY%, char0%, attr0%
scr.get Mouse.x, Mouse.y, char0%, attr0%
ms.put char0%, attr0%

ms.draw.draw:
scr.put Mouse.x, Mouse.y, byte%, attr% OR 16

ms.draw.end:
END SUB

SUB ms.get (byte%, attr%)
SHARED err$, MouseMod$, Mouse AS DosMouse

DEF SEG = &HB800
byte% = PEEK(5000)
attr% = PEEK(5001)
DEF SEG

END SUB

SUB ms.hide
SHARED err$, MouseMod$, Mouse AS DosMouse

DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 22
CALL absolute(func&)
DEF SEG

END SUB

SUB ms.init
SHARED err$, MouseMod$, Mouse AS DosMouse

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
err$ = "Mouse not installed"
ERROR 2
END SUB

SUB ms.load
SHARED err$, MouseMod$, Mouse AS DosMouse

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
err$ = "Mouse module - mouse.dll - cannot be found"
ERROR 1
END SUB

SUB ms.move (dx%, dy%)
SHARED err$, MouseMod$, Mouse AS DosMouse

ms.get char0%, attr0%
scr.put Mouse.x, Mouse.y, char0%, attr0%

Mouse.x = Mouse.x + dx%
Mouse.y = Mouse.y + dy% + (Mouse.x \ 80)
Mouse.x = (Mouse.x + 80) MOD 80
Mouse.y = (Mouse.y + 25) MOD 25

scr.get Mouse.x, Mouse.y, char0%, attr0%
ms.put char0%, attr0%
ms.pos Mouse.x, Mouse.y

END SUB

SUB ms.pos (x%, y%)
SHARED err$, MouseMod$, Mouse AS DosMouse

x& = x% * 8
y& = y% * 8
 
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

SUB ms.put (byte%, attr%)
SHARED err$, MouseMod$, Mouse AS DosMouse

DEF SEG = &HB800
POKE 5000, byte%
POKE 5001, attr%
DEF SEG

END SUB

SUB ms.range (x1%, y1%, x2%, y2%)
SHARED err$, MouseMod$, Mouse AS DosMouse

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
SHARED err$, MouseMod$, Mouse AS DosMouse

DEF SEG = VARSEG(MouseMod$)
func& = SADD(MouseMod$) + 16
CALL absolute(func&)
DEF SEG

END SUB

SUB ms.statabs
SHARED err$, MouseMod$, Mouse AS DosMouse

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
SHARED err$, MouseMod$, Mouse AS DosMouse

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

SUB scr.get (x%, y%, byte%, attr%)

ptr& = scr.addr&(x%, y%)
DEF SEG = &HB800
byte% = PEEK(ptr&)
attr% = PEEK(ptr& + 1)
DEF SEG

END SUB

SUB scr.put (x%, y%, byte%, attr%)

ptr& = scr.addr(x%, y%)
DEF SEG = &HB800
POKE ptr&, byte%
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
PRINT "INSERT, DELETE = Change Character"
PRINT "HOME, END = Change Character Color"
PRINT "PAGE-UP, PAGE-DOWN = Change Background Color"
PRINT "PAGE-UP, PAGE-DOWN = Change Character Blink"

END SUB

SUB tex.load (fsrc$)

'get data from file
length& = 4000
f% = FREEFILE
OPEN "B", #f%, fsrc$
IF LOF(f%) < length& THEN GOTO tex.load.end
chars$ = INPUT$(length&, #1)
CLOSE #f%

'write data to screen
DEF SEG = &HB800
FOR i& = 1 TO length&
POKE i& - 1, ASC(MID$(chars$, i&, 1))
NEXT
DEF SEG

tex.load.end:
END SUB

SUB tex.save (fdst$)

'hide mouse
ms.get byte%, attr%
scr.put Mouse.x, Mouse.y, byte%, attr%

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
PUT #1, 1, chars$
CLOSE #f%

END SUB


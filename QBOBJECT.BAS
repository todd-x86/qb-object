' QB Object
' .:: Rebuild ::.
' Version 1.2
' (C) Data Components Software Development
'
' NOTE: THIS IS A LIBRARY DESIGNED FOR USE BY OTHER PROGRAMS,
'       SO ONLY 1 WINDOW ALLOWED PER PROGRAM...
'
' NOTE: ALL OF THE CODE USED IN THIS LIBRARY WAS WRITTEN FROM SCRATCH
'       AND COUNTLESS HOURS OF RESEARCH AND STUDY FROM THE QB Object Library
'
' This library is not 100% complete to the QB Object Library by AMP Software,
' but there are hopefully going to be continuing improvements.
'$DYNAMIC
'DEFINT A-Z
'$INCLUDE: 'QB.BI'

DECLARE FUNCTION ListBox.ItemFromArray$ (lstboxid%, arrayid%)
DECLARE SUB Capture (Prefix$)
DECLARE FUNCTION ReturnActiveWin% ()
DECLARE SUB Display (Prefix$)
DECLARE SUB Label (x%, y%, text$, c%, id%)
DECLARE FUNCTION TextBox.Cont$ (id%)
DECLARE FUNCTION ListBox.Item$ (id%)
DECLARE SUB Bitmap (x%, y%, file$, id%)
DECLARE FUNCTION ValueBox.Cont% (id%)
DECLARE FUNCTION ListBox.Cont% (id%)
DECLARE SUB ListBox (x%, y%, x2%, lines%, max%, array$(), id%)
DECLARE SUB drwlistbx (x%, y%, x2%, lines%, lstboxid%, offset%)
DECLARE SUB Icon (x%, y%, filename$, disablecol%, id%)
DECLARE SUB loadIcon (x2%, y2%, filename$, disablecolor%)
DECLARE SUB loadbmp (file$, PosX%, PosY%)
DECLARE SUB drwbox1 (x%, y%, x2%, y2%)
DECLARE FUNCTION Option.Cont% (id%)
DECLARE SUB OptionCirc (x%, y%, checked%, group%, id%)
DECLARE SUB drwradio (x%, y%, checked%)
DECLARE SUB drwscrlbtn (x%, y%, updown%, down%)
DECLARE SUB drwscrlbar (x%, y%, y2%)
DECLARE SUB CheckBox (x%, y%, checked%, id%)
DECLARE FUNCTION Check.Cont% (id%)
DECLARE SUB drwcheckbox (x%, y%, checked%)
DECLARE SUB ValueBox (x%, y%, min%, max%, id%)
DECLARE SUB drwvalbox (x%, y%, max%, value%)
DECLARE SUB drwvalbtn (x%, y%, updown%, down%)
DECLARE SUB drwarrow (x%, y%, updown%)
DECLARE SUB TBox (id%, winid%)
DECLARE SUB TextBox (caption$, x%, y%, widinchar%, id%)
DECLARE SUB RemAllSel ()
DECLARE SUB drwtbox (txt$, x%, y%, widinchar%, sel%)
DECLARE FUNCTION Button.Cont% (id%)
DECLARE SUB button (caption$, x%, y%, id%)
DECLARE SUB drwbtn (x%, y%, txt$, pressed%, selected%)
DECLARE SUB drwsel (x%, y%, x2%, y2%, col%, steps%)
DECLARE SUB RedrawControls ()
DECLARE FUNCTION MouseLimit% (MiniX%, MiniY%, MaxiX%, MaxiY%)
DECLARE SUB GetControl ()
DECLARE SUB drwwinbtn (x%, y%, pressed%)
DECLARE SUB gprint (z$, x%, y%, c%)
DECLARE SUB MouseStatus (lb%, rb%, xMouse%, yMouse%)
DECLARE SUB drwwin (x%, y%, x2%, y2%, title$)
DECLARE SUB win (x%, y%, x2%, y2%, title$, id%)
DECLARE SUB mousedriver (ax%, bx%, cx%, dx%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseShow ()
DECLARE FUNCTION mouseinit% ()

TYPE TWin
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  y2 AS INTEGER
  active AS INTEGER
END TYPE
TYPE TBtn
  x AS INTEGER
  y AS INTEGER
  sel AS INTEGER
  clicked AS INTEGER
  win AS INTEGER
END TYPE
TYPE TTBox
  x AS INTEGER
  y AS INTEGER
  widinchar AS INTEGER
  sel AS INTEGER
  win AS INTEGER
END TYPE
TYPE TValBox
  x AS INTEGER
  y AS INTEGER
  min AS INTEGER
  max AS INTEGER
  win AS INTEGER
END TYPE
TYPE TCheckBox
  x AS INTEGER
  y AS INTEGER
  checked AS INTEGER
  win AS INTEGER
END TYPE
TYPE TRadioButton
  x AS INTEGER
  y AS INTEGER
  checked AS INTEGER
  group AS INTEGER
  win AS INTEGER
END TYPE
TYPE TLabel
  x AS INTEGER
  y AS INTEGER
  colr AS INTEGER
  win AS INTEGER
END TYPE
TYPE TBitmap
  x AS INTEGER
  y AS INTEGER
  win AS INTEGER
END TYPE
TYPE TIcon
  x AS INTEGER
  y AS INTEGER
  disablecol AS INTEGER
  win AS INTEGER
END TYPE
TYPE TListBox
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  lines AS INTEGER
  max AS INTEGER
  itemsel AS INTEGER
  first AS INTEGER
  win AS INTEGER
END TYPE
TYPE TLine
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  y2 AS INTEGER
  colr AS INTEGER
  win AS INTEGER
END TYPE
TYPE TBox
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  y2 AS INTEGER
  colr AS INTEGER
  filled AS INTEGER
  win AS INTEGER
END TYPE
TYPE TCircle
   x AS INTEGER
   y AS INTEGER
   rad AS INTEGER
   colr AS INTEGER
   startd AS INTEGER
   endd AS INTEGER
   win AS INTEGER
END TYPE
COMMON SHARED QLArray() AS STRING
COMMON SHARED QLBox() AS TListBox
COMMON SHARED CWin() AS STRING
COMMON SHARED CBtn() AS STRING
COMMON SHARED CTBox() AS STRING
COMMON SHARED QWin() AS TWin
COMMON SHARED QBtn() AS TBtn
COMMON SHARED QTBox() AS TTBox
COMMON SHARED QValBox() AS TValBox
COMMON SHARED QValues() AS INTEGER
COMMON SHARED QLabels() AS STRING
COMMON SHARED QLabel() AS TLabel
COMMON SHARED QCBox() AS TCheckBox
COMMON SHARED QRBtn() AS TRadioButton
COMMON SHARED QBmp() AS TBitmap
COMMON SHARED QBmps() AS STRING
COMMON SHARED QIcon() AS TIcon
COMMON SHARED QIcons() AS STRING
COMMON SHARED QLine() AS TLine
COMMON SHARED QBox() AS TBox
COMMON SHARED QCircle() AS TCircle
COMMON SHARED lists() AS STRING
COMMON SHARED Inregs AS RegType, Outregs AS RegType        'Interrupt
COMMON SHARED Regs AS RegTypeX                             'InterruptX
COMMON SHARED MOUSE$
COMMON SHARED activecont%, lstcont%

REM $STATIC
SUB Bitmap (x%, y%, file$, id%)
QBmps(id%) = file$
QBmp(id%).x = x%
QBmp(id%).y = y%
QBmp(id%).win = ReturnActiveWin%
RedrawControls
END SUB

SUB button (caption$, x%, y%, id%)
QBtn(id%).x = x%
QBtn(id%).y = y%
QBtn(id%).sel = 0
QBtn(id%).clicked = 0
QBtn(id%).win = ReturnActiveWin%
CBtn(id%) = caption$
RedrawControls
END SUB

FUNCTION Button.Cont% (id%)
Button.Cont% = 0
IF QBtn(id%).clicked = 1 THEN Button.Cont% = 1
QBtn(id%).clicked = 0
END FUNCTION

SUB Capture (Prefix$)
DEF SEG = &HA000
OUT &H3CE, 4 'tells the vga control what function we are going
'to use (in this case 4, change plane to read from)
FOR Plane = 0 TO 3
OUT &H3CF, Plane 'switches planes
BSAVE Prefix$ + LTRIM$(STR$(Plane)) + ".BSV", 0, 38400
NEXT Plane

END SUB

FUNCTION Check.Cont% (id%)
Check.Cont% = QCBox(id%).checked
END FUNCTION

SUB CheckBox (x%, y%, checked%, id%)
QCBox(id%).x = x%
QCBox(id%).y = y%
QCBox(id%).checked = checked%
QCBox(id%).win = ReturnActiveWin%
RedrawControls
END SUB

SUB CloseWindow (id%)
IF id% = 1 THEN
SYSTEM
ELSE
FOR sb = 1 TO 50
  QBtn(sb).sel = 0
NEXT sb
aw = id%
aw = aw - 1
FOR iw = 1 TO 50
  QWin(iw).active = 0
NEXT iw
QWin(aw).active = 1
lstcont% = 0
RedrawControls
END IF

END SUB

SUB Display (Prefix$)
DEF SEG = &HA000

OUT &H3C4, 2 'tells the vga control what function we are going
'to use (in this case 2, select what plane(s) to write to)
FOR Plane = 0 TO 3
OUT &H3C5, 2 ^ Plane 'switches planes
BLOAD Prefix$ + LTRIM$(STR$(Plane)) + ".BSV", 0
NEXT Plane
END SUB

SUB drwarrow (x%, y%, updown%)
IF updown% = 1 THEN
  PSET (x% + 3, y%), 0
  PSET (x% + 4, y%), 0

  PSET (x% + 2, y% + 1), 0
  PSET (x% + 3, y% + 1), 0
  PSET (x% + 4, y% + 1), 0
  PSET (x% + 5, y% + 1), 0
 

  PSET (x% + 1, y% + 2), 0
  PSET (x% + 2, y% + 2), 0
  PSET (x% + 3, y% + 2), 0
  PSET (x% + 4, y% + 2), 0
  PSET (x% + 5, y% + 2), 0
  PSET (x% + 6, y% + 2), 0
 

  PSET (x%, y% + 3), 0
  PSET (x% + 1, y% + 3), 0
  PSET (x% + 2, y% + 3), 0
  PSET (x% + 3, y% + 3), 0
  PSET (x% + 4, y% + 3), 0
  PSET (x% + 5, y% + 3), 0
  PSET (x% + 6, y% + 3), 0
  PSET (x% + 7, y% + 3), 0

END IF
IF updown% = 2 THEN
  PSET (x% + 3, y% + 3), 0
  PSET (x% + 4, y% + 3), 0

  PSET (x% + 2, y% + 2), 0
  PSET (x% + 3, y% + 2), 0
  PSET (x% + 4, y% + 2), 0
  PSET (x% + 5, y% + 2), 0


  PSET (x% + 1, y% + 1), 0
  PSET (x% + 2, y% + 1), 0
  PSET (x% + 3, y% + 1), 0
  PSET (x% + 4, y% + 1), 0
  PSET (x% + 5, y% + 1), 0
  PSET (x% + 6, y% + 1), 0


  PSET (x%, y%), 0
  PSET (x% + 1, y%), 0
  PSET (x% + 2, y%), 0
  PSET (x% + 3, y%), 0
  PSET (x% + 4, y%), 0
  PSET (x% + 5, y%), 0
  PSET (x% + 6, y%), 0
  PSET (x% + 7, y%), 0
                  
END IF
END SUB

SUB drwbox1 (x%, y%, x2%, y2%)
tx% = x%
ty% = y% - 2
tx2% = x2%
LINE (tx%, ty%)-(tx2%, y2%), 15, BF
LINE (tx%, ty%)-(tx2%, y2%), 8, B
LINE (tx%, y2% - 1)-(tx2%, y2% - 1), 15, B
LINE (tx2%, ty%)-(tx2%, y2% - 1), 15, B
LINE (tx% + 1, y2% - 2)-(tx2% - 1, y2% - 2), 7, B
LINE (tx2% - 1, ty% + 1)-(tx2% - 1, y2% - 2), 7, B
LINE (tx% + 1, ty% + 1)-(tx2% - 2, ty% + 1), 0, B
LINE (tx% + 1, ty% + 1)-(tx% + 1, y2% - 3), 0, B

END SUB

SUB drwbtn (x%, y%, txt$, pressed%, selected%)
x2% = x% + 23 + (LEN(txt$) * 8) + 23
LINE (x%, y%)-(x2%, y% + 30), 7, BF

IF pressed% = 0 THEN
LINE (x%, y%)-(x2%, y% + 30), 15, B
LINE (x%, y% + 30)-(x2%, y% + 30), 0, B
LINE (x2%, y%)-(x2%, y% + 30), 0, B
LINE (x% + 1, y% + 29)-(x2% - 1, y% + 29), 8, B
LINE (x2% - 1, y% + 1)-(x2% - 1, y% + 29), 8, B
END IF

IF selected% = 1 THEN
IF pressed% = 1 THEN
  x% = x% + 1: y% = y% + 1

END IF
  drwsel x% + 4, y% + 4, x2% - 4, y% + 26, 8, 2
  drwsel x% + 5, y% + 5, x2% - 5, y% + 25, 8, 2
  drwsel x% + 6, y% + 6, x2% - 6, y% + 24, 8, 2

IF pressed% = 1 THEN
  x% = x% - 1: y% = y% - 1
END IF

END IF
IF pressed% = 1 THEN
LINE (x%, y%)-(x2%, y% + 30), 0, B
LINE (x% + 1, y% + 1)-(x2% - 1, y% + 1), 8, B
LINE (x% + 1, y% + 1)-(x% + 1, y% + 29), 8, B
x% = x% + 1
y% = y% + 1
END IF

gprint txt$, x% + 23, y% + 8, 0

END SUB

SUB drwcheckbox (x%, y%, checked%)
LINE (x%, y%)-(x% + 13, y% + 13), 15, BF
LINE (x%, y%)-(x% + 12, y%), 8, B
LINE (x%, y%)-(x%, y% + 12), 8, B
LINE (x% + 1, y% + 1)-(x% + 11, y% + 1), 0, B
LINE (x% + 1, y% + 1)-(x% + 1, y% + 11), 0, B
LINE (x% + 1, y% + 12)-(x% + 12, y% + 12), 7, B
LINE (x% + 12, y% + 1)-(x% + 12, y% + 12), 7, B
IF checked% = 1 THEN
  PSET (x% + 2, y% + 6), 0
  PSET (x% + 3, y% + 6), 0
  PSET (x% + 4, y% + 6), 0
  PSET (x% + 3, y% + 7), 0
  PSET (x% + 4, y% + 7), 0
  PSET (x% + 5, y% + 7), 0
  PSET (x% + 6, y% + 7), 0
  PSET (x% + 7, y% + 7), 0
  PSET (x% + 8, y% + 7), 0
  PSET (x% + 4, y% + 8), 0
  PSET (x% + 5, y% + 8), 0
  PSET (x% + 6, y% + 8), 0
  PSET (x% + 7, y% + 8), 0
  PSET (x% + 5, y% + 9), 0
  PSET (x% + 6, y% + 9), 0
  PSET (x% + 7, y% + 9), 0
  PSET (x% + 5, y% + 10), 0
  PSET (x% + 6, y% + 10), 0
  PSET (x% + 7, y% + 6), 0
  PSET (x% + 8, y% + 6), 0
  PSET (x% + 8, y% + 5), 0
  PSET (x% + 9, y% + 5), 0
  PSET (x% + 8, y% + 4), 0
  PSET (x% + 9, y% + 4), 0
  PSET (x% + 9, y% + 3), 0
  PSET (x% + 10, y% + 3), 0
  PSET (x% + 9, y% + 2), 0
  PSET (x% + 10, y% + 2), 0

ELSEIF chexked% = 0 THEN
  LINE (x% + 2, y% + 2)-(x% + 10, y% + 10), 15, BF
END IF
END SUB

SUB drwlistbx (x%, y%, x2%, lines%, lstboxid%, offset%)
drwbox1 x%, y%, x2%, y% + 8 + (18 * lines%)
drwscrlbar x2% - 14, y%, y% + 6 + (18 * lines%)
ty% = y%
FOR i = (1 + offset%) TO (lines% + offset%)
gprint QLArray(lstboxid%, i), x% + 5, ty% + 4, 0
ty% = ty% + 18
NEXT i

END SUB

SUB drwradio (x%, y%, checked%)
x% = x% + 8
y% = y% + 8
CIRCLE (x%, y%), 8, 8
CIRCLE (x%, y%), 7, 0
DRAW "P" + "15" + ",0"
DRAW "BC4 M" + STR$(x% - 6) + "," + STR$(y% + 5)
DRAW "c15r1d1r2f1r4e1r2 e2u1r1u5h1u1h1 l1c7d1r1d2r1d4l1d2l1d1l2g1l4h1l1u1l1"
bc = POINT(x% - 4, y% + 8)
PSET (x% - 5, y% + 7), bc
PSET (x% - 4, y% + 7), bc
PSET (x% - 3, y% + 7), bc
PSET (x% - 2, y% + 8), bc
PSET (x% - 1, y% + 8), bc
PSET (x%, y% + 8), bc
PSET (x% + 1, y% + 8), bc
PSET (x% + 2, y% + 7), 15
PSET (x% + 3, y% + 7), 15
PSET (x% + 6, y% + 6), bc
PSET (x% + 8, y% + 3), bc
PSET (x% + 9, y% + 1), bc
PSET (x% + 9, y%), bc
IF checked% = 1 THEN col% = 0 ELSE col% = 15
CIRCLE (x%, y%), 4, col%
CIRCLE (x%, y%), 3, col%
CIRCLE (x%, y%), 2, col%
CIRCLE (x%, y%), 1, col%

END SUB

SUB drwscrlbar (x%, y%, y2%)
LINE (x%, y%)-(x% + 13, y2%), 8, BF
drwscrlbtn x%, y%, 1, 0
drwscrlbtn x%, y2% - 16, 2, 0
END SUB

SUB drwscrlbtn (x%, y%, updown%, down%)
LINE (x%, y%)-(x% + 14, y% + 16), 7, BF
IF down% = 1 THEN col% = 8
IF down% = 0 THEN col% = 15
LINE (x% + 1, y% + 1)-(x% + 12, y% + 1), col%, B
LINE (x% + 1, y% + 1)-(x% + 1, y% + 15), col%, B
LINE (x% + 1, y% + 15)-(x% + 12, y% + 15), 8, B
LINE (x% + 12, y% + 1)-(x% + 12, y% + 15), 8, B
LINE (x% + 1, y% + 16)-(x% + 13, y% + 16), 0, B
LINE (x% + 13, y% + 1)-(x% + 13, y% + 16), 0, B
drwarrow x% + 3, y% + 6, updown%
END SUB

SUB drwsel (x%, y%, x2%, y2%, col%, steps%)
FOR a2% = x% TO x2% STEP steps%
  PSET (a2%, y%), col%
  PSET (a2%, y2%), col%
NEXT
FOR a2% = y% TO y2% STEP steps%
  PSET (x%, a2%), col%
  PSET (x2%, a2%), col%
NEXT
END SUB

SUB drwtbox (txt$, x%, y%, widinchar%, sel%)
MouseHide
tx% = x%
ty% = y% - 2
tx2% = tx% + 4 + (widinchar% * 8) + 4
LINE (tx%, ty%)-(tx2%, ty% + 22), 15, BF
LINE (tx%, ty%)-(tx2%, ty% + 22), 8, B
LINE (tx%, ty% + 21)-(tx2%, ty% + 21), 15, B
LINE (tx2%, ty%)-(tx2%, ty% + 21), 15, B
LINE (tx% + 1, ty% + 20)-(tx2% - 1, ty% + 20), 7, B
LINE (tx2% - 1, ty% + 1)-(tx2% - 1, ty% + 20), 7, B
LINE (tx% + 1, ty% + 1)-(tx2% - 2, ty% + 1), 0, B
LINE (tx% + 1, ty% + 1)-(tx% + 1, ty% + 19), 0, B

IF sel% = 1 THEN
 drwsel tx% - 3, ty% - 3, tx2% + 3, ty% + 22 + 3, 0, 3
END IF
gprint txt$, INT(x%) + 5, INT(y%) + 2, 0
MouseShow
END SUB

SUB drwvalbox (x%, y%, max%, value%)
x2% = x% + (LEN(STR$(max%)) * 8) + 12 + 4
' NOTE: max% DEFINES HOW FAR TO MOVE
'       12 DEFINES THE VALUE BUTTONS AREA
'       4 DEFINES THE DETAIL OUTLINE OF THE VALUE BOX
LINE (x%, y%)-(x2%, y% + 22), 15, BF
LINE (x%, y%)-(x2%, y% + 22), 8, B
LINE (x%, y% + 21)-(x2%, y% + 21), 15, B
LINE (x2%, y%)-(x2%, y% + 21), 15, B
LINE (x% + 1, y% + 20)-(x2% - 1, y% + 20), 7, B
LINE (x2% - 1, y% + 1)-(x2% - 1, y% + 20), 7, B
LINE (x% + 1, y% + 1)-(x2% - 2, y% + 1), 0, B
LINE (x% + 1, y% + 1)-(x% + 1, y% + 19), 0, B
LINE (x2% - 4 - 12, y% + 2)-(x2% - 4, y% + 17), 7, BF
drwvalbtn x2% - 4 - 12, y% + 2, 1, 0
drwvalbtn x2% - 4 - 12, y% + 11, 2, 0
gprint STR$(value%), INT(x%), y% + 4, 0
END SUB

SUB drwvalbtn (x%, y%, updown%, down%)
'NOTE: Up = 1
'      Down = 2
LINE (x%, y%)-(x% + 14, y% + 8), 7, BF
IF down% = 1 THEN col% = 8
IF down% = 0 THEN col% = 15
LINE (x% + 1, y% + 1)-(x% + 12, y% + 1), col%, B
LINE (x% + 1, y% + 1)-(x% + 1, y% + 7), col%, B
LINE (x% + 1, y% + 7)-(x% + 12, y% + 7), 8, B
LINE (x% + 12, y% + 1)-(x% + 12, y% + 7), 8, B
LINE (x% + 1, y% + 8)-(x% + 13, y% + 8), 0, B
LINE (x% + 13, y% + 1)-(x% + 13, y% + 8), 0, B

drwarrow x% + 3, y% + 3, updown%

END SUB

SUB drwwin (x%, y%, x2%, y2%, title$)
 LINE (x%, y%)-(x2%, y2%), 7, BF
 LINE (x%, y%)-(x2%, y%), 8, B
 LINE (x%, y%)-(x%, y2%), 8, B
 LINE (x% + 1, y% + 1)-(x2% - 2, y% + 2), 15, BF
 LINE (x% + 1, y% + 1)-(x% + 2, y2% - 1), 15, BF
 LINE (x% + 1, y2%)-(x2%, y2%), 0, B
 LINE (x2% - 1, y% + 1)-(x2%, y2%), 0, B

 LINE (x% + 3, y% + 3)-(x2% - 3, y% + 23), 1, BF
 LINE (x% + 3, y% + 25)-(x2% - 3, y% + 25), 0, B
 gprint title$, x% + 7, y% + 7, 7
 drwwinbtn x2% - 20, y% + 5, 0
END SUB

SUB drwwinbtn (x%, y%, pressed%)
btx% = x%
bty% = y%
IF pressed% = 1 THEN btx% = btx% + 1: bty% = bty% + 1
LINE (x%, y%)-(x% + 16, y% + 16), 7, BF
LINE (x%, y%)-(x% + 16, y% + 16), 0, B
IF pressed% = 0 THEN
  LINE (x% + 1, y% + 1)-(x% + 14, y% + 1), 15, B
  LINE (x% + 1, y% + 1)-(x% + 1, y% + 14), 15, B
  LINE (x% + 1, y% + 15)-(x% + 15, y% + 15), 8, B
  LINE (x% + 15, y% + 1)-(x% + 15, y% + 15), 8, B

END IF
IF pressed% = 1 THEN
  LINE (x% + 1, y% + 1)-(x% + 14, y% + 1), 8, B
  LINE (x% + 1, y% + 1)-(x% + 1, y% + 14), 8, B

END IF
LINE (btx% + 3, bty% + 3)-(btx% + 11, bty% + 11), 0
LINE (btx% + 4, bty% + 3)-(btx% + 12, bty% + 11), 0
LINE (btx% + 3, bty% + 11)-(btx% + 11, bty% + 3), 0
LINE (btx% + 4, bty% + 11)-(btx% + 12, bty% + 3), 0
LINE (btx% + 3, bty% + 4)-(btx% + 6, bty% + 7), 15
LINE (btx% + 9, bty% + 7)-(btx% + 12, bty% + 4), 15

LINE (btx% + 7, bty% + 9)-(btx% + 5, bty% + 11), 15
LINE (btx% + 8, bty% + 9)-(btx% + 10, bty% + 11), 15

END SUB

SUB GetControl
MouseStatus lb%, rb%, x%, y%
IF lb% = -1 THEN
 ' FOR w = 50 TO 1 STEP -1 'Only get the controls in the active window
   w = ReturnActiveWin%
    IF QWin(w).x2 <> 0 THEN
    'Buttons
    FOR B = 1 TO 50
      IF QBtn(B).win = w THEN
      IF MouseLimit(QWin(w).x + 3 + QBtn(B).x, QWin(w).y + 26 + QBtn(B).y, 23 + 23 + (LEN(CBtn(B)) * 8), 30) = -1 AND CBtn(B) <> "" THEN
     
      MouseHide
      drwbtn QWin(w).x + QBtn(B).x + 3, QWin(w).y + QBtn(B).y + 26, CBtn(B), 1, 1
      MouseShow
      DO
      MouseStatus lb%, rb%, x%, y%
      LOOP UNTIL lb% = 0 OR MouseLimit(QWin(w).x + 3 + QBtn(B).x, QWin(w).y + 26 + QBtn(B).y, 23 + 23 + (LEN(CBtn(B)) * 8), 30) = 0
      MouseHide
     
      IF lstcont% <> 0 THEN drwbtn QWin(w).x + QBtn(lstcont%).x + 3, QWin(w).y + QBtn(lstcont%).y + 26, CBtn(lstcont%), 0, 0
      
      RemAllSel
      QBtn(B).sel = 1
      lstcont% = INT(B)
      drwbtn QWin(w).x + QBtn(B).x + 3, QWin(w).y + QBtn(B).y + 26, CBtn(B), 0, QBtn(B).sel
      MouseShow
      QBtn(B).clicked = 1
      
      EXIT SUB
      END IF
      END IF
    NEXT B

    'Text Boxes
    FOR t = 1 TO 50
      IF QTBox(t).win = w THEN
      IF MouseLimit(QWin(w).x + QTBox(t).x, QWin(w).y + QTBox(t).y, (QTBox(t).widinchar * 8) + 6, 28) = -1 AND QWin(w).x2 <> 0 AND QTBox(t).widinchar <> 0 THEN
        TBox INT(t), INT(w)
       
        drwtbox CTBox(t), QWin(w).x + QTBox(t).x, QWin(w).y + QTBox(t).y, QTBox(t).widinchar, QTBox(t).sel

      END IF
      END IF
    NEXT t

    'Value boxes
    FOR v = 1 TO 50
      'IF MouseLimit(QWin(w).x + QValBox(v).x, QWin(w).y + QValBox(v).y, (LEN(STR$(QValBox(v).max))*8) + 4, 28) = -1 AND QValBox(v).x <> 0 AND QValBox(v).y <> 0 THEN
      IF QValBox(v).win = w THEN
        IF MouseLimit(QWin(w).x + QValBox(v).x + (LEN(STR$(QValBox(v).max)) * 8), QWin(w).y + QValBox(v).y + 2, 16, 7) = -1 AND (QValBox(v).x <> 0 AND QValBox(v).y <> 0) THEN
           MouseHide
           drwvalbtn QWin(w).x + QValBox(v).x + (LEN(STR$(QValBox(v).max)) * 8), QWin(w).y + QValBox(v).y + 2, 1, 1
           MouseShow
          
           DO
           MouseStatus lb%, rb%, mx%, my%
           LOOP UNTIL lb% = 0
           QValues(v) = QValues(v) + 1
           IF QValBox(v).max <= QValues(v) THEN QValues(v) = QValBox(v).max
           MouseHide
           drwvalbtn QWin(w).x + QValBox(v).x + (LEN(STR$(QValBox(v).max)) * 8), QWin(w).y + QValBox(v).y + 2, 1, 0
           MouseShow
           x% = QWin(w).x + QValBox(v).x
           y% = QWin(w).y + QValBox(v).y
           LINE (x% + 2, y% + 4)-(x% + (LEN(STR$(QValBox(v).max)) * 8) - 4, y% + 15), 15, BF
           gprint STR$(QValues(v)), INT(x%), y% + 4, 0
           EXIT SUB
        END IF
        IF MouseLimit(QWin(w).x + QValBox(v).x + (LEN(STR$(QValBox(v).max)) * 8), QWin(w).y + QValBox(v).y + 11, 16, 7) = -1 AND (QValBox(v).x <> 0 AND QValBox(v).y <> 0) THEN
           MouseHide
           drwvalbtn QWin(w).x + QValBox(v).x + (LEN(STR$(QValBox(v).max)) * 8), QWin(w).y + QValBox(v).y + 11, 2, 1
           MouseShow
           
           DO
           MouseStatus lb%, rb%, mx%, my%
           LOOP UNTIL lb% = 0
           QValues(v) = QValues(v) - 1
           IF QValBox(v).min >= QValues(v) THEN QValues(v) = QValBox(v).min
           MouseHide
           drwvalbtn QWin(w).x + QValBox(v).x + (LEN(STR$(QValBox(v).max)) * 8), QWin(w).y + QValBox(v).y + 11, 2, 0
           MouseShow
           x% = QWin(w).x + QValBox(v).x
           y% = QWin(w).y + QValBox(v).y

           LINE (x% + 2, y% + 4)-(x% + (LEN(STR$(QValBox(v).max)) * 8) - 4, y% + 15), 15, BF
           gprint STR$(QValues(v)), INT(x%), y% + 4, 0
           EXIT SUB
        END IF
        
      END IF
    NEXT v

    'Check boxes
    FOR c = 1 TO 50
      IF QCBox(c).win = w THEN
      IF MouseLimit(QWin(w).x + QCBox(c).x, QWin(w).y + QCBox(c).y, 14, 14) = -1 AND (QCBox(c).x <> 0 AND QCBox(c).y <> 0) THEN
        IF QCBox(c).checked = 0 THEN
        QCBox(c).checked = 1
        ELSEIF QCBox(c).checked = 1 THEN
        QCBox(c).checked = 0
        END IF
        DO
        MouseStatus lb%, rb%, x%, y%
        LOOP UNTIL lb% = 0
        MouseHide
        drwcheckbox QWin(w).x + QCBox(c).x, QWin(w).y + QCBox(c).y, QCBox(c).checked
        MouseShow
      END IF
      END IF
    NEXT c

    'Radio Buttons
    FOR r = 1 TO 50
      IF QRBtn(r).win = w THEN
      IF MouseLimit(QWin(w).x + QRBtn(r).x, QWin(w).y + QRBtn(r).y, 16, 16) = -1 AND (QRBtn(r).x <> 0 AND QRBtn(r).y <> 0) THEN
        IF QRBtn(r).checked = 0 THEN
        
        FOR rz = 1 TO 50
          IF QRBtn(rz).group = QRBtn(r).group THEN
            QRBtn(rz).checked = 0
            MouseHide
            drwradio QWin(w).x + QRBtn(rz).x, QWin(w).y + QRBtn(rz).y, QRBtn(rz).checked
            MouseShow
          END IF
        NEXT rz
        QRBtn(r).checked = 1
        END IF
        DO
        MouseStatus lb%, rb%, x%, y%
        LOOP UNTIL lb% = 0
        MouseHide
        drwradio QWin(w).x + QRBtn(r).x, QWin(w).y + QRBtn(r).y, QRBtn(r).checked
        MouseShow
      END IF
      END IF
    NEXT r

    'List Boxes
    FOR l = 1 TO 50
      IF QLBox(l).win = w THEN
      IF QLArray(l, 1) <> "" THEN
        IF MouseLimit(QWin(w).x + QLBox(l).x2 - 16, QWin(w).y + QLBox(l).y + 2, 14, 16) = -1 THEN 'Up button
         IF QLBox(l).first > 1 THEN QLBox(l).first = QLBox(l).first - 1
         MouseHide
         drwscrlbtn QWin(w).x + QLBox(l).x2 - 14, QWin(w).y + QLBox(l).y, 1, 1
         MouseShow
         DO
           MouseStatus lb%, rb%, x%, y%
         LOOP UNTIL lb% = 0
         drwlistbx QWin(w).x + QLBox(l).x, QWin(w).y + QLBox(l).y, QWin(w).x + QLBox(l).x2, QLBox(l).lines, INT(l), QLBox(l).first - 1
         QLBox(l).itemsel = 0
         IF QLBox(l).itemsel <> 0 THEN
           my = QLBox(l).y + 5 + (18 * QLBox(l).itemsel - (QLBox(l).lines - 1)) - 32
           LINE (QWin(w).x + QLBox(l).x + 3, QWin(w).y + my - 4)-(QWin(w).x + QLBox(l).x + (QLBox(l).x2 - QLBox(l).x - 16), QWin(w).y + my + 18 - 4), 1, BF
           gprint QLArray(l, QLBox(l).itemsel - 1), QWin(w).x + QLBox(l).x + 5, QWin(w).y + my - 1, 15
         END IF
         MouseShow
        END IF

        IF MouseLimit(QWin(w).x + QLBox(l).x2 - 16, QWin(w).y + QLBox(l).y + (QLBox(l).lines * 18) + 2 - 10, 14, 16) = -1 THEN 'Down button
         IF QLBox(l).first < (QLBox(l).max - QLBox(l).lines + 1) THEN QLBox(l).first = QLBox(l).first + 1
         MouseHide
         drwscrlbtn QWin(w).x + QLBox(l).x2 - 14, QWin(w).y + QLBox(l).y + (QLBox(l).lines * 18) - 10, 2, 1
         MouseShow
         DO
           MouseStatus lb%, rb%, x%, y%
         LOOP UNTIL lb% = 0
         drwlistbx QWin(w).x + QLBox(l).x, QWin(w).y + QLBox(l).y, QWin(w).x + QLBox(l).x2, QLBox(l).lines, INT(l), QLBox(l).first - 1
         QLBox(l).itemsel = 0
         IF QLBox(l).itemsel <> 0 THEN
           my = QLBox(l).y + 5 + (18 * QLBox(l).itemsel - (QLBox(l).lines - 1)) - 32
           LINE (QWin(w).x + QLBox(l).x + 3, QWin(w).y + my - 4)-(QWin(w).x + QLBox(l).x + (QLBox(l).x2 - QLBox(l).x - 16), QWin(w).y + my + 18 - 4), 1, BF
           gprint QLArray(l, QLBox(l).itemsel - 1), QWin(w).x + QLBox(l).x + 5, QWin(w).y + my - 1, 15
         END IF
         MouseShow
        END IF

        FOR i = 0 TO (QLBox(l).lines - 1)
          my = QLBox(l).y + 5 + (18 * i)
          IF MouseLimit(QWin(w).x + QLBox(l).x + 5, QWin(w).y + my, (QLBox(l).x2 - QLBox(l).x - 20), 18) = -1 THEN
             MouseHide
             drwlistbx QWin(w).x + QLBox(l).x, QWin(w).y + QLBox(l).y, QWin(w).x + QLBox(l).x2, QLBox(l).lines, INT(l), QLBox(l).first - 1
             LINE (QWin(w).x + QLBox(l).x + 3, QWin(w).y + my - 4)-(QWin(w).x + QLBox(l).x + (QLBox(l).x2 - QLBox(l).x - 16), QWin(w).y + my + 18 - 4), 1, BF
             gprint QLArray(l, QLBox(l).first + i), QWin(w).x + QLBox(l).x + 5, QWin(w).y + my - 1, 15
             MouseShow
             DO
               MouseStatus lb%, rb%, x%, y%
             LOOP UNTIL lb% = 0
             QLBox(l).itemsel = QLBox(l).first + i
          END IF
        NEXT i
      END IF
      END IF
    NEXT l

    'Window Button - only one: Close (X)
    IF MouseLimit(QWin(w).x2 - 20, QWin(w).y + 5, 16, 16) = -1 THEN
      MouseHide
      drwwinbtn QWin(w).x2 - 20, QWin(w).y + 5, 1
      MouseShow
      DO
      MouseStatus lb%, rb%, x%, y%
      LOOP UNTIL lb% = 0 OR MouseLimit(QWin(w).x2 - 20, QWin(w).y + 5, 16, 16) = 0
      MouseHide
      drwwinbtn QWin(w).x2 - 20, QWin(w).y + 5, 0
      MouseShow
      IF ReturnActiveWin% = 1 THEN
      SYSTEM
      ELSE
      FOR sb = 1 TO 50
        QBtn(sb).sel = 0
      NEXT sb
      aw = ReturnActiveWin%
      aw = aw - 1
      FOR iw = 1 TO 50
        QWin(iw).active = 0
      NEXT iw
      QWin(aw).active = 1
      RedrawControls
      END IF
    END IF

    'Dragging Windows
    IF MouseLimit(QWin(w).x + 5, QWin(w).y + 5, (QWin(w).x2 - QWin(w).x - 20), 20) = -1 THEN
      mx% = x% - QWin(w).x: my% = y% - QWin(w).y
      wid% = QWin(w).x2 - QWin(w).x
      hei% = QWin(w).y2 - QWin(w).y
      DO
        MouseStatus lb%, rb%, x%, y%
        LINE (x% - mx%, y% - my%)-(x% + wid% - mx%, y% + hei% - my%), 15, B
        QWin(w).x = x% - mx%: QWin(w).y = y% - my%: QWin(w).x2 = x% + wid% - mx%: QWin(w).y2 = y% + hei% - my%
      LOOP UNTIL lb% = 0
      QWin(w).x = x% - mx%: QWin(w).y = y% - my%: QWin(w).x2 = x% + wid% - mx%: QWin(w).y2 = y% + hei% - my%
      IF ReturnActiveWin% <> 1 THEN
      Display "TEMP" + LTRIM$(STR$(ReturnActiveWin%))
      ELSE
      Display "TEMP"
      END IF
      RedrawControls
    END IF
    END IF
 ' NEXT w

END IF
END SUB

SUB gprint (z$, x%, y%, c%)
MouseHide
Regs.ax = &H1130
Regs.bx = &H600
CALL INTERRUPTX(&H10, Regs, Regs)
CharSegment% = Regs.es: CharOffset% = Regs.bp
CharWid% = 8: CharHgt% = 16

DEF SEG = CharSegment%
XX% = x%
FOR Char% = 1 TO LEN(z$)
   Ptr% = CharHgt% * ASC(MID$(z$, Char%, 1)) + CharOffset%
      FOR Ln% = 0 TO CharHgt% - 1
           BitPattern& = PEEK(Ptr% + Ln%) * 256&
                LineFormat% = (BitPattern& - 32768) XOR -32768
                     LINE (XX%, y% + Ln%)-STEP(CharWid% - 1, 0), c%, , LineFormat%
                        NEXT
                           XX% = XX% + CharWid%
                           NEXT
                           DEF SEG
MouseShow
END SUB

SUB Icon (x%, y%, filename$, disablecol%, id%)
QIcons(id%) = filename$
QIcon(id%).x = x%
QIcon(id%).y = y%
QIcon(id%).disablecol = disablecol%
QIcon(id%).win = ReturnActiveWin%
RedrawControls

END SUB

SUB Label (x%, y%, text$, c%, id%)
QLabel(id%).x = x%
QLabel(id%).y = y%
QLabel(id%).colr = c%
QLabel(id%).win = ReturnActiveWin%
QLabels(id%) = text$
RedrawControls
END SUB

SUB ListBox (x%, y%, x2%, lines%, max%, array$(), id%)
FOR a = 1 TO max%
  QLArray(id%, a) = array$(a)
NEXT a
QLBox(id%).x = x%
QLBox(id%).y = y%
QLBox(id%).x2 = x2%
QLBox(id%).lines = lines%
QLBox(id%).max = max%
QLBox(id%).itemsel = 0
QLBox(id%).first = 1
QLBox(id%).win = ReturnActiveWin%
RedrawControls
END SUB

SUB ListBox.AddItem (lstboxid%, item$, arrayid%)
QLArray(lstboxid%, arrayid%) = item$
END SUB

FUNCTION ListBox.Cont% (id%)
ListBox.Cont% = QLBox(id%).itemsel
END FUNCTION

SUB ListBox.DelItem (lstboxid%, arrayid%)

QLArray(lstboxid%, arrayid%) = ""
FOR i = arrayid% TO (QLBox(lstboxid%).max - 1)
QLArray(lstboxid%, i) = QLArray(lstboxid%, i + 1)
NEXT i
QLArray(lstboxid%, QLBox(lstboxid%).max) = ""
QLBox(lstboxid%).max = QLBox(lstboxid%).max - 1
END SUB

FUNCTION ListBox.Item$ (id%)
ListBox.Item$ = QLArray(id%, QLBox(id%).itemsel)
END FUNCTION

FUNCTION ListBox.ItemFromArray$ (lstboxid%, arrayid%)
ListBox.ItemFromArray$ = QLArray(lstboxid%, arrayid%)
END FUNCTION

SUB loadbmp (file$, PosX%, PosY%)
OPEN file$ FOR BINARY ACCESS READ AS #1
GET #1, 1, bfType%
IF bfType% <> 19778 THEN EXIT SUB
GET #1, 31, biCompression%
IF biCompression% <> 0 THEN EXIT SUB
GET #1, 29, biBitCount%
SELECT CASE biBitCount%
CASE 1: bmColors% = 2: bmStep% = 8
CASE 4: bmColors% = 16: bmStep% = 2
CASE 8: bmColors% = 256: bmStep% = 1
CASE ELSE: EXIT SUB
END SELECT
GET #1, 11, bfOffBits%
GET #1, 19, biWidth%
GET #1, 23, biHeight%
SEEK #1, 55
FOR bmPalette% = 0 TO bmColors% - 1
bmBlue% = ASC(INPUT$(1, 1)) \ 4
bmGreen% = ASC(INPUT$(1, 1)) \ 4
bmRed% = ASC(INPUT$(2, 1)) \ 4
OUT &H3C8, bmPalette%
OUT &H3C9, bmRed%
OUT &H3C9, bmGreen%
OUT &H3C9, bmBlue%
NEXT bmPalette%
LINE (PosX%, PosY%)-(PosX% + biWidth& - 1, PosY% + biHeight& - 1), 0, BF
SEEK #1, bfOffBits% + 1
FOR bmPosY% = PosY% + biHeight% - 1 TO PosY% STEP -1
bmBytes% = 0
FOR bmPosX% = PosX% TO PosX% + biWidth% - 1 STEP bmStep%
bmBytes% = bmBytes% + 1
bmPixel% = ASC(INPUT$(1, 1))
SELECT CASE bmColors%
CASE 2
IF (bmPixel% AND 128) THEN PSET (bmPosX%, bmPosY%), 1
IF (bmPixel% AND 64) THEN PSET (bmPosX% + 1, bmPosY%), 1
IF (bmPixel% AND 32) THEN PSET (bmPosX% + 2, bmPosY%), 1
IF (bmPixel% AND 16) THEN PSET (bmPosX% + 3, bmPosY%), 1
IF (bmPixel% AND 8) THEN PSET (bmPosX% + 4, bmPosY%), 1
IF (bmPixel% AND 4) THEN PSET (bmPosX% + 5, bmPosY%), 1
IF (bmPixel% AND 2) THEN PSET (bmPosX% + 6, bmPosY%), 1
IF (bmPixel% AND 1) THEN PSET (bmPosX% + 7, bmPosY%), 1
CASE 16
IF bmPixel% > 0 THEN
PSET (bmPosX%, bmPosY%), bmPixel% \ 16
PSET (bmPosX% + 1, bmPosY%), bmPixel% AND 15
END IF
CASE 256
IF bmPixel% > 0 THEN PSET (bmPosX%, bmPosY%), bmPixel%
END SELECT
NEXT bmPosX%
SELECT CASE bmBytes% MOD 4
CASE 1: bmPixel% = ASC(INPUT$(3, 1))
CASE 2: bmPixel% = ASC(INPUT$(2, 1))
CASE 3: bmPixel% = ASC(INPUT$(1, 1))
END SELECT
NEXT bmPosY%
CLOSE #1
END SUB

SUB loadIcon (x2%, y2%, filename$, disablecolor%)


STATIC icocol1, icocol2

'beneath = POINT(x2%, y2%)

'/* Icon File Main Header */'
Reserv1$ = SPACE$(2): Valid$ = SPACE$(2): NoOfIcons$ = SPACE$(2)

'/* Icon Main Header */'
PixelWidth$ = SPACE$(1): PixelHeight$ = SPACE$(1): NoOfColors$ = SPACE$(1)
Reserv2$ = SPACE$(1): Planes$ = SPACE$(2): BitCount$ = SPACE$(2)
TotalBytesOfImage$ = SPACE$(4): LocationOfImage$ = SPACE$(4)

'/* Icon Minor Header (BMP Info Header) */'
SizeOfHeader$ = SPACE$(4): Width$ = SPACE$(4): Height$ = SPACE$(4)
Plane$ = SPACE$(2): BitsPerPixel$ = SPACE$(2): Compressed$ = SPACE$(4)
SizeOfImage$ = SPACE$(4): XMeter$ = SPACE$(4): YMeter$ = SPACE$(4)
ClrUsed$ = SPACE$(4): ClrImportant$ = SPACE$(4)

OPEN filename$ FOR BINARY AS #255

GET #255, , Reserv1$
GET #255, , Valid$
GET #255, , NoOfIcons$

'   FOR IconNumber = 1 TO CVI(NoOfIcons$)
      '/* Extract Icon File Minor Header */'
      GET #255, , PixelWidth$
      GET #255, , PixelHeight$
      GET #255, , NoOfColors$
      GET #255, , Reserv2$
      GET #255, , Planes$
      GET #255, , BitCount$
      GET #255, , TotalBytesOfImage$
      GET #255, , LocationOfImage$
      Location& = LOC(255) + 1
      SEEK #255, CVL(LocationOfImage$) + 1
      MouseHide
      GOSUB ReadIcon
'      SEEK #255, Location&
'   NEXT IconNumber
CLOSE #255
MouseShow
EXIT SUB
ReadIcon:
'/* Extract Icon Minor Header */'
   GET #255, , SizeOfHeader$
   GET #255, , Width$
   GET #255, , Height$
   GET #255, , Plane$
   GET #255, , BitsPerPixel$
   GET #255, , Compressed$
   GET #255, , SizeOfImage$
   GET #255, , XMeter$
   GET #255, , YMeter$
   GET #255, , ClrUsed$
   GET #255, , ClrImportant$

   PixelWidth = CVL(Width$): PixelHeight = CVL(Height$)
   NumberOfColors& = 2 ^ CVI(BitsPerPixel$): PaletteBlue$ = SPACE$(1)
   PaletteGreen$ = SPACE$(1): PaletteRed$ = SPACE$(1): Empty$ = SPACE$(1)

   IF CVI(BitsPerPixel$) < 9 THEN
      FOR Loops = 0 TO NumberOfColors& - 1
         '/* Extract the palette of each of the colors and change the palette */'
         GET #255, , PaletteBlue$
         GET #255, , PaletteGreen$
         GET #255, , PaletteRed$
         GET #255, , Empty$
'         OUT &H3C8, Loops
'         OUT &H3C9, ASC(PaletteRed$) \ 4
'         OUT &H3C9, ASC(PaletteGreen$) \ 4
'         OUT &H3C9, ASC(PaletteBlue$) \ 4
      NEXT Loops
   END IF

   IF CVI(Reserv1$) = 0 THEN PixelHeight = PixelHeight \ 2

   IF CVI(BitsPerPixel$) = 4 THEN
      LineExtract$ = SPACE$(PixelWidth \ 2)
      IF (4 - CINT((PixelWidth MOD 8) / 2)) <> 4 THEN
         LineExtract$ = LineExtract$ + SPACE$(4 - CINT((PixelWidth MOD 8) / 2))
      END IF
      beneath = POINT(x2%, y2%)
      FOR y = PixelHeight - 1 TO 0 STEP -1
         GET #255, , LineExtract$
         FOR x = 0 TO PixelWidth - 1 STEP 2
            
            icocol1 = ASC(MID$(LineExtract$, INT(x / 2) + 1, 1)) \ 16
            icocol1 = icocol1
            PSET (x + x2%, y + y2%), icocol1
            icocol2 = ASC(MID$(LineExtract$, INT(x / 2) + 1, 1)) AND 15
            icocol2 = icocol2
            PSET (x + 1 + x2%, y + y2%), icocol2
            
         NEXT x
      NEXT y
      FOR a = y2% TO y2% + PixelHeight - 1
        FOR B = x2% TO x2% + PixelWidth - 1
          IF POINT(B, a) <> disablecolor THEN EXIT FOR
          IF POINT(B, a) = disablecolor THEN PSET (B, a), beneath
        NEXT B
        FOR B = x2% + PixelWidth - 1 TO x2% STEP -1
          IF POINT(B, a) <> disablecolor THEN EXIT FOR
          IF POINT(B, a) = disablecolor THEN PSET (B, a), beneath
        NEXT B
      NEXT a

      FOR a = x2% TO x2% + PixelWidth - 1
        d = 0
        FOR B = y2% TO y2% + PixelHeight - 1
          IF d = 1 AND POINT(a, B) <> disablecolor THEN EXIT FOR
          IF POINT(a, B) = disablecolor THEN PSET (a, B), beneath: d = 1
        NEXT B
        d = 0
        FOR B = y2% + PixelHeight - 1 TO y2% STEP -1
          IF d = 1 AND POINT(a, B) <> disablecolor THEN EXIT FOR
          IF POINT(a, B) = disablecolor THEN PSET (a, B), beneath: d = 1
        NEXT B
      NEXT a
    
    END IF

RETURN
END SUB

SUB mousedriver (ax%, bx%, cx%, dx%)

  DEF SEG = VARSEG(MOUSE$)
  MOUSE% = SADD(MOUSE$)
  CALL ABSOLUTE(ax%, bx%, cx%, dx%, MOUSE%)

END SUB

SUB MouseHide
 ax% = 2
 mousedriver ax%, 0, 0, 0
END SUB

FUNCTION mouseinit%
  ax% = 0
  mousedriver ax%, 0, 0, 0
  mouseinit% = ax%
END FUNCTION

'/* This function checks if the mouse is located in a given area           */'
FUNCTION MouseLimit% (MiniX%, MiniY%, MaxiX%, MaxiY%)

MouseLimit% = 0
CALL MouseStatus(LeftButton%, RightButton%, xMouse%, yMouse%)

IF xMouse% >= MiniX% AND xMouse% <= MiniX% + MaxiX% THEN
    IF yMouse% >= MiniY% AND yMouse% <= MiniY% + MaxiY% THEN
        MouseLimit% = -1
    END IF
END IF

END FUNCTION

'DEFLNG A-Z
SUB MousePut (x%, y%)
  ax% = 4
  cx% = x%
  dx% = y%
  mousedriver ax%, 0, cx%, dx%
END SUB

'DEFLNG A-Z
SUB MouseRange (x1%, y1%, x2%, y2%)
  ax% = 7
  cx% = x1%
  dx% = x2%
mousedriver ax%, 0, cx%, dx%
  ax% = 8
  cx% = y1%
  dx% = y2%
  mousedriver ax%, 0, cx%, dx%
END SUB

'DEFLNG A-Z
SUB MouseShow
  ax% = 1
  mousedriver ax%, 0, 0, 0
END SUB

'DEFLNG A-Z
SUB MouseStatus (lb%, rb%, xMouse%, yMouse%)
  ax% = 3
  mousedriver ax%, bx%, cx%, dx%
  lb% = ((bx% AND 1) <> 0)
  rb% = ((bx% AND 2) <> 0)
  xMouse% = cx%
  yMouse% = dx%
END SUB

FUNCTION Option.Cont% (id%)
Option.Cont% = QRBtn(id%).checked
END FUNCTION

SUB OptionCirc (x%, y%, checked%, group%, id%)
QRBtn(id%).x = x%
QRBtn(id%).y = y%
QRBtn(id%).group = group%
FOR r = 1 TO 50
  IF QRBtn(r).group = group% THEN QRBtn(r).checked = 0
NEXT r
QRBtn(id%).checked = checked%
QRBtn(id%).win = ReturnActiveWin%
RedrawControls
END SUB

SUB QB.box (x%, y%, x2%, y2%, colr%, filled%, id%)
QBox(id%).x = x%
QBox(id%).y = y%
QBox(id%).x2 = x2%
QBox(id%).y2 = y2%
QBox(id%).colr = colr%
QBox(id%).filled = filled%
QBox(id%).win = ReturnActiveWin%
RedrawControls
END SUB

SUB QB.circle (x%, y%, rad%, colr%, startd%, endd%, id%)
QCircle(id%).x = x%
QCircle(id%).y = y%
QCircle(id%).rad = rad%
QCircle(id%).colr = colr%
QCircle(id%).startd = startd%
QCircle(id%).endd = endd%
QCircle(id%).win = ReturnActiveWin%
RedrawControls
END SUB

SUB QB.line (x%, y%, x2%, y2%, colr%, id%)
QLine(id%).x = x%
QLine(id%).y = y%
QLine(id%).x2 = x2%
QLine(id%).y2 = y2%
QLine(id%).colr = colr%
QLine(id%).win = ReturnActiveWin%
RedrawControls
END SUB

SUB RedrawControls
MouseHide
'FOR w = 50 TO 1 STEP -1
w = ReturnActiveWin%
 IF CWin(w) <> "" THEN
 drwwin QWin(w).x, QWin(w).y, QWin(w).x2, QWin(w).y2, CWin(w)
 FOR B = 1 TO 50
  IF QBtn(B).win = w THEN
  IF CBtn(B) <> "" THEN
  drwbtn QWin(w).x + QBtn(B).x + 3, QWin(w).y + QBtn(B).y + 26, CBtn(B), 0, QBtn(B).sel
  END IF
  END IF
 NEXT B
 FOR t = 1 TO 50
  IF QTBox(t).win = w THEN
  IF QTBox(t).widinchar <> 0 THEN
  drwtbox CTBox(t), QWin(w).x + QTBox(t).x, QWin(w).y + QTBox(t).y, QTBox(t).widinchar, QTBox(t).sel
  END IF
  END IF
 NEXT t
 FOR v = 1 TO 50
  IF QValBox(v).win = w THEN
  IF QValBox(v).x <> 0 AND QValBox(v).y <> 0 THEN
  drwvalbox QWin(w).x + QValBox(v).x, QWin(w).y + QValBox(v).y, QValBox(v).max, QValues(v)
  END IF
  END IF
 NEXT v
 FOR l = 1 TO 50
  IF QLabel(l).win = w THEN
  IF QLabels(l) <> "" THEN
   gprint QLabels(l), INT(QWin(w).x) + INT(QLabel(l).x), INT(QWin(w).y) + INT(QLabel(l).y), INT(QLabel(l).colr)
  END IF
  END IF
 NEXT l
 FOR c = 1 TO 50
  IF QCBox(c).win = w THEN
  IF (QCBox(c).x <> 0 AND QCBox(c).y <> 0) THEN
   drwcheckbox QWin(w).x + QCBox(c).x, QWin(w).y + QCBox(c).y, QCBox(c).checked
  END IF
  END IF
 NEXT c
 FOR r = 1 TO 50
  IF QRBtn(r).win = w THEN
  IF QRBtn(r).group <> 0 OR (QRBtn(r).x <> 0 AND QRBtn(r).y <> 0) THEN
   drwradio QWin(w).x + QRBtn(r).x, QWin(w).y + QRBtn(r).y, QRBtn(r).checked
  END IF
  END IF
 NEXT r
 FOR p = 1 TO 50
   IF QBmp(p).win = w THEN
   IF QBmps(p) <> "" THEN
     loadbmp QBmps(p), QWin(w).x + QBmp(p).x, QWin(w).y + QBmp(p).y
   END IF
   END IF
 NEXT p
 FOR i = 1 TO 50
   IF QIcon(i).win = w THEN
   IF QIcons(i) <> "" THEN
     loadIcon QWin(w).x + QIcon(i).x, QWin(w).y + QIcon(i).y, QIcons(i), QIcon(i).disablecol
   END IF
   END IF
 NEXT i
 FOR l = 1 TO 50
   IF QLBox(l).win = w THEN
   IF QLArray(l, 1) <> "" THEN
     FOR a = 1 TO QLBox(l).max
       lists(a) = QLArray(l, a)
     NEXT a
     MouseHide
     
     drwlistbx QWin(w).x + QLBox(l).x, QWin(w).y + QLBox(l).y, QWin(w).x + QLBox(l).x2, QLBox(l).lines, INT(l), QLBox(l).first - 1
     IF QLBox(l).itemsel <> 0 THEN
     my = QLBox(l).y + 5 + (18 * (QLBox(l).itemsel - (QLBox(l).first + 1) + 1))
     LINE (QWin(w).x + QLBox(l).x + 3, QWin(w).y + my - 4)-(QWin(w).x + QLBox(l).x + (QLBox(l).x2 - QLBox(l).x - 16), QWin(w).y + my + 18 - 4), 1, BF
     gprint QLArray(l, QLBox(l).itemsel), QWin(w).x + QLBox(l).x + 5, QWin(w).y + my - 1, 15
     END IF
     
     MouseShow
   END IF
   END IF
 NEXT l
 FOR lin = 1 TO 50
   IF QLine(lin).win = w THEN
     LINE (QWin(w).x + QLine(lin).x, QWin(w).y + QLine(lin).y)-(QWin(w).x + QLine(lin).x2, QWin(w).y + QLine(lin).y2), QLine(lin).colr
   END IF
 NEXT lin
 FOR box = 1 TO 50
   IF QBox(box).win = w THEN
     IF QBox(box).filled = 0 THEN
     LINE (QWin(w).x + QBox(box).x, QWin(w).y + QBox(box).y)-(QWin(w).x + QBox(box).x2, QWin(w).y + QBox(box).y2), QBox(box).colr, B
     ELSEIF QBox(box).filled = 1 THEN
     LINE (QWin(w).x + QBox(box).x, QWin(w).y + QBox(box).y)-(QWin(w).x + QBox(box).x2, QWin(w).y + QBox(box).y2), QBox(box).colr, BF
     END IF
   END IF
 NEXT box
 FOR cir = 1 TO 50
   IF QCircle(cir).win = w THEN
     CIRCLE (QWin(w).x + QCircle(cir).x, QWin(w).y + QCircle(cir).y), QCircle(cir).rad, QCircle(cir).colr, QCircle(cir).startd, QCircle(cir).endd
   END IF
 NEXT cir
 END IF
'NEXT w
MouseShow
END SUB

SUB RemAllSel
FOR B = 1 TO 50
  QBtn(B).sel = 0
NEXT B
FOR t = 1 TO 50
 QTBox(t).sel = 0
NEXT t
END SUB

FUNCTION ReturnActiveWin%
FOR w = 1 TO 50
  IF QWin(w).active = 1 THEN ReturnActiveWin% = w: EXIT FUNCTION
NEXT w
END FUNCTION

SUB TBox (id%, winid%)
x% = QWin(winid%).x + QTBox(id%).x
y% = QWin(winid%).y + QTBox(id%).y
widinchar% = QTBox(id%).widinchar
text$ = CTBox(id%)
drwtbox text$, x%, y%, widinchar%, 1
text1$ = text$
gprint text1$ + "_", x% + 5, y% + 2, 0
DO
  DO
    KeyPressed$ = INKEY$
  LOOP WHILE KeyPressed$ = ""
  IF KeyPressed$ = CHR$(8) AND LEN(text1$) >= 1 THEN
    text1$ = LEFT$(text1$, LEN(text1$) - 1)
    LINE (x% + 5, y% + 2)-(x% + (widinchar% * 8) + 5, y% + 16), 15, BF
    gprint text1$ + "_", x% + 5, y% + 2, 0
  END IF
  IF KeyPressed$ = CHR$(13) THEN CTBox(id%) = text1$: EXIT DO
  IF KeyPressed$ > CHR$(29) AND KeyPressed$ < CHR$(127) AND LEN(text1$) <= (widinchar% - 2) THEN ' -2 is a safety precaution for making the text stay INSIDE the textbox
    text1$ = text1$ + KeyPressed$
    LINE (x% + 5, y% + 2)-(x% + (widinchar% * 8) + 5, y% + 16), 15, BF
    gprint text1$ + "_", x% + 5, y% + 2, 0
  END IF
  IF LEN(text1$) >= widinchar% THEN
    SOUND 2000, .5
  END IF
LOOP
END SUB

SUB TextBox (caption$, x%, y%, widinchar%, id%)
QTBox(id%).x = x%
QTBox(id%).y = y%
QTBox(id%).widinchar = widinchar%
CTBox(id%) = caption$
QTBox(id%).sel = 0
QTBox(id%).win = ReturnActiveWin%
RedrawControls
END SUB

FUNCTION TextBox.Cont$ (id%)
TextBox.Cont$ = CTBox(id%)
END FUNCTION

SUB TextBox.SetText (id%, text$)
CTBox(id%) = text$
RedrawControls
END SUB

SUB ValueBox (x%, y%, min%, max%, id%)
QValBox(id%).x = x%
QValBox(id%).y = y%
QValBox(id%).min = min%
QValBox(id%).max = max%
QValBox(id%).win = ReturnActiveWin%
QValues(id%) = min%
RedrawControls
END SUB

FUNCTION ValueBox.Cont% (id%)
ValueBox.Cont% = QValues(id%)
END FUNCTION

SUB win (x%, y%, x2%, y2%, title$, id%)
QWin(id%).x = x%
QWin(id%).y = y%
QWin(id%).x2 = x2%
QWin(id%).y2 = y2%
FOR w = 1 TO 50
  QWin(w).active = 0
NEXT w
QWin(id%).active = 1
CWin(id%) = title$
IF id% <> 1 THEN MouseHide: Capture "TEMP" + LTRIM$(STR$(id%)): MouseShow
lstcont% = 0
FOR B = 1 TO 50
  QBtn(B).sel = 0
NEXT B
RedrawControls
END SUB


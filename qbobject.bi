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
DECLARE SUB TextBox.SetText (id%, text$)
DECLARE SUB CloseWindow (id%)
DECLARE SUB ListBox.AddItem (lstboxid%, item$, arrayid%)
DECLARE SUB ListBox.DelItem (lstboxid%, arrayid%)
DECLARE FUNCTION ReturnActiveWin% ()
DECLARE SUB Display (Prefix$)
DECLARE SUB Capture (Prefix$)
DECLARE SUB Label (x%, y%, text$, c%, id%)
DECLARE FUNCTION TextBox.Cont$ (id%)
DECLARE FUNCTION ListBox.Item$ (id%)
DECLARE SUB Bitmap (x%, y%, file$, id%)
DECLARE FUNCTION ValueBox.Cont% (id%)
DECLARE FUNCTION ListBox.Cont% (id%)
DECLARE SUB ListBox (x%, y%, x2%, lines%, max%, array$(), id%)
DECLARE SUB drwlistbx (x%, y%, x2%, lines%, lstboxid%, offset%)
DECLARE SUB Icon (x%, y%, filename$, disablecol%, id%)
DECLARE SUB LoadIcon (x2%, y2%, filename$, disablecolor%)
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
DECLARE SUB tbox (id%, winid%)
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
DECLARE SUB QB.box (x%, y%, x2%, y2%, colr%, filled%, id%)
DECLARE SUB QB.circle (x%, y%, rad%, colr%, startd%, endd%, id%)
DECLARE SUB QB.line (x%, y%, x2%, y2%, colr%, id%)

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

REDIM QLArray(50, 100) AS STRING
REDIM QLBox(50) AS TListBox
REDIM CWin(50) AS STRING
REDIM CBtn(50) AS STRING
REDIM CTBox(50) AS STRING
REDIM QWin(50) AS TWin
REDIM QBtn(50) AS TBtn
REDIM QTBox(50) AS TTBox
REDIM QValBox(50) AS TValBox
REDIM QValues(50) AS INTEGER
REDIM QLabels(50) AS STRING
REDIM QLabel(50) AS TLabel
REDIM QCBox(50) AS TCheckBox
REDIM QRBtn(50) AS TRadioButton
REDIM QBmp(50) AS TBitmap
REDIM QBmps(50) AS STRING
REDIM QIcon(50) AS TIcon
REDIM QIcons(50) AS STRING
REDIM QLine(50) AS TLine
REDIM QBox(50) AS TBox
REDIM QCircle(50) AS TCircle
REDIM lists(100) AS STRING

MOUSE$ = SPACE$(57)
FOR i% = 1 TO 57
    READ a$
    H$ = CHR$(VAL("&H" + a$))
    MID$(MOUSE$, i%, 1) = H$
NEXT i%
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00

activecont% = 0
lstcont% = 0

SCREEN 12
Capture "TEMP"
MouseShow


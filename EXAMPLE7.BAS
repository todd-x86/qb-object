'$INCLUDE: 'QBOBJECT.BI'

win 15, 10, 440, 400, "Example 7 - Icons and Bitmaps", 1
Bitmap 5, 35, "someguy.bmp", 1
Label 65, 315, "Who is this?", 0, 1
Label 15, 335, "when loading BMPs, they mess up the palette... :-(", 12, 2
Label 15, 355, "sorry, I can't fix that.", 11, 3
Icon 385, 40, "diskdrv.ico", 0, 1
Icon 340, 40, "colors.ico", 0, 2
Icon 305, 40, "package.ico", 15, 3
DO
GetControl
LOOP


'$INCLUDE: 'QBOBJECT.BI'

win 100, 100, 440, 240, "Example 1 - Window", 1
label 5, 30, "This demonstrates how to use the window", 0, 1
label 25, 50, "Click the 'X' to close the window...", 9, 2
label 25, 70, "You can drag this window too!!!", 12, 3
DO
GetControl
LOOP


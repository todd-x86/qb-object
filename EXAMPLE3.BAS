'$INCLUDE: 'QBOBJECT.BI'

win 10, 10, 220, 140, "Example 3 - Textboxes", 1
Label 25, 35, "What is your name?", 0, 1
TextBox "", 25, 55, 16, 1
button "Hello!", 25, 55, 1
DO
GetControl
IF Button.Cont(1) THEN PRINT "Hello " + TextBox.Cont(1)
LOOP


'$INCLUDE: 'QBOBJECT.BI'

win 110, 200, 340, 305, "Example 5 - Checkboxes", 1
Label 15, 35, "What do you program in?", 0, 1
CheckBox 15, 55, 0, 1
Label 32, 55, "Delphi/Pascal", 0, 2
CheckBox 15, 70, 0, 2
Label 32, 70, "QuickBASIC", 0, 3
CheckBox 15, 85, 0, 3
Label 32, 85, "Other", 0, 4
button "OK", 145, 45, 1
DO
 GetControl
 IF Button.Cont(1) THEN
   IF Check.Cont(1) THEN
     PRINT "You program in Delphi/Pascal."
   END IF
   IF Check.Cont(2) THEN
     PRINT "You program in QuickBASIC."
   END IF
   IF Check.Cont(3) THEN
     PRINT "You program in another language."
   END IF
   LOCATE 1
 END IF
LOOP


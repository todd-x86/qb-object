'$INCLUDE: 'QBOBJECT.BI'

win 10, 10, 240, 160, "Example 4 - Valueboxes", 1
Label 25, 35, "How old are you?", 0, 1
ValueBox 35, 55, 1, 100, 1
button "Process", 35, 75, 1
DO
 GetControl
 IF Button.Cont(1) THEN
   IF ValueBox.Cont(1) = 14 THEN PRINT "Really? I'm 14 years old!!!"
   IF ValueBox.Cont(1) = 15 THEN PRINT "I'm going to be 15 on August 15, 2005!"
   IF ValueBox.Cont(1) > 15 THEN PRINT "Okay, you're" + STR$(ValueBox.Cont(1)) + " years old..."
   IF ValueBox.Cont(1) < 14 THEN PRINT "Wow, you're young..."
 END IF
LOOP


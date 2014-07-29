'$INCLUDE: 'QBOBJECT.BI'


win 15, 20, 400, 230, "Example 2 - Buttons", 1
button "Button 1", 5, 10, 1
button "Button 2", 5, 45, 2
button "Click to close me!", 5, 80, 3
Label 125, 79, "Button 2 does nothing", 0, 1
DO
 GetControl
 IF Button.Cont(1) THEN PRINT "I've been clicked!!!"
 IF Button.Cont(3) THEN SYSTEM
LOOP


'$INCLUDE: 'QBOBJECT.BI'

win 10, 10, 240, 160, "Example 6 - Radiobuttons", 1
Label 25, 35, "Display 1?    Yes     No", 0, 1
Label 25, 55, "Display 2?    Yes     No", 0, 2
Label 25, 75, "Display 3?    Yes     No", 0, 3
OptionCirc 116, 35, 0, 1, 1
OptionCirc 180, 35, 0, 1, 2
OptionCirc 116, 55, 0, 2, 3
OptionCirc 180, 55, 0, 2, 4
OptionCirc 116, 75, 0, 3, 5
OptionCirc 180, 75, 0, 3, 6

button "Process", 35, 75, 1
DO
 GetControl
 IF Button.Cont(1) THEN
   IF Option.Cont(1) THEN
     PRINT "Displaying 1"
   END IF
   IF Option.Cont(3) THEN
     PRINT "Displaying 2"
   END IF
   IF Option.Cont(5) THEN
     PRINT "Displaying 3"
   END IF
   LOCATE 1
 END IF
LOOP


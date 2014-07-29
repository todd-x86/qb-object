'$INCLUDE: 'QBOBJECT.BI'

DIM SHARED names(20) AS STRING

names(1) = "Todd"
names(2) = "Jacob"
names(3) = "Brandon"
names(4) = "Randy"
names(5) = "Alexander"
names(6) = "Steve"
names(7) = "Brendan"
names(8) = "Mike"
names(9) = "Bill"
names(10) = "Fabian"
names(11) = "Andrea"
names(12) = "Rose"
names(13) = "Amy"
names(14) = "Sam"
names(15) = "Jessica"
names(16) = "Julianne"
names(17) = "Katelyn"
names(18) = "Kate"
names(19) = "Amanda"
names(20) = "Theodora"

win 10, 25, 370, 290, "Example 8 - List Boxes", 1
ListBox 15, 55, 180, 10, 20, names(), 1
Label 15, 35, "What is your name?", 0, 1
button "Go", 210, 45, 1
DO
  GetControl
  IF Button.Cont(1) THEN
    IF ListBox.Cont(1) THEN
       PRINT "So, you're name is " + ListBox.Item(1) + "?"
    END IF
  END IF
LOOP


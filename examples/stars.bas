10 INPUT "What is your name"; U$
20 PRINT "Hello "; U$
30 INPUT "How many stars do you want"; N
40 S$ = ""
50 FOR I = 1 TO N
60 S$ = S$ + "*"
70 NEXT I
80 PRINT S$
90 INPUT "Do you want more stars"; A$
100 IF LEN(A$) = 0 THEN 90
110 A$ = LEFT$(A$, 1)
120 IF A$ = "Y" OR A$ = "y" THEN 30
130 PRINT "Goodbye ";U$
140 END

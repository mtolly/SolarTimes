






REM        xxxxxx      xxxxxx    xx              xx      xxxxxxxxx
REM       xxxxxxxx    xxxxxxxx   xx            xx  xx    xxxxxxxxxx
REM      xx      xx  xx      xx  xx            xx  xx    xx      xx
REM      xx      xx  xx      xx  xx           xx    xx   xx      xx
REM      xx          xx      xx  xx           xx    xx   xx      xx
REM       xxxxxxx    xx      xx  xx           xxxxxxxx   xxxxxxxxxx
REM        xxxxxxx   xx      xx  xx           xxxxxxxx   xxxxxxxxx
REM              xx  xx      xx  xx          xx      xx  xx  xx
REM      xx      xx  xx      xx  xx          xx      xx  xx   xx
REM      xx      xx  xx      xx  xx          xx      xx  xx    xx
REM       xxxxxxxx    xxxxxxxx   xxxxxxxxxx  xx      xx  xx     xx
REM        xxxxxx      xxxxxx    xxxxxxxxxx  xx      xx  xx      xx
REM
REM                        xxxxxxxxxx  xx  xx       xx  xxxxxxxxxx    xxxxxx
REM                        xxxxxxxxxx  xx  xxxx   xxxx  xxxxxxxxxx   xxxxxxxx
REM                            xx      xx  xxxx   xxxx  xx          xx      xx
REM                            xx      xx  xx xx xx xx  xx          xx      xx
REM                            xx      xx  xx xx xx xx  xx          xx
REM                            xx      xx  xx  xxx  xx  xxxxxxxxxx   xxxxxxx
REM                            xx      xx  xx  xxx  xx  xxxxxxxxxx    xxxxxxx
REM                            xx      xx  xx   x   xx  xx                  xx
REM                            xx      xx  xx       xx  xx          xx      xx
REM                            xx      xx  xx       xx  xx          xx      xx
REM                            xx      xx  xx       xx  xxxxxxxxxx   xxxxxxxx
REM                            xx      xx  xx       xx  xxxxxxxxxx    xxxxxx



REM  SolarTimes 52xx.bas









1 REM The name of this program is "SolarTimes"
2 REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
3 PRINT
4 REM This program is a revised, corrected and ammended program
5 REM by Harry Maybeck, Route 175, P.O. Box 62, Holderness, NH 03245
6 REM from various solar programs originally written by students,
7 REM High School teachers, University professors, Astronomers,
8 REM Pilots, airlines Flight Dispatchers and amateur and
9 REM professional Meteorologists. All programs had been placed
10 REM in the "PUBLIC DOMAIN". Upon running these various programs,
11 REM numerous errors in program language, technical statements,
12 REM and mathematical constants and equations were brought to light.
13 REM The technical statements and the mathematical (astronomical)
14 REM constants and equations errors resulted in varying and
15 REM erroneous results. These technical and mathematical errors
16 REM were corrected using various accepted astronomy, navigation
17 REM and mathematics texts and tables. The program language errors
18 REM resulted in unnecessary "crashes" while running the programs
19 REM using certain unordinary input (such as: Latitudes higher than
20 REM 70 degrees, East Longitudes, South Latitudes, 0 (zero)
21 REM Longitude or Latitude, etc.). These program language errors were
22 REM corrected using "Basic" definitions and instructions found
23 REM in "AMIGA (tm), User Guides (Amiga Basic)" and "KIDS AND
24 REM THE AMIGA (c)", E. H. Carlson, COMPUTE!(tm)Publications, Inc.
25 REM It is hoped that this compilation of, and adjustments to, these
26 REM many programs will provide a better program for the many
27 REM AMIGA(tm) users who may have need of the times of Sunrise and
28 REM Sunset for "any place" on "any day".
30 REM ****************************************************************
31 PRINT
32 REM One note regarding azimuth angles: In the SOUTHERN hemisphere,
33 REM this program assumes that the South Pole is zero degrees, and
34 REM the azimuth angle is measured COUNTER-clockwise through East.
35 REM Therefore, an azimuth angle of 108 degrees is NORTH of East.
36 REM When Lat and Long are input, use a negative value for Southern
37 REM Latitudes and for Eastern Longitudes.
40 REM ----------------- Program Begins ---------------------------
50 PRINT
60 PRINT"                     SolarTimes"
70 PRINT
80 PRINT"This program finds the declination of the sun, the equation"
81 PRINT"of time, the azimuth angles of sunrise and sunset, and the"
82 PRINT"times of sunrise and sunset for any point on earth."
83 PRINT
84 PRINT"When entering the name of the location...do NOT use a , (comma)."
85 PRINT
86 PRINT"Input Latitudes and Longitudes in Degrees and Minutes and"
87 PRINT"tenths of Minutes using the format DD.MMM"
88 PRINT"ie. 23 Degrees and 41.6 Minutes would be entered as 23.416"
89 PRINT
90 PRINT"Input South Latitudes and East Longitudes as NEGATIVE."
91 PRINT
100 PRINT

110 COLOR 0,1
120 REM
130 REM
140 DIM N(12)
150 PL=3.1415926#/26:J=57.2958779
160 SAY TRANSLATE$("enter the location")
170 INPUT "ENTER LOCATION (NAME)"; LC$
180 A$ = TRANSLATE$ (LC$)
190 SAY TRANSLATE$("the location iz"):SAY(A$)

195 REM Get the Latitude from the user.
200 SAY TRANSLATE$("enter the latitood. remember to enter south latitoods with a negativ number.")
210 INPUT "ENTER LATITUDE (FORMAT DD.MMM)"; D1

220 CALL BREAKINPUT(D1,latdeg,latmin)

229 REM Check for Latitude input errors.
230 IF latdeg>90 OR latdeg<-90 THEN SAY TRANSLATE$("that latitood duz not exsist on the earths surface. pleez re-enter the latitood."):GOTO 210
240 IF latmin>59.9 OR latmin<-59.9 THEN SAY TRANSLATE$("minuts must be less than sixty. pleez re-enter the latitood."):GOTO 210
241 IF latdeg=90 AND latmin>0 THEN SAY TRANSLATE$("that latitood duz not exsist on the earths surface. pleez re-enter the latitood."):GOTO 210
242 IF latdeg=-90 and latmin<-0 THEN SAY TRANSLATE$("that latitood duz not exsist on the earths surface. pleez re-enter the latitood."):GOTO 210

250 BD$=TRANSLATE$(STR$(latdeg))
260 BM$=TRANSLATE$(STR$(latmin))

270 IF D1>0 AND D1<89.599 THEN
280    SAY (BD$): SAY TRANSLATE$("da-grees")
290    SAY (BM$): SAY TRANSLATE$("minuts north latitude.")
291 END IF
300 IF D1<0 AND D1>-89.599 THEN
310    SAY (BD$): SAY TRANSLATE$("da-grees")
311    SAY (BM$): SAY TRANSLATE$("minuts south latitude.")
312 END IF

313 REM Say special Latitudes.
314 IF D1=0 THEN SAY TRANSLATE$("latitood iz on the ekwaytor")
315 IF D1>89.599 THEN SAY TRANSLATE$("latitood iz the north pole. no lonjitood has to be entered."):D2=0:GOTO 360
316 IF D1<-89.599 THEN SAY TRANSLATE$("latitood iz the south pole. no lonjitood has to be entered."):D2=0:GOTO 360

317 REM Get the Longitude from the user.
318 SAY TRANSLATE$("enter the lonjitood. remember to enter east lonjitoods widh a negativ number.")
319 INPUT "ENTER LONGITUDE (FORMAT DD.MMM)"; D2

320 CALL BREAKINPUT(D2,longdeg,longmin)

321 REM Check for Longitude input errors.
322 IF longdeg>180 OR longdeg<-180 THEN SAY TRANSLATE$("that lonjitood duz not exsist on the earths surface. pleez re-enter the lonjitood."):GOTO 319
323 IF longmin>59.9 OR longmin<-59.9 THEN SAY TRANSLATE$("minuts must be less than sixty. pleez re-enter the longitood."):GOTO 319
324 IF longdeg=180 AND longmin>0 THEN SAY TRANSLATE$("that lonjitood duz not exsist on the earths surface. pleez re-enter the lonjitood."):GOTO 319
325 IF longdeg=-180 and longmin<-0 THEN SAY TRANSLATE$("that lonjitood duz not exsist on the earths surface. pleez re-enter the lonjitood."):GOTO 319

340 BD$=TRANSLATE$(STR$(longdeg))
341 BM$=TRANSLATE$(STR$(longmin))

342 IF D2>0 AND D2<179.599 THEN
343    SAY (BD$): SAY TRANSLATE$("da-grees")
344    SAY (BM$): SAY TRANSLATE$("minuts west lonjitood.")
345 END IF
346 IF D2<0 AND D1>-179.599 THEN
347    SAY (BD$): SAY TRANSLATE$("da-grees")
348    SAY (BM$): SAY TRANSLATE$("minuts east lonjitood.")
349 END IF

350 REM Say special Longitudes.
351 IF D2>179.599# THEN SAY TRANSLATE$("lonjitood iz on the intenational date line.")
352 IF D2<-179.599# THEN SAY TRANSLATE$("lonjitood iz on the intenational date line")
353 IF D2=0 THEN SAY TRANSLATE$("lonjitood iz on the grenich prime meridian.")

360 LPRINT "Solar Times for ";LC$; CHR$(10)
370 IF D1>0 AND D1<89.599 THEN LPRINT"Latitude -  ";D1;"Deg N
380 IF D1<0 AND D1>-89.599 THEN LPRINT"Latitude -  ";(ABS(D1));"Deg S
390 IF D1=0 THEN LPRINT"Lat -   EQUATOR
400 IF D1>89.599 THEN LPRINT"Latitude -   NORTH POLE
410 IF D1<-89.599 THEN LPRINT"Latitude -   SOUTH POLE
420 IF D2>0 AND D2<179.599# THEN LPRINT"Longitude - ";D2;"Deg W
430 IF D2<0 AND D2>-179.599# THEN LPRINT"Longitude - ";(ABS(D2));"Deg E
440 IF D2>179.599# THEN LPRINT"Longitude - INTL DATE LINE
450 IF D2<-179.599# THEN LPRINT"Longitude - INTL DATE LINE
460 IF D2=0 THEN LPRINT"Longitude- GREENWICH (PRIME) MERIDAN

469 REM Change both D1 and D2 from DD.MMM format to DD.DDD (decimal format)
470 GOSUB 1680

480 LA = D1
490 IF LA < 0 THEN LA = LA + 180
500 IF D2 < 0 THEN D2 = D2 + 360
510 LO = (CINT(D2/15))*15 :REM finds time zone beginning
520 TD = (D2-LO) / 15
530 SAY TRANSLATE$("enter the date")

531 INPUT "ENTER MONTH,DAY (MM,DA)"; M,da
532 IF M>12  OR M<1 THEN SAY TRANSLATE$("there are only twelve months in the year. Pleez re-enter the date."):GOTO 531

533 REM - Check for # days in a specific month
534 IF M=1 AND da>31 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in januaree. pleez-reenter the date."):GOTO 531
535 IF M=3 AND da>31 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in march. pleez-reenter the date."):GOTO 531
536 IF M=5 AND da>31 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in may. pleez-reenter the date."):GOTO 531
537 IF M=7 AND da>31 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in jooly. pleez-reenter the date."):GOTO 531
538 IF M=8 AND da>31 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in august. pleez-reenter the date."):GOTO 531
539 IF M=10 AND da>31 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in october. pleez-reenter the date."):GOTO 531
540 IF M=12 AND da>31 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in december. pleez-reenter the date."):GOTO 531

541 IF M=4 AND da>30 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in aypril. pleez-reenter the date."):GOTO 531
542 IF M=6 AND da>30 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in joone. pleez-reenter the date."):GOTO 531
543 IF M=9 AND da>30 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in september. pleez-reenter the date."):GOTO 531
544 IF M=11 AND da>30 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in november. pleez-reenter the date."):GOTO 531
545 IF M=2 AND da>28 OR da<1 THEN SAY TRANSLATE$("there are-ent that many days in februaree in a normal year. remember there are only twenty-eight dayz in februaree except in a leap year when there are twenty-nine dayz. pleez re-enter the date."):GOTO 546
546 IF M=2 AND da>28 OR da<1 THEN INPUT "ENTER MONTH,DAY (MM,DA)";M,da
547 IF M=2 AND da>29 THEN GOTO 545

548 REM This calculates X which is the week of the year for the date input
550 FOR I=1 TO 12: READ N(I):NEXT I
560 DATA 0,31,59,90,120,151
570 DATA 181,212,243,273,304,334
580 x=(N(M)+da)/7

590 DIM Months$(12)
600 FOR MI = 1 TO 12: READ Months$(MI): NEXT MI
610 DATA "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"

620 LPRINT "DATE: "; Months$(M); da;"

630 DIM MonthsSay$(12)
640 FOR MI = 1 TO 12: READ MonthsSay$(MI): NEXT MI
650 DATA "januaree", "februaree", "march", "aypril", "may", "joone", "juluy"
660 DATA "august", "september", "october", "november", "december"

670 SAY TRANSLATE$(MonthsSay$(M))

680 DIM DaysSay$(31)
690 FOR DAYI = 1 TO 31: READ DaysSay$(DAYI): NEXT DAYI
700 DATA "first", "second", "third", "forth", "fifth", "sixth", "seventh"
710 DATA "eighth", "nynth", "tenth", "eeleventh", "twelfth", "thirteenth"
720 DATA "forteenth", "fifteenth", "sicksteenth", "seventeenth", "eight-teenth"
730 DATA "nynteenth", "twenteeith", "twentee-first", "twentee-second", "twentee-third"
740 DATA "twentee-forth", "twentee-fifth", "twentee-sixth", "twentee-seventh", "twentee-eighth"
750 DATA "twentee-nynth", "thirteeeth", "thirtee-first"

760 SAY TRANSLATE$(DaysSay$(da))

832 IF M=1 AND da=1 THEN SAY TRANSLATE$("new yearz day")
833 IF M=2 AND da=2 THEN SAY TRANSLATE$("ground hog day")
834 IF M=2 AND da=12 THEN SAY TRANSLATE$("aybraham linkonz birthday")
835 IF M=2 AND da=22 THEN SAY TRANSLATE$("george washingtonz birthday")
836 IF M=2 AND da=14 THEN SAY TRANSLATE$("valentinez day")
837 IF M=3 AND da=17 THEN SAY TRANSLATE$("saint patrickz day")
838 IF M=5 AND da=22 THEN SAY TRANSLATE$("viktoria day in canada")
839 IF M=5 AND da=30 THEN SAY TRANSLATE$("u s memorial day")
840 IF M=6 AND da=14 THEN SAY TRANSLATE$("u s flag day")
841 IF M=7 AND da=1 THEN SAY TRANSLATE$("dominion day in canada")
842 IF M=7 AND da=4 THEN SAY TRANSLATE$("u s independence day")
843 IF M=7 AND da=24 THEN SAY TRANSLATE$("happy birthday margee maybeck")
844 IF M=10 AND da=12 THEN SAY TRANSLATE$("columbus day")
845 IF M=10 AND da=31 THEN SAY TRANSLATE$("halloween")
846 IF M=11 AND da=1 THEN SAY TRANSLATE$("all saintz day")
847 IF M=11 AND da=11 THEN SAY TRANSLATE$("armistis or veteranz day")
848 IF M=12 AND da=25 THEN SAY TRANSLATE$("christmas day")

849 COLOR 1,0
850 PRINT
851 LPRINT
852 REM
853 COLOR 0,3
854 PRINT
855 PRINT

859 REM Report info for Declination of sun
860 D=.4560001-22.195*COS(PL*x)-.43*COS(2*PL*x)-.156*COS(3*PL*x)+3.83*SIN(PL*x)+.06*SIN(2*PL*x)-.082*SIN(3*PL*x)

870 REM This truncates all but tenths decimal for say function
871 DTEMP=D*10
872 II&=CLNG(DTEMP)
873 DTEMP=II&/10
890 H$=TRANSLATE$(STR$(DTEMP))

900 PRINT"DECLINATION OF SUN: ";
910 PRINT USING"###.#";D;
920 PRINT" DEGREES"
930 LPRINT"DECLINATION OF SUN:";
940 LPRINT USING" ###.#";D;
950 LPRINT" DEGREES"

960 IF D=0 THEN SAY TRANSLATE("the declination of the sun iz zero da-grees")
970 IF D>0 THEN SAY TRANSLATE("the declination of the sun iz"): SAY (H$):SAY TRANSLATE$("da-grees")
980 IF D<0 THEN SAY TRANSLATE("the declination of the sun iz mynus"): SAY (H$):SAY TRANSLATE$("da-grees")

989 REM Report info for Equation of Time
990 E=.008000001#+.51*COS(PL*x)-3.197*COS(2*PL*x)-.106*COS(3*PL*x)-.15*COS(4*PL*x)-7.317001*SIN(PL*x)-9.471001*SIN(2*PL*x)-.391*SIN(3*PL*x)-.242*SIN(4*PL*x)
991 REM This truncates all but tenths decimal for say function
992 DTEMP=E*10
993 II&=CLNG(DTEMP)
994 DTEMP=II&/10
995 J$=TRANSLATE$(STR$(DTEMP))

1010 PRINT"EQUATION OF TIME:";
1020 PRINT USING"###.#";E;
1030 PRINT" MINUTES"
1040 LPRINT"EQUATION OF TIME:";
1050 LPRINT USING"###.#";E;
1060 LPRINT" MINUTES"

1080 IF E>0 THEN SAY TRANSLATE$("the equation of time iz"): SAY (J$): SAY TRANSLATE$("minuts.")
1090 IF E=0 THEN SAY TRANSLATE$("the equation of time iz zero minuts.")
1110 IF E<0 THEN SAY TRANSLATE$("the equation of time iz mynus"): SAY (J$): SAY TRANSLATE$("minuts.")

1111 CL=COS(LA/J): SD=SIN(D/J): CD=COS(D/J): Y=SD/CL
1120 IF ABS(Y)=>1 THEN PRINT"NO SUNRISE OR SUNSET":
1130 IF ABS(Y)=>1 THEN LPRINT:
1140 IF ABS(Y)=>1 THEN SAY TRANSLATE$("there iz no sunrise or sunset at this location on this date.")
1150 IF ABS(Y)=>1 THEN LPRINT"NO SUNRISE OR SUNSET":END

1151 REM Report Azimuth of sunrise
1160 Z = 90 - J*ATN(Y/SQR(1-Y*Y))

1161 REM This truncates all but tenths decimal for say function
1162 DTEMP=Z*10
1163 II&=CLNG(DTEMP)
1164 DTEMP=II&/10
1200 K$=TRANSLATE$(STR$(DTEMP))

1210 PRINT"AZIMUTH OF SUNRISE:";
1220 PRINT USING"###.#";ABS(Z);
1230 PRINT" DEGREES"
1240 LPRINT"AZIMUTH OF SUNRISE:";
1250 LPRINT USING"###.#";ABS(Z);
1260 LPRINT" DEGREES"
1270 SAY TRANSLATE$("the azimuth of the sun at sunrize iz"):SAY(K$):SAY TRANSLATE$("dagrees")

1280 REM Report Azimuth of sunset
1290 REM This truncates all but tenths decimal for say function
1300 DTEMP=(360-ABS(Z))*10
1310 II&=CLNG(DTEMP)
1320 DTEMP=II&/10
1330 N$=TRANSLATE$(STR$(DTEMP))

1340 PRINT"AZIMUTH OF SUNSET: ";
1350 PRINT USING"####.#";360-ABS(Z);
1360 PRINT" DEGREES"
1370 LPRINT"AZIMUTH OF SUNSET: ";
1380 LPRINT USING"####.#";360-ABS(Z);
1390 LPRINT" DEGREES"
1400 SAY TRANSLATE$("the azimuth of the sun at sunset iz"):SAY(N$):SAY TRANSLATE$("dagrees")

1401 ST=SIN(Z/J)/CD
1402 IF ABS(ST)>=1 THEN T=6:TT=6:GOTO 1420
1403 CT=SQR(1-ST*ST)
1404 T=J/15*ATN(ST/CT)
1410 TT=T
1420 IF D<0 AND LA<90 THEN T=12-T:TT=T
1430 IF D>0 AND LA>90 THEN T=12-T:TT=T
1440 T=T+TD-E/60-.04

1450 GOSUB 1590
1460 LPRINT
1470 COLOR 3,0
1471 PRINT
1472 PRINT

1481 REM Print info to screen
1482 PRINT "TIME OF SUNRISE:";T1$;":";T2$;"am LST  ";GM$;"";T2$;" UTC"

1483 REM Print info to printer
1484 LPRINT "TIME OF SUNRISE:";T1$;":";T2$;"am LST  ";GM$;"";T2$;" UTC"

1485 SAY TRANSLATE$("the time of sunrise is"):SAY TRANSLATE$(T1$): SAY TRANSLATE$(T2$): SAY TRANSLATE$("ay em.  local standard time which is")
1486 SAY TRANSLATE$(GM$): SAY TRANSLATE$(T2$): SAY TRANSLATE$("universal co-ordinated tyme")

1510 T=12-TT:T=T+TD-E/60+.04
1520 CNT=1
1530 GOSUB 1590

1531  REM Print info to screen
1532 PRINT "TIME OF SUNSET: " ;T1$;":";T2$;"pm LST   ";GM$;"";T2$;" UTC"

     REM Print info to printer
1533 LPRINT "TIME OF SUNSET: " ;T1$;":";T2$;"pm LST   ";GM$;"";T2$;" UTC"
1534 SAY TRANSLATE$("the time of sunset is"):SAY TRANSLATE$(T1$): SAY TRANSLATE$(T2$): SAY TRANSLATE$("pee em.   local standard time which is")
1535 SAY TRANSLATE$(GM$): SAY TRANSLATE$(T2$): SAY TRANSLATE$("universal co-ordinated tyme")

1560 COLOR 1,0
1570 LPRINT
1571 PRINT
1572 REM Statements to determine if additional calculations are wanted.
1573 SAY TRANSLATE$("do you want another calculation at this location?")
1574 PRINT "Do you want another calculation at this location? (y OR n)"
1575 INPUT Q$
1576 IF Q$="y" THEN GOTO 530
1577 IF Q$="n" THEN GOTO 1578
1578 SAY TRANSLATE$("do you want a calculation at a different location?")
1579 PRINT "Do you want a calculation at different location? (y or n)"
1580 INPUT R$
1581 IF R$="y" THEN GOTO 160
1582 IF R$="n" THEN GOTO 1852

1583 END

1585 REM This subroutine calculates strings for time values
1590 T1=INT(T):T2=T-T1:T1$=STR$(T1):T2=INT((T2*600+5)/10)
1600 T2$=STR$(T2):T2$=RIGHT$(T2$,LEN(T2$)-1)
1610 IF INT(T2)<10 THEN T2$="0"+T2$
1620 GM = CINT(D2/15) :REM calculate difference between GM and local time
1630 IF CNT = 0 THEN GM = VAL(T1$)+GM :REM GMT for sunrise
1640 IF CNT > 0 THEN GM = VAL(T1$)+12+GM :REM GMT for sunset
1650 IF GM +(VAL(T2$)/60)> 24 THEN GM = GM - 24
1660 GM$ = STR$(GM)
1665 IF (GM < 10) THEN
1666   GM$ = RIGHT$(GM$, 1) : REM String has a leading blank, so strip it off
1667   GM$ = "0"+GM$ : REM add the leading zero if less than ten
1668 END IF
1670 RETURN

1680 REM This subroutine converts DD.MMM input to DD.DDD
1690 DEGTMP = (ABS(D1)-ABS(FIX(D1))) *100/60
1700 D1 = (FIX(ABS(D1))+DEGTMP)*SGN(D1)
1710 DEGTMP = (ABS(D2)-ABS(FIX(D2))) *100/60
1720 D2 = (FIX(ABS(D2))+DEGTMP)*SGN(D2)
1730 RETURN

1731 REM This subroutine takes a number like ##.###, and breaks it up into
1732 REM the degrees (left part) and the minutes (right part)
1733 REM This is a subroutine because we do this twice for lat and long

1740 SUB BREAKINPUT (MYINPUT, LEFTPART, RIGHTPART) STATIC
1750 REM Here we will do the math after multiplying by 1000 to be sure and
1760 REM eliminate any precision error. After we're done, divide by 1000.
1770    BIGINPUT = INT(MYINPUT * 1000)
1780    LEFTPART = FIX(MYINPUT)
1790    LEFTBIG = INT(LEFTPART * 1000)
1800    RIGHTBIG = BIGINPUT - LEFTBIG
1810    RIGHTPART = RIGHTBIG / 1000
1820    REM now move decimal place over for ##.# minutes
1830    RIGHTPART = RIGHTPART * 100
1840 END SUB

1852 LPRINT
1860 LPRINT
1870 END

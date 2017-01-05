FUNCTION PBMAIN
2000 'GOES CURRENT METER MESSAGE TO ENSEMBLE DECODER - P. LIBRARO (757) 635-4550
DIM V AS STRING
V = "6 MINUTE CURRENT METER DECODER 12/11 PJL"
   ON ERROR GOTO 0
DIM HL AS INTEGER
DIM ERCV AS INTEGER
DIM TRUE AS INTEGER
DIM FALSE AS INTEGER
DIM XF AS INTEGER
DIM RWLA AS SINGLE
DIM DCP AS INTEGER
DIM ARGS(1 TO 10) AS STRING
DIM MaxARgs AS INTEGER
DIM SNSRS AS STRING
DIM L AS INTEGER
DIM J AS INTEGER
DIM P AS INTEGER
DIM I AS INTEGER
DIM Q AS INTEGER
DIM T AS STRING
DIM OUTC AS STRING
DIM Y AS STRING
DIM OUTL AS STRING
DIM OUTS AS STRING
DIM OUTLN AS STRING
DIM MESSAGE AS STRING
DIM OLDMES AS STRING
DIM MFS AS STRING
DIM MF AS INTEGER
DIM STACS AS STRING
DIM WLF AS INTEGER
DIM F1 AS INTEGER
DIM HL1 AS LONG
DIM HL2 AS SINGLE
DIM R AS INTEGER
DIM VF AS INTEGER
DIM PARITY AS INTEGER
DIM PARITYS AS STRING
DIM LINES AS STRING
DIM LENST AS INTEGER
DIM Q102 AS INTEGER
DIM X102 AS LONG
DIM Z102 AS INTEGER
DIM Q102S AS STRING
DIM Q104 AS INTEGER
DIM Q104S AS STRING
DIM Y102 AS INTEGER
DIM Y102S AS STRING
DIM SENSOR AS INTEGER
DIM FLA AS INTEGER
DIM FLB AS INTEGER
DIM PLATFORMS AS STRING
DIM RDATES AS STRING
DIM MESLEN AS INTEGER
DIM DOFF AS INTEGER
DIM SOFF AS INTEGER
DIM STATS AS INTEGER
DIM RST AS INTEGER
DIM HL1S AS STRING
DIM HL2S AS STRING
DIM STNS AS STRING
DIM DATAS AS STRING
DIM CHS AS STRING
DIM WLS AS STRING
DIM ZS AS STRING
DIM DVALUE AS INTEGER
DIM BINNS AS STRING
DIM TERMS AS STRING
DIM TTERMS AS STRING
DIM STTERMS AS STRING
DIM OSENSOR AS INTEGER
DIM ONES AS STRING
DIM DOFFSET AS INTEGER
DIM SOFFSET AS INTEGER
DIM TIMEOFF AS INTEGER
DIM PR AS INTEGER
DIM TTWO AS INTEGER
DIM YR AS INTEGER
DIM START AS INTEGER
DIM MONTH AS INTEGER
DIM DAY AS INTEGER
DIM HOUR AS INTEGER
DIM MN AS INTEGER
DIM MNS AS STRING
DIM HOURS AS STRING
DIM DTAGS AS STRING
DIM YRS AS STRING
DIM DAYS AS STRING
DIM YRDAYS AS STRING
DIM ANS AS STRING
DIM ANNS AS STRING
DIM STATIONS AS STRING
DIM SDATES AS STRING
DIM CS AS STRING
DIM WLEN AS INTEGER
DIM BV AS SINGLE
DIM BVF AS INTEGER
DIM OFFSET AS SINGLE
DIM RNWL AS SINGLE
DIM NSAMP AS SINGLE
DIM QS AS STRING
DIM QCS AS STRING
DIM CQS AS STRING
DIM QCQS AS STRING
DIM SMPL AS INTEGER
DIM BTF AS INTEGER
DIM RWL AS SINGLE
DIM WF AS INTEGER
DIM NS(30) AS STRING
DIM NUMARGS AS INTEGER
DIM TIN AS INTEGER
DIM LF AS INTEGER
DIM HL(10) AS INTEGER
DIM HP(10) AS SINGLE
MaxARgs = 10
GOSUB 5000                                'initialize pRogRam
CLS : PRINT V
' Get input file name
' Get what was typed on the command line.
GOSUB 8000
        L = LEN(ARGS(1))
        IF L > 0 THEN GOTO 61
60      INPUT "INPUT FILE NAME"; ARGS(1)
61      OPEN ARGS(1) FOR INPUT AS #1                'OPEN FOR BINARY INPUT
        P = INSTR(ARGS(1), ".")
        IF P > 0 THEN
             T = LEFT$(ARGS(1), P)
        ELSE
             T = ARGS(1) + "."
        END IF
        OUTC = T + "CR": OPEN OUTC FOR OUTPUT AS #4
        WHILE ISFALSE EOF(1)
                MESSAGE = ""
                MESSAGE = INPUT$(1, #1)
                IF MESSAGE = CHR$(10) AND OLDMES <> CHR$(13) THEN
                        PRINT #4, CHR$(13) + CHR$(10);
'                ELSEIF (MESSAGE < CHR$(32) OR MESSAGE = CHR$(127))AND (MESSAGE <> CHR$(13) OR MESSAGE <> CHR$(10)) THEN
 '                       PRINT #4, CHR$(36)
                ELSE
                        PRINT #4, MESSAGE;
                END IF
                OLDMES = MESSAGE
        WEND
        PRINT #4,
        CLOSE #1
        CLOSE #4
        OPEN OUTC FOR INPUT AS #4
        PRINT
65    OUTL = T + "TXT": OPEN OUTL FOR OUTPUT AS #2
   MF = 1
   MFS = T + "TMP"
   OPEN T + "ERR" FOR OUTPUT AS #3
   '
   '======================================================MAIN PROGRAM LOOP
   WHILE NOT EOF(4)
    PARITY = 0
     LINE INPUT #4, LINES
     IF LEN(LINES) < 8 THEN GOTO 2250
     BINNS = MID$(LINES, 171,1)
     STACS = MID$(LINES, 41, 8)
     OPEN MFS FOR OUTPUT AS #1
     PRINT #1, LINES: CLOSE #1
     OPEN MFS FOR INPUT AS #1                   'LEN=1
     LENST = LOF(#1) - 50'length of input according to DOS
      Q104 = 8: GOSUB 2600: IF FLA = 0 THEN PLATFORMS = Q104S'get platform ID
      Q104 = 2: GOSUB 2600: FL1$ = Q104S
      Q104 = 9: GOSUB 2600: DATETIME$ = Q104S           'get date/time mesg
      Q104 = 1: GOSUB 2600: PARITYS = Q104S             'get parity byte
      IF PARITYS = "?" THEN PRINT #3, LINES' = "": goto 2250
      GOSUB 3000
      Q104 = 2: GOSUB 2600: FL2$ = Q104S                 'skip space
      Q104 = 5: GOSUB 2600: ML$ = Q104S: MESLEN = VAL(ML$)'GET STRING LENGTH
      Q104 = 1: GOSUB 2600                  'READ " OR SPACE
      Q104 = 2: GOSUB 2600: HEAD$ = Q104S             'READ CS, CR, or CA
      R = INSTR(1,STACS,"-")
      IF R <> 0 THEN
          P = R-1
      ELSE
          P = 8
      END IF
      Q104 = P: GOSUB 2600: STAT$ = Q104S             'get station ID
      STATIONS = STAT$
'     PRINT #2, R
    IF HEAD$ = "CR" THEN
      PRINT #2, STATIONS
      FOR J = 2 TO LENST STEP 2
        Q102 = 2: GOSUB 2400
        NUMS$ = HEX$(X102)
        HEXOUT$ = "000"     'PAD LEADING HEX OUTPUT WITH ZEROS
        PRINT #2, RIGHT$(HEXOUT$, (3 - LEN(NUMS$))); NUMS$;
        PRINT RIGHT$(HEXOUT$, (3 - LEN(NUMS$))); NUMS$
     NEXT J
      PRINT #2,
      PRINT #2,
   GOTO 2177
ELSEIF HEAD$ = "CG" THEN
      PRINT #2, STATIONS
      Q104 = 1: GOSUB 2600 'HEADER
'2014 07 17 07 49 39      0      0     -1 255 255   0   0   0   0   0  104  -7   4   0   0   0   2192       3888     3  62  65   65  31  29  23  Input
'  12 01 16 26 51 00000000 00110001  11.8 1486.8 132.5  -5.4   0.5   2.022  10.18     0 14868  OUTPUT
       IF Q104S = "-" THEN
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        PRINT #2, USING$ ("#######", X102);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        PRINT #2, USING$ ("#######", X102);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500: GOSUB 2700
        PRINT #2, USING$ ("#######", X102);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("#####", HL1);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500: GOSUB 2700
        HL1 = X102
        PRINT #2, USING$ ("####", HL1);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500: GOSUB 2700
        HL1 = X102
        PRINT #2, USING$ ("####", HL1);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1);
        Q104 = 3: GOSUB 2600: HL1S = Q104S
        Q102 = 3: Q102S = HL1S: GOSUB 2500: GOSUB 2800
        PRINT #2, USING$ ("#######", X102);
        Q104 = 3: GOSUB 2600: HL1S = Q104S
        Q102 = 3: Q102S = HL1S: GOSUB 2500: GOSUB 2800
        PRINT #2, "    ";
        PRINT #2, USING$ ("#######", X102);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("######", HL1);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("#####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ ("####", HL1)
     END IF
     WHILE NOT EOF(1)
' 1    577  -2388  10  14  49  29
     Q104 = 1: GOSUB 2600 'BINS
      IF Q104S = "+" THEN
        Q104 = 1: GOSUB 2600: HL1S = Q104S
        Q102 = 1: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        FOR Q = 1 TO 2
                Q104 = 3: GOSUB 2600: Q102S = Q104S
                Q102 = 3: GOSUB 2500: GOSUB 2800
                HL(Q) = X102
        NEXT Q
        FOR Q = 3 TO 6
                Q104 = 2: GOSUB 2600: Q102S = Q104S
                Q102 = 2: GOSUB 2500: GOSUB 2700
                HL(Q) = X102
        NEXT Q
     PRINT #2, USING$ ("##", HL1);
     PRINT #2, USING$ ("#######", HL(1)); : PRINT #2, USING$ ("#######", HL(2));
     PRINT USING$ ("###", HL1);
     PRINT USING$ ("#######", HL(1)); : PRINT USING$ ("#######", HL(2));
     FOR Q = 3 TO 6
       PRINT #2, USING$ ("####", HL(Q));
       PRINT USING$ ("####", HL(Q));
     NEXT Q
     PRINT #2,
     PRINT
     END IF
    WEND
    PRINT #2,
    GOTO 2177
      ELSEIF HEAD$ = "CA" THEN
      PRINT #2, STATIONS
      Q104 = 1: GOSUB 2600 'HEADER
'  12 01 2011 16 26 51 00000000 00110001  11.8 1486.8 132.5  -5.4   0.5   2.022  10.18     0 14868  INPUT
'  12 01 2011 16 26 51 00000000 00110001  11.8 1486.8 132.5  -5.4   0.5   2.022  10.18     0 14868  OUTPUT
       IF Q104S = "-" THEN
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ (" ####", HL1) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0##", HL1),2) ;
        Q104 = 3: GOSUB 2600: HL1S = Q104S
        Q102 = 3: Q102S = HL1S: GOSUB 2500
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0########", X102),8);
        Q104 = 3: GOSUB 2600: HL1S = Q104S
        Q102 = 3: Q102S = HL1S: GOSUB 2500
        PRINT #2, " ";
        PRINT #2, RIGHT$(USING$ ("*0########", X102),8);
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL2 = X102 * 0.1
        PRINT #2, USING$ (" ###.#", HL2) ;
        Q104 = 3: GOSUB 2600: HL1S = Q104S
        Q102 = 3: Q102S = HL1S: GOSUB 2500
        HL2 = X102 * 0.1
        PRINT #2, USING$ (" ####.#", HL2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL2 = X102 * 0.1
        PRINT #2, USING$ (" ###.#", HL2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500: GOSUB 2700
        HL2 = X102 * 0.1
        PRINT #2, USING$ ("####.#", HL2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500: GOSUB 2700
        HL2 = X102 * 0.1
        PRINT #2, USING$ (" ###.#", HL2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL2 = X102 * 0.001
        PRINT #2, USING$ (" ###.###", HL2) ;
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL2 = X102 * 0.01
        PRINT #2, USING$ (" ###.##", HL2) ;
        Q104 = 3: GOSUB 2600: HL1S = Q104S
        Q102 = 3: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ (" #####", HL1);
        Q104 = 3: GOSUB 2600: HL1S = Q104S
        Q102 = 3: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        PRINT #2, USING$ (" #####", HL1)
     END IF
     WHILE NOT EOF(1)
     Q104 = 1: GOSUB 2600 'BINS
     IF Q104S = "+" THEN
        FOR Q = 1 TO 3
                Q104 = 3: GOSUB 2600: Q102S = Q104S
                Q102 = 3: GOSUB 2500: GOSUB 2800
                HP(Q) = X102 *0.001
        NEXT Q
        PRINT #2, USING$ ("####.###", HP(1)); : PRINT #2, USING$ ("####.###", HP(2));  : PRINT #2, USING$ ("####.###", HP(3));
        PRINT USING$ ("####.###", HP(1)); : PRINT USING$ ("####.###", HP(2));  : PRINT USING$ ("####.###", HP(3));
        FOR Q = 1 TO 3
                Q104 = 2: GOSUB 2600: Q102S = Q104S
                Q102 = 2: GOSUB 2500
                HL(Q) = X102
        NEXT Q
     PRINT #2, USING$ ("####", HL(1)); : PRINT #2, ".0";: PRINT #2, USING$ ("####", HL(2));  : PRINT #2, ".0";:PRINT #2, USING$ ("####", HL(3));: PRINT #2, ".0"
     PRINT USING$ ("####", HL(1)); : PRINT ".0";: PRINT USING$ ("####", HL(2));  : PRINT ".0";:PRINT USING$ ("####", HL(3));: PRINT ".0" ;
     PRINT
    END IF
    WEND
    PRINT #2,
   GOTO 2177
  ELSEIF HEAD$ = "CS" THEN
'      PRINT #2, DATETIME$
      PRINT #2, STATIONS
      Q104 = 1: GOSUB 2600 'HEADER 1
      IF Q104S = "-" THEN
        Q104 = 2: GOSUB 2600: HL1S = Q104S
        Q102 = 2: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        Q104 = 5: GOSUB 2600: HL2S = Q104S
        FOR Q = 1 TO 11
                Q104 = 2: GOSUB 2600: Q102S = Q104S
                Q102 = 2: GOSUB 2500
                HL(Q) = X102
        NEXT Q
'        PRINT #2, STATIONS
        PRINT #2, USING$ ("####", HL1) ;
        PRINT #2, "       ";
        PRINT #2, USING$ ("&####", HL2S);
        PRINT #2, "       ";
        PRINT #2, USING$ ("#####", HL(1));
        PRINT #2, USING$ ("#####", HL(2));
        PRINT USING$ ("####", HL1);
        PRINT "       ";
        PRINT USING$ ("&####", HL2S);
        PRINT "         ";
        PRINT USING$ ("#####", HL(1));
        PRINT USING$ ("#####", HL(2));
        FOR Q = 3 TO 7
                PRINT #2, USING$ ("###", HL(Q));
                PRINT USING$ ("###", HL(Q));
        NEXT Q
        FOR Q = 8 TO 11
                PRINT #2, USING$ ("##", HL(Q));
                PRINT USING$ ("##", HL(Q));
        NEXT Q
        PRINT #2,
        PRINT
     ELSE
        GOTO 2177
     END IF
     Q104 = 1: GOSUB 2600  'HEADER 2
     IF Q104S = "-" THEN
        FOR Q = 1 TO 5
                Q104 = 2: GOSUB 2600: HL1S = Q104S
                Q102 = 2: Q102S = HL1S: GOSUB 2500
                HL(Q) = X102
        NEXT Q
        FOR Q = 1 TO 5
                PRINT #2, USING$ ("######", HL(Q));
                PRINT USING$ ("######", HL(Q));
        NEXT Q
        PRINT #2,
        PRINT
     END IF
     Q104 = 1: GOSUB 2600
     IF Q104S = "-" THEN   'HEADER 3
                Q104 = 2: GOSUB 2600: Q102S = Q104S
                Q102 = 2: GOSUB 2500
                HL(1) = X102
        FOR Q = 2 TO 3
                Q104 = 2: GOSUB 2600: Q102S = Q104S
                Q102 = 2: GOSUB 2500: GOSUB 2700
                HL(Q) = X102
        NEXT Q
        FOR Q = 4 TO 5
                Q104 = 3: GOSUB 2600: Q102S = Q104S
                Q102 = 3: GOSUB 2500
                HL(Q) = X102
        NEXT Q
        FOR Q = 6 TO 8
                Q104 = 2: GOSUB 2600: Q102S = Q104S
                Q102 = 2: GOSUB 2500
                HL(Q) = X102
        NEXT Q
        FOR Q = 9 TO 11
                Q104 = 3: GOSUB 2600: Q102S = Q104S
                Q102 = 3: GOSUB 2500
                HL(Q) = X102
        NEXT Q
        FOR Q = 1 TO 11
                PRINT #2, USING$ ("#######", HL(Q));
                PRINT USING$ ("#######", HL(Q));
        NEXT Q
        PRINT #2,
        PRINT
     END IF
     Q104 = 1: GOSUB 2600  'HEADER 4
     IF Q104S = "-" THEN
        FOR Q = 1 TO 16
                Q104 = 2: GOSUB 2600: Q102S = Q104S
                Q102 = 2: GOSUB 2500
                HL(Q) = X102
        NEXT Q
        FOR Q = 1 TO 16
                PRINT #2, USING$ ("####", HL(Q));
                PRINT USING$ ("####", HL(Q));
        NEXT Q
        PRINT #2,
        PRINT
      END IF
     WHILE NOT EOF(1)
  IF BINNS <> "+"  THEN
      Q104 = 1: GOSUB 2600 'BINS
      IF Q104S = "+" THEN
        Q104 = 1: GOSUB 2600: HL1S = Q104S
        Q102 = 1: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        FOR Q = 1 TO 6
                Q104 = 2: GOSUB 2600: Q102S = Q104S
                Q102 = 2: GOSUB 2500: GOSUB 2700
                HL(Q) = X102
        NEXT Q
     PRINT #2, USING$ ("###", HL1);
     PRINT #2, USING$ ("#######", HL(1)); : PRINT #2, USING$ ("#######", HL(2));
     PRINT USING$ ("###", HL1);
     PRINT USING$ ("#######", HL(1)); : PRINT USING$ ("#######", HL(2));
     FOR Q = 3 TO 6
       PRINT #2, USING$ ("####", HL(Q));
       PRINT USING$ ("####", HL(Q));
     NEXT Q
     PRINT #2,
     PRINT
     END IF
   ELSEIF BINNS = "+" THEN
    Q104 = 1: GOSUB 2600 'BINS
      IF Q104S = "+" THEN
        Q104 = 1: GOSUB 2600: HL1S = Q104S
        Q102 = 1: Q102S = HL1S: GOSUB 2500
        HL1 = X102
        FOR Q = 1 TO 9
                Q104 = 2: GOSUB 2600: Q102S = Q104S
                Q102 = 2: GOSUB 2500: GOSUB 2700
                HL(Q) = X102
        NEXT Q
     PRINT #2, USING$ ("###", HL1);
     PRINT #2, USING$ ("#######", HL(1)); : PRINT #2, USING$ ("#######", HL(2)); : PRINT #2, USING$ ("#######", HL(3));
     PRINT USING$ ("###", HL1);
     PRINT USING$ ("#######", HL(1)); : PRINT USING$ ("#######", HL(2)); : PRINT USING$ ("#######", HL(3));
     FOR Q = 4 TO 9
       PRINT #2, USING$ ("####", HL(Q));
       PRINT USING$ ("####", HL(Q));
     NEXT Q
     PRINT #2,
     PRINT
     END IF
    END IF
      WEND
    PRINT #2,
    GOTO 2177
   ELSE
        GOSUB 3100
2250 END IF
    CLOSE #1
2177 WEND
     CLOSE
     SHELL "DEL *.TMP"
     END
2300 '================================Get Next Character (Skip CR,LF)**
      CHS = ""
      CHS = INPUT$(1, #1)
      IF CHS = ZS THEN PARITY = PARITY + 1
      RETURN
      '
2400 '=================Get Sutron Number String and Convert to a BASIC Number
      ERCV = 0:  Q102S = "":  Z102 = Q102
      WHILE NOT EOF(1) AND Z102 > 0
        GOSUB 2300: IF CHS < "?" THEN 2450
        Q102S = Q102S + CHS: Z102 = Z102 - 1
      WEND
      GOTO 2500
2450 ERCV = 1
      RETURN
      '
2500 '================================Convert Sutron Number to a BASIC Number
      X102 = 0
      IF PARITY > 2 THEN RETURN
      FOR I = 1 TO Q102
       Y102S = MID$(Q102S, I, 1)
       IF Y102S = "$" OR Y102S = "" THEN 2550'"$" is parity error byte
       Y102 = ASC(Y102S)
       IF Y102 = 63 THEN Y102 = 127           '"?" is actually DEL
       X102 = X102 * 64 + (Y102 - 64)
      NEXT I
      IF X102 < 0 THEN X102 = DVALUE
      RETURN
2550 PARITY = PARITY + 1
      X102 = DVALUE
'      if parity > 5 then goto 3100
      RETURN
      '
2600 '=================================================Get Character String
      Q104S = ""
      WHILE NOT EOF(1) AND Q104 > 0: GOSUB 2300: Q104S = Q104S + CHS: Q104 = Q104 - 1: WEND
      RETURN
      '
2700 '===============================================Convert 2's Complement
      IF X102 = 2047 THEN X102 = DVALUE
      IF X102 >= 2048 AND X102 <> DVALUE THEN X102 = X102 - 4096
      RETURN
       '
2800 '===============================================Convert 2's Complement 3 byte
      IF X102 >= 131072 AND X102 <> DVALUE THEN X102 = X102 - 262144
      RETURN
      '
  3000 '========================================Get and File Signal Strength
      Q104 = 10: GOSUB 2600
      STNS = Q104S
      RETURN

3100 '==========================================Dump Block to Error File
      Q104 = (MESLEN - 12): GOSUB 2600
      PRINT #3, PLATFORMS + FL1$ + DATETIME$ + PARITYS + STNS + FL2$ + ML$ + STATIONS + Q104S
      PARITY = 0
      RETURN
      '
5000 '==================================================INITIALIZE PROGRAM**
      FLA = 0: START = 0
      TEST$ = "DataDATAMISSPDT TR TeUNEXMESS": DATAS = ""
      DIGIT$ = "0123456789"
      STX$ = CHR$(2): ETX$ = CHR$(3): CR$ = CHR$(13)
      ZS = CHR$(26): PAR$= CHR$(36): TM$ = "0": PR = 0: R = 0
      TERMS = STX$ + " " + CR$ + CHR$(10) + CHR$(20)              'terminators
      TERMS = "0" + TERMS                        'time tag + terminators
      SNSRS = "123456789:;<=>!()" + CHR$(34) + CHR$(46) + CHR$(37) + CHR$(38) + CHR$(39) + CHR$(42)'field unit sensor IDs
      'SNSR.TT.TERMS = "H" + "+"
      WLF = 0
      CS = CHR$(44): ANNS = "99999999999999999": STATIONS = "99999999": XF = 0
      DVALUE = 9999
      'FLG = 0: BWF = 0: CF = 0: WF = 0: PS = ".": EM = 1000: SEF = 0: SES = ""
      QS = CHR$(34): QCS = QS + CS: CQS = CS + QS: QCQS = QCS + QS
      RETURN
8000 '======================================GET FILEMAMES FROM COMMAND LINE
TRUE = -1
FALSE = 0
   NUMARGS = 0: TIN = FALSE
' Get the command line USING the COMMANDS function.
   Cl$ = COMMAND$
   L = LEN(Cl$)
   IF L = 0 THEN GOTO 60
' Go through the command line a character at a time.
   FOR I = 1 TO L
      D$ = MID$(Cl$, I, 1)
    'Test for character being a blank or a tab.
      IF (D$ <> " " AND D$ <> CHR$(9)) THEN
    ' Neither blank nor tab.
    ' Test to see if you're already inside an argument.
         IF NOT TIN THEN
      ' You've found the start of a new argument.
      ' Test for too many arguments.
            IF NUMARGS = MaxArgs THEN EXIT FOR
            NUMARGS = NUMARGS + 1
            TIN = TRUE
         END IF
     ' Add the character to the current argument.
         ARGS$(NUMARGS) = ARGS$(NUMARGS) + D$
      ELSE
   ' Found a blank or a tab.
   ' Set "Not in an argument" flag to FALSE.
         TIN = FALSE
      END IF
   NEXT I
RETURN
'       THAT'S ALL, FOLKS!
END FUNCTION

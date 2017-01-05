'=============================================================================
'
'  XPERTDPAS : Convert an XPERT ASCII text file dump to
'            a DPAS compatible XXX.QC file
'
'=============================================================================



#COMPILE EXE
#DIM ALL
GLOBAL astring AS STRING
GLOBAL bstring AS STRING
GLOBAL cstring AS STRING
GLOBAL dstring AS STRING
GLOBAL rstring AS STRING
GLOBAL dstring2 AS STRING
GLOBAL dstring3 AS STRING
GLOBAL dstringa AS STRING
GLOBAL dstringb AS STRING
GLOBAL dstringc AS STRING
GLOBAL b%
GLOBAL f AS INTEGER
GLOBAL ft AS INTEGER
GLOBAL fr AS INTEGER
GLOBAL fb AS INTEGER
GLOBAL fw1 AS INTEGER
GLOBAL fw2 AS INTEGER
GLOBAL p2 AS INTEGER
GLOBAL DAY AS STRING
GLOBAL DDATE AS STRING
GLOBAL oday AS STRING
GLOBAL otime AS STRING
GLOBAL MON AS STRING
GLOBAL MONTH AS STRING
GLOBAL YR AS STRING
GLOBAL YEAR AS STRING
GLOBAL HR AS STRING
GLOBAL MN AS STRING
GLOBAL m1 AS STRING
GLOBAL m2 AS STRING
GLOBAL m3 AS STRING
GLOBAL m4 AS STRING
GLOBAL m5 AS STRING
GLOBAL ix AS LONG
GLOBAL sFileName AS STRING
GLOBAL sFile2 AS STRING
GLOBAL sText AS STRING
GLOBAL date AS STRING
GLOBAL time AS STRING
GLOBAL gtime AS STRING
GLOBAL sensor AS STRING
GLOBAL value AS EXT
GLOBAL label AS STRING
GLOBAL QUALITY AS STRING
GLOBAL vl AS LONG
GLOBAL v2 AS INTEGER
GLOBAL f1 AS EXT
GLOBAL J AS INTEGER


SUB DPASDATE ()
DIM I AS INTEGER
MONTH = MID$(DATE,1,2)
DAY = MID$(DATE,4,2)
YEAR = MID$(DATE,7,4)
HR = MID$(DATE,12,3)
MN = MID$(DATE,15,2)
I = VAL(MN)
IF MONTH = "01" THEN MON = "JAN ": GOTO 25
IF MONTH = "02" THEN MON = "FEB ": GOTO 25
IF MONTH = "03" THEN MON = "MAR ": GOTO 25
IF MONTH = "04" THEN MON = "APR ": GOTO 25
IF MONTH = "05" THEN MON = "MAY ": GOTO 25
IF MONTH = "06" THEN MON = "JUN ": GOTO 25
IF MONTH = "07" THEN MON = "JUL ": GOTO 25
IF MONTH = "08" THEN MON = "AUG ": GOTO 25
IF MONTH = "09" THEN MON = "SEP ": GOTO 25
IF MONTH = "10" THEN MON = "OCT ": GOTO 25
IF MONTH = "11" THEN MON = "NOV ": GOTO 25
IF MONTH = "12" THEN MON = "DEC ": GOTO 25
25 IF ft = 1 THEN GOTO 30
SELECT CASE I
    CASE 0 TO 5
        MN = "00"
    CASE 6 TO 11
        MN = "06"
    CASE 12 TO 17
        MN = "12"
    CASE 16 TO 21
        MN = "18"
    CASE 24 TO 29
        MN = "24"
    CASE 30 TO 35
        MN = "30"
    CASE 36 TO 41
        MN = "36"
    CASE 42 TO 47
        MN = "42"
    CASE 48 TO 53
        MN = "48"
    CASE 54 TO 59
        MN = "54"
    CASE ELSE
        MN = "00"
END SELECT
30 ddate = MON + DAY + " " +  YEAR + " " + HR + MN
ft = 0
PRINT ddate, m1
END SUB
'-----------------------------------------------------------------------------
' Main application entry point...
'
FUNCTION PBMAIN()AS LONG
    'CONSOLE SCREEN 26, 80 ' 26 rows by 80 columns
    COLOR 14, 1, 1: CLS
    sFile2 = "9999"
    rstring = "12345671  XXX XX XXXX XX:XX XX D P 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999"
    bstring = "12345671  XXX XX XXXX XX:XX XX D P 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999"
    cstring = "12345671  XXX XX XXXX XX:XX XX D P 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999"
    astring = "XXXXXXXXXXX XX XXXX XX:XXXXXXXXXX               D
    dstring = bstring                       'main primary water data string
    dstring2 = rstring                      'secondary water data string
    dstring3 = cstring                      'backup data string
    dstringa = astring                      'primary wind string
    dstringb = astring                      'secondary wind string
    dstringc = astring                      'other ancillary data string
    sFileName = LTRIM$(COMMAND$,ANY "\/")
    sFileName = DIR$(sFileName, 7)
    IF ASC(sFileName, 1) = 34 THEN
        sFileName = MID$(sFileName, 2)
        ix = INSTR(sFileName, $DQ)
        IF ix THEN
            sFileName = LEFT$(sFileName, ix - 1)
        END IF
    END IF
        b = INSTR(sFileName, ANY ".")
        sFile2 = LEFT$(sFileName,b - 1)
        f = 0
        fb = 0
        fr = 0
        p2 = 0


    IF LEN(sFileName) = 0 OR sFileName = "/?" OR sFileName = "-?" THEN
        EXIT FUNCTION
    END IF


    IF LEN(DIR$(sFileName)) = 0 THEN
        STDERR "Can't find file " + sFileName
        EXIT FUNCTION
    END IF


    OPEN sFileName  FOR INPUT AS #1
    OPEN "xxx.qc" FOR APPEND AS #3
    OPEN "xxx.anc" FOR APPEND AS #4
      WHILE ISFALSE EOF(1)
      stext = ""
       J = VAL(MID$(sfile2,8,1))                            'J is DCP number
       MID$(dstring,1,8) = sfile2
       MID$(dstring2,1,8) = sfile2
       MID$(dstring3,1,8) = sfile2
        IF J = 2 THEN
       MID$(dstring2,8,1) = "3"
       ELSEIF J = 1 AND p2 = 1 THEN
       MID$(dstring2,8,1) = "1"
       MID$(dstring3,8,1) = "2"
       ELSEIF J = 1 AND p2 = 0 THEN
       MID$(dstring2,8,1) = "2"
       MID$(dstring3,8,1) = "2"
       ELSEIF J = 3 OR J = 4 THEN
           MID$(sfile2,8,1) = "3"
           MID$(dstring,8,1) = "3"
           MID$(dstring2,8,1) = "4"
           MID$(dstring3,8,1) = "4"
       END IF
       MID$(dstringa,1,8) = sfile2
       MID$(dstringb,1,8) = sfile2
       IF J > 2 THEN
            MID$(dstringb,8,1) = "4"
       ELSE
            MID$(dstringb,8,1) = "2"
       END IF
   OPEN stext FOR INPUT AS #2
        INPUT #1, date
        IF LEN (date) = 10 THEN
            INPUT #1, time
            date = date + " " + time
         END IF
         INPUT #1, sensor, value, label, QUALITY
        SELECT CASE SENSOR
        CASE = "SNS","SSNS"
           vl = 999999
           vl = INT(value * 1000)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstring,113,6)= m2
           IF fr THEN
            MID$(dstring2,113,6)= m2
           END IF
        CASE = "DAT","SDAT"
           vl = 999999
           vl = CLNG(value * 1000)
           m2 = SPACE$(7)
           RSET m2 = STR$(vl)
           MID$(dstring,105,7)= m2
           IF fr THEN
             MID$(dstring2,105,7)= m2
           END IF
           ODAY = MID$(DATE,4,2)
           otime = MID$(time,1,5)
           IF f THEN
              PRINT #3, dstring
              dstring = bstring
           END IF
           IF fr THEN
                PRINT #3, dstring2
                dstring2 = rstring
           END IF
           IF fb THEN
                PRINT #3, dstring3
                dstring3 = cstring
           END IF
           IF fw1 THEN
              IF gtime = date THEN
               MID$(dstringa,40,6)= m5
              ELSE
               MID$(dstringa,40,6)= m4
              END IF
               PRINT #4, dstringa
               fw1 = 0
               dstringa = astring
           END IF
           IF fw2 THEN
               PRINT #4, dstringb
               dstringb = astring
           END IF
           f = 0
           fr = 0
           fb = 0
           fw1 = 0
           fw2 = 0
        CASE = "WG","GST","PWG","SPWG"
           gtime = date
           m5 = SPACE$ (6)
           m5 = m4
           vl = 99999
           vl = INT(value*10)
           m4 = SPACE$(6)
           RSET m4 = STR$(vl)
        CASE = "SAE"
           IF label <> "B" AND QUALITY <> "B" THEN
           f=1
           DPASDATE
           vl = 999999
           vl = INT(value * 1000)
           MID$(dstring,11,17) = ddate
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
                MID$(dstring,43,6)= m2
           MID$(dstring,29,2)= "V1"
           END IF
        CASE = "BSAE","RSAE"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           fr = 1
           vl = 999999
           vl = INT(value * 1000)
           MID$(dstring2,11,17) = ddate
            m2 = SPACE$(6)
           RSET m2 = STR$(vl)
               MID$(dstring2,43,6)= m2
           MID$(dstring2,29,2)= "V1"
           END IF
        CASE = "MWWL"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           f = 1
           vl = 999999
           vl = INT(value * 1000)
           MID$(dstring,11,17) = ddate
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
                MID$(dstring,43,6)= m2
           MID$(dstring,29,2)= "Y1"
           END IF
        CASE = "PARO1","PAROS1"
           IF label <> "B" AND QUALITY <> "B" THEN
           f=1
           DPASDATE
           vl = 999999
           vl = INT(value * 1000)
           MID$(dstring,11,17) = ddate
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
                MID$(dstring,43,6)= m2
           MID$(dstring,29,2)= "N1"
           END IF
        CASE = "PARO2","PAROS2"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           fr = 1
           p2 = 1
           vl = 999999
           vl = INT(value * 1000)
           MID$(dstring2,11,17) = ddate
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
                MID$(dstring2,43,6)= m2
           MID$(dstring2,29,2)= "T1"
           END IF
        CASE = "ULSAVG"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           fr = 1
           p2 = 1
           vl = 999999
           vl = INT(value * 1000)
           MID$(dstring2,11,17) = ddate
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
                MID$(dstring2,43,6)= m2
           MID$(dstring2,29,2)= "Q2"
           END IF
        CASE = "AGWL"
           IF label <> "B" AND QUALITY <> "B" THEN
           f=1
          DPASDATE
           vl = 999999
           vl = INT(value * 1000)
           MID$(dstring,11,17) = ddate
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
                MID$(dstring,43,6)= m2
           MID$(dstring,29,2)= "Q1"
           END IF
        CASE = "PWL","SPWL"
           IF label <> "B" AND QUALITY <> "B" THEN
           f=1
          DPASDATE
           vl = 999999
           vl = INT(value * 1000)
           MID$(dstring,11,17) = ddate
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
                MID$(dstring,43,6)= m2
           MID$(dstring,29,2)= "A1"
           END IF
         CASE = "STD","SIG","PSIG","P1STD","P1SIG","SPSIG","AGSTD","MWSTD"
           IF label <> "B" AND QUALITY <> "B" THEN
           vl = 999999
           vl = INT(value * 1000)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstring,50,6)= m2
           END IF
          CASE = "OUT","POUT","P1OUT","SPOUT","AGOUT","MWOUT"
           IF label <> "B" AND QUALITY <> "B" THEN
           vl = 999999
           vl = INT(value)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
              MID$(dstring,57,6)= m2
           END IF
         CASE = "BWL","SBWL"
           IF label <> "B" AND QUALITY <> "B" THEN
           fb = 1
           vl = 999999
           vl = INT(value*1000)
           MID$(dstring3,11,17) = ddate
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstring3,43,6)= m2
           MID$(dstring3,29,2)= "B1"
           END IF
         CASE = "BSIG","SBSIG"
           IF label <> "B" AND QUALITY <> "B" THEN
           vl = 999999
           vl = INT(value*1000)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
            MID$(dstring3,50,6)= m2
           END IF
         CASE = "RSTD","RSIG","P2STD","P2SIG","ULSSTD"
           IF label <> "B" AND QUALITY <> "B" THEN
           vl = 999999
           vl = INT(value*1000)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
            MID$(dstring2,50,6)= m2
            END IF
         CASE = "ROUT","P2OUT","ULSOUT"
           IF label <> "B" AND QUALITY <> "B" THEN
           vl = 999999
           vl = INT(value)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
            MID$(dstring2,57,6)= m2
            END IF
         CASE = "BOUT","SBOUT"
           IF label <> "B" AND QUALITY <> "B" THEN
           vl = 999999
           vl = INT(value)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
            MID$(dstring3,57,6)= m2
            END IF
        CASE = "AQT1","SAQT1"
           vl = 999999
           vl = INT(value*10)
           IF label <> "B" AND QUALITY <> "B" THEN
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstring,92,6)= m2
           END IF
        CASE = "AQT2","SAQT2"
           vl = 999999
           vl = INT(value*10)
           IF label <> "B" AND QUALITY <> "B" THEN
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstring,99,6)= m2
           END IF
        CASE = "WS", "PWS","SPWS"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringa,9,17) = ddate
           MID$(dstringa,26,2)= "C1"
           fw1 = 1
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringa,28,6)= m2
           END IF
        CASE = "WD", "PWD","SPWD"
           vl = 999999
           vl = INT(value)
           IF label <> "B" AND QUALITY <> "B" THEN
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringa,34,6)= m2
           END IF
        CASE = "BWS", "RWS","SRWS"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringb,9,17) = ddate
           MID$(dstringb,26,2)= "C1"
           fw2 = 1
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringb,28,6)= m2
           END IF
        CASE = "BWD", "RWD","SRWD"
           vl = 999999
           vl = INT(value)
           IF label <> "B" AND QUALITY <> "B" THEN
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringb,34,6)= m2
           END IF
        CASE = "BWG","BGST","RWG","RGST","SRWG"
           vl = 999999
           vl = INT(value*10)
           IF label <> "B" AND QUALITY <> "B" THEN
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringb,40,6)= m2
           END IF
        CASE = "AT","PAT","SAT"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringc,1,8) = sfile2
           MID$(dstringc,9,17) = ddate
           MID$(dstringc,26,2)= "D1"
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringc,28,6)= m2
           PRINT #4, dstringc
           dstringc = astring
           END IF
        CASE = "WT","PWT","CTWT","SWT"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringc,1,8) = sfile2
           MID$(dstringc,9,17) = ddate
           MID$(dstringc,26,2)= "E1"
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringc,28,6)= m2
           PRINT #4, dstringc
           dstringc = astring
           END IF
        CASE = "BP","PBP","BARO","SBP"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringc,1,8) = sfile2
           MID$(dstringc,9,17) = ddate
           MID$(dstringc,26,2)= "F1"
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringc,28,6)= m2
           PRINT #4, dstringc
           dstringc = astring
           END IF
        CASE = "COND"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringc,1,8) = sfile2
           MID$(dstringc,9,17) = ddate
           MID$(dstringc,26,2)= "G1"
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringc,28,6)= m2
           PRINT #4, dstringc
           dstringc = astring
           END IF
        CASE = "TP","HP","NPR","STP"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringc,1,8) = sfile2
           MID$(dstringc,9,17) = ddate
           MID$(dstringc,8,1) = "2"
           MID$(dstringc,26,2)= "M1"
           vl = 999999
           vl = INT(value)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringc,28,6)= m2
           PRINT #4, dstringc
           dstringc = astring
           END IF
         CASE = "RH"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringc,1,8) = sfile2
           MID$(dstringc,9,17) = ddate
           MID$(dstringc,26,2)= "R1"
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringc,28,6)= m2
           PRINT #4, dstringc
           dstringc = astring
           END IF
         CASE = "RAIN"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringc,1,8) = sfile2
           MID$(dstringc,9,17) = ddate
           MID$(dstringc,26,2)= "J1"
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringc,28,6)= m2
           PRINT #4, dstringc
           dstringc = astring
           END IF
      CASE = "BAT","SBAT"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringc,1,8) = sfile2
           MID$(dstringc,9,17) = ddate
           MID$(dstringc,26,2)= "L1"
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringc,28,6)= m2
           PRINT #4, dstringc
           dstringc = astring
           END IF
      CASE = "SBAT","BBAT","SBBAT"
           IF label <> "B" AND QUALITY <> "B" THEN
           DPASDATE
           MID$(dstringc,1,8) = sfile2
           MID$(dstringc,9,17) = ddate
      IF J > 2 THEN
           MID$(dstringc,8,1) = "4"
      ELSE
           MID$(dstringc,8,1) = "2"
      END IF
           MID$(dstringc,26,2)= "L1"
           vl = 999999
           vl = INT(value*10)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstringc,28,6)= m2
           PRINT #4, dstringc
           dstringc = astring
           END IF
       CASE = "TWL"
           IF label <> "B" AND QUALITY <> "B" THEN
           ft = 1
           DPASDATE
           MID$(dstring,1,8) = sfile2
           MID$(dstring,11,17) = ddate
           MID$(dstring,29,2)= "U1"
           vl = 999999
           vl = INT(value*1000)
           m2 = SPACE$(6)
           RSET m2 = STR$(vl)
           MID$(dstring,43,6)= m2
           PRINT #3, dstring
           dstring = bstring
           END IF
  END SELECT
       WEND
    CLOSE
END FUNCTION

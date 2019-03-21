'----- ADCP Message Formatter -----------------------------------------------
'
'     This basic code is the self timed formatter for Sontec ADCP'S
'     In this case, we capture RECENT.DAT snd store to a working output.dat.
'     The GOES message is stored in SAT.Dat. That way, the user can define a
'     special log in name and password and retrieve the message that way.
'
'     The function SELFTIMED_STFormatter handles the message capture.  Most of this
'     routine is straight from the XPert BASIC manual.
'
'     Note that Selftime_STFormatter is the current message built by the C code
'------------------------------------------------------------------------------
'
Public FUNCTION SELFTIMED_STFormatter

   'RECENT.DAT IS WHERE THE CURRENT METER PUTS ITS ASCII DATA ENSEMBLE
   F1 = FREEFILE 'FREEFILE RETURNS NEXT AVAILABLE FILE NUMBER IF 0 IS RETURNED, NO FILE NUMBERS AVAILABLE
   strDatFileName = "\Flash Disk\RECENT.DAT\"
   OPEN strDatFileName FOR INPUT AS F1

   'READS UP TO 'numbytes' RAW BINARY DATA AND STORES THE RESULT IN 'data'. RETURNS NUMBER OF BYTES READ
   'NOT PERMITTED ON LOG FILES
   'SYNTAX:result = ReadB(#filenumber, data, numbytes)
   result = ReadB(F1, DATASTR, 3000)

   'CLOSE RECENT.DAT
   CLOSE F1

   'OUTPUT.DAT WILL BE A WORKING COPY OF RECENT.DAT
   F2 = FREEFILE
   OPEN "Output.Dat" FOR OUTPUT AS F2

   'TRANSFER RECENT.DAT TO WORKING FILE(OUTPUT.DAT)
   DATASTR = ""
   N = WriteB(F2,DATASTR,result) 

   'CLOSE OUTPUT.DAT AS AN OUTPUT AND RE-OPEN AS INPUT TO READ FROM
   CLOSE F2
   OPEN "Output.Dat" FOR INPUT AS F2

   'SAT.DAT IS WHERE THE GOES MESSAGE COPY WILL BE STORED FOR RETRIEVAL W/ USER ID LIKE PORTS TAG
   F3 = FREEFILE
   OPEN "SAT.Dat" FOR OUTPUT AS F3

   'PRINT XPERT STATION ID, CS PREFIX STANDS FOR(Currents_Sontek)
   PRINT F3, "CS";SYSTAT(0);      

   'TStr IS USED AS PLACEHOLDER TO READ LINES FROM RECENT.DAT
   TStr = "";

   'READ PAST 7 LINES - 8TH LINE BEGINS HEADER
   FOR I = 1 TO 8
		LINE INPUT F2, TStr   
   NEXT I

   ''''''''''''''THIS IS WHERE THE PSEUDOBINARY ENCODING OF DATA HAPPENS
   'AT THIS POINT TStr LOOKS LIKE THE STRING BELOW
   '  16        M878        3328 2010  5 17 15 58 47 2 2 1 1
   PRINT F3, "-";BIN6(VAL(MID(TStr, 1,5)),2);MID(TStr,12,5);Bin6(VAL(Mid(TStr,26,3)),2);Bin6(VAL(MID(TStr,30,4)),2);Bin6(VAL(MID(TStr,35,2)),2);Bin6(VAL(MID(TStr,38,2)),2);
   PRINT F3, Bin6(VAL(MID(TStr,41,2)),2);Bin6(VAL(MID(TStr,44,2)),2);Bin6(VAL(Mid(TStr,47,2)),2);Bin6(VAL(MID(TStr,49,2)),2);Bin6(VAL(MID(TStr,51,2)),2);Bin6(VAL(MID(TStr,53,2)),2);Bin6(VAL(MID(TStr,55,2)),2);
   TStr = ""
 
  LINE INPUT F2, TStr   'read in second header line
   '    35   400   150   300   299
   PRINT F3, "-";BIN6(VAL(MID(TStr,1,6)),2);BIN6(VAL(MID(TStr,7,6)),2);BIN6(VAL(MID(TStr,13,6)),2);BIN6(VAL(MID(TStr,19,6)),2);BIN6(VAL(MID(TStr,25,6)),2);
   TStr = ""
   LINE INPUT F2, TStr   'read in third header line
'   2813      4     -2   2571   4202      1      0      0      4      9  15354
   PRINT F3, "-";Right(BIN6(VAL(MID(TStr,1,7)),3),2);BIN6(VAL(MID(TStr,8,7)),2);BIN6(VAL(MID(TStr,15,7)),2);BIN6(VAL(MID(TStr,22,7)),3);BIN6(VAL(MID(TStr,29,7)),3);BIN6(VAL(MID(TStr,36,7)),2);BIN6(VAL(MID(TStr,43,7)),2);
   PRINT F3, BIN6(VAL(MID(TStr,50,7)),2);BIN6(VAL(MID(TStr,57,7)),3);BIN6(VAL(MID(TStr,64,7)),3);BIN6(VAL(MID(TStr,71,7)),3);
   TStr = ""
   LINE INPUT F2, TStr   'read in fourth header line
'   0   0   0   0  94 189 104 184  14  14   0   0 141 244   0 118
   PRINT F3, "-";BIN6(VAL(MID(TStr,1,4)),2);BIN6(VAL(MID(TStr,5,4)),2);BIN6(VAL(MID(TStr,9,4)),2);BIN6(VAL(MID(TStr,13,4)),2);BIN6(VAL(MID(TStr,17,4)),2);BIN6(VAL(MID(TStr,21,4)),2);BIN6(VAL(MID(TStr,25,4)),2);BIN6(VAL(MID(TStr,29,4)),2);
   PRINT F3, BIN6(VAL(MID(TStr,33,4)),2);BIN6(VAL(MID(TStr,37,4)),2);BIN6(VAL(MID(TStr,41,4)),2);BIN6(VAL(MID(TStr,45,4)),2);BIN6(VAL(MID(TStr,49,4)),2);BIN6(VAL(MID(TStr,53,4)),2);BIN6(VAL(MID(TStr,57,4)),2);BIN6(VAL(MID(TStr,61,4)),2);

   'READ IN ALL BIN LINES
   Do While Not Eof(F2) 
      TStr = ""
      LINE INPUT F2, TStr   'read in profile line
      If Len(TStr) > 2 then
         PRINT F3,"+";Right(BIN6(VAL(MID(TStr,1,3)),2),1);BIN6(VAL(MID(TStr,4,7)),2);BIN6(VAL(MID(TStr,11,7)),2);BIN6(VAL(MID(TStr,18,4)),2);BIN6(VAL(MID(TStr,22,4)),2);BIN6(VAL(MID(TStr,26,4)),2);BIN6(VAL(MID(TStr,30,4)),2);
      End if
   End Loop
	PRINT F3," ";
   CLOSE F2
   CLOSE F3
   OPEN "SAT.Dat" FOR INPUT AS F3
   result = ReadB(F3, DATASTR, 3000)
   CLOSE F3

   'THE FUNCTION NAMED VARIABLE IS RETURNED WHEN THE SELFTIMED_STFORMATTER FUNCTION IS CALLED
   Selftimed_STFormatter = DATASTR
END FUNCTION





'EXAMPLE RECENT.DAT FROM SONTEK CURRENT METER ON AN XPERT DATALOGGER
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'XPert Time:
'19/03/21 15:14:00
'ADCP Time:
'2019/03/21 15:14:02.86
'Profile Number:
'8854
'Profile data:
'  16       M1437        8854 2019  3 21 15  4  1 2 2 1 2
'    50   400   150   300   299
'   1856     25      0   1630  12243      1      0      0      0     16  15102
'   0   0   0   0  61 196  58 193  13  14   0   0 148 248   0 119
'  1   -194  -1042  12  27 172 179
'  2   -233  -1257   9  21 182 184
'  3   -264  -1445  10  23 178 179
'  4   -277  -1544   9  20 170 170
'  5   -252  -1606  10  22 163 164
'  6   -228  -1629  11  22 157 158
'  7   -209  -1632  11  23 153 157
'  8   -214  -1685  10  21 154 158
'  9   -220  -1742   9  20 153 159
' 10   -183  -1702   9  20 152 160
' 11   -193  -1717   9  20 154 158
' 12   -207  -1711  10  21 151 158
' 13   -191  -1741   9  20 149 157
' 14   -183  -1774  10  20 149 153
' 15   -166  -1751   9  21 149 152
' 16   -177  -1755   9  20 144 147
' 17   -143  -1741  10  22 138 141
' 18   -150  -1774  10  22 131 134
' 19   -155  -1791  11  22 126 129
' 20   -164  -1778  10  23 121 125
' 21   -177  -1781  11  23 119 120
' 22   -169  -1764  10  23 115 116
' 23   -161  -1735  11  22 112 112
' 24   -209  -1729  11  23 109 108
' 25   -190  -1695  10  21 106 104
' 26   -205  -1673  11  22 103 101
' 27   -185  -1628  11  24 102  97
' 28   -179  -1585  10  22  99  92
' 29   -175  -1567  10  22  97  89
' 30   -172  -1541  10  24  95  87
' 31   -180  -1567  10  23  92  84
' 32   -130  -1553  11  21  89  79
' 33   -134  -1562  10  21  86  76
' 34    -98  -1548   9  21  85  74
' 35   -108  -1568  10  22  83  73
' 36   -115  -1587  10  23  81  68
' 37   -128  -1611  10  23  80  63
' 38    -91  -1625  10  20  75  61
' 39   -120  -1633  11  22  68  60
' 40   -123  -1627  11  22  63  56
' 41   -117  -1622  11  25  59  52
' 42    -96  -1665  11  22  55  51
' 43   -120  -1683  11  28  52  49
' 44    -95  -1697  12  24  51  48
' 45   -102  -1660  12  26  48  49
' 46    -90  -1583  14  29  46  46
' 47   -101  -1569  14  31  45  45
' 48    -78  -1556  22  42  43  45
' 49    -86  -1526  19  35  40  45
' 50   -130  -1545  18  38  38  43


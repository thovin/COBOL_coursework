       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LAB9XXX.
       AUTHOR.        Tim J.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT WEATHER-FILE
                ASSIGN TO 'WEATHER.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

            SELECT WEATHER-REPORT
                ASSIGN TO 'HIGHLOWREPORT.TXT'.

       DATA DIVISION.
       FILE SECTION.

       FD WEATHER-FILE.
       01 WEATHER-ITEM.
          05 WI-STATION-CODE                  PIC X(3).
	      05 WI-WEATHER-ARRAY OCCURS 12 TIMES PIC 9(3).
  
       FD WEATHER-REPORT.

       01 REPORT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.

       01 END-OF-FILE-FLAG  PIC X  VALUE SPACE.
          88 MORE-RECORDS            VALUE 'Y'.
          88 NO-MORE-RECORDS         VALUE 'N'.

       01 STATION-TEXT.
          05        PIC X(20) VALUE "AZOKalamazoo".
          05        PIC X(20) VALUE "BUFBuffalo".
          05        PIC X(20) VALUE "CVGCincinnati".
          05        PIC X(20) VALUE "GRRGrand Rapids".
          05        PIC X(20) VALUE "HOUHouston-Hobby".
          05        PIC X(20) VALUE "LAXLos Angeles".
          05        PIC X(20) VALUE "MDWChicago-Midway".
          05        PIC X(20) VALUE "MKGMuskegon".
          05        PIC X(20) VALUE "NRTTokyo-Narita".
          05        PIC X(20) VALUE "ORDChicago-O'Hare".


       01 STATION-TBL REDEFINES STATION-TEXT.
           05 STATION-TBL-LINE OCCURS 10 TIMES
               INDEXED BY STATION-DATA-INDEX.
               10 ST-STATION-CODE      PIC X(3).
               10 ST-STATION-NAME      PIC X(17).





       01 DETAIL-FIELDS.
          05 WS-HIGH-TEMP           PIC 9(3).
          05 WS-LOW-TEMP            PIC 9(3).

       01 ARRAY-SUBSCRIPT.
          05 SUB                       PIC 99.

       01 HEADER-1.
          05        PIC X(25) VALUE SPACES.
          05        PIC X(25) VALUE '12 Hour Weather Summary'.

       01 HEADER-2.
          05        PIC X(31) VALUE 'Station'.
          05        PIC X(9)  VALUE 'High'.
          05        PIC X(3)  VALUE 'Low'.

       01 DETAIL-LINE.
          05 DETAIL-STATION-NAME  PIC X(17).
          05                      PIC X(15)    VALUE SPACES.
          05 DETAIL-HIGH-TEMP     PIC ZZ9.
          05                      PIC X(5)     VALUE SPACES.
          05 DETAIL-LOW-TEMP      PIC ZZ9.



       PROCEDURE DIVISION.

       100-MAIN.

           PERFORM 200-HOUSEKEEPING
           PERFORM 300-READ-ROUTINE
           PERFORM 600-EOJ-ROUTINE
           STOP RUN
          .

       200-HOUSEKEEPING.

           OPEN INPUT  WEATHER-FILE
                OUTPUT WEATHER-REPORT
           PERFORM 250-PRINT-THE-HEADERS

          .
       250-PRINT-THE-HEADERS.

           WRITE REPORT-RECORD FROM HEADER-1
           AFTER ADVANCING 2 LINES

           WRITE REPORT-RECORD FROM HEADER-2
           AFTER ADVANCING 2 LINES
          .

       300-READ-ROUTINE.

           PERFORM UNTIL NO-MORE-RECORDS
            READ WEATHER-FILE
               AT END
                   MOVE 'N' TO END-OF-FILE-FLAG
               NOT AT END
                   PERFORM 400-REPORT-ROUTINE
             END-READ
            END-PERFORM
          .

       400-REPORT-ROUTINE.

           PERFORM 450-SEARCH-ROUTINE

           MOVE ZERO TO WS-HIGH-TEMP
           MOVE 999  TO WS-LOW-TEMP



           PERFORM 500-FIND-THE-HI-AND-LO
                VARYING SUB FROM 1 BY 1
                  UNTIL SUB > 12

           MOVE WS-HIGH-TEMP TO DETAIL-HIGH-TEMP
           MOVE WS-LOW-TEMP  TO DETAIL-LOW-TEMP

           WRITE REPORT-RECORD FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE
          .
       450-SEARCH-ROUTINE.


           SET STATION-DATA-INDEX TO 1
           SEARCH STATION-TBL-LINE
               AT END
                   MOVE 'INVALID' TO DETAIL-STATION-NAME

               WHEN WI-STATION-CODE = 
                           ST-STATION-CODE(STATION-DATA-INDEX)
                    MOVE ST-STATION-NAME(STATION-DATA-INDEX) TO 
                                                   DETAIL-STATION-NAME









         .

       500-FIND-THE-HI-AND-LO.

           
           MOVE WI-WEATHER-ARRAY(1) TO WS-HIGH-TEMP
           MOVE WI-WEATHER-ARRAY(1) TO WS-LOW-TEMP
 
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
               IF WI-WEATHER-ARRAY(SUB) > WS-HIGH-TEMP
                   MOVE WI-WEATHER-ARRAY(SUB) TO WS-HIGH-TEMP
               
               ELSE
                   IF WI-WEATHER-ARRAY(SUB) < WS-LOW-TEMP
                       MOVE WI-WEATHER-ARRAY(SUB) TO WS-LOW-TEMP
                   END-IF
               END-IF







          .

       600-EOJ-ROUTINE.
           CLOSE WEATHER-FILE
                 WEATHER-REPORT
          .



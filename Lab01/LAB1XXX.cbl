       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LAB1.
       AUTHOR.     TimJordan.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBMPC.
       OBJECT-COMPUTER.    IBMPC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT SCOUT-FILE
               ASSIGN TO 'SCOUT-FILE.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REPORT-FILE 
             ASSIGN TO PRINTER 'REPORT.DAT'.



       DATA DIVISION.
       FILE SECTION.


       FD    SCOUT-FILE
           RECORD CONTAINS 16 CHARACTERS.
       01    SCOUT-REC.
           05 SR-NAME             PIC X(10).
           05 SR-SAMOAS-SOLD      PIC 999.
           05 SR-MINTS-SOLD       PIC 999.


       FD    REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
       01    REPORT-REC    PIC X(80).

       WORKING-STORAGE SECTION.
       01    WS-WORK-AREAS.
             05    ARE-THERE-MORE-RECORDS    PIC X(3) VALUE 'YES'.

       01  TEMP-FIELDS.
           05 WS-COOKIE-TOTAL     PIC 9999 VALUE 0. 

      *************************OUTPUT AREA********************************

       01 DETAIL-LINE.
          05 FILLER          PIC X(5) VALUE SPACES.
          05 DL-NAME-OUT     PIC X(10).
          05 FILLER          PIC X(5) VALUE SPACES.
          05 DL-SAMOAS-OUT   PIC 999.
          05 FILLER          PIC X(5) VALUE SPACES.
          05 DL-MINTS-OUT    PIC 999.
          05 FILLER          PIC X(5) VALUE SPACES.
          05 DL-COOKIE-TOTAL PIC 9,999.

       PROCEDURE DIVISION.

       100-MAIN-MODULE.

           PERFORM 125-HOUSEKEEPING
           PERFORM 150-READ-RECORDS
           PERFORM 250-CLOSE-ROUTINE

           .



       125-HOUSEKEEPING.

           OPEN INPUT SCOUT-FILE
                   OUTPUT REPORT-FILE
           .


       150-READ-RECORDS.

             PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
                  READ SCOUT-FILE
                      AT END
                          MOVE 'NO' TO ARE-THERE-MORE-RECORDS
                      NOT AT END
                          PERFORM 200-PROCESS-RTN
                  END-READ
              END-PERFORM
           .


       200-PROCESS-RTN.

              MOVE SR-NAME TO DL-NAME-OUT
              MOVE SR-SAMOAS-SOLD TO DL-SAMOAS-OUT
              MOVE SR-MINTS-SOLD TO DL-MINTS-OUT

              ADD SR-SAMOAS-SOLD TO SR-MINTS-SOLD GIVING WS-COOKIE-TOTAL

              MOVE WS-COOKIE-TOTAL TO DL-COOKIE-TOTAL


              MOVE DETAIL-LINE TO REPORT-REC

              WRITE REPORT-REC
           .

       250-CLOSE-ROUTINE.

              CLOSE SCOUT-FILE
              CLOSE REPORT-FILE

              STOP RUN
           .


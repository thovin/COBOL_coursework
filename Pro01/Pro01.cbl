       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pro01.
       AUTHOR. Tim J.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INVENTORY-FILE
               ASSIGN TO 'PR1FA20.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INVENTORY-REPORT-FILE
               ASSIGN TO PRINTER 'REPORT.TXT'.

        
       DATA DIVISION.
       FILE SECTION.

       FD INVENTORY-FILE
           RECORD CONTAINS 60 CHARACTERS.

       01 INVENTORY-RECORD.
           05 IR-CAT-NUM           PIC X(5).
           05 IR-PART-NAME         PIC X(15).
           05 FILLER               PIC X(5).
           05 IR-UNIT-PURCH-PRICE  PIC 999V99.
           05 FILLER               PIC X(6).
           05 IR-QUANT-ON-HAND     PIC 9(4).
           05 IR-QUANT-ON-ORDER    PIC 9(4).
           05 IR-REORDER-POINT     PIC 9(4).
           05 IR-WAREHOUSE-ID      PIC X(6).
           05 FILLER               PIC X.
           05 IR-WAREHOUSE-BIN     PIC X(5).


       FD INVENTORY-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01 REPORT-RECORD            PIC X(80).


       WORKING-STORAGE SECTION.

       01 VARIABLES.
           05 EOF-FLAG             PIC X           VALUE ' '.
           05 NEW-LINES            PIC 99          VALUE 1.
           
       01 WS-DATE.
           05 WS-YEAR              PIC XX.
           05 WS-MONTH             PIC XX.
           05 WS-DAY               PIC XX.


      **************        OUTPUT AREA        ********************

       01 HEADING-ONE.
           05 H1-MONTH             PIC 99.
           05                      PIC X           VALUE '/'.
           05 H1-DAY               PIC 99.
           05                      PIC X           VALUE '/'.
           05 H1-YEAR              PIC 99.
           05 FILLER               PIC X(26)       VALUE SPACES.
           05                      PIC X(9)        VALUE 'LUNA. LTD'.
           05 FILLER               PIC X(24)       VALUE SPACES.
           05                      PIC X(3)        VALUE 'TWJ'.
           05 FILLER               PIC X(10)       VALUE SPACES.

       01 HEADING-TWO.
           05 FILLER               PIC X(34)       VALUE SPACES.
           05                      PIC X(16)   VALUE 'INVENTORY REPORT'.
           05 FILLER               PIC X(30)       VALUE SPACES.

       01 HEADING-THREE.
           05 FILLER               PIC X(3)        VALUE SPACES.
           05                      PIC X(7)        VALUE 'CATALOG'.
           05 FILLER               PIC X(8)        VALUE SPACES.
           05                      PIC X(4)        VALUE 'PART'.
           05 FILLER               PIC X(9)        VALUE SPACES.
           05                      PIC X(8)        VALUE 'PURCHASE'.
           05 FILLER               PIC X(3)        VALUE SPACES.
           05                      PIC X(8)        VALUE 'QUANTITY'.
           05 FILLER               PIC X(3)        VALUE SPACES.
           05                      PIC X(8)        VALUE 'QUANTITY'.
           05 FILLER               PIC X(3)        VALUE SPACES.
           05                      PIC X(7)        VALUE 'REORDER'.
           05 FILLER               PIC X(9)        VALUE SPACES.

       01 HEADING-FOUR.
           05 FILLER               PIC X(3)        VALUE SPACES.
           05                      PIC X(6)        VALUE 'NUMBER'.
           05 FILLER               PIC X(9)        VALUE SPACES.
           05                      PIC X(4)        VALUE 'NAME'.
           05 FILLER               PIC X(10)       VALUE SPACES.
           05                      PIC X(5)        VALUE 'PRICE'.
           05 FILLER               PIC X(5)        VALUE SPACES.
           05                      PIC X(7)        VALUE 'ON HAND'.
           05 FILLER               PIC X(4)        VALUE SPACES.
           05                      PIC X(8)        VALUE 'ON ORDER'.
           05 FILLER               PIC X(4)        VALUE SPACES.
           05                      PIC X(5)        VALUE 'POINT'.
           05 FILLER               PIC X(10)        VALUE SPACES.

       01 DETAIL-LINE.
           05 FILLER               PIC X(4)        VALUE SPACES.
           05 DL-CAT-NUM           PIC X(5).
           05 FILLER               PIC X(4)        VALUE SPACES.
           05 DL-PART-NAME         PIC X(15).
           05 FILLER               PIC X(5)        VALUE SPACES.
           05 DL-PURCH-PRICE       PIC 999.99.
           05 FILLER               PIC X(5)        VALUE SPACES.
           05 DL-QUANT-ON-HAND     PIC 9(4).
           05 FILLER               PIC X(7)        VALUE SPACES.
           05 DL-QUANT-ON-ORDER    PIC 9(4).
           05 FILLER               PIC X(7)        VALUE SPACES.
           05 DL-REORDER-POINT     PIC 9(4).
           05 FILLER               PIC X(10)       VALUE SPACES.

       PROCEDURE DIVISION.

       10-CONTROL-MODULE.
           
           PERFORM 15-HOUSEKEEPING
           PERFORM 25-PRINT-DETAIL
           PERFORM 40-WRAP-UP
           .

       15-HOUSEKEEPING.

           OPEN INPUT INVENTORY-FILE
               OUTPUT INVENTORY-REPORT-FILE

           ACCEPT WS-DATE FROM DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR

           PERFORM 20-PRINT-HEADER
           .

       20-PRINT-HEADER.
           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PAGE
           MOVE 2 TO NEW-LINES

           MOVE HEADING-TWO TO REPORT-RECORD
           PERFORM 35-WRITE-LINE
           MOVE 3 TO NEW-LINES

           MOVE HEADING-THREE TO REPORT-RECORD
           PERFORM 35-WRITE-LINE

           MOVE HEADING-FOUR TO REPORT-RECORD
           PERFORM 35-WRITE-LINE
           MOVE 2 TO NEW-LINES
           .

       25-PRINT-DETAIL.
           
           PERFORM UNTIL EOF-FLAG = 'N'
               READ INVENTORY-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 30-GET-DETAIL-LINE
               END-READ
           END-PERFORM
           .
       
       30-GET-DETAIL-LINE.

           MOVE IR-CAT-NUM TO DL-CAT-NUM
           MOVE IR-PART-NAME TO DL-PART-NAME
           MOVE IR-UNIT-PURCH-PRICE TO DL-PURCH-PRICE
           MOVE IR-QUANT-ON-HAND TO DL-QUANT-ON-HAND
           MOVE IR-QUANT-ON-ORDER TO DL-QUANT-ON-ORDER
           MOVE IR-REORDER-POINT TO DL-REORDER-POINT

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 35-WRITE-LINE
           .

       35-WRITE-LINE.
           
           WRITE REPORT-RECORD
               AFTER ADVANCING NEW-LINES
           MOVE 1 TO NEW-LINES
           .

       40-WRAP-UP.

           CLOSE INVENTORY-FILE
               INVENTORY-REPORT-FILE

           STOP RUN
           .

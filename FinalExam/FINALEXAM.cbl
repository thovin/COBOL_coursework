       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALEXAM.
       AUTHOR. Tim J.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

        SELECT COSTUME-FILE
           ASSIGN TO 'FINALEXAM.TXT' 
           ORGANIZATION IS LINE SEQUENTIAL.

        SELECT SUMMARY-REPORT
           ASSIGN TO 'CORRECTOUTPUTFINAL.TXT'.


       DATA DIVISION.
       FILE SECTION.

       FD COSTUME-FILE 
       RECORD CONTAINS 60 CHARACTERS.

       01  COSTUME-RECORD.
           05  CR-CUST-ID          PIC 9(5).
           05  CR-CUST-NAME        PIC X(21).
           05  CR-PROD-PREFIX      PIC X(3).
           05  CR-PROD-SUFFIX      PIC X(3).
           05                      PIC X(3).
           05  CR-PROD-NAME        PIC X(14).
           05  CR-PROD-TYPE        PIC X(6).
           05  CR-PROD-SIZE        PIC X(9).
           05  CR-QTY-SOLD         PIC 9(3).
           05  CR-ITEM-PRICE       PIC 9(3)V99.


       FD SUMMARY-REPORT 
       RECORD CONTAINS 80 CHARACTERS.

       01  REPORT-RECORD PIC X(80).



       WORKING-STORAGE SECTION.
       01  FIELDS.
           05  END-OF-FILE-FLAG    PIC X VALUE 'Y'.
           05  DF-SALES-VALUE      PIC 9(5)V99.
           05  PROPER-SPACING      PIC 9 VALUE 1.
           05  WS-CURRENT-DATE.
               10  WS-YEAR         PIC 99.
               10  WS-MONTH        PIC 99.
               10  WS-DAY          PIC 99.
           05 GRAND-TOTAL          PIC 9(9)V99     VALUE 0.   


      **************        OUTPUT AREA        ********************
       01  HEADING1.
           05                      PIC X(3)        VALUE ' '.
           05 SH-DATE.
               10 H1-MONTH         PIC 99.
               10                  PIC X(1)        VALUE '/'.
               10 H1-DAY           PIC 99.
               10                  PIC X(1)        VALUE '/'.
               10 H1-YEAR          PIC 99.
           05                      PIC X(11)       VALUE ' '.
           05                      PIC X(35)       VALUE
                             'INVENTORY REPORT CHRISTMAS COSTUMES'.
           05                      PIC X(12)       VALUE ' '.
           05                      PIC X(3)        VALUE 'TWJ'.

       01  HEADING2.
           05                      PIC X(7)        VALUE 'PRODUCT'.
           05                      PIC X(5)        VALUE ' '.
           05                      PIC X(7)        VALUE 'PRODUCT'.
           05                      PIC X(6)        VALUE ' '.
           05                      PIC X(7)        VALUE 'PRODUCT'.
           05                      PIC X(4)        VALUE ' '.
           05                      PIC X(7)        VALUE 'PRODUCT'.
           05                      PIC X(4)        VALUE ' '.
           05                      PIC X(3)        VALUE 'QTY'.
           05                      PIC X(3)        VALUE ' '.
           05                      PIC X(8)        VALUE 'PURCHASE'.
           05                      PIC X(5)        VALUE ' '.
           05                      PIC X(5)        VALUE 'SALES'.

       01  HEADING3.
           05                      PIC X(2)        VALUE ' '.
           05                      PIC X(2)        VALUE 'ID'.
           05                      PIC X(10)       VALUE ' '.
           05                      PIC X(4)        VALUE 'NAME'.
           05                      PIC X(8)        VALUE ' '.
           05                      PIC X(4)        VALUE 'TYPE'.
           05                      PIC X(8)        VALUE ' '.
           05                      PIC X(4)        VALUE 'SIZE'.
           05                      PIC X(4)        VALUE ' '.
           05                      PIC X(4)        VALUE 'SOLD'.
           05                      PIC X(4)        VALUE ' '.
           05                      PIC X(5)        VALUE 'PRICE'.
           05                      PIC X(7)        VALUE ' '.
           05                      PIC X(5)        VALUE 'VALUE'.

       01  DETAIL-LINE.
           05  DL-PROD-ID          PIC X(6).
           05                      PIC X(3)        VALUE ' '.
           05  DL-PROD-NAME        PIC X(14).
           05                      PIC X(3)        VALUE ' '.
           05  DL-PROD-TYPE        PIC X(6).
           05                      PIC X(3)        VALUE ' '.
           05  DL-PROD-SIZE        PIC X(9).
           05                      PIC X(3)        VALUE ' '.
           05  DL-QTY-SOLD         PIC ZZ9.
           05                      PIC X(3)        VALUE ' '.
           05  DL-ITEM-PRICE       PIC $ZZ9.99.
           05                      PIC X(3)        VALUE ' '. 
           05  DL-SALES-VALUE      PIC $ZZZ,ZZ9.99.

       01  GRAND-TOTAL-LINE.
           05 FILLER               PIC X(38)       VALUE SPACES.
           05                      PIC X(12)       VALUE 'GRAND TOTAL:'.
           05 FILLER               PIC X(4)        VALUE SPACES.
           05 GTL-TOTAL            PIC $ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(11).

       



       PROCEDURE DIVISION.

      * RUNS ALL INDEPENDANT PARAGRAPHS
       01-CONTROL-MODULE.
           PERFORM 05-HOUSEKEEPING
           PERFORM 15-PRINT-HEADERS
           PERFORM 25-READ-INPUT-FILE
           PERFORM 100-WRAP-UP
           .

      * PREPARES FILES FOR I/O AND INITIALIZES DATE INFORMATION
       05-HOUSEKEEPING.
           OPEN INPUT  COSTUME-FILE
                OUTPUT SUMMARY-REPORT

           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR

           .

      * PRINTS REPORT HEADERS
       15-PRINT-HEADERS.
           WRITE REPORT-RECORD FROM HEADING1
                 AFTER ADVANCING PAGE

            WRITE REPORT-RECORD FROM HEADING2
               AFTER ADVANCING 3 LINES

            WRITE REPORT-RECORD  FROM HEADING3
               AFTER ADVANCING 1 LINE          
            MOVE 2 TO PROPER-SPACING


           .

      * PERFORM VARYING LOGIC FOR INPUT FILE
       25-READ-INPUT-FILE.
           PERFORM UNTIL END-OF-FILE-FLAG = 'N'
               READ COSTUME-FILE
                   AT END
                       MOVE 'N' TO END-OF-FILE-FLAG

                       MOVE GRAND-TOTAL TO GTL-TOTAL
                       WRITE REPORT-RECORD FROM GRAND-TOTAL-LINE
                           AFTER ADVANCING 2
                   NOT AT END
                       PERFORM 35-INPUT-LOGIC
               END-READ
           END-PERFORM

           .

      * LOGIC TO PROCESS AN INDIVIDUAL LINE OF THE INPUT FILE
       35-INPUT-LOGIC.
           MOVE CR-PROD-PREFIX(1:3) TO DL-PROD-ID(1:3)
           MOVE CR-PROD-SUFFIX(1:3) TO DL-PROD-ID(4:3)
           MOVE CR-PROD-NAME TO DL-PROD-NAME
           MOVE CR-PROD-TYPE TO DL-PROD-TYPE
           MOVE CR-PROD-SIZE TO DL-PROD-SIZE
           MOVE CR-QTY-SOLD TO DL-QTY-SOLD
           MOVE CR-ITEM-PRICE TO DL-ITEM-PRICE

           MULTIPLY CR-ITEM-PRICE BY CR-QTY-SOLD
               GIVING DF-SALES-VALUE
           MOVE DF-SALES-VALUE TO DL-SALES-VALUE
           ADD DF-SALES-VALUE TO GRAND-TOTAL


           MOVE DETAIL-LINE TO REPORT-RECORD
           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
           MOVE 1 TO PROPER-SPACING

           MOVE ZEROS TO DF-SALES-VALUE

           .



      * CLOSE FILES AND END EXECUTION
       100-WRAP-UP.
           CLOSE COSTUME-FILE
                 SUMMARY-REPORT

           STOP RUN
           .

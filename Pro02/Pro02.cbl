       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRO02.
       AUTHOR. Tim J.
      ****************************************************************
      * This program generates a sales report, 
      * grouped by customer

      ***************
      * INPUTS:
      *    The SALES FILE contains the following data in each record
      *        1: Customer ID
      *        2: Customer Name
      *        3: Product ID
      *        4: Product Name
      *        5: Quantity Sold
      *        6: Cost per Item

      * *******
      * OUTPUTS:
      *    The SALES RECORD FILE contains the following data:
      *        TWO HEADERS
      *        TWO COLUMN HEADERS
      *        DETAIL LINE:
      *            1: CUST-NAME (FIRST LINE IN EACH GROUP)
      *            2: PROD-ID
      *            3: PROD-NAME
      *            4: QTY-SOLD
      *            5: SALES-VALUE
      *        GROUP TOTAL (ONE PER GROUP)
      *        TOTAL-QTY-SOLD
      *        TOTAL-SALES
      ****************
      * CALCULATIONS:
      *    SALES [QTY SOLD * COST PER ITEM]
      *    GROUP QTY [SUM OF ALL QTY SOLD]
      *    GROUP SALES [SUM OF ALL SALES]
      *    TOTAL QTY SOLD [SUM TO ALL GROUP'S QTY SOLD]
      *    TOTAL SALES [SUM OF ALL GROUP'S SALES]
      ************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SALES-FILE
               ASSIGN TO 'PR2FA20.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SALES-REPORT-FILE
               ASSIGN TO PRINTER 'REPORT.TXT'.

       DATA DIVISION.
       FILE SECTION.

       FD SALES-FILE
           RECORD CONTAINS 60 CHARACTERS.

       01 SALES-RECORD.
           05 SR-CUST-ID           PIC 9(5).
           05 SR-CUST-NAME         PIC X(25).
           05 SR-PROD-ID           PIC XXX.
           05 FILLER               PIC X(5).
           05 SR-PROD-NAME         PIC X(14).
           05 SR-QTY-SOLD          PIC 999.
           05 SR-COST-PER-ITEM     PIC 999V99.

       FD SALES-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01 REPORT-RECORD            PIC X(80).


       WORKING-STORAGE SECTION.

       01 FLAGS-AND-COUNTERS.
           05 EOF-FLAG             PIC X           VALUE 'F'.
           05 NEW-LINES            PIC 99          VALUE 1.
           05 OLD-CUST-NAME        PIC X(24)       VALUE 'FIRST'.
           05 TEMP-SALES           PIC 9(9)V99     VALUE 0.

       01 RUNNING-TOTALS.
           05 GROUP-TOTAL-QTY      PIC 9(6)        VALUE 0.
           05 GROUP-TOTAL-SALES    PIC 9(6)V99     VALUE 0.
           05 REPORT-TOTAL-QTY     PIC 9(7)        VALUE 0.
           05 REPORT-TOTAL-SALES   PIC 9(7)V99     VALUE 0.

       01 WS-DATE.
           05 WS-YEAR              PIC XX.
           05 WS-MONTH             PIC XX.
           05 WS-DAY               PIC XX.


      **************        OUTPUT AREA        ********************

       01 HEADING-ONE.
           05 FILLER               PIC X(34)       VALUE SPACES.
           05                      PIC X(11)       VALUE 'ASHRALS LTD'.
           05 FILLER               PIC X(35)       VALUE SPACES.
      
       01 HEADING-TWO.
           05 FILLER               PIC X(10)       VALUE SPACES.
           05 H2-MONTH             PIC 99.
           05                      PIC X           VALUE '/'.
           05 H2-DAY               PIC 99.
           05                      PIC X           VALUE '/'.
           05 H2-YEAR              PIC 9(2).
           05 FILLER               PIC X(9)        VALUE SPACES.
           05                      PIC X(24)       VALUE 
                                   'SALES SPECULATION REPORT'.
           05 FILLER               PIC X(18)       VALUE SPACES.
           05                      PIC X(3)        VALUE 'TWJ'.
           05 FILLER               PIC X(8)        VALUE SPACES.

       01 HEADING-THREE.
           05 FILLER               PIC X(29)       VALUE SPACES.
           05                      PIC X(4)        VALUE 'PROD'.
           05 FILLER               PIC X(4)        VALUE SPACES.
           05                      PIC X(7)        VALUE 'PRODUCT'.
           05 FILLER               PIC X(9)        VALUE SPACES.
           05                      PIC X(3)        VALUE 'QTY'.
           05 FILLER               PIC X(9)        VALUE SPACES.
           05                      PIC X(5)        VALUE 'SALES'.
           05 FILLER               PIC X(10)       VALUE SPACES.

       01 HEADING-FOUR.
           05 FILLER               PIC X(5)        VALUE SPACES.
           05                      PIC X(13)       VALUE 
                                   'CUSTOMER NAME'.
           05 FILLER               PIC X(12)       VALUE SPACES.
           05                      PIC X(2)        VALUE 'ID'.
           05 FILLER               PIC X(7)        VALUE SPACES.
           05                      PIC X(4)        VALUE 'NAME'.
           05 FILLER               PIC X(9)        VALUE SPACES.
           05                      PIC X(4)        VALUE 'SOLD'.
           05 FILLER               PIC X(9)        VALUE SPACES.
           05                      PIC X(5)        VALUE 'VALUE'.
           05 FILLER               PIC X(10)       VALUE SPACES.

       01 GROUP-TOTAL-LINE.
           05 FILLER               PIC X(41)       VALUE SPACES.
           05                      PIC X(6)        VALUE 'TOTAL:'.
           05 FILLER               PIC X(3)        VALUE SPACES.
           05 GTL-TOTAL-QTY        PIC ZZZ,ZZ9.
           05 FILLER               PIC X(2)        VALUE SPACES.
           05 GTL-TOTAL-SALES      PIC $Z,ZZZ,ZZ9.99.
           05 FILLER               PIC X(8)        VALUE SPACES.

       01 FINAL-TOTAL-QTY-LINE.
           05 FILLER               PIC X(31)       VALUE SPACES.
           05                      PIC X(26)       VALUE 
                                       'GRAND TOTAL QUANTITY SOLD:'.
           05 FILLER               PIC X(6)        VALUE SPACES.
           05 FTQL-TOTAL-QTY       PIC Z,ZZZ,ZZ9.
           05 FILLER               PIC X(8)        VALUE SPACES.

       01 FINAL-TOTAL-SALES-LINE.
           05 FILLER               PIC X(28)       VALUE SPACES.
           05                      PIC X(24)       VALUE 
                                           'GRAND TOTAL SALES VALUE'.
           05 FILLER               PIC X(7)        VALUE SPACES.
           05 FTSL-TOTAL-SALES     PIC $Z,ZZZ,ZZ9.99.
           05 FILLER               PIC X(8).

       01 DETAIL-LINE.
           05 FILLER               PIC X(2)        VALUE SPACES.
           05 DL-CUST-NAME         PIC X(25).
           05 FILLER               PIC X(3)        VALUE SPACES.
           05 DL-PROD-ID           PIC X(3).
           05 FILLER               PIC X(2)        VALUE SPACES.
           05 DL-PROD-NAME         PIC X(14).
           05 FILLER               PIC X(4)        VALUE SPACES.
           05 DL-QTY-SOLD          PIC ZZZ9.
           05 FILLER               PIC X(4)        VALUE SPACES.
           05 DL-PROD-SALES        PIC $ZZZ,ZZ9.99.
           05 FILLER               PIC X(8)        VALUE SPACES.

           
       PROCEDURE DIVISION.
       
       10-CONTROL-MODULE.
           PERFORM 15-HOUSEKEEPING
           PERFORM 25-PROCESS-INFILE
           PERFORM 80-PRINT-ENDING
           PERFORM 100-WRAP-UP
           .

       15-HOUSEKEEPING.

           OPEN INPUT SALES-FILE
               OUTPUT SALES-REPORT-FILE

           ACCEPT WS-DATE FROM DATE
           MOVE WS-MONTH TO H2-MONTH
           MOVE WS-DAY TO H2-DAY
           MOVE WS-YEAR TO H2-YEAR

           PERFORM 20-PRINT-HEADER
           .

       20-PRINT-HEADER.
           
           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PAGE
           MOVE 1 TO NEW-LINES

           MOVE HEADING-TWO TO REPORT-RECORD
           PERFORM 35-WRITE-LINE
           MOVE 3 TO NEW-LINES

           MOVE HEADING-THREE TO REPORT-RECORD
           PERFORM 35-WRITE-LINE
           MOVE 1 TO NEW-LINES

           MOVE HEADING-FOUR TO REPORT-RECORD
           PERFORM 35-WRITE-LINE
           MOVE 2 TO NEW-LINES
           .

       25-PROCESS-INFILE.

           PERFORM UNTIL EOF-FLAG = 'T'
               READ SALES-FILE
                   AT END
                       MOVE 'T' TO EOF-FLAG
                   NOT AT END
                       PERFORM 30-PROCESS-INFILE-LINE
               END-READ
           END-PERFORM
           .

       30-PROCESS-INFILE-LINE.
           
           IF OLD-CUST-NAME = 'FIRST'
               MOVE SR-CUST-NAME TO OLD-CUST-NAME
               MOVE SR-CUST-NAME TO DL-CUST-NAME

               MOVE 2 TO NEW-LINES

           ELSE
               IF OLD-CUST-NAME NOT = SR-CUST-NAME
                   PERFORM 40-CONTROL-BREAK

                   MOVE 2 TO NEW-LINES
                   MOVE SR-CUST-NAME TO OLD-CUST-NAME
                   MOVE SR-CUST-NAME TO DL-CUST-NAME

               ELSE
                   MOVE SPACES TO DL-CUST-NAME
                   MOVE 1 TO NEW-LINES

               END-IF
           END-IF

           
           MOVE SR-PROD-ID TO DL-PROD-ID
           MOVE SR-PROD-NAME TO DL-PROD-NAME

           IF SR-QTY-SOLD IS NUMERIC
               MOVE SR-QTY-SOLD TO DL-QTY-SOLD

           ELSE
               MOVE ZEROES TO DL-QTY-SOLD

           END-IF

           MULTIPLY SR-QTY-SOLD BY SR-COST-PER-ITEM GIVING TEMP-SALES
           MOVE TEMP-SALES TO DL-PROD-SALES

           ADD SR-QTY-SOLD TO GROUP-TOTAL-QTY
           ADD SR-QTY-SOLD TO REPORT-TOTAL-QTY
           ADD TEMP-SALES TO GROUP-TOTAL-SALES
           ADD TEMP-SALES TO REPORT-TOTAL-SALES

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 35-WRITE-LINE
           .

       35-WRITE-LINE.
           WRITE REPORT-RECORD
               AFTER ADVANCING NEW-LINES
           .
       

       40-CONTROL-BREAK.
           MOVE GROUP-TOTAL-QTY TO GTL-TOTAL-QTY
           MOVE ZEROES TO GROUP-TOTAL-QTY

           MOVE GROUP-TOTAL-SALES TO GTL-TOTAL-SALES
           MOVE ZEROES TO GROUP-TOTAL-SALES

           MOVE GROUP-TOTAL-LINE TO REPORT-RECORD
           MOVE 2 TO NEW-LINES
           PERFORM 35-WRITE-LINE

           .

       80-PRINT-ENDING.
           PERFORM 40-CONTROL-BREAK

           MOVE 3 TO NEW-LINES
           MOVE REPORT-TOTAL-QTY TO FTQL-TOTAL-QTY
           MOVE REPORT-TOTAL-SALES TO FTSL-TOTAL-SALES

           MOVE FINAL-TOTAL-QTY-LINE TO REPORT-RECORD
           PERFORM 35-WRITE-LINE

           MOVE FINAL-TOTAL-SALES-LINE TO REPORT-RECORD
           MOVE 2 TO NEW-LINES
           PERFORM 35-WRITE-LINE

           .

       100-WRAP-UP.
           CLOSE SALES-FILE
               SALES-REPORT-FILE

           STOP RUN
           .

       IDENTIFICATION DIVISION.
       PROGRAM-ID.     LAB3XXX.
       AUTHOR.         Tim J.

       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT BOOK-INVEN-FILE
               ASSIGN TO "L3BOOKINFO.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT BOOK-INVEN-REPORT-FILE
               ASSIGN TO PRINTER "BOOKREPORTTWJ.TXT".
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD BOOK-INVEN-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  BOOK-INVEN-RECORD.
           05  BI-BOOK-CODE                PIC X(2).
           05  BI-TITLE                    PIC X(18).
           05  BI-AUTHOR                   PIC X(15).
           05  BI-SUBJECT-AREA.
               10  BI-SUBJ-PREFIX          PIC X(5).
               10  BI-SUBJ-SUFFIX          PIC X(3).
           05  BI-SHELF-LOCATION.
               10  BI-SHELF-ALPHA          PIC X(2).
               10  BI-SHELF-NUMERIC        PIC X(3).
           05  BI-UNIT-COST                PIC S99V99.
           05  BI-SELLING-PRICE            PIC S99V99.
           05  BI-QUANTITY-ON-HAND         PIC 9(3).
           05  BI-REORDER-LEVEL            PIC S999.
           05  BI-QUANTITY-ON-ORDER        PIC S999.
           05  BI-DATE-OF-LAST-ORDER.
               10  BI-MONTH                PIC XX.
               10  BI-DAY-YEAR             PIC 9(4).
           05                              PIC X(11).
      *
       FD  BOOK-INVEN-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
       01  REPORT-LINE                     PIC X(80).

      *
       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X       VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING              PIC 9       VALUE 1.
      *
       01  WS-CURRENT-DATE.
           05  WS-YEAR                     PIC 99.
           05  WS-MONTH                    PIC 99.
           05  WS-DAY                      PIC 99.
      *
       01  DETAIL-FIELDS.
           05  DF-TOTAL-VALUE              PIC S9(5)V99 VALUE +0.
      *
       01  CONSTANTS.
           05  NEW-YEAR-INCREASE           PIC S9V99   VALUE +1.05.
      *

      **********************OUTPUT AREA*************************
       01  HEADING-ONE.
           05                              PIC X(10) VALUE 'DATE:'.
           05  H1-DATE.
               10  H1-MONTH                PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-YEAR                 PIC 99.
           05                              PIC X(15) VALUE SPACES.
           05                              PIC X(30) VALUE
                                           'BOOK INVENTORY REPORT'.
           05                              PIC X(25) VALUE 'TWJ'.
      *
       01  HEADING-TWO.
           05                              PIC X(11) VALUE 'BOOK'.
           05                              PIC X(19) VALUE 'AUTHOR'.
           05                              PIC X(17) VALUE 'TITLE'.
           05                              PIC X(8)  VALUE 'QTY ON'.
           05                              PIC X(15) VALUE 'SELLING'.
           05                              PIC X(5)  VALUE 'TOTAL'.
      *
       01  HEADING-THREE.
           05                              PIC X(48)   VALUE 'CODE'.
           05                              PIC X(9)    VALUE 'HAND'.
           05                              PIC X(13)   VALUE 'PRICE'.
           05                              PIC X(5)    VALUE 'VALUE'.
      *
       01  DETAIL-LINE.
           05  DL-BOOK-CODE                PIC X(2).
           05                              PIC X(7).
           05  DL-AUTHOR                   PIC X(15).
           05                              PIC X(2)    VALUE SPACES.
           05  DL-TITLE                    PIC X(18).
           05                              PIC X(5)    VALUE SPACES.
           05  DL-QUANTITY-ON-HAND         PIC ZZ9-.
           05                              PIC X(4)    VALUE SPACES.
           05  DL-SELLING-PRICE            PIC ZZ.99.
           05                              PIC X(4)    VALUE SPACES.
           05  DL-TOTAL-VALUE              PIC ZZ,ZZZ.99.
      *
       PROCEDURE DIVISION.
      *
       100-PRINT-BOOK-INVEN-REPORT.
           PERFORM 200-HSKPING-ROUTINE
           PERFORM 300-READ-INVENTORY-FILE
           PERFORM 700-FINAL-ROUTINE
       .

       200-HSKPING-ROUTINE.
           OPEN INPUT  BOOK-INVEN-FILE
                OUTPUT BOOK-INVEN-REPORT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR
           PERFORM 400-HEADING-ROUTINE
       .

       300-READ-INVENTORY-FILE.
           PERFORM UNTIL NO-MORE-DATA
               READ BOOK-INVEN-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 500-PROCESS-INVEN-RECORD
               END-READ
           END-PERFORM

       .

       400-HEADING-ROUTINE.

           WRITE REPORT-LINE FROM HEADING-ONE
               AFTER ADVANCING PAGE
           MOVE 2 TO PROPER-SPACING

           MOVE HEADING-TWO TO REPORT-LINE
           PERFORM 600-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING

           MOVE HEADING-THREE TO REPORT-LINE
           PERFORM 600-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
       .

       500-PROCESS-INVEN-RECORD.

           MOVE BI-BOOK-CODE TO DL-BOOK-CODE
           MOVE BI-AUTHOR TO DL-AUTHOR
           MOVE BI-TITLE TO DL-TITLE

           IF BI-MONTH IS NOT NUMERIC

               MOVE 'INVALID MONTH' TO DL-TITLE

           END-IF

           IF BI-SELLING-PRICE IS NUMERIC

               MOVE BI-SELLING-PRICE TO DL-SELLING-PRICE

           ELSE

               MOVE 0 TO DL-SELLING-PRICE
           
           END-IF

           IF BI-QUANTITY-ON-HAND IS NUMERIC
               IF BI-QUANTITY-ON-HAND IS NOT EQUAL TO 0

                   MULTIPLY BI-QUANTITY-ON-HAND BY BI-SELLING-PRICE
                   GIVING DF-TOTAL-VALUE
               ELSE
                   MOVE 0 TO DF-TOTAL-VALUE
               END-IF

               MOVE BI-QUANTITY-ON-HAND TO DL-QUANTITY-ON-HAND

           ELSE
               MOVE 0 TO DL-QUANTITY-ON-HAND
               MOVE 0 TO DF-TOTAL-VALUE
               MOVE 0 TO DL-SELLING-PRICE

           END-IF

           MOVE DF-TOTAL-VALUE TO DL-TOTAL-VALUE

           MOVE DETAIL-LINE TO REPORT-LINE
           PERFORM 600-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           .

       600-WRITE-A-LINE.
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
         .

       700-FINAL-ROUTINE.
           CLOSE BOOK-INVEN-FILE
               BOOK-INVEN-REPORT-FILE
            STOP RUN
            .

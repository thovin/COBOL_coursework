       IDENTIFICATION DIVISION.
       PROGRAM-ID.     LAB8TWJ.
       AUTHOR.         Tim J.

       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT OLD-BOOK-FILE
               ASSIGN TO "L8OLDBOOKFILE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT NEW-BOOK-FILE
               ASSIGN TO DISK "L8NEWBOOKFILE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD OLD-BOOK-FILE.
       01  BOOK-INVEN-RECORD.
           05  BI-BOOK-CODE                PIC X(2).
           05  BI-TITLE                    PIC X(18).
           05  BI-AUTHOR                   PIC X(15).
           05  BI-UNIT-COST                PIC S99V99.
           05  BI-SELLING-PRICE            PIC S99V99.
           05  BI-QUANTITY-ON-HAND         PIC 9(3).
           05  BI-REORDER-LEVEL            PIC S999.
           05  BI-QUANTITY-ON-ORDER        PIC S999.
           05  BI-DATE-OF-LAST-ORDER.
               10  BI-MONTH                PIC XX.
               10  BI-YEAR                 PIC X(2).
      *
       FD  NEW-BOOK-FILE.
       01  NEW-BOOK-RECORD.

           05 NB-BOOK-CODE                 PIC X(4).
           05 NB-TITLE                     PIC X(18).
           05 NB-AUTHOR                    PIC X(15).
           05 NB-UNIT-COST                 PIC S99V99.
           05 NB-SELLING-PRICE             PIC S99V99.
           05 NB-QUANTITY-ON-HAND          PIC 9(3).
           05 NB-REORDER-LEVEL             PIC S999.
           05 NB-QUANTITY-ON-ORDER         PIC S999.
           05 NB-DATE-OF-LAST-ORDER.
               10 NB-MONTH                 PIC XX.
               10 NB-YEAR                  PIC X(4).










      *
       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X       VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.

      *
       PROCEDURE DIVISION.
      *
       100-PRINT-BOOK-INVEN-REPORT.
           PERFORM 200-HSKPING-ROUTINE
           PERFORM 300-READ-INVENTORY-FILE
           PERFORM 500-FINAL-ROUTINE
       .

       200-HSKPING-ROUTINE.
           OPEN INPUT  OLD-BOOK-FILE
                OUTPUT NEW-BOOK-FILE
       .

       300-READ-INVENTORY-FILE.
           PERFORM UNTIL NO-MORE-DATA
               READ OLD-BOOK-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 400-CREATE-NEW-FILE
               END-READ
           END-PERFORM

       .

       400-CREATE-NEW-FILE.


           MOVE 'CS' TO NB-BOOK-CODE
           MOVE BI-BOOK-CODE TO NB-BOOK-CODE(3:2)

           MOVE BI-TITLE TO NB-TITLE
           MOVE BI-AUTHOR TO NB-AUTHOR
           MOVE BI-UNIT-COST TO NB-UNIT-COST
           MOVE BI-SELLING-PRICE TO NB-SELLING-PRICE
           MOVE BI-QUANTITY-ON-HAND TO NB-QUANTITY-ON-HAND
           MOVE BI-REORDER-LEVEL TO NB-REORDER-LEVEL
           MOVE BI-QUANTITY-ON-ORDER TO NB-QUANTITY-ON-ORDER
           MOVE BI-DATE-OF-LAST-ORDER TO NB-DATE-OF-LAST-ORDER
           MOVE BI-MONTH TO NB-MONTH

           STRING
               '19' DELIMITED BY SIZE
               BI-YEAR DELIMITED BY SIZE
               
               INTO NB-YEAR
           END-STRING












           WRITE NEW-BOOK-RECORD
           .

       410-BOOK-CODE.

         .

       415-YEAR-MOD.


       .

       500-FINAL-ROUTINE.
           CLOSE OLD-BOOK-FILE
                 NEW-BOOK-FILE
            STOP RUN
            .


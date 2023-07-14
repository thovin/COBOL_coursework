       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRO04.
       AUTHOR. TIM J.
      ****************************************************************
      * This program sorts and merges three unsorted input files 
      * and creates a report based off the resulting file

      ***************
      * INPUTS:
      *    3 unsorted inventory files that are combined into the 
      *    inventory file
      *
      *    The INVENTORY FILE contains the following data per record:
      *        1: Vendor ID
      *        2: Warehouse ID
      *        3: Costume ID
      *        4: Costume data
      *            A: Costume name
      *            B: Costume sizing
      *            C: Costume type
      *            D: Number in stock
      *            E: ReOrder point
      *            F: Costume price

      * *******
      * OUTPUTS:
      *    The REPORT FILE contains the following:
      *        Two headers
      *        Three group headers:
      *            1: Vendor
      *            2: Warehouse
      *            3: Costume
      *        Detail line:
      *            1: Costume name
      *            2: Costume size
      *            3: Costume type
      *            4: Quantity in stock
      *            5: Total cost
      *        Three group footers
      *        One footer
      *
      *    The ERROR REPORT FILE contains the full inventory file record
      *    where there is an invalid vendor or warehouse code

      ****************
      * CALCULATIONS:
      *    COSTUME GROUP TOTALS
      *    WAREHOUSE GROUP TOTALS
      *    VENDOR GROUP TOTALS
      ************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CH-IN-FILE
               ASSIGN TO 'UNSORTED-CH20P4F20.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CH-SORTED-FILE
               ASSIGN TO 'SORTED-CH.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CH-SORT-FILE
               ASSIGN TO 'SORT-CH.TMP'.


           SELECT NY-IN-FILE
               ASSIGN TO 'UNSORTED-NY30P4F20.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NY-SORTED-FILE
               ASSIGN TO 'SORTED-NY.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NY-SORT-FILE
               ASSIGN TO 'SORT-NY.TMP'.


           SELECT LA-IN-FILE
               ASSIGN TO 'UNSORTED-LA10P4F20.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT LA-SORTED-FILE
               ASSIGN TO 'SORTED-LA.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT LA-SORT-FILE
               ASSIGN TO 'SORT-LA.TMP'.

           
           SELECT MERGED-INVENTORY-FILE
               ASSIGN TO 'MERGED-INVENTORY.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MERGE-FILE
               ASSIGN TO 'MERGE.TMP'.

           SELECT REPORT-FILE
               ASSIGN TO PRINTER 'REPORT.TXT'.

           SELECT ERROR-FILE
               ASSIGN TO PRINTER 'ERROR-REPORT.TXT'.
               

           

       
       DATA DIVISION.
       FILE SECTION.

       FD CH-IN-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 CH-IN-RECORD.
           05 CI-VENDOR-ID             PIC X(4).
           05 CI-WAREHOUSE-ID          PIC X(3).
           05 CI-COSTUME-ID            PIC X(3).
           05 FILLER                   PIC X(126).

       FD CH-SORTED-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 CH-SORTED-RECORD.
           05 CS-VENDOR-ID             PIC X(4).
           05 CS-WAREHOUSE-ID          PIC X(3).
           05 CS-COSTUME-ID            PIC X(3).
           05 FILLER                   PIC X(126).

       SD CH-SORT-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 CH-SORT-RECORD.
           05 CSG-VENDOR-ID            PIC X(4).
           05 CSG-WAREHOUSE-ID         PIC X(3).
           05 CSG-COSTUME-ID           PIC X(3).
           05 FILLER                   PIC X(126).


       FD NY-IN-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 NY-IN-RECORD.
           05 NI-VENDOR-ID             PIC X(4).
           05 NI-WAREHOUSE-ID          PIC X(3).
           05 NI-COSTUME-ID            PIC X(3).
           05 FILLER                   PIC X(126).

       FD NY-SORTED-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 NY-SORTED-RECORD.
           05 NS-VENDOR-ID             PIC X(4).
           05 NS-WAREHOUSE-ID          PIC X(3).
           05 NS-COSTUME-ID            PIC X(3).
           05 FILLER                   PIC X(126).

       SD NY-SORT-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 NY-SORT-RECORD.
           05 NSG-VENDOR-ID            PIC X(4).
           05 NSG-WAREHOUSE-ID         PIC X(3).
           05 NSG-COSTUME-ID           PIC X(3).
           05 FILLER                   PIC X(126).


       FD LA-IN-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 LA-IN-RECORD.
           05 LI-VENDOR-ID             PIC X(4).
           05 LI-WAREHOUSE-ID          PIC X(3).
           05 LI-COSTUME-ID            PIC X(3).
           05 FILLER                   PIC X(126).

       FD LA-SORTED-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 LA-SORTED-RECORD.
           05 LS-VENDOR-ID             PIC X(4).
           05 LS-WAREHOUSE-ID          PIC X(3).
           05 LS-COSTUME-ID            PIC X(3).
           05 FILLER                   PIC X(126).


       SD LA-SORT-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 LA-SORT-RECORD.
           05 LSG-VENDOR-ID            PIC X(4).
           05 LSG-WAREHOUSE-ID         PIC X(3).
           05 LSG-COSTUME-ID           PIC X(3).
           05 FILLER                   PIC X(126).

       


       FD MERGED-INVENTORY-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 INVENTORY-RECORD.
           05 IR-VENDOR-ID             PIC X(4).
           05 IR-WAREHOUSE-ID          PIC X(3).
           05 IR-COSTUME-ID            PIC X(3).
           05 IR-COSTUME-DATA OCCURS 6 TIMES.
               10 IR-COSTUME-NAME      PIC X(9).
               10 IR-COSTUME-SIZE      PIC A.
               10 IR-COSTUME-TYPE      PIC A.
               10 IR-NUM-IN-STOCK      PIC 9(3).
               10 IR-REORDER-POINT        PIC 9(3).
               10 IR-COSTUME-PRICE        PIC 99V99.

       SD MERGE-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 MERGE-RECORD.
           05 MR-VENDOR-ID             PIC X(4).
           05 MR-WAREHOUSE-ID          PIC X(3).
           05 MR-COSTUME-ID            PIC X(3).
           05 FILLER                   PIC X(126).

       FD REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01 REPORT-RECORD                PIC X(80).

       FD ERROR-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01 ERROR-RECORD                 PIC X(136).


       WORKING-STORAGE SECTION.

       01 FLAGS-AND-COUNTERS.
           05 EOF-FLAG                 PIC X           VALUE 'F'.
           05 NEW-LINES                PIC 99          VALUE 1.
           05 SUB                      PIC 99.
           05 VENDOR-CHECK             PIC X.
               88 VALID-VENDOR         VALUE 'Y'.
               88 INVALID-VENDOR       VALUE 'N'.
           05 WAREHOUSE-CHECK          PIC X.
               88 VALID-WAREHOUSE      VALUE 'Y'.
               88 INVALID-WAREHOUSE    VALUE 'N'.

       01 RUNNING-TOTALS.
           05 RT-VENDOR-TOTAL          PIC 9(8)V99   VALUE 0.
           05 RT-WAREHOUSE-TOTAL       PIC 9(7)V99  VALUE 0.
           05 RT-COSTUME-TOTAL         PIC 9(6)V99  VALUE 0.
           05 RT-GRAND-TOTAL           PIC 9(9)V99  VALUE 0.

       01 WS-DATE.
           05 WS-YEAR                  PIC XX.
           05 WS-MONTH                 PIC XX.
           05 WS-DAY                   PIC XX.

       01 HOLD-FIELDS.
           05 HF-COSTUME-NAME          PIC X(9).
           05 HF-WAREHOUSE-ID          PIC X(11).
           05 HF-VENDOR-ID             PIC X(4)        VALUE 'FRST'.
           05 HF-CALCULATED-COST       PIC 9(5)V99.

       01 COSTUME-SIZE-TEXT.
           05                          PIC X(7)        VALUE 'LLarge'.
           05                          PIC X(7)        VALUE 'MMedium'.
           05                          PIC X(7)        VALUE 'SSmall'.
           05                          PIC X(7)        VALUE 'PPlus'.

       01 COSTUME-SIZE-TBL REDEFINES COSTUME-SIZE-TEXT.
           05 COSTUME-SIZE-TBL-LINE OCCURS 4 TIMES
               INDEXED BY COSTUME-SIZE-INDEX.
               10 CST-CODE             PIC X(1).
               10 CST-VALUE            PIC X(6).


      **************        OUTPUT AREA        ********************

       01 HEADING-ONE.
           05 FILLER                   PIC X(34)       VALUE SPACES.
           05                          PIC X(11)    VALUE 'ASHRALS LTD'.
           05 FILLER                   PIC X(35)       VALUE SPACES.

       01 HEADING-TWO.
           05 FILLER                   PIC X(10)       VALUE SPACES.
           05 H2-MONTH                 PIC 99.
           05                          PIC X           VALUE '/'.
           05 H2-DAY                   PIC 99.
           05                          PIC X           VALUE '/'.
           05 H2-YEAR                  PIC 9(2).
           05 FILLER                   PIC X(12)       VALUE SPACES.
           05                          PIC X(16)       VALUE 
                                               'INVENTORY REPORT'.
           05 FILLER                   PIC X(20)       VALUE SPACES.
           05                          PIC XXX         VALUE 'TWJ'.
           05 FILLER                   PIC X(9)        VALUE SPACES.

       01 VENDOR-HEADING.
           05 FILLER                   PIC X(5)        VALUE SPACES.
           05                          PIC X(8)        VALUE 'VENDOR: '.
           05 VH-VENDOR-ID             PIC X(13).
           05 FILLER                   PIC X(54)       VALUE SPACES.

       01 WAREHOUSE-HEADING.
           05 FILLER                   PIC X(2)        VALUE SPACES.
           05                          PIC X(11)       VALUE 
                                               'WAREHOUSE: '.
           05 WH-WAREHOUSE-ID          PIC X(11).
           05 FILLER                   PIC X(56)       VALUE SPACES. 

       01 COSTUME-HEADER-ONE.
           05 FILLER                   PIC X(11)       VALUE SPACES.
           05                          PIC X(7)        VALUE 'COSTUME'.
           05 FILLER                   PIC X(7)        VALUE SPACES.
           05                          PIC X(7)        VALUE 'COSTUME'.
           05 FILLER                   PIC X(4)        VALUE SPACES.
           05                          PIC X(7)        VALUE 'COSTUME'.
           05 FILLER                   PIC X(3)        VALUE SPACES.
           05                          PIC X(6)        VALUE 'QTY IN'.
           05 FILLER                   PIC X(5)        VALUE SPACES.
           05                          PIC X(5)        VALUE 'TOTAL'.
           05 FILLER                   PIC X(18)       VALUE SPACES.

       01 COSTUME-HEADER-TWO.
           05 FILLER                   PIC X(13)       VALUE SPACES.
           05                          PIC X(4)        VALUE 'NAME'.
           05 FILLER                   PIC X(9)        VALUE SPACES.
           05                          PIC X(4)        VALUE 'SIZE'.
           05 FILLER                   PIC X(7)        VALUE SPACES.
           05                          PIC X(4)        VALUE 'TYPE'.
           05 FILLER                   PIC X(5)        VALUE SPACES.
           05                          PIC X(5)        VALUE 'STOCK'.
           05 FILLER                   PIC X(6)        VALUE SPACES.
           05                          PIC X(4)        VALUE 'COST'.
           05 FILLER                   PIC X(19)       VALUE SPACES.

       01 DETAIL-LINE.
           05 FILLER                   PIC X(10)       VALUE SPACES.
           05 DL-COSTUME-NAME          PIC X(9).
           05 FILLER                   PIC X(4)        VALUE SPACES.
           05 DL-COSTUME-SIZE          PIC X(8).
           05 FILLER                   PIC X(5)        VALUE SPACES.
           05 DL-COSTUME-TYPE          PIC X(5).
           05 FILLER                   PIC X(5)        VALUE SPACES.
           05 DL-QTY-IN-STOCK          PIC ZZ9.
           05 FILLER                   PIC X(4)        VALUE SPACES.
           05 DL-TOTAL-COST            PIC $ZZ,ZZ9.99.
           05 FILLER                   PIC X(16)       VALUE SPACES.

       01 COSTUME-TOTAL-LINE.
           05 FILLER                   PIC X(43)       VALUE SPACES.
           05                          PIC X(6)        VALUE 'TOTAL:'.
           05 FILLER                   PIC X(4)        VALUE SPACES.
           05 CTL-COSTUME-TOTAL        PIC $ZZZ,ZZ9.99.
           05 FILLER                   PIC X(16)       VALUE SPACES.

       01 WAREHOUSE-TOTAL-LINE.
           05 FILLER                   PIC X(14)       VALUE SPACES.
           05                          PIC X(20)       VALUE 
                                               'TOTAL FOR WAREHOUSE:'.
           05 FILLER                   PIC XX          VALUE SPACES.
           05 WTL-WAREHOUSE-ID         PIC X(11).
           05 FILLER                   PIC X(4)        VALUE SPACES.
           05 WTL-WAREHOUSE-TOTAL      PIC $Z,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(16)       VALUE SPACES.

       01 VENDOR-TOTAL-LINE.
           05 FILLER                   PIC X(17)       VALUE SPACES.
           05                          PIC X(17)       VALUE
                                                   'TOTAL FOR VENDOR:'.
           05 FILLER                   PIC XX          VALUE SPACES.
           05 VTL-VENDOR-ID            PIC X(13).
           05 FILLER                   PIC X           VALUE SPACES.
           05 VTL-VENDOR-TOTAL         PIC $ZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(16)       VALUE SPACES.

       01 GRAND-TOTAL-LINE.
           05 FILLER                   PIC X(29)       VALUE SPACES.
           05                          PIC X(17)       VALUE 
                                                   'GRAND TOTAL COST:'.
           05 FILLER                   PIC XXX         VALUE SPACES.
           05 GTL-GRAND-TOTAL          PIC $ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(16)       VALUE SPACES.


       PROCEDURE DIVISION.
       
       10-CONTROL-MODULE.
           PERFORM 23-CREATE-INFILE
           PERFORM 15-HOUSEKEEPING
           PERFORM 25-PROCESS-INFILE
           PERFORM 100-WRAP-UP
           .

       15-HOUSEKEEPING.
           
           OPEN INPUT MERGED-INVENTORY-FILE
               OUTPUT REPORT-FILE, ERROR-FILE

           ACCEPT WS-DATE FROM DATE
           MOVE WS-MONTH TO H2-MONTH
           MOVE WS-DAY TO H2-DAY
           MOVE WS-YEAR TO H2-YEAR

           PERFORM 20-PRINT-HEADER
           .

       20-PRINT-HEADER.

           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PAGE

           MOVE HEADING-TWO TO REPORT-RECORD
           PERFORM 35-WRITE-LINE
           MOVE 3 TO NEW-LINES
           .

       23-CREATE-INFILE.

           SORT CH-SORT-FILE
               ON ASCENDING KEY CSG-VENDOR-ID, CSG-WAREHOUSE-ID, 
                                                   CSG-COSTUME-ID
               USING CH-IN-FILE
               GIVING CH-SORTED-FILE

           SORT NY-SORT-FILE
               ON ASCENDING KEY NSG-VENDOR-ID, NSG-WAREHOUSE-ID, 
                                                   NSG-COSTUME-ID
               USING NY-IN-FILE
               GIVING NY-SORTED-FILE

           SORT LA-SORT-FILE
               ON ASCENDING KEY LSG-VENDOR-ID, LSG-WAREHOUSE-ID, 
                                                   LSG-COSTUME-ID
               USING LA-IN-FILE
               GIVING LA-SORTED-FILE

           
           MERGE MERGE-FILE
               ON ASCENDING KEY MR-VENDOR-ID, MR-WAREHOUSE-ID, 
                                                   MR-COSTUME-ID
               USING CH-IN-FILE, NY-IN-FILE, LA-IN-FILE
               GIVING MERGED-INVENTORY-FILE
           .

       25-PROCESS-INFILE.

           PERFORM UNTIL EOF-FLAG = 'T'
               READ MERGED-INVENTORY-FILE
                   AT END
                       MOVE 'T' TO EOF-FLAG
                       PERFORM 75-CLOSE-INFILE
                   NOT AT END
                       PERFORM 30-PROCESS-INFILE-LINE
               END-READ
           END-PERFORM
           .

       30-PROCESS-INFILE-LINE.

           IF HF-VENDOR-ID = 'FRST'
               PERFORM 31-EVALUATE-ID

               IF VALID-VENDOR AND VALID-WAREHOUSE
                   MOVE VENDOR-HEADING TO REPORT-RECORD
                   PERFORM 35-WRITE-LINE
                   MOVE 2 TO NEW-LINES
    
                   MOVE WAREHOUSE-HEADING TO REPORT-RECORD
                   PERFORM 35-WRITE-LINE
                   MOVE 3 TO NEW-LINES
    
                   MOVE COSTUME-HEADER-ONE TO REPORT-RECORD
                   PERFORM 35-WRITE-LINE
                   MOVE 1 TO NEW-LINES
                   MOVE COSTUME-HEADER-TWO TO REPORT-RECORD
                   PERFORM 35-WRITE-LINE
                   MOVE 2 TO NEW-LINES
    
                   MOVE IR-COSTUME-NAME(1) TO HF-COSTUME-NAME
                   MOVE IR-WAREHOUSE-ID TO HF-WAREHOUSE-ID
                   MOVE IR-VENDOR-ID TO HF-VENDOR-ID
               END-IF

           ELSE
               IF HF-VENDOR-ID NOT = IR-VENDOR-ID
                   PERFORM 31-EVALUATE-ID

                   IF VALID-VENDOR AND VALID-WAREHOUSE
                       PERFORM 40-COSTUME-BREAK
                       PERFORM 41-WAREHOUSE-BREAK
                       PERFORM 42-VENDOR-BREAK

                       PERFORM 20-PRINT-HEADER
                       MOVE VENDOR-HEADING TO REPORT-RECORD
                       PERFORM 35-WRITE-LINE
                       MOVE 2 TO NEW-LINES
        
                       MOVE WAREHOUSE-HEADING TO REPORT-RECORD
                       PERFORM 35-WRITE-LINE
                       MOVE 3 TO NEW-LINES
        
                       MOVE COSTUME-HEADER-ONE TO REPORT-RECORD
                       PERFORM 35-WRITE-LINE
                       MOVE 1 TO NEW-LINES
                       MOVE COSTUME-HEADER-TWO TO REPORT-RECORD
                       PERFORM 35-WRITE-LINE
                       MOVE 2 TO NEW-LINES
                   END-IF

               ELSE 
                   IF HF-WAREHOUSE-ID NOT = IR-WAREHOUSE-ID
                       PERFORM 31-EVALUATE-ID

                       IF VALID-VENDOR AND VALID-WAREHOUSE
                           PERFORM 40-COSTUME-BREAK
                           PERFORM 41-WAREHOUSE-BREAK

                           MOVE WAREHOUSE-HEADING TO REPORT-RECORD
                           PERFORM 35-WRITE-LINE
                           MOVE 3 TO NEW-LINES
            
                           MOVE COSTUME-HEADER-ONE TO REPORT-RECORD
                           PERFORM 35-WRITE-LINE
                           MOVE 1 TO NEW-LINES
                           MOVE COSTUME-HEADER-TWO TO REPORT-RECORD
                           PERFORM 35-WRITE-LINE
                           MOVE 2 TO NEW-LINES
                       END-IF
                   END-IF


                   IF HF-COSTUME-NAME NOT = IR-COSTUME-NAME(1)
                       PERFORM 40-COSTUME-BREAK
    
                       MOVE COSTUME-HEADER-ONE TO REPORT-RECORD
                       PERFORM 35-WRITE-LINE
                       MOVE 1 TO NEW-LINES
                       MOVE COSTUME-HEADER-TWO TO REPORT-RECORD
                       PERFORM 35-WRITE-LINE
                       MOVE 2 TO NEW-LINES
                   END-IF

                  
               END-IF
           END-IF


           MOVE IR-COSTUME-NAME(1) TO DL-COSTUME-NAME
           PERFORM VARYING SUB FROM 1 BY 1
               UNTIL SUB > 6

               SET COSTUME-SIZE-INDEX TO 1
               SEARCH COSTUME-SIZE-TBL-LINE
                   AT END
                       STRING
                           'BAD-' DELIMITED BY SIZE
                           IR-COSTUME-SIZE(SUB) DELIMITED BY SIZE
                       INTO DL-COSTUME-SIZE
                       END-STRING

                   WHEN IR-COSTUME-SIZE(SUB) = 
                                   CST-CODE(COSTUME-SIZE-INDEX)
                       MOVE CST-VALUE(COSTUME-SIZE-INDEX) TO 
                                          DL-COSTUME-SIZE     
               END-SEARCH

               EVALUATE IR-COSTUME-TYPE(SUB)
                   WHEN 'A'
                       MOVE 'Adult' TO DL-COSTUME-TYPE

                   WHEN 'C'
                       MOVE 'Child' TO DL-COSTUME-TYPE

                   WHEN OTHER
                       STRING
                           'BAD-' DELIMITED BY size
                           IR-COSTUME-TYPE(SUB) DELIMITED BY size
                       INTO DL-COSTUME-TYPE
                       END-STRING
               END-EVALUATE


               IF IR-NUM-IN-STOCK(SUB) IS NUMERIC
                   MOVE IR-NUM-IN-STOCK(SUB) TO DL-QTY-IN-STOCK
    
                   IF IR-COSTUME-PRICE(SUB) IS NUMERIC
                       MULTIPLY IR-COSTUME-PRICE(SUB) 
                               BY IR-NUM-IN-STOCK(SUB)
                               GIVING HF-CALCULATED-COST

                       MOVE HF-CALCULATED-COST TO DL-TOTAL-COST
    
                   ELSE
                      MOVE ZEROES TO HF-CALCULATED-COST
                      MOVE ZEROES TO DL-QTY-IN-STOCK
                      MOVE ZEROES TO DL-TOTAL-COST
                   END-IF
    
               ELSE
                   MOVE ZEROES TO DL-QTY-IN-STOCK
                   MOVE ZEROES TO HF-CALCULATED-COST
               END-IF
    
    
               ADD HF-CALCULATED-COST TO RT-VENDOR-TOTAL
               ADD HF-CALCULATED-COST TO RT-WAREHOUSE-TOTAL
               ADD HF-CALCULATED-COST TO RT-COSTUME-TOTAL
               ADD HF-CALCULATED-COST TO RT-GRAND-TOTAL

                   
               IF IR-COSTUME-DATA(SUB) NOT = SPACES AND VALID-VENDOR AND
                                                       VALID-WAREHOUSE
                   MOVE DETAIL-LINE TO REPORT-RECORD
                   PERFORM 35-WRITE-LINE
               END-IF
    
               MOVE SPACES TO DL-COSTUME-NAME
               MOVE 1 TO NEW-LINES

           END-PERFORM

           IF INVALID-VENDOR OR INVALID-WAREHOUSE
               MOVE INVENTORY-RECORD TO ERROR-RECORD
               WRITE ERROR-RECORD
           END-IF
           .

       31-EVALUATE-ID.
           MOVE 'Y' TO VENDOR-CHECK, WAREHOUSE-CHECK

           EVALUATE IR-VENDOR-ID
               WHEN 'LA10'
                   MOVE 'Los Angeles' TO VH-VENDOR-ID
        
               WHEN 'CH20'
                   MOVE 'Chicago' TO VH-VENDOR-ID
        
               WHEN 'NY30'
                   MOVE 'New York City' TO VH-VENDOR-ID
        
               WHEN OTHER
                   STRING
                       'INVALID-' DELIMITED BY SIZE
                       IR-VENDOR-ID DELIMITED BY SIZE
                   INTO VH-VENDOR-ID
                   END-STRING

                   MOVE 'N' TO VENDOR-CHECK
           END-EVALUATE

           EVALUATE IR-WAREHOUSE-ID
                   WHEN 'BHM'
                       MOVE 'Birmingham' TO WH-WAREHOUSE-ID
    
                   WHEN 'HUN'
                       MOVE 'Huntsville' TO WH-WAREHOUSE-ID
    
                   WHEN OTHER
                       STRING
                           'INVALID-' DELIMITED BY SIZE
                           IR-WAREHOUSE-ID DELIMITED BY SIZE
                       INTO WH-WAREHOUSE-ID
                       END-STRING

                       MOVE 'N' TO WAREHOUSE-CHECK
               END-EVALUATE

           .

       35-WRITE-LINE.
           WRITE REPORT-RECORD
               AFTER ADVANCING NEW-LINES
           .

       40-COSTUME-BREAK.
           MOVE RT-COSTUME-TOTAL TO CTL-COSTUME-TOTAL
           MOVE COSTUME-TOTAL-LINE TO REPORT-RECORD
           MOVE 2 TO NEW-LINES
           PERFORM 35-WRITE-LINE

           MOVE ZEROES TO RT-COSTUME-TOTAL
           MOVE IR-COSTUME-NAME(1) TO HF-COSTUME-NAME
           .

       41-WAREHOUSE-BREAK.
           EVALUATE HF-WAREHOUSE-ID
               WHEN 'BHM'
                   MOVE 'Birmingham' TO WTL-WAREHOUSE-ID

               WHEN 'HUN'
                   MOVE 'Huntsville' TO WTL-WAREHOUSE-ID

               WHEN OTHER
                   STRING
                       'INVALID-' DELIMITED BY SIZE
                       HF-WAREHOUSE-ID DELIMITED BY SIZE
                   INTO WTL-WAREHOUSE-ID
                   END-STRING
           END-EVALUATE

           MOVE RT-WAREHOUSE-TOTAL TO WTL-WAREHOUSE-TOTAL
           MOVE WAREHOUSE-TOTAL-LINE TO REPORT-RECORD
           PERFORM 35-WRITE-LINE

           MOVE ZEROES TO RT-WAREHOUSE-TOTAL
           MOVE IR-WAREHOUSE-ID TO HF-WAREHOUSE-ID
           .

       42-VENDOR-BREAK.
           EVALUATE HF-VENDOR-ID
               WHEN 'LA10'
                   MOVE 'Los Angeles' TO VTL-VENDOR-ID

               WHEN 'CH20'
                   MOVE 'Chicago' TO VTL-VENDOR-ID

               WHEN 'NY30'
                   MOVE 'New York City' TO VTL-VENDOR-ID

               WHEN OTHER
                   STRING
                       'INVALID-' DELIMITED BY SIZE
                       HF-VENDOR-ID DELIMITED BY SIZE
                   INTO VTL-VENDOR-ID
                   END-STRING
           END-EVALUATE


           MOVE RT-VENDOR-TOTAL TO VTL-VENDOR-TOTAL
           MOVE VENDOR-TOTAL-LINE TO REPORT-RECORD
           PERFORM 35-WRITE-LINE

           MOVE ZEROES TO RT-VENDOR-TOTAL
           MOVE IR-VENDOR-ID TO HF-VENDOR-ID
           .

       75-CLOSE-INFILE.
           PERFORM 40-COSTUME-BREAK
           PERFORM 41-WAREHOUSE-BREAK
           PERFORM 42-VENDOR-BREAK

           MOVE RT-GRAND-TOTAL TO GTL-GRAND-TOTAL
           MOVE GRAND-TOTAL-LINE TO REPORT-RECORD
           PERFORM 35-WRITE-LINE

           CLOSE MERGED-INVENTORY-FILE
           .

       100-WRAP-UP.
           CLOSE REPORT-FILE, ERROR-FILE

           STOP RUN
           .

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LAB7TWJ.
       AUTHOR.        Tim J.
   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT TRAIN-FILE
                ASSIGN TO 'TRAIN-FILE.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

            SELECT REPORT-FILE
                ASSIGN TO 'TRAINREPORT.TXT'.

       DATA DIVISION.
       FILE SECTION.

       FD TRAIN-FILE.

       01 TRAIN-RECORD.
          05 TI-STATION-CODE             PIC X(5).
          05 TI-STATION-MANAGER          PIC X(15).
          05                             PIC X(5).
          05 TI-STATION-CITY             PIC X(10).

          05 TI-TRAIN-ARRAY   OCCURS 5 TIMES.
             10 TI-TRAIN-NAME            PIC X(5).
             10 TI-TRAIN-REPAIR-STATUS   PIC XX.
             10 TI-TRAIN-REPAIR-LOC      PIC XXX.


       FD REPORT-FILE.

       01 REPORT-RECORD                   PIC X(80).

       WORKING-STORAGE SECTION.

       01 FLAGS-AND-SWITCHES.
          05 END-OF-FILE-FLAG           PIC X  VALUE SPACE.
             88 MORE-RECORDS                   VALUE 'Y'.
             88 NO-MORE-RECORDS                VALUE 'N'.

       01 REPORT-FIELDS.
          05 PROPER-SPACING PIC 9 VALUE 1.

           05 IND            PIC 9.


       01 WS-DATE.
          05 WS-YEAR           PIC 9999.
          05 WS-MONTH          PIC 99.
          05 WS-DAY            PIC 99.

       01 TRAIN-REPAIR-CODE.
          05 TC-00             PIC X(14)  VALUE 'RUNNING'.
          05 TC-RR             PIC X(14)  VALUE 'BEING REPAIRED'.
          05 TC-XX             PIC X(14)  VALUE 'BEING SCRAPPED'.

       01 TRAIN-LOCATION-CODE.
          05 TL-RT1             PIC X(7)  VALUE 'TRACK 1'.
          05 TL-RT2             PIC X(7)  VALUE 'TRACK 2'.
          05 TL-RT3             PIC X(7)  VALUE 'TRACK 3'.

      **********************OUTPUT AREA**********************

       01  HEADING-1.
           05                              PIC X(6) VALUE 'DATE:'.
           05  H1-DATE.
               10  H1-MONTH                PIC Z9.
               10                          PIC X    VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-YEAR                 PIC 9999.
           05                              PIC X(7) VALUE SPACES.
           05                              PIC X(25) VALUE
                                           'TRAIN REPORT'.
           05                              PIC X(17) VALUE 'TWJ'.

      *
       01  HEADING-2.
           05                              PIC X(5) VALUE SPACES.
           05                              PIC X(15) VALUE
                                               'STATION CODE: '.
           05                              PIC X(5) VALUE SPACES.
           05 H2-STATION-CODE              PIC X(5).
      *
       01  HEADING-3.
           05                         PIC X(5)  VALUE SPACES.
           05                         PIC X(10) VALUE 'TRAIN NAME'.
           05                         PIC X(10)  VALUE SPACES.
           05                         PIC X(8) VALUE 'LOCATION'.
           05                         PIC X(7)  VALUE SPACES.
           05                         PIC X(8)  VALUE 'STATUS'.

      *
       01  DETAIL-LINE.
           05                              PIC X(8) VALUE SPACES.
           05  DL-TRAIN-NAME               PIC X(5).
           05                              PIC X(12) VALUE SPACES.
           05  DL-TRAIN-LOCATION           PIC X(7).
           05                              PIC X(5) VALUE SPACES.
           05  DL-REPAIR-STATUS            PIC X(14).




       PROCEDURE DIVISION.

       100-MAIN.

           PERFORM 200-HOUSEKEEPING
           PERFORM 300-READ-ROUTINE
           PERFORM 600-EOJ-ROUTINE
          .

       200-HOUSEKEEPING.

           OPEN INPUT  TRAIN-FILE
                OUTPUT REPORT-FILE

           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR

           PERFORM 225-REPORT-HEADER
          .

       225-REPORT-HEADER.
           
             WRITE REPORT-RECORD FROM HEADING-1
             AFTER ADVANCING PAGE
             MOVE 3 TO PROPER-SPACING
          .
   
       250-STATION-HEADER.
             MOVE TI-STATION-CODE TO H2-STATION-CODE 
 
             MOVE 3 TO PROPER-SPACING       
             WRITE REPORT-RECORD FROM HEADING-2
                 AFTER ADVANCING PROPER-SPACING
             MOVE 2 TO PROPER-SPACING

             WRITE REPORT-RECORD FROM HEADING-3
                 AFTER ADVANCING PROPER-SPACING
             MOVE 2 TO PROPER-SPACING 
          .

       300-READ-ROUTINE.

           PERFORM UNTIL NO-MORE-RECORDS
            READ TRAIN-FILE
               AT END
                   MOVE 'N' TO END-OF-FILE-FLAG
               NOT AT END
                   PERFORM 400-PROCESS-ROUTINE
             END-READ
            END-PERFORM
          .

       400-PROCESS-ROUTINE.

           PERFORM 250-STATION-HEADER

  
           PERFORM VARYING IND
                   FROM 1 BY 1 UNTIL IND > 5


                PERFORM 500-EVALUATE-LOCATION

                PERFORM 550-EVALUATE-REPAIR

               MOVE TI-TRAIN-NAME(IND) TO DL-TRAIN-NAME

                WRITE REPORT-RECORD FROM DETAIL-LINE AFTER
                 ADVANCING PROPER-SPACING
                MOVE 1 TO PROPER-SPACING

           END-PERFORM
          .

       500-EVALUATE-LOCATION.


           EVALUATE TRUE
               WHEN TI-TRAIN-REPAIR-STATUS(IND) = '00'
                   MOVE TC-00 TO DL-REPAIR-STATUS

               WHEN TI-TRAIN-REPAIR-STATUS(IND) = 'RR'
                   MOVE TC-RR TO DL-REPAIR-STATUS

               WHEN TI-TRAIN-REPAIR-STATUS(IND) = 'XX'
                   MOVE TC-XX TO DL-REPAIR-STATUS

               END-EVALUATE





       .

       550-EVALUATE-REPAIR.


           EVALUATE TRUE
               WHEN TI-TRAIN-REPAIR-STATUS(IND) = '00'
                   MOVE TC-00 TO DL-TRAIN-LOCATION

               WHEN TI-TRAIN-REPAIR-LOC(IND) = 'RT1'
                       MOVE TL-RT1 TO DL-TRAIN-LOCATION

               WHEN TI-TRAIN-REPAIR-LOC(IND) = 'RT2'
                       MOVE TL-RT2 TO DL-TRAIN-LOCATION

               WHEN TI-TRAIN-REPAIR-LOC(IND) = 'RT3'
                       MOVE TL-RT3 TO DL-TRAIN-LOCATION

           END-EVALUATE







       .

       600-EOJ-ROUTINE.
           CLOSE TRAIN-FILE
                 REPORT-FILE
           STOP RUN
          .



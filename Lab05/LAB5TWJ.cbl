       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAB5TWJ.
       AUTHOR. Tim J.

       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRADE-FILE
               ASSIGN TO 'GRADES.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REPORT-FILE
               ASSIGN TO 'GRADEREPORT.TXT'.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  GRADE-FILE
           RECORD CONTAINS 66 CHARACTERS.
      *
       01  GF-RECORD.
      *
           05  GF-COURSE-NUM           PIC X(5).
           05  GF-NAME.
               10  GF-LASTNAME         PIC X(9).
               10  GF-FIRSTNAME        PIC X(11).
           05  GF-GRADE1			   PIC 999.
           05  GF-GRADE2               PIC 999.
           05  GF-GRADE3               PIC 999.
           05  GF-GRADE4               PIC 999.
           05 FILLER                   PIC X(29).
      *
       FD  REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  REPORT-REC               PIC X(80).
      *

       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG            PIC X         VALUE 'Y'.
               88  NO-MORE-DATA                  VALUE 'N'.
               88  MORE-DATA                     VALUE 'Y'.
      *
       01  CURRENT-DATE.
           05  CD-YEAR             PIC XX.
           05  CD-MONTH            PIC XX.
           05  CD-DAY              PIC XX.
      *
       01  DETAIL-FIELDS.
           05  DF-AVG              PIC S999V9    VALUE +0.
           05  DF-SUM-GRADES       PIC S9(4)     VALUE +0.
      *
       01  TOTAL-FIELDS.
           05  TF-CLASS-AVG        PIC S999V9    VALUE +0.
           05  TF-SUM-AVERAGES     PIC S9(4)V9   VALUE +0.
           05  TF-NUM-STUDENTS     PIC S99       VALUE +0.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING      PIC S9        VALUE +1.
      *
       01  CONSTANT-FIELDS.
           05  CF-NUM-TESTS        PIC S9        VALUE +4.
      **************************OUTPUT AREA***************************
       01  HEADER-1.
           05                      PIC X(5)      VALUE SPACES.
           05                      PIC X(22)     VALUE 'TWJ'.
           05                      PIC X(28)     VALUE 'CLASS GRADES'.
           05  H1-DATE.
               10  H1-MONTH        PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-DAY          PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-YEAR         PIC XX.
      *
       01  HEADER-2.
           05                      PIC X(6)      VALUE SPACES.
           05                      PIC X(6)      VALUE 'COURSE'.
           05                      PIC X(13)     VALUE SPACES.
           05                      PIC X(4)      VALUE 'NAME'.
           05                      PIC X(13)     VALUE SPACES.
           05                      PIC X(7)      VALUE 'AVERAGE'.
           05                      PIC X(5)      VALUE SPACES.
           05                      PIC X(5)      VALUE 'GRADE'.
      *
       01  DETAIL-LINE.
           05                      PIC X(8)     VALUE SPACES.
           05  DL-COURSE-NUM       PIC X(5).
           05                      PIC X(5)     VALUE SPACES.
           05  DL-NAME             PIC X(20).
           05                      PIC X(5)     VALUE SPACES.
           05  DL-AVERAGE          PIC ZZ9.9.
           05                      PIC X(8).
           05  DL-LETTER-GRADE     PIC X.
      *
       01  TOTAL-LINE.
           05                      PIC X(37)     VALUE SPACES.
           05                      PIC X(16)     VALUE 'CLASS AVERAGE'.
           05  TL-CLASS-AVG        PIC ZZ9.9.

      /
       PROCEDURE DIVISION.
      *                        CGJ
       10-CONTROL-MODULE.

           PERFORM 15-HOUSEKEEPING
           PERFORM 20-READ-A-REC
           PERFORM 45-COURSE-TOTALS
           PERFORM 50-EOF-ROUTINE
           .

       15-HOUSEKEEPING.

           OPEN INPUT GRADE-FILE
               OUTPUT REPORT-FILE

           ACCEPT CURRENT-DATE FROM DATE
           MOVE CD-MONTH TO H1-MONTH
           MOVE CD-DAY TO H1-DAY
           MOVE CD-YEAR TO H1-YEAR
           PERFORM 25-HEADER-ROUTINE
           .

       20-READ-A-REC.

          PERFORM UNTIL NO-MORE-DATA
           READ GRADE-FILE
               AT END
                    MOVE 'N' TO EOF-FLAG
               NOT AT END
                   PERFORM 30-FIND-INDIVIDUAL-AVG
           END-READ
          END-PERFORM
           .

       25-HEADER-ROUTINE.

           WRITE REPORT-REC FROM HEADER-1
               AFTER ADVANCING PAGE

           MOVE 3 TO PROPER-SPACING

           MOVE HEADER-2 TO REPORT-REC
           PERFORM 40-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
           .

       30-FIND-INDIVIDUAL-AVG.

           MOVE GF-COURSE-NUM TO DL-COURSE-NUM
           MOVE GF-NAME TO DL-NAME
 
      *  ADD THE 4 GRADES TO GET AN AVERAGE
           ADD GF-GRADE1, GF-GRADE2, GF-GRADE3, GF-GRADE4
                                               GIVING DF-SUM-GRADES




      *  GET THE STUDENT AVERAGE

           DIVIDE DF-SUM-GRADES BY CF-NUM-TESTS GIVING DF-AVG



           MOVE DF-AVG TO DL-AVERAGE

           PERFORM 35-EVALUATE-GRADE

           MOVE DETAIL-LINE TO REPORT-REC
           PERFORM 40-WRITE-A-LINE

           MOVE 1 TO PROPER-SPACING

           ADD 1 TO TF-NUM-STUDENTS
           ADD DF-AVG TO TF-SUM-AVERAGES
           MOVE ZEROS TO DF-SUM-GRADES
           .

       35-EVALUATE-GRADE.

           EVALUATE TRUE
               WHEN DF-AVG >= 90
                   MOVE 'A' TO DL-LETTER-GRADE

               WHEN DF-AVG >= 80
                   MOVE 'B' TO DL-LETTER-GRADE

               WHEN DF-AVG >= 70
                   MOVE 'C' TO DL-LETTER-GRADE

               WHEN DF-AVG >= 60
                   MOVE 'D' TO DL-LETTER-GRADE

               WHEN OTHER
                   MOVE 'F' TO DL-LETTER-GRADE







           .

       40-WRITE-A-LINE.

           WRITE REPORT-REC
               AFTER ADVANCING PROPER-SPACING
           .

       45-COURSE-TOTALS.

           DIVIDE TF-NUM-STUDENTS INTO TF-SUM-AVERAGES
                                   GIVING TF-CLASS-AVG



           MOVE TF-CLASS-AVG TO TL-CLASS-AVG
           MOVE TOTAL-LINE TO REPORT-REC
           MOVE 2 TO PROPER-SPACING
           PERFORM 40-WRITE-A-LINE
           .

       50-EOF-ROUTINE.

           CLOSE GRADE-FILE
                 REPORT-FILE

           STOP RUN
           .

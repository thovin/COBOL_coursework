       IDENTIFICATION DIVISION.
       PROGRAM-ID.     LAB10TWJ.
       AUTHOR.         Tim J.

       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.

           SELECT UNSORTED-STU-FILE1
               ASSIGN TO 'UNSORTEDSTUDENT1.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT UNSORTED-STU-FILE2
               ASSIGN TO 'UNSORTEDSTUDENT2.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT SORTED-STU-FILE1
               ASSIGN TO 'SORTEDSTUDENT1.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT SORTED-STU-FILE2
               ASSIGN TO 'SORTEDSTUDENT2.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT MERGED-SORTED-FILE
               ASSIGN TO 'MERGEDSORTEDSTUDENT.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
      *

           SELECT SORT-FILE-ONE
               ASSIGN TO 'SORTING1.TMP'.

           SELECT SORT-FILE-TWO
               ASSIGN TO 'SORTING1.TMP'.

           SELECT MERGEFILE
               ASSIGN TO 'MERGING.TMP'.

      *
           SELECT STUDENT-REPORT-FILE
               ASSIGN TO PRINTER 'L10STUDENTREPORT.TXT'.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD UNSORTED-STU-FILE1
           RECORD CONTAINS 41 CHARACTERS.
       01  UNSORTED-RECORD1.
           05  UR1-DEPT-CODE                    PIC A(4).
           05  UR1-CLASS-CODE                   PIC X(5).
           05  FILLER                          PIC X(32).
      *
       FD UNSORTED-STU-FILE2
           RECORD CONTAINS 41 CHARACTERS.
       01  UNSORTED-RECORD2.
           05  UR2-DEPT-CODE                    PIC A(4).
           05  UR2-CLASS-CODE                   PIC X(5).
           05  FILLER                          PIC X(32).
      *
       FD SORTED-STU-FILE1
           RECORD CONTAINS 41 CHARACTERS.
       01  SORTED-RECORD1.
           05  SR1-DEPT-CODE                    PIC A(4).
           05  SR1-CLASS-CODE                   PIC X(5).
           05  FILLER                          PIC X(32).
      *
       FD SORTED-STU-FILE2
           RECORD CONTAINS 41 CHARACTERS.
       01  STORTEDT-RECORD2.
           05  SR2-DEPT-CODE                    PIC A(4).
           05  SR2-CLASS-CODE                   PIC X(5).
           05  FILLER                          PIC X(32).
      *
       FD MERGED-SORTED-FILE
           RECORD CONTAINS 41 CHARACTERS.
       01  MERGED-SORTED-REC.
           05  MS-DEPT-CODE                    PIC A(4).
           05  MS-CLASS-CODE                   PIC X(5).
           05  MS-NAME                         PIC X(20).
           05  MS-TEST OCCURS 4 TIMES           PIC 9(3).
      *
      *
       SD SORT-FILE-ONE
           RECORD CONTAINS 41 CHARACTERS.
       01 SORT-RECORD-ONE.
           05 SRO-DEPT-CODE                    PIC A(4).
           05 SRO-CLASS-CODE                   PIC X(5).
           05 FILLER                           PIC X(32).

       SD SORT-FILE-TWO
           RECORD CONTAINS 41 CHARACTERS.
       01 SORT-RECORD-TWO.
           05 SRT-DEPT-CODE                    PIC A(4).
           05 SRT-CLASS-CODE                   PIC X(5).
           05 FILLER                           PIC X(32).

       SD MERGEFILE
           RECORD CONTAINS 41 CHARACTERS.
       01 MERGE-RECORD.
           05 MR-DEPT-CODE                     PIC A(4).
           05 MR-CLASS-CODE                    PIC X(5).
           05 FILLER                           PIC X(32).




      *
       FD  STUDENT-REPORT-FILE
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
       01 SUBSCRIPTS.
           05  SUB                         PIC 9       VALUE ZERO.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING              PIC S9      VALUE +1.
      *
       01  DETAIL-FIELDS.
           05  DF-TEST-TOTAL                PIC S9(5)  VALUE +0.
           05  DF-TEST-AVERAGE              PIC S9(5)V99 VALUE +0.
      *
       01  CONSTANT-FIELDS.
           05  CF-NUM-TESTS                PIC 99    VALUE 4.
      *
       01  WS-CURRENT-DATE.
           05  WS-YEAR                     PIC 99.
           05  WS-MONTH                    PIC 99.
           05  WS-DAY                      PIC 99.

      **********************OUTPUT AREA**************************
       01  HEADING-ONE.
           05                              PIC X(6) VALUE 'DATE:'.
           05  H1-DATE.
               10  H1-MONTH                PIC Z9.
               10                          PIC X    VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-YEAR                 PIC 99.
           05                              PIC X(20) VALUE SPACES.
           05                              PIC X(36) VALUE
                                           'STUDENT REPORT'.
           05                              PIC X(13) VALUE 'TWJ'.
      *
       01  HEADING-FOUR.
           05                              PIC X(4)  VALUE 'DEPT'.
           05                              PIC X(3)  VALUE SPACES.
           05                              PIC X(5)  VALUE 'CLASS'.
           05                              PIC X(10) VALUE SPACES.
           05                              PIC X(11) VALUE 'NAME'.
           05                              PIC X(5)  VALUE SPACES.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(7)  VALUE 'GRADE'.
      *
       01  DETAIL-LINE.
           05  DL-DEPT                     PIC X(4).
           05                              PIC X(3) VALUE SPACES.
           05  DL-CLASS                    PIC X(5).
           05                              PIC X(3) VALUE SPACES.
           05  DL-NAME                     PIC X(20).
           05                              PIC X(5).
           05  DL-TEST OCCURS 4 TIMES      PIC XXXBBBBB.
           05  DL-GRADE                    PIC X.

      *
       PROCEDURE DIVISION.
      *
       10-PRINT-STUDENT-REPORT.
           PERFORM 15-SORT-MERGE-STU-FILES
           PERFORM 20-HSKPING-ROUTINE
           PERFORM 25-READ-STUDENT-FILE
           PERFORM 45-FINAL-ROUTINE
       .

       15-SORT-MERGE-STU-FILES.

           SORT SORT-FILE-ONE
               ON ASCENDING KEY SRO-DEPT-CODE, SRO-CLASS-CODE
               USING UNSORTED-STU-FILE1
               GIVING SORTED-STU-FILE1
           
           SORT SORT-FILE-TWO
               ON ASCENDING KEY SRT-DEPT-CODE, SRT-CLASS-CODE
               USING UNSORTED-STU-FILE2
               GIVING SORTED-STU-FILE2

           
           MERGE MERGEFILE
               ON ASCENDING KEY MR-DEPT-CODE, MR-CLASS-CODE
               USING SORTED-STU-FILE1, SORTED-STU-FILE2
               GIVING MERGED-SORTED-FILE







       .

       20-HSKPING-ROUTINE.

           OPEN INPUT MERGED-SORTED-FILE
               OUTPUT STUDENT-REPORT-FILE



           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR
           PERFORM 30-HEADING-ROUTINE
       .

       25-READ-STUDENT-FILE.
           PERFORM UNTIL NO-MORE-DATA
               READ MERGED-SORTED-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 35-PROCESS-STUDENT-RECORD
               END-READ
           END-PERFORM

       .
       30-HEADING-ROUTINE.

           WRITE REPORT-LINE FROM HEADING-ONE
               AFTER ADVANCING PAGE
           MOVE 2 TO PROPER-SPACING

           WRITE REPORT-LINE FROM HEADING-FOUR
               AFTER ADVANCING 2 LINES
       .

       35-PROCESS-STUDENT-RECORD.

           MOVE MS-DEPT-CODE TO DL-DEPT
           MOVE MS-CLASS-CODE TO DL-CLASS
           MOVE MS-NAME TO DL-NAME

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > CF-NUM-TESTS

               MOVE MS-TEST(SUB) TO DL-TEST(SUB)
               ADD MS-TEST(SUB) TO DF-TEST-TOTAL

           END-PERFORM

           DIVIDE DF-TEST-TOTAL BY CF-NUM-TESTS
                  GIVING DF-TEST-AVERAGE ROUNDED

           EVALUATE TRUE
               WHEN DF-TEST-AVERAGE > 89
                   MOVE 'A' TO DL-GRADE
               WHEN DF-TEST-AVERAGE >= 80 AND DF-TEST-AVERAGE <= 89
                   MOVE 'B' TO DL-GRADE
               WHEN DF-TEST-AVERAGE >= 70 AND DF-TEST-AVERAGE <= 79
                   MOVE 'C' TO DL-GRADE
               WHEN DF-TEST-AVERAGE >= 60 AND DF-TEST-AVERAGE <= 69
                   MOVE 'D' TO DL-GRADE
               WHEN DF-TEST-AVERAGE < 60
                   MOVE 'F' TO DL-GRADE
           END-EVALUATE

           MOVE DETAIL-LINE TO REPORT-LINE
           PERFORM 40-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           MOVE ZEROS TO DF-TEST-TOTAL
       .

       40-WRITE-A-LINE.
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
       .

       45-FINAL-ROUTINE.

           CLOSE MERGED-SORTED-FILE
                 STUDENT-REPORT-FILE

           STOP RUN
       .



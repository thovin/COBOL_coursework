       IDENTIFICATION DIVISION.
       PROGRAM-ID.     LAB6TWJ.
       AUTHOR.         Timothy J.

       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT STUDENT-FILE
               ASSIGN TO "STUDENT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT STUDENT-REPORT-FILE
               ASSIGN TO PRINTER "MULTIBREAKREPORT.TXT".
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD STUDENT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  STUDENT-RECORD.
           05  SR-DEPT-CODE                    PIC A(4).
           05  SR-CLASS-CODE                   PIC X(5).
           05  SR-NAME                         PIC X(20).
           05  SR-TEST1                        PIC 9(3).
           05  SR-TEST2                        PIC 9(3).
           05  SR-TEST3                        PIC 9(3).
           05  SR-TEST4                        PIC 9(3).
           05  FILLER                          PIC X(39).

      *
       FD  STUDENT-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  REPORT-LINE                     PIC X(80).

      *
       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X       VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.
           05  FIRST-RECORD                PIC X(3)    VALUE 'YES'.
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
           05  DF-TEST-TOTAL                PIC S9(5)    VALUE +0.
           05  DF-TEST-AVERAGE              PIC S9(5)V99 VALUE +0.
      *
       01  CLASS-FIELDS.
           05  CF-STUDENT-COUNT             PIC S99      VALUE +0.
      *
       01  DEPT-FIELDS.
           05  DF-STUDENT-COUNT             PIC S99      VALUE +0.      
      *
       01  HOLD-FIELDS.
           05 OLD-DEPT-CODE                PIC A(4)       VALUE "FIRS".
           05 DF-CLASS-HOLD                PIC X(5).



      ********************OUTPUT AREA*********************************

       01  HEADING-1.
           05                              PIC X(6) VALUE 'DATE:'.
           05  H1-DATE.
               10  H1-MONTH                PIC Z9.
               10                          PIC X    VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-YEAR                 PIC 99.
           05                              PIC X(7) VALUE SPACES.
           05                              PIC X(25) VALUE
                                           'STUDENT REPORT'.
           05                              PIC X(17) VALUE 'TWJ'.
           05                              PIC X(5) VALUE SPACES.
           05 H1-PAGE-NO                   PIC 99 VALUE ZERO.
      *
       01  HEADING-2.
           05                              PIC X(5) VALUE SPACES.
           05                              PIC X(20) VALUE
                                               'DEPARTMENT CODE: '.
           05                              PIC X(5) VALUE SPACES.
           05 H2-DEPT-CODE                 PIC A(4).
      *
       01  HEADING-3.
           05                              PIC X(5) VALUE SPACES.
           05                              PIC X(12) VALUE
                                               'CLASS CODE: '.
           05                              PIC X(5) VALUE SPACES.
           05 H3-CLASS-CODE                PIC X(5).
      *
       01  HEADING-4.
           05                              PIC X(19) VALUE SPACES.
           05                              PIC X(11) VALUE 'NAME'.
           05                              PIC X(3) VALUE SPACES.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(7) VALUE 'GRADE'.
      *
       01  DETAIL-LINE.
           05                              PIC X(7) VALUE SPACES.
           05  DL-NAME                     PIC X(20).
           05                              PIC X(7).
           05  DL-TEST1                    PIC XXXBBBBB.
           05  DL-TEST2                    PIC XXXBBBBB.
           05  DL-TEST3                    PIC XXXBBBBB.
           05  DL-TEST4                    PIC XXXBBBBB.
           05  DL-GRADE                    PIC X.

      *
       01  CLASS-GROUP-LINE.
           05                              PIC X(45)   VALUE
                            'TOTAL MUMBER OF STUDENTS FOR CLASS '.
           05  CGL-CLASS-CODE              PIC X(5).
           05                              PIC X(5)    VALUE ' IS  '.
           05  CGL-CLASS-TOTAL             PIC ZZ9.

       01  DEPART-GROUP-LINE.
           05                              PIC X(45) VALUE
                            'TOTAL NUMBER OF STUDENTS FOR DEPT '.
           05  DGL-DEPT-CODE               PIC A(4).
           05                              PIC X(6)    VALUE ' IS  '.
           05  DGL-DEPT-TOTAL              PIC ZZ9.


      *
       PROCEDURE DIVISION.
      *
       100-PRINT-STUDENT-REPORT.
           PERFORM 200-HSKPING-ROUTINE
           PERFORM 400-READ-STUDENT-FILE
           PERFORM 1100-END-OF-JOB-ROUTINE
           PERFORM 1200-FINAL-ROUTINE
        .

       200-HSKPING-ROUTINE.
           OPEN INPUT  STUDENT-FILE
                OUTPUT STUDENT-REPORT-FILE

           ACCEPT WS-CURRENT-DATE FROM DATE

           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR

           PERFORM 300-REPORT-HEADER
       .

       300-REPORT-HEADER.

           ADD 1 TO H1-PAGE-NO

           WRITE REPORT-LINE FROM HEADING-1
               AFTER ADVANCING PAGE
           MOVE 2 TO PROPER-SPACING
       .

       400-READ-STUDENT-FILE.

           PERFORM UNTIL NO-MORE-DATA
               READ STUDENT-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 700-PROCESS-STUDENT-RECORD
               END-READ
           END-PERFORM
       .

       500-PRINT-DEPT-HEADER.

           MOVE SR-DEPT-CODE TO H2-DEPT-CODE
           WRITE REPORT-LINE FROM HEADING-2
               AFTER ADVANCING 2 LINES
       .

       600-PRINT-CLASS-HEADER.

           MOVE SR-CLASS-CODE TO H3-CLASS-CODE
           WRITE REPORT-LINE FROM HEADING-3
               AFTER ADVANCING 2 LINES

           WRITE REPORT-LINE FROM HEADING-4
               AFTER ADVANCING 2 LINES
       .

       700-PROCESS-STUDENT-RECORD.
           EVALUATE TRUE
               WHEN OLD-DEPT-CODE = 'FIRS'
                   MOVE SR-DEPT-CODE TO OLD-DEPT-CODE
                   MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
                   PERFORM 500-PRINT-DEPT-HEADER
                   PERFORM 600-PRINT-CLASS-HEADER


               WHEN OLD-DEPT-CODE NOT = SR-DEPT-CODE
                   PERFORM 1000-CLASS-BREAK
                   PERFORM 900-DEPT-BREAK
                   MOVE SR-DEPT-CODE TO OLD-DEPT-CODE
                   PERFORM 500-PRINT-DEPT-HEADER
                   PERFORM 600-PRINT-CLASS-HEADER
               

               WHEN DF-CLASS-HOLD NOT = SR-CLASS-CODE
                   PERFORM 1000-CLASS-BREAK
                   PERFORM 600-PRINT-CLASS-HEADER


               
           END-EVALUATE
















      ***********************

           MOVE SR-NAME TO DL-NAME
           MOVE SR-TEST1 TO DL-TEST1
           MOVE SR-TEST2 TO DL-TEST2
           MOVE SR-TEST3 TO DL-TEST3
           MOVE SR-TEST4 TO DL-TEST4

           ADD SR-TEST1
               SR-TEST2
               SR-TEST3
               SR-TEST4 TO DF-TEST-TOTAL


           DIVIDE DF-TEST-TOTAL BY 4
                  GIVING DF-TEST-AVERAGE ROUNDED 

           ADD 1 TO CF-STUDENT-COUNT
                    DF-STUDENT-COUNT                                

          IF DF-TEST-AVERAGE > 89
                   MOVE 'A' TO DL-GRADE
          ELSE

             IF DF-TEST-AVERAGE >= 80 AND DF-TEST-AVERAGE <= 89
                   MOVE 'B' TO DL-GRADE
             ELSE

                IF DF-TEST-AVERAGE >= 70 AND DF-TEST-AVERAGE <= 79
                   MOVE 'C' TO DL-GRADE
                ELSE

                   IF DF-TEST-AVERAGE >= 60 AND DF-TEST-AVERAGE <= 69
                       MOVE 'D' TO DL-GRADE
                   ELSE

                       IF DF-TEST-AVERAGE < 60
                          MOVE 'F' TO DL-GRADE
                       END-IF
                    END-IF
                 END-IF
               END-IF
            END-IF


           MOVE DETAIL-LINE TO REPORT-LINE
           PERFORM 800-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING

           MOVE ZEROS TO DF-TEST-AVERAGE
           MOVE ZEROS TO DF-TEST-TOTAL

           .

       800-WRITE-A-LINE.
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
           .

      *
       900-DEPT-BREAK.

           MOVE SR-DEPT-CODE TO DGL-DEPT-CODE
           MOVE DF-STUDENT-COUNT TO DGL-DEPT-TOTAL
           MOVE DEPART-GROUP-LINE TO REPORT-LINE

           PERFORM 800-WRITE-A-LINE

           MOVE ZERO TO DF-STUDENT-COUNT





         .


       1000-CLASS-BREAK.

         MOVE DF-CLASS-HOLD TO CGL-CLASS-CODE
         MOVE CF-STUDENT-COUNT TO CGL-CLASS-TOTAL
         MOVE CLASS-GROUP-LINE TO REPORT-LINE
         MOVE 2 TO PROPER-SPACING
         PERFORM 800-WRITE-A-LINE

         MOVE ZEROS TO CF-STUDENT-COUNT
                       CGL-CLASS-TOTAL

         MOVE SR-CLASS-CODE TO DF-CLASS-HOLD

         .

       1100-END-OF-JOB-ROUTINE.
           PERFORM 1000-CLASS-BREAK
           PERFORM 900-DEPT-BREAK


        .


       1200-FINAL-ROUTINE.
           CLOSE STUDENT-FILE
                 STUDENT-REPORT-FILE

            STOP RUN
            .

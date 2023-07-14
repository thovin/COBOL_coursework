       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MathLab.
       AUTHOR.  Tim J.
      *********************************************************************************
      * INPUTS
      *    THE INPUTS ARE HARD CODED IN WORKING STORAGE
      *        A = 3
      *        B = 6
      *        C = 4
      *        D = 2
      *        E = 3
      ********
      * OUTPUT 1
      *    SOLVE THE EQUATION IN THE INSTRUCTION USING MATH VERBS
      ********
      * OUTPUT 2
      *    SOLVE THE EQUATION IN THE INSTRUCTION USING A COMPUTE STATEMENT
      ****************************************************************
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-PC.
       OBJECT-COMPUTER.  IBM-PC.

       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT MLAB-REPORT
               ASSIGN TO PRINTER "PRINTTWJ.TXT".
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  MLAB-REPORT
           RECORD CONTAINS 80 CHARACTERS.
       01  REPORT-RECORD       PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
       01  TEMP-STORAGE.
           05  ANS             PIC S99V999 VALUE +0.
           05  TEMP1           PIC S99V999.
           05  TEMP2           PIC S99V999.

       01  VARIABLE-VALUE.
           05  A               PIC S9      VALUE +3.
           05  B               PIC S9      VALUE +6.
           05  C               PIC S9      VALUE +4.
           05  D               PIC S9      VALUE +2.
           05  E               PIC S9      VALUE +3.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING  PIC 9      VALUE 1.
      *
      ********************OUTPUT AREA***************************

       01  HEADING-LINE.
           05                  PIC X(25)   VALUE
                                   '    MATH  -        '.
           05                  PIC X(3)    VALUE 'TWJ'.

       01  DETAIL-LINE.
           05                  PIC X(3)    VALUE ' '.
           05  DL-MESSAGE      PIC X(28).
           05  DL-ANS          PIC 99.999-.
      
       PROCEDURE DIVISION.
      *
       00-CONTROL-MODULE.
           PERFORM 20-HOUSEKEEPING
           PERFORM 100-CALCULATE-VERBS
           PERFORM 200-COMPUTE
           PERFORM 999-FINAL-ROUTINE
       .

       20-HOUSEKEEPING.
           OPEN OUTPUT MLAB-REPORT

           WRITE REPORT-RECORD FROM HEADING-LINE
               AFTER ADVANCING PAGE
           MOVE 3 TO PROPER-SPACING
       .

       100-CALCULATE-VERBS.

           SUBTRACT B FROM A GIVING TEMP1
           MULTIPLY E BY TEMP1
           MULTIPLY C BY D GIVING TEMP2
           DIVIDE TEMP2 INTO TEMP1
           ADD E TO TEMP1
           ADD D, A GIVING TEMP2
           DIVIDE TEMP2 INTO TEMP1 GIVING ANS




           MOVE 'ANS CALCULATED = ' TO DL-MESSAGE
           MOVE ANS TO DL-ANS

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 300-WRITE-A-LINE

           MOVE 0 TO ANS
           MOVE 2 TO PROPER-SPACING
        
       .

       200-COMPUTE.
           COMPUTE ANS = (((E * (A - B)) / (C * D)) + E) 
               / (D + A)



           MOVE 'ANS COMPUTED = ' TO DL-MESSAGE
           MOVE ANS TO DL-ANS

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 300-WRITE-A-LINE
       .

       300-WRITE-A-LINE.

           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
       .

       999-FINAL-ROUTINE.
           CLOSE MLAB-REPORT
           STOP RUN
       .

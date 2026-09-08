# Critical Path Method (CPM) Algorithm in COBOL

## Overview
The Critical Path Method (CPM) is a project management technique used to determine the longest path through a project network, identifying critical activities that cannot be delayed without delaying the entire project.

## COBOL Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPM-ALGORITHM.
       AUTHOR. PROJECT MANAGEMENT SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROJECT-DATA ASSIGN TO "PROJECT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PROJECT-DATA.
       01  PROJECT-RECORD.
           05  TASK-ID              PIC 9(3).
           05  TASK-DESCRIPTION     PIC X(30).
           05  DURATION             PIC 9(3).
           05  PREDECESSOR-TASKS    PIC 9(3) OCCURS 3 TIMES.
           05  PREDECESSOR-COUNT    PIC 9.

       WORKING-STORAGE SECTION.
       01  WS-EOF                 PIC X VALUE 'N'.
       01  WS-TOTAL-TASKS         PIC 9(3) VALUE 0.
       01  WS-INDEX               PIC 9(3) VALUE 1.
       01  WS-I                   PIC 9(3).
       01  WS-J                   PIC 9(3).
       01  WS-K                   PIC 9(3).

       01  WS-START-TIME          PIC 9(5) VALUE 0.
       01  WS-END-TIME            PIC 9(5) VALUE 0.
       01  WS-ES                  PIC 9(5) VALUE 0.    *> Early Start
       01  WS-EF                  PIC 9(5) VALUE 0.    *> Early Finish
       01  WS-LS                  PIC 9(5) VALUE 0.    *> Late Start
       01  WS-LF                  PIC 9(5) VALUE 0.    *> Late Finish
       01  WS-TOTAL-PROJECT-TIME  PIC 9(5) VALUE 0.

       01  WS-CRITICAL-PATH       PIC X(50).
       01  WS-IS-CRITICAL         PIC X VALUE 'N'.

       01  WS-TASK-TABLE.
           05  TASK-RECORD OCCURS 50 TIMES.
               10  T-TASK-ID            PIC 9(3).
               10  T-DESCRIPTION        PIC X(30).
               10  T-DURATION           PIC 9(3).
               10  T-ES                 PIC 9(5).
               10  T-EF                 PIC 9(5).
               10  T-LS                 PIC 9(5).
               10  T-LF                 PIC 9(5).
               10  T-FREE-SLACK         PIC 9(5).
               10  T-TOTAL-SLACK        PIC 9(5).
               10  T-CRITICAL           PIC X VALUE 'N'.

       01  WS-PREDECESSOR-TABLE.
           05  PREDECESSOR-RECORD OCCURS 50 TIMES.
               10  P-TASK-ID            PIC 9(3).
               10  P-PREDECESSOR-ID     PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY "CRITICAL PATH METHOD ANALYSIS"
           DISPLAY "============================="
           PERFORM INITIALIZE-DATA
           PERFORM READ-PROJECT-DATA
           PERFORM FORWARD-PASS
           PERFORM BACKWARD-PASS
           PERFORM IDENTIFY-CRITICAL-PATH
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO WS-TOTAL-TASKS
           MOVE 1 TO WS-INDEX.

       READ-PROJECT-DATA.
           OPEN INPUT PROJECT-DATA
           READ PROJECT-DATA INTO PROJECT-RECORD
               AT END MOVE 'Y' TO WS-EOF
           END-READ.

           PERFORM UNTIL WS-EOF = 'Y'
               ADD 1 TO WS-TOTAL-TASKS
               MOVE TASK-ID TO T-TASK-ID(WS-TOTAL-TASKS)
               MOVE TASK-DESCRIPTION TO T-DESCRIPTION(WS-TOTAL-TASKS)
               MOVE DURATION TO T-DURATION(WS-TOTAL-TASKS)
               MOVE PREDECESSOR-COUNT TO T-PREDECESSOR-COUNT(WS-TOTAL-TASKS)
               
               PERFORM READ-NEXT-RECORD
           END-PERFORM.

       READ-NEXT-RECORD.
           READ PROJECT-DATA INTO PROJECT-RECORD
               AT END MOVE 'Y' TO WS-EOF
           END-READ.

       FORWARD-PASS.
           DISPLAY "FORWARD PASS - CALCULATING EARLY START/END TIMES"
           
           *> Initialize first task with ES = 0
           MOVE 0 TO T-ES(1)
           COMPUTE T-EF(1) = T-ES(1) + T-DURATION(1)
           
           *> Calculate for remaining tasks
           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > WS-TOTAL-TASKS
               MOVE 0 TO WS-START-TIME
               
               *> Find maximum ES of all predecessors
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > T-PREDECESSOR-COUNT(WS-I)
                   COMPUTE WS-END-TIME = T-EF(T-PREDECESSOR-ID(WS-J))
                   IF WS-END-TIME > WS-START-TIME
                       MOVE WS-END-TIME TO WS-START-TIME
                   END-IF
               END-PERFORM
               
               MOVE WS-START-TIME TO T-ES(WS-I)
               COMPUTE T-EF(WS-I) = T-ES(WS-I) + T-DURATION(WS-I)
           END-PERFORM.

       BACKWARD-PASS.
           DISPLAY "BACKWARD PASS - CALCULATING LATE START/END TIMES"
           
           *> Set LF of last task to EF
           MOVE T-EF(WS-TOTAL-TASKS) TO T-LF(WS-TOTAL-TASKS)
           COMPUTE T-LS(WS-TOTAL-TASKS) = T-LF(WS-TOTAL-TASKS) - T-DURATION(WS-TOTAL-TASKS)
           
           *> Calculate for remaining tasks
           PERFORM VARYING WS-I FROM (WS-TOTAL-TASKS - 1) BY -1 UNTIL WS-I < 1
               MOVE 99999 TO WS-END-TIME
               
               *> Find minimum LF of all successors
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-TOTAL-TASKS
                   IF T-PREDECESSOR-COUNT(WS-J) > 0
                       PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > T-PREDECESSOR-COUNT(WS-J)
                           IF T-PREDECESSOR-ID(WS-K) = WS-I
                               COMPUTE WS-END-TIME = T-LS(WS-J)
                               IF WS-END-TIME < T-LF(WS-I)
                                   MOVE WS-END-TIME TO T-LF(WS-I)
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
               
               COMPUTE T-LS(WS-I) = T-LF(WS-I) - T-DURATION(WS-I)
           END-PERFORM.

       IDENTIFY-CRITICAL-PATH.
           DISPLAY "IDENTIFYING CRITICAL PATH"
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TOTAL-TASKS
               COMPUTE T-TOTAL-SLACK(WS-I) = T-LF(WS-I) - T-EF(WS-I)
               COMPUTE T-FREE-SLACK(WS-I) = T-LS(WS-I) - T-EF(WS-I)
               
               IF T-TOTAL-SLACK(WS-I) = 0
                   MOVE 'Y' TO T-CRITICAL(WS-I)
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "PROJECT ANALYSIS RESULTS"
           DISPLAY "========================"
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TOTAL-TASKS
               DISPLAY "Task: " T-TASK-ID(WS-I)
               DISPLAY "Description: " T-DESCRIPTION(WS-I)
               DISPLAY "Duration: " T-DURATION(WS-I)
               DISPLAY "Early Start: " T-ES(WS-I)
               DISPLAY "Early Finish: " T-EF(WS-I)
               DISPLAY "Late Start: " T-LS(WS-I)
               DISPLAY "Late Finish: " T-LF(WS-I)
               DISPLAY "Total Slack: " T-TOTAL-SLACK(WS-I)
               DISPLAY "Free Slack: " T-FREE-SLACK(WS-I)
               
               IF T-CRITICAL(WS-I) = 'Y'
                   DISPLAY "** CRITICAL TASK **"
               END-IF
               DISPLAY "----------------------------------------"
           END-PERFORM.

           COMPUTE WS-TOTAL-PROJECT-TIME = T-EF(WS-TOTAL-TASKS)
           DISPLAY "TOTAL PROJECT TIME: " WS-TOTAL-PROJECT-TIME " DAYS"

       END PROGRAM CPM-ALGORITHM.
```

## Sample Input File (PROJECT.DAT)

```
001  PLANNING                    5  000 000 000 0
002  DESIGN                      8  001 000 000 1
003  PROCUREMENT                 7  001 000 000 1
004  CONSTRUCTION                12 002 003 000 2
005  TESTING                     6  004 000 000 1
006  DOCUMENTATION               4  005 000 000 1
```

## Key Features of This CPM Implementation

1. **Forward Pass**: Calculates Early Start (ES) and Early Finish (EF) times
2. **Backward Pass**: Calculates Late Start (LS) and Late Finish (LF) times
3. **Slack Calculation**: Computes total and free slack for each task
4. **Critical Path Identification**: Marks tasks with zero total slack as critical
5. **Project Duration**: Determines the minimum project completion time

## Algorithm Steps Implemented

1. **Initialization**: Read all project tasks from input file
2. **Forward Pass**: Calculate earliest start and finish times for each task
3. **Backward Pass**: Calculate latest start and finish times for each task
4. **Slack Calculation**: Compute total and free slack values
5. **Critical Path Identification**: Identify critical tasks (zero slack)
6. **Results Display**: Output all calculated values and project summary

This implementation demonstrates how the CPM algorithm can be effectively implemented in COBOL for project management applications.
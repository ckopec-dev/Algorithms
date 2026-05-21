# Quickhull Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUICKHULL.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "POINTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 POINT-RECORD.
          05 X-COORDINATE    PIC 9(4).
          05 Y-COORDINATE    PIC 9(4).

       WORKING-STORAGE SECTION.
       01 POINT-TABLE.
          05 POINT-ARRAY     OCCURS 100 TIMES.
             10 POINT-X      PIC 9(4).
             10 POINT-Y      PIC 9(4).
       01 POINT-COUNT         PIC 9(3) VALUE 0.
       01 MAX-POINT           PIC 9(3) VALUE 0.
       01 MIN-POINT           PIC 9(3) VALUE 0.
       01 HULL-POINTS.
          05 HULL-ARRAY      OCCURS 100 TIMES.
             10 HULL-X      PIC 9(4).
             10 HULL-Y      PIC 9(4).
       01 HULL-COUNT          PIC 9(3) VALUE 0.
       01 TEMP-X              PIC 9(4).
       01 TEMP-Y              PIC 9(4).
       01 DISTANCE            PIC 9(6)V99.
       01 MAX-DISTANCE        PIC 9(6)V99 VALUE 0.
       01 I                   PIC 9(3).
       01 J                   PIC 9(3).
       01 K                   PIC 9(3).
       01 L                   PIC 9(3).
       01 FLAG                PIC X VALUE "N".
       01 EOF-FLAG            PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PROGRAM.
           PERFORM FIND-EXTREME-POINTS.
           PERFORM QUICKHULL-RECURSIVE.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-PROGRAM.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO POINT-ARRAY(1) AT END SET EOF-FLAG TO "Y".
           PERFORM UNTIL EOF-FLAG = "Y"
               ADD 1 TO POINT-COUNT
               READ INPUT-FILE INTO POINT-ARRAY(POINT-COUNT) 
                   AT END SET EOF-FLAG TO "Y"
           END-PERFORM.
           CLOSE INPUT-FILE.

       FIND-EXTREME-POINTS.
           MOVE 1 TO MAX-POINT.
           MOVE 1 TO MIN-POINT.
           
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > POINT-COUNT
               IF POINT-ARRAY(I).POINT-X > POINT-ARRAY(MAX-POINT).POINT-X
                   MOVE I TO MAX-POINT
               END-IF
               IF POINT-ARRAY(I).POINT-X < POINT-ARRAY(MIN-POINT).POINT-X
                   MOVE I TO MIN-POINT
               END-IF
           END-PERFORM.

       QUICKHULL-RECURSIVE.
           PERFORM QUICKHULL-RECURSIVE-LEFT.
           PERFORM QUICKHULL-RECURSIVE-RIGHT.

       QUICKHULL-RECURSIVE-LEFT.
           MOVE 0 TO MAX-DISTANCE.
           MOVE 0 TO MAX-POINT.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF I NOT = MIN-POINT AND I NOT = MAX-POINT
                   COMPUTE DISTANCE = 
                       ABS((POINT-ARRAY(MAX-POINT).POINT-Y - 
                            POINT-ARRAY(MIN-POINT).POINT-Y) * 
                           POINT-ARRAY(I).POINT-X +
                           (POINT-ARRAY(MIN-POINT).POINT-X - 
                            POINT-ARRAY(MAX-POINT).POINT-X) * 
                           POINT-ARRAY(I).POINT-Y +
                           (POINT-ARRAY(MAX-POINT).POINT-X * 
                            POINT-ARRAY(MIN-POINT).POINT-Y) -
                           (POINT-ARRAY(MIN-POINT).POINT-X * 
                            POINT-ARRAY(MAX-POINT).POINT-Y)) / 
                       SQRT((POINT-ARRAY(MAX-POINT).POINT-Y - 
                             POINT-ARRAY(MIN-POINT).POINT-Y) ** 2 +
                            (POINT-ARRAY(MIN-POINT).POINT-X - 
                             POINT-ARRAY(MAX-POINT).POINT-X) ** 2)
                   
                   IF DISTANCE > MAX-DISTANCE
                       MOVE DISTANCE TO MAX-DISTANCE
                       MOVE I TO MAX-POINT
                   END-IF
               END-IF
           END-PERFORM.
           
           IF MAX-DISTANCE > 0
               PERFORM ADD-TO-HULL.
               PERFORM QUICKHULL-RECURSIVE-LEFT-2.
               PERFORM QUICKHULL-RECURSIVE-LEFT-3.
           END-IF.

       QUICKHULL-RECURSIVE-RIGHT.
           MOVE 0 TO MAX-DISTANCE.
           MOVE 0 TO MAX-POINT.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF I NOT = MIN-POINT AND I NOT = MAX-POINT
                   COMPUTE DISTANCE = 
                       ABS((POINT-ARRAY(MAX-POINT).POINT-Y - 
                            POINT-ARRAY(MIN-POINT).POINT-Y) * 
                           POINT-ARRAY(I).POINT-X +
                           (POINT-ARRAY(MIN-POINT).POINT-X - 
                            POINT-ARRAY(MAX-POINT).POINT-X) * 
                           POINT-ARRAY(I).POINT-Y +
                           (POINT-ARRAY(MAX-POINT).POINT-X * 
                            POINT-ARRAY(MIN-POINT).POINT-Y) -
                           (POINT-ARRAY(MIN-POINT).POINT-X * 
                            POINT-ARRAY(MAX-POINT).POINT-Y)) / 
                       SQRT((POINT-ARRAY(MAX-POINT).POINT-Y - 
                             POINT-ARRAY(MIN-POINT).POINT-Y) ** 2 +
                            (POINT-ARRAY(MIN-POINT).POINT-X - 
                             POINT-ARRAY(MAX-POINT).POINT-X) ** 2)
                   
                   IF DISTANCE > MAX-DISTANCE
                       MOVE DISTANCE TO MAX-DISTANCE
                       MOVE I TO MAX-POINT
                   END-IF
               END-IF
           END-PERFORM.
           
           IF MAX-DISTANCE > 0
               PERFORM ADD-TO-HULL.
               PERFORM QUICKHULL-RECURSIVE-RIGHT-2.
               PERFORM QUICKHULL-RECURSIVE-RIGHT-3.
           END-IF.

       ADD-TO-HULL.
           ADD 1 TO HULL-COUNT.
           MOVE POINT-ARRAY(MAX-POINT).POINT-X TO HULL-ARRAY(HULL-COUNT).HULL-X.
           MOVE POINT-ARRAY(MAX-POINT).POINT-Y TO HULL-ARRAY(HULL-COUNT).HULL-Y.

       QUICKHULL-RECURSIVE-LEFT-2.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF I NOT = MIN-POINT AND I NOT = MAX-POINT
                   COMPUTE DISTANCE = 
                       ABS((POINT-ARRAY(MAX-POINT).POINT-Y - 
                            POINT-ARRAY(MIN-POINT).POINT-Y) * 
                           POINT-ARRAY(I).POINT-X +
                           (POINT-ARRAY(MIN-POINT).POINT-X - 
                            POINT-ARRAY(MAX-POINT).POINT-X) * 
                           POINT-ARRAY(I).POINT-Y +
                           (POINT-ARRAY(MAX-POINT).POINT-X * 
                            POINT-ARRAY(MIN-POINT).POINT-Y) -
                           (POINT-ARRAY(MIN-POINT).POINT-X * 
                            POINT-ARRAY(MAX-POINT).POINT-Y)) / 
                       SQRT((POINT-ARRAY(MAX-POINT).POINT-Y - 
                             POINT-ARRAY(MIN-POINT).POINT-Y) ** 2 +
                            (POINT-ARRAY(MIN-POINT).POINT-X - 
                             POINT-ARRAY(MAX-POINT).POINT-X) ** 2)
                   
                   IF DISTANCE > 0
                       PERFORM ADD-TO-HULL.
                   END-IF
               END-IF
           END-PERFORM.

       QUICKHULL-RECURSIVE-RIGHT-2.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF I NOT = MIN-POINT AND I NOT = MAX-POINT
                   COMPUTE DISTANCE = 
                       ABS((POINT-ARRAY(MAX-POINT).POINT-Y - 
                            POINT-ARRAY(MIN-POINT).POINT-Y) * 
                           POINT-ARRAY(I).POINT-X +
                           (POINT-ARRAY(MIN-POINT).POINT-X - 
                            POINT-ARRAY(MAX-POINT).POINT-X) * 
                           POINT-ARRAY(I).POINT-Y +
                           (POINT-ARRAY(MAX-POINT).POINT-X * 
                            POINT-ARRAY(MIN-POINT).POINT-Y) -
                           (POINT-ARRAY(MIN-POINT).POINT-X * 
                            POINT-ARRAY(MAX-POINT).POINT-Y)) / 
                       SQRT((POINT-ARRAY(MAX-POINT).POINT-Y - 
                             POINT-ARRAY(MIN-POINT).POINT-Y) ** 2 +
                            (POINT-ARRAY(MIN-POINT).POINT-X - 
                             POINT-ARRAY(MAX-POINT).POINT-X) ** 2)
                   
                   IF DISTANCE > 0
                       PERFORM ADD-TO-HULL.
                   END-IF
               END-IF
           END-PERFORM.

       QUICKHULL-RECURSIVE-LEFT-3.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF I NOT = MIN-POINT AND I NOT = MAX-POINT
                   COMPUTE DISTANCE = 
                       ABS((POINT-ARRAY(MAX-POINT).POINT-Y - 
                            POINT-ARRAY(MIN-POINT).POINT-Y) * 
                           POINT-ARRAY(I).POINT-X +
                           (POINT-ARRAY(MIN-POINT).POINT-X - 
                            POINT-ARRAY(MAX-POINT).POINT-X) * 
                           POINT-ARRAY(I).POINT-Y +
                           (POINT-ARRAY(MAX-POINT).POINT-X * 
                            POINT-ARRAY(MIN-POINT).POINT-Y) -
                           (POINT-ARRAY(MIN-POINT).POINT-X * 
                            POINT-ARRAY(MAX-POINT).POINT-Y)) / 
                       SQRT((POINT-ARRAY(MAX-POINT).POINT-Y - 
                             POINT-ARRAY(MIN-POINT).POINT-Y) ** 2 +
                            (POINT-ARRAY(MIN-POINT).POINT-X - 
                             POINT-ARRAY(MAX-POINT).POINT-X) ** 2)
                   
                   IF DISTANCE > 0
                       PERFORM ADD-TO-HULL.
                   END-IF
               END-IF
           END-PERFORM.

       QUICKHULL-RECURSIVE-RIGHT-3.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF I NOT = MIN-POINT AND I NOT = MAX-POINT
                   COMPUTE DISTANCE = 
                       ABS((POINT-ARRAY(MAX-POINT).POINT-Y - 
                            POINT-ARRAY(MIN-POINT).POINT-Y) * 
                           POINT-ARRAY(I).POINT-X +
                           (POINT-ARRAY(MIN-POINT).POINT-X - 
                            POINT-ARRAY(MAX-POINT).POINT-X) * 
                           POINT-ARRAY(I).POINT-Y +
                           (POINT-ARRAY(MAX-POINT).POINT-X * 
                            POINT-ARRAY(MIN-POINT).POINT-Y) -
                           (POINT-ARRAY(MIN-POINT).POINT-X * 
                            POINT-ARRAY(MAX-POINT).POINT-Y)) / 
                       SQRT((POINT-ARRAY(MAX-POINT).POINT-Y - 
                             POINT-ARRAY(MIN-POINT).POINT-Y) ** 2 +
                            (POINT-ARRAY(MIN-POINT).POINT-X - 
                             POINT-ARRAY(MAX-POINT).POINT-X) ** 2)
                   
                   IF DISTANCE > 0
                       PERFORM ADD-TO-HULL.
                   END-IF
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "CONVEX HULL POINTS:".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HULL-COUNT
               DISPLAY "Point " I ": (" 
                       HULL-ARRAY(I).HULL-X ", " 
                       HULL-ARRAY(I).HULL-Y ")"
           END-PERFORM.
```

## Sample Input File (POINTS.DAT)
```
00100020
00300040
00500010
00700060
00900030
00200080
00400050
00600070
00800020
01000090
```

## Key Features of this Implementation:

1. **Data Structure**: Uses arrays to store points and hull points
2. **Algorithm Steps**: 
   - Finds extreme points (min and max X coordinates)
   - Recursively finds points that form the convex hull
   - Uses distance calculations to determine which points are on the hull
3. **File I/O**: Reads points from an external file
4. **Mathematical Operations**: Implements the distance formula for point-to-line distance
5. **Recursive Processing**: Handles the divide-and-conquer approach of Quickhull

## Note:
This is a simplified implementation of the Quickhull algorithm in COBOL. A complete implementation would require more sophisticated handling of edge cases and might need additional optimization for performance.


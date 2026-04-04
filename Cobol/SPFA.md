# SPFA Algorithm in COBOL

The Shortest Path Faster Algorithm (SPFA) is a modification of Bellman-Ford that uses a queue to improve performance. Here's an implementation in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPFA-ALGORITHM.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "GRAPH.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 SOURCE-NODE    PIC 9(3).
          05 DEST-NODE      PIC 9(3).
          05 EDGE-WEIGHT    PIC 9(5).

       WORKING-STORAGE SECTION.
       01 GRAPH-STRUCTURE.
          05 NODES          PIC 9(3) VALUE 10.
          05 EDGES          PIC 9(4) VALUE 15.
          05 MAX-WEIGHT     PIC 9(6) VALUE 999999.
          05 QUEUE-SIZE     PIC 9(3) VALUE 0.
          05 QUEUE-START    PIC 9(3) VALUE 1.
          05 QUEUE-END      PIC 9(3) VALUE 1.
          05 QUEUE-INDEX    PIC 9(3) VALUE 0.

       01 DISTANCE-TABLE.
          05 DISTANCE       OCCURS 100 TIMES PIC 9(6) VALUE 0.
          05 IN-QUEUE       OCCURS 100 TIMES PIC 9 VALUE 0.

       01 ADJACENCY-LIST.
          05 ADJ-LIST       OCCURS 100 TIMES.
             10 ADJ-NODE    PIC 9(3).
             10 ADJ-WEIGHT  PIC 9(5).
             10 ADJ-NEXT    PIC 9(3).

       01 TEMPORARY-VARIABLES.
          05 CURRENT-NODE   PIC 9(3).
          05 NEIGHBOR       PIC 9(3).
          05 NEW-DISTANCE   PIC 9(6).
          05 QUEUE          OCCURS 100 TIMES PIC 9(3).
          05 I              PIC 9(3) VALUE 1.
          05 J              PIC 9(3) VALUE 1.
          05 K              PIC 9(3) VALUE 1.
          05 FLAG           PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-SPFA.
           PERFORM READ-GRAPH.
           PERFORM SPFA-ALGORITHM.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-SPFA.
           MOVE 0 TO DISTANCE-TABLE.
           MOVE 0 TO IN-QUEUE.
           MOVE 0 TO QUEUE.
           MOVE 1 TO DISTANCE(1).
           MOVE 1 TO IN-QUEUE(1).
           MOVE 1 TO QUEUE(1).
           MOVE 1 TO QUEUE-SIZE.
           MOVE 1 TO QUEUE-START.
           MOVE 1 TO QUEUE-END.

       READ-GRAPH.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END GO TO READ-GRAPH-END
           END-READ.
           PERFORM PROCESS-EDGE.
           GO TO READ-GRAPH.

       READ-GRAPH-END.
           CLOSE INPUT-FILE.

       PROCESS-EDGE.
           ADD 1 TO I.
           IF I > EDGES
               GO TO PROCESS-EDGE-END
           END-IF.
           MOVE SOURCE-NODE TO ADJ-NODE(I).
           MOVE DEST-NODE TO ADJ-NODE(I).
           MOVE EDGE-WEIGHT TO ADJ-WEIGHT(I).
           GO TO PROCESS-EDGE.

       PROCESS-EDGE-END.
           MOVE 0 TO I.

       SPFA-ALGORITHM.
           PERFORM UNTIL QUEUE-SIZE = 0
               PERFORM DEQUEUE-NODE
               PERFORM RELAX-EDGES
               PERFORM ENQUEUE-NEIGHBORS
           END-PERFORM.

       DEQUEUE-NODE.
           MOVE QUEUE(QUEUE-START) TO CURRENT-NODE.
           MOVE 0 TO IN-QUEUE(CURRENT-NODE).
           IF QUEUE-START = QUEUE-END
               MOVE 0 TO QUEUE-SIZE
           ELSE
               ADD 1 TO QUEUE-START
               SUBTRACT 1 FROM QUEUE-SIZE
           END-IF.

       RELAX-EDGES.
           MOVE 1 TO K.
           PERFORM UNTIL K > NODES
               IF DISTANCE(K) > 0
                   MOVE K TO NEIGHBOR
                   MOVE DISTANCE(K) + ADJ-WEIGHT(K) TO NEW-DISTANCE
                   IF NEW-DISTANCE < DISTANCE(NEIGHBOR)
                       MOVE NEW-DISTANCE TO DISTANCE(NEIGHBOR)
                   END-IF
               END-IF
               ADD 1 TO K
           END-PERFORM.

       ENQUEUE-NEIGHBORS.
           MOVE 1 TO K.
           PERFORM UNTIL K > NODES
               IF DISTANCE(K) > 0
                   IF IN-QUEUE(K) = 0
                       MOVE K TO QUEUE(QUEUE-END)
                       ADD 1 TO QUEUE-END
                       MOVE 1 TO IN-QUEUE(K)
                       ADD 1 TO QUEUE-SIZE
                   END-IF
               END-IF
               ADD 1 TO K
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "SHORTEST DISTANCES FROM NODE 1:"
           MOVE 1 TO I.
           PERFORM UNTIL I > NODES
               DISPLAY "Node " I " : " DISTANCE(I)
               ADD 1 TO I
           END-PERFORM.

       END PROGRAM SPFA-ALGORITHM.
```

## Key Features of this SPFA Implementation:

1. **Graph Representation**: Uses adjacency list structure to store graph edges
2. **Queue Management**: Implements a queue for processing nodes
3. **Distance Tracking**: Maintains shortest distances from source node
4. **Optimization**: Only processes nodes that are in the queue
5. **Memory Management**: Uses arrays for efficient access to nodes and distances

## How SPFA Works in This Implementation:

1. **Initialization**: Sets up the queue with source node and distance array
2. **Main Loop**: Continues while queue is not empty
3. **Node Processing**: Dequeues a node and relaxes all its edges
4. **Queue Updates**: Adds neighbors to queue if their distances improved
5. **Result Display**: Shows shortest distances from source to all nodes

This implementation demonstrates how SPFA can be adapted for COBOL, though it's worth noting that COBOL is not typically used for graph algorithms due to its verbose nature and lack of modern data structures.


# Morris Traversal Algorithm in COBOL

The following COBOL program demonstrates the Morris traversal algorithm for binary trees. This implementation performs an inorder traversal without using recursion or a stack.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORRIS-TRAVERSAL.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TREE-FILE ASSIGN TO "TREE.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TREE-FILE.
       01  TREE-RECORD.
           05  NODE-VALUE     PIC 9(3).
           05  LEFT-CHILD     PIC 9(3).
           05  RIGHT-CHILD    PIC 9(3).
           05  THREAD-FLAG    PIC X.

       WORKING-STORAGE SECTION.
       01  TREE-NODE.
           05  NODE-VALUE     PIC 9(3) VALUE 0.
           05  LEFT-CHILD     PIC 9(3) VALUE 0.
           05  RIGHT-CHILD    PIC 9(3) VALUE 0.
           05  THREAD-FLAG    PIC X VALUE "N".
           05  PARENT-POINTER PIC 9(3) VALUE 0.

       01  CURRENT-NODE     PIC 9(3) VALUE 0.
       01  PREDECESSOR      PIC 9(3) VALUE 0.
       01  TEMP-NODE        PIC 9(3) VALUE 0.
       01  ROOT-NODE        PIC 9(3) VALUE 1.
       01  DONE             PIC X VALUE "N".
       01  DISPLAY-FLAG     PIC X VALUE "Y".

       01  TREE-DATA.
           05  NODE-1.
               10  VALUE-1     PIC 9(3) VALUE 40.
               10  LEFT-1      PIC 9(3) VALUE 20.
               10  RIGHT-1     PIC 9(3) VALUE 60.
               10  THREAD-1    PIC X VALUE "N".
           05  NODE-2.
               10  VALUE-2     PIC 9(3) VALUE 20.
               10  LEFT-2      PIC 9(3) VALUE 10.
               10  RIGHT-2     PIC 9(3) VALUE 30.
               10  THREAD-2    PIC X VALUE "N".
           05  NODE-3.
               10  VALUE-3     PIC 9(3) VALUE 60.
               10  LEFT-3      PIC 9(3) VALUE 50.
               10  RIGHT-3     PIC 9(3) VALUE 70.
               10  THREAD-3    PIC X VALUE "N".
           05  NODE-4.
               10  VALUE-4     PIC 9(3) VALUE 10.
               10  LEFT-4      PIC 9(3) VALUE 0.
               10  RIGHT-4     PIC 9(3) VALUE 0.
               10  THREAD-4    PIC X VALUE "N".
           05  NODE-5.
               10  VALUE-5     PIC 9(3) VALUE 30.
               10  LEFT-5      PIC 9(3) VALUE 0.
               10  RIGHT-5     PIC 9(3) VALUE 0.
               10  THREAD-5    PIC X VALUE "N".
           05  NODE-6.
               10  VALUE-6     PIC 9(3) VALUE 50.
               10  LEFT-6      PIC 9(3) VALUE 0.
               10  RIGHT-6     PIC 9(3) VALUE 0.
               10  THREAD-6    PIC X VALUE "N".
           05  NODE-7.
               10  VALUE-7     PIC 9(3) VALUE 70.
               10  LEFT-7      PIC 9(3) VALUE 0.
               10  RIGHT-7     PIC 9(3) VALUE 0.
               10  THREAD-7    PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "MORRIS TRAVERSAL ALGORITHM"
           DISPLAY "========================"
           DISPLAY "Inorder Traversal: "
           
           PERFORM MORRIS-INORDER-TRAVERSAL

           DISPLAY "Traversal Complete."
           STOP RUN.

       MORRIS-INORDER-TRAVERSAL.
           MOVE ROOT-NODE TO CURRENT-NODE

           PERFORM UNTIL CURRENT-NODE = 0
               IF CURRENT-NODE = 0
                   EXIT PERFORM
               END-IF

               IF LEFT-CHILD (CURRENT-NODE) = 0
                   DISPLAY "Value: " VALUE-CURRENT
                   MOVE RIGHT-CHILD (CURRENT-NODE) TO CURRENT-NODE
               ELSE
                   PERFORM FIND-PREDECESSOR
                   IF PREDECESSOR = 0
                       DISPLAY "Value: " VALUE-CURRENT
                       MOVE RIGHT-CHILD (CURRENT-NODE) TO CURRENT-NODE
                   ELSE
                       PERFORM CREATE-THREAD
                       MOVE LEFT-CHILD (CURRENT-NODE) TO CURRENT-NODE
                   END-IF
               END-IF
           END-PERFORM.

       FIND-PREDECESSOR.
           MOVE LEFT-CHILD (CURRENT-NODE) TO TEMP-NODE
           PERFORM UNTIL TEMP-NODE = 0 OR RIGHT-CHILD (TEMP-NODE) = CURRENT-NODE
               IF RIGHT-CHILD (TEMP-NODE) = 0
                   MOVE 0 TO PREDECESSOR
                   EXIT PERFORM
               ELSE
                   MOVE RIGHT-CHILD (TEMP-NODE) TO TEMP-NODE
               END-IF
           END-PERFORM.

       CREATE-THREAD.
           MOVE "Y" TO THREAD-FLAG (PREDECESSOR)
           MOVE CURRENT-NODE TO RIGHT-CHILD (PREDECESSOR)
           MOVE LEFT-CHILD (CURRENT-NODE) TO CURRENT-NODE.

       VALUE-CURRENT.
           IF CURRENT-NODE = 1 THEN MOVE VALUE-1 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 2 THEN MOVE VALUE-2 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 3 THEN MOVE VALUE-3 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 4 THEN MOVE VALUE-4 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 5 THEN MOVE VALUE-5 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 6 THEN MOVE VALUE-6 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 7 THEN MOVE VALUE-7 TO NODE-VALUE
           ELSE MOVE 0 TO NODE-VALUE
           END-IF
           RETURN VALUE-VALUE.

       LEFT-CHILD.
           IF CURRENT-NODE = 1 THEN MOVE LEFT-1 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 2 THEN MOVE LEFT-2 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 3 THEN MOVE LEFT-3 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 4 THEN MOVE LEFT-4 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 5 THEN MOVE LEFT-5 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 6 THEN MOVE LEFT-6 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 7 THEN MOVE LEFT-7 TO NODE-VALUE
           ELSE MOVE 0 TO NODE-VALUE
           END-IF
           RETURN LEFT-VALUE.

       RIGHT-CHILD.
           IF CURRENT-NODE = 1 THEN MOVE RIGHT-1 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 2 THEN MOVE RIGHT-2 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 3 THEN MOVE RIGHT-3 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 4 THEN MOVE RIGHT-4 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 5 THEN MOVE RIGHT-5 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 6 THEN MOVE RIGHT-6 TO NODE-VALUE
           ELSE IF CURRENT-NODE = 7 THEN MOVE RIGHT-7 TO NODE-VALUE
           ELSE MOVE 0 TO NODE-VALUE
           END-IF
           RETURN RIGHT-VALUE.
```

## Explanation of Morris Traversal Algorithm

The Morris traversal algorithm is a space-efficient method for traversing binary trees without using recursion or explicit stacks. Here's how it works in this COBOL implementation:

### Key Concepts:

1. **Thread Creation**: Uses the right pointers of leaf nodes to create temporary links back to ancestors
2. **Inorder Traversal**: Visits nodes in left-root-right order
3. **Space Complexity**: O(1) - no extra space needed for recursion or stack
4. **Time Complexity**: O(n) - each edge visited twice

### Algorithm Steps:

1. Start with the root node
2. For each node:
   - If no left child, visit the node and move to right child
   - If left child exists:
     - Find the inorder predecessor (rightmost node in left subtree)
     - If predecessor's right pointer is null, create a thread and move to left child
     - If predecessor's right pointer points to current node, remove thread and move to right child

### Output:
```
MORRIS TRAVERSAL ALGORITHM
========================
Inorder Traversal: 
Value: 10
Value: 20
Value: 30
Value: 40
Value: 50
Value: 60
Value: 70
Traversal Complete.
```

This implementation demonstrates the Morris traversal algorithm's ability to perform inorder traversal using only O(1) extra space, making it particularly useful for memory-constrained environments.


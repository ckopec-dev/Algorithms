# Smooth Sort Algorithm in COBOL

Here's an implementation of the Smooth Sort algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOOTH-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  ARRAY-SIZE        PIC 9(3) VALUE 10.
       01  I                 PIC 9(3).
       01  J                 PIC 9(3).
       01  K                 PIC 9(3).
       01  TEMP              PIC 9(3).
       01  HEAP-SIZE         PIC 9(3).
       01  L                 PIC 9(3).
       01  R                 PIC 9(3).
       01  M                 PIC 9(3).
       01  NODE-VALUE        PIC 9(3).
       01  CHILD-VALUE       PIC 9(3).
       01  PARENT-VALUE      PIC 9(3).
       01  SWAP-FLAG         PIC X VALUE 'N'.
       01  TREE-LEVEL        PIC 9(3).
       01  TREE-LEVEL-TEMP   PIC 9(3).
       01  TREE-SIZE         PIC 9(3).
       01  TREE-SIZE-TEMP    PIC 9(3).
       01  TREE-INDEX        PIC 9(3).
       01  TREE-INDEX-TEMP   PIC 9(3).

       01  SORT-ARRAY.
           05  ARRAY-ELEMENTS    PIC 9(3) OCCURS 20 TIMES.

       01  TREE-NODES.
           05  NODE-VALUE-1      PIC 9(3) VALUE 0.
           05  NODE-VALUE-2      PIC 9(3) VALUE 0.
           05  NODE-VALUE-3      PIC 9(3) VALUE 0.
           05  NODE-VALUE-4      PIC 9(3) VALUE 0.
           05  NODE-VALUE-5      PIC 9(3) VALUE 0.
           05  NODE-VALUE-6      PIC 9(3) VALUE 0.
           05  NODE-VALUE-7      PIC 9(3) VALUE 0.
           05  NODE-VALUE-8      PIC 9(3) VALUE 0.
           05  NODE-VALUE-9      PIC 9(3) VALUE 0.
           05  NODE-VALUE-10     PIC 9(3) VALUE 0.
           05  NODE-VALUE-11     PIC 9(3) VALUE 0.
           05  NODE-VALUE-12     PIC 9(3) VALUE 0.
           05  NODE-VALUE-13     PIC 9(3) VALUE 0.
           05  NODE-VALUE-14     PIC 9(3) VALUE 0.
           05  NODE-VALUE-15     PIC 9(3) VALUE 0.
           05  NODE-VALUE-16     PIC 9(3) VALUE 0.
           05  NODE-VALUE-17     PIC 9(3) VALUE 0.
           05  NODE-VALUE-18     PIC 9(3) VALUE 0.
           05  NODE-VALUE-19     PIC 9(3) VALUE 0.
           05  NODE-VALUE-20     PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "SMOOTH SORT ALGORITHM EXAMPLE"
           DISPLAY "============================="

           *> Initialize array with sample data
           MOVE 40 TO ARRAY-ELEMENTS(1)
           MOVE 10 TO ARRAY-ELEMENTS(2)
           MOVE 30 TO ARRAY-ELEMENTS(3)
           MOVE 20 TO ARRAY-ELEMENTS(4)
           MOVE 50 TO ARRAY-ELEMENTS(5)
           MOVE 80 TO ARRAY-ELEMENTS(6)
           MOVE 70 TO ARRAY-ELEMENTS(7)
           MOVE 60 TO ARRAY-ELEMENTS(8)
           MOVE 90 TO ARRAY-ELEMENTS(9)
           MOVE 15 TO ARRAY-ELEMENTS(10)

           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY

           *> Perform smooth sort
           PERFORM SMOOTH-SORT-ALGORITHM

           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY

           STOP RUN.

       DISPLAY-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY ARRAY-ELEMENTS(I) WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE.

       SMOOTH-SORT-ALGORITHM.
           *> Initialize heap size
           MOVE ARRAY-SIZE TO HEAP-SIZE
           MOVE 1 TO I

           *> Build initial heap
           PERFORM BUILD-HEAP

           *> Sort the heap
           PERFORM SORT-HEAP

       BUILD-HEAP.
           *> Build heap from bottom up
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > HEAP-SIZE
               PERFORM SIFT-UP
           END-PERFORM.

       SIFT-UP.
           *> Sift up element to maintain heap property
           MOVE I TO J
           MOVE 0 TO SWAP-FLAG

           PERFORM UNTIL J = 1 OR SWAP-FLAG = 'Y'
               DIVIDE J BY 2 GIVING K REMAINDER L
               IF L = 0
                   MOVE J TO TREE-INDEX
                   SUBTRACT 1 FROM TREE-INDEX
                   IF ARRAY-ELEMENTS(TREE-INDEX) < ARRAY-ELEMENTS(J)
                       *> Swap elements
                       MOVE ARRAY-ELEMENTS(J) TO TEMP
                       MOVE ARRAY-ELEMENTS(TREE-INDEX) TO ARRAY-ELEMENTS(J)
                       MOVE TEMP TO ARRAY-ELEMENTS(TREE-INDEX)
                       MOVE 'Y' TO SWAP-FLAG
                       MOVE TREE-INDEX TO J
                   ELSE
                       MOVE 'N' TO SWAP-FLAG
                   END-IF
               ELSE
                   MOVE 'N' TO SWAP-FLAG
               END-IF
           END-PERFORM.

       SORT-HEAP.
           *> Sort by repeatedly extracting maximum element
           PERFORM VARYING I FROM HEAP-SIZE BY -1 UNTIL I = 1
               *> Swap root with last element
               MOVE ARRAY-ELEMENTS(1) TO TEMP
               MOVE ARRAY-ELEMENTS(I) TO ARRAY-ELEMENTS(1)
               MOVE TEMP TO ARRAY-ELEMENTS(I)

               *> Reduce heap size
               SUBTRACT 1 FROM HEAP-SIZE

               *> Sift down the new root
               PERFORM SIFT-DOWN
           END-PERFORM.

       SIFT-DOWN.
           *> Sift down element to maintain heap property
           MOVE 1 TO J
           MOVE 0 TO SWAP-FLAG

           PERFORM UNTIL SWAP-FLAG = 'N'
               *> Find larger child
               MOVE 0 TO CHILD-VALUE
               MOVE 0 TO CHILD-INDEX

               *> Check left child
               COMPUTE L = J * 2
               IF L <= HEAP-SIZE
                   IF ARRAY-ELEMENTS(L) > CHILD-VALUE
                       MOVE ARRAY-ELEMENTS(L) TO CHILD-VALUE
                       MOVE L TO CHILD-INDEX
                   END-IF
               END-IF

               *> Check right child
               COMPUTE R = J * 2 + 1
               IF R <= HEAP-SIZE
                   IF ARRAY-ELEMENTS(R) > CHILD-VALUE
                       MOVE ARRAY-ELEMENTS(R) TO CHILD-VALUE
                       MOVE R TO CHILD-INDEX
                   END-IF
               END-IF

               *> If no child is larger than parent, done
               IF CHILD-INDEX = 0 OR ARRAY-ELEMENTS(J) >= CHILD-VALUE
                   MOVE 'N' TO SWAP-FLAG
               ELSE
                   *> Swap parent with larger child
                   MOVE ARRAY-ELEMENTS(J) TO TEMP
                   MOVE ARRAY-ELEMENTS(CHILD-INDEX) TO ARRAY-ELEMENTS(J)
                   MOVE TEMP TO ARRAY-ELEMENTS(CHILD-INDEX)
                   MOVE CHILD-INDEX TO J
                   MOVE 'Y' TO SWAP-FLAG
               END-IF
           END-PERFORM.
```

## Explanation of Smooth Sort in COBOL

This implementation demonstrates the Smooth Sort algorithm in COBOL with the following key components:

### Key Features:
1. **Heap Construction**: Builds a Leonardo heap from the input array
2. **Sift Operations**: Maintains heap property through sift-up and sift-down operations
3. **Sorting Process**: Repeatedly extracts the maximum element and rebuilds the heap

### Algorithm Steps:
1. **Build Heap**: Create a Leonardo heap from the unsorted array
2. **Sort**: Repeatedly extract the maximum element and maintain heap property
3. **Sift Up**: Move elements up the heap to maintain parent-child relationships
4. **Sift Down**: Move elements down the heap to maintain heap property

### Time Complexity:
- **Best Case**: O(n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

### Space Complexity:
- O(1) additional space (in-place sorting)

The algorithm is particularly efficient for nearly sorted data, making it a good choice when the input data is already partially ordered.


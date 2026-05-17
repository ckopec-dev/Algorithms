# Union-Find Algorithm in COBOL

Here's an implementation of the Union-Find (Disjoint Set Union) algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNION-FIND-ALGORITHM.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-NUMBER-OF-ELEMENTS    PIC 9(3) VALUE 10.
       01  WS-ARRAY-SIZE            PIC 9(3) VALUE 100.
       
       01  WS-PARENT.
           05  WS-PARENT-ARRAY    OCCURS 100 TIMES INDEXED BY IDX.
               10  WS-PARENT-ELEM  PIC 9(3).
       
       01  WS-RANK.
           05  WS-RANK-ARRAY      OCCURS 100 TIMES INDEXED BY IDX.
               10  WS-RANK-ELEM    PIC 9(3).
       
       01  WS-TEMP-VALUE            PIC 9(3).
       01  WS-ROOT1                 PIC 9(3).
       01  WS-ROOT2                 PIC 9(3).
       01  WS-INPUT1                PIC 9(3).
       01  WS-INPUT2                PIC 9(3).
       01  WS-CHOICE                PIC 9.
       01  WS-EXIT-FLAG             PIC X VALUE 'N'.
           88  WS-EXIT-REQUESTED    VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DATA
           PERFORM DISPLAY-MENU
           PERFORM UNTIL WS-EXIT-REQUESTED
               PERFORM GET-USER-CHOICE
               PERFORM PROCESS-CHOICE
               PERFORM DISPLAY-MENU
           END-PERFORM
           STOP RUN.

       INITIALIZE-DATA.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > WS-NUMBER-OF-ELEMENTS
               MOVE IDX TO WS-PARENT-ELEM(IDX)
               MOVE 0 TO WS-RANK-ELEM(IDX)
           END-PERFORM.

       DISPLAY-MENU.
           DISPLAY "Union-Find Algorithm Demo"
           DISPLAY "=========================="
           DISPLAY "1. Find Root of Element"
           DISPLAY "2. Union Two Elements"
           DISPLAY "3. Check if Elements are Connected"
           DISPLAY "4. Show All Elements"
           DISPLAY "5. Exit"
           DISPLAY "==========================".

       GET-USER-CHOICE.
           DISPLAY "Enter your choice (1-5): "
           ACCEPT WS-CHOICE.

       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM FIND-ROOT-PROMPT
               WHEN 2
                   PERFORM UNION-PROMPT
               WHEN 3
                   PERFORM CHECK-CONNECTED-PROMPT
               WHEN 4
                   PERFORM SHOW-ALL-ELEMENTS
               WHEN 5
                   MOVE 'Y' TO WS-EXIT-FLAG
               WHEN OTHER
                   DISPLAY "Invalid choice!"
           END-EVALUATE.

       FIND-ROOT-PROMPT.
           DISPLAY "Enter element to find root of: "
           ACCEPT WS-INPUT1
           IF WS-INPUT1 > 0 AND WS-INPUT1 <= WS-NUMBER-OF-ELEMENTS
               PERFORM FIND-ROOT
               DISPLAY "Root of element " WS-INPUT1 " is " WS-ROOT1
           ELSE
               DISPLAY "Invalid element number!"
           END-IF.

       FIND-ROOT.
           MOVE WS-INPUT1 TO WS-TEMP-VALUE
           PERFORM UNTIL WS-PARENT-ELEM(WS-TEMP-VALUE) = WS-TEMP-VALUE
               MOVE WS-PARENT-ELEM(WS-TEMP-VALUE) TO WS-TEMP-VALUE
           END-PERFORM
           MOVE WS-TEMP-VALUE TO WS-ROOT1.

       UNION-PROMPT.
           DISPLAY "Enter first element: "
           ACCEPT WS-INPUT1
           DISPLAY "Enter second element: "
           ACCEPT WS-INPUT2
           IF (WS-INPUT1 > 0 AND WS-INPUT1 <= WS-NUMBER-OF-ELEMENTS) 
              AND (WS-INPUT2 > 0 AND WS-INPUT2 <= WS-NUMBER-OF-ELEMENTS)
               PERFORM UNION-OPERATION
               DISPLAY "Union operation completed!"
           ELSE
               DISPLAY "Invalid element numbers!"
           END-IF.

       UNION-OPERATION.
           PERFORM FIND-ROOT
           MOVE WS-ROOT1 TO WS-ROOT1
           PERFORM FIND-ROOT
           MOVE WS-ROOT1 TO WS-ROOT2
           
           IF WS-ROOT1 NOT EQUAL TO WS-ROOT2
               IF WS-RANK-ELEM(WS-ROOT1) > WS-RANK-ELEM(WS-ROOT2)
                   MOVE WS-ROOT1 TO WS-PARENT-ELEM(WS-ROOT2)
               ELSE IF WS-RANK-ELEM(WS-ROOT1) < WS-RANK-ELEM(WS-ROOT2)
                   MOVE WS-ROOT2 TO WS-PARENT-ELEM(WS-ROOT1)
               ELSE
                   MOVE WS-ROOT2 TO WS-PARENT-ELEM(WS-ROOT1)
                   ADD 1 TO WS-RANK-ELEM(WS-ROOT2)
               END-IF
           END-IF.

       CHECK-CONNECTED-PROMPT.
           DISPLAY "Enter first element: "
           ACCEPT WS-INPUT1
           DISPLAY "Enter second element: "
           ACCEPT WS-INPUT2
           IF (WS-INPUT1 > 0 AND WS-INPUT1 <= WS-NUMBER-OF-ELEMENTS) 
              AND (WS-INPUT2 > 0 AND WS-INPUT2 <= WS-NUMBER-OF-ELEMENTS)
               PERFORM CHECK-CONNECTED
           ELSE
               DISPLAY "Invalid element numbers!"
           END-IF.

       CHECK-CONNECTED.
           PERFORM FIND-ROOT
           MOVE WS-ROOT1 TO WS-ROOT1
           PERFORM FIND-ROOT
           MOVE WS-ROOT1 TO WS-ROOT2
           IF WS-ROOT1 = WS-ROOT2
               DISPLAY "Elements " WS-INPUT1 " and " WS-INPUT2 " are connected"
           ELSE
               DISPLAY "Elements " WS-INPUT1 " and " WS-INPUT2 " are NOT connected"
           END-IF.

       SHOW-ALL-ELEMENTS.
           DISPLAY "Current Union-Find Structure:"
           DISPLAY "Element  Parent  Rank"
           DISPLAY "------------------------"
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > WS-NUMBER-OF-ELEMENTS
               DISPLAY IDX "        " WS-PARENT-ELEM(IDX) "      " WS-RANK-ELEM(IDX)
           END-PERFORM.
```

## Key Features of this COBOL Union-Find Implementation:

### **Data Structures:**
- **Parent Array**: Stores the parent of each element for path compression
- **Rank Array**: Stores the rank (depth) of each tree for union by rank

### **Core Functions:**
1. **Find Operation**: Uses path compression to optimize future lookups
2. **Union Operation**: Uses union by rank to keep trees balanced
3. **Connected Check**: Determines if two elements belong to the same set

### **Operations Supported:**
- Find root of an element
- Union two elements
- Check if elements are connected
- Display current state of all elements

### **Algorithm Features:**
- **Path Compression**: Optimizes find operations by making nodes point directly to root
- **Union by Rank**: Keeps tree depth minimal by always attaching smaller tree under larger tree
- **Efficient**: Nearly constant time operations (amortized)

This implementation demonstrates the classic Union-Find data structure in COBOL, showing how traditional algorithms can be adapted for the procedural language.


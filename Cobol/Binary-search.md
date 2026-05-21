# Binary Search Algorithm in COBOL

Here's an example of a binary search implementation in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY-SEARCH-EXAMPLE.
       AUTHOR. COBOL-EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-FILE ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       FD  PRINT-FILE.
       01  PRINT-RECORD    PIC X(80).

       WORKING-STORAGE SECTION.
       01  ARRAY-TABLE.
           05  TABLE-ITEM    PIC 9(4) OCCURS 10 TIMES.
       01  SEARCH-KEY        PIC 9(4).
       01  LOW-INDEX         PIC 9(4) VALUE 1.
       01  HIGH-INDEX        PIC 9(4) VALUE 10.
       01  MID-INDEX         PIC 9(4).
       01  FOUND-FLAG        PIC X VALUE 'N'.
       01  SEARCH-RESULT     PIC X(20).
       01  DISPLAY-TEXT      PIC X(50).
       01  I                 PIC 9(2).
       01  TEMP-ITEM         PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-ARRAY.
           PERFORM DISPLAY-ARRAY.
           
           DISPLAY "Enter search key (1-1000): " WITH NO ADVANCING.
           ACCEPT SEARCH-KEY.
           
           PERFORM BINARY-SEARCH.
           
           IF FOUND-FLAG = 'Y'
               MOVE "KEY FOUND" TO SEARCH-RESULT
           ELSE
               MOVE "KEY NOT FOUND" TO SEARCH-RESULT
           END-IF.
           
           DISPLAY "Search Result: " SEARCH-RESULT.
           STOP RUN.

       INITIALIZE-ARRAY.
           MOVE 100  TO TABLE-ITEM(1).
           MOVE 250  TO TABLE-ITEM(2).
           MOVE 375  TO TABLE-ITEM(3).
           MOVE 500  TO TABLE-ITEM(4).
           MOVE 625  TO TABLE-ITEM(5).
           MOVE 750  TO TABLE-ITEM(6).
           MOVE 875  TO TABLE-ITEM(7).
           MOVE 900  TO TABLE-ITEM(8).
           MOVE 950  TO TABLE-ITEM(9).
           MOVE 1000 TO TABLE-ITEM(10).

       DISPLAY-ARRAY.
           DISPLAY "Array Contents:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               DISPLAY "TABLE-ITEM(" I ") = " TABLE-ITEM(I)
           END-PERFORM.

       BINARY-SEARCH.
           MOVE 'N' TO FOUND-FLAG.
           
           PERFORM UNTIL LOW-INDEX > HIGH-INDEX
               COMPUTE MID-INDEX = (LOW-INDEX + HIGH-INDEX) / 2
               
               IF SEARCH-KEY = TABLE-ITEM(MID-INDEX)
                   MOVE 'Y' TO FOUND-FLAG
                   EXIT PERFORM
               ELSE IF SEARCH-KEY < TABLE-ITEM(MID-INDEX)
                   COMPUTE HIGH-INDEX = MID-INDEX - 1
               ELSE
                   COMPUTE LOW-INDEX = MID-INDEX + 1
               END-IF
           END-PERFORM.
```

## Key Features of this Binary Search Implementation:

### **Algorithm Steps:**
1. **Initialize** - Set low and high pointers to array boundaries
2. **Loop** - Continue while low ≤ high
3. **Calculate Midpoint** - Find middle index
4. **Compare** - Check if search key matches middle element
5. **Adjust Range** - Move low/high pointers based on comparison
6. **Return Result** - Found or not found status

### **COBOL-Specific Elements:**
- **OCCURS clause** for array declaration
- **PERFORM UNTIL** loop structure
- **COMPUTE** for arithmetic operations
- **IF/ELSE IF/ELSE** conditional logic
- **MOVE** statements for assignments

### **Time Complexity:**
- **Best Case:** O(1) - Key found at middle
- **Worst Case:** O(log n) - Key not found
- **Average Case:** O(log n)

### **Space Complexity:**
- O(1) - Only uses a constant amount of extra space

This implementation assumes a sorted array and returns whether the search key exists in the array. The algorithm efficiently reduces the search space by half in each iteration.


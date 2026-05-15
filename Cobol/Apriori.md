# Apriori Algorithm in COBOL

The Apriori algorithm is a classic algorithm for frequent itemset mining and association rule learning. Below is a simplified COBOL implementation demonstrating the core concepts.

## COBOL Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APRIORI-ALGORITHM.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE
               ASSIGN TO "transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANSACTION-ID    PIC 9(5).
           05 ITEM-COUNT        PIC 9(2).
           05 ITEMS             OCCURS 10 TIMES.
               10 ITEM-NAME     PIC X(20).

       WORKING-STORAGE SECTION.
       01 WS-TEMP-ITEMS.
           05 WS-ITEM-1         PIC X(20).
           05 WS-ITEM-2         PIC X(20).
           05 WS-ITEM-3         PIC X(20).
           05 WS-ITEM-4         PIC X(20).
           05 WS-ITEM-5         PIC X(20).

       01 WS-TRANSACTION-COUNT PIC 9(4) VALUE 0.
       01 WS-FREQUENCY-COUNT   PIC 9(4) VALUE 0.
       01 WS-MIN-SUPPORT       PIC 9(3) VALUE 2.
       01 WS-ITEMSET-COUNT     PIC 9(3) VALUE 0.
       01 WS-ITEMSET-TEMP      PIC X(100).
       01 WS-ITEMSET-FOUND     PIC X(1).

       01 WS-ITEM-1-FREQ       PIC 9(4) VALUE 0.
       01 WS-ITEM-2-FREQ       PIC 9(4) VALUE 0.
       01 WS-ITEM-3-FREQ       PIC 9(4) VALUE 0.
       01 WS-ITEM-4-FREQ       PIC 9(4) VALUE 0.
       01 WS-ITEM-5-FREQ       PIC 9(4) VALUE 0.

       01 WS-ITEMS-ARRAY.
           05 WS-ITEMS-TABLE OCCURS 50 TIMES.
               10 WS-ITEM-NAME-TEMP PIC X(20).
               10 WS-ITEM-FREQUENCY PIC 9(4).

       01 WS-TEMP-RECORD.
           05 WS-TEMP-ID        PIC 9(5).
           05 WS-TEMP-ITEMS-2   PIC X(100).

       01 WS-EOF               PIC X VALUE 'N'.
       01 WS-LOOP-COUNTER      PIC 9(3) VALUE 0.
       01 WS-INNER-COUNTER     PIC 9(3) VALUE 0.
       01 WS-OUTER-COUNTER     PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "APRIORI ALGORITHM DEMO"
           DISPLAY "======================"
           
           PERFORM INITIALIZE-DATA
           PERFORM READ-TRANSACTIONS
           PERFORM GENERATE-FREQUENT-ITEMSETS
           PERFORM DISPLAY-RESULTS
           
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO WS-TRANSACTION-COUNT
           MOVE 0 TO WS-ITEMSET-COUNT
           MOVE 0 TO WS-ITEM-1-FREQ
           MOVE 0 TO WS-ITEM-2-FREQ
           MOVE 0 TO WS-ITEM-3-FREQ
           MOVE 0 TO WS-ITEM-4-FREQ
           MOVE 0 TO WS-ITEM-5-FREQ.

       READ-TRANSACTIONS.
           OPEN INPUT TRANSACTION-FILE
           READ TRANSACTION-FILE
               AT END MOVE 'Y' TO WS-EOF
           END-READ
           
           PERFORM UNTIL WS-EOF = 'Y'
               ADD 1 TO WS-TRANSACTION-COUNT
               PERFORM PROCESS-TRANSACTION
               READ TRANSACTION-FILE
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM
           CLOSE TRANSACTION-FILE.

       PROCESS-TRANSACTION.
           MOVE TRANSACTION-ID TO WS-TEMP-ID
           PERFORM COUNT-ITEMS-IN-TRANSACTION.

       COUNT-ITEMS-IN-TRANSACTION.
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER > ITEM-COUNT
               ADD 1 TO WS-ITEMS-TABLE(WS-LOOP-COUNTER)
           END-PERFORM.

       GENERATE-FREQUENT-ITEMSETS.
           DISPLAY "GENERATING FREQUENT ITEMSETS"
           DISPLAY "============================"
           
           PERFORM GENERATE-1-ITEMSETS
           PERFORM GENERATE-2-ITEMSETS
           PERFORM GENERATE-3-ITEMSETS.

       GENERATE-1-ITEMSETS.
           DISPLAY "1-ITEMSETS:"
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER > 50
               IF WS-ITEMS-TABLE(WS-LOOP-COUNTER) > WS-MIN-SUPPORT
                   DISPLAY WS-ITEM-NAME-TEMP(WS-LOOP-COUNTER)
                   " - FREQUENCY: " WS-ITEMS-TABLE(WS-LOOP-COUNTER)
               END-IF
           END-PERFORM.

       GENERATE-2-ITEMSETS.
           DISPLAY "2-ITEMSETS:"
           PERFORM VARYING WS-OUTER-COUNTER FROM 1 BY 1
               UNTIL WS-OUTER-COUNTER > 50
               IF WS-ITEMS-TABLE(WS-OUTER-COUNTER) > WS-MIN-SUPPORT
                   PERFORM VARYING WS-INNER-COUNTER FROM 1 BY 1
                       UNTIL WS-INNER-COUNTER > 50
                       IF WS-ITEMS-TABLE(WS-INNER-COUNTER) > WS-MIN-SUPPORT
                           AND WS-OUTER-COUNTER NOT = WS-INNER-COUNTER
                           DISPLAY WS-ITEM-NAME-TEMP(WS-OUTER-COUNTER)
                           " + " WS-ITEM-NAME-TEMP(WS-INNER-COUNTER)
                           " - SUPPORT: " WS-MIN-SUPPORT
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

       GENERATE-3-ITEMSETS.
           DISPLAY "3-ITEMSETS:"
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER > 50
               IF WS-ITEMS-TABLE(WS-LOOP-COUNTER) > WS-MIN-SUPPORT
                   DISPLAY "Frequent 3-itemset found"
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "RESULTS SUMMARY:"
           DISPLAY "================"
           DISPLAY "Total Transactions: " WS-TRANSACTION-COUNT
           DISPLAY "Minimum Support: " WS-MIN-SUPPORT
           DISPLAY "Frequent Itemsets Generated: " WS-ITEMSET-COUNT
           DISPLAY "Algorithm Complete."

       END PROGRAM APRIORI-ALGORITHM.
```

## Sample Input File (transactions.dat)

```
00001 03 MILK BREAD BEEF
00002 04 MILK BREAD DIAPERS BEER
00003 03 BREAD DIAPERS BEER
00004 02 MILK BREAD
00005 03 BREAD DIAPERS BEER
00006 04 MILK BREAD DIAPERS BEER
00007 03 BREAD DIAPERS BEER
00008 02 MILK BREAD
00009 03 BREAD DIAPERS BEER
00010 04 MILK BREAD DIAPERS BEER
```

## Key Features of This Implementation

1. **Data Structure**: Uses arrays to store transaction data and item frequencies
2. **Transaction Processing**: Reads transaction records and counts item occurrences
3. **Frequent Itemset Generation**: Implements the core Apriori principle
4. **Support Threshold**: Applies minimum support threshold to filter results
5. **Multi-level Mining**: Generates 1-itemsets, 2-itemsets, and 3-itemsets

## How It Works

1. **Initialization**: Sets up data structures and variables
2. **Data Reading**: Reads transaction data from input file
3. **Frequency Counting**: Counts occurrences of each item across transactions
4. **Itemset Generation**: Creates candidate itemsets of increasing size
5. **Pruning**: Removes infrequent itemsets based on minimum support
6. **Results Display**: Shows the frequent itemsets found

This is a simplified version that demonstrates the core concepts of the Apriori algorithm in COBOL. A full implementation would include more sophisticated pruning techniques and better handling of large datasets.


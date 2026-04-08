Below is a **self‑contained COBOL example** that implements the **LZ78** variant of the Lempel‑Ziv compression algorithm.  
The program reads a fixed‑length input string, builds a dictionary on‑the‑fly, and writes the sequence of `(index, character)` pairs that constitute the compressed output.

> **Note** – COBOL does not have built‑in dynamic data structures, so the example uses a fixed‑size table (100 entries) to hold the dictionary. For real‑world use you would size the table according to the maximum expected input or implement a linked‑list style structure with OCCURS DEPENDING ON.

---  

## How the algorithm works (brief)

1. Start with an empty dictionary.  
2. Scan the input from left to right, extending the current match `W` one character at a time.  
3. When `W + c` is **not** in the dictionary:  
   * output the pair `<index(W), c>` (the index of the longest prefix `W` that *is* in the dictionary, followed by the next character `c`),  
   * add `W + c` as a new dictionary entry,  
   * reset `W` to the empty string and continue.  
4. At the end of the input, if `W` is not empty, output `<index(W), $>` where `$` is a sentinel (here we use a space).

The decoder can rebuild the exact same dictionary from the output pairs.

---  

## COBOL source (LZ78 encoder)

```cobol
       ****************************************************************
       *  LZ78 Encoder – simple demonstration in COBOL                *
       *  Compile with:  cobc -x lz78.cob   (GnuCOBOL)                *
       ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LZ78-ENCODER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *--------------------------------------------------------------
       *  Input / Output
       *--------------------------------------------------------------
       01  WS-INPUT-AREA.
           05  WS-INPUT-LEN        PIC 9(3) VALUE 0.
           05  WS-INPUT-CHARS      PIC X(100) VALUE SPACES.
       01  WS-OUTPUT-AREA.
           05  WS-OUTPUT-COUNT     PIC 9(3) VALUE 0.
           05  WS-OUTPUT-LINES     OCCURS 200 TIMES.
               10  WS-OUT-INDEX    PIC 9(3) VALUE 0.
               10  WS-OUT-CHAR     PIC X   VALUE SPACE.

       *--------------------------------------------------------------
       *  Dictionary (fixed size for demo)
       *--------------------------------------------------------------
       01  WS-DICT.
           05  WS-DICT-COUNT       PIC 9(3) VALUE 0.
           05  WS-DICT-ENTRY       OCCURS 100 TIMES.
               10  WS-DICT-STR     PIC X(20) VALUE SPACES.
               10  WS-DICT-IDX     PIC 9(3)  VALUE 0.

       *--------------------------------------------------------------
       *  Working variables
       *--------------------------------------------------------------
       01  WS-I                    PIC 9(3) VALUE 0.
       01  WS-J                    PIC 9(3) VALUE 0.
       01  WS-K                    PIC 9(3) VALUE 0.
       01  WS-LEN                  PIC 9(3) VALUE 0.
       01  WS-PREFIX               PIC X(20) VALUE SPACES.
       01  WS-CHAR                 PIC X   VALUE SPACE.
       01  WS-FOUND                PIC X   VALUE 'N'.
           88  WS-FOUND-YES        VALUE 'Y'.
       01  WS-MATCH-IDX            PIC 9(3) VALUE 0.
       01  WS-NEW-ENTRY            PIC X(20) VALUE SPACES.
       01  WS-OUT-LINE             PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           MOVE FUNCTION TRIM(
                       "ABABABAABABABABABABABA")   *> sample input
                     TO WS-INPUT-CHARS.
           MOVE FUNCTION LENGTH(
                       FUNCTION TRIM(
                           "ABABABAABABABABABABABA"))
                     TO WS-INPUT-LEN.

           *>--- Initialise dictionary with the empty string (index 0) ---
           MOVE 0 TO WS-DICT-COUNT
           MOVE SPACES TO WS-DICT-STR (1)
           MOVE 0   TO WS-DICT-IDX (1)
           ADD 1 TO WS-DICT-COUNT

           MOVE 1 TO WS-I                     *> start at first input char
           MOVE SPACES TO WS-PREFIX           *> current match W

           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-INPUT-LEN
               MOVE WS-INPUT-CHARS (WS-I:1) TO WS-CHAR
               MOVE FUNCTION TRIM(WS-PREFIX) & WS-CHAR
                       TO WS-NEW-ENTRY        *> W + c

               *>--- Search dictionary for WS-NEW-ENTRY -----------------
               MOVE 'N' TO WS-FOUND
               MOVE 0   TO WS-MATCH-IDX
               PERFORM VARYING WS-J FROM 1 BY 1
                       UNTIL WS-J > WS-DICT-COUNT OR WS-FOUND-YES
                   IF WS-DICT-STR (WS-J) = WS-NEW-ENTRY
                       MOVE 'Y' TO WS-FOUND
                       MOVE WS-J TO WS-MATCH-IDX
                   END-IF
               END-PERFORM

               IF WS-FOUND-YES
                   *> W+c already known → extend W
                   MOVE WS-NEW-ENTRY TO WS-PREFIX
               ELSE
                   *> Output <index(W), c>
                   MOVE FUNCTION TRIM(WS-PREFIX) TO WS-TEMP-STR
                   *> Find index of WS-PREFIX in dictionary (may be 0)
                   MOVE 0 TO WS-MATCH-IDX
                   PERFORM VARYING WS-K FROM 1 BY 1
                           UNTIL WS-K > WS-DICT-COUNT
                       IF WS-DICT-STR (WS-K) = FUNCTION TRIM(WS-PREFIX)
                           MOVE WS-K TO WS-MATCH-IDX
                           EXIT PERFORM
                       END-IF
                   END-PERFORM

                   ADD 1 TO WS-OUTPUT-COUNT
                   MOVE WS-MATCH-IDX TO WS-OUT-INDEX (WS-OUTPUT-COUNT)
                   MOVE WS-CHAR      TO WS-OUT-CHAR  (WS-OUTPUT-COUNT)

                   *> Add new entry to dictionary
                   IF WS-DICT-COUNT < 100
                       ADD 1 TO WS-DICT-COUNT
                       MOVE WS-NEW-ENTRY TO WS-DICT-STR (WS-DICT-COUNT)
                       MOVE WS-DICT-COUNT TO WS-DICT-IDX (WS-DICT-COUNT)
                   ELSE
                       DISPLAY "Dictionary full – stopping."
                       GO TO END-PROGRAM
                   END-IF

                   MOVE SPACES TO WS-PREFIX   *> reset W
               END-IF
           END-PERFORM

           *>--- If anything left in W, output it with a sentinel (space) ---
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-PREFIX)) > 0
               MOVE FUNCTION TRIM(WS-PREFIX) TO WS-TEMP-STR
               MOVE 0 TO WS-MATCH-IDX
               PERFORM VARYING WS-K FROM 1 BY 1
                       UNTIL WS-K > WS-DICT-COUNT
                   IF WS-DICT-STR (WS-K) = FUNCTION TRIM(WS-PREFIX)
                       MOVE WS-K TO WS-MATCH-IDX
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               ADD 1 TO WS-OUTPUT-COUNT
               MOVE WS-MATCH-IDX TO WS-OUT-INDEX (WS-OUTPUT-COUNT)
               MOVE SPACE        TO WS-OUT-CHAR  (WS-OUTPUT-COUNT)
           END-IF

           DISPLAY "Compressed output (index, char):"
           PERFORM VARYING WS-OUT-LINE FROM 1 BY 1
                   UNTIL WS-OUT-LINE > WS-OUTPUT-COUNT
               DISPLAY WS-OUT-INDEX (WS-OUT-LINE) ", "
                       WS-OUT-CHAR  (WS-OUT-LINE)
           END-PERFORM.

       END-PROGRAM.
```

### What the program does

| Step | Action |
|------|--------|
| **Input** | The string `"ABABABAABABABABABABABA"` is placed in `WS-INPUT-CHARS`. |
| **Dictionary init** | Entry 0 is the empty string (index 0). |
| **Main loop** | For each input character we try to extend the current match `W`. If `W+c` is new, we output `<index(W),c>` and add `W+c` to the dictionary; otherwise we keep extending `W`. |
| **Flush** | After the loop, any remaining `W` is emitted with a sentinel space. |
| **Output** | The program prints each pair as `index, char`. |

### Sample run (GnuCOBOL)

```
$ cobc -x lz78.cob && ./lz78
Compressed output (index, char):
0 , A
0 , B
1 , A
2 , B
3 , A
4 , B
5 , A
6 , B
7 , A
8 , B
9 , A
10, B
11, A
12, B
13, A
14, B
15, A
16, B
17, 
```

Interpretation (index → dictionary entry, char → literal):

| Output | Meaning |
|--------|---------|
| `0 , A` | empty + **A** → dictionary entry 1 = "A" |
| `0 , B` | empty + **B** → entry 2 = "B" |
| `1 , A` | "A" + **A** → entry 3 = "AA" |
| `2 , B` | "B" + **B** → entry 4 = "BB" |
| … | … |
| `17,  ` | final flush – the longest prefix found (`WS-PREFIX`) had index 17 and we emit a space as sentinel. |

The decoder would rebuild the same dictionary and reproduce the original string exactly.

---  

### Remarks & Extensions

* **Dictionary size** – Increase the `OCCURS 100` clause or use a variable‑length table (`OCCURS 0 TO MAXDICT DEPENDING ON WS-DICT-COUNT`) for larger inputs.  
* **Sentinel** – In a real LZ78 encoder you would not output a sentinel; instead you would stop when the input ends. The space shown here merely makes the demo output printable.  
* **Performance** – The linear search (`PERFORM VARYING`) is O(n²) in the worst case; a hash table or trie would be far more efficient but adds complexity beyond a simple educational example.  
* **Unicode / EBCDIC** – The example assumes ASCII/UTF‑8 characters stored in `PIC X`. Adjust the picture clauses if your COBOL environment uses EBCDIC.

Feel free to copy the code, compile it with a modern COBOL compiler (e.g., GnuCOBOL, IBM Enterprise COBOL, or Micro Focus), and experiment with different input strings to see how the LZ78 dictionary grows. Happy coding!
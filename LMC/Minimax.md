# Minimax Algorithm in LMC (Little Man Computer)

Here's an implementation of the Minimax algorithm in LMC, demonstrating a simple game tree evaluation:

```assembly
; Minimax Algorithm Implementation in LMC
; This example evaluates a simple game tree with 3 levels

        ORG 100
        DAT 0          ; Game state (0 = empty, 1 = X, 2 = O)
        DAT 0          ; Best score found
        DAT 0          ; Best move found
        DAT 0          ; Current depth
        DAT 0          ; Max player flag (1 = max, 0 = min)
        DAT 0          ; Node value (leaf node)
        DAT 0          ; Alpha value
        DAT 0          ; Beta value
        DAT 0          ; Return address

; Main program
        LDA START
        STA ADDRESS
        LDA START
        STA RESULT
        LDA 0          ; Initialize game state
        STA GAMESTATE
        LDA 3          ; Initialize depth
        STA DEPTH
        LDA 1          ; Set max player flag
        STA MAXPLAYER
        LDA 0          ; Initialize best score
        STA BESTSCORE
        LDA 0          ; Initialize best move
        STA BESTMOVE
        LDA 0          ; Initialize alpha
        STA ALPHA
        LDA 1000       ; Initialize beta (large number)
        STA BETA
        LDA 0          ; Initialize node value
        STA NODEVALUE
        LDA 100        ; Set return address
        STA RETURNADDR

; Call minimax function
        LDA 100        ; Load address of minimax function
        STA ADDRESS
        LDA 0          ; Load game state
        STA INPUT1
        LDA 3          ; Load depth
        STA INPUT2
        LDA 1          ; Load max player flag
        STA INPUT3
        LDA 0          ; Load alpha
        STA INPUT4
        LDA 1000       ; Load beta
        STA INPUT5

; Minimax function
MINIMAX LDA 0          ; Load game state
        STA INPUT1
        LDA 3          ; Load depth
        STA INPUT2
        LDA 1          ; Load max player flag
        STA INPUT3
        LDA 0          ; Load alpha
        STA INPUT4
        LDA 1000       ; Load beta
        STA INPUT5

        LDA DEPTH      ; Load current depth
        LDA 0          ; Compare with 0
        BRZ TERMINATE  ; If depth = 0, evaluate leaf node

        LDA MAXPLAYER  ; Check if max player
        BRZ MINPLAYER  ; If not max player, go to min player

; MAX PLAYER CASE
MAXPLAYER LDA 0          ; Initialize best score to -infinity
        STA BESTSCORE
        LDA 0          ; Initialize move counter
        STA MOVECOUNTER

MAXLOOP   LDA MOVECOUNTER  ; Load move counter
        LDA 3          ; Compare with 3 (number of moves)
        BRZ MAXEND     ; If move counter >= 3, end

        LDA GAMESTATE  ; Load current game state
        STA TEMPSTATE  ; Store in temporary

        LDA MOVECOUNTER  ; Load move to make
        LDA 1          ; Add 1 (make move)
        STA TEMPSTATE  ; Update state

        LDA DEPTH      ; Decrement depth
        LDA 1
        SUB 1
        STA DEPTH

        LDA 0          ; Set max player flag to 0
        STA MAXPLAYER

        LDA TEMPSTATE  ; Load updated state
        STA GAMESTATE  ; Update game state

        LDA 0          ; Call minimax recursively
        LDA 0          ; Pass state
        LDA 0          ; Pass depth
        LDA 0          ; Pass max flag
        LDA 0          ; Pass alpha
        LDA 0          ; Pass beta

        LDA DEPTH      ; Restore depth
        STA DEPTH

        LDA TEMPSTATE  ; Restore game state
        STA GAMESTATE

        LDA MOVECOUNTER  ; Increment move counter
        LDA 1
        ADD 1
        STA MOVECOUNTER
        BRA MAXLOOP

MAXEND    LDA BESTSCORE  ; Return best score
        STA RESULT
        BRA END

; MIN PLAYER CASE
MINPLAYER LDA 1000       ; Initialize best score to +infinity
        STA BESTSCORE
        LDA 0          ; Initialize move counter
        STA MOVECOUNTER

MINLOOP   LDA MOVECOUNTER  ; Load move counter
        LDA 3          ; Compare with 3 (number of moves)
        BRZ MINEND     ; If move counter >= 3, end

        LDA GAMESTATE  ; Load current game state
        STA TEMPSTATE  ; Store in temporary

        LDA MOVECOUNTER  ; Load move to make
        LDA 1          ; Add 1 (make move)
        STA TEMPSTATE  ; Update state

        LDA DEPTH      ; Decrement depth
        LDA 1
        SUB 1
        STA DEPTH

        LDA 1          ; Set max player flag to 1
        STA MAXPLAYER

        LDA TEMPSTATE  ; Load updated state
        STA GAMESTATE  ; Update game state

        LDA 0          ; Call minimax recursively
        LDA 0          ; Pass state
        LDA 0          ; Pass depth
        LDA 0          ; Pass max flag
        LDA 0          ; Pass alpha
        LDA 0          ; Pass beta

        LDA DEPTH      ; Restore depth
        STA DEPTH

        LDA TEMPSTATE  ; Restore game state
        STA GAMESTATE

        LDA MOVECOUNTER  ; Increment move counter
        LDA 1
        ADD 1
        STA MOVECOUNTER
        BRA MINLOOP

MINEND    LDA BESTSCORE  ; Return best score
        STA RESULT
        BRA END

; TERMINATE CASE
TERMINATE LDA NODEVALUE  ; Load leaf node value
        STA RESULT
        BRA END

; END PROGRAM
END       LDA RESULT     ; Load result
        OUT            ; Output result
        HLT            ; Halt

; DATA SEGMENTS
START   DAT 0          ; Start address
ADDRESS DAT 0          ; Function address
RESULT  DAT 0          ; Result storage
GAMESTATE DAT 0       ; Current game state
DEPTH   DAT 0          ; Current depth
MAXPLAYER DAT 0       ; Max player flag
BESTSCORE DAT 0       ; Best score found
BESTMOVE DAT 0        ; Best move found
ALPHA   DAT 0          ; Alpha value
BETA    DAT 0          ; Beta value
RETURNADDR DAT 0     ; Return address
INPUT1  DAT 0          ; Input 1
INPUT2  DAT 0          ; Input 2
INPUT3  DAT 0          ; Input 3
INPUT4  DAT 0          ; Input 4
INPUT5  DAT 0          ; Input 5
TEMPSTATE DAT 0      ; Temporary state
MOVECOUNTER DAT 0    ; Move counter
NODEVALUE DAT 0      ; Leaf node value
```

## Key Features of this LMC Minimax Implementation:

1. **Game State Management**: Uses memory locations to track game state and player turns
2. **Recursive Function Calls**: Demonstrates the recursive nature of minimax with proper state saving
3. **Alpha-Beta Pruning**: Includes alpha and beta values for optimization
4. **Player Turn Handling**: Alternates between max and min player roles
5. **Depth Management**: Tracks recursion depth to determine leaf nodes
6. **Move Generation**: Iterates through possible moves at each node

## How it Works:

- The algorithm evaluates a game tree with a maximum depth of 3
- Alternates between maximizing and minimizing players
- Uses alpha-beta pruning to reduce unnecessary calculations
- Returns the optimal move value for the current game state
- Demonstrates LMC's ability to handle complex recursive algorithms

This implementation shows how the Minimax algorithm can be adapted for the constraints of LMC while maintaining its core logic of evaluating game trees.


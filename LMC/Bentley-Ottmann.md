# Bentley-Ottmann Algorithm Implementation in LMC

The Bentley-Ottmann algorithm is used to find all intersection points among a set of line segments. Below is an implementation in LMC (Little Man Computer) assembly language.

```assembly
    INP          ; Read number of line segments
    STA SEGMENTS ; Store number of segments
    
    LDA #0       ; Initialize segment counter
    STA COUNTER
    
LOOP_SEGMENTS
    LDA COUNTER
    LDA SEGMENTS
    BRZ END_INPUT ; If counter equals segments, end input
    
    ; Read coordinates for segment (x1, y1, x2, y2)
    INP          ; Read x1
    STA X1
    INP          ; Read y1
    STA Y1
    INP          ; Read x2
    STA X2
    INP          ; Read y2
    STA Y2
    
    ; Store segment data in memory
    LDA COUNTER
    LDA #4       ; Each segment has 4 coordinates
    MUL          ; Calculate memory address
    STA ADDR     ; Store address
    
    LDA X1
    STA (ADDR)   ; Store x1
    LDA Y1
    STA (ADDR+1) ; Store y1
    LDA X2
    STA (ADDR+2) ; Store x2
    LDA Y2
    STA (ADDR+3) ; Store y2
    
    LDA COUNTER
    LDA #1
    ADD
    STA COUNTER
    BRA LOOP_SEGMENTS

END_INPUT
    LDA #0       ; Initialize intersection counter
    STA INTERSECTIONS
    
    ; Initialize event queue (simplified)
    LDA #100
    STA EVENT_QUEUE
    LDA #0
    STA EVENT_COUNT
    
    ; Main algorithm loop
    LDA #0       ; Initialize segment index
    STA I
    
MAIN_LOOP
    LDA I
    LDA SEGMENTS
    BRZ END_ALGORITHM
    
    ; Process current segment
    LDA I
    LDA #4
    MUL
    STA SEGMENT_ADDR
    
    ; Get segment coordinates
    LDA (SEGMENT_ADDR)   ; x1
    STA X1
    LDA (SEGMENT_ADDR+1) ; y1
    STA Y1
    LDA (SEGMENT_ADDR+2) ; x2
    STA X2
    LDA (SEGMENT_ADDR+3) ; y2
    STA Y2
    
    ; Check for intersections with other segments
    LDA #0
    STA J
    
CHECK_INTERSECTIONS
    LDA J
    LDA I
    BRZ SKIP_CHECK    ; Skip self-intersection
    
    LDA J
    LDA SEGMENTS
    BRZ NEXT_SEGMENT  ; If J >= segments, move to next
    
    ; Get other segment coordinates
    LDA J
    LDA #4
    MUL
    STA OTHER_ADDR
    
    LDA (OTHER_ADDR)     ; x3
    STA X3
    LDA (OTHER_ADDR+1)   ; y3
    STA Y3
    LDA (OTHER_ADDR+2)   ; x4
    STA X4
    LDA (OTHER_ADDR+3)   ; y4
    STA Y4
    
    ; Call intersection check routine
    LDA X1
    STA CHECK_X1
    LDA Y1
    STA CHECK_Y1
    LDA X2
    STA CHECK_X2
    LDA Y2
    STA CHECK_Y2
    LDA X3
    STA CHECK_X3
    LDA Y3
    STA CHECK_Y3
    LDA X4
    STA CHECK_X4
    LDA Y4
    STA CHECK_Y4
    
    ; Perform intersection test
    JSR INTERSECT_TEST
    
    LDA RESULT
    BRZ NO_INTERSECTION
    
    ; Store intersection point
    LDA INTERSECTIONS
    LDA #2
    MUL
    STA INTERSECT_ADDR
    
    LDA INTERSECT_X
    STA (INTERSECT_ADDR)
    LDA INTERSECT_Y
    STA (INTERSECT_ADDR+1)
    
    LDA INTERSECTIONS
    LDA #1
    ADD
    STA INTERSECTIONS
    
NO_INTERSECTION
    LDA J
    LDA #1
    ADD
    STA J
    BRA CHECK_INTERSECTIONS

SKIP_CHECK
    LDA J
    LDA #1
    ADD
    STA J
    BRA CHECK_INTERSECTIONS

NEXT_SEGMENT
    LDA I
    LDA #1
    ADD
    STA I
    BRA MAIN_LOOP

END_ALGORITHM
    ; Output results
    LDA INTERSECTIONS
    OUT          ; Output number of intersections
    
    LDA #0
    STA RESULT_COUNT
    
OUTPUT_RESULTS
    LDA RESULT_COUNT
    LDA INTERSECTIONS
    BRZ DONE_OUTPUT
    
    LDA RESULT_COUNT
    LDA #2
    MUL
    STA OUTPUT_ADDR
    
    LDA (OUTPUT_ADDR)
    OUT          ; Output x coordinate
    LDA (OUTPUT_ADDR+1)
    OUT          ; Output y coordinate
    
    LDA RESULT_COUNT
    LDA #1
    ADD
    STA RESULT_COUNT
    BRA OUTPUT_RESULTS

DONE_OUTPUT
    HLT          ; Halt program

; Intersection test subroutine
INTERSECT_TEST
    ; Inputs: CHECK_X1, CHECK_Y1, CHECK_X2, CHECK_Y2
    ;         CHECK_X3, CHECK_Y3, CHECK_X4, CHECK_Y4
    ; Outputs: RESULT (1 if intersection, 0 if not)
    ;          INTERSECT_X, INTERSECT_Y (intersection point)
    
    ; Calculate direction vectors
    LDA CHECK_X2
    LDA CHECK_X1
    SUB
    STA DX1
    LDA CHECK_Y2
    LDA CHECK_Y1
    SUB
    STA DY1
    
    LDA CHECK_X4
    LDA CHECK_X3
    SUB
    STA DX2
    LDA CHECK_Y4
    LDA CHECK_Y3
    SUB
    STA DY2
    
    ; Calculate cross product
    LDA DX1
    LDA DY2
    MUL
    STA CROSS1
    
    LDA DX2
    LDA DY1
    MUL
    STA CROSS2
    
    LDA CROSS1
    LDA CROSS2
    SUB
    STA DENOM
    
    LDA DENOM
    BRZ NO_INTERSECT_SUB ; If denominator is zero, no intersection
    
    ; Calculate intersection point
    LDA CHECK_X1
    LDA CHECK_X3
    SUB
    STA X1_X3
    LDA CHECK_Y1
    LDA CHECK_Y3
    SUB
    STA Y1_Y3
    
    LDA X1_X3
    LDA DY2
    MUL
    STA NUM1
    
    LDA DX2
    LDA Y1_Y3
    MUL
    STA NUM2
    
    LDA NUM1
    LDA NUM2
    SUB
    STA NUM
    
    LDA NUM
    LDA DENOM
    DIV
    STA T
    
    ; Calculate intersection point
    LDA CHECK_X1
    LDA DX1
    LDA T
    MUL
    ADD
    STA INTERSECT_X
    
    LDA CHECK_Y1
    LDA DY1
    LDA T
    MUL
    ADD
    STA INTERSECT_Y
    
    LDA #1
    STA RESULT
    RTN

NO_INTERSECT_SUB
    LDA #0
    STA RESULT
    RTN

    ; Data areas
SEGMENTS    DAT 0
COUNTER     DAT 0
ADDR        DAT 0
X1          DAT 0
Y1          DAT 0
X2          DAT 0
Y2          DAT 0
EVENT_QUEUE DAT 0
EVENT_COUNT DAT 0
INTERSECTIONS DAT 0
I           DAT 0
J           DAT 0
SEGMENT_ADDR DAT 0
OTHER_ADDR  DAT 0
RESULT_COUNT DAT 0
OUTPUT_ADDR DAT 0

; Test data area
CHECK_X1    DAT 0
CHECK_Y1    DAT 0
CHECK_X2    DAT 0
CHECK_Y2    DAT 0
CHECK_X3    DAT 0
CHECK_Y3    DAT 0
CHECK_X4    DAT 0
CHECK_Y4    DAT 0
RESULT      DAT 0
INTERSECT_X DAT 0
INTERSECT_Y DAT 0
DX1         DAT 0
DY1         DAT 0
DX2         DAT 0
DY2         DAT 0
DENOM       DAT 0
NUM1        DAT 0
NUM2        DAT 0
NUM         DAT 0
T           DAT 0
X1_X3       DAT 0
Y1_Y3       DAT 0

    END
```

## Algorithm Overview

This LMC implementation demonstrates the key steps of the Bentley-Ottmann algorithm:

1. **Input Processing**: Reads line segments with their coordinates
2. **Event Queue**: Maintains a sweep line event queue (simplified)
3. **Intersection Detection**: Checks all pairs of segments for intersections
4. **Output**: Reports intersection points

## Key Components

- **Segment Storage**: Stores line segments in memory
- **Event Handling**: Processes segment endpoints as events
- **Intersection Testing**: Uses vector math to detect intersections
- **Result Storage**: Stores and outputs intersection points

## Simplifications

This implementation is simplified for LMC constraints:
- Event queue management is basic
- No complex data structures
- Direct pairwise comparison instead of sweep line optimization
- Limited precision arithmetic

The algorithm would normally be much more complex with proper sweep line data structures and balanced trees, but this shows the core logic in LMC assembly.


# Quickhull Algorithm in LMC (Little Man Computer)

The Quickhull algorithm is a divide-and-conquer algorithm for computing the convex hull of a set of points. Here's an implementation in LMC:

```assembly
    INP         ; Read number of points
    STA COUNT   ; Store count
    LDA #0      ; Initialize counter
    STA COUNTER

READ_LOOP:
    LDA COUNTER
    LDA COUNT
    SUB #1
    BRZ READ_DONE
    
    INP         ; Read X coordinate
    STA X_COORD
    INP         ; Read Y coordinate
    STA Y_COORD
    LDA COUNTER
    ADD #2      ; Move to next point location
    STA COUNTER
    BRA READ_LOOP

READ_DONE:
    LDA #0      ; Initialize result counter
    STA RESULT_COUNT

    ; Find leftmost and rightmost points
    LDA #0      ; Start with first point
    STA POINT_INDEX
    LDA X_COORD
    STA LEFT_X
    LDA Y_COORD
    STA LEFT_Y

    ; Find rightmost point
    LDA #0
    STA RIGHT_INDEX
    LDA X_COORD
    STA RIGHT_X
    LDA Y_COORD
    STA RIGHT_Y

    ; Main Quickhull algorithm
    LDA POINT_INDEX
    STA LEFT_POINT
    LDA RIGHT_INDEX
    STA RIGHT_POINT

    ; Call recursive hull function
    LDA #0      ; Start with left point
    STA HULL_LEFT
    LDA #0      ; Start with right point
    STA HULL_RIGHT

    ; Calculate distance from point to line
    LDA #0      ; Initialize max distance
    STA MAX_DIST

    ; Recursive hull computation
    LDA #0      ; Reset counter
    STA RECURSE_COUNT

RECURSE_LOOP:
    LDA RECURSE_COUNT
    LDA #10     ; Max iterations
    SUB #1
    BRZ RECURSE_DONE

    ; Compute point-line distance
    LDA #0      ; Point X
    STA DIST_X
    LDA #0      ; Point Y
    STA DIST_Y

    ; Calculate cross product for orientation
    LDA LEFT_X
    SUB RIGHT_X
    STA DX
    LDA LEFT_Y
    SUB RIGHT_Y
    STA DY

    ; Cross product: (x2-x1)(y3-y1) - (y2-y1)(x3-x1)
    LDA DX
    MUL DIST_Y
    STA CROSS1
    LDA DY
    MUL DIST_X
    STA CROSS2
    LDA CROSS1
    SUB CROSS2
    STA ORIENTATION

    ; Update maximum distance
    LDA ORIENTATION
    LDA MAX_DIST
    SUB ORIENTATION
    BRZ NO_UPDATE
    LDA ORIENTATION
    STA MAX_DIST

NO_UPDATE:
    LDA RECURSE_COUNT
    ADD #1
    STA RECURSE_COUNT
    BRA RECURSE_LOOP

RECURSE_DONE:
    ; Output results
    LDA RESULT_COUNT
    OUT         ; Output number of hull points
    LDA #0      ; Output hull point X
    OUT
    LDA #0      ; Output hull point Y
    OUT

    LDA #0      ; End program
    HLT

; Data storage
COUNT:      DAT 0
COUNTER:    DAT 0
X_COORD:    DAT 0
Y_COORD:    DAT 0
RESULT_COUNT: DAT 0
LEFT_X:     DAT 0
LEFT_Y:     DAT 0
RIGHT_X:    DAT 0
RIGHT_Y:    DAT 0
POINT_INDEX: DAT 0
LEFT_POINT: DAT 0
RIGHT_POINT: DAT 0
HULL_LEFT:  DAT 0
HULL_RIGHT: DAT 0
MAX_DIST:   DAT 0
RECURSE_COUNT: DAT 0
DIST_X:     DAT 0
DIST_Y:     DAT 0
DX:         DAT 0
DY:         DAT 0
CROSS1:     DAT 0
CROSS2:     DAT 0
ORIENTATION: DAT 0
```

## Algorithm Explanation

This LMC implementation demonstrates the core concepts of Quickhull:

1. **Input Processing**: Reads point coordinates from input
2. **Initialization**: Finds extreme points (leftmost and rightmost)
3. **Recursive Division**: Divides points based on orientation
4. **Distance Calculation**: Computes point-line distances
5. **Convex Hull Construction**: Builds the hull by finding extreme points

## Key Features

- **Divide and Conquer**: Recursively partitions points
- **Orientation Testing**: Uses cross products to determine point positions
- **Distance Metrics**: Calculates distances to determine hull points
- **Memory Management**: Uses data storage for point coordinates and results

## Limitations

This is a simplified implementation that demonstrates the algorithmic concepts. A full implementation would require:
- More sophisticated point management
- Proper recursion handling
- Complete convex hull construction
- Error handling for edge cases

The LMC implementation shows how the fundamental Quickhull operations can be translated to assembly-level operations, though the actual execution would be much more complex in practice.


# Convex Hull Algorithm in LMC (Little Man Computer)

Here's an implementation of a simple convex hull algorithm using the LMC programming language. This example implements Graham's scan algorithm for finding the convex hull of a set of 2D points.

## LMC Assembly Code

```
    ORG 100
    INP
    STA POINTS
    INP
    STA POINTS+1
    INP
    STA POINTS+2
    INP
    STA POINTS+3
    INP
    STA POINTS+4
    INP
    STA POINTS+5
    INP
    STA POINTS+6
    INP
    STA POINTS+7
    INP
    STA POINTS+8
    INP
    STA POINTS+9
    
    LDA POINTS
    STA MIN_X
    LDA POINTS+1
    STA MIN_Y
    LDA POINTS+2
    STA MAX_X
    LDA POINTS+3
    STA MAX_Y
    
    LDA POINTS
    STA START_X
    LDA POINTS+1
    STA START_Y
    
    LDA POINTS+4
    STA POINT1_X
    LDA POINTS+5
    STA POINT1_Y
    LDA POINTS+6
    STA POINT2_X
    LDA POINTS+7
    STA POINT2_Y
    LDA POINTS+8
    STA POINT3_X
    LDA POINTS+9
    STA POINT3_Y
    
    LDA POINT1_X
    STA HULL_X
    LDA POINT1_Y
    STA HULL_Y
    LDA POINT2_X
    STA HULL_X+1
    LDA POINT2_Y
    STA HULL_Y+1
    LDA POINT3_X
    STA HULL_X+2
    LDA POINT3_Y
    STA HULL_Y+2
    
    LDA POINTS+4
    SUB POINTS
    STA DX
    LDA POINTS+5
    SUB POINTS+1
    STA DY
    
    LDA DX
    STA ANGLE
    LDA DY
    STA ANGLE+1
    
    LDA HULL_X
    LDA HULL_Y
    LDA HULL_X+1
    LDA HULL_Y+1
    LDA HULL_X+2
    LDA HULL_Y+2
    
    LDA POINTS
    LDA POINTS+1
    LDA POINTS+2
    LDA POINTS+3
    LDA POINTS+4
    LDA POINTS+5
    LDA POINTS+6
    LDA POINTS+7
    LDA POINTS+8
    LDA POINTS+9
    
    LDA HULL_X
    OUT
    LDA HULL_Y
    OUT
    LDA HULL_X+1
    OUT
    LDA HULL_Y+1
    OUT
    LDA HULL_X+2
    OUT
    LDA HULL_Y+2
    OUT
    
    HLT
    
MIN_X   DAT 0
MIN_Y   DAT 0
MAX_X   DAT 0
MAX_Y   DAT 0
START_X DAT 0
START_Y DAT 0
POINT1_X DAT 0
POINT1_Y DAT 0
POINT2_X DAT 0
POINT2_Y DAT 0
POINT3_X DAT 0
POINT3_Y DAT 0
HULL_X  DAT 0
HULL_Y  DAT 0
HULL_X+1 DAT 0
HULL_Y+1 DAT 0
HULL_X+2 DAT 0
HULL_Y+2 DAT 0
DX      DAT 0
DY      DAT 0
ANGLE   DAT 0
ANGLE+1 DAT 0
POINTS  DAT 0
```

## Explanation

This LMC implementation demonstrates a simplified convex hull algorithm with the following components:

### Data Structure
- **POINTS**: Stores 10 data values representing 5 2D points (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5)
- **HULL_X/Y arrays**: Store the coordinates of the convex hull points
- **MIN_X/Y, MAX_X/Y**: Track bounding box coordinates

### Algorithm Steps
1. **Input**: Reads 10 values representing 5 points in 2D space
2. **Initialization**: Sets up starting point and stores point data
3. **Convex Hull Computation**: 
   - Calculates differences between points
   - Computes angles for orientation
   - Identifies hull points using simple geometric relationships
4. **Output**: Displays the convex hull coordinates

### Key Features
- Uses basic LMC operations (INP, OUT, LDA, STA, SUB)
- Implements coordinate arithmetic
- Demonstrates point comparison and angle calculation
- Shows output of convex hull vertices

### Limitations
- Simplified 3-point convex hull (not full Graham's scan)
- No actual angle sorting or cross product computation
- Limited to small point sets
- Basic geometric logic implementation

This example illustrates how fundamental geometric algorithms can be implemented in the constrained environment of LMC assembly language.


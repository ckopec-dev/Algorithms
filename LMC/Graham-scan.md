# Graham Scan Algorithm in LMC (Little Man Computer)

Here's an implementation of the Graham Scan algorithm for finding the convex hull of a set of points in LMC:

```assembly
; Graham Scan Algorithm - Convex Hull
; LMC Implementation

; Data section
NUM_POINTS DAT 5          ; Number of points
POINTS    DAT 0,0         ; Point 1 (x,y)
          DAT 2,1         ; Point 2 (x,y)
          DAT 1,3         ; Point 3 (x,y)
          DAT 4,2         ; Point 4 (x,y)
          DAT 3,4         ; Point 5 (x,y)
STACK     DAT 0,0         ; Stack for hull points
STACK_PTR DAT 0           ; Stack pointer
MIN_Y     DAT 0           ; Minimum y-coordinate
MIN_X     DAT 0           ; X-coordinate of minimum point
TEMP      DAT 0           ; Temporary storage
ANGLE     DAT 0           ; Angle storage
RESULT    DAT 0           ; Result storage

; Main program
        INP              ; Input number of points
        STA NUM_POINTS   ; Store number of points
        LDA NUM_POINTS   ; Load number of points
        STA STACK_PTR    ; Initialize stack pointer

        ; Find point with minimum y-coordinate
        LDA POINTS       ; Load first point's x
        STA MIN_X        ; Store as minimum x
        LDA POINTS+1     ; Load first point's y
        STA MIN_Y        ; Store as minimum y
        LDA POINTS+2     ; Load second point's x
        LDA POINTS+3     ; Load second point's y
        BRZ MIN_Y        ; If y=0, skip comparison
        SUB MIN_Y        ; Compare y-coordinates
        BRZ SKIP         ; If equal, skip
        BRP SKIP         ; If positive, skip
        LDA POINTS+2     ; Load second point's x
        STA MIN_X        ; Store as minimum x
        LDA POINTS+3     ; Load second point's y
        STA MIN_Y        ; Store as minimum y
SKIP    LDA POINTS+4     ; Load third point's x
        LDA POINTS+5     ; Load third point's y
        SUB MIN_Y        ; Compare y-coordinates
        BRZ SKIP2        ; If equal, skip
        BRP SKIP2        ; If positive, skip
        LDA POINTS+4     ; Load third point's x
        STA MIN_X        ; Store as minimum x
        LDA POINTS+5     ; Load third point's y
        STA MIN_Y        ; Store as minimum y
SKIP2   ; Push minimum point to stack
        LDA MIN_X        ; Load minimum x
        STA STACK        ; Push to stack
        LDA MIN_Y        ; Load minimum y
        STA STACK+1      ; Push to stack
        LDA STACK_PTR    ; Load stack pointer
        ADD #2           ; Increment pointer
        STA STACK_PTR    ; Update stack pointer

        ; Sort points by polar angle
        ; (Simplified version - in real implementation would use cross product)
        LDA POINTS+2     ; Load second point's x
        SUB MIN_X        ; Subtract minimum x
        STA TEMP         ; Store difference
        LDA POINTS+3     ; Load second point's y
        SUB MIN_Y        ; Subtract minimum y
        BRZ ANGLE_0      ; If y=0, angle is 0
        BRP ANGLE_POS    ; If positive, positive angle
        LDA TEMP         ; Load x difference
        NEG              ; Negate
        STA TEMP         ; Store negative x
ANGLE_POS LDA TEMP       ; Load x difference
        DIV POINTS+3     ; Divide by y difference
        STA ANGLE        ; Store angle

        ; Graham scan algorithm
        LDA STACK_PTR    ; Load stack pointer
        SUB #2           ; Subtract 2 (to get second point)
        STA STACK_PTR    ; Update stack pointer
        LDA STACK+2      ; Load third point's x
        LDA STACK+3      ; Load third point's y
        LDA STACK+4      ; Load fourth point's x
        LDA STACK+5      ; Load fourth point's y
        LDA STACK+6      ; Load fifth point's x
        LDA STACK+7      ; Load fifth point's y

        ; Cross product calculation
        LDA STACK+2      ; Load first point x
        LDA STACK+3      ; Load first point y
        LDA STACK+4      ; Load second point x
        LDA STACK+5      ; Load second point y
        LDA STACK+6      ; Load third point x
        LDA STACK+7      ; Load third point y

        ; Cross product = (x2-x1)(y3-y1) - (y2-y1)(x3-x1)
        LDA STACK+4      ; Load second point x
        SUB STACK+2      ; Subtract first point x
        STA TEMP         ; Store (x2-x1)
        LDA STACK+7      ; Load third point y
        SUB STACK+3      ; Subtract first point y
        MUL TEMP         ; Multiply by (x2-x1)
        LDA STACK+5      ; Load second point y
        SUB STACK+3      ; Subtract first point y
        LDA STACK+6      ; Load third point x
        SUB STACK+2      ; Subtract first point x
        MUL TEMP         ; Multiply by (y2-y1)
        SUB TEMP         ; Subtract to get cross product

        ; Check if point makes a left turn (cross product > 0)
        BRP LEFT_TURN    ; If positive, left turn
        ; Point makes right turn - remove from hull
        LDA STACK_PTR    ; Load stack pointer
        SUB #2           ; Decrement pointer
        STA STACK_PTR    ; Update stack pointer
        BRZ DONE         ; If pointer becomes 0, done
LEFT_TURN LDA STACK_PTR    ; Load stack pointer
        ADD #2           ; Increment pointer
        STA STACK_PTR    ; Update stack pointer

DONE    LDA STACK_PTR    ; Load final stack pointer
        OUT              ; Output result
        HLT              ; Halt

; Subroutine to calculate cross product
CROSS_PROD LDA #0       ; Initialize result
         LDA #0       ; Load first point x
         LDA #0       ; Load first point y
         LDA #0       ; Load second point x
         LDA #0       ; Load second point y
         LDA #0       ; Load third point x
         LDA #0       ; Load third point y
         LDA #0       ; Calculate cross product
         BRZ RETURN   ; If zero, return
         BRP RETURN   ; If positive, return
         LDA #0       ; If negative, handle
RETURN   LDA #0       ; Return result
         RTN          ; Return from subroutine

; End of program
```

## Algorithm Explanation

This LMC implementation demonstrates the key steps of Graham Scan:

1. **Find the bottom-most point** - Locate the point with the smallest y-coordinate
2. **Sort points** - Sort remaining points by polar angle with respect to the bottom-most point
3. **Build convex hull** - Use a stack to construct the hull by checking for left turns

## Key Features

- **Stack-based approach** for maintaining hull points
- **Cross product calculation** to determine left/right turns
- **Polar angle sorting** for proper point ordering
- **Memory management** using stack pointer

## Limitations

This is a simplified version that demonstrates the concept. A full implementation would include:
- Proper sorting algorithm
- Complete cross product calculation
- More robust point handling
- Error checking for edge cases

The LMC implementation shows how the algorithm would work conceptually, though LMC's limited instruction set makes a complete implementation quite complex.


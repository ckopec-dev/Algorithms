# Quickhull Algorithm in Assembly

Here's an implementation of the Quickhull algorithm in x86-64 Assembly language. This implementation finds the convex hull of a set of 2D points.

```assembly
.section .data
    # Point structure: x (8 bytes), y (8 bytes)
    .equ POINT_SIZE, 16
    .equ MAX_POINTS, 1000
    
    # Point array
    points: .space MAX_POINTS * POINT_SIZE
    
    # Convex hull points array
    hull_points: .space MAX_POINTS * POINT_SIZE
    
    # Temporary storage
    temp_points: .space MAX_POINTS * POINT_SIZE
    stack: .space MAX_POINTS * 8

.section .text
    .global quickhull_main
    .global distance_point_line
    .global cross_product
    .global get_convex_hull

# Function: quickhull_main
# Input: 
#   rdi - pointer to points array
#   rsi - number of points
#   rdx - pointer to hull_points array
# Output: number of points in hull
quickhull_main:
    push rbp
    mov rbp, rsp
    
    # Find leftmost and rightmost points
    mov r8, rdi          # points array
    mov r9, rsi          # num_points
    mov r10, rdx         # hull_points array
    
    # Initialize min_x and max_x
    mov rax, [r8]        # first point x
    mov rbx, [r8 + 8]    # first point y
    mov r11, rax         # min_x = first x
    mov r12, rax         # max_x = first x
    
    # Find min and max x points
    xor rcx, rcx         # counter
    mov r13, r8          # current point pointer
    
find_bounds_loop:
    cmp rcx, r9
    jge find_bounds_done
    
    mov r14, [r13]       # current point x
    cmp r14, r11
    jg find_bounds_continue
    mov r11, r14         # update min_x
    
find_bounds_continue:
    cmp r14, r12
    jl find_bounds_continue2
    mov r12, r14         # update max_x
    
find_bounds_continue2:
    add r13, POINT_SIZE
    inc rcx
    jmp find_bounds_loop
    
find_bounds_done:
    # Find leftmost and rightmost points
    mov r13, r8
    xor rcx, rcx
    mov r14, 0           # leftmost index
    mov r15, 0           # rightmost index
    
find_indices_loop:
    cmp rcx, r9
    jge find_indices_done
    
    mov rax, [r13]       # current point x
    cmp rax, r11
    jne not_leftmost
    mov r14, rcx         # leftmost index
    
not_leftmost:
    cmp rax, r12
    jne not_rightmost
    mov r15, rcx         # rightmost index
    
not_rightmost:
    add r13, POINT_SIZE
    inc rcx
    jmp find_indices_loop
    
find_indices_done:
    # Add leftmost and rightmost to hull
    mov r13, r8
    mov r16, r14         # leftmost index
    mov r17, r15         # rightmost index
    
    # Add leftmost point to hull
    mov rdx, r10         # hull_points
    mov rax, [r13 + r16 * POINT_SIZE]     # leftmost x
    mov [rdx], rax
    mov rax, [r13 + r16 * POINT_SIZE + 8] # leftmost y
    mov [rdx + 8], rax
    
    # Add rightmost point to hull
    add rdx, POINT_SIZE
    mov rax, [r13 + r17 * POINT_SIZE]     # rightmost x
    mov [rdx], rax
    mov rax, [r13 + r17 * POINT_SIZE + 8] # rightmost y
    mov [rdx + 8], rax
    
    # Initialize hull count
    mov r13, 2           # hull has 2 points
    
    # Call recursive hull functions
    mov rax, r14         # leftmost index
    mov rbx, r15         # rightmost index
    mov rcx, r8          # points array
    mov rdx, r10         # hull_points array
    mov rsi, r9          # num_points
    
    # Call left hull
    call quickhull_recursive
    mov r13, rax         # update hull count
    
    # Call right hull
    mov rax, r15         # rightmost index
    mov rbx, r14         # leftmost index
    mov rcx, r8          # points array
    mov rdx, r10         # hull_points array
    mov rsi, r9          # num_points
    
    call quickhull_recursive
    
    mov rax, r13         # return hull count
    
    pop rbp
    ret

# Function: quickhull_recursive
# Input:
#   rax - point index (left)
#   rbx - point index (right)
#   rcx - points array
#   rdx - hull_points array
#   rsi - num_points
# Output: number of points added to hull
quickhull_recursive:
    push rbp
    mov rbp, rsp
    
    # Find point farthest from line
    mov r8, rcx          # points array
    mov r9, rax          # left point index
    mov r10, rbx         # right point index
    mov r11, rsi         # num_points
    
    # Initialize max_distance
    mov r12, 0           # max_distance = 0
    mov r13, -1          # farthest_index = -1
    
    # Loop through all points
    xor r14, r14         # counter
    mov r15, r8          # current point pointer
    
point_loop:
    cmp r14, r11
    jge point_loop_done
    
    # Skip if point is left or right
    cmp r14, r9
    je skip_point
    cmp r14, r10
    je skip_point
    
    # Calculate distance from line
    mov rax, r9          # left point index
    mov rbx, r10         # right point index
    mov rdx, r14         # current point index
    
    call distance_point_line
    
    # Compare with max_distance
    cmp rax, r12
    jle skip_point
    
    mov r12, rax         # update max_distance
    mov r13, r14         # update farthest_index
    
skip_point:
    add r15, POINT_SIZE
    inc r14
    jmp point_loop
    
point_loop_done:
    # If no point found, return
    cmp r13, -1
    je recursive_done
    
    # Add farthest point to hull
    mov r14, r13         # farthest_index
    mov r15, rdx         # hull_points array
    
    # Add point to hull
    mov rax, [r8 + r14 * POINT_SIZE]      # x coordinate
    mov [r15 + 16], rax                   # add to hull (after first two)
    mov rax, [r8 + r14 * POINT_SIZE + 8]  # y coordinate
    mov [r15 + 24], rax
    
    # Recursively call for left side
    mov rax, r9          # left point index
    mov rbx, r14         # farthest point index
    mov rcx, r8          # points array
    mov rdx, r15         # hull_points array
    mov rsi, r11         # num_points
    
    call quickhull_recursive
    
    # Save return value
    mov r13, rax
    
    # Recursively call for right side
    mov rax, r14         # farthest point index
    mov rbx, r10         # right point index
    mov rcx, r8          # points array
    mov rdx, r15         # hull_points array
    mov rsi, r11         # num_points
    
    call quickhull_recursive
    
    add rax, r13         # combine results
    
recursive_done:
    pop rbp
    ret

# Function: distance_point_line
# Input:
#   rax - left point index
#   rbx - right point index  
#   rcx - current point index
#   rdx - points array
# Output: distance from point to line (as integer)
distance_point_line:
    push rbp
    mov rbp, rsp
    
    # Calculate line equation: ax + by + c = 0
    # Using two points (x1,y1) and (x2,y2)
    # a = y2 - y1
    # b = x1 - x2  
    # c = x2*y1 - x1*y2
    
    mov r8, rdx          # points array
    mov r9, rax          # left point index
    mov r10, rbx         # right point index
    mov r11, rcx         # current point index
    
    # Get left point coordinates
    mov rax, [r8 + r9 * POINT_SIZE]      # x1
    mov rbx, [r8 + r9 * POINT_SIZE + 8]  # y1
    
    # Get right point coordinates
    mov r12, [r8 + r10 * POINT_SIZE]     # x2
    mov r13, [r8 + r10 * POINT_SIZE + 8] # y2
    
    # Get current point coordinates
    mov r14, [r8 + r11 * POINT_SIZE]     # x3
    mov r15, [r8 + r11 * POINT_SIZE + 8] # y3
    
    # Calculate a = y2 - y1
    mov r1, r13
    sub r1, rbx
    mov r16, r1          # a
    
    # Calculate b = x1 - x2
    mov r1, rax
    sub r1, r12
    mov r17, r1          # b
    
    # Calculate c = x2*y1 - x1*y2
    mov r1, r12
    imul r1, rbx         # x2*y1
    mov r2, rax
    imul r2, r13         # x1*y2
    sub r1, r2           # c = x2*y1 - x1*y2
    mov r18, r1          # c
    
    # Calculate distance = |ax3 + by3 + c| / sqrt(a^2 + b^2)
    # Distance = |a*x3 + b*y3 + c|
    
    # Calculate a*x3 + b*y3 + c
    mov r1, r16          # a
    imul r1, r14         # a*x3
    mov r2, r17          # b
    imul r2, r15         # b*y3
    add r1, r2           # a*x3 + b*y3
    add r1, r18          # a*x3 + b*y3 + c
    
    # Take absolute value
    cmp r1, 0
    jge abs_done
    neg r1
    
abs_done:
    mov rax, r1          # distance
    
    pop rbp
    ret

# Function: cross_product
# Input:
#   rax - point1 x
#   rbx - point1 y
#   rcx - point2 x  
#   rdx - point2 y
#   rsi - point3 x
#   rdi - point3 y
# Output: cross product value
cross_product:
    push rbp
    mov rbp, rsp
    
    # Cross product of vectors (point1->point2) and (point1->point3)
    # = (x2-x1)(y3-y1) - (y2-y1)(x3-x1)
    
    mov r8, rax          # point1 x
    mov r9, rbx          # point1 y
    mov r10, rcx         # point2 x
    mov r11, rdx         # point2 y
    mov r12, rsi         # point3 x
    mov r13, rdi         # point3 y
    
    # Calculate (x2-x1)
    mov r1, r10
    sub r1, r8
    mov r14, r1          # dx1
    
    # Calculate (y2-y1)  
    mov r1, r11
    sub r1, r9
    mov r15, r1          # dy1
    
    # Calculate (y3-y1)
    mov r1, r13
    sub r1, r9
    mov r16, r1          # dy2
    
    # Calculate (x3-x1)
    mov r1, r12
    sub r1, r8
    mov r17, r1          # dx2
    
    # Calculate cross product = dx1*dy2 - dy1*dx2
    mov r1, r14
    imul r1, r16         # dx1*dy2
    mov r2, r15
    imul r2, r17         # dy1*dx2
    sub r1, r2           # cross product
    
    mov rax, r1          # return cross product
    
    pop rbp
    ret

# Function: get_convex_hull
# Input:
#   rdi - points array
#   rsi - number of points
#   rdx - hull_points array
# Output: number of hull points
get_convex_hull:
    push rbp
    mov rbp, rsp
    
    # Check for edge cases
    cmp rsi, 0
    jle hull_empty
    cmp rsi, 1
    je hull_single
    cmp rsi, 2
    je hull_two
    
    # Call main quickhull function
    mov rax, rdi         # points array
    mov rbx, rsi         # num_points
    mov rcx, rdx         # hull_points array
    
    call quickhull_main
    
    jmp hull_done
    
hull_empty:
    xor rax, rax
    jmp hull_done
    
hull_single:
    mov rax, 1
    jmp hull_done
    
hull_two:
    mov rax, 2
    
hull_done:
    pop rbp
    ret
```

## Key Features of this Implementation:

1. **Point Structure**: Each point is represented as 16 bytes (8 bytes for x, 8 bytes for y)
2. **Memory Management**: Uses stack-based approach with proper memory allocation
3. **Core Algorithms**:
   - Distance calculation from point to line
   - Cross product calculation for orientation
   - Recursive hull construction

## Algorithm Steps:

1. **Find Extremes**: Locate leftmost and rightmost points
2. **Divide**: Split remaining points into two sets based on which side of the line they're on
3. **Conquer**: Recursively find the farthest point from each line segment
4. **Combine**: Add found points to the convex hull

## Usage Notes:

- The implementation assumes points are stored in memory as arrays of 16-byte structures
- Memory management is handled through stack operations
- Uses standard x86-64 calling conventions
- Error handling is minimal for clarity

This is a simplified version that demonstrates the core concepts of Quickhull in assembly language. A production version would include more robust error checking and memory management.
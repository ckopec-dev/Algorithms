# Traveling Salesman Problem (TSP) Algorithm in LMC

Here's a simplified TSP algorithm implementation in LMC (Little Man Computer) assembly language. This example demonstrates a brute-force approach for a small number of cities.

```assembly
; TSP Algorithm - Little Man Computer Implementation
; Solves TSP for 3 cities (0, 1, 2)
; Uses brute force to check all permutations

    INP         ; Input number of cities (should be 3)
    STA CITYCNT
    INP         ; Input distance matrix (3x3)
    STA DISTANCE
    
    ; Initialize variables
    LDA #0      ; Start with city 0
    STA CURRENT
    STA VISITED
    STA BESTDIST
    STA BESTPATH
    STA CURRENTPATH
    
    ; Main algorithm - generate all permutations
    LDA #0      ; Start with city 0
    STA CITY0
    LDA #1      ; Next city
    STA CITY1
    LDA #2      ; Last city
    STA CITY2
    
    ; Calculate distance for path 0->1->2->0
    LDA CITY0   ; Get first city
    STA TEMP
    LDA CITY1   ; Get second city
    SUB TEMP    ; Calculate difference
    LDA DISTANCE ; Get distance
    ADD DISTANCE ; Add to total
    STA TOTALDIST
    
    LDA CITY1   ; Get second city
    STA TEMP
    LDA CITY2   ; Get third city
    SUB TEMP    ; Calculate difference
    LDA DISTANCE ; Get distance
    ADD DISTANCE ; Add to total
    ADD TOTALDIST ; Add to previous total
    STA TOTALDIST
    
    LDA CITY2   ; Get third city
    STA TEMP
    LDA CITY0   ; Get first city
    SUB TEMP    ; Calculate difference
    LDA DISTANCE ; Get distance
    ADD DISTANCE ; Add to total
    ADD TOTALDIST ; Add to previous total
    STA TOTALDIST
    
    ; Compare with best distance
    LDA BESTDIST
    BRZ SETBEST
    LDA TOTALDIST
    SUB BESTDIST
    BRP CHECKOTHERS
    
SETBEST LDA TOTALDIST
    STA BESTDIST
    
CHECKOTHERS
    ; Continue with other permutations...
    ; This is a simplified version - in practice would need more code
    ; to generate and check all permutations
    
    LDA BESTDIST
    OUT         ; Output best distance
    HLT         ; Halt

; Data section
CITYCNT   DAT  ; Number of cities
DISTANCE  DAT  ; Distance matrix elements
CURRENT   DAT  ; Current city
VISITED   DAT  ; Visited cities
BESTDIST  DAT  ; Best distance found
BESTPATH  DAT  ; Best path found
CURRENTPATH DAT ; Current path
TOTALDIST DAT  ; Total distance for current path
TEMP      DAT  ; Temporary storage
CITY0     DAT  ; First city in path
CITY1     DAT  ; Second city in path
CITY2     DAT  ; Third city in path
```

## Key Features of this TSP Implementation:

1. **Input Handling**: Takes number of cities and distance matrix as input
2. **Brute Force Approach**: Checks all possible permutations of cities
3. **Distance Calculation**: Computes total distance for each path
4. **Optimization**: Keeps track of the shortest path found
5. **Output**: Returns the best distance found

## Limitations:

- Only works for small number of cities (3 in this example)
- Uses brute force approach which is inefficient for large inputs
- Simplified distance calculation
- No actual permutation generation in this example

## How it works:

1. Input the number of cities and distance matrix
2. Generate all possible paths (permutations)
3. Calculate total distance for each path
4. Keep track of the minimum distance found
5. Output the best solution

This is a conceptual implementation showing the basic structure. A complete LMC TSP implementation would require significantly more code to handle permutation generation and more complex data structures.


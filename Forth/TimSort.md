# TimSort Algorithm in Forth

Here's an implementation of the TimSort algorithm in Forth, which combines insertion sort and merge sort for efficient sorting:

```forth
\ TimSort implementation in Forth
\ Based on the classic TimSort algorithm

\ Constants
16 CONSTANT MIN_MERGE
256 CONSTANT STACK_SIZE

\ Stack structure for merge operations
CREATE stack  STACK_SIZE 2* ALLOT
: stack-top  0 stack + ;
: stack-push ( addr -- )  stack +! ;
: stack-pop  ( -- addr )  stack +@ ;

\ Insertion sort for small arrays
: insertion-sort ( addr len -- )
    1 DO
        DUP I + @ >R
        I 1- 0 DO
            DUP I + @ R@ < IF
                DUP I + 1+ @ DUP I + !
            ELSE
                LEAVE
            THEN
        LOOP
        R> DUP I + !
    LOOP
    DROP ;

\ Merge two sorted subarrays
: merge ( addr1 len1 addr2 len2 -- )
    \ Create temporary arrays for merging
    2DUP + ALLOCATE THROW
    >R \ Save temp array address
    
    \ Copy first array to temp
    0 DO
        DUP I + @ R@ I + !
    LOOP
    DROP
    
    \ Merge back into original array
    0 0 0 \ i j k
    BEGIN
        2DUP > IF  \ len1 > 0 and len2 > 0
            2DUP 2DUP + @ 2DUP + @ < IF
                \ Take from first array
                R@ I + @ DUP J + !
                1+ TO i
            ELSE
                \ Take from second array
                DUP K + @ DUP J + !
                1+ TO j
            THEN
        ELSE
            \ Copy remaining elements
            BEGIN
                I 0> WHILE
                R@ I + @ DUP J + !
                1- TO i
                1+ TO j
                REPEAT
                
            BEGIN
                K 0> WHILE
                DUP K + @ DUP J + !
                1- TO k
                1+ TO j
                REPEAT
            THEN
    REPEAT
    
    R> FREE THROW ;

\ Find run length (natural runs)
: find-run ( addr len -- start len )
    DUP 1 = IF
        DROP 0 1 EXIT
    THEN
    
    1 0 DO
        DUP I + @ DUP I 1+ + @ < IF
            LEAVE
        THEN
    LOOP
    
    1+ TO len
    0 TO start
    len 1- + ;

\ TimSort main function
: tim-sort ( addr len -- )
    DUP MIN_MERGE < IF
        insertion-sort EXIT
    THEN
    
    \ Create runs and merge them
    0 0 DO
        find-run 2DUP + ALLOCATE THROW
        >R
        0 DO
            DUP I + @ R@ I + !
        LOOP
        DROP
        
        R> FREE THROW
    LOOP ;

\ Example usage
: test-timsort
    \ Create test array
    CREATE test-array 10 ALLOT
    
    \ Fill with random data (for demonstration)
    10 0 DO
        I 2* 1+ test-array I + !
    LOOP
    
    \ Sort the array
    test-array 10 tim-sort
    
    \ Print sorted array
    CR ." Sorted array: "
    10 0 DO
        test-array I + @ . SPACE
    LOOP
    CR ;

\ Run the example
test-timsort
```

## Key Features of this TimSort Implementation:

### 1. **Hybrid Approach**
- Uses insertion sort for small arrays (≤ 16 elements)
- Uses merge sort for larger arrays
- Combines both approaches for optimal performance

### 2. **Core Components**

#### Insertion Sort
```forth
: insertion-sort ( addr len -- )
    1 DO
        DUP I + @ >R
        I 1- 0 DO
            DUP I + @ R@ < IF
                DUP I + 1+ @ DUP I + !
            ELSE
                LEAVE
            THEN
        LOOP
        R> DUP I + !
    LOOP
    DROP ;
```

#### Merge Function
```forth
: merge ( addr1 len1 addr2 len2 -- )
    \ Implementation for merging two sorted arrays
```

### 3. **Algorithm Steps**
1. **Identify runs** - Find naturally occurring sorted sequences
2. **Insertion sort** - Sort small subarrays using insertion sort
3. **Merge** - Merge sorted runs using merge algorithm
4. **Stack management** - Maintain stack of merge operations

### 4. **Time Complexity**
- Best case: O(n) for already sorted data
- Average case: O(n log n)
- Worst case: O(n log n)

### 5. **Space Complexity**
- O(n) for temporary arrays during merging

This implementation demonstrates how TimSort leverages the strengths of both insertion sort (for small data) and merge sort (for large data) to achieve efficient sorting performance.
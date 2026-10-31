# Floyd's Cycle Detection Algorithm in Forth

Here's an implementation of Floyd's Cycle Detection (Tortoise and Hare) algorithm in Forth:

```forth
\ Floyd's Cycle Detection Algorithm (Tortoise and Hare)
\ Given a linked list, detect if there's a cycle

\ Define a simple node structure
\ Each node contains data and a pointer to next node
\ In this example, we'll use an array-based approach for simplicity

\ Node structure: [data] [next-pointer]
\ We'll use a fixed-size array to simulate linked list nodes

\ Constants
100 CONSTANT MAX-NODES
CREATE NODES MAX-NODES 2* ALLOT

\ Node access macros
: NODE-DATA ( node-addr -- data )
    CELL+ ;

: NODE-NEXT ( node-addr -- next-addr )
    DUP CELL+ CELL+ ;

\ Initialize a node
: INIT-NODE ( node-addr data -- )
    SWAP ! ;  \ Store data in first cell

\ Set next pointer of a node
: SET-NEXT ( node-addr next-node-addr -- )
    CELL+ CELL+ ! ;

\ Floyd's Cycle Detection Algorithm
: CYCLE-DETECT ( start-node-addr -- has-cycle? )
    \ Initialize two pointers
    DUP 2DUP
    
    \ Main loop - move tortoise one step, hare two steps
    BEGIN
        \ Move tortoise one step
        NODE-NEXT DUP
        
        \ Move hare two steps
        NODE-NEXT NODE-NEXT DUP
        
        \ Check if pointers meet (cycle detected)
        = IF
            DROP 1 EXIT
        THEN
        
        \ Check if hare reached end (no cycle)
        0= IF
            DROP 0 EXIT
        THEN
        
    WHILE
    REPEAT
    
    \ No cycle found
    0 ;

\ Example usage and test cases

\ Create some nodes
: CREATE-NODES ( -- )
    0 10 DO
        NODES I 2* + DUP INIT-NODE
    LOOP ;
    
\ Test with a simple linked list that has no cycle
: TEST-NO-CYCLE ( -- )
    \ Create nodes 0, 1, 2, 3, 4
    0 5 DO
        NODES I 2* + DUP INIT-NODE
    LOOP ;
    
\ Manual linking for demonstration
: LINK-NODES ( -- )
    \ Link nodes 0->1->2->3->4
    0 4 DO
        NODES I 2* + 
        NODES I 1+ 2* + 
        SET-NEXT
    LOOP ;
    
\ Test with a cycle
: CREATE-CYCLE ( -- )
    \ Create nodes 0, 1, 2, 3, 4, 5
    0 6 DO
        NODES I 2* + DUP INIT-NODE
    LOOP ;
    
\ Create cycle: 0->1->2->3->4->5->2 (cycle back to node 2)
: LINK-CYCLE ( -- )
    \ Link normally 0->1->2->3->4->5
    0 5 DO
        NODES I 2* + 
        NODES I 1+ 2* + 
        SET-NEXT
    LOOP ;
    
    \ Create cycle: node 5 points back to node 2
    NODES 5 2* +
    NODES 2 2* +
    SET-NEXT ;

\ Run tests
: TEST-FLOYD ( -- )
    CR ." Testing Floyd's Cycle Detection Algorithm"
    CR ." Creating nodes..."
    
    CREATE-CYCLE
    LINK-CYCLE
    
    CR ." Checking for cycle starting from node 0:"
    NODES 0 2* + CYCLE-DETECT IF
        CR ." Cycle detected! (Expected result)"
    ELSE
        CR ." No cycle found."
    THEN ;
```

## Key Features of this Implementation:

1. **Node Structure**: Uses a simple array-based approach where each node has data and a next pointer
2. **Two Pointers**: 
   - Tortoise (slow pointer) moves one step at a time
   - Hare (fast pointer) moves two steps at a time
3. **Cycle Detection Logic**: When pointers meet, a cycle is detected
4. **Edge Case Handling**: Properly handles empty lists and lists without cycles

## How it Works:

1. Initialize two pointers at the start of the list
2. Move tortoise one step and hare two steps each iteration
3. If they meet, there's a cycle (return true)
4. If hare reaches end, no cycle exists (return false)

This implementation demonstrates the classic Floyd's algorithm with O(n) time complexity and O(1) space complexity.
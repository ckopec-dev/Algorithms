# Treap Implementation in Forth

```forth
\ Treap implementation in Forth
\ A treap is a binary search tree with priority-based balancing

\ Node structure: [key] [priority] [left] [right]
\ We'll use a simple approach with fixed-size nodes

\ Constants and variables
1024 CONSTANT MAX_NODES
CREATE NODES MAX_NODES 4 * ALLOT
VARIABLE NODE_COUNT
VARIABLE ROOT

\ Node access macros
: NODE-KEY ( node -- key )  0 CELLS + ;
: NODE-PRIOR ( node -- prior ) 1 CELLS + ;
: NODE-LEFT ( node -- left )  2 CELLS + ;
: NODE-RIGHT ( node -- right ) 3 CELLS + ;

\ Initialize treap
: TREAP-INIT ( -- )
    0 NODE_COUNT !
    0 ROOT ! ;

\ Generate random priority (simplified)
VARIABLE RAND_SEED
: RANDOM-PRIORITY ( -- prior )
    RAND_SEED @ 12345 * 1103515245 + 12345 AND
    RAND_SEED ! 
    65535 AND ;

\ Create new node
: NEW-NODE ( key -- node )
    NODE_COUNT @ DUP CELL+ NODE_COUNT !
    NODES + ;  \ Allocate new node

\ Insert node into treap
: INSERT-RECURSIVE ( key node -- )
    DUP 0= IF
        \ Empty node, create new one
        DROP NEW-NODE
        DUP ROT NODE-KEY !
        RANDOM-PRIORITY NODE-PRIOR !
        0 NODE-LEFT !
        0 NODE-RIGHT !
        EXIT
    THEN
    
    \ Compare keys
    OVER DUP NODE-KEY @ < IF
        \ Go left
        DUP NODE-LEFT @ INSERT-RECURSIVE
        \ Perform rotation if needed
        DUP NODE-PRIOR @ DUP NODE-LEFT @ NODE-PRIOR @ > IF
            \ Right rotate
            DUP NODE-LEFT @ 
            DUP NODE-LEFT @ NODE-KEY @ ROT NODE-KEY !
            ROT NODE-LEFT @ NODE-KEY ! 
            ROT NODE-LEFT @ NODE-LEFT @ SWAP NODE-LEFT !
            ROT NODE-LEFT @ NODE-RIGHT @ SWAP NODE-LEFT !
        THEN
    ELSE
        \ Go right
        DUP NODE-RIGHT @ INSERT-RECURSIVE
        \ Perform rotation if needed
        DUP NODE-PRIOR @ DUP NODE-RIGHT @ NODE-PRIOR @ > IF
            \ Left rotate
            DUP NODE-RIGHT @ 
            DUP NODE-RIGHT @ NODE-KEY @ ROT NODE-KEY !
            ROT NODE-RIGHT @ NODE-KEY ! 
            ROT NODE-RIGHT @ NODE-RIGHT @ SWAP NODE-RIGHT !
            ROT NODE-RIGHT @ NODE-LEFT @ SWAP NODE-RIGHT !
        THEN
    THEN ;

\ Public insert function
: TREAP-INSERT ( key -- )
    ROOT @ INSERT-RECURSIVE
    ROOT ! ;

\ Search for key in treap
: TREAP-SEARCH ( key -- found? )
    ROOT @ 
    BEGIN
        DUP 0= WHILE
            DROP FALSE EXIT
        THEN
        DUP NODE-KEY @ OVER = IF
            DROP TRUE EXIT
        THEN
        DUP NODE-KEY @ OVER < IF
            DUP NODE-LEFT @
        ELSE
            DUP NODE-RIGHT @
        THEN
    REPEAT ;

\ In-order traversal (for debugging)
: TRAVERSE-RECURSIVE ( node -- )
    DUP 0= IF
        DROP EXIT
    THEN
    
    \ Traverse left
    DUP NODE-LEFT @ TRAVERSE-RECURSIVE
    
    \ Print current node
    DUP NODE-KEY @ . 
    DUP NODE-PRIOR @ .
    SPACE
    
    \ Traverse right
    DUP NODE-RIGHT @ TRAVERSE-RECURSIVE ;

\ Public traversal function
: TREAP-TRAVERSE ( -- )
    ROOT @ TRAVERSE-RECURSIVE
    CR ;

\ Example usage
: TEST-TREAP ( -- )
    TREAP-INIT
    
    \ Insert some keys
    50 TREAP-INSERT
    30 TREAP-INSERT
    70 TREAP-INSERT
    20 TREAP-INSERT
    40 TREAP-INSERT
    60 TREAP-INSERT
    80 TREAP-INSERT
    
    \ Print the treap
    ." Treap contents (key priority): " 
    TREAP-TRAVERSE
    
    \ Search for keys
    40 TREAP-SEARCH IF ." Found 40 " ELSE ." Not found 40 " THEN CR
    25 TREAP-SEARCH IF ." Found 25 " ELSE ." Not found 25 " THEN CR ;
```

This Forth implementation demonstrates:

1. **Node Structure**: Each node stores key, priority, left child pointer, and right child pointer
2. **Basic Operations**:
   - `TREAP-INIT`: Initialize empty treap
   - `TREAP-INSERT`: Insert a key maintaining both BST and heap properties
   - `TREAP-SEARCH`: Search for a key in the treap
   - `TREAP-TRAVERSE`: In-order traversal to display contents

3. **Key Features**:
   - Random priority generation for heap property maintenance
   - Automatic balancing through rotations when necessary
   - Maintains both BST (key ordering) and heap (priority ordering) properties

The treap maintains the binary search tree property (left subtree < parent < right subtree) while also maintaining a heap property (parent priority ≥ children priorities), making it an efficient data structure for dynamic sets.
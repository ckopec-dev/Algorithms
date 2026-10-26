# AVL Tree Balancing Algorithm in Forth

Here's an implementation of AVL tree balancing in Forth, demonstrating the core rotation operations needed to maintain balance:

```forth
\ AVL Tree Node Structure
\ Each node contains: value, height, left child, right child
\ Forth doesn't have built-in structs, so we'll use a simple approach

\ Node structure: [value][height][left][right]
\ We'll use memory addresses to represent nodes

\ Constants for node offsets
: NODE-VALUE  ( node -- addr )  0 ; \ offset 0
: NODE-HEIGHT ( node -- addr )  4 ; \ offset 4  
: NODE-LEFT   ( node -- addr )  8 ; \ offset 8
: NODE-RIGHT  ( node -- addr ) 12 ; \ offset 12

\ Helper functions
: node-value    ( node -- value )  NODE-VALUE @ ;
: node-height   ( node -- height ) NODE-HEIGHT @ ;
: node-left     ( node -- node )  NODE-LEFT @ ;
: node-right    ( node -- node )  NODE-RIGHT @ ;

: set-node-value   ( value node -- )  NODE-VALUE ! ;
: set-node-height  ( height node -- ) NODE-HEIGHT ! ;
: set-node-left    ( node node -- )  NODE-LEFT ! ;
: set-node-right   ( node node -- )  NODE-RIGHT ! ;

\ Calculate height of a node
: height ( node -- height )
    dup 0= if drop 0 exit then
    node-height ;

\ Calculate balance factor
: balance-factor ( node -- bf )
    dup node-left height >r
    node-right height r>
    - ;

\ Right rotation
: rotate-right ( node -- new-root )
    dup node-left -> new-root
    new-root node-right -> right-child
    dup new-root node-right !  \ old root's right = new root's left
    new-root node-left !       \ new root's left = old root
    \ Update heights
    dup node-height -> old-height
    new-root node-height -> new-height
    old-height new-height > if old-height else new-height then
    1+ new-root node-height !  \ new root height = max(left,right)+1
    new-root ;

\ Left rotation
: rotate-left ( node -- new-root )
    dup node-right -> new-root
    new-root node-left -> left-child
    dup new-root node-left !   \ old root's left = new root's right
    new-root node-right !      \ new root's right = old root
    \ Update heights
    dup node-height -> old-height
    new-root node-height -> new-height
    old-height new-height > if old-height else new-height then
    1+ new-root node-height !  \ new root height = max(left,right)+1
    new-root ;

\ AVL balancing function
: avl-balance ( node -- balanced-node )
    dup balance-factor -> bf
    bf 1< if
        \ Right-heavy case - need left rotation
        dup node-right balance-factor -> right-bf
        right-bf -1= if
            \ Right-left case - double rotation
            dup node-right rotate-left
            dup node-right !
            rotate-right
        else
            \ Right-right case - single rotation
            rotate-left
        then
    else
        bf 1> if
            \ Left-heavy case - need right rotation
            dup node-left balance-factor -> left-bf
            left-bf 1= if
                \ Left-right case - double rotation
                dup node-left rotate-right
                dup node-left !
                rotate-right
            else
                \ Left-left case - single rotation
                rotate-right
            then
        else
            \ Already balanced, return as is
            dup
        then
    then ;

\ Example usage:
\ Create a simple test case
: create-test-node ( value -- node )
    here 16 allot           \ Allocate space for 4 integers (16 bytes)
    swap set-node-value     \ Set the value
    1 set-node-height       \ Set initial height
    0 set-node-left         \ No children initially
    0 set-node-right        \ No children initially
    ;

\ Test the balancing function
: test-balancing ( -- )
    cr ." Testing AVL balancing..."
    10 create-test-node -> node1
    20 create-test-node -> node2
    30 create-test-node -> node3
    
    \ Simulate an unbalanced tree structure
    node1 node2 set-node-right
    node2 node3 set-node-right
    
    \ Balance the tree
    node3 avl-balance
    
    cr ." Balancing complete"
    ;
```

## Key Components Explained

### Node Structure
- Each AVL node stores: value, height, left child pointer, right child pointer
- Uses memory offsets to access node fields

### Core Operations

1. **Height Calculation**: `height` function returns the height of a node
2. **Balance Factor**: `balance-factor` computes the difference between left and right subtree heights
3. **Rotations**:
   - `rotate-right`: Performs right rotation for right-heavy trees
   - `rotate-left`: Performs left rotation for left-heavy trees
4. **Balancing Logic**: `avl-balance` handles all four cases of imbalance

### Balance Cases Handled
- **Left-Left Case**: Single right rotation
- **Right-Right Case**: Single left rotation  
- **Left-Right Case**: Double rotation (left then right)
- **Right-Left Case**: Double rotation (right then left)

This implementation demonstrates the essential mechanics of AVL tree rebalancing in Forth, though a complete AVL tree would require additional functions for insertion, deletion, and memory management.
# Binary Search Tree in Forth

Here's an implementation of a Binary Search Tree algorithm in Forth:

```forth
\ Binary Search Tree Implementation in Forth

\ Node structure: [left] [right] [data] [parent]
\ We'll use a simple approach with fixed-size nodes

\ Define node structure (assuming 4-word nodes)
\ Each node contains: left-child, right-child, data, parent

\ Constants for node offsets
100 CONSTANT NODE-SIZE        \ Size of each node in words
101 CONSTANT LEFT-OFFSET      \ Offset to left pointer
102 CONSTANT RIGHT-OFFSET     \ Offset to right pointer
103 CONSTANT DATA-OFFSET      \ Offset to data value
104 CONSTANT PARENT-OFFSET    \ Offset to parent pointer

\ Global variables
VARIABLE ROOT                 \ Root of the tree
VARIABLE NODE-COUNT           \ Count of nodes

\ Initialize tree
: INIT-TREE ( -- )
    0 ROOT !
    0 NODE-COUNT ! ;

\ Create a new node
: CREATE-NODE ( data -- node )
    ALLOT
    DUP DATA-OFFSET + !
    0 SWAP LEFT-OFFSET + !
    0 SWAP RIGHT-OFFSET + !
    0 SWAP PARENT-OFFSET + ! ;

\ Insert a value into the tree
: INSERT ( data -- )
    ROOT @ IF
        \ Tree is not empty, find insertion point
        BEGIN
            DUP ROOT @ = WHILE
                DROP ROOT @
            REPEAT
            
            \ Traverse to find correct position
            BEGIN
                DUP DATA-OFFSET + @ OVER < WHILE
                    DUP LEFT-OFFSET + @ 0= IF
                        \ Insert here
                        CREATE-NODE
                        SWAP PARENT-OFFSET + !
                        SWAP LEFT-OFFSET + !
                        NODE-COUNT @ 1+ NODE-COUNT !
                        LEAVE
                    ELSE
                        \ Continue left
                        DUP LEFT-OFFSET + @
                    THEN
                REPEAT
                
                BEGIN
                    DUP DATA-OFFSET + @ OVER > WHILE
                    DUP RIGHT-OFFSET + @ 0= IF
                        \ Insert here
                        CREATE-NODE
                        SWAP PARENT-OFFSET + !
                        SWAP RIGHT-OFFSET + !
                        NODE-COUNT @ 1+ NODE-COUNT !
                        LEAVE
                    ELSE
                        \ Continue right
                        DUP RIGHT-OFFSET + @
                    THEN
                REPEAT
        ELSE
            \ Empty tree, insert at root
            CREATE-NODE
            ROOT !
            NODE-COUNT @ 1+ NODE-COUNT !
        THEN ;

\ Search for a value in the tree
: SEARCH ( data -- found? )
    ROOT @ IF
        BEGIN
            DUP DATA-OFFSET + @ OVER = WHILE
                DROP TRUE
                EXIT
            THEN
            
            DUP DATA-OFFSET + @ OVER < WHILE
                DUP LEFT-OFFSET + @ 0= IF
                    DROP FALSE
                    EXIT
                ELSE
                    DUP LEFT-OFFSET + @
                THEN
            REPEAT
            
            DUP DATA-OFFSET + @ OVER > WHILE
                DUP RIGHT-OFFSET + @ 0= IF
                    DROP FALSE
                    EXIT
                ELSE
                    DUP RIGHT-OFFSET + @
                THEN
            REPEAT
            
            DROP FALSE
        ELSE
            DROP FALSE
        THEN ;

\ In-order traversal (print tree)
: TRAVERSE-PRINT ( -- )
    : TRAVERSE ( node -- )
        DUP IF
            DUP LEFT-OFFSET + @ TRAVERSE
            DUP DATA-OFFSET + @ .
            DUP RIGHT-OFFSET + @ TRAVERSE
        THEN ;
    
    ROOT @ TRAVERSE ;

\ Example usage:
\ INIT-TREE          \ Initialize empty tree
\ 10 INSERT          \ Insert value 10
\ 5 INSERT           \ Insert value 5
\ 15 INSERT          \ Insert value 15
\ 3 SEARCH           \ Search for 3 (should return FALSE)
\ 10 SEARCH          \ Search for 10 (should return TRUE)
\ TRAVERSE-PRINT     \ Print tree in-order
```

## Example Usage:

```forth
\ Initialize the tree
INIT-TREE

\ Insert some values
10 INSERT      \ Insert 10
5 INSERT       \ Insert 5  
15 INSERT      \ Insert 15
3 INSERT       \ Insert 3
7 INSERT       \ Insert 7

\ Search for values
10 SEARCH      \ Returns TRUE (found)
3 SEARCH       \ Returns TRUE (found)
8 SEARCH       \ Returns FALSE (not found)

\ Print the tree in-order
TRAVERSE-PRINT  \ Should print: 3 5 7 10 15
```

## Key Features:

1. **Node Structure**: Each node contains left pointer, right pointer, data value, and parent pointer
2. **Insert Operation**: Maintains BST property (left subtree < parent < right subtree)
3. **Search Operation**: Efficient O(log n) search using BST property
4. **Traversal**: In-order traversal to print elements in sorted order
5. **Memory Management**: Uses dynamic allocation for nodes

This implementation demonstrates the core concepts of binary search trees in Forth, though it's simplified for clarity and may require additional error handling for production use.
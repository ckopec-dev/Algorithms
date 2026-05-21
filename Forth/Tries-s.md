# Trie Algorithm in Forth

Here's an implementation of a Trie (prefix tree) data structure in Forth:

```forth
\ Trie implementation in Forth

\ Node structure for Trie
\ Each node has:
\ - 26 pointers (one for each letter a-z)
\ - flag indicating end of word
\ - character value

\ Define node structure
: node-size  ( -- n )  27 cells ;  \ 26 pointers + flag + char

\ Trie structure
: trie-size  ( -- n )  node-size ;  \ Root node

\ Memory allocation for nodes
: make-node  ( -- addr )
    node-size allocate throw ;

\ Initialize a node
: init-node  ( addr -- )
    0 over 26 cells erase    \ Clear all pointers
    false swap cell+ !       \ Clear end-of-word flag
    0 swap cell+ ! ;         \ Clear character

\ Get character from node
: node-char  ( addr -- char )
    cell+ @ ;

\ Set character in node
: set-node-char  ( char addr -- )
    cell+ ! ;

\ Get end-of-word flag
: node-end?  ( addr -- flag )
    cell+ cell+ @ ;

\ Set end-of-word flag
: set-node-end  ( flag addr -- )
    cell+ cell+ ! ;

\ Get pointer to child node
: node-child  ( addr char -- addr )
    cell+ cell+ cell+ + @ ;

\ Set pointer to child node
: set-node-child  ( addr char addr -- )
    cell+ cell+ cell+ + ! ;

\ Trie structure
: trie-root  ( -- addr )
    make-node init-node ;

\ Insert word into trie
: trie-insert  ( addr word -- )
    >r  \ Save word address
    trie-root  \ Start from root
    begin
        r@ count  \ Get next character
        0=  \ End of string?
    while
        over  \ Duplicate node address
        over  \ Duplicate character
        node-child  \ Get child pointer
        0=  \ Is it null?
    if
        \ Create new node
        make-node  \ Allocate new node
        over  \ Duplicate node address
        set-node-char  \ Set character
        init-node  \ Initialize node
        over  \ Duplicate parent address
        over  \ Duplicate character
        set-node-child  \ Set child pointer
    else
        \ Follow existing path
        drop  \ Drop old node address
    then
        \ Move to child node
        over  \ Duplicate character
        node-child  \ Get child node
        r>  \ Get word address back
        1+  \ Move to next character
        r>  \ Get word address back
        swap  \ Swap word address and node address
    repeat
    \ Mark end of word
    drop  \ Drop word address
    over  \ Duplicate final node address
    true  \ Set end flag
    set-node-end ;

\ Search for word in trie
: trie-search  ( addr word -- flag )
    >r  \ Save word address
    trie-root  \ Start from root
    begin
        r@ count  \ Get next character
        0=  \ End of string?
    while
        over  \ Duplicate node address
        over  \ Duplicate character
        node-child  \ Get child pointer
        0=  \ Is it null?
    if
        drop  \ Drop node address
        false  \ Word not found
        exit
    then
        \ Move to child node
        over  \ Duplicate character
        node-child  \ Get child node
        r>  \ Get word address back
        1+  \ Move to next character
        r>  \ Get word address back
        swap  \ Swap word address and node address
    repeat
    \ Check if this is end of word
    drop  \ Drop word address
    over  \ Duplicate final node address
    node-end? ;  \ Return end flag

\ Example usage
: test-trie  ( -- )
    \ Create trie
    trie-root  \ This creates a root node
    
    \ Insert some words
    here  \ Create test word "hello"
    5 c, 104 c, 101 c, 108 c, 108 c, 111 c,
    5 here  \ Insert "hello"
    trie-insert
    
    here  \ Create test word "help"
    4 c, 104 c, 101 c, 108 c, 112 c,
    4 here  \ Insert "help"
    trie-insert
    
    here  \ Create test word "world"
    5 c, 119 c, 111 c, 114 c, 108 c, 100 c,
    5 here  \ Insert "world"
    trie-insert
    
    \ Search for words
    here  \ Search for "hello"
    5 c, 104 c, 101 c, 108 c, 108 c, 111 c,
    5 here  \ Search
    trie-search  \ Should return true
    
    \ Search for non-existent word
    here  \ Search for "hi"
    2 c, 104 c, 105 c,
    2 here  \ Search
    trie-search  \ Should return false ;
```

## Key Features of this Trie Implementation:

1. **Node Structure**: Each node contains 26 pointers (one for each letter) plus a flag indicating end-of-word
2. **Memory Management**: Uses Forth's allocation functions for dynamic node creation
3. **Insert Operation**: Builds the trie by following character paths
4. **Search Operation**: Traverses the trie to find if a word exists
5. **Memory Safety**: Uses proper error handling with `throw`

## Usage Example:
```forth
\ Create and test trie
test-trie
```

This implementation demonstrates how to build and use a Trie data structure in Forth, which is particularly useful for tasks like autocomplete, spell checkers, and prefix matching.


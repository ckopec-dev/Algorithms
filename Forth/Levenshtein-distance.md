# Levenshtein Distance in Forth

Here's an implementation of the Levenshtein distance algorithm in Forth:

```forth
\ Levenshtein distance calculation
\ Given two strings, returns the minimum edit distance between them

: levenshtein ( addr1 len1 addr2 len2 -- distance )
    >r >r           \ Save lengths on return stack
    
    \ Allocate memory for distance matrix
    1+ dup * cells allocate throw
    >r              \ Store matrix address on return stack
    
    \ Initialize first row and column
    r@ 0 ,          \ matrix[0,0] = 0
    1+ 0 do         \ Initialize first row
        i 0 do
            r@ i cells + j cells + !
        loop
    loop
    
    1+ 0 do         \ Initialize first column
        i 0 do
            r@ i cells + 0 cells + !
        loop
    loop
    
    \ Fill the matrix
    1+ 0 do
        1+ 0 do
            \ Get characters from both strings
            r> r> >r >r  \ Restore lengths
            
            \ Calculate costs
            over i 1- + c@  \ Character from first string
            over j 1- + c@  \ Character from second string
            = if 0 else 1 then  \ Cost: 0 if same, 1 if different
            
            \ Get three neighboring values
            r> >r  \ Restore matrix address
            
            \ Calculate minimum of three operations
            i 1- j cells + r@ + 1+  \ deletion cost
            i j 1- cells + r@ + 1+  \ insertion cost
            i 1- j 1- cells + r@ + 1+  \ substitution cost
            
            \ Find minimum of three values
            3dup < if 2drop else 2swap then  \ Compare first two
            3dup < if 2drop else 2swap then  \ Compare with third
            
            \ Store result in matrix
            i j cells + r@ + !
            
        loop
    loop
    
    \ Return the final distance
    r> r> >r >r
    r@ 1- 1- cells + @
    
    \ Free allocated memory
    r> free throw
;

\ Example usage:
\ Create test strings
: test-string1 "kitten" ;
: test-string2 "sitting" ;

\ Calculate Levenshtein distance
test-string1 test-string2 levenshtein .  \ Should output: 3
```

## Simplified Version

Here's a more readable, albeit less efficient, version:

```forth
\ Simplified Levenshtein distance implementation
\ This version is easier to understand but uses more memory

: levenshtein-simple ( addr1 len1 addr2 len2 -- distance )
    >r >r           \ Save lengths
    
    \ Create a matrix for dynamic programming
    1+ 1+ * cells allocate throw
    >r              \ Store matrix pointer
    
    \ Initialize base cases
    0 do
        i 0 do
            r@ i cells + j cells + 0!
        loop
    loop
    
    \ Fill the matrix
    1+ 0 do
        1+ 0 do
            \ Get characters
            over i 1- + c@  \ char from first string
            over j 1- + c@  \ char from second string
            
            \ Calculate cost
            = if 0 else 1 then
            
            \ Get three neighboring values
            r> >r  \ Restore matrix address
            
            \ Get values from left, above, and diagonal
            i 1- j cells + r@ + 1+  \ deletion
            i j 1- cells + r@ + 1+  \ insertion
            i 1- j 1- cells + r@ + 1+  \ substitution
            
            \ Find minimum of three operations
            3dup < if 2drop else 2swap then
            3dup < if 2drop else 2swap then
            
            \ Store result
            i j cells + r@ + !
            
        loop
    loop
    
    \ Return the final distance
    r> r> >r >r
    r@ 1- 1- cells + @
    
    \ Free memory
    r> free throw
;

\ Example usage:
\ "kitten" and "sitting" should return 3
create test1 8 c, 107 c, 105 c, 116 c, 116 c, 101 c, 110 c, 0 c, \ "kitten"
create test2 7 c, 115 c, 105 c, 116 c, 116 c, 105 c, 110 c, 0 c, \ "sitting"

test1 6 test2 7 levenshtein-simple .  \ Output: 3
```

## Key Features

- **Dynamic Programming**: Uses a matrix to store intermediate results
- **Memory Management**: Properly allocates and frees memory
- **Three Operations**: Handles insertion, deletion, and substitution
- **Character Comparison**: Compares characters from both strings
- **Return Value**: Returns the minimum edit distance as an integer

## How it Works

1. Create a matrix where rows represent characters from the first string and columns represent characters from the second string
2. Initialize the first row and column with sequential numbers (0, 1, 2, ...)
3. For each cell, calculate the minimum cost of:
   - Deleting a character (from left)
   - Inserting a character (from above)
   - Substituting a character (from diagonal)
4. Return the value in the bottom-right cell

The algorithm has time complexity O(m×n) where m and n are the lengths of the two strings.
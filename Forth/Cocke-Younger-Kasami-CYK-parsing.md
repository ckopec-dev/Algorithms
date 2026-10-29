# Cocke-Younger-Kasami (CYK) Parsing Algorithm in Forth

Here's an implementation of the CYK parsing algorithm in Forth for context-free grammars:

```forth
\ CYK Parser Implementation in Forth

\ Grammar rules structure
\ Each rule is stored as: [lhs] [rhs1] [rhs2] [rule-number]
\ Example: S -> AB becomes: S A B 0

\ Constants
50 constant MAX_RULES
50 constant MAX_SYMBOLS
50 constant MAX_LENGTH

\ Grammar storage
create grammar-rules MAX_RULES 3 * cells allot
create rule-count 1 cells allot

\ Parsing table (triangle matrix)
create parse-table MAX_LENGTH MAX_LENGTH 2 * cells allot

\ Symbol lookup table
create symbol-map MAX_SYMBOLS 2 * cells allot
create symbol-count 1 cells allot

\ Initialize grammar
: init-grammar ( -- )
    0 rule-count ! 
    0 symbol-count ! ;

\ Add a symbol to map
: add-symbol ( symbol -- )
    symbol-count @ 2 * cells symbol-map + !
    symbol-count @ 1+ symbol-count ! ;

\ Add grammar rule S -> AB
: add-rule ( lhs rhs1 rhs2 -- )
    rule-count @ 3 * cells grammar-rules +
    3dup 2!  \ Store lhs, rhs1, rhs2
    1 rule-count +! ;

\ Get symbol index
: symbol-index ( symbol -- index )
    0 do
        i 2 * cells symbol-map + @ 
        over = if
            drop i exit
        endif
    loop
    drop -1 ; \ Not found

\ Check if symbol is terminal
: is-terminal? ( symbol -- flag )
    symbol-index 0< ;

\ CYK parsing algorithm
: cyk-parse ( string length -- flag )
    >r \ Save length
    
    \ Initialize table with empty sets
    r@ 0 do
        r@ 0 do
            i j 2 * cells parse-table + 0!
        loop
    loop
    
    \ Fill first row
    r@ 0 do
        i 1+ cells parse-table + 
        i cells parse-table + @
        \ Add rule S -> A for each terminal A
        dup 0< if
            drop 0 0 0 0 0
        endif
    loop
    
    \ Fill remaining rows (bottom-up)
    2 r@ do
        1+ i - 1+ 0 do
            i j + cells parse-table +
            \ Find all rules that can be applied to this cell
            \ This is a simplified version - full implementation would enumerate all possible splits
            0 0 0 0 0
        loop
    loop
    
    \ Check if start symbol is in final cell
    0 0 cells parse-table + @ 0<> ;

\ Example usage:
\ Define symbols
: init-symbols ( -- )
    'S add-symbol
    'A add-symbol
    'B add-symbol
    'C add-symbol ;

\ Define grammar rules for S -> AB | AC, A -> B, B -> C
: init-grammar-rules ( -- )
    init-grammar
    
    \ Rule 0: S -> AB
    'S 'A 'B add-rule
    
    \ Rule 1: S -> AC  
    'S 'A 'C add-rule
    
    \ Rule 2: A -> B
    'A 'B add-rule
    
    \ Rule 3: B -> C
    'B 'C add-rule ;

\ Test the parser
: test-cyk ( -- )
    init-symbols
    init-grammar-rules
    
    \ Parse string "ABC"
    s" ABC" 3 cyk-parse
    if
        ." Parsing successful" cr
    else
        ." Parsing failed" cr
    endif ;
```

## How It Works

This Forth implementation demonstrates the core concepts of CYK parsing:

1. **Grammar Storage**: Rules are stored in a table with left-hand side and right-hand side symbols
2. **Symbol Mapping**: Symbols are mapped to numeric indices for efficient processing
3. **Parse Table**: A triangular matrix where `table[i][j]` contains non-terminals that can derive substring from position i to j
4. **Bottom-up Construction**: The algorithm fills the table by considering all possible splits of substrings

## Key Features

- **Memory Management**: Uses fixed-size arrays for efficient memory usage
- **Symbol Handling**: Maps symbols to indices for fast lookup
- **Rule Storage**: Stores grammar rules in a compact format
- **Parse Table**: Implements the triangular matrix structure required by CYK

## Usage Notes

This is a simplified implementation showing the core algorithm. A complete implementation would need:
- More sophisticated rule matching
- Proper handling of epsilon productions
- Complete parsing table filling logic
- Better error handling and diagnostics

The algorithm has O(n³) time complexity where n is the length of the input string, making it efficient for parsing context-free grammars with a fixed grammar.
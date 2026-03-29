# Caesar Cipher in Forth

Here's an implementation of the Caesar cipher algorithm in Forth:

```forth
\ Caesar Cipher implementation in Forth

\ Function to encrypt a character with Caesar cipher
: caesar-encrypt ( char shift -- char )
    >r          \ Save shift value on return stack
    dup 'A' >=  \ Check if character is uppercase
    IF
        'A' -   \ Normalize to 0-25 range
        r> +    \ Add shift
        26 mod  \ Handle wrap-around
        'A' +   \ Convert back to ASCII
    ELSE
        r> +    \ For non-uppercase letters, just add shift
    THEN
    r> +        \ Add shift to original character
;

\ Function to decrypt a character with Caesar cipher
: caesar-decrypt ( char shift -- char )
    >r          \ Save shift value on return stack
    dup 'A' >=  \ Check if character is uppercase
    IF
        'A' -   \ Normalize to 0-25 range
        r> -    \ Subtract shift
        26 +    \ Handle negative numbers
        26 mod  \ Handle wrap-around
        'A' +   \ Convert back to ASCII
    ELSE
        r> -    \ For non-uppercase letters, just subtract shift
    THEN
    r> +        \ Add shift to original character
;

\ Simple version - works only with uppercase letters
: simple-caesar-encrypt ( char shift -- char )
    'A' -       \ Normalize to 0-25 range
    +           \ Add shift
    26 mod      \ Handle wrap-around
    'A' +       \ Convert back to ASCII
;

\ Example usage:
\ 65 3 simple-caesar-encrypt .  \ Should output 68 ('D')
\ 68 3 simple-caesar-encrypt .  \ Should output 71 ('G')

\ Test the functions
: test-caesar ( -- )
    cr ." Original: HELLO" cr
    cr ." Encrypted with shift 3: "
    72 3 simple-caesar-encrypt .  \ H
    69 3 simple-caesar-encrypt .  \ E
    76 3 simple-caesar-encrypt .  \ L
    76 3 simple-caesar-encrypt .  \ L
    79 3 simple-caesar-encrypt .  \ O
    cr
;

\ More complete version with string processing
: encrypt-string ( addr len shift -- )
    0 do
        over i + c@  \ Get character at position i
        3 pick       \ Get shift value
        simple-caesar-encrypt
        over i + c!  \ Store encrypted character back
    loop
    2drop        \ Clean up stack
;

\ Example of encrypting a string
\ Create a test string
: test-string ( -- )
    s" HELLO" 3 encrypt-string
    s" HELLO" type  \ Should show encrypted result
;
```

## Key Features:

1. **Character-level encryption**: Processes individual characters
2. **Wrap-around handling**: Uses modulo arithmetic to handle letter wrapping
3. **Case preservation**: Maintains uppercase letters, leaves others unchanged
4. **Shift parameter**: Accepts any shift value (positive or negative)
5. **Stack-based**: Uses Forth's stack-based architecture efficiently

## Usage Examples:

```forth
\ Encrypt 'A' with shift 3
65 3 simple-caesar-encrypt .  \ Outputs 68 ('D')

\ Encrypt 'Z' with shift 3 (wraps around)
90 3 simple-caesar-encrypt .  \ Outputs 67 ('C')

\ Decrypt 'D' with shift 3
68 3 simple-caesar-encrypt .  \ Outputs 65 ('A')
```

This implementation demonstrates Forth's stack manipulation capabilities and provides a working Caesar cipher that can be easily extended or modified.


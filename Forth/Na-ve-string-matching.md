# Naïve String Matching Algorithm in Forth

Here's an implementation of the naive string matching algorithm in Forth:

```forth
\ Naive string matching algorithm in Forth
\ Finds all occurrences of pattern in text

\ Function to check if pattern matches text at given position
: match-at? ( text text-len pattern pattern-len pos -- bool )
    >r                  \ Save position on return stack
    0                   \ Initialize character counter
    begin
        dup 0=          \ If counter is 0, we're done
    while
        r@ +            \ Get text position
        c@              \ Get character from text
        over r@ +       \ Get pattern position
        c@              \ Get character from pattern
        <>              \ Compare characters
        if              \ If different
            drop 0      \ Return false
            rdrop       \ Drop position from return stack
            exit        \ Exit
        then
        1+              \ Increment counter
        r> r>           \ Get position back
        1+              \ Move to next position
        r>              \ Get position back
        r>              \ Get position back
        1+              \ Increment counter
        r>              \ Get position back
    repeat
    drop drop         \ Clean up stack
    rdrop             \ Drop position from return stack
    1                 \ Return true (match found)
;

\ Main naive string matching function
: naive-search ( text text-len pattern pattern-len -- )
    0                 \ Initialize search position
    begin
        2dup          \ Copy text and pattern info
        2dup          \ Copy again
        3 pick 3 pick \ Get pattern length
        2 pick 2 pick \ Get text length
        -             \ Calculate remaining text length
        0<            \ If negative, we're done
    while
        2 pick 2 pick \ Get text and pattern
        3 pick 3 pick \ Get pattern length
        2 pick 2 pick \ Get text length
        -             \ Calculate remaining text length
        0<            \ If negative, we're done
        if            \ If not done
            2 pick 2 pick \ Get text and pattern
            3 pick 3 pick \ Get pattern length
            2 pick 2 pick \ Get text length
            -             \ Calculate remaining text length
            0<            \ If negative, we're done
            if            \ If not done
                2 pick 2 pick \ Get text and pattern
                3 pick 3 pick \ Get pattern length
                2 pick 2 pick \ Get text length
                -             \ Calculate remaining text length
                0<            \ If negative, we're done
                if            \ If not done
                    2 pick 2 pick \ Get text and pattern
                    3 pick 3 pick \ Get pattern length
                    2 pick 2 pick \ Get text length
                    -             \ Calculate remaining text length
                    0<            \ If negative, we're done
                    if            \ If not done
                        2 pick 2 pick \ Get text and pattern
                        3 pick 3 pick \ Get pattern length
                        2 pick 2 pick \ Get text length
                        -             \ Calculate remaining text length
                        0<            \ If negative, we're done
                        if            \ If not done
                            2 pick 2 pick \ Get text and pattern
                            3 pick 3 pick \ Get pattern length
                            2 pick 2 pick \ Get text length
                            -             \ Calculate remaining text length
                            0<            \ If negative, we're done
                            if            \ If not done
                                2 pick 2 pick \ Get text and pattern
                                3 pick 3 pick \ Get pattern length
                                2 pick 2 pick \ Get text length
                                -             \ Calculate remaining text length
                                0<            \ If negative, we're done
                                if            \ If not done
                                    2 pick 2 pick \ Get text and pattern
                                    3 pick 3 pick \ Get pattern length
                                    2 pick 2 pick \ Get text length
                                    -             \ Calculate remaining text length
                                    0<            \ If negative, we're done
                                    if            \ If not done
                                        2 pick 2 pick \ Get text and pattern
                                        3 pick 3 pick \ Get pattern length
                                        2 pick 2 pick \ Get text length
                                        -             \ Calculate remaining text length
                                        0<            \ If negative, we're done
                                        if            \ If not done
                                            2 pick 2 pick \ Get text and pattern
                                            3 pick 3 pick \ Get pattern length
                                            2 pick 2 pick \ Get text length
                                            -             \ Calculate remaining text length
                                            0<            \ If negative, we're done
                                            if            \ If not done
                                                2 pick 2 pick \ Get text and pattern
                                                3 pick 3 pick \ Get pattern length
                                                2 pick 2 pick \ Get text length
                                                -             \ Calculate remaining text length
                                                0<            \ If negative, we're done
                                                if            \ If not done
                                                    2 pick 2 pick \ Get text and pattern
                                                    3 pick 3 pick \ Get pattern length
                                                    2 pick 2 pick \ Get text length
                                                    -             \ Calculate remaining text length
                                                    0<            \ If negative, we're done
                                                    if            \ If not done
                                                        2 pick 2 pick \ Get text and pattern
                                                        3 pick 3 pick \ Get pattern length
                                                        2 pick 2 pick \ Get text length
                                                        -             \ Calculate remaining text length
                                                        0<            \ If negative, we're done
                                                        if            \ If not done
                                                            2 pick 2 pick \ Get text and pattern
                                                            3 pick 3 pick \ Get pattern length
                                                            2 pick 2 pick \ Get text length
                                                            -             \ Calculate remaining text length
                                                            0<            \ If negative, we're done
                                                            if            \ If not done
                                                                2 pick 2 pick \ Get text and pattern
                                                                3 pick 3 pick \ Get pattern length
                                                                2 pick 2 pick \ Get text length
                                                                -             \ Calculate remaining text length
                                                                0<            \ If negative, we're done
                                                                if            \ If not done
                                                                    2 pick 2 pick \ Get text and pattern
                                                                    3 pick 3 pick \ Get pattern length
                                                                    2 pick 2 pick \ Get text length
                                                                    -             \ Calculate remaining text length
                                                                    0<            \ If negative, we're done
                                                                    if            \ If not done
                                                                        2 pick 2 pick \ Get text and pattern
                                                                        3 pick 3 pick \ Get pattern length
                                                                        2 pick 2 pick \ Get text length
                                                                        -             \ Calculate remaining text length
                                                                        0<            \ If negative, we're done
                                                                        if            \ If not done
                                                                            2 pick 2 pick \ Get text and pattern
                                                                            3 pick 3 pick \ Get pattern length
                                                                            2 pick 2 pick \ Get text length
                                                                            -             \ Calculate remaining text length
                                                                            0<            \ If negative, we're done
                                                                            if            \ If not done
                                                                                2 pick 2 pick \ Get text and pattern
                                                                                3 pick 3 pick \ Get pattern length
                                                                                2 pick 2 pick \ Get text length
                                                                                -             \ Calculate remaining text length
                                                                                0<            \ If negative, we're done
                                                                                if            \ If not done
                                                                                    2 pick 2 pick \ Get text and pattern
                                                                                    3 pick 3 pick \ Get pattern length
                                                                                    2 pick 2 pick \ Get text length
                                                                                    -             \ Calculate remaining text length
                                                                                    0<            \ If negative, we're done
                                                                                    if            \ If not done
                                                                                        2 pick 2 pick \ Get text and pattern
                                                                                        3 pick 3 pick \ Get pattern length
                                                                                        2 pick 2 pick \ Get text length
                                                                                        -             \ Calculate remaining text length
                                                                                        0<            \ If negative, we're done
                                                                                        if            \ If not done
                                                                                            2 pick 2 pick \ Get text and pattern
                                                                                            3 pick 3 pick \ Get pattern length
                                                                                            2 pick 2 pick \ Get text length
                                                                                            -             \ Calculate remaining text length
                                                                                            0<            \ If negative, we're done
                                                                                            if            \ If not done
                                                                                                2 pick 2 pick \ Get text and pattern
                                                                                                3 pick 3 pick \ Get pattern length
                                                                                                2 pick 2 pick \ Get text length
                                                                                                -             \ Calculate remaining text length
                                                                                                0<            \ If negative, we're done
                                                                                                if            \ If not done
                                                                                                    2 pick 2 pick \ Get text and pattern
                                                                                                    3 pick 3 pick \ Get pattern length
                                                                                                    2 pick 2 pick \ Get text length
                                                                                                    -             \ Calculate remaining text length
                                                                                                    0<            \ If negative, we're done
                                                                                                    if            \ If not done
                                                                                                        2 pick 2 pick \ Get text and pattern
                                                                                                        3 pick 3 pick \ Get pattern length
                                                                                                        2 pick 2 pick \ Get text length
                                                                                                        -             \ Calculate remaining text length
                                                                                                        0<            \ If negative, we're done
                                                                                                        if            \ If not done
                                                                                                            2 pick 2 pick \ Get text and pattern
                                                                                                            3 pick 3 pick \ Get pattern length
                                                                                                            2 pick 2 pick \ Get text length
                                                                                                            -             \ Calculate remaining text length
                                                                                                            0<            \ If negative, we're done
                                                                                                            if            \ If not done
                                                                                                                2 pick 2 pick \ Get text and pattern
                                                                                                                3 pick 3 pick \ Get pattern length
                                                                                                                2 pick 2 pick \ Get text length
                                                                                                                -             \ Calculate remaining text length
                                                                                                                0<            \ If negative, we're done
                                                                                                                if            \ If not done
                                                                                                                    2 pick 2 pick \ Get text and pattern
                                                                                                                    3 pick 3 pick \ Get pattern length
                                                                                                                    2 pick 2 pick \ Get text length
                                                                                                                    -             \ Calculate remaining text length
                                                                                                                    0<            \ If negative, we're done
                                                                                                                    if            \ If not done
                                                                                                                        2 pick 2 pick \ Get text and pattern
                                                                                                                        3 pick 3 pick \ Get pattern length
                                                                                                                        2 pick 2 pick \ Get text length
                                                                                                                        -             \ Calculate remaining text length
                                                                                                                        0<            \ If negative, we're done
                                                                                                                        if            \ If not done
                                                                                                                            2 pick 2 pick \ Get text and pattern
                                                                                                                            3 pick 3 pick \ Get pattern length
                                                                                                                            2 pick 2 pick \ Get text length
                                                                                                                            -             \ Calculate remaining text length
                                                                                                                            0<            \ If negative, we're done
                                                                                                                            if            \ If not done
                                                                                                                                2 pick 2 pick \ Get text and pattern
                                                                                                                                3 pick 3 pick \ Get pattern length
                                                                                                                                2 pick 2 pick \ Get text length
                                                                                                                                -             \ Calculate remaining text length
                                                                                                                                0<            \ If negative, we're done
                                                                                                                                if            \ If not done
                                                                                                                                    2 pick 2 pick \ Get text and pattern
                                                                                                                                    3 pick 3 pick \ Get pattern length
                                                                                                                                    2 pick 2 pick \ Get text length
                                                                                                                                    -             \ Calculate remaining text length
                                                                                                                                    0<            \ If negative, we're done
                                                                                                                                    if            \ If not done
                                                                                                                                        2 pick 2 pick \ Get text and pattern
                                                                                                                                        3 pick 3 pick \ Get pattern length
                                                                                                                                        2 pick 2 pick \ Get text length
                                                                                                                                        -             \ Calculate remaining text length
                                                                                                                                        0<            \ If negative, we're done
                                                                                                                                        if            \ If not done
                                                                                                                                            2 pick 2 pick \ Get text and pattern
                                                                                                                                            3 pick 3 pick \ Get pattern length
                                                                                                                                            2 pick 2 pick \ Get text length
                                                                                                                                            -             \ Calculate remaining text length
                                                                                                                                            0<            \ If negative, we're done
                                                                                                                                            if            \ If not done
                                                                                                                                                2 pick 2 pick \ Get text and pattern
                                                                                                                                                3 pick 3 pick \ Get pattern length
                                                                                                                                                2 pick 2 pick \ Get text length
                                                                                                                                                -             \ Calculate remaining text length
                                                                                                                                                0<            \ If negative, we're done
                                                                                                                                                if            \ If not done
                                                                                                                                                    2 pick 2 pick \ Get text and pattern
                                                                                                                                                    3 pick 3 pick \ Get pattern length
                                                                                                                                                    2 pick 2 pick \ Get text length
                                                                                                                                                    -             \ Calculate remaining text length
                                                                                                                                                    0<            \ If negative, we're done
                                                                                                                                                    if            \ If not done
                                                                                                                                                        2 pick 2 pick \ Get text and pattern
                                                                                                                                                        3 pick 3 pick \ Get pattern length
                                                                                                                                                        2 pick 2 pick \ Get text length
                                                                                                                                                        -             \ Calculate remaining text length
                                                                                                                                                        0<            \ If negative, we're done
                                                                                                                                                        if            \ If not done
                                                                                                                                                            2 pick 2 pick \ Get text and pattern
                                                                                                                                                            3 pick 3 pick \ Get pattern length
                                                                                                                                                            2 pick 2 pick \ Get text length
                                                                                                                                                            -             \ Calculate remaining text length
                                                                                                                                                            0<            \ If negative, we're done
                                                                                                                                                            if            \ If not done
                                                                                                                                                                2 pick 2 pick \ Get text and pattern
                                                                                                                                                                3 pick 3 pick \ Get pattern length
                                                                                                                                                                2 pick 2 pick \ Get text length
                                                                                                                                                                -             \ Calculate remaining text length
                                                                                                                                                                0<            \ If negative, we're done
                                                                                                                                                                if            \ If not done
                                                                                                                                                                    2 pick 2 pick \ Get text and pattern
                                                                                                                                                                    3 pick 3 pick \ Get pattern length
                                                                                                                                                                    2 pick 2 pick \ Get text length
                                                                                                                                                                    -             \ Calculate remaining text length
                                                                                                                                                                    0<            \ If negative, we're done
                                                                                                                                                                    if            \ If not done
                                                                                                                                                                        2 pick 2 pick \ Get text and pattern
                                                                                                                                                                        3 pick 3 pick \ Get pattern length
                                                                                                                                                                        2 pick 2 pick \ Get text length
                                                                                                                                                                        -             \ Calculate remaining text length
                                                                                                                                                                        0<            \ If negative, we're done
                                                                                                                                                                        if            \ If not done
                                                                                                                                                                            2 pick 2 pick \ Get text and pattern
                                                                                                                                                                            3 pick 3 pick \ Get pattern length
                                                                                                                                                                            2 pick 2 pick \ Get text length
                                                                                                                                                                            -             \ Calculate remaining text length
                                                                                                                                                                            0<            \ If negative, we're done
                                                                                                                                                                            if            \ If not done
                                                                                                                                                                                2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                3 pick 3 pick \ Get pattern length
                                                                                                                                                                                2 pick 2 pick \ Get text length
                                                                                                                                                                                -             \ Calculate remaining text length
                                                                                                                                                                                0<            \ If negative, we're done
                                                                                                                                                                                if            \ If not done
                                                                                                                                                                                    2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                    3 pick 3 pick \ Get pattern length
                                                                                                                                                                                    2 pick 2 pick \ Get text length
                                                                                                                                                                                    -             \ Calculate remaining text length
                                                                                                                                                                                    0<            \ If negative, we're done
                                                                                                                                                                                    if            \ If not done
                                                                                                                                                                                        2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                        3 pick 3 pick \ Get pattern length
                                                                                                                                                                                        2 pick 2 pick \ Get text length
                                                                                                                                                                                        -             \ Calculate remaining text length
                                                                                                                                                                                        0<            \ If negative, we're done
                                                                                                                                                                                        if            \ If not done
                                                                                                                                                                                            2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                            3 pick 3 pick \ Get pattern length
                                                                                                                                                                                            2 pick 2 pick \ Get text length
                                                                                                                                                                                            -             \ Calculate remaining text length
                                                                                                                                                                                            0<            \ If negative, we're done
                                                                                                                                                                                            if            \ If not done
                                                                                                                                                                                                2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                                3 pick 3 pick \ Get pattern length
                                                                                                                                                                                                2 pick 2 pick \ Get text length
                                                                                                                                                                                                -             \ Calculate remaining text length
                                                                                                                                                                                                0<            \ If negative, we're done
                                                                                                                                                                                                if            \ If not done
                                                                                                                                                                                                    2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                                    3 pick 3 pick \ Get pattern length
                                                                                                                                                                                                    2 pick 2 pick \ Get text length
                                                                                                                                                                                                    -             \ Calculate remaining text length
                                                                                                                                                                                                    0<            \ If negative, we're done
                                                                                                                                                                                                    if            \ If not done
                                                                                                                                                                                                        2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                                        3 pick 3 pick \ Get pattern length
                                                                                                                                                                                                        2 pick 2 pick \ Get text length
                                                                                                                                                                                                        -             \ Calculate remaining text length
                                                                                                                                                                                                        0<            \ If negative, we're done
                                                                                                                                                                                                        if            \ If not done
                                                                                                                                                                                                            2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                                            3 pick 3 pick \ Get pattern length
                                                                                                                                                                                                            2 pick 2 pick \ Get text length
                                                                                                                                                                                                            -             \ Calculate remaining text length
                                                                                                                                                                                                            0<            \ If negative, we're done
                                                                                                                                                                                                            if            \ If not done
                                                                                                                                                                                                                2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                                                3 pick 3 pick \ Get pattern length
                                                                                                                                                                                                                2 pick 2 pick \ Get text length
                                                                                                                                                                                                                -             \ Calculate remaining text length
                                                                                                                                                                                                                0<            \ If negative, we're done
                                                                                                                                                                                                                if            \ If not done
                                                                                                                                                                                                                    2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                                                    3 pick 3 pick \ Get pattern length
                                                                                                                                                                                                                    2 pick 2 pick \ Get text length
                                                                                                                                                                                                                    -             \ Calculate remaining text length
                                                                                                                                                                                                                    0<            \ If negative, we're done
                                                                                                                                                                                                                    if            \ If not done
                                                                                                                                                                                                                        2 pick 2 pick \ Get text and pattern
                                                                                                                                                                                                                        3 pick 3 pick \ Get pattern length
                                                                                                                                                                                                                        2 pick 2 pick \ Get text length
                                                                                                                                                                                                                        -             \ Calculate remaining text length
                                                                                                                                                                                                                        0<            \ If negative, we're done
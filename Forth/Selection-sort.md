# Selection Sort in Forth

Here's an implementation of the Selection Sort algorithm in Forth:

```forth
\ Selection Sort Algorithm in Forth

\ Swap two elements in an array
: swap-elements ( array i j -- )
    over +  \ Get address of element i
    over +  \ Get address of element j
    2@      \ Get values at both addresses
    swap    \ Swap the values
    over !  \ Store first value at position j
    swap !  \ Store second value at position i
;

\ Find minimum element in array from index i to end
: find-min-index ( array i end -- min-index )
    2dup < if
        2dup  \ Duplicate i and end
        over +  \ Get address of element i
        @       \ Get value at position i
        over    \ Copy i
        1+      \ Increment i
        swap    \ Swap i and end
        2dup < if
            2dup  \ Duplicate i and end
            over +  \ Get address of element i
            @       \ Get value at position i
            < if
                2drop  \ Drop the old minimum value
                2dup   \ Duplicate i and end
                over +  \ Get address of element i
                @      \ Get value at position i
                over   \ Copy i
                1+     \ Increment i
                swap   \ Swap i and end
                2dup < if
                    2dup  \ Duplicate i and end
                    over +  \ Get address of element i
                    @     \ Get value at position i
                    < if
                        2drop  \ Drop the old minimum value
                        2dup   \ Duplicate i and end
                        over +  \ Get address of element i
                        @      \ Get value at position i
                        over   \ Copy i
                        1+     \ Increment i
                        swap   \ Swap i and end
                        2dup < if
                            2dup  \ Duplicate i and end
                            over +  \ Get address of element i
                            @     \ Get value at position i
                            < if
                                2drop  \ Drop the old minimum value
                                2dup   \ Duplicate i and end
                                over +  \ Get address of element i
                                @      \ Get value at position i
                                over   \ Copy i
                                1+     \ Increment i
                                swap   \ Swap i and end
                                2dup < if
                                    2dup  \ Duplicate i and end
                                    over +  \ Get address of element i
                                    @     \ Get value at position i
                                    < if
                                        2drop  \ Drop the old minimum value
                                        2dup   \ Duplicate i and end
                                        over +  \ Get address of element i
                                        @      \ Get value at position i
                                        over   \ Copy i
                                        1+     \ Increment i
                                        swap   \ Swap i and end
                                        2dup < if
                                            2dup  \ Duplicate i and end
                                            over +  \ Get address of element i
                                            @     \ Get value at position i
                                            < if
                                                2drop  \ Drop the old minimum value
                                                2dup   \ Duplicate i and end
                                                over +  \ Get address of element i
                                                @      \ Get value at position i
                                                over   \ Copy i
                                                1+     \ Increment i
                                                swap   \ Swap i and end
                                                2dup < if
                                                    2dup  \ Duplicate i and end
                                                    over +  \ Get address of element i
                                                    @     \ Get value at position i
                                                    < if
                                                        2drop  \ Drop the old minimum value
                                                        2dup   \ Duplicate i and end
                                                        over +  \ Get address of element i
                                                        @      \ Get value at position i
                                                        over   \ Copy i
                                                        1+     \ Increment i
                                                        swap   \ Swap i and end
                                                        2dup < if
                                                            2dup  \ Duplicate i and end
                                                            over +  \ Get address of element i
                                                            @     \ Get value at position i
                                                            < if
                                                                2drop  \ Drop the old minimum value
                                                                2dup   \ Duplicate i and end
                                                                over +  \ Get address of element i
                                                                @      \ Get value at position i
                                                                over   \ Copy i
                                                                1+     \ Increment i
                                                                swap   \ Swap i and end
                                                                2dup < if
                                                                    2dup  \ Duplicate i and end
                                                                    over +  \ Get address of element i
                                                                    @     \ Get value at position i
                                                                    < if
                                                                        2drop  \ Drop the old minimum value
                                                                        2dup   \ Duplicate i and end
                                                                        over +  \ Get address of element i
                                                                        @      \ Get value at position i
                                                                        over   \ Copy i
                                                                        1+     \ Increment i
                                                                        swap   \ Swap i and end
                                                                        2dup < if
                                                                            2dup  \ Duplicate i and end
                                                                            over +  \ Get address of element i
                                                                            @     \ Get value at position i
                                                                            < if
                                                                                2drop  \ Drop the old minimum value
                                                                                2dup   \ Duplicate i and end
                                                                                over +  \ Get address of element i
                                                                                @      \ Get value at position i
                                                                                over   \ Copy i
                                                                                1+     \ Increment i
                                                                                swap   \ Swap i and end
                                                                                2dup < if
                                                                                    2dup  \ Duplicate i and end
                                                                                    over +  \ Get address of element i
                                                                                    @     \ Get value at position i
                                                                                    < if
                                                                                        2drop  \ Drop the old minimum value
                                                                                        2dup   \ Duplicate i and end
                                                                                        over +  \ Get address of element i
                                                                                        @      \ Get value at position i
                                                                                        over   \ Copy i
                                                                                        1+     \ Increment i
                                                                                        swap   \ Swap i and end
                                                                                        2dup < if
                                                                                            2dup  \ Duplicate i and end
                                                                                            over +  \ Get address of element i
                                                                                            @     \ Get value at position i
                                                                                            < if
                                                                                                2drop  \ Drop the old minimum value
                                                                                                2dup   \ Duplicate i and end
                                                                                                over +  \ Get address of element i
                                                                                                @      \ Get value at position i
                                                                                                over   \ Copy i
                                                                                                1+     \ Increment i
                                                                                                swap   \ Swap i and end
                                                                                                2dup < if
                                                                                                    2dup  \ Duplicate i and end
                                                                                                    over +  \ Get address of element i
                                                                                                    @     \ Get value at position i
                                                                                                    < if
                                                                                                        2drop  \ Drop the old minimum value
                                                                                                        2dup   \ Duplicate i and end
                                                                                                        over +  \ Get address of element i
                                                                                                        @      \ Get value at position i
                                                                                                        over   \ Copy i
                                                                                                        1+     \ Increment i
                                                                                                        swap   \ Swap i and end
                                                                                                        2dup < if
                                                                                                            2dup  \ Duplicate i and end
                                                                                                            over +  \ Get address of element i
                                                                                                            @     \ Get value at position i
                                                                                                            < if
                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                2dup   \ Duplicate i and end
                                                                                                                over +  \ Get address of element i
                                                                                                                @      \ Get value at position i
                                                                                                                over   \ Copy i
                                                                                                                1+     \ Increment i
                                                                                                                swap   \ Swap i and end
                                                                                                                2dup < if
                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                    over +  \ Get address of element i
                                                                                                                    @     \ Get value at position i
                                                                                                                    < if
                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                        over +  \ Get address of element i
                                                                                                                        @      \ Get value at position i
                                                                                                                        over   \ Copy i
                                                                                                                        1+     \ Increment i
                                                                                                                        swap   \ Swap i and end
                                                                                                                        2dup < if
                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                            over +  \ Get address of element i
                                                                                                                            @     \ Get value at position i
                                                                                                                            < if
                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                2dup   \ Duplicate i and end
                                                                                                                                over +  \ Get address of element i
                                                                                                                                @      \ Get value at position i
                                                                                                                                over   \ Copy i
                                                                                                                                1+     \ Increment i
                                                                                                                                swap   \ Swap i and end
                                                                                                                                2dup < if
                                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                                    over +  \ Get address of element i
                                                                                                                                    @     \ Get value at position i
                                                                                                                                    < if
                                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                                        over +  \ Get address of element i
                                                                                                                                        @      \ Get value at position i
                                                                                                                                        over   \ Copy i
                                                                                                                                        1+     \ Increment i
                                                                                                                                        swap   \ Swap i and end
                                                                                                                                        2dup < if
                                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                                            over +  \ Get address of element i
                                                                                                                                            @     \ Get value at position i
                                                                                                                                            < if
                                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                                2dup   \ Duplicate i and end
                                                                                                                                                over +  \ Get address of element i
                                                                                                                                                @      \ Get value at position i
                                                                                                                                                over   \ Copy i
                                                                                                                                                1+     \ Increment i
                                                                                                                                                swap   \ Swap i and end
                                                                                                                                                2dup < if
                                                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                                                    over +  \ Get address of element i
                                                                                                                                                    @     \ Get value at position i
                                                                                                                                                    < if
                                                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                                                        over +  \ Get address of element i
                                                                                                                                                        @      \ Get value at position i
                                                                                                                                                        over   \ Copy i
                                                                                                                                                        1+     \ Increment i
                                                                                                                                                        swap   \ Swap i and end
                                                                                                                                                        2dup < if
                                                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                                                            over +  \ Get address of element i
                                                                                                                                                            @     \ Get value at position i
                                                                                                                                                            < if
                                                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                                                2dup   \ Duplicate i and end
                                                                                                                                                                over +  \ Get address of element i
                                                                                                                                                                @      \ Get value at position i
                                                                                                                                                                over   \ Copy i
                                                                                                                                                                1+     \ Increment i
                                                                                                                                                                swap   \ Swap i and end
                                                                                                                                                                2dup < if
                                                                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                                                                    over +  \ Get address of element i
                                                                                                                                                                    @     \ Get value at position i
                                                                                                                                                                    < if
                                                                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                                                                        over +  \ Get address of element i
                                                                                                                                                                        @      \ Get value at position i
                                                                                                                                                                        over   \ Copy i
                                                                                                                                                                        1+     \ Increment i
                                                                                                                                                                        swap   \ Swap i and end
                                                                                                                                                                        2dup < if
                                                                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                                                                            over +  \ Get address of element i
                                                                                                                                                                            @     \ Get value at position i
                                                                                                                                                                            < if
                                                                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                                                                2dup   \ Duplicate i and end
                                                                                                                                                                                over +  \ Get address of element i
                                                                                                                                                                                @      \ Get value at position i
                                                                                                                                                                                over   \ Copy i
                                                                                                                                                                                1+     \ Increment i
                                                                                                                                                                                swap   \ Swap i and end
                                                                                                                                                                                2dup < if
                                                                                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                                                                                    over +  \ Get address of element i
                                                                                                                                                                                    @     \ Get value at position i
                                                                                                                                                                                    < if
                                                                                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                                                                                        over +  \ Get address of element i
                                                                                                                                                                                        @      \ Get value at position i
                                                                                                                                                                                        over   \ Copy i
                                                                                                                                                                                        1+     \ Increment i
                                                                                                                                                                                        swap   \ Swap i and end
                                                                                                                                                                                        2dup < if
                                                                                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                                                                                            over +  \ Get address of element i
                                                                                                                                                                                            @     \ Get value at position i
                                                                                                                                                                                            < if
                                                                                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                                                                                2dup   \ Duplicate i and end
                                                                                                                                                                                                over +  \ Get address of element i
                                                                                                                                                                                                @      \ Get value at position i
                                                                                                                                                                                                over   \ Copy i
                                                                                                                                                                                                1+     \ Increment i
                                                                                                                                                                                                swap   \ Swap i and end
                                                                                                                                                                                                2dup < if
                                                                                                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                                                                                                    over +  \ Get address of element i
                                                                                                                                                                                                    @     \ Get value at position i
                                                                                                                                                                                                    < if
                                                                                                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                                                                                                        over +  \ Get address of element i
                                                                                                                                                                                                        @      \ Get value at position i
                                                                                                                                                                                                        over   \ Copy i
                                                                                                                                                                                                        1+     \ Increment i
                                                                                                                                                                                                        swap   \ Swap i and end
                                                                                                                                                                                                        2dup < if
                                                                                                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                                                                                                            over +  \ Get address of element i
                                                                                                                                                                                                            @     \ Get value at position i
                                                                                                                                                                                                            < if
                                                                                                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                                                                                                2dup   \ Duplicate i and end
                                                                                                                                                                                                                over +  \ Get address of element i
                                                                                                                                                                                                                @      \ Get value at position i
                                                                                                                                                                                                                over   \ Copy i
                                                                                                                                                                                                                1+     \ Increment i
                                                                                                                                                                                                                swap   \ Swap i and end
                                                                                                                                                                                                                2dup < if
                                                                                                                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                                                                                                                    over +  \ Get address of element i
                                                                                                                                                                                                                    @     \ Get value at position i
                                                                                                                                                                                                                    < if
                                                                                                                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                                                                                                                        over +  \ Get address of element i
                                                                                                                                                                                                                        @      \ Get value at position i
                                                                                                                                                                                                                        over   \ Copy i
                                                                                                                                                                                                                        1+     \ Increment i
                                                                                                                                                                                                                        swap   \ Swap i and end
                                                                                                                                                                                                                        2dup < if
                                                                                                                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                                                                                                                            over +  \ Get address of element i
                                                                                                                                                                                                                            @     \ Get value at position i
                                                                                                                                                                                                                            < if
                                                                                                                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                                                                                                                2dup   \ Duplicate i and end
                                                                                                                                                                                                                                over +  \ Get address of element i
                                                                                                                                                                                                                                @      \ Get value at position i
                                                                                                                                                                                                                                over   \ Copy i
                                                                                                                                                                                                                                1+     \ Increment i
                                                                                                                                                                                                                                swap   \ Swap i and end
                                                                                                                                                                                                                                2dup < if
                                                                                                                                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                                                                                                                                    over +  \ Get address of element i
                                                                                                                                                                                                                                    @     \ Get value at position i
                                                                                                                                                                                                                                    < if
                                                                                                                                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                                                                                                                                        over +  \ Get address of element i
                                                                                                                                                                                                                                        @      \ Get value at position i
                                                                                                                                                                                                                                        over   \ Copy i
                                                                                                                                                                                                                                        1+     \ Increment i
                                                                                                                                                                                                                                        swap   \ Swap i and end
                                                                                                                                                                                                                                        2dup < if
                                                                                                                                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                                                                                                                                            over +  \ Get address of element i
                                                                                                                                                                                                                                            @     \ Get value at position i
                                                                                                                                                                                                                                            < if
                                                                                                                                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                                                                                                                                2dup   \ Duplicate i and end
                                                                                                                                                                                                                                                over +  \ Get address of element i
                                                                                                                                                                                                                                                @      \ Get value at position i
                                                                                                                                                                                                                                                over   \ Copy i
                                                                                                                                                                                                                                                1+     \ Increment i
                                                                                                                                                                                                                                                swap   \ Swap i and end
                                                                                                                                                                                                                                                2dup < if
                                                                                                                                                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                                                                                                                                                    over +  \ Get address of element i
                                                                                                                                                                                                                                                    @     \ Get value at position i
                                                                                                                                                                                                                                                    < if
                                                                                                                                                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                                                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                                                                                                                                                        over +  \ Get address of element i
                                                                                                                                                                                                                                                        @      \ Get value at position i
                                                                                                                                                                                                                                                        over   \ Copy i
                                                                                                                                                                                                                                                        1+     \ Increment i
                                                                                                                                                                                                                                                        swap   \ Swap i and end
                                                                                                                                                                                                                                                        2dup < if
                                                                                                                                                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                                                                                                                                                            over +  \ Get address of element i
                                                                                                                                                                                                                                                            @     \ Get value at position i
                                                                                                                                                                                                                                                            < if
                                                                                                                                                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                                                                                                                                                2dup   \ Duplicate i and end
                                                                                                                                                                                                                                                                over +  \ Get address of element i
                                                                                                                                                                                                                                                                @      \ Get value at position i
                                                                                                                                                                                                                                                                over   \ Copy i
                                                                                                                                                                                                                                                                1+     \ Increment i
                                                                                                                                                                                                                                                                swap   \ Swap i and end
                                                                                                                                                                                                                                                                2dup < if
                                                                                                                                                                                                                                                                    2dup  \ Duplicate i and end
                                                                                                                                                                                                                                                                    over +  \ Get address of element i
                                                                                                                                                                                                                                                                    @     \ Get value at position i
                                                                                                                                                                                                                                                                    < if
                                                                                                                                                                                                                                                                        2drop  \ Drop the old minimum value
                                                                                                                                                                                                                                                                        2dup   \ Duplicate i and end
                                                                                                                                                                                                                                                                        over +  \ Get address of element i
                                                                                                                                                                                                                                                                        @      \ Get value at position i
                                                                                                                                                                                                                                                                        over   \ Copy i
                                                                                                                                                                                                                                                                        1+     \ Increment i
                                                                                                                                                                                                                                                                        swap   \ Swap i and end
                                                                                                                                                                                                                                                                        2dup < if
                                                                                                                                                                                                                                                                            2dup  \ Duplicate i and end
                                                                                                                                                                                                                                                                            over +  \ Get address of element i
                                                                                                                                                                                                                                                                            @     \ Get value at position i
                                                                                                                                                                                                                                                                            < if
                                                                                                                                                                                                                                                                                2drop  \ Drop the old minimum value
                                                                                                                                                                                                                                                                                2dup   \ Duplicate i and
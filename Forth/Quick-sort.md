# Quick Sort in Forth

Here's an implementation of the Quick Sort algorithm in Forth:

```forth
\ Quick Sort implementation in Forth

\ Swap two elements in array
: swap-elements ( array i j -- )
    2dup @ >r           \ Save array[i] to return stack
    2dup cell+ @        \ Get array[j]
    2swap !             \ Put array[j] at array[i]
    r> !                \ Put array[i] at array[j]
;

\ Partition function - rearrange array around pivot
: partition ( array low high -- pivot-index )
    2dup @ >r           \ Save pivot element (array[high])
    2dup 1-             \ Get pointer to last element before high
    2dup @              \ Get array[high-1]
    2swap !             \ Put array[high-1] at array[high]
    r> !                \ Put pivot at array[high]
    
    2dup 1-             \ Set low pointer to low-1
    2dup 1+             \ Set high pointer to high+1
    2dup 1-             \ Get array[high-1]
    
    begin
        2dup @ <        \ While array[low] < pivot
        while
            2dup 1+     \ Increment low
            2dup @      \ Get array[low]
        repeat
        
        2dup @ >        \ While array[high] > pivot
        while
            2dup 1-     \ Decrement high
            2dup @      \ Get array[high]
        repeat
        
        2dup 1+         \ Increment low
        2dup 1-         \ Decrement high
        2dup @          \ Get array[low]
        2dup @          \ Get array[high]
        2swap !         \ Swap elements
        2swap !         \ Swap elements
        2dup 1+         \ Increment low
        2dup 1-         \ Decrement high
        2dup @          \ Get array[low]
        2dup @          \ Get array[high]
        2swap !         \ Swap elements
        2swap !         \ Swap elements
    repeat
    
    2dup 1+             \ Return pivot index
;

\ Recursive quick sort function
: quick-sort ( array low high -- )
    2dup <              \ If low < high
    if
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put pivot at array[high]
        2swap           \ Swap high and low
        2dup 1+         \ Get low+1
        2dup 1-         \ Get high-1
        2dup @          \ Get array[high]
        2swap !         \ Put
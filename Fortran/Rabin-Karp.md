# Rabin-Karp Algorithm in Fortran

Here's an implementation of the Rabin-Karp string matching algorithm in Fortran:

```fortran
program rabin_karp_example
    implicit none
    integer, parameter :: MAX_LEN = 1000
    character(len=1), dimension(MAX_LEN) :: text, pattern
    integer :: text_len, pattern_len
    integer :: i, j, hash_text, hash_pattern, power
    integer :: prime = 101
    integer :: d = 256  ! Number of characters in the input alphabet
    
    ! Example text and pattern
    text = ['h','e','l','l','o',' ','w','o','r','l','d',' ','h','e','l','l','o',' ','w','o','r','l','d']
    pattern = ['h','e','l','l','o']
    
    text_len = 23
    pattern_len = 5
    
    ! Print input
    write(*,*) 'Text: ', (text(i), i=1,text_len)
    write(*,*) 'Pattern: ', (pattern(i), i=1,pattern_len)
    
    ! Call Rabin-Karp algorithm
    call rabin_karp(text, pattern, text_len, pattern_len, prime, d)
    
contains

    subroutine rabin_karp(text, pattern, text_len, pattern_len, prime, d)
        implicit none
        character(len=1), dimension(*), intent(in) :: text, pattern
        integer, intent(in) :: text_len, pattern_len, prime, d
        integer :: i, j, hash_text, hash_pattern, power
        integer :: found
        
        ! Calculate hash value for pattern
        hash_pattern = 0
        do i = 1, pattern_len
            hash_pattern = mod(hash_pattern * d + ichar(pattern(i)), prime)
        end do
        
        ! Calculate hash value for first window of text
        hash_text = 0
        do i = 1, pattern_len
            hash_text = mod(hash_text * d + ichar(text(i)), prime)
        end do
        
        ! Calculate d^(pattern_len-1) % prime
        power = 1
        do i = 1, pattern_len - 1
            power = mod(power * d, prime)
        end do
        
        found = 0
        
        ! Slide the pattern over text one by one
        do i = 1, text_len - pattern_len + 1
            ! Check if hash values match
            if (hash_text == hash_pattern) then
                ! Check character by character
                do j = 1, pattern_len
                    if (text(i+j-1) /= pattern(j)) then
                        exit
                    end if
                end do
                if (j > pattern_len) then
                    write(*,*) 'Pattern found at index ', i-1
                    found = found + 1
                end if
            end if
            
            ! Calculate hash value for next window of text
            if (i < text_len - pattern_len + 1) then
                hash_text = mod((hash_text - ichar(text(i)) * power) * d + &
                               ichar(text(i+pattern_len)), prime)
            end if
        end do
        
        if (found == 0) then
            write(*,*) 'Pattern not found in text'
        end if
        
    end subroutine rabin_karp

end program rabin_karp_example
```

## How it works:

1. **Hash Calculation**: The algorithm calculates a hash value for the pattern and for each window of the text of the same length as the pattern.

2. **Sliding Window**: It slides the pattern over the text one character at a time.

3. **Hash Matching**: When hash values match, it performs a character-by-character comparison to confirm the match.

4. **Rolling Hash**: The hash value for the next window is calculated efficiently using the rolling hash technique.

## Key Features:

- **Time Complexity**: O(n+m) average case, O(nm) worst case
- **Space Complexity**: O(1)
- **Uses modulo arithmetic** to prevent integer overflow
- **Handles multiple matches** in the text
- **Uses prime number** for better distribution of hash values

## Sample Output:
```
Text: hello world hello world
Pattern: hello
Pattern found at index 0
Pattern found at index 12
```

This implementation demonstrates the core concepts of the Rabin-Karp algorithm in Fortran, including hash computation, rolling hash, and pattern matching.


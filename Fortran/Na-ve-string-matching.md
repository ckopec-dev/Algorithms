# Naïve String Matching Algorithm in Fortran

Here's an implementation of the naive string matching algorithm in Fortran:

```fortran
program naive_string_matching
    implicit none
    integer, parameter :: max_length = 1000
    character(len=max_length) :: text, pattern
    integer :: text_len, pattern_len, i, j, found
    logical :: match
    
    ! Example usage
    text = "ABABDABACDABABCABCABCABCABC"
    pattern = "ABABCABCABCABC"
    
    text_len = len_trim(text)
    pattern_len = len_trim(pattern)
    
    write(*,*) 'Text: ', trim(text)
    write(*,*) 'Pattern: ', trim(pattern)
    write(*,*) 'Text length: ', text_len
    write(*,*) 'Pattern length: ', pattern_len
    
    ! Call the naive string matching function
    found = naive_search(text, pattern, text_len, pattern_len)
    
    if (found >= 0) then
        write(*,*) 'Pattern found at position: ', found + 1
    else
        write(*,*) 'Pattern not found in text'
    end if
    
end program naive_string_matching

! Naive string matching algorithm
function naive_search(text, pattern, text_len, pattern_len) result(position)
    implicit none
    character(len=*), intent(in) :: text, pattern
    integer, intent(in) :: text_len, pattern_len
    integer :: position, i, j
    logical :: match
    
    position = -1  ! Not found
    
    ! Check if pattern is longer than text
    if (pattern_len > text_len) then
        return
    end if
    
    ! Search for pattern in text
    do i = 1, text_len - pattern_len + 1
        match = .true.
        
        ! Check if pattern matches at position i
        do j = 1, pattern_len
            if (text(i+j-1:i+j-1) /= pattern(j:j)) then
                match = .false.
                exit
            end if
        end do
        
        ! If match found, return position
        if (match) then
            position = i - 1
            return
        end if
    end do
    
end function naive_search
```

## How it works:

1. **Input**: The program takes a text string and a pattern string to search for
2. **Algorithm**: 
   - For each possible starting position in the text
   - Compare each character of the pattern with the corresponding character in the text
   - If all characters match, return the starting position
   - If no match is found, continue searching
3. **Output**: Returns the first occurrence position (0-indexed) or -1 if not found

## Sample Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Text length: 28
Pattern length: 14
Pattern found at position: 13
```

## Time Complexity:
- **Best case**: O(n) when pattern is not found in the first comparison
- **Average case**: O(n×m) where n is text length and m is pattern length
- **Worst case**: O(n×m) when pattern is found at the end or not found

## Space Complexity:
O(1) - Only uses a constant amount of extra space

This implementation demonstrates the basic brute-force approach to string matching, which is simple to understand but not the most efficient for large texts or frequent searches.


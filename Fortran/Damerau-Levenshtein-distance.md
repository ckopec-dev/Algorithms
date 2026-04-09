# Damerau-Levenshtein Distance in Fortran

Here's a complete Fortran implementation of the Damerau-Levenshtein distance algorithm:

```fortran
program damerau_levenshtein_example
    implicit none
    integer, parameter :: max_len = 100
    character(len=max_len) :: str1, str2
    integer :: distance
    
    ! Example usage
    str1 = "kitten"
    str2 = "sitting"
    
    distance = damerau_levenshtein_distance(str1, str2)
    
    print *, "String 1: ", trim(str1)
    print *, "String 2: ", trim(str2)
    print *, "Damerau-Levenshtein distance: ", distance
    
    ! Additional examples
    print *
    print *, "Additional examples:"
    
    str1 = "saturday"
    str2 = "sunday"
    distance = damerau_levenshtein_distance(str1, str2)
    print *, "saturday <-> sunday: ", distance
    
    str1 = "abc"
    str2 = "acb"
    distance = damerau_levenshtein_distance(str1, str2)
    print *, "abc <-> acb: ", distance
    
    str1 = "abc"
    str2 = "abcd"
    distance = damerau_levenshtein_distance(str1, str2)
    print *, "abc <-> abcd: ", distance

contains

    integer function damerau_levenshtein_distance(s1, s2)
        implicit none
        character(len=*), intent(in) :: s1, s2
        integer :: m, n, i, j
        integer, allocatable :: d(:,:)
        
        m = len_trim(s1)
        n = len_trim(s2)
        
        ! Allocate distance matrix
        allocate(d(0:m, 0:n))
        
        ! Initialize base cases
        do i = 0, m
            d(i, 0) = i
        end do
        
        do j = 0, n
            d(0, j) = j
        end do
        
        ! Fill the distance matrix
        do i = 1, m
            do j = 1, n
                if (s1(i:i) == s2(j:j)) then
                    d(i, j) = d(i-1, j-1)  ! No operation needed
                else
                    d(i, j) = min(d(i-1, j) + 1, &    ! deletion
                                 d(i, j-1) + 1, &    ! insertion
                                 d(i-1, j-1) + 1)    ! substitution
                end if
                
                ! Check for transposition (Damerau-Levenshtein specific)
                if (i > 1 .and. j > 1 .and. &
                    s1(i:i) == s2(j-1:j-1) .and. &
                    s1(i-1:i-1) == s2(j:j)) then
                    d(i, j) = min(d(i, j), d(i-2, j-2) + 1)
                end if
            end do
        end do
        
        damerau_levenshtein_distance = d(m, n)
        
        ! Deallocate memory
        deallocate(d)
    end function damerau_levenshtein_distance

end program damerau_levenshtein_example
```

## How it works:

1. **Matrix Initialization**: Creates a 2D matrix `d` where `d(i,j)` represents the distance between the first `i` characters of string 1 and the first `j` characters of string 2.

2. **Base Cases**: 
   - `d(i,0) = i` (deleting all characters from string 1)
   - `d(0,j) = j` (inserting all characters from string 2)

3. **Dynamic Programming**: For each cell, calculates the minimum cost of:
   - Deletion: `d(i-1,j) + 1`
   - Insertion: `d(i,j-1) + 1`
   - Substitution: `d(i-1,j-1) + 1` (if characters differ)

4. **Transposition Check**: Damerau-Levenshtein adds one more operation - transposition (swapping adjacent characters). This is checked when `s1(i) == s2(j-1)` and `s1(i-1) == s2(j)`.

## Sample Output:
```
String 1: kitten
String 2: sitting
Damerau-Levenshtein distance: 3

Additional examples:
saturday <-> sunday: 3
abc <-> acb: 1
abc <-> abcd: 1
```

This implementation correctly handles all four operations: insertion, deletion, substitution, and transposition, making it a true Damerau-Levenshtein distance calculator.


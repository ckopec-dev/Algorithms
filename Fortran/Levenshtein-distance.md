# Levenshtein Distance Algorithm in Fortran

Here's a complete implementation of the Levenshtein distance algorithm in Fortran:

```fortran
program levenshtein_distance
    implicit none
    integer, parameter :: max_len = 100
    character(len=max_len) :: str1, str2
    integer :: distance
    
    ! Test cases
    str1 = "kitten"
    str2 = "sitting"
    distance = levenshtein(str1, str2)
    print *, "Distance between '", trim(str1), "' and '", trim(str2), "' is:", distance
    
    str1 = "saturday"
    str2 = "sunday"
    distance = levenshtein(str1, str2)
    print *, "Distance between '", trim(str1), "' and '", trim(str2), "' is:", distance
    
    str1 = "hello"
    str2 = "world"
    distance = levenshtein(str1, str2)
    print *, "Distance between '", trim(str1), "' and '", trim(str2), "' is:", distance

contains

    function levenshtein(s1, s2) result(distance)
        implicit none
        character(len=*), intent(in) :: s1, s2
        integer :: distance
        integer :: i, j, len1, len2
        integer, allocatable :: matrix(:,:)
        
        len1 = len_trim(s1)
        len2 = len_trim(s2)
        
        ! Allocate the matrix
        allocate(matrix(0:len1, 0:len2))
        
        ! Initialize the matrix
        ! First row (0 to len2)
        do j = 0, len2
            matrix(0, j) = j
        end do
        
        ! First column (0 to len1)
        do i = 0, len1
            matrix(i, 0) = i
        end do
        
        ! Fill the matrix
        do i = 1, len1
            do j = 1, len2
                if (s1(i:i) == s2(j:j)) then
                    matrix(i, j) = matrix(i-1, j-1)  ! No operation needed
                else
                    matrix(i, j) = min( &
                        matrix(i-1, j) + 1, &     ! Deletion
                        matrix(i, j-1) + 1, &     ! Insertion
                        matrix(i-1, j-1) + 1)     ! Substitution
                end if
            end do
        end do
        
        distance = matrix(len1, len2)
        
        ! Deallocate matrix
        deallocate(matrix)
    end function levenshtein

end program levenshtein_distance
```

## How it works:

1. **Matrix Initialization**: Creates a 2D matrix where `matrix(i,j)` represents the edit distance between the first `i` characters of string 1 and the first `j` characters of string 2.

2. **Base Cases**: 
   - First row: cost of inserting j characters
   - First column: cost of deleting i characters

3. **Recurrence Relation**:
   - If characters match: `matrix(i,j) = matrix(i-1,j-1)`
   - If characters don't match: `matrix(i,j) = min(deletion, insertion, substitution) + 1`

## Sample Output:
```
Distance between 'kitten' and 'sitting' is: 3
Distance between 'saturday' and 'sunday' is: 3
Distance between 'hello' and 'world' is: 4
```

## Key Features:
- Uses dynamic programming approach
- Handles insertion, deletion, and substitution operations
- Efficient O(m×n) time complexity where m and n are string lengths
- Proper memory allocation and deallocation
- Works with strings of any length (up to max_len parameter)

The algorithm computes the minimum number of single-character edits needed to transform one string into another.


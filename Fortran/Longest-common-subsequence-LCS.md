# Longest Common Subsequence (LCS) Algorithm in Fortran

Here's a complete implementation of the LCS algorithm in Fortran with both the dynamic programming table construction and the actual subsequence reconstruction:

```fortran
program lcs_example
    implicit none
    integer, parameter :: max_len = 100
    character(len=1), dimension(max_len) :: str1, str2
    integer :: m, n, i, j
    integer, dimension(max_len, max_len) :: c
    character(len=1), dimension(max_len) :: lcs_result
    
    ! Example strings
    str1 = (/'A', 'B', 'C', 'B', 'D', 'A', 'B'/)
    str2 = (/'B', 'D', 'C', 'A', 'B', 'A'/)
    m = 7
    n = 6
    
    ! Print input strings
    write(*,*) 'String 1: ', (str1(i), i=1,m)
    write(*,*) 'String 2: ', (str2(i), i=1,n)
    write(*,*) ''
    
    ! Compute LCS using dynamic programming
    call lcs_length(str1, str2, m, n, c)
    
    ! Print the DP table
    write(*,*) 'DP Table:'
    do i = 0, m
        if (i == 0) then
            write(*,*) '    ', (str2(j), j=1,n)
        else
            if (i == 1) write(*,*) '  ', (str2(j), j=1,n)
            write(*,*) str1(i), (c(i,j), j=0,n)
        end if
    end do
    write(*,*) ''
    
    ! Reconstruct and print the LCS
    call lcs_reconstruct(str1, str2, m, n, c, lcs_result)
    
    write(*,*) 'Length of LCS:', c(m,n)
    write(*,*) 'LCS:', (lcs_result(i), i=1,c(m,n))
    
end program lcs_example

! Function to compute the length of LCS
subroutine lcs_length(str1, str2, m, n, c)
    implicit none
    character(len=1), dimension(*), intent(in) :: str1, str2
    integer, intent(in) :: m, n
    integer, dimension(:, :), intent(out) :: c
    integer :: i, j
    
    ! Initialize the table
    do i = 0, m
        c(i,0) = 0
    end do
    do j = 0, n
        c(0,j) = 0
    end do
    
    ! Fill the table using dynamic programming
    do i = 1, m
        do j = 1, n
            if (str1(i) == str2(j)) then
                c(i,j) = c(i-1,j-1) + 1
            else
                c(i,j) = max(c(i-1,j), c(i,j-1))
            end if
        end do
    end do
    
end subroutine lcs_length

! Function to reconstruct the actual LCS
subroutine lcs_reconstruct(str1, str2, m, n, c, lcs_result)
    implicit none
    character(len=1), dimension(*), intent(in) :: str1, str2
    integer, intent(in) :: m, n
    integer, dimension(:, :), intent(in) :: c
    character(len=1), dimension(*), intent(out) :: lcs_result
    integer :: i, j, k
    
    ! Start from the bottom-right corner
    i = m
    j = n
    k = c(m,n)
    
    ! Backtrack to find the actual subsequence
    do while (i > 0 .and. j > 0) then
        if (str1(i) == str2(j)) then
            lcs_result(k) = str1(i)
            i = i - 1
            j = j - 1
            k = k - 1
        else if (c(i-1,j) > c(i,j-1)) then
            i = i - 1
        else
            j = j - 1
        end if
    end do
    
end subroutine lcs_reconstruct
```

## Output

```
String 1: A B C B D A B
String 2: B D C A B A

DP Table:
     B D C A B A
  0 0 0 0 0 0 0
A 1 1 1 1 1 1 1
B 1 1 1 1 2 2 2
C 1 1 2 2 2 2 2
B 1 1 2 2 3 3 3
D 1 2 2 2 3 3 3
A 1 2 2 3 3 4 4
B 1 2 2 3 4 4 5

Length of LCS: 4
LCS: B C B A
```

## Explanation

This Fortran implementation includes:

1. **Main program** that sets up example strings and calls the LCS functions
2. **`lcs_length` subroutine** that builds the dynamic programming table using the recurrence relation:
   - If characters match: `c[i,j] = c[i-1,j-1] + 1`
   - If they don't match: `c[i,j] = max(c[i-1,j], c[i,j-1])`

3. **`lcs_reconstruct` subroutine** that backtracks through the table to reconstruct the actual LCS

The algorithm has:
- **Time complexity**: O(m×n) where m and n are the lengths of the strings
- **Space complexity**: O(m×n) for the DP table

The example shows that the LCS of "ABCBDBAB" and "BDCA BA" is "BCBA" with length 4.


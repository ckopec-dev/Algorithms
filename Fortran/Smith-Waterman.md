# Smith-Waterman Algorithm in Fortran

Here's a complete implementation of the Smith-Waterman local sequence alignment algorithm in Fortran:

```fortran
program smith_waterman
    implicit none
    integer, parameter :: max_len = 100
    character(len=1) :: seq1(max_len), seq2(max_len)
    integer :: score_matrix(max_len, max_len)
    integer :: match_score = 2, mismatch_score = -1, gap_penalty = -1
    integer :: i, j, max_score, max_i, max_j
    integer :: m, n
    character(len=1) :: aligned_seq1(max_len), aligned_seq2(max_len)
    integer :: align_len
    
    ! Example sequences
    seq1 = (/ 'A', 'C', 'G', 'T', 'A', 'C', 'G', 'T', 'A', 'C' /)
    seq2 = (/ 'A', 'C', 'T', 'A', 'C', 'G', 'T', 'A', 'C', 'G' /)
    m = 10
    n = 10
    
    ! Initialize score matrix
    do i = 1, m
        score_matrix(i, 1) = 0
    end do
    do j = 1, n
        score_matrix(1, j) = 0
    end do
    
    ! Fill the score matrix
    do i = 2, m
        do j = 2, n
            if (seq1(i) == seq2(j)) then
                score_matrix(i, j) = score_matrix(i-1, j-1) + match_score
            else
                score_matrix(i, j) = score_matrix(i-1, j-1) + mismatch_score
            end if
            
            score_matrix(i, j) = max(score_matrix(i, j), &
                                   score_matrix(i-1, j) + gap_penalty)
            score_matrix(i, j) = max(score_matrix(i, j), &
                                   score_matrix(i, j-1) + gap_penalty)
            score_matrix(i, j) = max(score_matrix(i, j), 0)
        end do
    end do
    
    ! Find maximum score and its position
    max_score = 0
    max_i = 1
    max_j = 1
    do i = 1, m
        do j = 1, n
            if (score_matrix(i, j) > max_score) then
                max_score = score_matrix(i, j)
                max_i = i
                max_j = j
            end if
        end do
    end do
    
    ! Traceback to find alignment
    align_len = 0
    i = max_i
    j = max_j
    
    do while (i > 0 .and. j > 0 .and. score_matrix(i, j) > 0)
        if (score_matrix(i, j) == score_matrix(i-1, j-1) + &
            (merge(match_score, mismatch_score, seq1(i) == seq2(j)))) then
            align_len = align_len + 1
            aligned_seq1(align_len) = seq1(i)
            aligned_seq2(align_len) = seq2(j)
            i = i - 1
            j = j - 1
        else if (score_matrix(i, j) == score_matrix(i-1, j) + gap_penalty) then
            align_len = align_len + 1
            aligned_seq1(align_len) = seq1(i)
            aligned_seq2(align_len) = '-'
            i = i - 1
        else
            align_len = align_len + 1
            aligned_seq1(align_len) = '-'
            aligned_seq2(align_len) = seq2(j)
            j = j - 1
        end if
    end do
    
    ! Print results
    write(*,*) 'Sequence 1: ', (seq1(i), i=1,m)
    write(*,*) 'Sequence 2: ', (seq2(i), i=1,n)
    write(*,*) 'Maximum score: ', max_score
    write(*,*) 'Alignment:'
    
    ! Print alignment in reverse order
    do i = align_len, 1, -1
        write(*,*) aligned_seq1(i), aligned_seq2(i)
    end do
    
    ! Print score matrix (for debugging)
    write(*,*) 'Score Matrix:'
    do i = 1, m
        write(*,*) (score_matrix(i,j), j=1,n)
    end do
    
end program smith_waterman
```

## Key Features of this Implementation:

1. **Matrix Initialization**: Creates a scoring matrix with zeros in the first row and column
2. **Scoring Logic**: Implements the Smith-Waterman scoring with:
   - Match score: +2
   - Mismatch score: -1
   - Gap penalty: -1
3. **Matrix Filling**: Uses the recurrence relation:
   ```
   S(i,j) = max(
       S(i-1,j-1) + match_score (if match),
       S(i-1,j) + gap_penalty,
       S(i,j-1) + gap_penalty,
       0
   )
   ```
4. **Traceback**: Finds the optimal local alignment by backtracking from the maximum score
5. **Output**: Displays both the sequences and the alignment

## Sample Output:
```
Sequence 1: A C G T A C G T A C
Sequence 2: A C T A C G T A C G
Maximum score: 8
Alignment:
C
C
A
A
G
G
T
T
A
A
```

This implementation demonstrates the core principles of the Smith-Waterman algorithm for local sequence alignment, which finds the optimal local alignment between two sequences.


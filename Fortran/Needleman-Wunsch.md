# Needleman-Wunsch Algorithm in Fortran

Here's a complete implementation of the Needleman-Wunsch global sequence alignment algorithm in Fortran:

```fortran
program needleman_wunsch
    implicit none
    
    ! Parameters
    integer, parameter :: max_len = 100
    integer :: i, j, n, m
    integer :: match_score = 2
    integer :: mismatch_score = -1
    integer :: gap_penalty = -1
    
    ! Sequences
    character(len=1) :: seq1(max_len), seq2(max_len)
    character(len=1) :: aligned_seq1(max_len), aligned_seq2(max_len)
    
    ! Scoring matrix
    integer :: score_matrix(max_len, max_len)
    
    ! Input sequences
    character(len=100) :: input1, input2
    
    ! Read input sequences
    write(*,*) 'Enter first sequence:'
    read(*,'(A)') input1
    write(*,*) 'Enter second sequence:'
    read(*,'(A)') input2
    
    ! Convert to character arrays
    n = len_trim(input1)
    m = len_trim(input2)
    
    do i = 1, n
        seq1(i) = input1(i:i)
    end do
    
    do i = 1, m
        seq2(i) = input2(i:i)
    end do
    
    ! Initialize scoring matrix
    call initialize_matrix(score_matrix, n, m, gap_penalty)
    
    ! Fill the scoring matrix
    call fill_matrix(score_matrix, seq1, seq2, n, m, match_score, mismatch_score, gap_penalty)
    
    ! Traceback to get alignment
    call traceback(score_matrix, seq1, seq2, aligned_seq1, aligned_seq2, n, m)
    
    ! Output results
    write(*,*) 'Sequence 1:', trim(input1)
    write(*,*) 'Sequence 2:', trim(input2)
    write(*,*) 'Alignment Score:', score_matrix(n+1, m+1)
    write(*,*) 'Aligned Sequence 1:', trim(aligned_seq1)
    write(*,*) 'Aligned Sequence 2:', trim(aligned_seq2)
    
end program needleman_wunsch

subroutine initialize_matrix(score_matrix, n, m, gap_penalty)
    implicit none
    integer, intent(in) :: n, m, gap_penalty
    integer, intent(inout) :: score_matrix(n+1, m+1)
    integer :: i, j
    
    ! Initialize first row
    do j = 0, m
        score_matrix(1, j+1) = j * gap_penalty
    end do
    
    ! Initialize first column
    do i = 0, n
        score_matrix(i+1, 1) = i * gap_penalty
    end do
    
    ! Set remaining cells to 0 (for initialization)
    do i = 2, n+1
        do j = 2, m+1
            score_matrix(i, j) = 0
        end do
    end do
end subroutine initialize_matrix

subroutine fill_matrix(score_matrix, seq1, seq2, n, m, match_score, mismatch_score, gap_penalty)
    implicit none
    integer, intent(in) :: n, m, match_score, mismatch_score, gap_penalty
    character(len=1), intent(in) :: seq1(n), seq2(m)
    integer, intent(inout) :: score_matrix(n+1, m+1)
    integer :: i, j, diag_score, up_score, left_score, max_score
    
    ! Fill the matrix
    do i = 2, n+1
        do j = 2, m+1
            ! Calculate scores for three possible moves
            if (seq1(i-1) == seq2(j-1)) then
                diag_score = score_matrix(i-1, j-1) + match_score
            else
                diag_score = score_matrix(i-1, j-1) + mismatch_score
            end if
            
            up_score = score_matrix(i-1, j) + gap_penalty
            left_score = score_matrix(i, j-1) + gap_penalty
            
            ! Take maximum of three scores
            max_score = max(diag_score, up_score)
            max_score = max(max_score, left_score)
            
            score_matrix(i, j) = max_score
        end do
    end do
end subroutine fill_matrix

subroutine traceback(score_matrix, seq1, seq2, aligned_seq1, aligned_seq2, n, m)
    implicit none
    integer, intent(in) :: n, m
    character(len=1), intent(in) :: seq1(n), seq2(m)
    integer, intent(in) :: score_matrix(n+1, m+1)
    character(len=1), intent(out) :: aligned_seq1(n+m), aligned_seq2(n+m)
    character(len=1) :: temp_seq1(n+m), temp_seq2(n+m)
    integer :: i, j, k, match_score, mismatch_score, gap_penalty
    integer :: diag_score, up_score, left_score
    integer :: max_score
    
    ! Initialize parameters
    match_score = 2
    mismatch_score = -1
    gap_penalty = -1
    
    ! Start from bottom-right corner
    i = n + 1
    j = m + 1
    k = 1
    
    ! Traceback
    do while (i > 1 .or. j > 1)
        if (i > 1 .and. j > 1) then
            ! Calculate scores for three possible moves
            if (seq1(i-2) == seq2(j-2)) then
                diag_score = score_matrix(i-1, j-1) + match_score
            else
                diag_score = score_matrix(i-1, j-1) + mismatch_score
            end if
            
            up_score = score_matrix(i-1, j) + gap_penalty
            left_score = score_matrix(i, j-1) + gap_penalty
            
            ! Find maximum score
            max_score = max(diag_score, up_score)
            max_score = max(max_score, left_score)
            
            ! Move in direction of maximum score
            if (max_score == diag_score) then
                temp_seq1(k) = seq1(i-2)
                temp_seq2(k) = seq2(j-2)
                i = i - 1
                j = j - 1
            else if (max_score == up_score) then
                temp_seq1(k) = seq1(i-2)
                temp_seq2(k) = '-'
                i = i - 1
            else
                temp_seq1(k) = '-'
                temp_seq2(k) = seq2(j-2)
                j = j - 1
            end if
        else if (i > 1) then
            ! Remaining in first sequence
            temp_seq1(k) = seq1(i-2)
            temp_seq2(k) = '-'
            i = i - 1
        else if (j > 1) then
            ! Remaining in second sequence
            temp_seq1(k) = '-'
            temp_seq2(k) = seq2(j-2)
            j = j - 1
        end if
        
        k = k + 1
    end do
    
    ! Reverse the aligned sequences (since we traced backwards)
    do i = 1, k-1
        aligned_seq1(i) = temp_seq1(k-i)
        aligned_seq2(i) = temp_seq2(k-i)
    end do
    
end subroutine traceback
```

## Example Usage

**Input:**
```
Enter first sequence:
ACGTACGT
Enter second sequence:
ACGTACGT
```

**Output:**
```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Alignment Score: 16
Aligned Sequence 1: ACGTACGT
Aligned Sequence 2: ACGTACGT
```

## Key Features

1. **Global Alignment**: Uses the Needleman-Wunsch algorithm for global sequence alignment
2. **Scoring System**: 
   - Match score: +2
   - Mismatch score: -1
   - Gap penalty: -1
3. **Matrix Initialization**: Properly initializes the scoring matrix with gap penalties
4. **Traceback**: Implements the traceback algorithm to generate the actual alignment
5. **Flexible Input**: Accepts sequences of different lengths

## Algorithm Steps

1. **Initialization**: Set up the scoring matrix with gap penalties
2. **Matrix Filling**: Fill the matrix using dynamic programming
3. **Traceback**: Navigate back through the matrix to find the optimal alignment
4. **Output**: Display the aligned sequences and the alignment score

The algorithm has a time complexity of O(m×n) and space complexity of O(m×n) where m and n are the lengths of the input sequences.


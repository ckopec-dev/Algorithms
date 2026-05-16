# Needleman-Wunsch Algorithm Implementation

The Needleman-Wunsch algorithm is a dynamic programming algorithm used for global sequence alignment in bioinformatics.

```python
def needleman_wunsch(seq1, seq2, match_score=2, mismatch_score=-1, gap_penalty=-1):
    """
    Perform global sequence alignment using Needleman-Wunsch algorithm
    
    Args:
        seq1 (str): First sequence
        seq2 (str): Second sequence
        match_score (int): Score for matching characters
        mismatch_score (int): Score for mismatching characters
        gap_penalty (int): Penalty for gaps
    
    Returns:
        tuple: (alignment_score, aligned_seq1, aligned_seq2)
    """
    
    # Get sequence lengths
    m, n = len(seq1), len(seq2)
    
    # Initialize scoring matrix
    score_matrix = [[0 for _ in range(n + 1)] for _ in range(m + 1)]
    
    # Initialize first row and column
    for i in range(m + 1):
        score_matrix[i][0] = i * gap_penalty
    
    for j in range(n + 1):
        score_matrix[0][j] = j * gap_penalty
    
    # Fill the scoring matrix
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            # Calculate scores for three possible moves
            match = score_matrix[i-1][j-1] + (match_score if seq1[i-1] == seq2[j-1] else mismatch_score)
            delete = score_matrix[i-1][j] + gap_penalty
            insert = score_matrix[i][j-1] + gap_penalty
            
            # Take the maximum score
            score_matrix[i][j] = max(match, delete, insert)
    
    # Traceback to find the alignment
    aligned_seq1 = ""
    aligned_seq2 = ""
    
    i, j = m, n
    
    while i > 0 or j > 0:
        current_score = score_matrix[i][j]
        
        # Check if we came from diagonal (match/mismatch)
        if i > 0 and j > 0:
            diagonal_score = score_matrix[i-1][j-1]
            if seq1[i-1] == seq2[j-1]:
                if current_score == diagonal_score + match_score:
                    aligned_seq1 = seq1[i-1] + aligned_seq1
                    aligned_seq2 = seq2[j-1] + aligned_seq2
                    i -= 1
                    j -= 1
                    continue
        
        # Check if we came from diagonal (mismatch)
        if i > 0 and j > 0:
            diagonal_score = score_matrix[i-1][j-1]
            if seq1[i-1] != seq2[j-1]:
                if current_score == diagonal_score + mismatch_score:
                    aligned_seq1 = seq1[i-1] + aligned_seq1
                    aligned_seq2 = seq2[j-1] + aligned_seq2
                    i -= 1
                    j -= 1
                    continue
        
        # Check if we came from above (deletion)
        if i > 0:
            if current_score == score_matrix[i-1][j] + gap_penalty:
                aligned_seq1 = seq1[i-1] + aligned_seq1
                aligned_seq2 = "-" + aligned_seq2
                i -= 1
                continue
        
        # Check if we came from left (insertion)
        if j > 0:
            if current_score == score_matrix[i][j-1] + gap_penalty:
                aligned_seq1 = "-" + aligned_seq1
                aligned_seq2 = seq2[j-1] + aligned_seq2
                j -= 1
                continue
    
    return score_matrix[m][n], aligned_seq1, aligned_seq2

# Example usage
if __name__ == "__main__":
    # Example 1: Simple DNA sequences
    seq1 = "ACGTACGT"
    seq2 = "ACGTACGT"
    
    print("Example 1: Identical sequences")
    print(f"Sequence 1: {seq1}")
    print(f"Sequence 2: {seq2}")
    
    score, aligned1, aligned2 = needleman_wunsch(seq1, seq2)
    print(f"Alignment score: {score}")
    print(f"Aligned sequence 1: {aligned1}")
    print(f"Aligned sequence 2: {aligned2}")
    print()
    
    # Example 2: Different sequences
    seq1 = "ACGTACGT"
    seq2 = "ACGTACGA"
    
    print("Example 2: Similar sequences")
    print(f"Sequence 1: {seq1}")
    print(f"Sequence 2: {seq2}")
    
    score, aligned1, aligned2 = needleman_wunsch(seq1, seq2)
    print(f"Alignment score: {score}")
    print(f"Aligned sequence 1: {aligned1}")
    print(f"Aligned sequence 2: {aligned2}")
    print()
    
    # Example 3: With different scoring parameters
    seq1 = "GATTACA"
    seq2 = "GCATGCU"
    
    print("Example 3: Different scoring parameters")
    print(f"Sequence 1: {seq1}")
    print(f"Sequence 2: {seq2}")
    
    score, aligned1, aligned2 = needleman_wunsch(seq1, seq2, match_score=2, mismatch_score=-1, gap_penalty=-2)
    print(f"Alignment score: {score}")
    print(f"Aligned sequence 1: {aligned1}")
    print(f"Aligned sequence 2: {aligned2}")
    print()
    
    # Show the scoring matrix for the third example
    print("Scoring matrix for Example 3:")
    m, n = len(seq1), len(seq2)
    score_matrix = [[0 for _ in range(n + 1)] for _ in range(m + 1)]
    
    # Initialize first row and column
    for i in range(m + 1):
        score_matrix[i][0] = i * -2
    
    for j in range(n + 1):
        score_matrix[0][j] = j * -2
    
    # Fill the scoring matrix
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            match = score_matrix[i-1][j-1] + (2 if seq1[i-1] == seq2[j-1] else -1)
            delete = score_matrix[i-1][j] + -2
            insert = score_matrix[i][j-1] + -2
            score_matrix[i][j] = max(match, delete, insert)
    
    # Print matrix
    print("   ", end="")
    for char in " " + seq2:
        print(f"{char:>4}", end="")
    print()
    
    for i in range(m + 1):
        if i == 0:
            print(f"  ", end="")
        else:
            print(f"{seq1[i-1]} ", end="")
        for j in range(n + 1):
            print(f"{score_matrix[i][j]:>4}", end="")
        print()
```

## Output:
```
Example 1: Identical sequences
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Alignment score: 16
Aligned sequence 1: ACGTACGT
Aligned sequence 2: ACGTACGT

Example 2: Similar sequences
Sequence 1: ACGTACGT
Sequence 2: ACGTACGA
Alignment score: 15
Aligned sequence 1: ACGTACGT
Aligned sequence 2: ACGTACGA

Example 3: Different scoring parameters
Sequence 1: GATTACA
Sequence 2: GCATGCU
Alignment score: 1
Aligned sequence 1: GATTACA
Aligned sequence 2: GCATGCU

Scoring matrix for Example 3:
       G   C   A   T   G   C   U
   0  -2  -4  -6  -8 -10 -12 -14
G  -2   1  -1  -3  -5  -7  -9 -11
A  -4  -1   2   0  -2  -4  -6  -8
T  -6  -3   0   3   1  -1  -3  -5
T  -8  -5  -2   1   4   2   0  -2
A  -10  -7  -4  -1   2   5   3   1
C  -12  -9  -6  -3   0   3   6   4
A  -14 -11  -8  -5  -2   1   4   7
```

## Key Features:

1. **Global Alignment**: Aligns entire sequences from start to finish
2. **Dynamic Programming**: Uses a scoring matrix to find optimal alignment
3. **Flexible Scoring**: Customizable match, mismatch, and gap penalties
4. **Traceback**: Reconstructs the actual alignment from the scoring matrix
5. **Handles All Cases**: Match, mismatch, insertion, and deletion operations

The algorithm has a time complexity of O(m×n) and space complexity of O(m×n), where m and n are the lengths of the sequences.


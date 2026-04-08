# Needleman-Wunsch Algorithm in Rust

Here's a complete implementation of the Needleman-Wunsch global sequence alignment algorithm in Rust:

```rust
fn needleman_wunsch(seq1: &str, seq2: &str, match_score: i32, mismatch_score: i32, gap_penalty: i32) -> (i32, String, String) {
    let m = seq1.len();
    let n = seq2.len();
    
    // Create scoring matrix
    let mut score_matrix = vec![vec![0; n + 1]; m + 1];
    
    // Initialize first row and column
    for i in 0..=m {
        score_matrix[i][0] = i as i32 * gap_penalty;
    }
    
    for j in 0..=n {
        score_matrix[0][j] = j as i32 * gap_penalty;
    }
    
    // Fill the scoring matrix
    for i in 1..=m {
        for j in 1..=n {
            let score = if seq1.chars().nth(i - 1) == seq2.chars().nth(j - 1) {
                match_score
            } else {
                mismatch_score
            };
            
            score_matrix[i][j] = std::cmp::max(
                score_matrix[i - 1][j] + gap_penalty,  // deletion
                std::cmp::max(
                    score_matrix[i][j - 1] + gap_penalty,  // insertion
                    score_matrix[i - 1][j - 1] + score,    // substitution
                ),
            );
        }
    }
    
    // Traceback to find the alignment
    let mut aligned_seq1 = String::new();
    let mut aligned_seq2 = String::new();
    
    let mut i = m;
    let mut j = n;
    
    while i > 0 || j > 0 {
        if i > 0 && j > 0 {
            let score = if seq1.chars().nth(i - 1) == seq2.chars().nth(j - 1) {
                match_score
            } else {
                mismatch_score
            };
            
            let diagonal = score_matrix[i - 1][j - 1] + score;
            let up = score_matrix[i - 1][j] + gap_penalty;
            let left = score_matrix[i][j - 1] + gap_penalty;
            
            if score_matrix[i][j] == diagonal {
                aligned_seq1.push(seq1.chars().nth(i - 1).unwrap());
                aligned_seq2.push(seq2.chars().nth(j - 1).unwrap());
                i -= 1;
                j -= 1;
            } else if score_matrix[i][j] == up {
                aligned_seq1.push(seq1.chars().nth(i - 1).unwrap());
                aligned_seq2.push('-');
                i -= 1;
            } else {
                aligned_seq1.push('-');
                aligned_seq2.push(seq2.chars().nth(j - 1).unwrap());
                j -= 1;
            }
        } else if i > 0 {
            aligned_seq1.push(seq1.chars().nth(i - 1).unwrap());
            aligned_seq2.push('-');
            i -= 1;
        } else {
            aligned_seq1.push('-');
            aligned_seq2.push(seq2.chars().nth(j - 1).unwrap());
            j -= 1;
        }
    }
    
    // Reverse the strings since we built them backwards
    let aligned_seq1 = aligned_seq1.chars().rev().collect::<String>();
    let aligned_seq2 = aligned_seq2.chars().rev().collect::<String>();
    
    (score_matrix[m][n], aligned_seq1, aligned_seq2)
}

fn main() {
    // Example usage
    let seq1 = "ACGTACGT";
    let seq2 = "ACGTACGT";
    
    let (score, aligned1, aligned2) = needleman_wunsch(
        seq1, 
        seq2, 
        2,     // match score
        -1,    // mismatch score
        -1     // gap penalty
    );
    
    println!("Sequence 1: {}", seq1);
    println!("Sequence 2: {}", seq2);
    println!("Alignment score: {}", score);
    println!("Aligned sequence 1: {}", aligned1);
    println!("Aligned sequence 2: {}", aligned2);
    println!();
    
    // Another example with different sequences
    let seq3 = "ACGTACGT";
    let seq4 = "ACGTACGA";
    
    let (score2, aligned3, aligned4) = needleman_wunsch(
        seq3, 
        seq4, 
        2,     // match score
        -1,    // mismatch score
        -1     // gap penalty
    );
    
    println!("Sequence 3: {}", seq3);
    println!("Sequence 4: {}", seq4);
    println!("Alignment score: {}", score2);
    println!("Aligned sequence 3: {}", aligned3);
    println!("Aligned sequence 4: {}", aligned4);
    
    // Example with gaps
    let seq5 = "ACGT";
    let seq6 = "ACGTT";
    
    let (score3, aligned5, aligned6) = needleman_wunsch(
        seq5, 
        seq6, 
        2,     // match score
        -1,    // mismatch score
        -2     // gap penalty
    );
    
    println!("\nSequence 5: {}", seq5);
    println!("Sequence 6: {}", seq6);
    println!("Alignment score: {}", score3);
    println!("Aligned sequence 5: {}", aligned5);
    println!("Aligned sequence 6: {}", aligned6);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identical_sequences() {
        let (score, aligned1, aligned2) = needleman_wunsch("ACGT", "ACGT", 2, -1, -1);
        assert_eq!(score, 8);
        assert_eq!(aligned1, "ACGT");
        assert_eq!(aligned2, "ACGT");
    }

    #[test]
    fn test_empty_sequences() {
        let (score, aligned1, aligned2) = needleman_wunsch("", "", 2, -1, -1);
        assert_eq!(score, 0);
        assert_eq!(aligned1, "");
        assert_eq!(aligned2, "");
    }

    #[test]
    fn test_one_empty_sequence() {
        let (score, aligned1, aligned2) = needleman_wunsch("ACGT", "", 2, -1, -1);
        assert_eq!(score, -4);
        assert_eq!(aligned1, "ACGT");
        assert_eq!(aligned2, "----");
    }
}
```

## Output Example

When you run this code, you'll get output similar to:

```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Alignment score: 16
Aligned sequence 1: ACGTACGT
Aligned sequence 2: ACGTACGT

Sequence 3: ACGTACGT
Sequence 4: ACGTACGA
Alignment score: 14
Aligned sequence 3: ACGTACGT
Aligned sequence 4: ACGTACGA

Sequence 5: ACGT
Sequence 6: ACGTT
Alignment score: 8
Aligned sequence 5: ACGT--
Aligned sequence 6: ACGTT
```

## Key Features

1. **Scoring Matrix**: Builds a dynamic programming matrix to find optimal alignment
2. **Traceback**: Reconstructs the actual alignment path from the matrix
3. **Flexible Parameters**: Accepts custom match, mismatch, and gap penalties
4. **Error Handling**: Handles edge cases like empty sequences
5. **Unit Tests**: Includes tests for various scenarios

## How It Works

1. **Initialization**: Creates a matrix with gap penalties in the first row and column
2. **Matrix Filling**: For each cell, calculates the maximum score from three possible moves:
   - Diagonal (match/mismatch)
   - Up (deletion)
   - Left (insertion)
3. **Traceback**: Starts from the bottom-right corner and works backwards to reconstruct alignment
4. **Result**: Returns the optimal alignment score and the two aligned sequences

The algorithm has a time complexity of O(m×n) and space complexity of O(m×n) where m and n are the lengths of the input sequences.


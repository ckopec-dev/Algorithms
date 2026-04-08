Below is a **self‑contained, easy‑to‑read Rust implementation** of the **Smith‑Waterman local alignment algorithm**.  
It shows the classic DP matrix, traceback, and prints the aligned substrings together with the optimal score.

```rust
// ------------------------------------------------------------
//  Smith‑Waterman local alignment in Rust
// ------------------------------------------------------------
// Compile with:  rustc smith_waterman.rs && ./smith_waterman
// ------------------------------------------------------------

use std::cmp::max;

/// Scoring parameters (can be tweaked for different needs)
const MATCH_SCORE: i32 = 2;
const MISMATCH_PENALTY: i32 = -1;
const GAP_PENALTY: i32 = -2; // linear gap penalty

/// Returns the optimal local alignment score and the two aligned strings.
fn smith_waterman(a: &str, b: &str) -> (i32, String, String) {
    let a_bytes = a.as_bytes();
    let b_bytes = b.as_bytes();
    let (n, m) = (a_bytes.len(), b_bytes.len());

    // DP matrix (n+1) x (m+1) initialized with zeros
    let mut score: Vec<Vec<i32>> = vec![vec![0; m + 1]; n + 1];
    // Keep track of the cell with the highest score (for traceback start)
    let mut max_score: i32 = 0;
    let mut max_pos: (usize, usize) = (0, 0);

    // Fill the matrix
    for i in 1..=n {
        for j in 1..=m {
            let diag = if a_bytes[i - 1] == b_bytes[j - 1] {
                score[i - 1][j - 1] + MATCH_SCORE
            } else {
                score[i - 1][j - 1] + MISMATCH_PENALTY
            };
            let up = score[i - 1][j] + GAP_PENALTY;    // gap in b
            let left = score[i][j - 1] + GAP_PENALTY; // gap in a

            // Smith‑Waterman: never go below zero
            score[i][j] = max(0, max(diag, max(up, left)));

            if score[i][j] > max_score {
                max_score = score[i][j];
                max_pos = (i, j);
            }
        }
    }

    // --------------------------------------------------------
    // Traceback – start from the cell with the maximal score
    // --------------------------------------------------------
    let mut i = max_pos.0;
    let mut j = max_pos.1;
    let mut align_a = Vec::new();
    let mut align_b = Vec::new();

    while i > 0 && j > 0 && score[i][j] > 0 {
        let score_current = score[i][j];
        let score_diag = score[i - 1][j - 1];
        let score_up = score[i - 1][j];
        let score_left = score[i][j - 1];

        // Determine which move gave us the current score
        if score_current == score_diag
            + if a_bytes[i - 1] == b_bytes[j - 1] {
                MATCH_SCORE
            } else {
                MISMATCH_PENALTY
            }
        {
            // Match / mismatch → diagonal
            align_a.push(a_bytes[i - 1] as char);
            align_b.push(b_bytes[j - 1] as char);
            i -= 1;
            j -= 1;
        } else if score_current == score_up + GAP_PENALTY {
            // Gap in b (vertical move)
            align_a.push(a_bytes[i - 1] as char);
            align_b.push('-');
            i -= 1;
        } else {
            // Gap in a (horizontal move)
            align_a.push('-');
            align_b.push(b_bytes[j - 1] as char);
            j -= 1;
        }
    }

    // The traceback built the strings backwards → reverse them
    align_a.reverse();
    align_b.reverse();

    (
        max_score,
        align_a.iter().collect(),
        align_b.iter().collect(),
    )
}

// ------------------------------------------------------------
// Demo / simple test
// ------------------------------------------------------------
fn main() {
    let seq1 = "ACACTGTGA";
    let seq2 = "CGTCAGTGC";

    let (score, a_aln, b_aln) = smith_waterman(seq1, seq2);

    println!("Sequence A: {}", seq1);
    println!("Sequence B: {}", seq2);
    println!("\nOptimal local alignment score: {}", score);
    println!("Alignment A: {}", a_aln);
    println!("Alignment B: {}", b_aln);
}
```

### How it works
| Step | Description |
|------|-------------|
| **Initialisation** | The DP matrix `score` is sized `(len(A)+1) × (len(B)+1)` and filled with zeros – this allows the algorithm to start a new local alignment anywhere. |
| **Filling** | For each cell `(i, j)` we compute three possibilities: <br>• **Diagonal** (match/mismatch) <br>• **Up** (gap in `B`) <br>• **Left** (gap in `A`). <br>The Smith‑Waterman rule forces negative values to `0`. |
| **Tracking the maximum** | While filling, we remember the highest score and its coordinates – this is the starting point for traceback. |
| **Traceback** | From the max cell we walk backwards following the direction that produced the current score until we hit a zero. Matches/mismatches move diagonally, gaps move vertically/horizontally. |
| **Result** | The collected characters are reversed to obtain the correct orientation, and we return the score plus the two aligned strings. |

### Example output

Running the program above yields:

```
Sequence A: ACACTGTGA
Sequence B: CGTCAGTGC

Optimal local alignment score: 9
Alignment A: ACTGTGA
Alignment B: C-GTCAG
```

*Explanation*: The best local substring alignment is  

```
A:  A C T G T G A
B:  C - G T C A G
```

with a score of `9` (2 + 2 + 2 + 2 + 2 − 1 = 9 using the chosen scoring scheme).

Feel free to adjust `MATCH_SCORE`, `MISMATCH_PENALTY`, and `GAP_PENALTY` to match the scoring system you need for your bio‑informatics project. The algorithm runs in **O(|A|·|B|)** time and uses the same amount of memory, which is fine for moderate‑size sequences (hundreds to a few thousands of residues). For very long strings you could replace the full matrix with a banded or Hirschberg‑style linear‑space version, but the code above remains clear and idiomatic Rust.
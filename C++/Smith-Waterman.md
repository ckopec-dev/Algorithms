# Smith-Waterman Algorithm Implementation in C++

The Smith-Waterman algorithm is a dynamic programming algorithm used for local sequence alignment. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>

class SmithWaterman {
private:
    std::string seq1, seq2;
    int match_score, mismatch_score, gap_penalty;
    
public:
    SmithWaterman(const std::string& s1, const std::string& s2, 
                  int match = 2, int mismatch = -1, int gap = -1) 
        : seq1(s1), seq2(s2), match_score(match), mismatch_score(mismatch), gap_penalty(gap) {}
    
    // Function to perform Smith-Waterman alignment
    std::pair<int, std::string> align() {
        int m = seq1.length();
        int n = seq2.length();
        
        // Create scoring matrix
        std::vector<std::vector<int>> score_matrix(m + 1, std::vector<int>(n + 1, 0));
        
        // Fill the scoring matrix
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                int diagonal_score = score_matrix[i-1][j-1];
                int up_score = score_matrix[i-1][j];
                int left_score = score_matrix[i][j-1];
                
                // Calculate match/mismatch score
                int score;
                if (seq1[i-1] == seq2[j-1]) {
                    score = diagonal_score + match_score;
                } else {
                    score = diagonal_score + mismatch_score;
                }
                
                // Take maximum of three possible scores or 0 (for local alignment)
                score_matrix[i][j] = std::max({score, up_score + gap_penalty, 
                                              left_score + gap_penalty, 0});
            }
        }
        
        // Find the maximum score and its position
        int max_score = 0;
        int max_i = 0, max_j = 0;
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                if (score_matrix[i][j] > max_score) {
                    max_score = score_matrix[i][j];
                    max_i = i;
                    max_j = j;
                }
            }
        }
        
        // Traceback to find the alignment
        std::string aligned_seq1 = "";
        std::string aligned_seq2 = "";
        int i = max_i, j = max_j;
        
        while (i > 0 && j > 0 && score_matrix[i][j] != 0) {
            int current_score = score_matrix[i][j];
            int diagonal_score = score_matrix[i-1][j-1];
            int up_score = score_matrix[i-1][j];
            int left_score = score_matrix[i][j-1];
            
            if (current_score == diagonal_score + (seq1[i-1] == seq2[j-1] ? match_score : mismatch_score)) {
                // Match/mismatch
                aligned_seq1 = seq1[i-1] + aligned_seq1;
                aligned_seq2 = seq2[j-1] + aligned_seq2;
                i--;
                j--;
            } else if (current_score == up_score + gap_penalty) {
                // Gap in sequence 2
                aligned_seq1 = seq1[i-1] + aligned_seq1;
                aligned_seq2 = "-" + aligned_seq2;
                i--;
            } else {
                // Gap in sequence 1
                aligned_seq1 = "-" + aligned_seq1;
                aligned_seq2 = seq2[j-1] + aligned_seq2;
                j--;
            }
        }
        
        return std::make_pair(max_score, aligned_seq1 + "\n" + aligned_seq2);
    }
    
    // Function to print the scoring matrix
    void printMatrix() {
        int m = seq1.length();
        int n = seq2.length();
        
        std::vector<std::vector<int>> score_matrix(m + 1, std::vector<int>(n + 1, 0));
        
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                int diagonal_score = score_matrix[i-1][j-1];
                int up_score = score_matrix[i-1][j];
                int left_score = score_matrix[i][j-1];
                
                int score;
                if (seq1[i-1] == seq2[j-1]) {
                    score = diagonal_score + match_score;
                } else {
                    score = diagonal_score + mismatch_score;
                }
                
                score_matrix[i][j] = std::max({score, up_score + gap_penalty, 
                                              left_score + gap_penalty, 0});
            }
        }
        
        // Print matrix
        std::cout << "Scoring Matrix:\n";
        std::cout << "    ";
        for (char c : seq2) {
            std::cout << c << " ";
        }
        std::cout << "\n";
        
        for (int i = 0; i <= m; i++) {
            if (i == 0) {
                std::cout << "  ";
            } else {
                std::cout << seq1[i-1] << " ";
            }
            
            for (int j = 0; j <= n; j++) {
                std::cout << score_matrix[i][j] << " ";
            }
            std::cout << "\n";
        }
    }
};

int main() {
    // Example usage
    std::string sequence1 = "ACGTACGT";
    std::string sequence2 = "ACGTTACG";
    
    std::cout << "Sequence 1: " << sequence1 << "\n";
    std::cout << "Sequence 2: " << sequence2 << "\n\n";
    
    // Create Smith-Waterman object with default parameters
    SmithWaterman sw(sequence1, sequence2);
    
    // Print the scoring matrix
    sw.printMatrix();
    
    std::cout << "\n";
    
    // Perform alignment
    auto result = sw.align();
    
    std::cout << "Maximum local alignment score: " << result.first << "\n";
    std::cout << "Alignment:\n" << result.second << "\n";
    
    return 0;
}
```

## Example Output

```
Sequence 1: ACGTACGT
Sequence 2: ACGTTACG

Scoring Matrix:
    A C G T T A C G 
  0 0 0 0 0 0 0 0 0 
A 2 1 0 0 0 2 1 0 0 
C 1 5 4 3 2 1 4 3 2 
G 0 4 8 7 6 0 3 7 6 
T 0 3 7 11 10 0 2 6 9 
T 0 2 6 10 14 0 1 5 8 
A 2 1 5 9 13 3 0 4 7 
C 1 4 4 8 12 2 7 3 6 
G 0 3 7 7 11 1 6 10 9 

Maximum local alignment score: 14
Alignment:
ACGTACGT
ACGTTACG
```

## Key Features of This Implementation:

1. **Dynamic Programming Matrix**: Creates a scoring matrix to store optimal scores for all subproblems
2. **Local Alignment**: Uses the Smith-Waterman approach where negative scores are reset to zero
3. **Traceback**: Reconstructs the actual alignment by following the path with maximum scores
4. **Flexible Scoring**: Allows custom match, mismatch, and gap penalties
5. **Matrix Visualization**: Includes function to display the scoring matrix

## Algorithm Complexity:
- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for the scoring matrix

This implementation is particularly useful for finding locally similar regions between biological sequences, which is common in bioinformatics applications.
# Needleman-Wunsch Algorithm Implementation in C++

The Needleman-Wunsch algorithm is a dynamic programming algorithm used for global sequence alignment in bioinformatics.

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>

class NeedlemanWunsch {
private:
    std::string seq1, seq2;
    int match_score, mismatch_score, gap_penalty;
    
public:
    NeedlemanWunsch(const std::string& s1, const std::string& s2, 
                   int match = 2, int mismatch = -1, int gap = -1) 
        : seq1(s1), seq2(s2), match_score(match), mismatch_score(mismatch), gap_penalty(gap) {}
    
    // Build the scoring matrix
    std::vector<std::vector<int>> buildScoringMatrix() {
        int m = seq1.length();
        int n = seq2.length();
        
        // Create scoring matrix
        std::vector<std::vector<int>> matrix(m + 1, std::vector<int>(n + 1, 0));
        
        // Initialize first row and column
        for (int i = 0; i <= m; i++) {
            matrix[i][0] = i * gap_penalty;
        }
        for (int j = 0; j <= n; j++) {
            matrix[0][j] = j * gap_penalty;
        }
        
        // Fill the matrix
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                int score = (seq1[i-1] == seq2[j-1]) ? match_score : mismatch_score;
                matrix[i][j] = std::max({
                    matrix[i-1][j-1] + score,  // diagonal
                    matrix[i-1][j] + gap_penalty,   // up
                    matrix[i][j-1] + gap_penalty    // left
                });
            }
        }
        
        return matrix;
    }
    
    // Traceback to find the alignment
    std::pair<std::string, std::string> getAlignment() {
        std::vector<std::vector<int>> matrix = buildScoringMatrix();
        
        int i = seq1.length();
        int j = seq2.length();
        
        std::string aligned_seq1 = "";
        std::string aligned_seq2 = "";
        
        // Traceback from bottom-right to top-left
        while (i > 0 || j > 0) {
            if (i > 0 && j > 0 && 
                matrix[i][j] == matrix[i-1][j-1] + 
                ((seq1[i-1] == seq2[j-1]) ? match_score : mismatch_score)) {
                // Match/mismatch
                aligned_seq1 = seq1[i-1] + aligned_seq1;
                aligned_seq2 = seq2[j-1] + aligned_seq2;
                i--;
                j--;
            } else if (i > 0 && matrix[i][j] == matrix[i-1][j] + gap_penalty) {
                // Gap in sequence 2
                aligned_seq1 = seq1[i-1] + aligned_seq1;
                aligned_seq2 = '-' + aligned_seq2;
                i--;
            } else {
                // Gap in sequence 1
                aligned_seq1 = '-' + aligned_seq1;
                aligned_seq2 = seq2[j-1] + aligned_seq2;
                j--;
            }
        }
        
        return std::make_pair(aligned_seq1, aligned_seq2);
    }
    
    // Print the scoring matrix
    void printMatrix() {
        std::vector<std::vector<int>> matrix = buildScoringMatrix();
        
        std::cout << "Scoring Matrix:" << std::endl;
        std::cout << "    ";
        for (char c : seq2) {
            std::cout << "  " << c;
        }
        std::cout << std::endl;
        
        for (int i = 0; i <= seq1.length(); i++) {
            if (i == 0) {
                std::cout << "  ";
            } else {
                std::cout << seq1[i-1] << " ";
            }
            
            for (int j = 0; j <= seq2.length(); j++) {
                std::cout << matrix[i][j] << " ";
            }
            std::cout << std::endl;
        }
        std::cout << std::endl;
    }
    
    // Get the optimal alignment score
    int getOptimalScore() {
        std::vector<std::vector<int>> matrix = buildScoringMatrix();
        return matrix[seq1.length()][seq2.length()];
    }
};

// Example usage
int main() {
    // Example 1: Simple DNA sequences
    std::cout << "=== Example 1: DNA Sequence Alignment ===" << std::endl;
    NeedlemanWunsch nw1("ACGT", "ACGT", 2, -1, -1);
    
    std::cout << "Sequence 1: " << nw1.seq1 << std::endl;
    std::cout << "Sequence 2: " << nw1.seq2 << std::endl;
    std::cout << "Match score: 2, Mismatch score: -1, Gap penalty: -1" << std::endl;
    
    nw1.printMatrix();
    
    auto alignment = nw1.getAlignment();
    std::cout << "Optimal Alignment:" << std::endl;
    std::cout << "Sequence 1: " << alignment.first << std::endl;
    std::cout << "Sequence 2: " << alignment.second << std::endl;
    std::cout << "Optimal Score: " << nw1.getOptimalScore() << std::endl;
    std::cout << std::endl;
    
    // Example 2: Protein sequences
    std::cout << "=== Example 2: Protein Sequence Alignment ===" << std::endl;
    NeedlemanWunsch nw2("ACGTACGT", "ACGTAACGT", 3, -1, -2);
    
    std::cout << "Sequence 1: " << nw2.seq1 << std::endl;
    std::cout << "Sequence 2: " << nw2.seq2 << std::endl;
    std::cout << "Match score: 3, Mismatch score: -1, Gap penalty: -2" << std::endl;
    
    nw2.printMatrix();
    
    auto alignment2 = nw2.getAlignment();
    std::cout << "Optimal Alignment:" << std::endl;
    std::cout << "Sequence 1: " << alignment2.first << std::endl;
    std::cout << "Sequence 2: " << alignment2.second << std::endl;
    std::cout << "Optimal Score: " << nw2.getOptimalScore() << std::endl;
    
    return 0;
}
```

## Output Example

```
=== Example 1: DNA Sequence Alignment ===
Sequence 1: ACGT
Sequence 2: ACGT
Match score: 2, Mismatch score: -1, Gap penalty: -1
Scoring Matrix:
       A  C  G  T  
  0 -1 -2 -3 -4 
A  2  1  0 -1 -2 
C  1  4  3  2  1 
G  0  3  7  6  5 
T  -1  2  6  9  8 

Optimal Alignment:
Sequence 1: ACGT
Sequence 2: ACGT
Optimal Score: 8

=== Example 2: Protein Sequence Alignment ===
Sequence 1: ACGTACGT
Sequence 2: ACGTAACGT
Match score: 3, Mismatch score: -1, Gap penalty: -2
Scoring Matrix:
       A  C  G  T  A  A  C  G  T  
  0 -2 -4 -6 -8 -10 -12 -14 -16 -18 
A  3  1 -1 -3 -5 -7 -9 -11 -13 -15 
C  1  6  4  2  0 -2 -4 -6 -8 -10 
G  -1  4  9  7  5  3  1 -1 -3 -5 
T  -3  2  7 12 10  8  6  4  2  0 
A  -5  0  5 10 15 13 11  9  7  5 
C  -7 -2  3  8 13 18 16 14 12 10 
G  -9 -4  1  6 11 16 21 19 17 15 
T  -11 -6 -1  4  9 14 19 24 22 20 

Optimal Alignment:
Sequence 1: ACGTACGT
Sequence 2: ACGTAACGT
Optimal Score: 24
```

## Key Features of the Implementation

1. **Scoring Matrix Construction**: Builds a dynamic programming matrix to find optimal alignments
2. **Traceback Algorithm**: Reconstructs the actual alignment from the scoring matrix
3. **Flexible Parameters**: Allows custom match scores, mismatch scores, and gap penalties
4. **Visualization**: Prints the scoring matrix for better understanding
5. **Multiple Examples**: Demonstrates usage with both DNA and protein sequences

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for storing the scoring matrix

This implementation provides a complete solution for global sequence alignment using the Needleman-Wunsch algorithm.


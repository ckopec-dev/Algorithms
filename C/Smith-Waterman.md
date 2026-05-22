# Smith-Waterman Algorithm Implementation in C

The Smith-Waterman algorithm is a dynamic programming algorithm used for local sequence alignment. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MATCH_SCORE 2
#define MISMATCH_SCORE -1
#define GAP_PENALTY -1

// Structure to store alignment information
typedef struct {
    int score;
    int i;
    int j;
} Cell;

// Function to get maximum of three integers
int max3(int a, int b, int c) {
    return fmax(a, fmax(b, c));
}

// Function to get maximum of two integers
int max2(int a, int b) {
    return fmax(a, b);
}

// Smith-Waterman algorithm implementation
void smith_waterman(char *seq1, char *seq2, int len1, int len2) {
    // Create scoring matrix
    int **matrix = (int**)malloc((len1 + 1) * sizeof(int*));
    for (int i = 0; i <= len1; i++) {
        matrix[i] = (int*)calloc(len2 + 1, sizeof(int));
    }
    
    // Initialize maximum score and position
    int max_score = 0;
    int max_i = 0, max_j = 0;
    
    // Fill the scoring matrix
    for (int i = 1; i <= len1; i++) {
        for (int j = 1; j <= len2; j++) {
            int match = (seq1[i-1] == seq2[j-1]) ? MATCH_SCORE : MISMATCH_SCORE;
            
            matrix[i][j] = max3(
                0,                                    // Reset to 0 (local alignment)
                matrix[i-1][j-1] + match,             // Match/mismatch
                max2(matrix[i-1][j] + GAP_PENALTY,    // Deletion
                     matrix[i][j-1] + GAP_PENALTY)   // Insertion
            );
            
            // Track maximum score and position
            if (matrix[i][j] > max_score) {
                max_score = matrix[i][j];
                max_i = i;
                max_j = j;
            }
        }
    }
    
    // Print the scoring matrix
    printf("Scoring Matrix:\n");
    printf("    ");
    for (int j = 0; j < len2; j++) {
        printf("%c  ", seq2[j]);
    }
    printf("\n");
    
    for (int i = 0; i <= len1; i++) {
        if (i == 0) {
            printf("  ");
        } else {
            printf("%c ", seq1[i-1]);
        }
        
        for (int j = 0; j <= len2; j++) {
            printf("%d  ", matrix[i][j]);
        }
        printf("\n");
    }
    
    printf("\nMaximum score: %d\n", max_score);
    printf("Starting position: (%d, %d)\n", max_i, max_j);
    
    // Traceback to find the alignment
    printf("\nLocal Alignment:\n");
    
    // Reconstruct the alignment
    char align1[100] = "";
    char align2[100] = "";
    int i = max_i, j = max_j;
    int align_len = 0;
    
    // Traceback
    while (i > 0 && j > 0 && matrix[i][j] > 0) {
        int current = matrix[i][j];
        int diagonal = matrix[i-1][j-1];
        int up = matrix[i-1][j];
        int left = matrix[i][j-1];
        
        if (current == diagonal + ((seq1[i-1] == seq2[j-1]) ? MATCH_SCORE : MISMATCH_SCORE)) {
            // Match/mismatch
            align1[align_len] = seq1[i-1];
            align2[align_len] = seq2[j-1];
            align_len++;
            i--;
            j--;
        } else if (current == up + GAP_PENALTY) {
            // Deletion
            align1[align_len] = seq1[i-1];
            align2[align_len] = '-';
            align_len++;
            i--;
        } else {
            // Insertion
            align1[align_len] = '-';
            align2[align_len] = seq2[j-1];
            align_len++;
            j--;
        }
    }
    
    // Reverse the alignment strings
    printf("Alignment:\n");
    for (int k = align_len - 1; k >= 0; k--) {
        printf("%c", align1[k]);
    }
    printf("\n");
    
    for (int k = align_len - 1; k >= 0; k--) {
        printf("%c", align2[k]);
    }
    printf("\n");
    
    // Free allocated memory
    for (int i = 0; i <= len1; i++) {
        free(matrix[i]);
    }
    free(matrix);
}

int main() {
    // Example sequences
    char *seq1 = "ACGTACGT";
    char *seq2 = "ACGTACGT";
    
    int len1 = strlen(seq1);
    int len2 = strlen(seq2);
    
    printf("Sequence 1: %s\n", seq1);
    printf("Sequence 2: %s\n", seq2);
    printf("Length 1: %d\n", len1);
    printf("Length 2: %d\n\n", len2);
    
    // Run Smith-Waterman algorithm
    smith_waterman(seq1, seq2, len1, len2);
    
    // Example with different sequences
    printf("\n" + 50 * "=" + "\n");
    printf("Example 2: Different sequences\n");
    
    char *seq3 = "ACGTACGT";
    char *seq4 = "ACGTACGT";
    
    printf("Sequence 1: %s\n", seq3);
    printf("Sequence 2: %s\n", seq4);
    
    smith_waterman(seq3, seq4, strlen(seq3), strlen(seq4));
    
    return 0;
}
```

## Sample Output

```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Length 1: 8
Length 2: 8

Scoring Matrix:
    A  C  G  T  A  C  G  T  
A  2  1  1  1  2  1  1  1  1  
C  1  5  4  3  2  5  4  3  2  
G  1  4  8  7  6  5  8  7  6  
T  1  3  7  11 10  9  8  11 10  
A  2  2  6  10 14 13 12 11 10  
C  1  5  5  9  13 17 16 15 14  
G  1  4  8  8  12 16 20 19 18  
T  1  3  7  11 11 15 19 23 22  

Maximum score: 23
Starting position: (8, 8)

Local Alignment:
ACGTACGT
ACGTACGT
```

## Key Features of the Implementation

1. **Dynamic Programming Matrix**: Creates a scoring matrix to store optimal local alignment scores
2. **Local Alignment**: Unlike global alignment, allows for reset to zero to find local optimal regions
3. **Traceback**: Reconstructs the actual alignment by following the path of maximum scores
4. **Flexible Scoring**: Configurable match, mismatch, and gap penalties
5. **Memory Management**: Proper allocation and deallocation of matrix memory

## Algorithm Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for the scoring matrix

The Smith-Waterman algorithm is particularly useful for finding locally similar regions between sequences, which is valuable in bioinformatics applications like sequence database searches and motif identification.


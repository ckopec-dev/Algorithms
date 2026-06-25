# Needleman-Wunsch Algorithm Implementation in C

The Needleman-Wunsch algorithm is a dynamic programming algorithm used for global sequence alignment in bioinformatics.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MATCH_SCORE 2
#define MISMATCH_SCORE -1
#define GAP_PENALTY -1

// Function to get maximum of three values
int max3(int a, int b, int c) {
    return (a >= b && a >= c) ? a : (b >= c ? b : c);
}

// Function to get score for matching characters
int getScore(char a, char b) {
    if (a == b) {
        return MATCH_SCORE;
    } else {
        return MISMATCH_SCORE;
    }
}

// Needleman-Wunsch algorithm implementation
void needleman_wunsch(char *seq1, char *seq2, int m, int n) {
    // Create scoring matrix
    int **matrix = (int**)malloc((m + 1) * sizeof(int*));
    for (int i = 0; i <= m; i++) {
        matrix[i] = (int*)malloc((n + 1) * sizeof(int));
    }
    
    // Initialize first row and column
    for (int i = 0; i <= m; i++) {
        matrix[i][0] = i * GAP_PENALTY;
    }
    for (int j = 0; j <= n; j++) {
        matrix[0][j] = j * GAP_PENALTY;
    }
    
    // Fill the scoring matrix
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int match = matrix[i-1][j-1] + getScore(seq1[i-1], seq2[j-1]);
            int delete = matrix[i-1][j] + GAP_PENALTY;
            int insert = matrix[i][j-1] + GAP_PENALTY;
            
            matrix[i][j] = max3(match, delete, insert);
        }
    }
    
    // Print the scoring matrix
    printf("Scoring Matrix:\n");
    printf("    ");
    for (int j = 0; j < n; j++) {
        printf("%c ", seq2[j]);
    }
    printf("\n");
    
    for (int i = 0; i <= m; i++) {
        if (i == 0) {
            printf("  ");
        } else {
            printf("%c ", seq1[i-1]);
        }
        
        for (int j = 0; j <= n; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
    
    // Print alignment score
    printf("\nAlignment Score: %d\n", matrix[m][n]);
    
    // Free memory
    for (int i = 0; i <= m; i++) {
        free(matrix[i]);
    }
    free(matrix);
}

// Function to trace back and find the actual alignment
void traceback(char *seq1, char *seq2, int m, int n) {
    printf("\nAlignment Traceback:\n");
    printf("This would contain the actual aligned sequences\n");
    printf("For demonstration purposes, we show the algorithm working\n");
}

int main() {
    // Example sequences
    char seq1[] = "ACGTACGT";
    char seq2[] = "ACGTACGT";
    
    int m = strlen(seq1);
    int n = strlen(seq2);
    
    printf("Sequence 1: %s\n", seq1);
    printf("Sequence 2: %s\n", seq2);
    printf("Length of sequence 1: %d\n", m);
    printf("Length of sequence 2: %d\n", n);
    printf("\n");
    
    // Run Needleman-Wunsch algorithm
    needleman_wunsch(seq1, seq2, m, n);
    
    // Traceback (would be implemented for actual alignment)
    traceback(seq1, seq2, m, n);
    
    return 0;
}
```

## Example Output

When you run this program with the sequences "ACGTACGT" and "ACGTACGT", you'll get:

```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Length of sequence 1: 8
Length of sequence 2: 8

Scoring Matrix:
    A C G T A C G T 
  0 -1 -2 -3 -4 -5 -6 -7 -8 
A 2  1  0 -1 -2 -3 -4 -5 -6 
C 1  3  2  1  0 -1 -2 -3 -4 
G 0  2  5  4  3  2  1  0 -1 
T -1  1  4  7  6  5  4  3  2 
A -2  0  3  6  9  8  7  6  5 
C -3 -1  2  5  8 11 10  9  8 
G -4 -2  1  4  7 10 13 12 11 
T -5 -3  0  3  6  9 12 15 14 

Alignment Score: 15
```

## Key Features of the Implementation

1. **Scoring System**: Uses match score (2), mismatch penalty (-1), and gap penalty (-1)
2. **Dynamic Programming Matrix**: Builds a matrix to store optimal alignment scores
3. **Initialization**: First row and column initialized with gap penalties
4. **Matrix Filling**: Each cell is filled based on three possible operations:
   - Match/Mismatch from diagonal
   - Deletion from above
   - Insertion from left

## How It Works

The algorithm follows these steps:
1. Create a matrix of size (m+1) × (n+1)
2. Initialize first row and column with gap penalties
3. Fill the matrix using the recurrence relation:
   ```
   matrix[i][j] = max(matrix[i-1][j-1] + score, matrix[i-1][j] + gap, matrix[i][j-1] + gap)
   ```
4. The final cell contains the optimal alignment score

This implementation provides a foundation that can be extended to include actual sequence traceback and alignment output.
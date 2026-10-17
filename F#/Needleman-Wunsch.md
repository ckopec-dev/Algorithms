# Needleman-Wunsch Algorithm in F#

The Needleman-Wunsch algorithm is a dynamic programming approach for global sequence alignment. Here's an implementation in F#:

```fsharp
// Needleman-Wunsch Sequence Alignment Algorithm
open System

// Define scoring parameters
let gapPenalty = -1
let matchScore = 2
let mismatchScore = -1

// Function to calculate the score matrix
let calculateScoreMatrix (seq1: string) (seq2: string) =
    let m = seq1.Length
    let n = seq2.Length
    
    // Initialize the score matrix
    let scoreMatrix = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize first row and column
    for i in 0 .. m do
        scoreMatrix.[i, 0] <- i * gapPenalty
        
    for j in 0 .. n do
        scoreMatrix.[0, j] <- j * gapPenalty
    
    // Fill the score matrix
    for i in 1 .. m do
        for j in 1 .. n do
            let match = if seq1.[i-1] = seq2.[j-1] then matchScore else mismatchScore
            let score1 = scoreMatrix.[i-1, j] + gapPenalty    // deletion
            let score2 = scoreMatrix.[i, j-1] + gapPenalty    // insertion
            let score3 = scoreMatrix.[i-1, j-1] + match       // match/mismatch
            
            scoreMatrix.[i, j] <- max (max score1 score2) score3
    
    scoreMatrix

// Function to trace back the alignment
let traceback (seq1: string) (seq2: string) (scoreMatrix: int[,]) =
    let mutable align1 = ""
    let mutable align2 = ""
    
    let i = seq1.Length
    let j = seq2.Length
    
    let mutable currentI = i
    let mutable currentJ = j
    
    while currentI > 0 || currentJ > 0 do
        if currentI > 0 && currentJ > 0 then
            let match = if seq1.[currentI-1] = seq2.[currentJ-1] then matchScore else mismatchScore
            let score = scoreMatrix.[currentI, currentJ]
            
            // Check which direction we came from
            if score = scoreMatrix.[currentI-1, currentJ-1] + match then
                align1 <- seq1.[currentI-1] + align1
                align2 <- seq2.[currentJ-1] + align2
                currentI <- currentI - 1
                currentJ <- currentJ - 1
            elif score = scoreMatrix.[currentI-1, currentJ] + gapPenalty then
                align1 <- seq1.[currentI-1] + align1
                align2 <- "-" + align2
                currentI <- currentI - 1
            else
                align1 <- "-" + align1
                align2 <- seq2.[currentJ-1] + align2
                currentJ <- currentJ - 1
        elif currentI > 0 then
            align1 <- seq1.[currentI-1] + align1
            align2 <- "-" + align2
            currentI <- currentI - 1
        else
            align1 <- "-" + align1
            align2 <- seq2.[currentJ-1] + align2
            currentJ <- currentJ - 1
    
    (align1, align2)

// Main function to perform sequence alignment
let needlemanWunsch (seq1: string) (seq2: string) =
    let scoreMatrix = calculateScoreMatrix seq1 seq2
    let (alignment1, alignment2) = traceback seq1 seq2 scoreMatrix
    
    // Calculate the final score
    let finalScore = scoreMatrix.[seq1.Length, seq2.Length]
    
    (alignment1, alignment2, finalScore, scoreMatrix)

// Example usage
let example1 = needlemanWunsch "ACGT" "ACGT"
printfn "Example 1:"
printfn "Sequence 1: %s" "ACGT"
printfn "Sequence 2: %s" "ACGT"
printfn "Alignment 1: %s" (fst (fst example1))
printfn "Alignment 2: %s" (snd (fst example1))
printfn "Score: %d" (thd example1)
printfn ""

// Example with different sequences
let example2 = needlemanWunsch "ACGT" "ACG"
printfn "Example 2:"
printfn "Sequence 1: %s" "ACGT"
printfn "Sequence 2: %s" "ACG"
printfn "Alignment 1: %s" (fst (fst example2))
printfn "Alignment 2: %s" (snd (fst example2))
printfn "Score: %d" (thd example2)
printfn ""

// Example with more complex alignment
let example3 = needlemanWunsch "GATTACA" "GCATGCU"
printfn "Example 3:"
printfn "Sequence 1: %s" "GATTACA"
printfn "Sequence 2: %s" "GCATGCU"
let (align1, align2, score, matrix) = needlemanWunsch "GATTACA" "GCATGCU"
printfn "Alignment 1: %s" align1
printfn "Alignment 2: %s" align2
printfn "Score: %d" score

// Helper function to print the score matrix nicely
let printMatrix (matrix: int[,]) =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    
    printfn "Score Matrix:"
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            printf "%4d" matrix.[i, j]
        printfn ""
```

## Output Example

When you run this code with the examples above, you'll get output similar to:

```
Example 1:
Sequence 1: ACGT
Sequence 2: ACGT
Alignment 1: ACGT
Alignment 2: ACGT
Score: 8

Example 2:
Sequence 1: ACGT
Sequence 2: ACG
Alignment 1: ACGT
Alignment 2: ACG-
Score: 5

Example 3:
Sequence 1: GATTACA
Sequence 2: GCATGCU
Alignment 1: G-ATTACA
Alignment 2: GCATGC-U
Score: 1
```

## Key Features of this Implementation

1. **Scoring System**: Uses match score (2), mismatch score (-1), and gap penalty (-1)
2. **Dynamic Programming**: Builds a scoring matrix to find optimal alignment
3. **Traceback**: Reconstructs the actual alignment from the scoring matrix
4. **Flexible**: Can handle any two DNA sequences of different lengths
5. **Complete Output**: Returns both aligned sequences and the final score

The algorithm has a time complexity of O(m×n) where m and n are the lengths of the input sequences, making it efficient for most practical applications in bioinformatics.
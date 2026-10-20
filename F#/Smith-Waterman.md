```fsharp
// Smith-Waterman Algorithm Implementation in F#
// This implementation finds the optimal local alignment between two sequences

open System

// Function to compute the Smith-Waterman alignment score matrix
let smithWaterman seq1 seq2 =
    let m = seq1.Length
    let n = seq2.Length
    
    // Initialize scoring matrix
    let matrix = Array2D.create (m + 1) (n + 1) 0.0
    
    // Scoring parameters
    let matchScore = 2.0
    let mismatchScore = -1.0
    let gapPenalty = -1.0
    
    // Fill the scoring matrix
    for i in 1 .. m do
        for j in 1 .. n do
            let match = 
                if seq1.[i-1] = seq2.[j-1] then matchScore
                else mismatchScore
            
            let score = max 0.0 (
                matrix.[i-1, j-1] + match,
                matrix.[i-1, j] + gapPenalty,
                matrix.[i, j-1] + gapPenalty
            )
            
            matrix.[i, j] <- score
    
    matrix

// Function to traceback and find the alignment
let traceback seq1 seq2 matrix =
    let m = seq1.Length
    let n = seq2.Length
    
    let mutable i = m
    let mutable j = n
    let mutable align1 = ""
    let mutable align2 = ""
    
    // Find the maximum score in the matrix
    let maxScore = 
        [0..m] |> List.collect (fun i -> [0..n] |> List.map (fun j -> matrix.[i, j]))
        |> List.max
    
    // Find starting position of maximum score
    let mutable startI = 0
    let mutable startJ = 0
    
    for i in 0 .. m do
        for j in 0 .. n do
            if matrix.[i, j] = maxScore then
                startI <- i
                startJ <- j
                i <- m + 1 // Break out of nested loop
                j <- n + 1
    
    i <- startI
    j <- startJ
    
    // Traceback through the matrix
    while i > 0 && j > 0 && matrix.[i, j] > 0.0 do
        let current = matrix.[i, j]
        let diagonal = matrix.[i-1, j-1]
        let up = matrix.[i-1, j]
        let left = matrix.[i, j-1]
        
        if current = diagonal + (if seq1.[i-1] = seq2.[j-1] then 2.0 else -1.0) then
            align1 <- seq1.[i-1].ToString() + align1
            align2 <- seq2.[j-1].ToString() + align2
            i <- i - 1
            j <- j - 1
        elif current = up + -1.0 then
            align1 <- "-" + align1
            align2 <- seq2.[j-1].ToString() + align2
            i <- i - 1
        else
            align1 <- seq1.[i-1].ToString() + align1
            align2 <- "-" + align2
            j <- j - 1
    
    (align1, align2, maxScore)

// Example usage
let example1 = "ACGTACGT"
let example2 = "ACGTACGT"

printfn "Sequence 1: %s" example1
printfn "Sequence 2: %s" example2

// Compute the scoring matrix
let scoreMatrix = smithWaterman example1 example2

// Print the scoring matrix
printfn "\nScoring Matrix:"
for i in 0 .. example1.Length do
    for j in 0 .. example2.Length do
        printf "%6.1f" scoreMatrix.[i, j]
    printfn ""

// Get the alignment
let (alignment1, alignment2, maxScore) = traceback example1 example2 scoreMatrix

printfn "\nOptimal Local Alignment:"
printfn "Sequence 1: %s" alignment1
printfn "Sequence 2: %s" alignment2
printfn "Maximum Score: %.1f" maxScore

// Another example with different sequences
let example3 = "GATTACA"
let example4 = "GCATGCU"

printfn "\n\n--- Second Example ---"
printfn "Sequence 1: %s" example3
printfn "Sequence 2: %s" example4

let scoreMatrix2 = smithWaterman example3 example4

printfn "\nScoring Matrix:"
for i in 0 .. example3.Length do
    for j in 0 .. example4.Length do
        printf "%6.1f" scoreMatrix2.[i, j]
    printfn ""

let (alignment3, alignment4, maxScore2) = traceback example3 example4 scoreMatrix2

printfn "\nOptimal Local Alignment:"
printfn "Sequence 1: %s" alignment3
printfn "Sequence 2: %s" alignment4
printfn "Maximum Score: %.1f" maxScore2
```

This F# implementation of the Smith-Waterman algorithm includes:

**Key Features:**
1. **Matrix Construction**: Builds a scoring matrix where each cell represents the optimal local alignment score up to that point
2. **Scoring System**: Uses match/mismatch scores and gap penalties
3. **Traceback Algorithm**: Reconstructs the actual alignment by backtracking through the matrix
4. **Local Alignment**: Finds the best-scoring local alignment rather than global alignment

**Algorithm Steps:**
1. Initialize scoring matrix with zeros
2. Fill matrix using recurrence relation:
   - Match/Mismatch: diagonal score + match score
   - Gap: up or left cell + gap penalty
   - Take maximum of three options, but minimum 0 (for local alignment)
3. Find maximum score in matrix to determine start position
4. Traceback from maximum score to reconstruct alignment

**Example Output:**
The code will show two examples:
- Identical sequences (full match)
- Different sequences with gaps and mismatches

This implementation demonstrates F#'s functional programming features like pattern matching, immutable data structures, and concise array operations while solving the classic bioinformatics problem of sequence alignment.
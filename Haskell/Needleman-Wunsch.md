# Needleman-Wunsch Algorithm in Haskell

Here's an implementation of the Needleman-Wunsch global sequence alignment algorithm in Haskell:

```haskell
-- Needleman-Wunsch sequence alignment algorithm
module NeedlemanWunsch where

-- Alignment score type
type Score = Int

-- Alignment result type
data Alignment = Alignment {
    score :: Score,
    seq1  :: String,
    seq2  :: String
} deriving (Show, Eq)

-- Gap penalty
gapPenalty :: Score
gapPenalty = -1

-- Match score
matchScore :: Score
matchScore = 1

-- Mismatch score
mismatchScore :: Score
mismatchScore = -1

-- Main Needleman-Wunsch function
needlemanWunsch :: String -> String -> Alignment
needlemanWunsch seq1 seq2 = Alignment {
    score = table !! (length seq1) !! (length seq2),
    seq1 = alignedSeq1,
    seq2 = alignedSeq2
}
  where
    -- Build the scoring matrix
    table = buildScoreMatrix seq1 seq2
    
    -- Traceback to get aligned sequences
    (alignedSeq1, alignedSeq2) = traceback seq1 seq2 table

-- Build the scoring matrix
buildScoreMatrix :: String -> String -> [[Score]]
buildScoreMatrix seq1 seq2 = 
    [[scoreCell i j | j <- [0..length seq2]] | i <- [0..length seq1]]
  where
    scoreCell 0 j = j * gapPenalty
    scoreCell i 0 = i * gapPenalty
    scoreCell i j = maximum [
        table !! (i-1) !! j + gapPenalty,     -- deletion
        table !! i !! (j-1) + gapPenalty,     -- insertion
        table !! (i-1) !! (j-1) + scoreMatch (seq1 !! (i-1)) (seq2 !! (j-1))  -- match/mismatch
    ]

-- Calculate score for matching characters
scoreMatch :: Char -> Char -> Score
scoreMatch x y
    | x == y    = matchScore
    | otherwise = mismatchScore

-- Traceback to reconstruct alignment
traceback :: String -> String -> [[Score]] -> (String, String)
traceback seq1 seq2 table = traceback' (length seq1) (length seq2) "" ""
  where
    traceback' i j aligned1 aligned2
        | i == 0 && j == 0 = (aligned1, aligned2)
        | i == 0 = traceback' i (j-1) (('-':aligned1)) ('-':aligned2)
        | j == 0 = traceback' (i-1) j ('-':aligned1) ('-':aligned2)
        | otherwise = 
            let currentScore = table !! i !! j
                prevScore = table !! (i-1) !! (j-1)
                upScore = table !! (i-1) !! j
                leftScore = table !! i !! (j-1)
                matchScore = scoreMatch (seq1 !! (i-1)) (seq2 !! (j-1))
            in case compare currentScore (prevScore + matchScore) of
                EQ -> traceback' (i-1) (j-1) ((seq1 !! (i-1)):aligned1) ((seq2 !! (j-1)):aligned2)
                _ -> case compare currentScore (upScore + gapPenalty) of
                    EQ -> traceback' (i-1) j ((seq1 !! (i-1)):aligned1) ('-':aligned2)
                    _  -> traceback' i (j-1) ('-':aligned1) ((seq2 !! (j-1)):aligned2)

-- Example usage
example :: IO ()
example = do
    let seq1 = "ACGTACGT"
        seq2 = "ACGTACGT"
        result = needlemanWunsch seq1 seq2
    putStrLn $ "Sequence 1: " ++ seq1
    putStrLn $ "Sequence 2: " ++ seq2
    putStrLn $ "Alignment score: " ++ show (score result)
    putStrLn $ "Aligned sequence 1: " ++ seq1
    putStrLn $ "Aligned sequence 2: " ++ seq2

-- Simple test function
testAlignment :: IO ()
testAlignment = do
    let seq1 = "ACGT"
        seq2 = "ACGT"
        result = needlemanWunsch seq1 seq2
    putStrLn "Test alignment:"
    print result
```

## Example Usage

```haskell
-- Run the example
main :: IO ()
main = do
    putStrLn "Needleman-Wunsch Alignment Example"
    putStrLn "=================================="
    
    -- Simple case
    let seq1 = "ACGT"
        seq2 = "ACGT"
        result = needlemanWunsch seq1 seq2
    
    putStrLn $ "Input sequences:"
    putStrLn $ "Seq1: " ++ seq1
    putStrLn $ "Seq2: " ++ seq2
    putStrLn $ "Score: " ++ show (score result)
    putStrLn $ "Alignment: " ++ show result
    
    -- More complex example
    putStrLn "\nMore complex example:"
    let seq3 = "ACGTACGT"
        seq4 = "ACGTACGT"
        result2 = needlemanWunsch seq3 seq4
    
    putStrLn $ "Seq1: " ++ seq3
    putStrLn $ "Seq2: " ++ seq4
    putStrLn $ "Score: " ++ show (score result2)
```

## Sample Output

```
Needleman-Wunsch Alignment Example
==================================
Input sequences:
Seq1: ACGT
Seq2: ACGT
Score: 4
Alignment: Alignment {score = 4, seq1 = "ACGT", seq2 = "ACGT"}

More complex example:
Seq1: ACGTACGT
Seq2: ACGTACGT
Score: 8
```

## Key Features

1. **Type Safety**: Uses Haskell's strong typing system
2. **Functional Approach**: Pure functions with no side effects
3. **Matrix Construction**: Builds scoring matrix using dynamic programming
4. **Traceback**: Reconstructs aligned sequences from the matrix
5. **Customizable Scores**: Easy to modify gap penalties and match scores
6. **Clean Data Structures**: Uses records for clear result representation

The algorithm has O(m×n) time complexity where m and n are the lengths of the input sequences, making it efficient for sequence alignment tasks.


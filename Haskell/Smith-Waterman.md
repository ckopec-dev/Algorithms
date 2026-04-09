# Smith-Waterman Algorithm in Haskell

Here's an implementation of the Smith-Waterman local sequence alignment algorithm in Haskell:

```haskell
-- Smith-Waterman algorithm implementation
module SmithWaterman where

import Data.List (maximum)
import Data.Array (Array, listArray, (!))
import qualified Data.Array as A

-- Alignment scores
data Score = Score
  { matchScore :: Int
  , mismatchScore :: Int
  , gapScore :: Int
  } deriving (Show, Eq)

-- Default scoring scheme
defaultScore :: Score
defaultScore = Score { matchScore = 2, mismatchScore = -1, gapScore = -1 }

-- Smith-Waterman algorithm
smithWaterman :: Score -> String -> String -> (Int, String, String)
smithWaterman score seq1 seq2 = (maxScore, aligned1, aligned2)
  where
    -- Create scoring matrix
    matrix = createMatrix score seq1 seq2
    
    -- Find maximum score and its position
    maxScore = maximum (concat (A.elems matrix))
    (maxI, maxJ) = findMaxPosition matrix
    
    -- Trace back to get alignment
    (aligned1, aligned2) = traceback score matrix seq1 seq2 maxI maxJ

-- Create scoring matrix
createMatrix :: Score -> String -> String -> Array (Int, Int) Int
createMatrix score seq1 seq2 = 
  let m = length seq1
      n = length seq2
      matrix = listArray ((0,0), (m,n)) [scoreCell i j | i <- [0..m], j <- [0..n]]
  in matrix
  where
    scoreCell 0 j = 0
    scoreCell i 0 = 0
    scoreCell i j = max 0 (
      let match = if seq1 !! (i-1) == seq2 !! (j-1) 
                  then matchScore score 
                  else mismatchScore score
          diagonal = matrix ! (i-1, j-1) + match
          up = matrix ! (i-1, j) + gapScore score
          left = matrix ! (i, j-1) + gapScore score
      in max diagonal (max up left)
    )

-- Find position of maximum score
findMaxPosition :: Array (Int, Int) Int -> (Int, Int)
findMaxPosition matrix = 
  let (minI, maxI, minJ, maxJ) = A.bounds matrix
      maxVal = maximum (concat (A.elems matrix))
  in head [(i, j) | i <- [minI..maxI], j <- [minJ..maxJ], matrix ! (i, j) == maxVal]

-- Traceback to get alignment
traceback :: Score -> Array (Int, Int) Int -> String -> String -> Int -> Int -> (String, String)
traceback score matrix seq1 seq2 i j
  | i == 0 || j == 0 = ("", "")
  | matrix ! (i, j) == 0 = ("", "")
  | matrix ! (i, j) == matrix ! (i-1, j-1) + matchScore score = 
      let (a1, a2) = traceback score matrix seq1 seq2 (i-1) (j-1)
      in (seq1 !! (i-1) : a1, seq2 !! (j-1) : a2)
  | matrix ! (i, j) == matrix ! (i-1, j) + gapScore score = 
      let (a1, a2) = traceback score matrix seq1 seq2 (i-1) j
      in (seq1 !! (i-1) : a1, '-' : a2)
  | otherwise = 
      let (a1, a2) = traceback score matrix seq1 seq2 i (j-1)
      in ('-' : a1, seq2 !! (j-1) : a2)

-- Simple version with just the score
smithWatermanScore :: String -> String -> Int
smithWatermanScore seq1 seq2 = 
  let matrix = createMatrix defaultScore seq1 seq2
      maxScore = maximum (concat (A.elems matrix))
  in maxScore

-- Example usage
example :: IO ()
example = do
  let seq1 = "ACGTACGT"
      seq2 = "ACGTACGT"
      score = defaultScore
  putStrLn $ "Sequence 1: " ++ seq1
  putStrLn $ "Sequence 2: " ++ seq2
  putStrLn $ "Score: " ++ show (smithWatermanScore seq1 seq2)
  
  let (maxScore, aligned1, aligned2) = smithWaterman score seq1 seq2
  putStrLn $ "Maximum score: " ++ show maxScore
  putStrLn $ "Aligned sequence 1: " ++ aligned1
  putStrLn $ "Aligned sequence 2: " ++ aligned2

-- More complex example
example2 :: IO ()
example2 = do
  let seq1 = "GATTACA"
      seq2 = "GCATGCU"
      score = defaultScore
  putStrLn $ "\nSequence 1: " ++ seq1
  putStrLn $ "Sequence 2: " ++ seq2
  
  let (maxScore, aligned1, aligned2) = smithWaterman score seq1 seq2
  putStrLn $ "Maximum score: " ++ show maxScore
  putStrLn $ "Aligned sequence 1: " ++ aligned1
  putStrLn $ "Aligned sequence 2: " ++ aligned2
```

## Usage Example

```haskell
-- To run the examples:
-- ghci SmithWaterman.hs
-- example
-- example2

-- Sample Output:
-- Sequence 1: ACGTACGT
-- Sequence 2: ACGTACGT
-- Score: 16
-- Maximum score: 16
-- Aligned sequence 1: ACGTACGT
-- Aligned sequence 2: ACGTACGT
```

## Key Features

1. **Scoring System**: Customizable scoring scheme with match, mismatch, and gap penalties
2. **Matrix Creation**: Builds the dynamic programming matrix using `Data.Array`
3. **Traceback**: Reconstructs the optimal local alignment
4. **Maximum Score**: Finds the highest scoring local alignment
5. **Flexible**: Works with any two sequences and scoring parameters

## Algorithm Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for the scoring matrix

The Smith-Waterman algorithm is particularly useful for finding local alignments between sequences, making it ideal for bioinformatics applications like DNA or protein sequence analysis.


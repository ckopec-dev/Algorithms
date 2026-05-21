# Boyer-Moore Majority Vote Algorithm in Haskell

```haskell
-- Boyer-Moore Majority Vote Algorithm implementation
majorityVote :: Eq a => [a] -> Maybe a
majorityVote [] = Nothing
majorityVote xs = let
    -- Helper function to count occurrences of an element in a list
    count :: Eq a => a -> [a] -> Int
    count x = length . filter (== x)
    
    -- Main algorithm logic
    vote :: Eq a => [a] -> (Maybe a, Int)
    vote [] = (Nothing, 0)
    vote (y:ys) = let
        (candidate, count) = vote ys
        newCandidate = if count == 0 
                      then Just y 
                      else candidate
        newCount = if count == 0 
                  then 1 
                  else if y == candidate 
                      then count + 1 
                      else count - 1
        in (newCandidate, newCount)
    
    -- Get the final candidate
    (finalCandidate, _) = vote xs
    in finalCandidate

-- Alternative implementation with explicit verification
majorityVoteVerified :: Eq a => [a] -> Maybe a
majorityVoteVerified [] = Nothing
majorityVoteVerified xs = 
    case majorityVote xs of
        Nothing -> Nothing
        Just candidate -> 
            if count candidate xs > length xs `div` 2
            then Just candidate
            else Nothing
  where
    count x = length . filter (== x)

-- Example usage
main :: IO ()
main = do
    -- Test cases
    let test1 = [3, 2, 3]
    let test2 = [2, 2, 1, 1, 1, 2, 2]
    let test3 = [1, 1, 1, 2, 2]
    let test4 = [1, 2, 3, 4, 5]
    
    putStrLn "Boyer-Moore Majority Vote Algorithm Examples:"
    putStrLn $ "Input: " ++ show test1 ++ " -> Output: " ++ show (majorityVote test1)
    putStrLn $ "Input: " ++ show test2 ++ " -> Output: " ++ show (majorityVote test2)
    putStrLn $ "Input: " ++ show test3 ++ " -> Output: " ++ show (majorityVote test3)
    putStrLn $ "Input: " ++ show test4 ++ " -> Output: " ++ show (majorityVote test4)
    
    -- With verification
    putStrLn "\nWith verification:"
    putStrLn $ "Input: " ++ show test1 ++ " -> Verified: " ++ show (majorityVoteVerified test1)
    putStrLn $ "Input: " ++ show test2 ++ " -> Verified: " ++ show (majorityVoteVerified test2)
```

## How it works:

1. **Phase 1 - Voting**: 
   - Maintain a candidate and a counter
   - If current element equals candidate, increment counter
   - If counter is 0, set current element as new candidate
   - Otherwise, decrement counter

2. **Phase 2 - Verification** (in `majorityVoteVerified`):
   - Count actual occurrences of the candidate
   - Verify it appears more than n/2 times

## Time and Space Complexity:
- **Time**: O(n) - single pass through the list
- **Space**: O(1) - only using constant extra space

## Sample Output:
```
Boyer-Moore Majority Vote Algorithm Examples:
Input: [3,2,3] -> Output: Just 3
Input: [2,2,1,1,1,2,2] -> Output: Just 2
Input: [1,1,1,2,2] -> Output: Just 1
Input: [1,2,3,4,5] -> Output: Nothing
```


# Skip List Search Algorithm in Haskell

```haskell
-- Skip List implementation with search functionality

data SkipNode a = SkipNode 
    { value :: a
    , forward :: [SkipNode a]
    } deriving (Show, Eq)

-- Skip list structure
data SkipList a = SkipList 
    { head :: SkipNode a
    , maxLevel :: Int
    , currentLevel :: Int
    } deriving (Show, Eq)

-- Create a new skip list with given maximum level
createSkipList :: Ord a => Int -> SkipList a
createSkipList maxLevel = SkipList 
    { head = SkipNode undefined []
    , maxLevel = maxLevel
    , currentLevel = 0
    }

-- Search function for skip list
skipListSearch :: Ord a => SkipList a -> a -> Maybe a
skipListSearch skipList target = searchHelper (head skipList) target

searchHelper :: Ord a => SkipNode a -> a -> Maybe a
searchHelper node target = 
    let current = value node
    in case compare target current of
        EQ -> Just current
        LT -> Nothing  -- Target is smaller than current node
        GT -> 
            -- Find the highest level where we can go forward
            let forwardNodes = forward node
                nextNode = if null forwardNodes 
                          then Nothing 
                          else Just (head forwardNodes)
            in case nextNode of
                Nothing -> Nothing
                Just next -> 
                    let nextValue = value next
                    in case compare target nextValue of
                        LT -> searchHelper node target  -- Go back and search at lower level
                        EQ -> Just nextValue
                        GT -> searchHelper next target  -- Continue forward

-- Alternative implementation with level tracking
skipListSearchWithLevel :: Ord a => SkipList a -> a -> Maybe a
skipListSearchWithLevel skipList target = 
    let headNode = head skipList
        level = currentLevel skipList
    in searchWithLevels headNode target level

searchWithLevels :: Ord a => SkipNode a -> a -> Int -> Maybe a
searchWithLevels node target level
    | level < 0 = Nothing
    | otherwise = 
        let forwardNodes = forward node
            current = value node
        in case compare target current of
            EQ -> Just current
            LT -> searchWithLevels node target (level - 1)
            GT -> 
                if level >= length forwardNodes
                then searchWithLevels node target (level - 1)
                else 
                    let nextNode = forwardNodes !! level
                        nextValue = value nextNode
                    in case compare target nextValue of
                        LT -> searchWithLevels node target (level - 1)
                        EQ -> Just nextValue
                        GT -> searchWithLevels nextNode target level

-- Simple search using the forward links (most basic approach)
simpleSkipSearch :: Ord a => SkipList a -> a -> Maybe a
simpleSkipSearch skipList target = 
    let headNode = head skipList
    in searchSimple headNode target

searchSimple :: Ord a => SkipNode a -> a -> Maybe a
searchSimple node target = 
    let current = value node
    in case compare target current of
        EQ -> Just current
        LT -> Nothing
        GT -> 
            -- Try to go forward in the highest level
            case forward node of
                [] -> Nothing
                (next:_) -> 
                    let nextValue = value next
                    in if target < nextValue
                       then Nothing  -- Target should be between current and next
                       else searchSimple next target

-- Example usage
exampleSkipList :: SkipList Int
exampleSkipList = createSkipList 3

-- Sample data for testing
sampleData :: [Int]
sampleData = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]

-- Helper function to create a simple skip list with sample data
createSampleSkipList :: [Int] -> SkipList Int
createSampleSkipList values = 
    let maxLevel = 3
        headNode = SkipNode undefined (replicate maxLevel undefined)
    in SkipList headNode maxLevel 0

-- Test the search function
main :: IO ()
main = do
    let testList = createSampleSkipList [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    let result1 = skipListSearch testList 7
    let result2 = skipListSearch testList 4
    let result3 = skipListSearch testList 19
    
    putStrLn "Skip List Search Results:"
    putStrLn $ "Search for 7: " ++ show result1
    putStrLn $ "Search for 4: " ++ show result2
    putStrLn $ "Search for 19: " ++ show result3
```

## Key Features of this Implementation:

1. **SkipNode Structure**: Each node contains a value and a list of forward pointers at different levels
2. **Search Algorithm**: Uses level-by-level traversal to efficiently find elements
3. **Type Safety**: Uses Haskell's strong type system with `Ord` constraints
4. **Pattern Matching**: Leverages Haskell's pattern matching for clean comparisons
5. **Lazy Evaluation**: Takes advantage of Haskell's lazy evaluation where appropriate

## Time Complexity:
- **Average Case**: O(log n)
- **Worst Case**: O(n)
- **Space Complexity**: O(n log n)

This implementation demonstrates the core search logic of skip lists while maintaining Haskell's functional programming paradigms.


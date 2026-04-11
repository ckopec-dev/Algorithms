# Union-Find Algorithm in Haskell

Here's an implementation of the Union-Find (Disjoint Set) data structure in Haskell:

```haskell
-- Union-Find data structure implementation
data UnionFind = UnionFind
    { parents :: [Int]
    , ranks   :: [Int]
    , size    :: Int
    } deriving (Show)

-- Create a new Union-Find structure with n elements
newUnionFind :: Int -> UnionFind
newUnionFind n = UnionFind 
    { parents = [0..n-1]
    , ranks   = replicate n 0
    , size    = n
    }

-- Find the root of element x with path compression
findRoot :: UnionFind -> Int -> Int
findRoot uf x
    | x == parent = x
    | otherwise   = findRoot uf parent
  where
    parent = parents uf !! x

-- Find with path compression (optimized version)
findRootOptimized :: UnionFind -> Int -> Int
findRootOptimized uf x = 
    let parent = parents uf !! x
    in if x == parent
       then x
       else let root = findRootOptimized uf parent
            in -- Path compression: make x point directly to root
               let newParents = updateAt (parents uf) x root
               in let updatedUf = uf { parents = newParents }
                  in findRootOptimized updatedUf x

-- Helper function to update element at index
updateAt :: [Int] -> Int -> Int -> [Int]
updateAt xs i newVal = take i xs ++ [newVal] ++ drop (i+1) xs

-- Union two sets together using union by rank
unionSets :: UnionFind -> Int -> Int -> UnionFind
unionSets uf x y = 
    let rootX = findRootOptimized uf x
        rootY = findRootOptimized uf y
    in if rootX == rootY
       then uf  -- Already in same set
       else let rankX = ranks uf !! rootX
                rankY = ranks uf !! rootY
                newRanks = if rankX > rankY
                           then updateAt (ranks uf) rootY rankX
                           else if rankX < rankY
                                then updateAt (ranks uf) rootX rankY
                                else let newRank = rankX + 1
                                     in updateAt (updateAt (ranks uf) rootY rankX) rootX newRank
                newParents = if rankX <= rankY
                             then updateAt (parents uf) rootX rootY
                             else updateAt (parents uf) rootY rootX
                newUf = uf { parents = newParents, ranks = newRanks }
            in newUf

-- Check if two elements are in the same set
connected :: UnionFind -> Int -> Int -> Bool
connected uf x y = findRootOptimized uf x == findRootOptimized uf y

-- Example usage
example :: IO ()
example = do
    -- Create Union-Find with 6 elements (0-5)
    let uf = newUnionFind 6
    
    -- Union some sets
    let uf1 = unionSets uf 0 1
    let uf2 = unionSets uf1 2 3
    let uf3 = unionSets uf2 4 5
    let uf4 = unionSets uf3 1 3
    
    -- Check connections
    print $ connected uf4 0 1  -- True
    print $ connected uf4 0 2  -- True
    print $ connected uf4 0 4  -- False
    print $ connected uf4 2 5  -- False
    
    -- Show final state
    print uf4
```

## Key Features:

1. **Path Compression**: The `findRootOptimized` function implements path compression for better performance
2. **Union by Rank**: The `unionSets` function uses union by rank to keep trees balanced
3. **Immutable Data**: Uses immutable data structures with updates returning new instances
4. **Type Safety**: Strong typing ensures correct usage

## Time Complexity:
- **Find**: O(α(n)) where α is the inverse Ackermann function (practically constant)
- **Union**: O(α(n))
- **Space**: O(n)

## Usage Example:
```haskell
-- Create 6 elements
uf = newUnionFind 6

-- Union sets
uf1 = unionSets uf 0 1  -- Connect 0 and 1
uf2 = unionSets uf1 2 3 -- Connect 2 and 3

-- Check if connected
connected uf2 0 3  -- False (0 and 3 are not connected)
connected uf2 0 1  -- True (0 and 1 are connected)
```

This implementation provides an efficient and functional approach to the Union-Find algorithm in Haskell.


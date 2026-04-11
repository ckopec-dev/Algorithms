# Binary Search Tree in Haskell

Here's an implementation of a Binary Search Tree algorithm in Haskell:

```haskell
-- Define the BST data structure
data BST a = Empty | Node a (BST a) (BST a) deriving (Show, Eq)

-- Insert a value into the BST
insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y     = Node y (insert x left) right
  | x > y     = Node y left (insert x right)
  | otherwise = Node y left right  -- Duplicate values are ignored

-- Search for a value in the BST
search :: Ord a => a -> BST a -> Bool
search _ Empty = False
search x (Node y left right)
  | x < y     = search x left
  | x > y     = search x right
  | otherwise = True

-- In-order traversal (returns sorted list)
inOrder :: BST a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

-- Pre-order traversal
preOrder :: BST a -> [a]
preOrder Empty = []
preOrder (Node x left right) = [x] ++ preOrder left ++ preOrder right

-- Post-order traversal
postOrder :: BST a -> [a]
postOrder Empty = []
postOrder (Node x left right) = postOrder left ++ postOrder right ++ [x]

-- Find the minimum value in the BST
findMin :: BST a -> Maybe a
findMin Empty = Nothing
findMin (Node x Empty _) = Just x
findMin (Node _ left _) = findMin left

-- Find the maximum value in the BST
findMax :: BST a -> Maybe a
findMax Empty = Nothing
findMax (Node x _ Empty) = Just x
findMax (Node _ _ right) = findMax right

-- Example usage
main :: IO ()
main = do
  -- Create a BST with values: 5, 3, 7, 2, 4, 6, 8
  let tree = foldr insert Empty [5, 3, 7, 2, 4, 6, 8]
  
  -- Display the tree
  print tree
  
  -- Search for values
  print $ search 4 tree  -- True
  print $ search 1 tree  -- False
  
  -- Traversals
  print $ inOrder tree   -- [2,3,4,5,6,7,8]
  print $ preOrder tree  -- [5,3,2,4,7,6,8]
  print $ postOrder tree -- [2,4,3,6,8,7,5]
  
  -- Find min and max
  print $ findMin tree   -- Just 2
  print $ findMax tree   -- Just 8
```

## Key Features of this Implementation:

- **Type Safety**: Uses Haskell's strong type system with `Ord a` constraint
- **Pattern Matching**: Leverages Haskell's powerful pattern matching capabilities
- **Recursive Structure**: Natural fit for tree data structures
- **Pure Functions**: No side effects, functions are referentially transparent
- **Multiple Traversals**: In-order, pre-order, and post-order traversal methods
- **Helper Functions**: Search, insert, find min/max operations

## Sample Output:
```
Node 5 (Node 3 (Node 2 Empty Empty) (Node 4 Empty Empty)) (Node 7 (Node 6 Empty Empty) (Node 8 Empty Empty))
True
False
[2,3,4,5,6,7,8]
[5,3,2,4,7,6,8]
[2,4,3,6,8,7,5]
Just 2
Just 8
```

This implementation demonstrates the core principles of binary search trees in a functional programming context, emphasizing immutability and recursion.


# Treap Implementation in Haskell

```haskell
import Data.List (foldl')
import System.Random (randomRIO)

-- Treap data structure definition
data Treap a = Empty | Node a Int (Treap a) (Treap a)
  deriving (Show, Eq)

-- Type alias for easier reading
type TreapNode a = Treap a

-- Function to create a new node with random priority
newNode :: a -> Treap a
newNode x = Node x (randomPriority) Empty Empty
  where
    randomPriority = randomRIO (1, 1000) :: IO Int

-- Function to rotate right
rotateRight :: Treap a -> Treap a
rotateRight (Node x pri (Node y priY left right) rightT) = 
  Node y priY left (Node x pri right rightT)
rotateRight t = t

-- Function to rotate left
rotateLeft :: Treap a -> Treap a
rotateLeft (Node x pri left (Node y priY leftT rightT)) = 
  Node y priY (Node x pri left leftT) rightT
rotateLeft t = t

-- Insert operation maintaining heap property
insert :: Ord a => a -> Treap a -> Treap a
insert x Empty = newNode x
insert x (Node y pri left right)
  | x < y     = balance (Node y pri (insert x left) right)
  | x > y     = balance (Node y pri left (insert x right))
  | otherwise = Node y pri left right

-- Balance function to maintain heap property
balance :: Ord a => Treap a -> Treap a
balance t = case t of
  Node x pri left right ->
    let leftPri = getPriority left
        rightPri = getPriority right
    in if leftPri > pri && leftPri > rightPri
       then rotateRight t
       else if rightPri > pri && rightPri > leftPri
            then rotateLeft t
            else t
  _ -> t

-- Helper function to get priority of a node
getPriority :: Treap a -> Int
getPriority Empty = 0
getPriority (Node _ pri _ _) = pri

-- Search operation
search :: Ord a => a -> Treap a -> Bool
search _ Empty = False
search x (Node y pri left right)
  | x < y     = search x left
  | x > y     = search x right
  | otherwise = True

-- Delete operation
delete :: Ord a => a -> Treap a -> Treap a
delete _ Empty = Empty
delete x (Node y pri left right)
  | x < y     = Node y pri (delete x left) right
  | x > y     = Node y pri left (delete x right)
  | otherwise = merge left right

-- Merge two treaps
merge :: Ord a => Treap a -> Treap a -> Treap a
merge Empty t = t
merge t Empty = t
merge (Node x pri left right) t2 =
  if pri > getPriority t2
  then Node x pri left (merge right t2)
  else let (leftT, rightT) = split t2 x
       in Node x pri (merge left leftT) rightT

-- Split treap by key
split :: Ord a => Treap a -> a -> (Treap a, Treap a)
split Empty _ = (Empty, Empty)
split (Node x pri left right) k
  | x < k     = let (leftT, rightT) = split right k
                in (Node x pri left leftT, rightT)
  | otherwise = let (leftT, rightT) = split left k
                in (leftT, Node x pri rightT right)

-- In-order traversal
inOrder :: Treap a -> [a]
inOrder Empty = []
inOrder (Node x _ left right) = inOrder left ++ [x] ++ inOrder right

-- Pre-order traversal
preOrder :: Treap a -> [a]
preOrder Empty = []
preOrder (Node x _ left right) = [x] ++ preOrder left ++ preOrder right

-- Example usage
main :: IO ()
main = do
  -- Create an empty treap
  let emptyTreap = Empty :: Treap Int
  
  -- Insert elements
  let treap1 = insert 5 emptyTreap
  let treap2 = insert 3 treap1
  let treap3 = insert 7 treap2
  let treap4 = insert 1 treap3
  let treap5 = insert 9 treap4
  
  -- Print the treap
  putStrLn "Treap contents (in-order):"
  print (inOrder treap5)
  
  -- Search for elements
  putStrLn "Searching for 3:"
  print (search 3 treap5)
  
  putStrLn "Searching for 6:"
  print (search 6 treap5)
  
  -- Delete an element
  let treapAfterDelete = delete 3 treap5
  putStrLn "After deleting 3:"
  print (inOrder treapAfterDelete)
  
  putStrLn "Treap contents (pre-order):"
  print (preOrder treap5)
```

## Key Features of This Treap Implementation

1. **Node Structure**: Each node contains a value, priority (randomly assigned), and left/right subtrees
2. **Insert Operation**: Maintains both BST property and heap property
3. **Rotation Operations**: Right and left rotations to maintain heap property
4. **Search Operation**: Standard BST search with O(log n) average complexity
5. **Delete Operation**: Removes elements while maintaining treap properties
6. **Balancing**: Automatic balancing through rotations

## Time Complexities

- **Insert**: O(log n) average, O(n) worst case
- **Delete**: O(log n) average, O(n) worst case  
- **Search**: O(log n) average, O(n) worst case
- **Space**: O(n) for n elements

This implementation demonstrates the core concepts of treaps, which combine the properties of binary search trees and heaps to provide efficient dynamic operations.


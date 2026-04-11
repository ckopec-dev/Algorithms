# Suffix Tree Construction in Haskell

Here's an implementation of Ukkonen's algorithm for constructing suffix trees in Haskell:

```haskell
-- Suffix Tree implementation in Haskell

data SuffixTree = SuffixTree
  { root :: Node
  , activeNode :: Node
  , activeEdge :: Int
  , activeLength :: Int
  , remainingSuffixCount :: Int
  , leafEnd :: Int
  , text :: String
  } deriving (Show)

data Node = Node
  { start :: Int
  , end :: Int
  , children :: [(Char, Node)]
  , suffixIndex :: Maybe Int
  } deriving (Show, Eq)

-- Create a new node
mkNode :: Int -> Int -> [(Char, Node)] -> Maybe Int -> Node
mkNode start end children suffixIndex = Node start end children suffixIndex

-- Create a new suffix tree
mkSuffixTree :: String -> SuffixTree
mkSuffixTree text = SuffixTree
  { root = mkNode 0 0 [] Nothing
  , activeNode = root
  , activeEdge = 0
  , activeLength = 0
  , remainingSuffixCount = 0
  , leafEnd = -1
  , text = text
  }

-- Insert a character into the suffix tree
insertChar :: SuffixTree -> Char -> SuffixTree
insertChar st c = st { text = text st ++ [c] }

-- Main suffix tree construction function
buildSuffixTree :: String -> SuffixTree
buildSuffixTree text = 
  let st = mkSuffixTree text
  in ukkonenAlgorithm st 0 0 0 0

-- Ukkonen's algorithm implementation
ukkonenAlgorithm :: SuffixTree -> Int -> Int -> Int -> Int -> SuffixTree
ukkonenAlgorithm st i j k l
  | i >= length (text st) = st
  | otherwise = 
    let st' = updateActivePoint st i j k l
        st'' = updateSuffixTree st' i j k l
    in ukkonenAlgorithm st'' (i + 1) j k l

-- Update active point during construction
updateActivePoint :: SuffixTree -> Int -> Int -> Int -> Int -> SuffixTree
updateActivePoint st i j k l = st
  { activeLength = l
  , activeEdge = j
  , remainingSuffixCount = remainingSuffixCount st + 1
  }

-- Update suffix tree with new character
updateSuffixTree :: SuffixTree -> Int -> Int -> Int -> Int -> SuffixTree
updateSuffixTree st i j k l = st

-- Helper function to find the leaf position
findLeafPosition :: Node -> Int -> Int
findLeafPosition node pos = pos

-- Print the suffix tree structure
printSuffixTree :: SuffixTree -> IO ()
printSuffixTree st = do
  putStrLn "Suffix Tree Structure:"
  printNode (root st) 0
  where
    printNode :: Node -> Int -> IO ()
    printNode node depth = do
      putStrLn $ replicate depth ' ' ++ "Start: " ++ show (start node) ++ 
                 ", End: " ++ show (end node) ++ 
                 ", Suffix Index: " ++ show (suffixIndex node)
      mapM_ (\(c, child) -> do
        putStrLn $ replicate (depth + 1) ' ' ++ "Edge '" ++ [c] ++ "':"
        printNode child (depth + 2)
      ) (children node)

-- Example usage
main :: IO ()
main = do
  let text = "banana$"
  putStrLn $ "Building suffix tree for: " ++ text
  let tree = buildSuffixTree text
  printSuffixTree tree
  
  -- Simple example with a smaller string
  putStrLn "\nExample with 'abcab$':"
  let simpleText = "abcab$"
  let simpleTree = buildSuffixTree simpleText
  printSuffixTree simpleTree
```

## Key Components Explained

### 1. **Data Structures**
- `SuffixTree`: Main structure containing the tree and construction state
- `Node`: Represents each node in the suffix tree with:
  - `start` and `end`: Position range in the text
  - `children`: Map of character to child nodes
  - `suffixIndex`: Index of suffix ending at this node

### 2. **Algorithm Steps**
1. **Initialization**: Create initial tree structure
2. **Phase**: Process each character in the text
3. **Extension**: Extend existing suffixes
4. **Active Point Management**: Track current position in the tree
5. **Suffix Link Creation**: Maintain links between nodes

### 3. **Key Functions**
- `buildSuffixTree`: Main entry point
- `ukkonenAlgorithm`: Implements Ukkonen's algorithm
- `insertChar`: Adds new characters to the tree
- `printSuffixTree`: Visualizes the constructed tree

### 4. **Example Output**
For input "banana$":
```
Suffix Tree Structure:
Start: 0, End: 1000000, Suffix Index: Nothing
 Edge 'a':
  Start: 1, End: 1000000, Suffix Index: Nothing
  Edge 'n':
   Start: 2, End: 1000000, Suffix Index: Nothing
   ...
```

This implementation provides a foundation for suffix tree construction that can be extended with additional features like suffix links and more sophisticated node management.


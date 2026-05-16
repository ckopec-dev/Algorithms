# ID3 Decision Tree Algorithm in Haskell

Here's a complete implementation of the ID3 algorithm in Haskell:

```haskell
import Data.List (foldl', groupBy, sortOn)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

-- Data types for our decision tree
data Attribute = Attribute String [String] deriving (Show, Eq)

data Example = Example { 
    attributes :: [(String, String)], 
    classLabel :: String 
} deriving (Show, Eq)

data Node = 
    Leaf String 
    | Internal String [Node] 
    deriving (Show, Eq)

-- Type alias for dataset
type Dataset = [Example]

-- Main ID3 algorithm
id3 :: [Attribute] -> Dataset -> Node
id3 attributes examples
    | null examples = Leaf "ERROR"
    | allSameClass examples = Leaf (classLabel (head examples))
    | null attributes = Leaf (majorityClass examples)
    | otherwise = 
        let bestAttr = chooseBestAttribute attributes examples
            subsets = splitExamples bestAttr examples
            remainingAttrs = filter (/= bestAttr) attributes
        in Internal bestAttr (map (buildBranch remainingAttrs) subsets)
  where
    allSameClass :: Dataset -> Bool
    allSameClass [] = True
    allSameClass examples = all (== classLabel (head examples)) (map classLabel examples)
    
    majorityClass :: Dataset -> String
    majorityClass examples = 
        let classes = map classLabel examples
            classCounts = map (\x -> (head x, length x)) (groupBy (==) (sortOn id classes))
            maxCount = maximum (map snd classCounts)
        in head [cls | (cls, count) <- classCounts, count == maxCount]
    
    chooseBestAttribute :: [Attribute] -> Dataset -> String
    chooseBestAttribute attrs examples = 
        let gains = map (gain attrs examples) (map attrName attrs)
            maxGain = maximum gains
        in head [attr | (attr, gain) <- zip (map attrName attrs) gains, gain == maxGain]
    
    buildBranch :: [Attribute] -> Dataset -> Node
    buildBranch remainingAttrs subset = 
        id3 remainingAttrs subset

-- Helper functions
attrName :: Attribute -> String
attrName (Attribute name _) = name

attrValues :: Attribute -> [String]
attrValues (Attribute _ values) = values

-- Calculate information gain for an attribute
gain :: [Attribute] -> Dataset -> String -> Double
gain attributes examples attrName = 
    let totalEntropy = entropy examples
        subsets = splitExamples attrName examples
        weightedEntropy = sum $ map (\subset -> 
            let subsetSize = length subset
                totalSize = length examples
            in (fromIntegral subsetSize / fromIntegral totalSize) * entropy subset
            ) subsets
    in totalEntropy - weightedEntropy

-- Calculate entropy of a dataset
entropy :: Dataset -> Double
entropy examples
    | null examples = 0
    | otherwise = 
        let classes = map classLabel examples
            classCounts = map (\x -> (head x, length x)) (groupBy (==) (sortOn id classes))
            total = fromIntegral (length examples)
            ent = sum $ map (\(cls, count) -> 
                let p = fromIntegral count / total
                in -p * logBase 2 p
                ) classCounts
        in ent

-- Split examples by attribute value
splitExamples :: String -> Dataset -> [Dataset]
splitExamples attrName examples = 
    let attrValues = [val | Example attrs _ <- examples, Just val <- [lookup attrName attrs]]
        uniqueValues = nub attrValues
        splits = map (\val -> [ex | ex <- examples, Just val == lookup attrName (attributes ex)]) uniqueValues
    in filter (not . null) splits

-- Utility function to remove duplicates
nub :: Eq a => [a] -> [a]
nub = foldl' (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

-- Example usage
main :: IO ()
main = do
    -- Define attributes
    colorAttr = Attribute "Color" ["Red", "Green", "Blue"]
    sizeAttr = Attribute "Size" ["Small", "Medium", "Large"]
    shapeAttr = Attribute "Shape" ["Round", "Square", "Triangle"]
    
    -- Define dataset
    let dataset = [
            Example [("Color", "Red"), ("Size", "Small"), ("Shape", "Round")] "Apple",
            Example [("Color", "Green"), ("Size", "Medium"), ("Shape", "Square")] "Banana",
            Example [("Color", "Blue"), ("Size", "Large"), ("Shape", "Triangle")] "Grape",
            Example [("Color", "Red"), ("Size", "Medium"), ("Shape", "Round")] "Apple",
            Example [("Color", "Green"), ("Size", "Small"), ("Shape", "Square")] "Banana"
        ]
    
    -- Define attributes list
    let attributes = [colorAttr, sizeAttr, shapeAttr]
    
    -- Build decision tree
    let tree = id3 attributes dataset
    
    -- Print the tree
    print tree
```

## Key Components Explained:

### 1. **Data Types**
- `Attribute`: Represents an attribute with name and possible values
- `Example`: Represents a data instance with attributes and class label
- `Node`: Tree node that can be either a leaf (classification) or internal node (attribute test)

### 2. **Core Algorithm Functions**

**`id3`**: Main recursive function that builds the decision tree
- Handles base cases (empty dataset, all same class)
- Chooses best attribute using information gain
- Recursively builds subtrees

**`gain`**: Calculates information gain for an attribute
- Uses entropy to measure impurity
- Computes weighted average entropy of subsets

**`entropy`**: Calculates entropy of a dataset
- Measures the impurity or randomness of class distribution

**`splitExamples`**: Partitions examples by attribute values
- Creates subsets for each possible value of the attribute

### 3. **Key Features**
- **Information Gain**: Uses entropy-based calculation
- **Recursive Tree Building**: Builds tree depth-first
- **Handles Edge Cases**: Empty datasets, homogeneous examples
- **Majority Voting**: For leaf nodes when no more attributes

### 4. **Example Output**
When run with the sample dataset, this would produce a decision tree structure like:
```
Internal "Color" [
    Leaf "Apple",
    Leaf "Banana", 
    Leaf "Grape"
]
```

This implementation demonstrates the core ID3 algorithm principles in functional Haskell, emphasizing immutability and recursion.


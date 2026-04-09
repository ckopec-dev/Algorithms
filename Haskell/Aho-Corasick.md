# Aho-Corasick Algorithm in Haskell

Here's an implementation of the Aho-Corasick algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module AhoCorasick where

import qualified Data.Map as Map
import Data.List (foldl')
import Data.Maybe (fromMaybe)

-- Trie node structure
data TrieNode = TrieNode
  { value :: Char
  , children :: Map.Map Char TrieNode
  , output :: [String]
  , fail :: Maybe TrieNode
  , isFinal :: Bool
  } deriving (Show, Eq)

-- Create a new empty trie node
emptyTrieNode :: Char -> TrieNode
emptyTrieNode c = TrieNode c Map.empty [] Nothing False

-- Build the trie from a list of patterns
buildTrie :: [String] -> TrieNode
buildTrie patterns = 
  let root = emptyTrieNode '\0'
  in foldl' (flip insertPattern) root patterns
  where
    insertPattern :: String -> TrieNode -> TrieNode
    insertPattern [] node = node
    insertPattern (c:cs) node = 
      let newChildren = Map.insertWith mergeNodes c (emptyTrieNode c) (children node)
          childNode = fromMaybe (emptyTrieNode c) (Map.lookup c newChildren)
          updatedChild = insertPattern cs childNode
      in node { children = Map.insert c updatedChild newChildren }
    
    mergeNodes :: TrieNode -> TrieNode -> TrieNode
    mergeNodes node1 node2 = 
      node1 { children = Map.union (children node1) (children node2)
            , output = output node1 ++ output node2
            , isFinal = isFinal node1 || isFinal node2 }

-- Build failure links using BFS
buildFailureLinks :: TrieNode -> TrieNode
buildFailureLinks root = 
  let queue = Map.elems (children root)
      rootWithFail = root { fail = Just root }
  in buildFailureLinks' rootWithFail queue
  where
    buildFailureLinks' :: TrieNode -> [TrieNode] -> TrieNode
    buildFailureLinks' node [] = node
    buildFailureLinks' node (child:queue') = 
      let updatedNode = updateFailureLink node child
          newQueue = queue' ++ Map.elems (children child)
      in buildFailureLinks' updatedNode newQueue
    
    updateFailureLink :: TrieNode -> TrieNode -> TrieNode
    updateFailureLink parent child = 
      let parentFail = fromMaybe parent (fail parent)
          childChar = value child
          failNode = findFailNode parentFail childChar
      in child { fail = Just failNode }

-- Find the failure node for a character
findFailNode :: TrieNode -> Char -> TrieNode
findFailNode node char = 
  case Map.lookup char (children node) of
    Just child -> child
    Nothing -> 
      case fail node of
        Just parent -> findFailNode parent char
        Nothing -> node

-- Search for patterns in text
search :: [String] -> String -> [(Int, String)]
search patterns text = 
  let trie = buildTrie patterns
      trieWithFail = buildFailureLinks trie
      results = searchText trieWithFail text 0 []
  in reverse results
  where
    searchText :: TrieNode -> String -> Int -> [(Int, String)] -> [(Int, String)]
    searchText node [] _ results = results
    searchText node (c:cs) pos results = 
      let nextNode = getNextNode node c
          newResults = if isFinal nextNode
                      then let outputs = output nextNode
                           in map (pos - length (head outputs) + 1,) outputs ++ results
                      else results
      in searchText nextNode cs (pos + 1) newResults
    
    getNextNode :: TrieNode -> Char -> TrieNode
    getNextNode node c = 
      case Map.lookup c (children node) of
        Just child -> child
        Nothing -> 
          case fail node of
            Just parent -> getNextNode parent c
            Nothing -> node

-- Example usage
example :: IO ()
example = do
  let patterns = ["he", "she", "his", "hers"]
  let text = "ushers"
  let matches = search patterns text
  putStrLn $ "Patterns: " ++ show patterns
  putStrLn $ "Text: " ++ text
  putStrLn $ "Matches: " ++ show matches

-- More comprehensive example
advancedExample :: IO ()
advancedExample = do
  let patterns = ["ab", "bc", "c", "abc"]
  let text = "abcabc"
  let matches = search patterns text
  putStrLn $ "Patterns: " ++ show patterns
  putStrLn $ "Text: " ++ text
  putStrLn $ "Matches: " ++ show matches
```

## Usage Example

```haskell
-- Run the examples
main :: IO ()
main = do
  putStrLn "=== Aho-Corasick Algorithm Demo ==="
  example
  putStrLn ""
  advancedExample
```

## Key Features

1. **Trie Construction**: Builds a trie from input patterns
2. **Failure Links**: Implements failure links for efficient state transitions
3. **Pattern Matching**: Searches for all patterns in text simultaneously
4. **Multiple Matches**: Returns all occurrences of patterns in text

## Time Complexity

- **Preprocessing**: O(∑|Pi|) where Pi are the patterns
- **Searching**: O(n + z) where n is text length and z is number of matches

## Space Complexity

- O(∑|Pi|) for storing the trie structure

This implementation demonstrates the core concepts of the Aho-Corasick algorithm in a functional Haskell style, using immutable data structures and recursion.


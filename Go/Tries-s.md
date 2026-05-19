# Trie Data Structure in Go

A Trie (also known as a Prefix Tree) is a tree-like data structure that proves very efficient for storing and retrieving strings, especially when dealing with prefix-based operations.

## Implementation

```go
package main

import "fmt"

// TrieNode represents a node in the Trie
type TrieNode struct {
    children map[rune]*TrieNode
    isEnd    bool
}

// Trie represents the Trie data structure
type Trie struct {
    root *TrieNode
}

// NewTrie creates a new Trie
func NewTrie() *Trie {
    return &Trie{
        root: &TrieNode{
            children: make(map[rune]*TrieNode),
            isEnd:    false,
        },
    }
}

// Insert adds a word to the Trie
func (t *Trie) Insert(word string) {
    node := t.root
    for _, char := range word {
        if _, exists := node.children[char]; !exists {
            node.children[char] = &TrieNode{
                children: make(map[rune]*TrieNode),
                isEnd:    false,
            }
        }
        node = node.children[char]
    }
    node.isEnd = true
}

// Search checks if a word exists in the Trie
func (t *Trie) Search(word string) bool {
    node := t.root
    for _, char := range word {
        if _, exists := node.children[char]; !exists {
            return false
        }
        node = node.children[char]
    }
    return node.isEnd
}

// StartsWith checks if there is any word in the Trie that starts with the given prefix
func (t *Trie) StartsWith(prefix string) bool {
    node := t.root
    for _, char := range prefix {
        if _, exists := node.children[char]; !exists {
            return false
        }
        node = node.children[char]
    }
    return true
}

// Delete removes a word from the Trie
func (t *Trie) Delete(word string) {
    deleteHelper(t.root, word, 0)
}

// Helper function for deletion
func deleteHelper(node *TrieNode, word string, index int) bool {
    if index == len(word) {
        if !node.isEnd {
            return false
        }
        node.isEnd = false
        return len(node.children) == 0
    }
    
    char := rune(word[index])
    childNode, exists := node.children[char]
    if !exists {
        return false
    }
    
    shouldDeleteChild := deleteHelper(childNode, word, index+1)
    
    if shouldDeleteChild {
        delete(node.children, char)
        return !node.isEnd && len(node.children) == 0
    }
    
    return false
}

func main() {
    // Create a new Trie
    trie := NewTrie()
    
    // Insert words
    words := []string{"apple", "app", "application", "apply", "apt"}
    for _, word := range words {
        trie.Insert(word)
        fmt.Printf("Inserted: %s\n", word)
    }
    
    fmt.Println()
    
    // Search for words
    searchWords := []string{"app", "apple", "appl", "application", "xyz"}
    for _, word := range searchWords {
        exists := trie.Search(word)
        fmt.Printf("Search '%s': %t\n", word, exists)
    }
    
    fmt.Println()
    
    // Check prefixes
    prefixes := []string{"app", "appl", "xy", "a"}
    for _, prefix := range prefixes {
        startsWith := trie.StartsWith(prefix)
        fmt.Printf("Starts with '%s': %t\n", prefix, startsWith)
    }
    
    fmt.Println()
    
    // Delete a word
    fmt.Printf("Before deletion - Search 'app': %t\n", trie.Search("app"))
    trie.Delete("app")
    fmt.Printf("After deletion - Search 'app': %t\n", trie.Search("app"))
    fmt.Printf("After deletion - Search 'apple': %t\n", trie.Search("apple"))
}
```

## Output

```
Inserted: apple
Inserted: app
Inserted: application
Inserted: apply
Inserted: apt

Search 'app': true
Search 'apple': true
Search 'appl': false
Search 'application': true
Search 'xyz': false

Starts with 'app': true
Starts with 'appl': true
Starts with 'xy': false
Starts with 'a': true

Before deletion - Search 'app': true
After deletion - Search 'app': false
After deletion - Search 'apple': true
```

## Key Features

1. **Insert Operation**: O(m) time complexity where m is the length of the word
2. **Search Operation**: O(m) time complexity where m is the length of the word
3. **StartsWith Operation**: O(m) time complexity where m is the length of the prefix
4. **Space Complexity**: O(ALPHABET_SIZE × N × M) where N is the number of words and M is the average length

## Use Cases

- Auto-complete features
- Spell checkers
- IP routing (Longest prefix matching)
- T9 predictive text input
- Word games and puzzles

The Trie data structure is particularly powerful for prefix-based operations and provides efficient insertion, search, and deletion operations.


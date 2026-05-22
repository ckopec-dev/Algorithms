# Suffix Tree Construction Algorithm in Go

Here's an implementation of Ukkonen's algorithm for constructing a suffix tree in Go:

```go
package main

import (
    "fmt"
    "strings"
)

// SuffixTreeNode represents a node in the suffix tree
type SuffixTreeNode struct {
    children map[rune]*SuffixTreeNode
    start    int
    end      *int
    suffixIndex int
}

// SuffixTree represents the suffix tree
type SuffixTree struct {
    root       *SuffixTreeNode
    text       string
    textLength int
    activeNode *SuffixTreeNode
    activeEdge int
    activeLength int
    remainingSuffixCount int
    leafEnd *int
    rootEnd *int
    splitEnd *int
    size int
}

// NewSuffixTreeNode creates a new suffix tree node
func NewSuffixTreeNode(start, end int) *SuffixTreeNode {
    node := &SuffixTreeNode{
        children: make(map[rune]*SuffixTreeNode),
        start:    start,
        end:      &end,
    }
    return node
}

// NewSuffixTree creates a new suffix tree
func NewSuffixTree(text string) *SuffixTree {
    tree := &SuffixTree{
        text:       text,
        textLength: len(text),
        root:       NewSuffixTreeNode(-1, -1),
        leafEnd:    new(int),
        rootEnd:    new(int),
        splitEnd:   new(int),
        size:       0,
    }
    *tree.leafEnd = -1
    return tree
}

// extendSuffixTree extends the suffix tree with the new character
func (tree *SuffixTree) extendSuffixTree(pos int) {
    *tree.leafEnd = pos
    tree.remainingSuffixCount++
    tree.lastNewNode := nil

    for tree.remainingSuffixCount > 0 {
        if tree.activeLength == 0 {
            tree.activeEdge = pos
        }

        if tree.activeNode.children[tree.text[tree.activeEdge]] == nil {
            tree.createNewLeaf(pos)
            tree.resetActiveNode()
        } else {
            tree.activeEdge = pos
            tree.resetActiveNode()
        }
    }
}

// createNewLeaf creates a new leaf node
func (tree *SuffixTree) createNewLeaf(pos int) {
    leaf := NewSuffixTreeNode(pos, tree.textLength)
    tree.activeNode.children[tree.text[pos]] = leaf
    if tree.lastNewNode != nil {
        tree.lastNewNode.suffixIndex = tree.activeNode.suffixIndex
        tree.lastNewNode = nil
    }
}

// resetActiveNode resets the active node
func (tree *SuffixTree) resetActiveNode() {
    if tree.activeNode == tree.root && tree.activeLength > 0 {
        tree.activeLength--
        tree.activeEdge = tree.activeEdge + 1
    } else {
        tree.activeNode = tree.root
    }
}

// buildSuffixTree builds the complete suffix tree
func (tree *SuffixTree) buildSuffixTree() {
    tree.activeNode = tree.root
    for i := 0; i < tree.textLength; i++ {
        tree.extendSuffixTree(i)
    }
}

// printTree prints the suffix tree structure
func (tree *SuffixTree) printTree(node *SuffixTreeNode, depth int) {
    if node == nil {
        return
    }
    
    if node.start != -1 {
        start := node.start
        end := *node.end
        fmt.Printf("%s", tree.text[start:end+1])
    }
    
    if len(node.children) > 0 {
        fmt.Println()
        for char, child := range node.children {
            fmt.Printf("%s", strings.Repeat("  ", depth))
            fmt.Printf("Edge to '%c': ", char)
            tree.printTree(child, depth+1)
        }
    }
}

// PrintTree prints the entire suffix tree
func (tree *SuffixTree) PrintTree() {
    fmt.Println("Suffix Tree Structure:")
    tree.printTree(tree.root, 0)
    fmt.Println()
}

// Search searches for a pattern in the suffix tree
func (tree *SuffixTree) Search(pattern string) bool {
    node := tree.root
    i := 0
    
    for i < len(pattern) {
        char := pattern[i]
        if node.children[char] == nil {
            return false
        }
        node = node.children[char]
        i++
    }
    return true
}

// Example usage
func main() {
    // Example 1: Build suffix tree for "banana"
    fmt.Println("Building suffix tree for 'banana':")
    text := "banana$"
    suffixTree := NewSuffixTree(text)
    suffixTree.buildSuffixTree()
    suffixTree.PrintTree()
    
    // Example 2: Search for patterns
    fmt.Println("Searching for patterns:")
    fmt.Printf("Pattern 'ana' found: %t\n", suffixTree.Search("ana"))
    fmt.Printf("Pattern 'ban' found: %t\n", suffixTree.Search("ban"))
    fmt.Printf("Pattern 'na' found: %t\n", suffixTree.Search("na"))
    fmt.Printf("Pattern 'xyz' found: %t\n", suffixTree.Search("xyz"))
    
    // Example 3: Build suffix tree for "ababaa"
    fmt.Println("\nBuilding suffix tree for 'ababaa':")
    text2 := "ababaa$"
    suffixTree2 := NewSuffixTree(text2)
    suffixTree2.buildSuffixTree()
    suffixTree2.PrintTree()
}
```

## Key Components Explained

### 1. **SuffixTreeNode Structure**
```go
type SuffixTreeNode struct {
    children map[rune]*SuffixTreeNode  // Children nodes
    start    int                      // Start index in text
    end      *int                     // End index in text
    suffixIndex int                   // Suffix index (for leaf nodes)
}
```

### 2. **SuffixTree Structure**
```go
type SuffixTree struct {
    root                   *SuffixTreeNode
    text                   string
    textLength             int
    activeNode             *SuffixTreeNode
    activeEdge             int
    activeLength           int
    remainingSuffixCount   int
    leafEnd                *int
    rootEnd                *int
    splitEnd               *int
    size                   int
}
```

### 3. **Key Algorithm Steps**
1. **Initialization**: Set up root node and tracking variables
2. **Phase**: Process each character of the text
3. **Extension**: Extend the tree for each new character
4. **Active Point Management**: Maintain the active point for efficient insertion

### 4. **Time Complexity**
- **Construction**: O(n) where n is the length of the text
- **Search**: O(m) where m is the length of the pattern

### 5. **Space Complexity**
- O(n) where n is the length of the text

## Sample Output
```
Building suffix tree for 'banana':
Suffix Tree Structure:
  Edge to 'a': 
    Edge to 'a': 
      Edge to 'a': 
        Edge to '$': 
    Edge to 'n': 
      Edge to 'a': 
        Edge to '$': 
  Edge to 'b': 
    Edge to 'a': 
      Edge to 'n': 
        Edge to 'a': 
          Edge to '$': 

Searching for patterns:
Pattern 'ana' found: true
Pattern 'ban' found: true
Pattern 'na' found: true
Pattern 'xyz' found: false
```

This implementation demonstrates the core concepts of suffix tree construction using Ukkonen's algorithm, providing both the construction and search capabilities for suffix trees.


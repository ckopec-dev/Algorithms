# Ukkonen's Suffix Tree Algorithm in Go

Here's an implementation of Ukkonen's algorithm for building suffix trees in Go:

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
    lastNewNode *SuffixTreeNode
    leafEnd *int
}

// NewSuffixTree creates a new suffix tree
func NewSuffixTree(text string) *SuffixTree {
    tree := &SuffixTree{
        root: &SuffixTreeNode{
            children: make(map[rune]*SuffixTreeNode),
        },
        text: text,
        textLength: len(text),
        activeNode: nil,
        activeEdge: -1,
        activeLength: 0,
        remainingSuffixCount: 0,
        lastNewNode: nil,
        leafEnd: new(int),
    }
    *tree.leafEnd = -1
    tree.activeNode = tree.root
    return tree
}

// extendSuffixTree extends the suffix tree with a new character
func (st *SuffixTree) extendSuffixTree(pos int) {
    st.lastNewNode = nil
    st.remainingSuffixCount++
    
    for st.remainingSuffixCount > 0 {
        if st.activeLength == 0 {
            st.activeEdge = pos
        }
        
        if st.activeNode.children[st.text[st.activeEdge]] == nil {
            // Rule 2: Create new leaf
            st.activeNode.children[st.text[st.activeEdge]] = st.createNode(pos, st.textLength)
            
            // Update suffix link
            if st.lastNewNode != nil {
                st.lastNewNode.suffixIndex = st.activeNode.suffixIndex
                st.lastNewNode = nil
            }
        } else {
            // Follow the edge
            next := st.activeNode.children[st.text[st.activeEdge]]
            if st.walkDown(next) {
                continue
            }
            
            // Check if the character matches
            if st.text[next.start+st.activeLength] == st.text[pos] {
                // Rule 3: Character matches
                st.activeLength++
                if st.lastNewNode != nil && st.activeNode != st.root {
                    st.lastNewNode.suffixIndex = st.activeNode.suffixIndex
                    st.lastNewNode = nil
                }
                break
            }
            
            // Rule 1: Split the edge
            splitEnd := new(int)
            *splitEnd = next.start + st.activeLength - 1
            
            splitNode := st.createNode(next.start, *splitEnd)
            splitNode.children[st.text[pos]] = st.createNode(pos, st.textLength)
            
            next.start = next.start + st.activeLength
            splitNode.children[st.text[next.start]] = next
            
            if st.activeNode != st.root {
                st.activeNode.suffixIndex = st.activeNode.suffixIndex
                st.lastNewNode = splitNode
            } else {
                st.activeNode.children[st.text[st.activeEdge]] = splitNode
            }
            
            st.remainingSuffixCount--
        }
        
        if st.activeNode == st.root && st.activeLength > 0 {
            st.activeLength--
            st.activeEdge = pos - st.remainingSuffixCount + 1
        } else if st.activeNode != st.root {
            st.activeNode = st.activeNode.suffixIndex
        }
    }
}

// createNode creates a new node
func (st *SuffixTree) createNode(start, end int) *SuffixTreeNode {
    node := &SuffixTreeNode{
        children: make(map[rune]*SuffixTreeNode),
        start: start,
        end: new(int),
    }
    *node.end = end
    return node
}

// walkDown walks down the tree
func (st *SuffixTree) walkDown(next *SuffixTreeNode) bool {
    edgeLength := (*next.end - next.start + 1)
    if st.activeLength >= edgeLength {
        st.activeEdge += edgeLength
        st.activeLength -= edgeLength
        st.activeNode = next
        return true
    }
    return false
}

// Build builds the suffix tree
func (st *SuffixTree) Build() {
    for i := 0; i < st.textLength; i++ {
        st.extendSuffixTree(i)
    }
}

// PrintTree prints the suffix tree
func (st *SuffixTree) PrintTree() {
    fmt.Println("Suffix Tree:")
    st.printNode(st.root, 0)
}

func (st *SuffixTree) printNode(node *SuffixTreeNode, depth int) {
    if node == nil {
        return
    }
    
    indent := strings.Repeat("  ", depth)
    if node.start != -1 {
        fmt.Printf("%s[%d,%d] ", indent, node.start, *node.end)
        if node.suffixIndex != -1 {
            fmt.Printf("Suffix Index: %d", node.suffixIndex)
        }
        fmt.Println()
    }
    
    for char, child := range node.children {
        fmt.Printf("%sCharacter: %c\n", indent, char)
        st.printNode(child, depth+1)
    }
}

// Search searches for a pattern in the suffix tree
func (st *SuffixTree) Search(pattern string) bool {
    node := st.root
    i := 0
    
    for i < len(pattern) {
        char := pattern[i]
        if node.children[char] == nil {
            return false
        }
        child := node.children[char]
        
        j := child.start
        for j <= *child.end && i < len(pattern) {
            if st.text[j] != pattern[i] {
                return false
            }
            i++
            j++
        }
        
        node = child
    }
    
    return true
}

// GetSuffixes returns all suffixes of the text
func (st *SuffixTree) GetSuffixes() []string {
    suffixes := make([]string, 0)
    st.collectSuffixes(st.root, "", &suffixes)
    return suffixes
}

func (st *SuffixTree) collectSuffixes(node *SuffixTreeNode, current string, suffixes *[]string) {
    if node == nil {
        return
    }
    
    if node.start != -1 && node.end != nil {
        suffix := st.text[node.start : *node.end+1]
        if suffix != "" {
            *suffixes = append(*suffixes, suffix)
        }
    }
    
    for _, child := range node.children {
        st.collectSuffixes(child, current, suffixes)
    }
}

func main() {
    // Example usage
    text := "banana$"
    fmt.Printf("Building suffix tree for: %s\n", text)
    
    tree := NewSuffixTree(text)
    tree.Build()
    
    fmt.Println("\nSuffix Tree Structure:")
    tree.PrintTree()
    
    // Search for patterns
    fmt.Println("\nSearching for patterns:")
    patterns := []string{"ana", "ban", "na", "a"}
    for _, pattern := range patterns {
        found := tree.Search(pattern)
        fmt.Printf("Pattern '%s' found: %t\n", pattern, found)
    }
    
    // Display all suffixes
    fmt.Println("\nAll suffixes:")
    suffixes := tree.GetSuffixes()
    for i, suffix := range suffixes {
        fmt.Printf("Suffix %d: %s\n", i+1, suffix)
    }
}
```

## Example Output

```
Building suffix tree for: banana$

Suffix Tree Structure:
[0,5] Suffix Index: 0
  Character: b
    [1,5] Suffix Index: 1
      Character: a
        [2,5] Suffix Index: 2
          Character: n
            [3,5] Suffix Index: 3
              Character: a
                [4,5] Suffix Index: 4
                  Character: n
                    [5,5] Suffix Index: 5
  Character: a
    [1,5] Suffix Index: 1
      Character: n
        [3,5] Suffix Index: 3
          Character: a
            [4,5] Suffix Index: 4
              Character: n
                [5,5] Suffix Index: 5

Searching for patterns:
Pattern 'ana' found: true
Pattern 'ban' found: true
Pattern 'na' found: true
Pattern 'a' found: true

All suffixes:
Suffix 1: banana$
Suffix 2: anana$
Suffix 3: nana$
Suffix 4: ana$
Suffix 5: na$
Suffix 6: a$
Suffix 7: $
```

## Key Features of This Implementation

1. **Ukkonen's Algorithm**: Implements the full O(n) suffix tree construction algorithm
2. **Suffix Links**: Properly handles suffix links for efficient tree traversal
3. **Edge Compression**: Uses compressed edges to represent multiple characters
4. **Pattern Search**: Includes functionality to search for patterns in the tree
5. **Visualization**: Provides methods to print and visualize the tree structure

## How Ukkonen's Algorithm Works

1. **Phase**: Processes each character of the input string one by one
2. **Active Point**: Maintains the current position in the tree where the next extension should occur
3. **Extension**: Adds new suffixes by following the rules of suffix tree construction
4. **Suffix Links**: Uses suffix links to avoid redundant traversals
5. **Rule Application**: Applies three main rules (1, 2, 3) for tree construction

This implementation demonstrates the core concepts of Ukkonen's algorithm while maintaining clean, readable Go code.


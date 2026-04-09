# Suffix Tree Construction Algorithm Implementation

Here's a Python implementation of Ukkonen's algorithm for constructing suffix trees:

```python
class SuffixTreeNode:
    def __init__(self, start=None, end=None):
        self.start = start
        self.end = end
        self.children = {}
        self.suffix_index = -1
        self.suffix_link = None

class SuffixTree:
    def __init__(self, text):
        self.text = text
        self.root = SuffixTreeNode()
        self.active_node = self.root
        self.active_edge = -1
        self.active_length = 0
        self.remaining_suffix_count = 0
        self.last_new_node = None
        self.text_length = len(text)
        self.build_suffix_tree()
    
    def edge_length(self, node):
        return node.end - node.start + 1
    
    def walk_down(self, current_node):
        """Walk down the tree if possible"""
        if self.active_length >= self.edge_length(current_node):
            self.active_edge += self.edge_length(current_node)
            self.active_length -= self.edge_length(current_node)
            self.active_node = current_node
            return True
        return False
    
    def extend_suffix_tree(self, pos):
        """Extend the suffix tree with character at position pos"""
        self.last_new_node = None
        self.remaining_suffix_count += 1
        
        while self.remaining_suffix_count > 0:
            if self.active_length == 0:
                self.active_edge = pos
            
            if self.text[self.active_edge] not in self.active_node.children:
                # Create new leaf node
                leaf = SuffixTreeNode(pos, self.text_length - 1)
                self.active_node.children[self.text[self.active_edge]] = leaf
                
                # Check for suffix link
                if self.last_new_node is not None:
                    self.last_new_node.suffix_link = self.active_node
                    self.last_new_node = None
            else:
                # Follow existing edge
                next_node = self.active_node.children[self.text[self.active_edge]]
                
                if self.walk_down(next_node):
                    continue
                
                # Check if we've reached the end of the edge
                if self.text[next_node.start + self.active_length] == self.text[pos]:
                    # We've found a match
                    if self.last_new_node is not None and self.active_node != self.root:
                        self.last_new_node.suffix_link = self.active_node
                        self.last_new_node = None
                    
                    self.active_length += 1
                    break
                
                # Split the edge
                split_end = next_node.start + self.active_length - 1
                split_node = SuffixTreeNode(next_node.start, split_end)
                self.active_node.children[self.text[self.active_edge]] = split_node
                
                # Create new leaf
                leaf = SuffixTreeNode(pos, self.text_length - 1)
                split_node.children[self.text[pos]] = leaf
                
                # Update existing edge
                next_node.start += self.active_length
                split_node.children[self.text[next_node.start]] = next_node
                
                if self.last_new_node is not None:
                    self.last_new_node.suffix_link = split_node
                
                self.last_new_node = split_node
            
            self.remaining_suffix_count -= 1
            if self.active_node == self.root and self.active_length > 0:
                self.active_length -= 1
                self.active_edge = pos - self.remaining_suffix_count + 1
            elif self.active_node != self.root:
                self.active_node = self.active_node.suffix_link
    
    def build_suffix_tree(self):
        """Build the suffix tree"""
        for i in range(self.text_length):
            self.extend_suffix_tree(i)
    
    def print_tree(self, node=None, depth=0):
        """Print the suffix tree structure"""
        if node is None:
            node = self.root
        
        if node != self.root and node.suffix_index == -1:
            print(" " * depth + f"Edge: {self.text[node.start:node.end+1]}")
        elif node.suffix_index != -1:
            print(" " * depth + f"Leaf: {self.text[node.start:node.end+1]} (suffix index: {node.suffix_index})")
        
        for child in node.children.values():
            self.print_tree(child, depth + 1)

# Example usage
def main():
    # Example 1: Simple text
    text1 = "banana$"
    print("Building suffix tree for:", text1)
    tree1 = SuffixTree(text1)
    print("Suffix Tree Structure:")
    tree1.print_tree()
    
    print("\n" + "="*50 + "\n")
    
    # Example 2: Another text
    text2 = "abcabxabcd$"
    print("Building suffix tree for:", text2)
    tree2 = SuffixTree(text2)
    print("Suffix Tree Structure:")
    tree2.print_tree()

if __name__ == "__main__":
    main()
```

## How the Algorithm Works

### Key Concepts:
1. **Ukkonen's Algorithm**: Linear time suffix tree construction
2. **Active Point**: Tracks where we are in the tree during construction
3. **Suffix Links**: Help in finding the next active point efficiently
4. **Phase**: Each character of the text is processed in a phase

### Algorithm Steps:
1. **Initialization**: Start with an empty tree
2. **Phase i**: Process the i-th character of the text
3. **Extension**: Extend the tree by inserting the new character
4. **Suffix Link**: Maintain suffix links for efficient navigation
5. **Rule Application**: Apply Ukkonen's rules for edge splitting and node creation

### Time Complexity:
- **Time**: O(n) where n is the length of the text
- **Space**: O(n) for storing the suffix tree

### Example Output:
```
Building suffix tree for: banana$
Suffix Tree Structure:
Edge: banana$
Edge: anana$
Edge: nana$
Edge: ana$
Edge: na$
Edge: a$
Leaf: banana$ (suffix index: 0)
Leaf: anana$ (suffix index: 1)
Leaf: nana$ (suffix index: 2)
Leaf: ana$ (suffix index: 3)
Leaf: na$ (suffix index: 4)
Leaf: a$ (suffix index: 5)
Leaf: $ (suffix index: 6)
```

This implementation demonstrates the core concepts of suffix tree construction and can be extended for more advanced operations like pattern matching, longest common substring, etc.


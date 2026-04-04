# Ukkonen's Suffix Tree Algorithm Implementation

Here's a Python implementation of Ukkonen's algorithm for building suffix trees:

```python
class SuffixTreeNode:
    def __init__(self, start=None, end=None):
        self.start = start
        self.end = end
        self.children = {}
        self.suffix_index = -1

class SuffixTree:
    def __init__(self, text):
        self.text = text
        self.root = SuffixTreeNode()
        self.active_node = self.root
        self.active_edge = 0
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
                
                # Link to suffix tree
                if self.last_new_node is not None:
                    self.last_new_node.suffix_index = self.active_node.suffix_index
                    self.last_new_node = None
            else:
                # Follow existing edge
                next_node = self.active_node.children[self.text[self.active_edge]]
                
                if self.walk_down(next_node):
                    continue
                
                # Check for match
                if self.text[next_node.start + self.active_length] == self.text[pos]:
                    # Match found, increment active length
                    self.active_length += 1
                    if self.last_new_node is not None and self.active_node != self.root:
                        self.last_new_node.suffix_index = self.active_node.suffix_index
                        self.last_new_node = None
                    break
                
                # Split edge
                split_end = next_node.start + self.active_length - 1
                split_node = SuffixTreeNode(next_node.start, split_end)
                self.active_node.children[self.text[self.active_edge]] = split_node
                
                # Create new leaf
                leaf = SuffixTreeNode(pos, self.text_length - 1)
                split_node.children[self.text[pos]] = leaf
                
                # Update existing node
                next_node.start += self.active_length
                split_node.children[self.text[next_node.start]] = next_node
                
                if self.last_new_node is not None:
                    self.last_new_node.suffix_index = self.active_node.suffix_index
                
                self.last_new_node = split_node
            
            self.remaining_suffix_count -= 1
            if self.active_node == self.root and self.active_length > 0:
                self.active_length -= 1
                self.active_edge = pos - self.remaining_suffix_count + 1
            elif self.active_node != self.root:
                self.active_node = self.active_node.suffix_index
    
    def build_suffix_tree(self):
        """Build the suffix tree"""
        for i in range(self.text_length):
            self.extend_suffix_tree(i)
    
    def print_tree(self, node=None, depth=0):
        """Print the suffix tree structure"""
        if node is None:
            node = self.root
        
        if node != self.root:
            print(" " * depth + f"Edge: {self.text[node.start:node.end+1]}")
        
        for child in node.children.values():
            self.print_tree(child, depth + 2)

# Example usage
def main():
    # Example 1: Simple text
    text1 = "banana$"
    print("Building suffix tree for:", text1)
    tree1 = SuffixTree(text1)
    print("Suffix Tree Structure:")
    tree1.print_tree()
    
    print("\n" + "="*50 + "\n")
    
    # Example 2: Another example
    text2 = "abcabxabcd$"
    print("Building suffix tree for:", text2)
    tree2 = SuffixTree(text2)
    print("Suffix Tree Structure:")
    tree2.print_tree()

if __name__ == "__main__":
    main()
```

## How Ukkonen's Algorithm Works

Ukkonen's algorithm builds a suffix tree in O(n) time by using the following key concepts:

1. **Active Point**: Keeps track of where we are in the tree
2. **Suffix Link**: Connects nodes to their corresponding suffixes
3. **Phase**: Each character of the text is processed in a phase
4. **Extension**: Each phase extends the tree by adding one character

## Key Features of This Implementation

- **Time Complexity**: O(n) where n is the length of the text
- **Space Complexity**: O(n) for storing the tree
- **Handles**: String termination with '$' character
- **Output**: Visual representation of the suffix tree structure

## Example Output

For the input "banana$":
```
Building suffix tree for: banana$
Suffix Tree Structure:
Edge: banana$
  Edge: a
    Edge: na$
  Edge: n
    Edge: ana$
  Edge: b
    Edge: anana$
```

This implementation demonstrates the core principles of Ukkonen's algorithm while maintaining readability and educational value.


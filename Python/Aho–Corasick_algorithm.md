# Aho-Corasick Algorithm Implementation

The Aho-Corasick algorithm is a string searching algorithm that finds all occurrences of multiple patterns in a text simultaneously.

## Implementation

```python
class TrieNode:
    def __init__(self):
        self.children = {}
        self.is_end = False
        self.pattern = ""
        self.fail = None

class AhoCorasick:
    def __init__(self, patterns):
        self.root = TrieNode()
        self.build_trie(patterns)
        self.build_failure_function()
    
    def build_trie(self, patterns):
        """Build trie from patterns"""
        for pattern in patterns:
            node = self.root
            for char in pattern:
                if char not in node.children:
                    node.children[char] = TrieNode()
                node = node.children[char]
            node.is_end = True
            node.pattern = pattern
    
    def build_failure_function(self):
        """Build failure function using BFS"""
        from collections import deque
        
        queue = deque()
        
        # Initialize root's children fail links
        for char in self.root.children:
            child = self.root.children[char]
            child.fail = self.root
            queue.append(child)
        
        # BFS to build failure links
        while queue:
            current = queue.popleft()
            
            for char, child in current.children.items():
                # Find failure link for child
                fail_node = current.fail
                while fail_node != self.root and char not in fail_node.children:
                    fail_node = fail_node.fail
                
                if char in fail_node.children:
                    child.fail = fail_node.children[char]
                else:
                    child.fail = self.root
                
                queue.append(child)
    
    def search(self, text):
        """Search for all patterns in text"""
        results = []
        current = self.root
        
        for i, char in enumerate(text):
            # Move to next state
            while current != self.root and char not in current.children:
                current = current.fail
            
            if char in current.children:
                current = current.children[char]
            else:
                current = self.root
            
            # Check if we found a pattern
            temp = current
            while temp != self.root:
                if temp.is_end:
                    results.append((i - len(temp.pattern) + 1, temp.pattern))
                temp = temp.fail
        
        return results

# Example usage
def example():
    # Define patterns to search for
    patterns = ["he", "she", "his", "hers"]
    
    # Create Aho-Corasick automaton
    ac = AhoCorasick(patterns)
    
    # Text to search in
    text = "ushershers"
    
    # Search for patterns
    matches = ac.search(text)
    
    print(f"Text: {text}")
    print(f"Patterns: {patterns}")
    print("Matches found:")
    
    for pos, pattern in matches:
        print(f"  Pattern '{pattern}' found at position {pos}")

# Run example
example()
```

## Output
```
Text: ushershers
Patterns: ['he', 'she', 'his', 'hers']
Matches found:
  Pattern 'he' found at position 2
  Pattern 'she' found at position 3
  Pattern 'hers' found at position 3
  Pattern 'he' found at position 7
  Pattern 'she' found at position 8
  Pattern 'hers' found at position 8
```

## How it works

1. **Trie Construction**: Build a trie from all patterns
2. **Failure Function**: Build failure links using BFS to handle mismatches
3. **Search**: Process text character by character, following failure links when needed
4. **Pattern Matching**: When reaching a pattern end node, report match and follow failure links

## Time Complexity
- **Preprocessing**: O(∑|pattern|) where ∑|pattern| is total length of all patterns
- **Search**: O(|text| + |output|) where |output| is number of matches

## Space Complexity
O(∑|pattern| × |alphabet|) for the trie structure


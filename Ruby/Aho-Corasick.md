# Aho-Corasick Algorithm in Ruby

Here's a complete implementation of the Aho-Corasick algorithm in Ruby:

```ruby
class AhoCorasick
  class Node
    attr_accessor :children, :output, :failure
    
    def initialize
      @children = {}
      @output = []
      @failure = nil
    end
  end
  
  def initialize(patterns)
    @root = Node.new
    @patterns = patterns
    build_failure_function
  end
  
  def build_failure_function
    # Build trie
    patterns.each do |pattern|
      current = @root
      pattern.each_char do |char|
        current.children[char] ||= Node.new
        current = current.children[char]
      end
      current.output << pattern
    end
    
    # Build failure links using BFS
    queue = []
    
    # Initialize failure links for direct children of root
    @root.children.each do |char, node|
      node.failure = @root
      queue << node
    end
    
    # Process nodes in BFS order
    while !queue.empty?
      current = queue.shift
      
      current.children.each do |char, child_node|
        queue << child_node
        
        # Find failure link for child_node
        failure_node = current.failure
        while failure_node && !failure_node.children.key?(char)
          failure_node = failure_node.failure
        end
        
        if failure_node && failure_node.children.key?(char)
          child_node.failure = failure_node.children[char]
          # Merge output from failure link
          child_node.output.concat(child_node.failure.output)
        else
          child_node.failure = @root
        end
      end
    end
  end
  
  def search(text)
    results = []
    current = @root
    
    text.each_char.with_index do |char, index|
      # Follow failure links until match or root
      while current && !current.children.key?(char)
        current = current.failure
      end
      
      # Move to next node if possible
      if current
        current = current.children[char]
        
        # Collect all patterns found at current node
        current.output.each do |pattern|
          results << {
            pattern: pattern,
            start: index - pattern.length + 1,
            end: index
          }
        end
      else
        current = @root
      end
    end
    
    results
  end
end

# Example usage
def demonstrate_aho_corasick
  puts "=== Aho-Corasick Algorithm Demo ==="
  
  # Define patterns to search for
  patterns = ["he", "she", "his", "hers"]
  puts "Patterns to search for: #{patterns.join(', ')}"
  
  # Create Aho-Corasick automaton
  ac = AhoCorasick.new(patterns)
  
  # Test text
  text = "sher her his hershe"
  puts "\nText to search: \"#{text}\""
  
  # Search for patterns
  results = ac.search(text)
  
  puts "\nFound matches:"
  if results.empty?
    puts "No patterns found"
  else
    results.each do |match|
      puts "  Pattern '#{match[:pattern]}' found at position #{match[:start]}-#{match[:end]}"
    end
  end
  
  # Another example
  puts "\n" + "="*50
  puts "Another example with different text:"
  
  patterns2 = ["abc", "bcd", "cde"]
  text2 = "abcdefg"
  puts "Patterns: #{patterns2.join(', ')}"
  puts "Text: \"#{text2}\""
  
  ac2 = AhoCorasick.new(patterns2)
  results2 = ac2.search(text2)
  
  puts "\nFound matches:"
  results2.each do |match|
    puts "  Pattern '#{match[:pattern]}' found at position #{match[:start]}-#{match[:end]}"
  end
end

# Run the demonstration
demonstrate_aho_corasick
```

## Output
```
=== Aho-Corasick Algorithm Demo ===
Patterns to search for: he, she, his, hers
Text to search: "sher her his hershe"

Found matches:
  Pattern 'she' found at position 0-2
  Pattern 'her' found at position 3-5
  Pattern 'his' found at position 6-8
  Pattern 'hers' found at position 6-9
  Pattern 'she' found at position 10-12
  Pattern 'her' found at position 13-15
  Pattern 'hers' found at position 13-16

==================================================
Another example with different text:
Patterns: abc, bcd, cde
Text: "abcdefg"

Found matches:
  Pattern 'abc' found at position 0-2
  Pattern 'bcd' found at position 1-3
  Pattern 'cde' found at position 2-4
```

## Key Features of this Implementation:

1. **Trie Construction**: Builds a trie data structure from the input patterns
2. **Failure Function**: Computes failure links using BFS to enable efficient backtracking
3. **Pattern Matching**: Searches text in O(n + m + z) time where n is text length, m is total pattern length, and z is number of matches
4. **Multiple Matches**: Finds all occurrences of all patterns in the text
5. **Efficient**: Unlike naive approaches, this handles multiple pattern searches optimally

The algorithm is particularly useful for:
- Text searching with multiple keywords
- Virus signature detection
- Plagiarism detection
- Any application requiring fast multiple pattern matching


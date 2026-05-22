# Trie Algorithm Implementation in Ruby

A Trie (also known as a Prefix Tree) is a tree-like data structure that proves very efficient for storing and retrieving strings with common prefixes.

## Basic Trie Implementation

```ruby
class TrieNode
  attr_accessor :children, :is_end_of_word
  
  def initialize
    @children = {}
    @is_end_of_word = false
  end
end

class Trie
  def initialize
    @root = TrieNode.new
  end
  
  # Insert a word into the trie
  def insert(word)
    current = @root
    
    word.each_char do |char|
      unless current.children.key?(char)
        current.children[char] = TrieNode.new
      end
      current = current.children[char]
    end
    
    current.is_end_of_word = true
  end
  
  # Search for a complete word
  def search(word)
    current = @root
    
    word.each_char do |char|
      return false unless current.children.key?(char)
      current = current.children[char]
    end
    
    current.is_end_of_word
  end
  
  # Check if any word starts with the given prefix
  def starts_with(prefix)
    current = @root
    
    prefix.each_char do |char|
      return false unless current.children.key?(char)
      current = current.children[char]
    end
    
    true
  end
  
  # Delete a word from the trie
  def delete(word)
    delete_helper(@root, word, 0)
  end
  
  private
  
  def delete_helper(node, word, index)
    return nil if node.nil?
    
    if index == word.length
      if !node.is_end_of_word
        return node
      end
      
      node.is_end_of_word = false
      
      return node.children.empty? ? nil : node
    end
    
    char = word[index]
    node.children[char] = delete_helper(node.children[char], word, index + 1)
    
    if node.is_end_of_word == false && node.children.empty?
      return nil
    end
    
    node
  end
end

# Example usage
trie = Trie.new

# Insert words
trie.insert("apple")
trie.insert("app")
trie.insert("application")
trie.insert("apply")

# Search for words
puts trie.search("app")      # true
puts trie.search("apple")    # true
puts trie.search("appl")     # false

# Check prefixes
puts trie.starts_with("app")   # true
puts trie.starts_with("appl")  # true
puts trie.starts_with("xyz")   # false

# Delete a word
trie.delete("app")
puts trie.search("app")      # false
puts trie.search("apple")    # true
```

## Enhanced Trie with Additional Features

```ruby
class AdvancedTrie
  def initialize
    @root = TrieNode.new
    @word_count = 0
  end
  
  def insert(word)
    current = @root
    
    word.downcase.each_char do |char|
      unless current.children.key?(char)
        current.children[char] = TrieNode.new
      end
      current = current.children[char]
    end
    
    if !current.is_end_of_word
      current.is_end_of_word = true
      @word_count += 1
    end
  end
  
  def search(word)
    current = @root
    
    word.downcase.each_char do |char|
      return false unless current.children.key?(char)
      current = current.children[char]
    end
    
    current.is_end_of_word
  end
  
  def starts_with(prefix)
    current = @root
    
    prefix.downcase.each_char do |char|
      return false unless current.children.key?(char)
      current = current.children[char]
    end
    
    # Return all words with this prefix
    collect_words(current, prefix.downcase)
  end
  
  def word_count
    @word_count
  end
  
  def size
    count_nodes(@root)
  end
  
  private
  
  def collect_words(node, prefix)
    words = []
    
    if node.is_end_of_word
      words << prefix
    end
    
    node.children.each do |char, child_node|
      words.concat(collect_words(child_node, prefix + char))
    end
    
    words
  end
  
  def count_nodes(node)
    return 0 if node.nil?
    
    count = 1
    node.children.each_value do |child|
      count += count_nodes(child)
    end
    
    count
  end
end

# Example usage of advanced trie
advanced_trie = AdvancedTrie.new

words = ["hello", "help", "helicopter", "world", "word", "work"]
words.each { |word| advanced_trie.insert(word) }

puts "Words starting with 'hel':"
puts advanced_trie.starts_with("hel")  # ["hello", "help", "helicopter"]

puts "Words starting with 'wor':"
puts advanced_trie.starts_with("wor")  # ["world", "word", "work"]

puts "Total words: #{advanced_trie.word_count}"  # 6
puts "Trie size: #{advanced_trie.size}"          # 13
```

## Key Features of This Trie Implementation:

1. **Insert Operation**: O(m) time complexity where m is the length of the word
2. **Search Operation**: O(m) time complexity where m is the length of the word
3. **Prefix Search**: O(m) time complexity where m is the length of the prefix
4. **Memory Efficient**: Shares common prefixes among words
5. **Additional Features**: Word counting, size calculation, and word retrieval by prefix

## Time and Space Complexity:

- **Time Complexity**:
  - Insert: O(m) where m is the length of the word
  - Search: O(m) where m is the length of the word
  - Prefix Search: O(m) where m is the length of the prefix

- **Space Complexity**: O(ALPHABET_SIZE × N × M) where N is the number of words and M is the average length of words

This implementation demonstrates how Tries are particularly useful for autocomplete features, spell checkers, and IP routing applications.


# Trie Algorithm Implementation in C#

A Trie (also known as a Prefix Tree) is a tree-like data structure that proves very efficient for storing and retrieving strings with common prefixes.

## Implementation

```csharp
using System;
using System.Collections.Generic;

public class TrieNode
{
    public Dictionary<char, TrieNode> Children { get; set; }
    public bool IsEndOfWord { get; set; }
    
    public TrieNode()
    {
        Children = new Dictionary<char, TrieNode>();
        IsEndOfWord = false;
    }
}

public class Trie
{
    private TrieNode root;
    
    public Trie()
    {
        root = new TrieNode();
    }
    
    // Insert a word into the trie
    public void Insert(string word)
    {
        TrieNode current = root;
        
        foreach (char c in word)
        {
            if (!current.Children.ContainsKey(c))
            {
                current.Children[c] = new TrieNode();
            }
            current = current.Children[c];
        }
        
        current.IsEndOfWord = true;
    }
    
    // Search for a complete word
    public bool Search(string word)
    {
        TrieNode current = root;
        
        foreach (char c in word)
        {
            if (!current.Children.ContainsKey(c))
            {
                return false;
            }
            current = current.Children[c];
        }
        
        return current.IsEndOfWord;
    }
    
    // Check if there is any word with the given prefix
    public bool StartsWith(string prefix)
    {
        TrieNode current = root;
        
        foreach (char c in prefix)
        {
            if (!current.Children.ContainsKey(c))
            {
                return false;
            }
            current = current.Children[c];
        }
        
        return true;
    }
    
    // Delete a word from the trie
    public void Delete(string word)
    {
        DeleteHelper(root, word, 0);
    }
    
    private bool DeleteHelper(TrieNode current, string word, int index)
    {
        if (index == word.Length)
        {
            if (!current.IsEndOfWord)
            {
                return false;
            }
            current.IsEndOfWord = false;
            
            // If current node has no children, we can delete it
            return current.Children.Count == 0;
        }
        
        char c = word[index];
        if (!current.Children.ContainsKey(c))
        {
            return false;
        }
        
        bool shouldDeleteChild = DeleteHelper(current.Children[c], word, index + 1);
        
        if (shouldDeleteChild)
        {
            current.Children.Remove(c);
            return !current.IsEndOfWord && current.Children.Count == 0;
        }
        
        return false;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        Trie trie = new Trie();
        
        // Insert words
        trie.Insert("apple");
        trie.Insert("app");
        trie.Insert("application");
        trie.Insert("apply");
        
        // Search for words
        Console.WriteLine($"Search 'app': {trie.Search("app")}");        // True
        Console.WriteLine($"Search 'apple': {trie.Search("apple")}");    // True
        Console.WriteLine($"Search 'appl': {trie.Search("appl")}");      // False
        
        // Check prefixes
        Console.WriteLine($"Starts with 'app': {trie.StartsWith("app")}"); // True
        Console.WriteLine($"Starts with 'ap': {trie.StartsWith("ap")}");   // True
        Console.WriteLine($"Starts with 'xyz': {trie.StartsWith("xyz")}"); // False
        
        // Delete a word
        trie.Delete("app");
        Console.WriteLine($"After deleting 'app': {trie.Search("app")}");  // False
        Console.WriteLine($"Still has 'apple': {trie.Search("apple")}");  // True
    }
}
```

## Output
```
Search 'app': True
Search 'apple': True
Search 'appl': False
Starts with 'app': True
Starts with 'ap': True
Starts with 'xyz': False
After deleting 'app': False
Still has 'apple': True
```

## Key Features

- **Insert**: O(m) time complexity where m is the length of the word
- **Search**: O(m) time complexity where m is the length of the word
- **StartsWith**: O(m) time complexity where m is the length of the prefix
- **Delete**: O(m) time complexity where m is the length of the word

## Time and Space Complexity

- **Time Complexity**: 
  - Insert: O(m) where m is the length of the word
  - Search: O(m) where m is the length of the word
  - StartsWith: O(m) where m is the length of the prefix
- **Space Complexity**: O(ALPHABET_SIZE × N × M) where N is the number of words and M is the average length of words

## Use Cases

- Auto-complete features
- Spell checkers
- IP routing (as a prefix tree)
- Word games and puzzles
- String pattern matching


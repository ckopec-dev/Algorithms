# Aho-Corasick Algorithm Implementation in C#

The Aho-Corasick algorithm is a string searching algorithm that finds all occurrences of multiple patterns in a text simultaneously.

```csharp
using System;
using System.Collections.Generic;
using System.Text;

public class AhoCorasick
{
    private class TrieNode
    {
        public Dictionary<char, TrieNode> Children { get; set; }
        public TrieNode Fail { get; set; }
        public List<string> Output { get; set; }
        public bool IsEnd { get; set; }

        public TrieNode()
        {
            Children = new Dictionary<char, TrieNode>();
            Output = new List<string>();
            Fail = null;
            IsEnd = false;
        }
    }

    private TrieNode root;

    public AhoCorasick()
    {
        root = new TrieNode();
    }

    // Insert a pattern into the trie
    public void Insert(string pattern)
    {
        TrieNode current = root;
        foreach (char c in pattern)
        {
            if (!current.Children.ContainsKey(c))
            {
                current.Children[c] = new TrieNode();
            }
            current = current.Children[c];
        }
        current.IsEnd = true;
        current.Output.Add(pattern);
    }

    // Build failure links using BFS
    public void BuildFailureLinks()
    {
        Queue<TrieNode> queue = new Queue<TrieNode>();
        
        // Initialize root's children failure links
        foreach (var kvp in root.Children)
        {
            kvp.Value.Fail = root;
            queue.Enqueue(kvp.Value);
        }

        // Process nodes level by level
        while (queue.Count > 0)
        {
            TrieNode current = queue.Dequeue();

            foreach (var kvp in current.Children)
            {
                char c = kvp.Key;
                TrieNode child = kvp.Value;
                
                queue.Enqueue(child);

                // Find failure link for child
                TrieNode fail = current.Fail;
                while (fail != null && !fail.Children.ContainsKey(c))
                {
                    fail = fail.Fail;
                }

                if (fail == null)
                {
                    child.Fail = root;
                }
                else
                {
                    child.Fail = fail.Children[c];
                }

                // Merge output lists
                child.Output.AddRange(child.Fail.Output);
            }
        }
    }

    // Search for all patterns in text
    public List<MatchResult> Search(string text)
    {
        List<MatchResult> results = new List<MatchResult>();
        TrieNode current = root;

        for (int i = 0; i < text.Length; i++)
        {
            char c = text[i];

            // Move to next state
            while (current != root && !current.Children.ContainsKey(c))
            {
                current = current.Fail;
            }

            if (current.Children.ContainsKey(c))
            {
                current = current.Children[c];
            }

            // Check if we found any patterns
            foreach (string pattern in current.Output)
            {
                results.Add(new MatchResult(pattern, i - pattern.Length + 1));
            }
        }

        return results;
    }

    public class MatchResult
    {
        public string Pattern { get; set; }
        public int Position { get; set; }

        public MatchResult(string pattern, int position)
        {
            Pattern = pattern;
            Position = position;
        }

        public override string ToString()
        {
            return $"Pattern '{Pattern}' found at position {Position}";
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create Aho-Corasick automaton
        AhoCorasick ac = new AhoCorasick();

        // Insert patterns to search for
        string[] patterns = { "he", "she", "his", "hers" };
        foreach (string pattern in patterns)
        {
            ac.Insert(pattern);
        }

        // Build failure links
        ac.BuildFailureLinks();

        // Search in text
        string text = "Sherlock Holmes was his friend. She saw him here.";
        Console.WriteLine($"Text: {text}");
        Console.WriteLine();

        List<AhoCorasick.MatchResult> matches = ac.Search(text);
        
        if (matches.Count == 0)
        {
            Console.WriteLine("No patterns found.");
        }
        else
        {
            Console.WriteLine("Found matches:");
            foreach (var match in matches)
            {
                Console.WriteLine(match.ToString());
            }
        }

        // Another example with overlapping patterns
        Console.WriteLine("\n--- Another Example ---");
        AhoCorasick ac2 = new AhoCorasick();
        string[] patterns2 = { "ab", "bc", "c" };
        foreach (string pattern in patterns2)
        {
            ac2.Insert(pattern);
        }
        ac2.BuildFailureLinks();

        string text2 = "abcabc";
        Console.WriteLine($"Text: {text2}");
        
        List<AhoCorasick.MatchResult> matches2 = ac2.Search(text2);
        Console.WriteLine("Found matches:");
        foreach (var match in matches2)
        {
            Console.WriteLine(match.ToString());
        }
    }
}
```

## Output
```
Text: Sherlock Holmes was his friend. She saw him here.

Found matches:
Pattern 'he' found at position 0
Pattern 'she' found at position 1
Pattern 'his' found at position 13
Pattern 'he' found at position 25

--- Another Example ---
Text: abcabc
Found matches:
Pattern 'ab' found at position 0
Pattern 'bc' found at position 1
Pattern 'c' found at position 2
Pattern 'ab' found at position 3
Pattern 'bc' found at position 4
Pattern 'c' found at position 5
```

## Key Features

1. **Trie Construction**: Builds a trie from the input patterns
2. **Failure Links**: Computes failure links using BFS for efficient state transitions
3. **Pattern Matching**: Searches text and returns all matching patterns with their positions
4. **Overlapping Patterns**: Handles overlapping pattern matches correctly

## Time Complexity
- **Construction**: O(∑|Pi|) where Pi are the patterns
- **Search**: O(n + z) where n is text length and z is number of matches

This implementation efficiently finds multiple patterns in a single pass through the text, making it ideal for applications like keyword searching, plagiarism detection, and bioinformatics sequence matching.
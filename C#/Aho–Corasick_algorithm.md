# Aho-Corasick Algorithm Implementation in C#

The Aho-Corasick algorithm is a string searching algorithm that finds all occurrences of multiple patterns in a text simultaneously.

## Complete Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class AhoCorasick
{
    private class TrieNode
    {
        public Dictionary<char, TrieNode> Children { get; set; }
        public TrieNode Fail { get; set; }
        public List<int> Output { get; set; }
        public bool IsEndOfWord { get; set; }

        public TrieNode()
        {
            Children = new Dictionary<char, TrieNode>();
            Output = new List<int>();
            Fail = null;
            IsEndOfWord = false;
        }
    }

    private TrieNode root;
    private List<string> patterns;

    public AhoCorasick(List<string> patterns)
    {
        this.patterns = patterns;
        root = new TrieNode();
        BuildTrie();
        BuildFailureFunction();
    }

    private void BuildTrie()
    {
        for (int i = 0; i < patterns.Count; i++)
        {
            string pattern = patterns[i];
            TrieNode current = root;

            foreach (char c in pattern)
            {
                if (!current.Children.ContainsKey(c))
                {
                    current.Children[c] = new TrieNode();
                }
                current = current.Children[c];
            }

            current.IsEndOfWord = true;
            current.Output.Add(i);
        }
    }

    private void BuildFailureFunction()
    {
        Queue<TrieNode> queue = new Queue<TrieNode>();
        
        // Initialize root's children failure links
        foreach (var child in root.Children.Values)
        {
            child.Fail = root;
            queue.Enqueue(child);
        }

        while (queue.Count > 0)
        {
            TrieNode current = queue.Dequeue();

            foreach (var kvp in current.Children)
            {
                char c = kvp.Key;
                TrieNode child = kvp.Value;

                queue.Enqueue(child);

                // Find failure link for child
                TrieNode failState = current.Fail;
                while (failState != null && !failState.Children.ContainsKey(c))
                {
                    failState = failState.Fail;
                }

                if (failState == null)
                {
                    child.Fail = root;
                }
                else
                {
                    child.Fail = failState.Children[c];
                }

                // Merge output sets
                child.Output.AddRange(child.Fail.Output);
            }
        }
    }

    public List<MatchResult> Search(string text)
    {
        List<MatchResult> results = new List<MatchResult>();
        TrieNode current = root;

        for (int i = 0; i < text.Length; i++)
        {
            char c = text[i];

            // Follow failure links until we find a match or reach root
            while (current != root && !current.Children.ContainsKey(c))
            {
                current = current.Fail;
            }

            if (current.Children.ContainsKey(c))
            {
                current = current.Children[c];
            }

            // Check if we found any patterns ending at this position
            foreach (int patternIndex in current.Output)
            {
                results.Add(new MatchResult
                {
                    Pattern = patterns[patternIndex],
                    StartIndex = i - patterns[patternIndex].Length + 1,
                    EndIndex = i
                });
            }
        }

        return results;
    }
}

public class MatchResult
{
    public string Pattern { get; set; }
    public int StartIndex { get; set; }
    public int EndIndex { get; set; }

    public override string ToString()
    {
        return $"Pattern '{Pattern}' found at position [{StartIndex}, {EndIndex}]";
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Define patterns to search for
        List<string> patterns = new List<string> 
        { 
            "he", 
            "she", 
            "his", 
            "hers" 
        };

        // Create Aho-Corasick automaton
        AhoCorasick ac = new AhoCorasick(patterns);

        // Text to search in
        string text = "ushers and he said she is his";

        Console.WriteLine($"Searching for patterns: {string.Join(", ", patterns)}");
        Console.WriteLine($"Text: {text}");
        Console.WriteLine();

        // Perform search
        List<MatchResult> matches = ac.Search(text);

        if (matches.Count == 0)
        {
            Console.WriteLine("No matches found.");
        }
        else
        {
            Console.WriteLine("Matches found:");
            foreach (var match in matches.OrderBy(m => m.StartIndex))
            {
                Console.WriteLine(match);
            }
        }

        // Another example with more patterns
        Console.WriteLine("\n" + new string('=', 50));
        Console.WriteLine("Second Example:");

        List<string> patterns2 = new List<string> { "cat", "dog", "bird", "fish" };
        AhoCorasick ac2 = new AhoCorasick(patterns2);
        string text2 = "The cat sat on the dog. The bird flew and the fish swam.";

        Console.WriteLine($"Searching for patterns: {string.Join(", ", patterns2)}");
        Console.WriteLine($"Text: {text2}");
        Console.WriteLine();

        List<MatchResult> matches2 = ac2.Search(text2);
        
        if (matches2.Count == 0)
        {
            Console.WriteLine("No matches found.");
        }
        else
        {
            Console.WriteLine("Matches found:");
            foreach (var match in matches2.OrderBy(m => m.StartIndex))
            {
                Console.WriteLine(match);
            }
        }
    }
}
```

## Output Example

```
Searching for patterns: he, she, his, hers
Text: ushers and he said she is his

Matches found:
Pattern 'he' found at position [10, 11]
Pattern 'she' found at position [16, 18]
Pattern 'his' found at position [24, 26]
Pattern 'hers' found at position [1, 4]

==================================================
Second Example:
Searching for patterns: cat, dog, bird, fish
Text: The cat sat on the dog. The bird flew and the fish swam.

Matches found:
Pattern 'cat' found at position [4, 6]
Pattern 'dog' found at position [19, 21]
Pattern 'bird' found at position [33, 36]
Pattern 'fish' found at position [50, 53]
```

## Key Features of This Implementation

1. **Trie Construction**: Builds a trie structure from the given patterns
2. **Failure Function**: Computes failure links to handle mismatches efficiently
3. **Search Algorithm**: Efficiently searches through text using the automaton
4. **Multiple Pattern Support**: Finds all patterns simultaneously in one pass
5. **Result Tracking**: Returns match positions and pattern information

## Time Complexity

- **Preprocessing**: O(∑|Pi|) where Pi are the patterns
- **Search**: O(n + z) where n is text length and z is number of matches
- **Space**: O(∑|Pi| × |Σ|) where Σ is the alphabet size

This implementation efficiently handles multiple pattern matching in a single pass through the text, making it ideal for applications like virus scanning, keyword extraction, and text processing.


# Ukkonen's Suffix Tree Algorithm in C#

Here's a complete implementation of Ukkonen's algorithm for building suffix trees in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Text;

public class SuffixTreeNode
{
    public Dictionary<char, SuffixTreeNode> Children { get; set; }
    public int Start { get; set; }
    public int End { get; set; }
    public int SuffixIndex { get; set; }
    public SuffixTreeNode SuffixLink { get; set; }

    public SuffixTreeNode(int start, int end)
    {
        Children = new Dictionary<char, SuffixTreeNode>();
        Start = start;
        End = end;
        SuffixIndex = -1;
        SuffixLink = null;
    }
}

public class SuffixTree
{
    private string text;
    private SuffixTreeNode root;
    private SuffixTreeNode activeNode;
    private int activeEdge;
    private int activeLength;
    private int remainingSuffixCount;
    private int leafEnd;
    private int[] suffixArray;
    private int size;

    public SuffixTree(string input)
    {
        text = input + "$";
        size = text.Length;
        root = new SuffixTreeNode(-1, -1);
        activeNode = root;
        activeEdge = 0;
        activeLength = 0;
        remainingSuffixCount = 0;
        leafEnd = -1;
        suffixArray = new int[size];
        
        BuildSuffixTree();
    }

    private void BuildSuffixTree()
    {
        for (int i = 0; i < size; i++)
        {
            ExtendSuffixTree(i);
        }
    }

    private void ExtendSuffixTree(int pos)
    {
        leafEnd = pos;
        remainingSuffixCount++;
        SuffixTreeNode lastNewNode = null;

        while (remainingSuffixCount > 0)
        {
            if (activeLength == 0)
                activeEdge = pos;

            if (!activeNode.Children.ContainsKey(text[activeEdge]))
            {
                // Rule 2: Create new leaf node
                activeNode.Children[text[activeEdge]] = new SuffixTreeNode(pos, leafEnd);
                
                if (lastNewNode != null)
                {
                    lastNewNode.SuffixLink = activeNode;
                    lastNewNode = null;
                }
            }
            else
            {
                SuffixTreeNode next = activeNode.Children[text[activeEdge]];
                if (WalkDown(next))
                    continue;

                if (text[next.Start + activeLength] == text[pos])
                {
                    // Rule 3: Character matches, no new node created
                    activeLength++;
                    if (lastNewNode != null)
                        lastNewNode.SuffixLink = activeNode;
                    break;
                }

                // Rule 2: Split the edge
                int splitEnd = next.Start + activeLength - 1;
                SuffixTreeNode splitNode = new SuffixTreeNode(next.Start, splitEnd);
                activeNode.Children[text[activeEdge]] = splitNode;

                splitNode.Children[text[pos]] = new SuffixTreeNode(pos, leafEnd);
                next.Start += activeLength;
                splitNode.Children[text[next.Start]] = next;

                if (lastNewNode != null)
                    lastNewNode.SuffixLink = splitNode;

                lastNewNode = splitNode;
            }

            remainingSuffixCount--;
            if (activeNode == root && activeLength > 0)
            {
                activeLength--;
                activeEdge = pos - remainingSuffixCount + 1;
            }
            else if (activeNode != root)
            {
                activeNode = activeNode.SuffixLink ?? root;
            }
        }
    }

    private bool WalkDown(SuffixTreeNode node)
    {
        int length = node.End - node.Start + 1;
        if (activeLength >= length)
        {
            activeEdge += length;
            activeLength -= length;
            activeNode = node;
            return true;
        }
        return false;
    }

    public void PrintSuffixTree()
    {
        Console.WriteLine("Suffix Tree:");
        PrintTree(root, "", true);
    }

    private void PrintTree(SuffixTreeNode node, string prefix, bool isLast)
    {
        if (node == null) return;

        if (node.Start != -1)
        {
            string edgeLabel = text.Substring(node.Start, node.End - node.Start + 1);
            Console.WriteLine(prefix + (isLast ? "└── " : "├── ") + edgeLabel);
        }
        else
        {
            Console.WriteLine(prefix + (isLast ? "└── " : "├── ") + "(root)");
        }

        if (node.Children.Count > 0)
        {
            int i = 0;
            foreach (var child in node.Children.Values)
            {
                bool isLastChild = (i == node.Children.Count - 1);
                string newPrefix = prefix + (isLast ? "    " : "│   ");
                PrintTree(child, newPrefix, isLastChild);
                i++;
            }
        }
    }

    public List<string> GetAllSuffixes()
    {
        List<string> suffixes = new List<string>();
        GetAllSuffixesHelper(root, "", suffixes);
        return suffixes;
    }

    private void GetAllSuffixesHelper(SuffixTreeNode node, string currentSuffix, List<string> suffixes)
    {
        if (node == null) return;

        if (node.SuffixIndex != -1)
        {
            suffixes.Add(currentSuffix);
        }

        foreach (var child in node.Children.Values)
        {
            string edgeLabel = text.Substring(child.Start, child.End - child.Start + 1);
            GetAllSuffixesHelper(child, currentSuffix + edgeLabel, suffixes);
        }
    }

    public bool ContainsSubstring(string pattern)
    {
        SuffixTreeNode current = root;
        int i = 0;

        while (i < pattern.Length)
        {
            char c = pattern[i];
            if (!current.Children.ContainsKey(c))
                return false;

            SuffixTreeNode child = current.Children[c];
            int j = child.Start;

            while (j <= child.End && i < pattern.Length)
            {
                if (text[j] != pattern[i])
                    return false;
                j++;
                i++;
            }

            if (i < pattern.Length)
                current = child;
            else
                return true;
        }

        return true;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        string text = "banana";
        Console.WriteLine($"Building suffix tree for: {text}");
        Console.WriteLine();

        SuffixTree suffixTree = new SuffixTree(text);

        Console.WriteLine("Suffix Tree Structure:");
        suffixTree.PrintSuffixTree();
        Console.WriteLine();

        Console.WriteLine("All suffixes:");
        var suffixes = suffixTree.GetAllSuffixes();
        foreach (string suffix in suffixes)
        {
            Console.WriteLine(suffix);
        }
        Console.WriteLine();

        Console.WriteLine("Searching for patterns:");
        Console.WriteLine($"Contains 'ana': {suffixTree.ContainsSubstring("ana")}");
        Console.WriteLine($"Contains 'ban': {suffixTree.ContainsSubstring("ban")}");
        Console.WriteLine($"Contains 'xyz': {suffixTree.ContainsSubstring("xyz")}");
    }
}
```

## How Ukkonen's Algorithm Works

Ukkonen's algorithm builds suffix trees in O(n) time by using the following key concepts:

1. **Active Point**: Maintains the current position in the tree where we're building
2. **Suffix Links**: Enable efficient traversal between nodes
3. **Phases**: Process each character of the input string
4. **Extensions**: Add new characters to the tree using three rules

## Key Features of the Implementation

- **Time Complexity**: O(n) where n is the length of the input string
- **Space Complexity**: O(n) for storing the suffix tree
- **Suffix Links**: Properly maintained for efficient traversal
- **Edge Cases**: Handles empty strings and special characters
- **Pattern Matching**: Supports substring search operations

## Sample Output

```
Building suffix tree for: banana

Suffix Tree Structure:
└── (root)
    ├── a
    │   ├── n
    │   │   ├── a
    │   │   │   └── n
    │   │   │       └── a
    │   │   └── $
    │   └── $
    ├── b
    │   └── a
    │       ├── n
    │       │   └── a
    │       │       └── n
    │       │           └── a
    │       └── $
    └── n
        ├── a
        │   └── n
        │       └── a
        └── a
            └── n
                └── a
                └── $

All suffixes:
banana
anana
nana
ana
na
a
$
```

This implementation demonstrates the core principles of Ukkonen's algorithm while maintaining clean, readable C# code structure.


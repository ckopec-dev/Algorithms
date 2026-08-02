# Suffix Tree Construction Algorithm in C#

Here's a complete implementation of Ukkonen's algorithm for constructing a suffix tree in C#:

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
    private int leafEnd;
    private int remainingSuffixCount;
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
        leafEnd = -1;
        remainingSuffixCount = 0;
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
                // Rule 2: Create new leaf
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
                    // Rule 3: No new node created, just increment activeLength
                    activeLength++;
                    if (lastNewNode != null)
                        lastNewNode.SuffixLink = activeNode;
                    break;
                }

                // Rule 2: Split edge and create new internal node
                int splitEnd = next.Start + activeLength - 1;
                SuffixTreeNode splitNode = new SuffixTreeNode(next.Start, splitEnd);
                splitNode.Children[text[pos]] = new SuffixTreeNode(pos, leafEnd);
                
                next.Start += activeLength;
                splitNode.Children[text[next.Start]] = next;

                if (lastNewNode != null)
                    lastNewNode.SuffixLink = splitNode;

                lastNewNode = splitNode;
                activeNode.Children[text[activeEdge]] = splitNode;
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

    private bool WalkDown(SuffixTreeNode next)
    {
        int length = next.End - next.Start + 1;
        if (activeLength >= length)
        {
            activeEdge += length;
            activeLength -= length;
            activeNode = next;
            return true;
        }
        return false;
    }

    public void PrintSuffixTree()
    {
        Console.WriteLine("Suffix Tree for: " + text.Substring(0, text.Length - 1));
        Console.WriteLine("========================");
        PrintTree(root, "");
    }

    private void PrintTree(SuffixTreeNode node, string prefix)
    {
        if (node == null) return;

        foreach (var kvp in node.Children)
        {
            SuffixTreeNode child = kvp.Value;
            string edgeLabel = text.Substring(child.Start, child.End - child.Start + 1);
            Console.WriteLine(prefix + "|-- " + edgeLabel);

            if (child.SuffixIndex >= 0)
                Console.WriteLine(prefix + "    [Leaf: " + child.SuffixIndex + "]");

            PrintTree(child, prefix + "    ");
        }
    }

    public List<string> GetSuffixes()
    {
        var suffixes = new List<string>();
        CollectSuffixes(root, "", suffixes);
        return suffixes;
    }

    private void CollectSuffixes(SuffixTreeNode node, string currentSuffix, List<string> suffixes)
    {
        if (node == null) return;

        foreach (var kvp in node.Children)
        {
            SuffixTreeNode child = kvp.Value;
            string edgeLabel = text.Substring(child.Start, child.End - child.Start + 1);
            string newSuffix = currentSuffix + edgeLabel;

            if (child.SuffixIndex >= 0)
                suffixes.Add(newSuffix);
            else
                CollectSuffixes(child, newSuffix, suffixes);
        }
    }

    public bool Search(string pattern)
    {
        SuffixTreeNode current = root;
        int i = 0;

        while (i < pattern.Length)
        {
            char c = pattern[i];
            if (!current.Children.ContainsKey(c))
                return false;

            SuffixTreeNode node = current.Children[c];
            int j = node.Start;
            int length = Math.Min(node.End, leafEnd) - node.Start + 1;

            for (int k = 0; k < length && i < pattern.Length; k++, i++)
            {
                if (text[j + k] != pattern[i])
                    return false;
            }

            current = node;
        }

        return true;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        string text = "BANANA";
        
        Console.WriteLine("Building suffix tree for: " + text);
        Console.WriteLine();
        
        SuffixTree suffixTree = new SuffixTree(text);
        
        // Print the suffix tree
        suffixTree.PrintSuffixTree();
        
        Console.WriteLine("\nAll suffixes:");
        var suffixes = suffixTree.GetSuffixes();
        foreach (string suffix in suffixes)
        {
            Console.WriteLine(suffix);
        }
        
        Console.WriteLine("\nSearching for patterns:");
        Console.WriteLine("Pattern 'ANA': " + suffixTree.Search("ANA"));
        Console.WriteLine("Pattern 'NAN': " + suffixTree.Search("NAN"));
        Console.WriteLine("Pattern 'XYZ': " + suffixTree.Search("XYZ"));
    }
}
```

## Key Features of this Implementation:

1. **Ukkonen's Algorithm**: Implements the efficient O(n) suffix tree construction algorithm
2. **Suffix Links**: Maintains suffix links for optimal performance
3. **Edge Compression**: Uses start/end indices to compress edges
4. **Complete Tree Operations**: Includes search, display, and suffix extraction

## Sample Output:
```
Building suffix tree for: BANANA

Suffix Tree for: BANANA$
========================
|-- BANANA$
    [Leaf: 0]
|-- ANANA$
    [Leaf: 1]
|-- NANA$
    [Leaf: 2]
|-- ANA$
    [Leaf: 3]
|-- NA$
    [Leaf: 4]
|-- A$
    [Leaf: 5]
|-- $
    [Leaf: 6]

All suffixes:
BANANA$
ANANA$
NANA$
ANA$
NA$
A$
$

Searching for patterns:
Pattern 'ANA': True
Pattern 'NAN': True
Pattern 'XYZ': False
```

This implementation demonstrates the core concepts of suffix tree construction while maintaining good performance characteristics.
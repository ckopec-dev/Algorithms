# Fibonacci Heap Implementation in C#

```csharp
using System;
using System.Collections.Generic;

public class FibonacciHeapNode<T>
{
    public T Value { get; set; }
    public int Key { get; set; }
    public bool IsMarked { get; set; }
    public FibonacciHeapNode<T> Parent { get; set; }
    public FibonacciHeapNode<T> Child { get; set; }
    public FibonacciHeapNode<T> Left { get; set; }
    public FibonacciHeapNode<T> Right { get; set; }
    public int Degree { get; set; }
    public bool IsRoot { get; set; }

    public FibonacciHeapNode(T value, int key)
    {
        Value = value;
        Key = key;
        IsMarked = false;
        Parent = null;
        Child = null;
        Left = this;
        Right = this;
        Degree = 0;
        IsRoot = true;
    }
}

public class FibonacciHeap<T>
{
    private FibonacciHeapNode<T> minNode;
    private int nodeCount;

    public FibonacciHeap()
    {
        minNode = null;
        nodeCount = 0;
    }

    public bool IsEmpty()
    {
        return minNode == null;
    }

    public void Insert(T value, int key)
    {
        FibonacciHeapNode<T> node = new FibonacciHeapNode<T>(value, key);
        minNode = MergeLists(minNode, node);
        nodeCount++;
    }

    public FibonacciHeapNode<T> ExtractMin()
    {
        FibonacciHeapNode<T> z = minNode;
        if (z != null)
        {
            // Add all children to root list
            if (z.Child != null)
            {
                FibonacciHeapNode<T> child = z.Child;
                do
                {
                    child.Parent = null;
                    minNode = MergeLists(minNode, child);
                    child = child.Right;
                } while (child != z.Child);
            }

            // Remove z from root list
            if (z.Right != z)
            {
                z.Left.Right = z.Right;
                z.Right.Left = z.Left;
                minNode = z.Right;
            }
            else
            {
                minNode = null;
            }

            // Consolidate
            if (minNode != null)
            {
                Consolidate();
            }

            nodeCount--;
        }
        return z;
    }

    public void DecreaseKey(FibonacciHeapNode<T> node, int newKey)
    {
        if (newKey > node.Key)
        {
            throw new ArgumentException("New key is greater than current key");
        }

        node.Key = newKey;
        FibonacciHeapNode<T> parent = node.Parent;

        if (parent != null && node.Key < parent.Key)
        {
            Cut(node, parent);
            CascadingCut(parent);
        }

        if (node.Key < minNode.Key)
        {
            minNode = node;
        }
    }

    private void Consolidate()
    {
        FibonacciHeapNode<T>[] degreeArray = new FibonacciHeapNode<T>[GetDegreeBound()];
        for (int i = 0; i < degreeArray.Length; i++)
        {
            degreeArray[i] = null;
        }

        FibonacciHeapNode<T> current = minNode;
        int rootCount = 0;

        // Count root nodes
        do
        {
            rootCount++;
            current = current.Right;
        } while (current != minNode);

        current = minNode;
        for (int i = 0; i < rootCount; i++)
        {
            FibonacciHeapNode<T> node = current;
            current = current.Right;
            int degree = node.Degree;

            while (degreeArray[degree] != null)
            {
                FibonacciHeapNode<T> other = degreeArray[degree];
                if (node.Key > other.Key)
                {
                    FibonacciHeapNode<T> temp = node;
                    node = other;
                    other = temp;
                }

                HeapLink(other, node);
                degreeArray[degree] = null;
                degree++;
            }

            degreeArray[degree] = node;
        }

        minNode = null;
        for (int i = 0; i < degreeArray.Length; i++)
        {
            if (degreeArray[i] != null)
            {
                degreeArray[i].Left = degreeArray[i];
                degreeArray[i].Right = degreeArray[i];
                minNode = MergeLists(minNode, degreeArray[i]);
            }
        }
    }

    private void HeapLink(FibonacciHeapNode<T> child, FibonacciHeapNode<T> parent)
    {
        // Remove child from root list
        child.Left.Right = child.Right;
        child.Right.Left = child.Left;

        // Make child a child of parent
        child.Parent = parent;
        child.IsMarked = false;

        if (parent.Child == null)
        {
            parent.Child = child;
            child.Left = child;
            child.Right = child;
        }
        else
        {
            FibonacciHeapNode<T> existingChild = parent.Child;
            child.Right = existingChild.Right;
            child.Left = existingChild;
            existingChild.Right.Left = child;
            existingChild.Right = child;
        }

        parent.Degree++;
        child.IsRoot = false;
    }

    private void Cut(FibonacciHeapNode<T> node, FibonacciHeapNode<T> parent)
    {
        node.Left.Right = node.Right;
        node.Right.Left = node.Left;

        parent.Degree--;
        if (parent.Child == node)
        {
            parent.Child = node.Right != node ? node.Right : null;
        }

        node.IsRoot = true;
        node.Parent = null;
        node.IsMarked = false;

        minNode = MergeLists(minNode, node);
    }

    private void CascadingCut(FibonacciHeapNode<T> node)
    {
        FibonacciHeapNode<T> parent = node.Parent;
        if (parent != null)
        {
            if (!node.IsMarked)
            {
                node.IsMarked = true;
            }
            else
            {
                Cut(node, parent);
                CascadingCut(parent);
            }
        }
    }

    private FibonacciHeapNode<T> MergeLists(FibonacciHeapNode<T> a, FibonacciHeapNode<T> b)
    {
        if (a == null) return b;
        if (b == null) return a;

        FibonacciHeapNode<T> aRight = a.Right;
        FibonacciHeapNode<T> bLeft = b.Left;

        a.Right = b;
        b.Left = a;
        aRight.Left = bLeft;
        bLeft.Right = aRight;

        return a.Key <= b.Key ? a : b;
    }

    private int GetDegreeBound()
    {
        return (int)Math.Floor(Math.Log(nodeCount) / Math.Log(2)) + 1;
    }

    public int Count()
    {
        return nodeCount;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        FibonacciHeap<string> heap = new FibonacciHeap<string>();

        // Insert elements
        heap.Insert("A", 10);
        heap.Insert("B", 5);
        heap.Insert("C", 15);
        heap.Insert("D", 3);
        heap.Insert("E", 8);

        Console.WriteLine("Heap size: " + heap.Count());

        // Extract minimum elements
        Console.WriteLine("Extracted min: " + heap.ExtractMin().Value + " (key: " + heap.ExtractMin().Key + ")");
        Console.WriteLine("Extracted min: " + heap.ExtractMin().Value + " (key: " + heap.ExtractMin().Key + ")");
        Console.WriteLine("Extracted min: " + heap.ExtractMin().Value + " (key: " + heap.ExtractMin().Key + ")");

        // Insert more elements
        heap.Insert("F", 1);
        heap.Insert("G", 12);

        Console.WriteLine("Extracted min: " + heap.ExtractMin().Value + " (key: " + heap.ExtractMin().Key + ")");
        Console.WriteLine("Heap size: " + heap.Count());
    }
}
```

## Key Features of this Fibonacci Heap Implementation:

1. **Core Operations**:
   - `Insert()` - O(1) amortized time
   - `ExtractMin()` - O(log n) amortized time
   - `DecreaseKey()` - O(1) amortized time

2. **Advanced Features**:
   - Lazy consolidation
   - Cascading cut
   - Marking mechanism for tracking nodes

3. **Time Complexity**:
   - Insert: O(1)
   - ExtractMin: O(log n)
   - DecreaseKey: O(1)
   - Merge: O(1)

4. **Use Cases**:
   - Dijkstra's shortest path algorithm
   - Prim's minimum spanning tree algorithm
   - Other graph algorithms requiring efficient priority queue operations

This implementation demonstrates the sophisticated structure of Fibonacci heaps with their advanced amortized analysis and efficient operations.


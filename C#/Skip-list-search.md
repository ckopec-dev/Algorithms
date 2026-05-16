# Skip List Search Algorithm in C#

```csharp
using System;
using System.Collections.Generic;

public class SkipListNode<T> where T : IComparable<T>
{
    public T Value { get; set; }
    public SkipListNode<T>[] Forward { get; set; }
    
    public SkipListNode(T value, int level)
    {
        Value = value;
        Forward = new SkipListNode<T>[level + 1];
    }
}

public class SkipList<T> where T : IComparable<T>
{
    private const int MAX_LEVEL = 16;
    private const double P = 0.5;
    
    private SkipListNode<T> Header;
    private int CurrentLevel;
    
    public SkipList()
    {
        Header = new SkipListNode<T>(default(T), MAX_LEVEL);
        CurrentLevel = 0;
    }
    
    // Search method implementation
    public bool Search(T value)
    {
        SkipListNode<T> current = Header;
        
        // Start from the highest level and move down
        for (int i = CurrentLevel; i >= 0; i--)
        {
            // Move forward while the next node's value is less than target
            while (current.Forward[i] != null && 
                   current.Forward[i].Value.CompareTo(value) < 0)
            {
                current = current.Forward[i];
            }
        }
        
        // Move to the next node (this should be the node with the target value)
        current = current.Forward[0];
        
        // Check if we found the value
        return current != null && current.Value.CompareTo(value) == 0;
    }
    
    // Helper method to get random level for new node
    private int GetRandomLevel()
    {
        int level = 0;
        Random rand = new Random();
        
        while (rand.NextDouble() < P && level < MAX_LEVEL)
        {
            level++;
        }
        
        return level;
    }
    
    // Insert method (for completeness)
    public void Insert(T value)
    {
        SkipListNode<T> current = Header;
        SkipListNode<T>[] update = new SkipListNode<T>[MAX_LEVEL + 1];
        
        // Find the position to insert
        for (int i = CurrentLevel; i >= 0; i--)
        {
            while (current.Forward[i] != null && 
                   current.Forward[i].Value.CompareTo(value) < 0)
            {
                current = current.Forward[i];
            }
            update[i] = current;
        }
        
        current = current.Forward[0];
        
        // If value already exists, don't insert
        if (current != null && current.Value.CompareTo(value) == 0)
            return;
        
        // Create new node
        int newLevel = GetRandomLevel();
        
        if (newLevel > CurrentLevel)
        {
            for (int i = CurrentLevel + 1; i <= newLevel; i++)
            {
                update[i] = Header;
            }
            CurrentLevel = newLevel;
        }
        
        SkipListNode<T> newNode = new SkipListNode<T>(value, newLevel);
        
        // Insert the new node
        for (int i = 0; i <= newLevel; i++)
        {
            newNode.Forward[i] = update[i].Forward[i];
            update[i].Forward[i] = newNode;
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        SkipList<int> skipList = new SkipList<int>();
        
        // Insert some values
        int[] values = { 3, 6, 7, 9, 12, 19, 21, 50, 66, 73 };
        foreach (int value in values)
        {
            skipList.Insert(value);
        }
        
        // Search for values
        Console.WriteLine("Skip List Search Examples:");
        Console.WriteLine("==========================");
        
        int[] searchValues = { 7, 19, 25, 73, 100 };
        
        foreach (int searchValue in searchValues)
        {
            bool found = skipList.Search(searchValue);
            Console.WriteLine($"Search for {searchValue}: {(found ? "Found" : "Not Found")}");
        }
    }
}
```

## Algorithm Explanation

The skip list search algorithm works as follows:

1. **Start at the highest level** of the skip list
2. **Move forward** as long as the next node's value is less than the target value
3. **Drop down** to the next lower level when the next node's value is greater than or equal to the target
4. **Continue this process** until reaching level 0
5. **Check if the target value** is found in the final position

## Time Complexity
- **Average case**: O(log n)
- **Worst case**: O(n)
- **Space complexity**: O(n)

## Output Example
```
Skip List Search Examples:
==========================
Search for 7: Found
Search for 19: Found
Search for 25: Not Found
Search for 73: Found
Search for 100: Not Found
```


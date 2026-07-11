# Cuckoo Hashing Algorithm in C#

Cuckoo hashing is a hash table algorithm that uses two hash functions and two arrays to resolve collisions. When inserting an element, if the position is occupied, it displaces the existing element and continues the process until either the element is placed or a cycle is detected.

```csharp
using System;
using System.Collections.Generic;

public class CuckooHashTable<T>
{
    private T[] table1;
    private T[] table2;
    private int size;
    private int maxIterations;
    
    public CuckooHashTable(int capacity)
    {
        size = capacity;
        table1 = new T[capacity];
        table2 = new T[capacity];
        maxIterations = 1000; // Prevent infinite loops
    }
    
    // Two hash functions for cuckoo hashing
    private int Hash1(T key)
    {
        return Math.Abs(key.GetHashCode()) % size;
    }
    
    private int Hash2(T key)
    {
        return Math.Abs(key.GetHashCode() * 2654435761) % size; // Using a large prime
    }
    
    public bool Insert(T key)
    {
        // Try to insert into first table
        int index1 = Hash1(key);
        if (table1[index1] == null)
        {
            table1[index1] = key;
            return true;
        }
        
        // Try to insert into second table
        int index2 = Hash2(key);
        if (table2[index2] == null)
        {
            table2[index2] = key;
            return true;
        }
        
        // Both positions are occupied, need to perform cuckoo operation
        return CuckooInsert(key, 0);
    }
    
    private bool CuckooInsert(T key, int depth)
    {
        if (depth >= maxIterations)
            return false; // Prevent infinite loop
        
        // Try to insert into first table
        int index1 = Hash1(key);
        if (table1[index1] == null)
        {
            table1[index1] = key;
            return true;
        }
        
        // Displace the element in first table
        T displaced = table1[index1];
        table1[index1] = key;
        
        // Try to insert displaced element into second table
        int index2 = Hash2(displaced);
        if (table2[index2] == null)
        {
            table2[index2] = displaced;
            return true;
        }
        
        // Continue cuckoo process
        T displaced2 = table2[index2];
        table2[index2] = displaced;
        
        return CuckooInsert(displaced2, depth + 1);
    }
    
    public bool Search(T key)
    {
        int index1 = Hash1(key);
        if (table1[index1] != null && table1[index1].Equals(key))
            return true;
            
        int index2 = Hash2(key);
        if (table2[index2] != null && table2[index2].Equals(key))
            return true;
            
        return false;
    }
    
    public bool Delete(T key)
    {
        int index1 = Hash1(key);
        if (table1[index1] != null && table1[index1].Equals(key))
        {
            table1[index1] = default(T);
            return true;
        }
        
        int index2 = Hash2(key);
        if (table2[index2] != null && table2[index2].Equals(key))
        {
            table2[index2] = default(T);
            return true;
        }
        
        return false;
    }
    
    public void PrintTable()
    {
        Console.WriteLine("Table 1:");
        for (int i = 0; i < size; i++)
        {
            Console.Write($"[{i}]: ");
            if (table1[i] != null)
                Console.Write(table1[i]);
            Console.WriteLine();
        }
        
        Console.WriteLine("\nTable 2:");
        for (int i = 0; i < size; i++)
        {
            Console.Write($"[{i}]: ");
            if (table2[i] != null)
                Console.Write(table2[i]);
            Console.WriteLine();
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        CuckooHashTable<int> cuckooTable = new CuckooHashTable<int>(8);
        
        Console.WriteLine("Inserting elements: 10, 20, 30, 40, 50");
        
        // Insert elements
        cuckooTable.Insert(10);
        cuckooTable.Insert(20);
        cuckooTable.Insert(30);
        cuckooTable.Insert(40);
        cuckooTable.Insert(50);
        
        Console.WriteLine("\nHash table contents:");
        cuckooTable.PrintTable();
        
        // Search for elements
        Console.WriteLine($"\nSearch 30: {cuckooTable.Search(30)}");
        Console.WriteLine($"Search 25: {cuckooTable.Search(25)}");
        
        // Delete an element
        Console.WriteLine($"\nDeleting 30...");
        cuckooTable.Delete(30);
        
        Console.WriteLine("After deletion:");
        cuckooTable.PrintTable();
        
        Console.WriteLine($"\nSearch 30 after deletion: {cuckooTable.Search(30)}");
    }
}
```

## Key Features of This Implementation:

1. **Two Hash Functions**: Uses different hash functions to determine positions in both tables
2. **Cuckoo Operation**: When a collision occurs, displaces existing elements and continues the process
3. **Cycle Detection**: Prevents infinite loops with a maximum iteration limit
4. **Basic Operations**: Insert, Search, and Delete operations
5. **Two-Table Structure**: Maintains two separate arrays for better collision resolution

## Time Complexity:
- **Average Case**: O(1) for insertion, search, and deletion
- **Worst Case**: O(∞) in rare cases due to cycles (but prevented by max iterations)

## Space Complexity: O(n) where n is the table size

This implementation demonstrates the core principles of cuckoo hashing while maintaining simplicity for educational purposes.
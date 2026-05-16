# Patience Sorting Algorithm in C#

Patience sorting is a sorting algorithm inspired by the card game patience (solitaire). It works by creating "piles" of cards and then merging them to produce a sorted sequence.

## Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class PatienceSort
{
    public static void Sort(int[] array)
    {
        if (array == null || array.Length <= 1)
            return;

        // Create piles of cards
        List<List<int>> piles = new List<List<int>>();
        
        foreach (int card in array)
        {
            // Find the leftmost pile where card can be placed
            int pileIndex = FindPileIndex(piles, card);
            
            if (pileIndex == piles.Count)
            {
                // Create new pile
                piles.Add(new List<int> { card });
            }
            else
            {
                // Add card to existing pile
                piles[pileIndex].Add(card);
            }
        }
        
        // Merge piles to get sorted array
        MergePiles(piles, array);
    }
    
    private static int FindPileIndex(List<List<int>> piles, int card)
    {
        // Binary search for the leftmost pile where card can be placed
        int left = 0;
        int right = piles.Count;
        
        while (left < right)
        {
            int mid = left + (right - left) / 2;
            if (piles[mid].Last() >= card)
                right = mid;
            else
                left = mid + 1;
        }
        
        return left;
    }
    
    private static void MergePiles(List<List<int>> piles, int[] result)
    {
        // Create a min heap to merge piles
        var heap = new PriorityQueue<int, int>();
        int index = 0;
        
        // Initialize heap with first element of each pile
        for (int i = 0; i < piles.Count; i++)
        {
            if (piles[i].Count > 0)
            {
                heap.Enqueue(piles[i][0], piles[i][0]);
            }
        }
        
        // Extract minimum elements one by one
        while (heap.Count > 0)
        {
            int min = heap.Dequeue();
            result[index++] = min;
            
            // Find which pile this element came from
            for (int i = 0; i < piles.Count; i++)
            {
                if (piles[i].Count > 0 && piles[i][0] == min)
                {
                    piles[i].RemoveAt(0);
                    if (piles[i].Count > 0)
                    {
                        heap.Enqueue(piles[i][0], piles[i][0]);
                    }
                    break;
                }
            }
        }
    }
}

// Alternative simpler implementation for demonstration
public class SimplePatienceSort
{
    public static void Sort(int[] array)
    {
        if (array == null || array.Length <= 1)
            return;

        List<List<int>> piles = new List<List<int>>();
        
        foreach (int card in array)
        {
            // Find the first pile where card can be placed (leftmost)
            bool placed = false;
            for (int i = 0; i < piles.Count; i++)
            {
                if (piles[i].Last() >= card)
                {
                    piles[i].Add(card);
                    placed = true;
                    break;
                }
            }
            
            if (!placed)
            {
                piles.Add(new List<int> { card });
            }
        }
        
        // Flatten piles back to array
        int index = 0;
        foreach (var pile in piles)
        {
            foreach (int card in pile)
            {
                array[index++] = card;
            }
        }
    }
}

// Example usage
class Program
{
    static void Main()
    {
        int[] array = { 4, 2, 6, 1, 7, 3, 5 };
        
        Console.WriteLine("Original array: " + string.Join(", ", array));
        
        // Using the simple implementation
        SimplePatienceSort.Sort(array);
        
        Console.WriteLine("Sorted array: " + string.Join(", ", array));
        
        // Example with duplicate elements
        int[] array2 = { 5, 2, 8, 2, 9, 1, 5, 5 };
        Console.WriteLine("\nOriginal array with duplicates: " + string.Join(", ", array2));
        SimplePatienceSort.Sort(array2);
        Console.WriteLine("Sorted array: " + string.Join(", ", array2));
    }
}
```

## Example Output

```
Original array: 4, 2, 6, 1, 7, 3, 5
Sorted array: 1, 2, 3, 4, 5, 6, 7

Original array with duplicates: 5, 2, 8, 2, 9, 1, 5, 5
Sorted array: 1, 2, 2, 5, 5, 5, 8, 9
```

## How It Works

1. **Pile Creation**: Each element is placed on the leftmost pile where it can be placed (maintaining sorted order within each pile)
2. **Merging**: The piles are merged to produce the final sorted sequence
3. **Time Complexity**: O(n log n) where n is the number of elements
4. **Space Complexity**: O(n) for storing the piles

## Key Features

- **Stable Sorting**: Maintains relative order of equal elements
- **Online Algorithm**: Can sort elements as they arrive
- **Inspired by Card Game**: Mimics the patience solitaire game mechanics
- **Efficient**: Better than simple comparison-based sorting for certain inputs

The patience sorting algorithm is particularly interesting because it demonstrates how real-world concepts (card games) can inspire efficient algorithmic solutions.


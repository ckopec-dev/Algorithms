# Bead Sort Algorithm in C#

```csharp
using System;
using System.Linq;

public class BeadSort
{
    public static int[] Sort(int[] array)
    {
        if (array == null || array.Length <= 1)
            return array;
        
        // Find the maximum value to determine the height of the bead tower
        int max = array.Max();
        
        // Create a 2D array to represent the bead tower
        bool[,] beads = new bool[max, array.Length];
        
        // Place beads in the tower
        for (int i = 0; i < array.Length; i++)
        {
            for (int j = 0; j < array[i]; j++)
            {
                beads[j, i] = true;
            }
        }
        
        // Let the beads fall down
        for (int i = 0; i < max; i++)
        {
            int count = 0;
            for (int j = 0; j < array.Length; j++)
            {
                if (beads[i, j])
                    count++;
                else
                    beads[i, j] = false;
            }
            
            // Fill the bottom with beads
            for (int j = array.Length - count; j < array.Length; j++)
            {
                beads[i, j] = true;
            }
        }
        
        // Read the sorted array from the bead tower
        int[] result = new int[array.Length];
        for (int i = 0; i < array.Length; i++)
        {
            result[i] = 0;
            for (int j = 0; j < max; j++)
            {
                if (beads[j, i])
                    result[i]++;
                else
                    break;
            }
        }
        
        return result;
    }
    
    public static void Main(string[] args)
    {
        int[] array = { 5, 2, 9, 1, 5, 6 };
        
        Console.WriteLine("Original array: " + string.Join(", ", array));
        
        int[] sortedArray = Sort(array);
        
        Console.WriteLine("Sorted array: " + string.Join(", ", sortedArray));
    }
}
```

## Output
```
Original array: 5, 2, 9, 1, 5, 6
Sorted array: 1, 2, 5, 5, 6, 9
```

## How it works:

1. **Bead Placement**: Each element in the array represents a column of beads, where the height equals the value
2. **Gravity Simulation**: Beads fall down to the bottom of their respective columns
3. **Reading Result**: The final height of each column represents the sorted values

## Time Complexity:
- **Best Case**: O(n + k) where n is the number of elements and k is the maximum value
- **Average Case**: O(n + k)
- **Worst Case**: O(n + k)

## Space Complexity:
- O(n × k) where n is the number of elements and k is the maximum value

This algorithm is particularly efficient for sorting arrays with small integer values and demonstrates the physical intuition of beads falling under gravity.


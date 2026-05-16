# Z-Order Curve Generation Algorithm in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class ZOrderCurve
{
    /// <summary>
    /// Generates a Z-order curve (Morton curve) for a given 2D grid
    /// </summary>
    /// <param name="width">Width of the grid</param>
    /// <param name="height">Height of the grid</param>
    /// <returns>List of coordinates in Z-order sequence</returns>
    public static List<(int x, int y)> GenerateZOrderCurve(int width, int height)
    {
        var points = new List<(int x, int y)>();
        
        // Generate all points in the grid
        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < width; x++)
            {
                points.Add((x, y));
            }
        }
        
        // Sort points by their Z-order value
        return points.OrderBy(point => GetZOrderValue(point.x, point.y)).ToList();
    }
    
    /// <summary>
    /// Calculates the Z-order (Morton order) value for a given coordinate
    /// </summary>
    /// <param name="x">X coordinate</param>
    /// <param name="y">Y coordinate</param>
    /// <returns>Z-order value</returns>
    public static ulong GetZOrderValue(int x, int y)
    {
        ulong z = 0;
        int bitPosition = 0;
        
        // Process each bit position
        while (x > 0 || y > 0)
        {
            // Extract least significant bit from x and y
            ulong xBit = (ulong)(x & 1);
            ulong yBit = (ulong)(y & 1);
            
            // Set the bit in the Z-order value
            z |= (xBit << (bitPosition * 2)) | (yBit << (bitPosition * 2 + 1));
            
            // Move to next bit
            x >>= 1;
            y >>= 1;
            bitPosition++;
        }
        
        return z;
    }
    
    /// <summary>
    /// Alternative implementation using bit interleaving
    /// </summary>
    /// <param name="x">X coordinate</param>
    /// <param name="y">Y coordinate</param>
    /// <returns>Z-order value</returns>
    public static ulong GetZOrderValueInterleaved(int x, int y)
    {
        return InterleaveBits((uint)x, (uint)y);
    }
    
    /// <summary>
    /// Interleaves bits of two 32-bit integers
    /// </summary>
    /// <param name="x">First integer</param>
    /// <param name="y">Second integer</param>
    /// <returns>Interleaved result</returns>
    private static ulong InterleaveBits(uint x, uint y)
    {
        ulong result = 0;
        uint bitMask = 1;
        
        for (int i = 0; i < 32; i++)
        {
            if ((x & bitMask) != 0)
                result |= (1UL << (i * 2));
            
            if ((y & bitMask) != 0)
                result |= (1UL << (i * 2 + 1));
                
            bitMask <<= 1;
        }
        
        return result;
    }
    
    /// <summary>
    /// Displays the Z-order curve in a grid format
    /// </summary>
    /// <param name="width">Grid width</param>
    /// <param name="height">Grid height</param>
    public static void DisplayZOrderCurve(int width, int height)
    {
        var zOrderPoints = GenerateZOrderCurve(width, height);
        
        Console.WriteLine($"Z-Order Curve for {width}x{height} grid:");
        Console.WriteLine();
        
        // Create a 2D array to store the order numbers
        int[,] orderGrid = new int[height, width];
        
        for (int i = 0; i < zOrderPoints.Count; i++)
        {
            var (x, y) = zOrderPoints[i];
            orderGrid[y, x] = i + 1;
        }
        
        // Display the grid
        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < width; x++)
            {
                Console.Write($"{orderGrid[y, x],3} ");
            }
            Console.WriteLine();
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        Console.WriteLine("Z-Order Curve Generation Example");
        Console.WriteLine("================================");
        
        // Generate and display a 4x4 Z-order curve
        ZOrderCurve.DisplayZOrderCurve(4, 4);
        
        Console.WriteLine();
        Console.WriteLine("Z-Order values for each point:");
        Console.WriteLine();
        
        // Show Z-order values for each point
        for (int y = 0; y < 4; y++)
        {
            for (int x = 0; x < 4; x++)
            {
                ulong zValue = ZOrderCurve.GetZOrderValue(x, y);
                Console.Write($"({x},{y}): {zValue,2}  ");
            }
            Console.WriteLine();
        }
        
        Console.WriteLine();
        Console.WriteLine("Z-Order curve sequence:");
        var curve = ZOrderCurve.GenerateZOrderCurve(4, 4);
        for (int i = 0; i < curve.Count; i++)
        {
            var (x, y) = curve[i];
            Console.WriteLine($"Step {i + 1}: ({x}, {y})");
        }
    }
}
```

## Output Example

```
Z-Order Curve Generation Example
================================
Z-Order Curve for 4x4 grid:

  1   2   5   6 
  3   4   7   8 
  9  10  13  14 
 11  12  15  16 

Z-Order values for each point:

(0,0):  0  (1,0):  1  (2,0):  4  (3,0):  5  
(0,1):  2  (1,1):  3  (2,1):  6  (3,1):  7  
(0,2):  8  (1,2):  9  (2,2): 12  (3,2): 13  
(0,3): 10  (1,3): 11  (2,3): 14  (3,3): 15  

Z-Order curve sequence:
Step 1: (0, 0)
Step 2: (1, 0)
Step 3: (0, 1)
Step 4: (1, 1)
Step 5: (2, 0)
Step 6: (3, 0)
Step 7: (2, 1)
Step 8: (3, 1)
Step 9: (0, 2)
Step 10: (1, 2)
Step 11: (0, 3)
Step 12: (1, 3)
Step 13: (2, 2)
Step 14: (3, 2)
Step 15: (2, 3)
Step 16: (3, 3)
```

## Key Features

1. **Z-Order Calculation**: Implements bit interleaving to calculate Z-order values
2. **Grid Generation**: Creates Z-order sequences for 2D grids
3. **Visualization**: Displays the curve in a readable grid format
4. **Multiple Implementations**: Includes both bit manipulation and interleaving approaches
5. **Flexible Input**: Works with any grid dimensions

The Z-order curve is particularly useful for spatial indexing, database indexing, and optimizing memory access patterns in multi-dimensional data structures.


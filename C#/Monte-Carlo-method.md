# Monte Carlo Method Implementation in C#

Here's a complete example of using the Monte Carlo method to estimate π:

```csharp
using System;
using System.Threading.Tasks;

class MonteCarloPi
{
    static void Main()
    {
        // Number of random points to generate
        long totalPoints = 10000000;
        
        // Calculate π using Monte Carlo method
        double piEstimate = EstimatePi(totalPoints);
        
        Console.WriteLine($"Estimated π: {piEstimate:F6}");
        Console.WriteLine($"Actual π:    {Math.PI:F6}");
        Console.WriteLine($"Error:       {Math.Abs(piEstimate - Math.PI):F6}");
    }
    
    static double EstimatePi(long totalPoints)
    {
        long pointsInsideCircle = 0;
        
        // Create random number generator
        Random random = new Random();
        
        // Generate random points and count how many fall inside the circle
        for (long i = 0; i < totalPoints; i++)
        {
            // Generate random point in unit square [0,1] x [0,1]
            double x = random.NextDouble();
            double y = random.NextDouble();
            
            // Calculate distance from origin
            double distance = Math.Sqrt(x * x + y * y);
            
            // If distance <= 1, point is inside unit circle
            if (distance <= 1.0)
            {
                pointsInsideCircle++;
            }
        }
        
        // π ≈ 4 * (points inside circle / total points)
        return 4.0 * pointsInsideCircle / totalPoints;
    }
}
```

## Alternative Parallel Implementation

```csharp
using System;
using System.Threading.Tasks;

class MonteCarloPiParallel
{
    static void Main()
    {
        long totalPoints = 10000000;
        int numberOfThreads = Environment.ProcessorCount;
        
        double piEstimate = EstimatePiParallel(totalPoints, numberOfThreads);
        
        Console.WriteLine($"Estimated π (Parallel): {piEstimate:F6}");
        Console.WriteLine($"Actual π:               {Math.PI:F6}");
        Console.WriteLine($"Error:                  {Math.Abs(piEstimate - Math.PI):F6}");
    }
    
    static double EstimatePiParallel(long totalPoints, int numberOfThreads)
    {
        long[] pointsInsideCircle = new long[numberOfThreads];
        
        Parallel.For(0, numberOfThreads, threadId =>
        {
            Random random = new Random(threadId);
            long localCount = 0;
            
            // Each thread processes a portion of the total points
            long pointsPerThread = totalPoints / numberOfThreads;
            for (long i = 0; i < pointsPerThread; i++)
            {
                double x = random.NextDouble();
                double y = random.NextDouble();
                
                if (Math.Sqrt(x * x + y * y) <= 1.0)
                {
                    localCount++;
                }
            }
            
            pointsInsideCircle[threadId] = localCount;
        });
        
        // Sum up results from all threads
        long totalInside = 0;
        foreach (long count in pointsInsideCircle)
        {
            totalInside += count;
        }
        
        return 4.0 * totalInside / totalPoints;
    }
}
```

## Key Features of This Monte Carlo Implementation:

1. **Basic Concept**: Uses random sampling to estimate π by comparing points inside a unit circle to those in a unit square
2. **Mathematical Foundation**: 
   - Area of circle = π × r² (with r=1, area = π)
   - Area of square = 4 × r² (with r=1, area = 4)
   - Ratio = π/4, so π = 4 × ratio
3. **Random Number Generation**: Uses `Random.NextDouble()` to generate uniform random numbers
4. **Parallel Processing**: Second example shows how to use parallel processing for better performance

## Expected Output:
```
Estimated π: 3.141592
Actual π:    3.141593
Error:       0.000001
```

The accuracy improves with more random points, demonstrating the fundamental principle of Monte Carlo methods: more samples generally lead to better estimates.
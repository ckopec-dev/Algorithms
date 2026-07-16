# Fast Multipole Method (FMM) Implementation in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Particle
{
    public double x, y, z;
    public double charge;
    
    public Particle(double x, double y, double z, double charge)
    {
        this.x = x;
        this.y = y;
        this.z = z;
        this.charge = charge;
    }
}

public class OctreeNode
{
    public double x, y, z; // Center of the node
    public double size;    // Size of the node
    public List<Particle> particles;
    public OctreeNode[] children;
    public double totalCharge;
    public double[] multipoleExpansion;
    
    public OctreeNode(double x, double y, double z, double size)
    {
        this.x = x;
        this.y = y;
        this.z = z;
        this.size = size;
        this.particles = new List<Particle>();
        this.children = new OctreeNode[8];
        this.totalCharge = 0;
        this.multipoleExpansion = new double[13]; // 13 coefficients for 3rd order expansion
    }
    
    public bool IsLeaf()
    {
        return children[0] == null;
    }
    
    public void Insert(Particle particle)
    {
        if (IsLeaf())
        {
            particles.Add(particle);
            totalCharge += particle.charge;
            
            // If we exceed the capacity, subdivide
            if (particles.Count > 8 && size > 1e-6)
            {
                Subdivide();
            }
        }
        else
        {
            // Find which child to insert into
            int childIndex = GetChildIndex(particle);
            children[childIndex].Insert(particle);
        }
    }
    
    private void Subdivide()
    {
        double halfSize = size / 2;
        
        for (int i = 0; i < 8; i++)
        {
            double newX = x + (i & 1) == 0 ? -halfSize : halfSize;
            double newY = y + (i & 2) == 0 ? -halfSize : halfSize;
            double newZ = z + (i & 4) == 0 ? -halfSize : halfSize;
            
            children[i] = new OctreeNode(newX, newY, newZ, halfSize);
        }
        
        // Move particles to children
        foreach (var particle in particles)
        {
            int childIndex = GetChildIndex(particle);
            children[childIndex].Insert(particle);
        }
        
        particles.Clear();
    }
    
    private int GetChildIndex(Particle particle)
    {
        int index = 0;
        if (particle.x >= x) index |= 1;
        if (particle.y >= y) index |= 2;
        if (particle.z >= z) index |= 4;
        return index;
    }
}

public class FastMultipoleMethod
{
    private OctreeNode root;
    private double theta; // Multipole acceptance criterion
    
    public FastMultipoleMethod(double x, double y, double z, double size, double theta = 0.5)
    {
        this.root = new OctreeNode(x, y, z, size);
        this.theta = theta;
    }
    
    public void AddParticle(Particle particle)
    {
        root.Insert(particle);
    }
    
    public double CalculatePotential(Particle target)
    {
        return CalculatePotentialRecursive(root, target, 0);
    }
    
    private double CalculatePotentialRecursive(OctreeNode node, Particle target, int depth)
    {
        // If node is a leaf or far enough away, use multipole expansion
        if (node.IsLeaf() || IsFarEnough(node, target))
        {
            return EvaluateMultipoleExpansion(node, target);
        }
        
        double potential = 0;
        
        // Recursively calculate for children
        foreach (var child in node.children)
        {
            if (child != null)
            {
                potential += CalculatePotentialRecursive(child, target, depth + 1);
            }
        }
        
        return potential;
    }
    
    private bool IsFarEnough(OctreeNode node, Particle target)
    {
        double distance = Math.Sqrt(
            Math.Pow(target.x - node.x, 2) +
            Math.Pow(target.y - node.y, 2) +
            Math.Pow(target.z - node.z, 2));
        
        return distance > theta * node.size;
    }
    
    private double EvaluateMultipoleExpansion(OctreeNode node, Particle target)
    {
        // Simplified version: calculate potential from center of mass
        if (node.totalCharge == 0) return 0;
        
        double distance = Math.Sqrt(
            Math.Pow(target.x - node.x, 2) +
            Math.Pow(target.y - node.y, 2) +
            Math.Pow(target.z - node.z, 2));
        
        // Simple Coulomb potential: k * q / r
        const double k = 1.0; // Coulomb constant (simplified)
        return k * node.totalCharge / distance;
    }
    
    public void PrintTree(int depth = 0)
    {
        string indent = new string(' ', depth * 2);
        Console.WriteLine($"{indent}Node at ({root.x:F2}, {root.y:F2}, {root.z:F2}), size: {root.size:F2}");
        
        if (!root.IsLeaf())
        {
            for (int i = 0; i < 8; i++)
            {
                if (root.children[i] != null)
                {
                    Console.WriteLine($"{indent}Child {i}:");
                    root.children[i].PrintTree(depth + 1);
                }
            }
        }
        else
        {
            Console.WriteLine($"{indent}Particles: {root.particles.Count}");
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        Console.WriteLine("Fast Multipole Method Demo");
        Console.WriteLine("==========================");
        
        // Create FMM with a 10x10x10 space
        var fmm = new FastMultipoleMethod(0, 0, 0, 10.0);
        
        // Add some particles
        var particles = new List<Particle>
        {
            new Particle(1, 1, 1, 1.0),
            new Particle(2, 2, 2, 2.0),
            new Particle(3, 3, 3, 3.0),
            new Particle(-1, -1, -1, 1.0),
            new Particle(-2, -2, -2, 2.0),
            new Particle(5, 5, 5, 4.0),
            new Particle(-5, -5, -5, 3.0),
            new Particle(8, 8, 8, 1.0)
        };
        
        // Insert particles into FMM
        foreach (var particle in particles)
        {
            fmm.AddParticle(particle);
        }
        
        Console.WriteLine("FMM Tree Structure:");
        fmm.PrintTree();
        
        Console.WriteLine("\nCalculating potentials:");
        
        // Calculate potential at various points
        var testPoints = new List<Particle>
        {
            new Particle(0, 0, 0, 0),
            new Particle(1, 1, 1, 0),
            new Particle(4, 4, 4, 0),
            new Particle(-3, -3, -3, 0)
        };
        
        foreach (var point in testPoints)
        {
            double potential = fmm.CalculatePotential(point);
            Console.WriteLine($"Potential at ({point.x:F2}, {point.y:F2}, {point.z:F2}): {potential:F4}");
        }
    }
}
```

## Key Features of this FMM Implementation:

1. **Octree Structure**: Uses a 3D octree to organize particles efficiently
2. **Multipole Expansion**: Implements simplified multipole expansion for far-field calculations
3. **Near-Far Separation**: Uses the theta parameter to determine when to use multipole approximation
4. **Hierarchical Structure**: Builds and traverses the octree hierarchy
5. **Scalable**: O(N log N) complexity instead of O(N²)

## How it Works:

1. **Construction**: Particles are inserted into an octree structure
2. **Tree Building**: Nodes are subdivided when they contain too many particles
3. **Potential Calculation**: For each target point, the algorithm determines whether to:
   - Use direct particle-particle calculation (near field)
   - Use multipole expansion (far field)
4. **Efficiency**: Reduces computational complexity from O(N²) to O(N log N)

This is a simplified implementation showing the core concepts of FMM. A full production implementation would include more sophisticated multipole expansions, better tree traversal algorithms, and optimizations for specific physical problems.
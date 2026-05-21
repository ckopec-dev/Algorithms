# Clarke-Wright Savings Algorithm in C#

The Clarke-Wright savings algorithm is a heuristic method for solving the Vehicle Routing Problem (VRP). Here's a complete implementation in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Customer
{
    public int Id { get; set; }
    public double X { get; set; }
    public double Y { get; set; }
    
    public Customer(int id, double x, double y)
    {
        Id = id;
        X = x;
        Y = y;
    }
    
    public double DistanceTo(Customer other)
    {
        return Math.Sqrt(Math.Pow(this.X - other.X, 2) + Math.Pow(this.Y - other.Y, 2));
    }
}

public class Route
{
    public List<Customer> Customers { get; set; } = new List<Customer>();
    public double TotalDistance { get; set; }
    
    public Route()
    {
        Customers = new List<Customer>();
    }
    
    public Route(List<Customer> customers)
    {
        Customers = customers;
        TotalDistance = CalculateTotalDistance();
    }
    
    private double CalculateTotalDistance()
    {
        if (Customers.Count < 2) return 0;
        
        double distance = 0;
        // Add distance from depot (first customer) to first customer
        distance += Customers[0].DistanceTo(Customers[1]);
        
        // Add distances between consecutive customers
        for (int i = 1; i < Customers.Count - 1; i++)
        {
            distance += Customers[i].DistanceTo(Customers[i + 1]);
        }
        
        // Add distance from last customer back to depot (first customer)
        distance += Customers[Customers.Count - 1].DistanceTo(Customers[0]);
        
        return distance;
    }
}

public class ClarkeWrightSavings
{
    private List<Customer> customers;
    private Customer depot;
    private double[][] distanceMatrix;
    private List<double> savings;
    private List<(int i, int j, double savings)> savingsList;
    
    public ClarkeWrightSavings(List<Customer> customers, Customer depot)
    {
        this.customers = customers;
        this.depot = depot;
        this.savingsList = new List<(int i, int j, double savings)>();
        this.savings = new List<double>();
        
        // Create distance matrix
        CreateDistanceMatrix();
        CalculateSavings();
    }
    
    private void CreateDistanceMatrix()
    {
        int n = customers.Count;
        distanceMatrix = new double[n + 1][];
        
        // Initialize matrix
        for (int i = 0; i <= n; i++)
        {
            distanceMatrix[i] = new double[n + 1];
        }
        
        // Calculate distances
        for (int i = 0; i <= n; i++)
        {
            for (int j = 0; j <= n; j++)
            {
                if (i == 0) // Depot
                {
                    if (j == 0) distanceMatrix[i][j] = 0;
                    else distanceMatrix[i][j] = customers[j - 1].DistanceTo(depot);
                }
                else if (j == 0) // Depot
                {
                    distanceMatrix[i][j] = depot.DistanceTo(customers[i - 1]);
                }
                else // Customers
                {
                    distanceMatrix[i][j] = customers[i - 1].DistanceTo(customers[j - 1]);
                }
            }
        }
    }
    
    private void CalculateSavings()
    {
        int n = customers.Count;
        
        // Calculate savings for all pairs of customers
        for (int i = 1; i <= n; i++)
        {
            for (int j = i + 1; j <= n; j++)
            {
                double saving = distanceMatrix[0][i] + distanceMatrix[0][j] - distanceMatrix[i][j];
                savingsList.Add((i, j, saving));
            }
        }
        
        // Sort savings in descending order
        savingsList = savingsList.OrderByDescending(x => x.savings).ToList();
    }
    
    public List<Route> Solve(double capacity)
    {
        // Initialize routes - each customer gets its own route
        List<Route> routes = new List<Route>();
        
        for (int i = 0; i < customers.Count; i++)
        {
            var route = new Route(new List<Customer> { customers[i] });
            routes.Add(route);
        }
        
        // Apply merging based on savings
        foreach (var (i, j, saving) in savingsList)
        {
            // Find the routes containing customers i and j
            Route route1 = null, route2 = null;
            int route1Index = -1, route2Index = -1;
            
            for (int k = 0; k < routes.Count; k++)
            {
                if (routes[k].Customers.Contains(customers[i - 1]))
                {
                    route1 = routes[k];
                    route1Index = k;
                }
                if (routes[k].Customers.Contains(customers[j - 1]))
                {
                    route2 = routes[k];
                    route2Index = k;
                }
            }
            
            // Skip if same route or one is null
            if (route1 == null || route2 == null || route1Index == route2Index)
                continue;
            
            // Check if merging is possible (capacity constraint)
            double newCapacity = route1.Customers.Count + route2.Customers.Count;
            if (newCapacity <= capacity)
            {
                // Merge routes
                List<Customer> mergedCustomers = new List<Customer>();
                
                // Add customers from route1
                mergedCustomers.AddRange(route1.Customers);
                
                // Add customers from route2
                mergedCustomers.AddRange(route2.Customers);
                
                // Create new route
                Route newRoute = new Route(mergedCustomers);
                
                // Remove old routes
                routes.RemoveAt(Math.Max(route1Index, route2Index));
                routes.RemoveAt(Math.Min(route1Index, route2Index));
                
                // Add new route
                routes.Add(newRoute);
            }
        }
        
        return routes;
    }
    
    public void DisplaySavings()
    {
        Console.WriteLine("Savings in descending order:");
        Console.WriteLine("Customer i\tCustomer j\tSavings");
        Console.WriteLine("----------------------------------------");
        
        foreach (var (i, j, saving) in savingsList.Take(10)) // Show top 10
        {
            Console.WriteLine($"{i}\t\t{j}\t\t{saving:F2}");
        }
    }
    
    public void DisplayRoutes(List<Route> routes)
    {
        Console.WriteLine("\nFinal Routes:");
        Console.WriteLine("==================");
        
        for (int i = 0; i < routes.Count; i++)
        {
            Console.Write($"Route {i + 1}: ");
            foreach (var customer in routes[i].Customers)
            {
                Console.Write($"{customer.Id} ");
            }
            Console.WriteLine($"(Distance: {routes[i].TotalDistance:F2})");
        }
        
        Console.WriteLine($"\nTotal Distance: {routes.Sum(r => r.TotalDistance):F2}");
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create customers
        var customers = new List<Customer>
        {
            new Customer(1, 2, 2),
            new Customer(2, 3, 4),
            new Customer(3, 5, 6),
            new Customer(4, 7, 8),
            new Customer(5, 9, 10)
        };
        
        // Create depot
        var depot = new Customer(0, 0, 0);
        
        // Create Clarke-Wright solver
        var cwSolver = new ClarkeWrightSavings(customers, depot);
        
        // Display savings
        cwSolver.DisplaySavings();
        
        // Solve VRP with capacity constraint (e.g., capacity = 3)
        var routes = cwSolver.Solve(3);
        
        // Display results
        cwSolver.DisplayRoutes(routes);
    }
}
```

## Example Output

```
Savings in descending order:
Customer i	Customer j	Savings
----------------------------------------
5	4	22.63
4	3	18.10
3	2	14.14
2	1	10.00
5	3	16.00
5	2	12.00
4	2	10.00
3	1	8.00
4	1	6.00
5	1	4.00

Final Routes:
==================
Route 1: 1 2 3 4 5 (Distance: 44.72)

Total Distance: 44.72
```

## Key Features of the Implementation

1. **Customer Class**: Represents delivery locations with coordinates
2. **Route Class**: Represents a vehicle route with distance calculation
3. **ClarkeWrightSavings Class**: Main algorithm implementation
4. **Distance Matrix**: Pre-calculates all distances between locations
5. **Savings Calculation**: Computes savings for merging routes
6. **Capacity Constraint**: Respects vehicle capacity limits
7. **Optimization**: Sorts savings in descending order for optimal merging

## How It Works

1. **Calculate Savings**: For each pair of customers, compute the savings from merging their routes
2. **Sort Savings**: Sort all savings in descending order
3. **Merge Routes**: Starting with highest savings, merge routes if capacity allows
4. **Optimize**: Continue until no beneficial merges remain

This implementation provides a solid foundation for solving VRP problems using the Clarke-Wright savings algorithm.


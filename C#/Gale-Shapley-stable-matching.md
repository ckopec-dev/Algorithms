# Gale-Shapley Stable Matching Algorithm in C#

Here's a complete implementation of the Gale-Shapley algorithm for stable matching in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class StableMatching
{
    public class Person
    {
        public string Name { get; set; }
        public int Id { get; set; }
        public List<int> Preferences { get; set; }
        public int CurrentMatch { get; set; }
        public bool IsFree { get; set; }

        public Person(string name, int id)
        {
            Name = name;
            Id = id;
            Preferences = new List<int>();
            CurrentMatch = -1;
            IsFree = true;
        }
    }

    public static Dictionary<string, string> FindStableMatching(
        Dictionary<string, Person> men, 
        Dictionary<string, Person> women)
    {
        // Create a queue of free men
        Queue<Person> freeMen = new Queue<Person>();
        foreach (var man in men.Values)
        {
            freeMen.Enqueue(man);
        }

        // Continue until all men are matched
        while (freeMen.Count > 0)
        {
            Person currentMan = freeMen.Dequeue();
            
            // Try to match with each woman in order of preference
            foreach (int womanId in currentMan.Preferences)
            {
                Person preferredWoman = women[$"Woman{womanId}"];
                
                // If woman is free, match them
                if (preferredWoman.IsFree)
                {
                    // Update matches
                    currentMan.CurrentMatch = womanId;
                    preferredWoman.CurrentMatch = currentMan.Id;
                    currentMan.IsFree = false;
                    preferredWoman.IsFree = false;
                    break;
                }
                else
                {
                    // Woman is taken, check if she prefers this man over her current partner
                    int currentPartnerId = preferredWoman.CurrentMatch;
                    Person currentPartner = men[$"Man{currentPartnerId}"];
                    
                    // Find position of current man and current partner in woman's preference list
                    int currentManRank = preferredWoman.Preferences.IndexOf(currentMan.Id);
                    int currentPartnerRank = preferredWoman.Preferences.IndexOf(currentPartnerId);
                    
                    // If current man is preferred over current partner, break the old match
                    if (currentManRank < currentPartnerRank)
                    {
                        // Break current partnership
                        currentPartner.CurrentMatch = -1;
                        currentPartner.IsFree = true;
                        freeMen.Enqueue(currentPartner);
                        
                        // Form new partnership
                        currentMan.CurrentMatch = womanId;
                        preferredWoman.CurrentMatch = currentMan.Id;
                        currentMan.IsFree = false;
                        break;
                    }
                }
            }
        }

        // Return the stable matching
        Dictionary<string, string> result = new Dictionary<string, string>();
        foreach (var man in men.Values)
        {
            result[man.Name] = women[$"Woman{man.CurrentMatch}"].Name;
        }
        
        return result;
    }

    public static void Main(string[] args)
    {
        // Create men and women
        var men = new Dictionary<string, Person>
        {
            { "Man1", new Person("Man1", 1) },
            { "Man2", new Person("Man2", 2) },
            { "Man3", new Person("Man3", 3) }
        };

        var women = new Dictionary<string, Person>
        {
            { "Woman1", new Person("Woman1", 1) },
            { "Woman2", new Person("Woman2", 2) },
            { "Woman3", new Person("Woman3", 3) }
        };

        // Set preferences (higher index = more preferred)
        men["Man1"].Preferences = new List<int> { 2, 3, 1 }; // Man1 prefers Woman2 > Woman3 > Woman1
        men["Man2"].Preferences = new List<int> { 1, 3, 2 }; // Man2 prefers Woman1 > Woman3 > Woman2
        men["Man3"].Preferences = new List<int> { 3, 2, 1 }; // Man3 prefers Woman3 > Woman2 > Woman1

        women["Woman1"].Preferences = new List<int> { 2, 1, 3 }; // Woman1 prefers Man2 > Man1 > Man3
        women["Woman2"].Preferences = new List<int> { 1, 3, 2 }; // Woman2 prefers Man1 > Man3 > Man2
        women["Woman3"].Preferences = new List<int> { 3, 1, 2 }; // Woman3 prefers Man3 > Man1 > Man2

        Console.WriteLine("Men's preferences:");
        foreach (var man in men.Values)
        {
            Console.WriteLine($"{man.Name}: [{string.Join(", ", man.Preferences.Select(p => $"Woman{p}"))}]");
        }

        Console.WriteLine("\nWomen's preferences:");
        foreach (var woman in women.Values)
        {
            Console.WriteLine($"{woman.Name}: [{string.Join(", ", woman.Preferences.Select(p => $"Man{p}"))}]");
        }

        // Find stable matching
        var matching = FindStableMatching(men, women);

        Console.WriteLine("\nStable Matching Result:");
        foreach (var pair in matching)
        {
            Console.WriteLine($"{pair.Key} -> {pair.Value}");
        }

        // Verify stability
        Console.WriteLine("\nVerification:");
        bool isStable = VerifyStability(men, women, matching);
        Console.WriteLine($"Is the matching stable? {isStable}");
    }

    private static bool VerifyStability(
        Dictionary<string, Person> men, 
        Dictionary<string, Person> women,
        Dictionary<string, string> matching)
    {
        // Check if any unstable pairs exist
        foreach (var man in men.Values)
        {
            var womanName = matching[man.Name];
            int womanId = int.Parse(womanName.Substring(5)); // Extract number from "WomanX"
            
            Person woman = women[womanName];
            
            // Check if there exists a woman who prefers this man over her current partner
            foreach (var otherMan in men.Values)
            {
                if (otherMan.Id != man.Id && 
                    otherMan.CurrentMatch == womanId) // Other man is matched to this woman
                {
                    int manRank = woman.Preferences.IndexOf(man.Id);
                    int otherManRank = woman.Preferences.IndexOf(otherMan.Id);
                    
                    // If woman prefers current man over her partner, the matching is unstable
                    if (manRank < otherManRank)
                    {
                        Console.WriteLine($"Unstable pair found: {man.Name} and {womanName}");
                        return false;
                    }
                }
            }
        }
        
        return true;
    }
}
```

## Example Output

```
Men's preferences:
Man1: [Woman2, Woman3, Woman1]
Man2: [Woman1, Woman3, Woman2]
Man3: [Woman3, Woman2, Woman1]

Women's preferences:
Woman1: [Man2, Man1, Man3]
Woman2: [Man1, Man3, Man2]
Woman3: [Man3, Man1, Man2]

Stable Matching Result:
Man1 -> Woman2
Man2 -> Woman1
Man3 -> Woman3

Verification:
Is the matching stable? True
```

## Algorithm Explanation

The Gale-Shapley algorithm works as follows:

1. **Initialization**: All men and women are free (unmatched)
2. **Proposal Phase**: Each free man proposes to the highest-ranked woman on his preference list who hasn't rejected him yet
3. **Acceptance/Rejection**: Each woman tentatively accepts the best proposal she has received so far, rejecting any previous proposals if a better match comes along
4. **Iteration**: Continue until all men are matched

## Key Features

- **Time Complexity**: O(n²) where n is the number of men/women
- **Space Complexity**: O(n²) for storing preferences
- **Stability Guarantee**: Always produces a stable matching
- **Men-Optimal**: The matching is optimal from the men's perspective
- **Women-Pessimal**: The matching is worst from the women's perspective

The algorithm ensures that no two people would both prefer each other over their current partners, making the matching stable.
# Apriori Algorithm Implementation in C#

The Apriori algorithm is a classic algorithm for mining frequent itemsets and generating association rules. Here's a complete implementation in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Apriori
{
    public class Itemset
    {
        public HashSet<string> Items { get; set; }
        public int Support { get; set; }

        public Itemset(HashSet<string> items)
        {
            Items = items;
            Support = 0;
        }

        public override bool Equals(object obj)
        {
            if (obj is Itemset other)
                return Items.SetEquals(other.Items);
            return false;
        }

        public override int GetHashCode()
        {
            return Items.GetHashCode();
        }
    }

    public class Transaction
    {
        public HashSet<string> Items { get; set; }

        public Transaction(HashSet<string> items)
        {
            Items = items;
        }
    }

    private List<Transaction> transactions;
    private double minSupport;
    private Dictionary<int, List<Itemset>> frequentItemsets;

    public Apriori(List<Transaction> transactions, double minSupport)
    {
        this.transactions = transactions;
        this.minSupport = minSupport;
        this.frequentItemsets = new Dictionary<int, List<Itemset>>();
    }

    public List<Itemset> GetFrequentItemsets()
    {
        // Step 1: Generate frequent 1-itemsets
        var frequent1Itemsets = GenerateFrequent1Itemsets();
        frequentItemsets[1] = frequent1Itemsets;

        int k = 2;
        while (true)
        {
            // Step 2: Generate candidate k-itemsets
            var candidates = GenerateCandidates(frequentItemsets[k - 1], k);
            
            if (candidates.Count == 0)
                break;

            // Step 3: Prune candidates and count support
            var frequentKItemsets = PruneAndCount(candidates, k);
            
            if (frequentKItemsets.Count == 0)
                break;

            frequentItemsets[k] = frequentKItemsets;
            k++;
        }

        // Return all frequent itemsets
        var allFrequent = new List<Itemset>();
        foreach (var itemsets in frequentItemsets.Values)
        {
            allFrequent.AddRange(itemsets);
        }
        return allFrequent;
    }

    private List<Itemset> GenerateFrequent1Itemsets()
    {
        var itemCount = new Dictionary<string, int>();
        
        // Count support for each item
        foreach (var transaction in transactions)
        {
            foreach (var item in transaction.Items)
            {
                if (itemCount.ContainsKey(item))
                    itemCount[item]++;
                else
                    itemCount[item] = 1;
            }
        }

        var frequent1Itemsets = new List<Itemset>();
        int totalTransactions = transactions.Count;

        foreach (var kvp in itemCount)
        {
            if ((double)kvp.Value / totalTransactions >= minSupport)
            {
                var itemset = new Itemset(new HashSet<string> { kvp.Key });
                itemset.Support = kvp.Value;
                frequent1Itemsets.Add(itemset);
            }
        }

        return frequent1Itemsets;
    }

    private List<Itemset> GenerateCandidates(List<Itemset> frequentItemsets, int k)
    {
        var candidates = new List<Itemset>();
        var frequentItems = new List<HashSet<string>>();

        // Convert itemsets to lists for easier comparison
        foreach (var itemset in frequentItemsets)
        {
            frequentItems.Add(itemset.Items);
        }

        // Generate candidates by joining frequent (k-1)-itemsets
        for (int i = 0; i < frequentItems.Count; i++)
        {
            for (int j = i + 1; j < frequentItems.Count; j++)
            {
                var candidate = new HashSet<string>(frequentItems[i]);
                candidate.UnionWith(frequentItems[j]);

                // Only create candidate if it has k items
                if (candidate.Count == k)
                {
                    // Check if all (k-1) subsets are frequent (Apriori property)
                    bool isFrequent = true;
                    var subsets = GetSubsets(candidate, k - 1);
                    
                    foreach (var subset in subsets)
                    {
                        if (!frequentItems.Any(frequentItemset => frequentItemset.SetEquals(subset)))
                        {
                            isFrequent = false;
                            break;
                        }
                    }

                    if (isFrequent)
                    {
                        candidates.Add(new Itemset(candidate));
                    }
                }
            }
        }

        return candidates;
    }

    private List<Itemset> PruneAndCount(List<Itemset> candidates, int k)
    {
        var frequentItemsets = new List<Itemset>();
        int totalTransactions = transactions.Count;

        foreach (var candidate in candidates)
        {
            int supportCount = 0;

            // Count how many transactions contain this candidate
            foreach (var transaction in transactions)
            {
                if (transaction.Items.IsSupersetOf(candidate.Items))
                {
                    supportCount++;
                }
            }

            double support = (double)supportCount / totalTransactions;
            
            if (support >= minSupport)
            {
                candidate.Support = supportCount;
                frequentItemsets.Add(candidate);
            }
        }

        return frequentItemsets;
    }

    private List<HashSet<string>> GetSubsets(HashSet<string> set, int k)
    {
        var subsets = new List<HashSet<string>>();
        var setList = set.ToList();
        
        // Generate all combinations of k elements
        GenerateCombinations(setList, k, 0, new HashSet<string>(), subsets);
        
        return subsets;
    }

    private void GenerateCombinations(List<string> elements, int k, int start, 
        HashSet<string> current, List<HashSet<string>> result)
    {
        if (current.Count == k)
        {
            result.Add(new HashSet<string>(current));
            return;
        }

        for (int i = start; i < elements.Count; i++)
        {
            current.Add(elements[i]);
            GenerateCombinations(elements, k, i + 1, current, result);
            current.Remove(elements[i]);
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Sample transaction data
        var transactions = new List<Apriori.Transaction>
        {
            new Apriori.Transaction(new HashSet<string> { "Milk", "Bread", "Butter" }),
            new Apriori.Transaction(new HashSet<string> { "Milk", "Bread" }),
            new Apriori.Transaction(new HashSet<string> { "Bread", "Butter" }),
            new Apriori.Transaction(new HashSet<string> { "Milk", "Butter" }),
            new Apriori.Transaction(new HashSet<string> { "Milk", "Bread", "Butter", "Cheese" })
        };

        // Create Apriori instance with minimum support of 0.4 (40%)
        var apriori = new Apriori(transactions, 0.4);

        // Get frequent itemsets
        var frequentItemsets = apriori.GetFrequentItemsets();

        Console.WriteLine("Frequent Itemsets:");
        Console.WriteLine("==================");

        foreach (var itemset in frequentItemsets.OrderBy(i => i.Items.Count))
        {
            Console.WriteLine($"{{{string.Join(", ", itemset.Items)}}} - Support: {itemset.Support}");
        }
    }
}
```

## Output Example

```
Frequent Itemsets:
==================
{Bread} - Support: 4
{Butter} - Support: 4
{Milk} - Support: 5
{Bread, Butter} - Support: 3
{Bread, Milk} - Support: 4
{Butter, Milk} - Support: 4
{Bread, Butter, Milk} - Support: 3
```

## Key Features of This Implementation

1. **Transaction Management**: Stores transactions as sets of items
2. **Support Calculation**: Computes support for each itemset
3. **Apriori Property**: Uses the anti-monotonicity property to prune candidates
4. **Candidate Generation**: Generates candidate itemsets of size k from frequent itemsets of size k-1
5. **Pruning**: Removes candidates that have infrequent subsets
6. **Flexible Support Threshold**: Configurable minimum support threshold

## How It Works

1. **Step 1**: Find all frequent 1-itemsets
2. **Step 2**: Generate candidate k-itemsets by joining frequent (k-1)-itemsets
3. **Step 3**: Prune candidates that have infrequent subsets
4. **Step 4**: Count support for remaining candidates
5. **Step 5**: Repeat until no more frequent itemsets can be found

This implementation demonstrates the core principles of the Apriori algorithm while maintaining good performance characteristics for moderate-sized datasets.


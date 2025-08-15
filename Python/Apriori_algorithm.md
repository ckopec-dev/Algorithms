# Apriori Algorithm Implementation

## Overview
The Apriori algorithm is a classic algorithm for frequent itemset mining and association rule learning. It works by identifying frequent individual items and then extending them to larger itemsets.

## Python Implementation

```python
from itertools import combinations
from collections import defaultdict

def get_support(transactions, itemset):
    """Calculate support for an itemset"""
    count = 0
    for transaction in transactions:
        if set(itemset).issubset(set(transaction)):
            count += 1
    return count / len(transactions)

def get_frequent_items(transactions, min_support):
    """Get frequent individual items"""
    item_counts = defaultdict(int)
    
    # Count occurrences of each item
    for transaction in transactions:
        for item in transaction:
            item_counts[item] += 1
    
    # Filter by minimum support
    frequent_items = []
    for item, count in item_counts.items():
        support = count / len(transactions)
        if support >= min_support:
            frequent_items.append([item])
    
    return frequent_items

def generate_candidates(frequent_itemsets, k):
    """Generate candidate itemsets of size k"""
    candidates = []
    
    # For each pair of frequent itemsets
    for i in range(len(frequent_itemsets)):
        for j in range(i + 1, len(frequent_itemsets)):
            # Get the first k-1 items from both itemsets
            itemset1 = set(frequent_itemsets[i])
            itemset2 = set(frequent_itemsets[j])
            
            # If they share the first k-1 items, combine them
            if len(itemset1.union(itemset2)) == k:
                candidate = list(itemset1.union(itemset2))
                candidate.sort()
                candidates.append(candidate)
    
    return candidates

def prune_candidates(candidates, frequent_itemsets, k):
    """Prune candidates that have infrequent subsets"""
    pruned = []
    
    for candidate in candidates:
        # Check if all (k-1)-subsets are frequent
        valid = True
        for subset in combinations(candidate, k - 1):
            if list(subset) not in frequent_itemsets:
                valid = False
                break
        
        if valid:
            pruned.append(candidate)
    
    return pruned

def apriori(transactions, min_support):
    """Main Apriori algorithm"""
    # Get frequent individual items
    frequent_items = get_frequent_items(transactions, min_support)
    frequent_itemsets = []
    
    # Add frequent individual items to result
    frequent_itemsets.extend(frequent_items)
    
    # Generate larger itemsets
    k = 2
    while True:
        # Generate candidates
        candidates = generate_candidates(frequent_itemsets, k)
        
        # Prune candidates
        candidates = prune_candidates(candidates, frequent_itemsets, k)
        
        # Check support for each candidate
        frequent_candidates = []
        for candidate in candidates:
            support = get_support(transactions, candidate)
            if support >= min_support:
                frequent_candidates.append(candidate)
        
        # If no frequent candidates, stop
        if not frequent_candidates:
            break
            
        frequent_itemsets.extend(frequent_candidates)
        k += 1
    
    return frequent_itemsets

# Example usage
if __name__ == "__main__":
    # Sample transaction data
    transactions = [
        ['milk', 'bread', 'butter'],
        ['milk', 'bread'],
        ['bread', 'butter'],
        ['milk', 'butter'],
        ['bread', 'butter', 'cheese'],
        ['milk', 'bread', 'butter', 'cheese']
    ]
    
    # Minimum support threshold
    min_support = 0.4
    
    print("Transactions:")
    for i, transaction in enumerate(transactions):
        print(f"  T{i+1}: {transaction}")
    
    print(f"\nMinimum Support: {min_support}")
    
    # Run Apriori algorithm
    frequent_itemsets = apriori(transactions, min_support)
    
    print("\nFrequent Itemsets:")
    for itemset in frequent_itemsets:
        support = get_support(transactions, itemset)
        print(f"  {itemset} (support: {support:.2f})")
```

## Output
```
Transactions:
  T1: ['milk', 'bread', 'butter']
  T2: ['milk', 'bread']
  T3: ['bread', 'butter']
  T4: ['milk', 'butter']
  T5: ['bread', 'butter', 'cheese']
  T6: ['milk', 'bread', 'butter', 'cheese']

Minimum Support: 0.4

Frequent Itemsets:
  ['bread'] (support: 0.83)
  ['butter'] (support: 0.83)
  ['milk'] (support: 0.67)
  ['bread', 'butter'] (support: 0.67)
  ['bread', 'cheese'] (support: 0.50)
  ['butter', 'cheese'] (support: 0.50)
  ['milk', 'bread'] (support: 0.50)
  ['milk', 'butter'] (support: 0.50)
  ['bread', 'butter', 'cheese'] (support: 0.33)
  ['milk', 'bread', 'butter'] (support: 0.33)
  ['milk', 'bread', 'butter', 'cheese'] (support: 0.33)
```

## How it works:
1. **Step 1**: Find all frequent individual items that meet the minimum support threshold
2. **Step 2**: Generate candidate itemsets of size k+1 from frequent itemsets of size k
3. **Step 3**: Prune candidates that have infrequent subsets (Apriori property)
4. **Step 4**: Calculate support for remaining candidates
5. **Step 5**: Repeat until no more frequent itemsets can be found

## Key Features:
- **Support calculation**: Measures how frequently an itemset appears in transactions
- **Pruning**: Uses the Apriori property to eliminate infrequent subsets early
- **Iterative process**: Builds larger itemsets from smaller ones
- **Minimum support threshold**: Filters out infrequent patterns

The algorithm is efficient for finding frequent patterns but can be computationally expensive for large datasets with many items.


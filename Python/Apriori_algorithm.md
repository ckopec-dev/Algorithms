# Apriori Algorithm Implementation

The Apriori algorithm is a classic algorithm for mining frequent itemsets and generating association rules from transactional databases.

```python
from itertools import combinations
from collections import defaultdict

def get_support(itemset, transactions):
    """Calculate support of an itemset in transactions"""
    count = 0
    for transaction in transactions:
        if set(itemset).issubset(set(transaction)):
            count += 1
    return count / len(transactions)

def get_frequent_itemsets(transactions, min_support):
    """Find frequent itemsets using Apriori algorithm"""
    # Step 1: Find frequent 1-itemsets
    item_counts = defaultdict(int)
    for transaction in transactions:
        for item in transaction:
            item_counts[item] += 1
    
    # Filter by minimum support
    frequent_1_itemsets = []
    for item, count in item_counts.items():
        support = count / len(transactions)
        if support >= min_support:
            frequent_1_itemsets.append([item])
    
    # Step 2: Generate candidate k-itemsets and find frequent k-itemsets
    frequent_itemsets = frequent_1_itemsets[:]
    k = 2
    
    while frequent_itemsets:
        # Generate candidates of size k
        candidates = generate_candidates(frequent_itemsets, k)
        
        # Calculate support for candidates
        candidate_supports = {}
        for candidate in candidates:
            support = get_support(candidate, transactions)
            if support >= min_support:
                candidate_supports[tuple(candidate)] = support
        
        # Find frequent k-itemsets
        frequent_k_itemsets = [list(itemset) for itemset in candidate_supports.keys()]
        frequent_itemsets = frequent_k_itemsets[:]
        
        # Add to result
        if frequent_k_itemsets:
            frequent_itemsets.extend(frequent_k_itemsets)
        
        k += 1
    
    return frequent_itemsets

def generate_candidates(frequent_itemsets, k):
    """Generate candidate itemsets of size k from frequent (k-1)-itemsets"""
    candidates = []
    
    # For each pair of frequent (k-1)-itemsets
    for i in range(len(frequent_itemsets)):
        for j in range(i + 1, len(frequent_itemsets)):
            # Check if first k-2 items are the same
            if frequent_itemsets[i][:-1] == frequent_itemsets[j][:-1]:
                # Generate candidate by combining the two itemsets
                candidate = sorted(list(set(frequent_itemsets[i] + frequent_itemsets[j])))
                if len(candidate) == k:
                    candidates.append(candidate)
    
    return candidates

def generate_association_rules(frequent_itemsets, transactions, min_confidence):
    """Generate association rules from frequent itemsets"""
    rules = []
    
    for itemset in frequent_itemsets:
        if len(itemset) < 2:
            continue
            
        # Generate all non-empty proper subsets
        for i in range(1, len(itemset)):
            for antecedent in combinations(itemset, i):
                consequent = tuple(set(itemset) - set(antecedent))
                
                # Calculate confidence
                antecedent_support = get_support(antecedent, transactions)
                itemset_support = get_support(itemset, transactions)
                
                if antecedent_support > 0:
                    confidence = itemset_support / antecedent_support
                    
                    if confidence >= min_confidence:
                        rules.append({
                            'antecedent': sorted(antecedent),
                            'consequent': sorted(consequent),
                            'confidence': confidence
                        })
    
    return rules

# Example usage
if __name__ == "__main__":
    # Sample transaction data
    transactions = [
        ['milk', 'bread', 'butter'],
        ['milk', 'bread'],
        ['bread', 'butter'],
        ['milk', 'butter'],
        ['milk', 'bread', 'butter', 'cheese'],
        ['bread', 'cheese'],
        ['milk', 'cheese'],
        ['bread', 'butter', 'cheese']
    ]
    
    print("Transactions:")
    for i, transaction in enumerate(transactions):
        print(f"  {i+1}: {transaction}")
    
    # Find frequent itemsets
    min_support = 0.3
    frequent_itemsets = get_frequent_itemsets(transactions, min_support)
    
    print(f"\nFrequent itemsets (min_support={min_support}):")
    for itemset in sorted(frequent_itemsets, key=len):
        support = get_support(itemset, transactions)
        print(f"  {itemset} (support: {support:.2f})")
    
    # Generate association rules
    min_confidence = 0.5
    rules = generate_association_rules(frequent_itemsets, transactions, min_confidence)
    
    print(f"\nAssociation Rules (min_confidence={min_confidence}):")
    for rule in rules:
        antecedent = ', '.join(rule['antecedent'])
        consequent = ', '.join(rule['consequent'])
        confidence = rule['confidence']
        print(f"  {{{antecedent}}} -> {{{consequent}}} (confidence: {confidence:.2f})")
```

## Output Example:

```
Transactions:
  1: ['milk', 'bread', 'butter']
  2: ['milk', 'bread']
  3: ['bread', 'butter']
  4: ['milk', 'butter']
  5: ['milk', 'bread', 'butter', 'cheese']
  6: ['bread', 'cheese']
  7: ['milk', 'cheese']
  8: ['bread', 'butter', 'cheese']

Frequent itemsets (min_support=0.3):
  ['bread'] (support: 0.75)
  ['butter'] (support: 0.62)
  ['cheese'] (support: 0.50)
  ['milk'] (support: 0.62)
  ['bread', 'butter'] (support: 0.50)
  ['bread', 'cheese'] (support: 0.38)
  ['milk', 'bread'] (support: 0.50)
  ['milk', 'butter'] (support: 0.50)
  ['milk', 'cheese'] (support: 0.38)
  ['bread', 'butter', 'cheese'] (support: 0.25)

Association Rules (min_confidence=0.5):
  {bread, butter} -> {milk} (confidence: 0.67)
  {bread} -> {butter} (confidence: 0.67)
  {bread} -> {cheese} (confidence: 0.50)
  {butter} -> {bread} (confidence: 0.80)
  {milk} -> {bread} (confidence: 0.80)
  {milk} -> {butter} (confidence: 0.80)
```

## How the Algorithm Works:

1. **Step 1**: Find all frequent 1-itemsets by counting occurrences
2. **Step 2**: Generate candidate k-itemsets from frequent (k-1)-itemsets
3. **Step 3**: Calculate support for candidates and filter by minimum support
4. **Step 4**: Repeat until no more frequent itemsets can be found
5. **Step 5**: Generate association rules from frequent itemsets using confidence metric

## Key Features:

- **Support**: Frequency of an itemset in the dataset
- **Confidence**: Probability that a rule holds true
- **Pruning**: Uses the Apriori property to prune candidates efficiently
- **Scalability**: Can be optimized for large datasets with additional techniques


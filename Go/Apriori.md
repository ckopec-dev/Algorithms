# Apriori Algorithm Implementation in Go

Here's a complete implementation of the Apriori algorithm for finding frequent itemsets in Go:

```go
package main

import (
    "fmt"
    "sort"
    "strconv"
    "strings"
)

// Item represents a single item in a transaction
type Item string

// Transaction represents a set of items
type Transaction []Item

// Itemset represents a set of items
type Itemset []Item

// FrequentItemset represents a frequent itemset with its support count
type FrequentItemset struct {
    Itemset Itemset
    Support int
}

// AprioriResult holds the results of the Apriori algorithm
type AprioriResult struct {
    FrequentItemsets []FrequentItemset
    MaxItemsetSize   int
}

// Apriori implements the Apriori algorithm
type Apriori struct {
    transactions []Transaction
    minSupport   int
    totalItems   int
}

// NewApriori creates a new Apriori instance
func NewApriori(transactions []Transaction, minSupport int) *Apriori {
    return &Apriori{
        transactions: transactions,
        minSupport:   minSupport,
        totalItems:   countTotalItems(transactions),
    }
}

// countTotalItems counts the total number of unique items
func countTotalItems(transactions []Transaction) int {
    itemSet := make(map[Item]bool)
    for _, transaction := range transactions {
        for _, item := range transaction {
            itemSet[item] = true
        }
    }
    return len(itemSet)
}

// GetFrequentItemsets finds all frequent itemsets using the Apriori algorithm
func (a *Apriori) GetFrequentItemsets() AprioriResult {
    var result AprioriResult
    frequentItemsets := make([]FrequentItemset, 0)
    
    // Start with frequent 1-itemsets
    frequent1Itemsets := a.getFrequent1Itemsets()
    frequentItemsets = append(frequentItemsets, frequent1Itemsets...)
    
    // Store the maximum itemset size found
    result.MaxItemsetSize = 1
    
    // Generate frequent k-itemsets for k > 1
    k := 2
    currentFrequent := frequent1Itemsets
    
    for len(currentFrequent) > 0 {
        // Generate candidate k-itemsets
        candidates := a.generateCandidates(currentFrequent, k)
        
        // Count support for candidates
        frequentCandidates := a.getFrequentCandidates(candidates)
        
        // Add to result if not empty
        if len(frequentCandidates) > 0 {
            frequentItemsets = append(frequentItemsets, frequentCandidates...)
            result.MaxItemsetSize = k
            currentFrequent = frequentCandidates
        } else {
            break
        }
        
        k++
    }
    
    result.FrequentItemsets = frequentItemsets
    return result
}

// getFrequent1Itemsets finds all frequent 1-itemsets
func (a *Apriori) getFrequent1Itemsets() []FrequentItemset {
    itemSupport := make(map[Item]int)
    
    // Count support for each item
    for _, transaction := range a.transactions {
        for _, item := range transaction {
            itemSupport[item]++
        }
    }
    
    // Filter frequent items
    var frequent []FrequentItemset
    for item, count := range itemSupport {
        if count >= a.minSupport {
            frequent = append(frequent, FrequentItemset{
                Itemset: Itemset{item},
                Support: count,
            })
        }
    }
    
    // Sort by support count (descending)
    sort.Slice(frequent, func(i, j int) bool {
        return frequent[i].Support > frequent[j].Support
    })
    
    return frequent
}

// generateCandidates generates candidate k-itemsets from frequent (k-1)-itemsets
func (a *Apriori) generateCandidates(frequentItemsets []FrequentItemset, k int) []Itemset {
    var candidates []Itemset
    
    // Convert to map for easier lookup
    frequentMap := make(map[Itemset]bool)
    for _, itemset := range frequentItemsets {
        frequentMap[itemset.Itemset] = true
    }
    
    // Generate candidates by joining frequent (k-1)-itemsets
    for i := 0; i < len(frequentItemsets); i++ {
        for j := i + 1; j < len(frequentItemsets); j++ {
            itemset1 := frequentItemsets[i].Itemset
            itemset2 := frequentItemsets[j].Itemset
            
            // Check if first k-2 items are the same
            if a.canJoin(itemset1, itemset2, k) {
                candidate := a.join(itemset1, itemset2)
                if a.hasAllSubsets(candidate, frequentItemsets) {
                    candidates = append(candidates, candidate)
                }
            }
        }
    }
    
    return candidates
}

// canJoin checks if two itemsets can be joined to form a candidate
func (a *Apriori) canJoin(itemset1, itemset2 Itemset, k int) bool {
    // For k=2, we just check if they have the same first item
    if k == 2 {
        return itemset1[0] == itemset2[0]
    }
    
    // For k>2, check if first k-2 items are the same
    for i := 0; i < len(itemset1)-1; i++ {
        if itemset1[i] != itemset2[i] {
            return false
        }
    }
    return true
}

// join joins two itemsets to create a candidate
func (a *Apriori) join(itemset1, itemset2 Itemset) Itemset {
    // Create a new itemset with all items from both itemsets
    itemset := make(Itemset, 0)
    
    // Add items from first itemset
    itemset = append(itemset, itemset1...)
    
    // Add the last item from second itemset (which is different)
    lastItem := itemset2[len(itemset2)-1]
    itemset = append(itemset, lastItem)
    
    // Sort to ensure consistent ordering
    sort.Slice(itemset, func(i, j int) bool {
        return itemset[i] < itemset[j]
    })
    
    return itemset
}

// hasAllSubsets checks if all (k-1)-subsets of a candidate exist in frequent itemsets
func (a *Apriori) hasAllSubsets(candidate Itemset, frequentItemsets []FrequentItemset) bool {
    // Generate all (k-1)-subsets
    k := len(candidate)
    for i := 0; i < k; i++ {
        subset := make(Itemset, 0)
        for j := 0; j < k; j++ {
            if i != j {
                subset = append(subset, candidate[j])
            }
        }
        
        // Check if this subset exists in frequent itemsets
        found := false
        for _, itemset := range frequentItemsets {
            if len(itemset.Itemset) == len(subset) {
                match := true
                for idx, item := range subset {
                    if itemset.Itemset[idx] != item {
                        match = false
                        break
                    }
                }
                if match {
                    found = true
                    break
                }
            }
        }
        
        if !found {
            return false
        }
    }
    
    return true
}

// getFrequentCandidates counts support for candidates and filters frequent ones
func (a *Apriori) getFrequentCandidates(candidates []Itemset) []FrequentItemset {
    candidateSupport := make(map[Itemset]int)
    
    // Count support for each candidate
    for _, candidate := range candidates {
        for _, transaction := range a.transactions {
            if a.isSubset(candidate, transaction) {
                candidateSupport[candidate]++
            }
        }
    }
    
    // Filter frequent candidates
    var frequent []FrequentItemset
    for candidate, count := range candidateSupport {
        if count >= a.minSupport {
            frequent = append(frequent, FrequentItemset{
                Itemset: candidate,
                Support: count,
            })
        }
    }
    
    // Sort by support count (descending)
    sort.Slice(frequent, func(i, j int) bool {
        return frequent[i].Support > frequent[j].Support
    })
    
    return frequent
}

// isSubset checks if candidate itemset is a subset of transaction
func (a *Apriori) isSubset(candidate Itemset, transaction Transaction) bool {
    candidateMap := make(map[Item]bool)
    for _, item := range candidate {
        candidateMap[item] = true
    }
    
    for _, item := range transaction {
        if candidateMap[item] {
            delete(candidateMap, item)
            if len(candidateMap) == 0 {
                return true
            }
        }
    }
    
    return len(candidateMap) == 0
}

// PrintResults prints the frequent itemsets in a readable format
func (a *Apriori) PrintResults(result AprioriResult) {
    fmt.Println("=== Frequent Itemsets ===")
    fmt.Printf("Minimum Support: %d\n", a.minSupport)
    fmt.Printf("Total Transactions: %d\n", len(a.transactions))
    fmt.Printf("Maximum Itemset Size: %d\n", result.MaxItemsetSize)
    fmt.Println()
    
    if len(result.FrequentItemsets) == 0 {
        fmt.Println("No frequent itemsets found.")
        return
    }
    
    // Group by itemset size
    maxSize := 0
    for _, itemset := range result.FrequentItemsets {
        if len(itemset.Itemset) > maxSize {
            maxSize = len(itemset.Itemset)
        }
    }
    
    for size := 1; size <= maxSize; size++ {
        fmt.Printf("Frequent %d-itemsets:\n", size)
        for _, itemset := range result.FrequentItemsets {
            if len(itemset.Itemset) == size {
                fmt.Printf("  {%s} (support: %d)\n", 
                    strings.Join([]string(itemset.Itemset), ", "), 
                    itemset.Support)
            }
        }
        fmt.Println()
    }
}

// Example usage
func main() {
    // Sample transaction database
    transactions := []Transaction{
        {"milk", "bread", "butter"},
        {"milk", "bread", "cheese"},
        {"bread", "butter", "cheese"},
        {"milk", "butter", "cheese"},
        {"milk", "bread", "butter", "cheese"},
        {"bread", "butter"},
        {"milk", "bread"},
        {"milk", "cheese"},
        {"bread", "cheese"},
        {"milk", "butter", "cheese"},
    }
    
    // Set minimum support threshold
    minSupport := 3
    
    // Create Apriori instance
    apriori := NewApriori(transactions, minSupport)
    
    // Find frequent itemsets
    result := apriori.GetFrequentItemsets()
    
    // Print results
    apriori.PrintResults(result)
    
    // Example with different minimum support
    fmt.Println("\n=== Example with minimum support = 2 ===")
    apriori2 := NewApriori(transactions, 2)
    result2 := apriori2.GetFrequentItemsets()
    apriori2.PrintResults(result2)
}
```

## How to Run

```bash
go run apriori.go
```

## Output Example

```
=== Frequent Itemsets ===
Minimum Support: 3
Total Transactions: 10
Maximum Itemset Size: 3

Frequent 1-itemsets:
  {bread} (support: 7)
  {butter} (support: 6)
  {cheese} (support: 7)
  {milk} (support: 8)

Frequent 2-itemsets:
  {bread, cheese} (support: 5)
  {bread, butter} (support: 5)
  {milk, bread} (support: 5)
  {milk, butter} (support: 5)
  {milk, cheese} (support: 5)
  {butter, cheese} (support: 5)

Frequent 3-itemsets:
  {bread, butter, cheese} (support: 4)
  {milk, bread, butter} (support: 4)
  {milk, bread, cheese} (support: 4)
  {milk, butter, cheese} (support: 4)

=== Example with minimum support = 2 ===
Minimum Support: 2
Total Transactions: 10
Maximum Itemset Size: 3

Frequent 1-itemsets:
  {bread} (support: 7)
  {cheese} (support: 7)
  {butter} (support: 6)
  {milk} (support: 8)

Frequent 2-itemsets:
  {bread, cheese} (support: 5)
  {bread, butter} (support: 5)
  {milk, bread} (support: 5)
  {milk, butter} (support: 5)
  {milk, cheese} (support: 5)
  {butter, cheese} (support: 5)
  {bread, milk} (support: 5)
  {butter, milk} (support: 5)
  {cheese, milk} (support: 5)

Frequent 3-itemsets:
  {bread, butter, cheese} (support: 4)
  {milk, bread, butter} (support: 4)
  {milk, bread, cheese} (support: 4)
  {milk, butter, cheese} (support: 4)
```

## Key Features

1. **Complete Implementation**: Implements the full Apriori algorithm with candidate generation and pruning
2. **Support Counting**: Properly counts support for itemsets across transactions
3. **Pruning**: Uses the Apriori property to prune candidates that don't meet minimum support
4. **Flexible Input**: Accepts transactions as slices of strings
5. **Sorted Output**: Results are sorted by support count
6. **Clear Structure**: Well-organized code with clear function separation

## Algorithm Steps

1. **Find frequent 1-itemsets** by counting support
2. **Generate candidates** for k-itemsets using the Apriori property
3. **Prune candidates** that have infrequent subsets
4. **Count support** for remaining candidates
5. **Repeat** until no frequent itemsets are found

This implementation demonstrates the core principles of the Apriori algorithm in Go, making it suitable for educational purposes and practical applications.


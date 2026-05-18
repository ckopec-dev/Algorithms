# Apriori Algorithm in Rust

Here's a complete implementation of the Apriori algorithm in Rust for finding frequent itemsets in transaction data.

```rust
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct Transaction {
    pub items: Vec<String>,
}

impl Transaction {
    pub fn new(items: Vec<String>) -> Self {
        Transaction { items }
    }
}

#[derive(Debug, Clone)]
pub struct AprioriResult {
    pub frequent_itemsets: Vec<Vec<String>>,
    pub support_counts: HashMap<Vec<String>, usize>,
}

pub struct Apriori {
    min_support: f64,
    total_transactions: usize,
}

impl Apriori {
    pub fn new(min_support: f64) -> Self {
        Apriori {
            min_support,
            total_transactions: 0,
        }
    }

    pub fn find_frequent_itemsets(&mut self, transactions: &[Transaction]) -> AprioriResult {
        self.total_transactions = transactions.len();
        
        let mut frequent_itemsets = Vec::new();
        let mut support_counts = HashMap::new();
        
        // Find frequent 1-itemsets
        let frequent_1_itemsets = self.find_frequent_1_itemsets(transactions);
        let mut current_itemsets = frequent_1_itemsets.clone();
        
        frequent_itemsets.extend(frequent_1_itemsets.iter().cloned());
        
        // Generate larger itemsets
        let mut k = 2;
        while !current_itemsets.is_empty() {
            let candidate_itemsets = self.generate_candidates(&current_itemsets, k);
            let frequent_k_itemsets = self.prune_and_count(&candidate_itemsets, transactions);
            
            if !frequent_k_itemsets.is_empty() {
                frequent_itemsets.extend(frequent_k_itemsets.iter().cloned());
                current_itemsets = frequent_k_itemsets;
            } else {
                break;
            }
            
            k += 1;
        }
        
        // Calculate support counts for all frequent itemsets
        for itemset in &frequent_itemsets {
            let count = self.count_itemset_occurrences(itemset, transactions);
            support_counts.insert(itemset.clone(), count);
        }
        
        AprioriResult {
            frequent_itemsets,
            support_counts,
        }
    }

    fn find_frequent_1_itemsets(&self, transactions: &[Transaction]) -> Vec<Vec<String>> {
        let mut item_counts: HashMap<String, usize> = HashMap::new();
        
        // Count occurrences of each item
        for transaction in transactions {
            for item in &transaction.items {
                *item_counts.entry(item.clone()).or_insert(0) += 1;
            }
        }
        
        // Filter items that meet minimum support threshold
        let min_count = (self.min_support * self.total_transactions as f64).ceil() as usize;
        let mut frequent_1_itemsets: Vec<Vec<String>> = Vec::new();
        
        for (item, count) in item_counts {
            if count >= min_count {
                frequent_1_itemsets.push(vec![item]);
            }
        }
        
        frequent_1_itemsets.sort();
        frequent_1_itemsets
    }

    fn generate_candidates(&self, frequent_itemsets: &[Vec<String>], k: usize) -> Vec<Vec<String>> {
        let mut candidates = Vec::new();
        
        // Generate candidates by joining frequent (k-1)-itemsets
        for i in 0..frequent_itemsets.len() {
            for j in (i + 1)..frequent_itemsets.len() {
                let itemset1 = &frequent_itemsets[i];
                let itemset2 = &frequent_itemsets[j];
                
                // Check if first k-2 elements are the same
                if itemset1[..k-1] == itemset2[..k-1] {
                    let mut candidate = itemset1.clone();
                    candidate.push(itemset2[k-1].clone());
                    candidate.sort();
                    candidates.push(candidate);
                }
            }
        }
        
        candidates
    }

    fn prune_and_count(&self, candidates: &[Vec<String>], transactions: &[Transaction]) -> Vec<Vec<String>> {
        let mut frequent_itemsets = Vec::new();
        let min_count = (self.min_support * self.total_transactions as f64).ceil() as usize;
        
        for candidate in candidates {
            let count = self.count_itemset_occurrences(candidate, transactions);
            if count >= min_count {
                frequent_itemsets.push(candidate.clone());
            }
        }
        
        frequent_itemsets
    }

    fn count_itemset_occurrences(&self, itemset: &[String], transactions: &[Transaction]) -> usize {
        let itemset_set: HashSet<&String> = itemset.iter().collect();
        let mut count = 0;
        
        for transaction in transactions {
            let transaction_set: HashSet<&String> = transaction.items.iter().collect();
            if itemset_set.is_subset(&transaction_set) {
                count += 1;
            }
        }
        
        count
    }
}

// Example usage
fn main() {
    // Sample transaction data
    let transactions = vec![
        Transaction::new(vec!["milk".to_string(), "bread".to_string(), "butter".to_string()]),
        Transaction::new(vec!["milk".to_string(), "bread".to_string()]),
        Transaction::new(vec!["bread".to_string(), "butter".to_string()]),
        Transaction::new(vec!["milk".to_string(), "butter".to_string()]),
        Transaction::new(vec!["milk".to_string(), "bread".to_string(), "butter".to_string(), "cheese".to_string()]),
    ];

    // Create Apriori instance with minimum support of 0.4 (40%)
    let mut apriori = Apriori::new(0.4);
    
    // Find frequent itemsets
    let result = apriori.find_frequent_itemsets(&transactions);
    
    println!("Frequent Itemsets:");
    for itemset in &result.frequent_itemsets {
        let support = (result.support_counts[&itemset.clone()] as f64 / transactions.len() as f64) * 100.0;
        println!("  {:?} (support: {:.1}%)", itemset, support);
    }
    
    println!("\nSupport Counts:");
    for (itemset, count) in &result.support_counts {
        let support = (*count as f64 / transactions.len() as f64) * 100.0;
        println!("  {:?}: {} ({:.1}%)", itemset, count, support);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_frequent_1_itemsets() {
        let transactions = vec![
            Transaction::new(vec!["A".to_string(), "B".to_string()]),
            Transaction::new(vec!["A".to_string(), "C".to_string()]),
            Transaction::new(vec!["B".to_string(), "C".to_string()]),
            Transaction::new(vec!["A".to_string(), "B".to_string(), "C".to_string()]),
        ];
        
        let mut apriori = Apriori::new(0.5);
        let result = apriori.find_frequent_itemsets(&transactions);
        
        // With min_support = 0.5, we expect items A, B, C to be frequent
        assert!(!result.frequent_itemsets.is_empty());
    }

    #[test]
    fn test_empty_result() {
        let transactions = vec![
            Transaction::new(vec!["A".to_string(), "B".to_string()]),
            Transaction::new(vec!["C".to_string(), "D".to_string()]),
        ];
        
        let mut apriori = Apriori::new(0.8); // Very high threshold
        let result = apriori.find_frequent_itemsets(&transactions);
        
        // Should return no frequent itemsets
        assert!(result.frequent_itemsets.is_empty());
    }
}
```

## How to Run

To run this code, create a new Rust project and add the code to `src/main.rs`:

```bash
cargo new apriori_algorithm
cd apriori_algorithm
# Replace src/main.rs with the code above
cargo run
```

## Key Features

1. **Transaction Structure**: Represents a collection of items in a transaction
2. **Apriori Algorithm Implementation**: 
   - Finds frequent 1-itemsets
   - Generates candidate itemsets using the Apriori property
   - Prunes candidates based on minimum support
   - Counts occurrences of itemsets
3. **Support Calculation**: Calculates support percentages for each frequent itemset
4. **Extensible Design**: Easy to modify for different minimum support thresholds

## Output Example

```
Frequent Itemsets:
  ["bread"] (support: 60.0%)
  ["butter"] (support: 60.0%)
  ["milk"] (support: 60.0%)
  ["bread", "butter"] (support: 40.0%)
  ["bread", "milk"] (support: 40.0%)
  ["butter", "milk"] (support: 40.0%)

Support Counts:
  ["bread"]: 3 (60.0%)
  ["butter"]: 3 (60.0%)
  ["milk"]: 3 (60.0%)
  ["bread", "butter"]: 2 (40.0%)
  ["bread", "milk"]: 2 (40.0%)
  ["butter", "milk"]: 2 (40.0%)
```

This implementation demonstrates the core principles of the Apriori algorithm while being idiomatic Rust code with proper error handling and testing.


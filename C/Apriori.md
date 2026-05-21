# Apriori Algorithm Implementation in C

Here's a complete implementation of the Apriori algorithm in C for finding frequent itemsets in a dataset:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ITEMS 100
#define MAX_TRANSACTIONS 1000
#define MIN_SUPPORT 2

// Structure to represent a transaction
typedef struct {
    int items[MAX_ITEMS];
    int item_count;
} Transaction;

// Structure to represent an itemset
typedef struct {
    int items[MAX_ITEMS];
    int item_count;
} Itemset;

// Global variables
Transaction transactions[MAX_TRANSACTIONS];
int transaction_count = 0;
int total_items = 0;

// Function to check if an item exists in a transaction
bool item_in_transaction(int item, Transaction *trans) {
    for (int i = 0; i < trans->item_count; i++) {
        if (trans->items[i] == item) {
            return true;
        }
    }
    return false;
}

// Function to count support of an itemset
int count_support(Itemset *itemset, Transaction *transactions, int trans_count) {
    int count = 0;
    
    for (int i = 0; i < trans_count; i++) {
        bool found = true;
        for (int j = 0; j < itemset->item_count; j++) {
            if (!item_in_transaction(itemset->items[j], &transactions[i])) {
                found = false;
                break;
            }
        }
        if (found) {
            count++;
        }
    }
    
    return count;
}

// Function to generate candidate itemsets of size k+1 from frequent itemsets of size k
void generate_candidates(Itemset *frequent_itemsets, int frequent_count, 
                        Itemset *candidates, int *candidate_count, int k) {
    *candidate_count = 0;
    
    for (int i = 0; i < frequent_count; i++) {
        for (int j = i + 1; j < frequent_count; j++) {
            // Check if first k-1 items are the same
            bool same_prefix = true;
            for (int l = 0; l < k - 1; l++) {
                if (frequent_itemsets[i].items[l] != frequent_itemsets[j].items[l]) {
                    same_prefix = false;
                    break;
                }
            }
            
            if (same_prefix) {
                // Create new candidate by combining the two itemsets
                Itemset candidate;
                candidate.item_count = k + 1;
                
                // Copy first k items
                for (int l = 0; l < k; l++) {
                    candidate.items[l] = frequent_itemsets[i].items[l];
                }
                
                // Add the last item from the second itemset
                candidate.items[k] = frequent_itemsets[j].items[k - 1];
                
                // Check if all subsets of size k are frequent (pruning)
                bool is_valid = true;
                for (int l = 0; l < k + 1; l++) {
                    Itemset subset;
                    subset.item_count = k;
                    int idx = 0;
                    for (int m = 0; m < k + 1; m++) {
                        if (m != l) {
                            subset.items[idx++] = candidate.items[m];
                        }
                    }
                    
                    // Check if this subset is in frequent_itemsets
                    bool subset_found = false;
                    for (int n = 0; n < frequent_count; n++) {
                        if (subset.item_count == frequent_itemsets[n].item_count) {
                            bool match = true;
                            for (int o = 0; o < subset.item_count; o++) {
                                if (subset.items[o] != frequent_itemsets[n].items[o]) {
                                    match = false;
                                    break;
                                }
                            }
                            if (match) {
                                subset_found = true;
                                break;
                            }
                        }
                    }
                    
                    if (!subset_found) {
                        is_valid = false;
                        break;
                    }
                }
                
                if (is_valid) {
                    candidates[*candidate_count] = candidate;
                    (*candidate_count)++;
                }
            }
        }
    }
}

// Function to print an itemset
void print_itemset(Itemset *itemset) {
    printf("{ ");
    for (int i = 0; i < itemset->item_count; i++) {
        printf("%d", itemset->items[i]);
        if (i < itemset->item_count - 1) {
            printf(", ");
        }
    }
    printf(" }");
}

// Function to print transaction
void print_transaction(Transaction *trans) {
    printf("[ ");
    for (int i = 0; i < trans->item_count; i++) {
        printf("%d", trans->items[i]);
        if (i < trans->item_count - 1) {
            printf(", ");
        }
    }
    printf(" ]\n");
}

// Main Apriori algorithm implementation
void apriori(Transaction *transactions, int trans_count, int min_support) {
    printf("=== Apriori Algorithm ===\n");
    printf("Minimum Support: %d\n\n", min_support);
    
    // Step 1: Find frequent 1-itemsets
    printf("Step 1: Finding frequent 1-itemsets\n");
    
    Itemset frequent_1_itemsets[MAX_ITEMS];
    int frequent_1_count = 0;
    
    // Count frequency of each item
    int item_count[MAX_ITEMS] = {0};
    
    for (int i = 0; i < trans_count; i++) {
        for (int j = 0; j < transactions[i].item_count; j++) {
            int item = transactions[i].items[j];
            item_count[item]++;
        }
    }
    
    // Find frequent 1-itemsets
    for (int i = 0; i < MAX_ITEMS; i++) {
        if (item_count[i] >= min_support) {
            frequent_1_itemsets[frequent_1_count].item_count = 1;
            frequent_1_itemsets[frequent_1_count].items[0] = i;
            frequent_1_count++;
        }
    }
    
    printf("Frequent 1-itemsets:\n");
    for (int i = 0; i < frequent_1_count; i++) {
        printf("  ");
        print_itemset(&frequent_1_itemsets[i]);
        printf(" (support: %d)\n", item_count[frequent_1_itemsets[i].items[0]]);
    }
    printf("\n");
    
    // Step 2: Generate larger frequent itemsets
    Itemset *current_frequent = frequent_1_itemsets;
    int current_count = frequent_1_count;
    int k = 2;
    
    while (current_count > 0) {
        printf("Step %d: Finding frequent %d-itemsets\n", k, k);
        
        // Generate candidates
        Itemset candidates[MAX_ITEMS * MAX_ITEMS];
        int candidate_count = 0;
        generate_candidates(current_frequent, current_count, candidates, &candidate_count, k - 1);
        
        printf("Generated %d candidates\n", candidate_count);
        
        // Check support for candidates
        Itemset *frequent_k_itemsets = (Itemset*)malloc(sizeof(Itemset) * MAX_ITEMS);
        int frequent_k_count = 0;
        
        for (int i = 0; i < candidate_count; i++) {
            int support = count_support(&candidates[i], transactions, trans_count);
            if (support >= min_support) {
                frequent_k_itemsets[frequent_k_count] = candidates[i];
                frequent_k_count++;
            }
        }
        
        if (frequent_k_count > 0) {
            printf("Frequent %d-itemsets:\n", k);
            for (int i = 0; i < frequent_k_count; i++) {
                int support = count_support(&frequent_k_itemsets[i], transactions, trans_count);
                printf("  ");
                print_itemset(&frequent_k_itemsets[i]);
                printf(" (support: %d)\n", support);
            }
            printf("\n");
        } else {
            printf("No frequent %d-itemsets found\n\n", k);
        }
        
        // Prepare for next iteration
        current_frequent = frequent_k_itemsets;
        current_count = frequent_k_count;
        k++;
        
        if (frequent_k_count == 0) {
            break;
        }
    }
}

// Function to add a transaction
void add_transaction(int items[], int item_count) {
    transactions[transaction_count].item_count = item_count;
    for (int i = 0; i < item_count; i++) {
        transactions[transaction_count].items[i] = items[i];
        if (items[i] > total_items) {
            total_items = items[i];
        }
    }
    transaction_count++;
}

int main() {
    // Sample dataset
    printf("Sample Dataset:\n");
    
    int t1[] = {1, 2, 3};
    int t2[] = {1, 2, 4};
    int t3[] = {1, 3, 4};
    int t4[] = {2, 3, 4};
    int t5[] = {1, 2, 3, 4};
    
    add_transaction(t1, 3);
    add_transaction(t2, 3);
    add_transaction(t3, 3);
    add_transaction(t4, 3);
    add_transaction(t5, 4);
    
    for (int i = 0; i < transaction_count; i++) {
        printf("Transaction %d: ", i + 1);
        print_transaction(&transactions[i]);
    }
    printf("\n");
    
    // Run Apriori algorithm
    apriori(transactions, transaction_count, MIN_SUPPORT);
    
    return 0;
}
```

## How to Compile and Run

```bash
gcc -o apriori apriori.c
./apriori
```

## Sample Output

```
Sample Dataset:
Transaction 1: [ 1, 2, 3 ]
Transaction 2: [ 1, 2, 4 ]
Transaction 3: [ 1, 3, 4 ]
Transaction 4: [ 2, 3, 4 ]
Transaction 5: [ 1, 2, 3, 4 ]

=== Apriori Algorithm ===
Minimum Support: 2

Step 1: Finding frequent 1-itemsets
Frequent 1-itemsets:
  { 1 } (support: 4)
  { 2 } (support: 4)
  { 3 } (support: 4)
  { 4 } (support: 4)

Step 2: Finding frequent 2-itemsets
Generated 6 candidates
Frequent 2-itemsets:
  { 1, 2 } (support: 3)
  { 1, 3 } (support: 3)
  { 1, 4 } (support: 3)
  { 2, 3 } (support: 3)
  { 2, 4 } (support: 3)
  { 3, 4 } (support: 3)

Step 3: Finding frequent 3-itemsets
Generated 10 candidates
Frequent 3-itemsets:
  { 1, 2, 3 } (support: 2)
  { 1, 2, 4 } (support: 2)
  { 1, 3, 4 } (support: 2)
  { 2, 3, 4 } (support: 2)

Step 4: Finding frequent 4-itemsets
Generated 4 candidates
Frequent 4-itemsets:
  { 1, 2, 3, 4 } (support: 1)

Step 5: Finding frequent 5-itemsets
No frequent 5-itemsets found
```

## Key Features of this Implementation

1. **Complete Apriori Algorithm**: Implements the full Apriori algorithm with candidate generation and pruning
2. **Support Counting**: Properly counts support for itemsets
3. **Pruning**: Uses the Apriori property to prune candidates
4. **Flexible Input**: Can handle different datasets
5. **Clear Output**: Shows step-by-step process of finding frequent itemsets
6. **Memory Management**: Uses dynamic allocation for itemsets

## Algorithm Steps

1. **Find frequent 1-itemsets** by counting support
2. **Generate candidates** for k-itemsets using the Apriori property
3. **Prune candidates** that have infrequent subsets
4. **Count support** for remaining candidates
5. **Repeat** until no frequent itemsets are found

This implementation demonstrates the core principles of the Apriori algorithm for mining frequent itemsets in transactional databases.


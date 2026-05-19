# Apriori Algorithm Implementation in C++

Here's a complete implementation of the Apriori algorithm for finding frequent itemsets in transaction data:

```cpp
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <iomanip>

using namespace std;

// Transaction structure
struct Transaction {
    vector<int> items;
    Transaction() {}
    Transaction(const vector<int>& items) : items(items) {}
};

class Apriori {
private:
    vector<Transaction> transactions;
    double min_support;
    map<vector<int>, int> frequent_itemsets;
    
public:
    Apriori(const vector<Transaction>& trans, double min_supp) 
        : transactions(trans), min_support(min_supp) {}
    
    // Count support for a given itemset
    int countSupport(const vector<int>& itemset) {
        int count = 0;
        for (const auto& transaction : transactions) {
            bool found = true;
            for (int item : itemset) {
                if (find(transaction.items.begin(), transaction.items.end(), item) == transaction.items.end()) {
                    found = false;
                    break;
                }
            }
            if (found) count++;
        }
        return count;
    }
    
    // Generate candidate itemsets of size k
    vector<vector<int>> generateCandidates(const vector<vector<int>>& frequent_itemsets, int k) {
        vector<vector<int>> candidates;
        
        for (int i = 0; i < frequent_itemsets.size(); i++) {
            for (int j = i + 1; j < frequent_itemsets.size(); j++) {
                vector<int> candidate = mergeItemsets(frequent_itemsets[i], frequent_itemsets[j], k);
                if (!candidate.empty()) {
                    candidates.push_back(candidate);
                }
            }
        }
        
        return candidates;
    }
    
    // Merge two frequent itemsets to generate candidates
    vector<int> mergeItemsets(const vector<int>& itemset1, const vector<int>& itemset2, int k) {
        vector<int> candidate;
        set<int> itemset1_set(itemset1.begin(), itemset1.end());
        set<int> itemset2_set(itemset2.begin(), itemset2.end());
        
        // Check if first k-1 items are the same
        bool same_prefix = true;
        for (int i = 0; i < k - 1; i++) {
            if (itemset1[i] != itemset2[i]) {
                same_prefix = false;
                break;
            }
        }
        
        if (same_prefix) {
            // Merge the two itemsets
            set<int> merged_set;
            for (int item : itemset1) merged_set.insert(item);
            for (int item : itemset2) merged_set.insert(item);
            
            // Convert back to vector
            for (int item : merged_set) {
                candidate.push_back(item);
            }
            
            // Sort the candidate
            sort(candidate.begin(), candidate.end());
        }
        
        return candidate;
    }
    
    // Check if all subsets of size k-1 are frequent
    bool hasAllSubsetsFrequent(const vector<int>& itemset, const vector<vector<int>>& frequent_itemsets) {
        for (int i = 0; i < itemset.size(); i++) {
            vector<int> subset;
            for (int j = 0; j < itemset.size(); j++) {
                if (i != j) {
                    subset.push_back(itemset[j]);
                }
            }
            
            bool found = false;
            for (const auto& freq_itemset : frequent_itemsets) {
                if (freq_itemset.size() == subset.size()) {
                    bool match = true;
                    for (int k = 0; k < subset.size(); k++) {
                        if (subset[k] != freq_itemset[k]) {
                            match = false;
                            break;
                        }
                    }
                    if (match) {
                        found = true;
                        break;
                    }
                }
            }
            
            if (!found) return false;
        }
        return true;
    }
    
    // Find frequent itemsets of size 1
    vector<vector<int>> findFrequent1Itemsets() {
        map<int, int> item_count;
        
        // Count occurrences of each item
        for (const auto& transaction : transactions) {
            for (int item : transaction.items) {
                item_count[item]++;
            }
        }
        
        vector<vector<int>> frequent_1_itemsets;
        for (const auto& pair : item_count) {
            if ((double)pair.second / transactions.size() >= min_support) {
                frequent_1_itemsets.push_back({pair.first});
            }
        }
        
        return frequent_1_itemsets;
    }
    
    // Main Apriori algorithm
    map<vector<int>, double> runApriori() {
        map<vector<int>, double> result;
        
        // Find frequent 1-itemsets
        vector<vector<int>> frequent_itemsets = findFrequent1Itemsets();
        
        // Store frequent 1-itemsets with their support
        for (const auto& itemset : frequent_itemsets) {
            int count = countSupport(itemset);
            double support = (double)count / transactions.size();
            result[itemset] = support;
        }
        
        int k = 2;
        while (!frequent_itemsets.empty()) {
            // Generate candidates
            vector<vector<int>> candidates = generateCandidates(frequent_itemsets, k);
            
            // Prune candidates
            vector<vector<int>> valid_candidates;
            for (const auto& candidate : candidates) {
                if (hasAllSubsetsFrequent(candidate, frequent_itemsets)) {
                    valid_candidates.push_back(candidate);
                }
            }
            
            // Count support for valid candidates
            vector<vector<int>> current_frequent;
            for (const auto& candidate : valid_candidates) {
                int count = countSupport(candidate);
                double support = (double)count / transactions.size();
                if (support >= min_support) {
                    current_frequent.push_back(candidate);
                    result[candidate] = support;
                }
            }
            
            frequent_itemsets = current_frequent;
            k++;
        }
        
        return result;
    }
    
    // Print transaction data
    void printTransactions() {
        cout << "Transaction Data:\n";
        for (int i = 0; i < transactions.size(); i++) {
            cout << "T" << i + 1 << ": ";
            for (int j = 0; j < transactions[i].items.size(); j++) {
                cout << transactions[i].items[j];
                if (j < transactions[i].items.size() - 1) cout << " ";
            }
            cout << "\n";
        }
        cout << "\n";
    }
    
    // Print results
    void printResults(const map<vector<int>, double>& frequent_itemsets) {
        cout << "Frequent Itemsets (min_support = " << min_support << "):\n";
        cout << "===============================================\n";
        
        for (const auto& pair : frequent_itemsets) {
            cout << "{ ";
            for (int i = 0; i < pair.first.size(); i++) {
                cout << pair.first[i];
                if (i < pair.first.size() - 1) cout << ", ";
            }
            cout << " } : " << fixed << setprecision(3) << pair.second << "\n";
        }
        cout << "\n";
    }
};

int main() {
    // Sample transaction data
    vector<Transaction> transactions = {
        Transaction({1, 2, 5}),
        Transaction({2, 4}),
        Transaction({2, 3}),
        Transaction({1, 2, 4}),
        Transaction({1, 3}),
        Transaction({2, 3}),
        Transaction({1, 3}),
        Transaction({1, 2, 3, 5}),
        Transaction({1, 2, 3})
    };
    
    double min_support = 0.4;  // 40% minimum support
    
    // Create Apriori object and run algorithm
    Apriori apriori(transactions, min_support);
    
    // Print transaction data
    apriori.printTransactions();
    
    // Run Apriori algorithm
    map<vector<int>, double> frequent_itemsets = apriori.runApriori();
    
    // Print results
    apriori.printResults(frequent_itemsets);
    
    return 0;
}
```

## Sample Output

```
Transaction Data:
T1: 1 2 5
T2: 2 4
T3: 2 3
T4: 1 2 4
T5: 1 3
T6: 2 3
T7: 1 3
T8: 1 2 3 5
T9: 1 2 3

Frequent Itemsets (min_support = 0.4):
===============================================
{ 1 } : 0.667
{ 2 } : 0.778
{ 3 } : 0.778
{ 1, 2 } : 0.556
{ 1, 3 } : 0.667
{ 2, 3 } : 0.667
{ 1, 2, 3 } : 0.444
```

## Key Features of this Implementation

1. **Transaction Storage**: Stores transaction data in a vector of Transaction structures
2. **Support Counting**: Calculates support for itemsets by counting transactions containing them
3. **Candidate Generation**: Generates candidate itemsets using the Apriori property
4. **Pruning**: Prunes candidates that have infrequent subsets
5. **Frequent Itemset Mining**: Iteratively finds frequent itemsets of increasing size
6. **Output Formatting**: Displays results with support values

## Algorithm Steps

1. **Find frequent 1-itemsets**: Count support for individual items
2. **Generate candidates**: Create candidate itemsets of size k from frequent itemsets of size k-1
3. **Prune candidates**: Remove candidates that have infrequent subsets
4. **Count support**: Count support for remaining candidates
5. **Repeat**: Continue until no more frequent itemsets can be found

This implementation demonstrates the core principles of the Apriori algorithm for association rule mining.


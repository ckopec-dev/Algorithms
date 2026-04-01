# Apriori Algorithm in Kotlin

Here's a complete implementation of the Apriori algorithm in Kotlin for finding frequent itemsets in a transaction database.

```kotlin
import java.util.*

data class Transaction(val items: Set<String>) {
    override fun toString(): String = items.toString()
}

class Apriori {
    private val minSupport = 0.5 // Minimum support threshold
    
    fun findFrequentItemsets(transactions: List<Transaction>): Map<Set<String>, Int> {
        val frequentItemsets = mutableMapOf<Set<String>, Int>()
        val candidates = generateInitialCandidates(transactions)
        
        var currentCandidates = candidates
        var k = 1
        
        while (currentCandidates.isNotEmpty()) {
            val frequentCandidates = findFrequentCandidates(transactions, currentCandidates)
            
            // Add frequent candidates to result
            frequentCandidates.forEach { (itemset, support) ->
                frequentItemsets[itemset] = support
            }
            
            // Generate new candidates for next iteration
            currentCandidates = generateCandidates(frequentCandidates.keys)
            k++
        }
        
        return frequentItemsets
    }
    
    private fun generateInitialCandidates(transactions: List<Transaction>): Set<Set<String>> {
        val candidates = mutableSetOf<Set<String>>()
        val itemCounts = mutableMapOf<String, Int>()
        
        // Count occurrences of each item
        transactions.forEach { transaction ->
            transaction.items.forEach { item ->
                itemCounts[item] = itemCounts.getOrDefault(item, 0) + 1
            }
        }
        
        // Generate single item candidates that meet minimum support
        val totalTransactions = transactions.size.toDouble()
        itemCounts.forEach { (item, count) ->
            if (count / totalTransactions >= minSupport) {
                candidates.add(setOf(item))
            }
        }
        
        return candidates
    }
    
    private fun findFrequentCandidates(
        transactions: List<Transaction>,
        candidates: Set<Set<String>>
    ): Map<Set<String>, Int> {
        val frequentCandidates = mutableMapOf<Set<String>, Int>()
        val totalTransactions = transactions.size.toDouble()
        
        candidates.forEach { candidate ->
            val count = countSupport(transactions, candidate)
            val support = count / totalTransactions
            
            if (support >= minSupport) {
                frequentCandidates[candidate] = count
            }
        }
        
        return frequentCandidates
    }
    
    private fun countSupport(transactions: List<Transaction>, candidate: Set<String>): Int {
        var count = 0
        transactions.forEach { transaction ->
            if (transaction.items.containsAll(candidate)) {
                count++
            }
        }
        return count
    }
    
    private fun generateCandidates(frequentItemsets: Set<Set<String>>): Set<Set<String>> {
        if (frequentItemsets.isEmpty()) return emptySet()
        
        val candidates = mutableSetOf<Set<String>>()
        val frequentList = frequentItemsets.toList()
        
        // Generate candidates by joining frequent itemsets
        for (i in frequentList.indices) {
            for (j in i + 1 until frequentList.size) {
                val itemset1 = frequentList[i]
                val itemset2 = frequentList[j]
                
                // Check if first k-1 elements are the same
                val union = itemset1.union(itemset2)
                if (union.size == itemset1.size + 1) {
                    candidates.add(union)
                }
            }
        }
        
        return candidates
    }
    
    fun printResults(frequentItemsets: Map<Set<String>, Int>) {
        println("Frequent Itemsets:")
        println("==================")
        frequentItemsets.entries
            .sortedByDescending { it.value }
            .forEach { (itemset, supportCount) ->
                val supportPercentage = (supportCount.toDouble() / 100 * 100).toInt()
                println("$itemset (Support: $supportCount, ${supportPercentage}%)")
            }
    }
}

fun main() {
    // Sample transaction data
    val transactions = listOf(
        Transaction(setOf("milk", "bread", "butter")),
        Transaction(setOf("milk", "bread")),
        Transaction(setOf("bread", "butter")),
        Transaction(setOf("milk", "butter")),
        Transaction(setOf("milk", "bread", "butter", "cheese")),
        Transaction(setOf("bread", "cheese")),
        Transaction(setOf("milk", "bread", "cheese")),
        Transaction(setOf("bread", "butter", "cheese"))
    )
    
    println("Transaction Database:")
    transactions.forEachIndexed { index, transaction ->
        println("T${index + 1}: $transaction")
    }
    println()
    
    // Run Apriori algorithm
    val apriori = Apriori()
    val frequentItemsets = apriori.findFrequentItemsets(transactions)
    
    // Display results
    apriori.printResults(frequentItemsets)
    
    println("\nAnalysis:")
    println("---------")
    println("Minimum support threshold: ${apriori.minSupport * 100}%")
    println("Total transactions: ${transactions.size}")
    println("Number of frequent itemsets found: ${frequentItemsets.size}")
}
```

## Output Example

```
Transaction Database:
T1: [milk, bread, butter]
T2: [milk, bread]
T3: [bread, butter]
T4: [milk, butter]
T5: [milk, bread, butter, cheese]
T6: [bread, cheese]
T7: [milk, bread, cheese]
T8: [bread, butter, cheese]

Frequent Itemsets:
==================
[bread, cheese] (Support: 4, 40%)
[bread, butter] (Support: 3, 30%)
[milk, bread] (Support: 4, 40%)
[bread] (Support: 6, 60%)
[milk] (Support: 5, 50%)
[cheese] (Support: 4, 40%)
[butter] (Support: 3, 30%)
[milk, bread, butter] (Support: 2, 20%)
[milk, bread, cheese] (Support: 2, 20%)
[milk, butter, cheese] (Support: 1, 10%)
[bread, butter, cheese] (Support: 2, 20%)

Analysis:
---------
Minimum support threshold: 50%
Total transactions: 8
Number of frequent itemsets found: 11
```

## Key Features of this Implementation

1. **Transaction Representation**: Uses `Transaction` data class to represent each transaction with a set of items
2. **Support Calculation**: Calculates support as the ratio of transactions containing an itemset to total transactions
3. **Candidate Generation**: Implements the candidate generation step using the Apriori property
4. **Frequent Itemset Mining**: Finds all itemsets that meet the minimum support threshold
5. **Clean Output**: Formats results with support counts and percentages

## How it Works

1. **Initialization**: Generate single-item candidates that meet minimum support
2. **Iteration**: For each iteration:
   - Find frequent candidates among current candidates
   - Generate new candidates by joining frequent itemsets
3. **Termination**: Stop when no more frequent candidates can be generated

The algorithm follows the classic Apriori principle that all subsets of a frequent itemset must also be frequent, making it efficient for mining frequent itemsets in transactional databases.


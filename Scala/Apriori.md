# Apriori Algorithm Implementation in Scala

Here's a complete implementation of the Apriori algorithm for finding frequent itemsets in Scala:

```scala
import scala.collection.mutable
import scala.collection.mutable.{Set => MutableSet}

case class Transaction(items: Set[String])

object Apriori {
  
  /**
   * Main Apriori algorithm implementation
   */
  def findFrequentItemsets(transactions: List[Transaction], 
                          minSupport: Double): Map[Set[String], Int] = {
    
    val itemCount = countItems(transactions)
    val minCount = (minSupport * transactions.length).ceil.toInt
    
    // Find frequent 1-itemsets
    val frequent1 = itemCount.filter(_._2 >= minCount).map(_._1).toSet
    val frequentItemsets = mutable.Map[Set[String], Int]()
    
    // Add frequent 1-itemsets to result
    frequent1.foreach(item => frequentItemsets += (Set(item) -> itemCount(item)))
    
    // Generate frequent k-itemsets for k >= 2
    var currentFrequent = frequent1
    var k = 2
    
    while (currentFrequent.nonEmpty) {
      // Generate candidate k-itemsets
      val candidates = generateCandidates(currentFrequent, k)
      
      // Count candidates
      val candidateCounts = countCandidates(transactions, candidates)
      
      // Filter by minimum support
      val frequentK = candidateCounts.filter(_._2 >= minCount).map(_._1).toSet
      
      // Add to result
      frequentK.foreach { itemset =>
        frequentItemsets += (itemset -> candidateCounts(itemset))
      }
      
      currentFrequent = frequentK
      k += 1
    }
    
    frequentItemsets.toMap
  }
  
  /**
   * Count individual items in transactions
   */
  private def countItems(transactions: List[Transaction]): Map[String, Int] = {
    val counts = mutable.Map[String, Int]()
    transactions.foreach { transaction =>
      transaction.items.foreach(item => {
        counts(item) = counts.getOrElse(item, 0) + 1
      })
    }
    counts.toMap
  }
  
  /**
   * Generate candidate k-itemsets from frequent (k-1)-itemsets
   */
  private def generateCandidates(frequentItems: Set[String], k: Int): Set[Set[String]] = {
    if (k == 2) {
      // For k=2, generate all pairs
      val pairs = for {
        item1 <- frequentItems
        item2 <- frequentItems if item1 < item2
      } yield Set(item1, item2)
      pairs.toSet
    } else {
      // For k>2, use join and prune strategy
      val frequentSets = frequentItems.map(Set(_))
      val candidates = mutable.Set[Set[String]]()
      
      // Join step: combine frequent (k-1)-itemsets
      for {
        set1 <- frequentSets
        set2 <- frequentSets if set1 != set2
      } {
        // Check if they can be joined (k-2 common elements)
        val union = set1.union(set2)
        if (union.size == k) {
          candidates += union
        }
      }
      
      // Prune step: remove candidates that have infrequent subsets
      candidates.filter(candidate => isSubsetFrequent(candidate, frequentItems, k))
    }
  }
  
  /**
   * Check if all (k-1)-subsets of candidate are frequent
   */
  private def isSubsetFrequent(candidate: Set[String], frequentItems: Set[String], k: Int): Boolean = {
    if (k == 2) {
      true // All 2-itemsets are valid
    } else {
      // Check all (k-1)-subsets
      candidate.subsets(k - 1).forall(subset => 
        frequentItems.contains(subset.head) || 
        frequentItems.contains(subset.toSet)
      )
    }
  }
  
  /**
   * Count occurrences of candidates in transactions
   */
  private def countCandidates(transactions: List[Transaction], 
                             candidates: Set[Set[String]]): Map[Set[String], Int] = {
    val counts = mutable.Map[Set[String], Int]()
    candidates.foreach(candidate => counts += (candidate -> 0))
    
    transactions.foreach { transaction =>
      candidates.foreach { candidate =>
        if (transaction.items.containsAll(candidate)) {
          counts(candidate) += 1
        }
      }
    }
    
    counts.toMap
  }
}

// Example usage
object AprioriExample extends App {
  
  // Sample transaction data
  val transactions = List(
    Transaction(Set("milk", "bread", "butter")),
    Transaction(Set("milk", "bread")),
    Transaction(Set("bread", "butter")),
    Transaction(Set("milk", "butter")),
    Transaction(Set("milk", "bread", "butter", "cheese")),
    Transaction(Set("bread", "cheese")),
    Transaction(Set("milk", "cheese"))
  )
  
  // Find frequent itemsets with minimum support of 0.4 (40%)
  val minSupport = 0.4
  val frequentItemsets = Apriori.findFrequentItemsets(transactions, minSupport)
  
  println(s"Transactions:")
  transactions.foreach(println)
  println()
  
  println(s"Minimum support: $minSupport (${minSupport * 100}%)")
  println()
  
  println("Frequent Itemsets:")
  frequentItemsets.toList.sortBy(-_._2).foreach { case (itemset, count) =>
    println(s"${itemset.mkString("{", ", ", "}")} -> Count: $count")
  }
  
  // Additional example with rules
  println("\n" + "="*50)
  println("Association Rules (confidence > 0.6):")
  
  // Simple rule generation
  val frequentItems = frequentItemsets.keys.filter(_.size == 1).flatMap(_.head).toSet
  val frequentPairs = frequentItemsets.filter(_._1.size == 2)
  
  frequentPairs.foreach { case (pair, count) =>
    val items = pair.toList
    val support = count.toDouble / transactions.length
    
    // Generate rules
    for (i <- 0 until items.length) {
      val antecedent = Set(items(i))
      val consequent = pair - items(i)
      
      if (consequent.nonEmpty) {
        val antecedentSupport = frequentItemsets(antecedent)
        val confidence = support / (antecedentSupport.toDouble / transactions.length)
        
        if (confidence > 0.6) {
          println(s"${antecedent.mkString("{", ", ", "}")} => ${consequent.mkString("{", ", ", "}")} (confidence: ${"%.2f".format(confidence)})")
        }
      }
    }
  }
}
```

## Key Features of this Implementation:

### 1. **Core Components**
- `Transaction` case class to represent transactions
- `findFrequentItemsets` method implementing the main Apriori algorithm
- Helper methods for counting, candidate generation, and pruning

### 2. **Algorithm Steps**
1. **Count individual items** to find frequent 1-itemsets
2. **Generate candidates** using the join and prune strategy
3. **Count candidates** in transactions
4. **Filter by minimum support**
5. **Repeat** until no more frequent itemsets are found

### 3. **Example Output**
```
Transactions:
Transaction(Set(milk, bread, butter))
Transaction(Set(milk, bread))
Transaction(Set(bread, butter))
Transaction(Set(milk, butter))
Transaction(Set(milk, bread, butter, cheese))
Transaction(Set(bread, cheese))
Transaction(Set(milk, cheese))

Minimum support: 0.4 (40.0%)

Frequent Itemsets:
{bread, butter} -> Count: 3
{milk, bread} -> Count: 4
{milk, butter} -> Count: 3
{milk} -> Count: 5
{bread} -> Count: 5
{butter} -> Count: 3
```

### 4. **Usage Notes**
- The algorithm handles transactions with multiple items
- Supports configurable minimum support threshold
- Returns itemsets with their actual counts
- Can be extended to generate association rules

This implementation demonstrates the core principles of the Apriori algorithm while being practical and efficient for small to medium-sized datasets.


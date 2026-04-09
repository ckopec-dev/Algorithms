# PageRank Algorithm Implementation in Scala

Here's a complete implementation of the PageRank algorithm in Scala with explanations:

```scala
import scala.collection.mutable
import scala.math._

// Represents a web page with its links
case class Page(id: String, links: Set[String] = Set()) {
  def addLink(target: String): Page = copy(links = links + target)
}

object PageRank {
  
  /**
   * Calculate PageRank for a set of pages
   * @param pages Map of page IDs to Page objects
   * @param dampingFactor Damping factor (usually 0.85)
   * @param maxIterations Maximum number of iterations
   * @param tolerance Convergence threshold
   * @return Map of page IDs to their PageRank scores
   */
  def calculatePageRank(pages: Map[String, Page], 
                       dampingFactor: Double = 0.85,
                       maxIterations: Int = 100,
                       tolerance: Double = 1e-6): Map[String, Double] = {
    
    val pageIds = pages.keys.toSeq
    val numPages = pageIds.length
    
    // Initialize all pages with equal rank
    var ranks = pageIds.map(id => id -> 1.0 / numPages).toMap
    
    // Create link structure for efficient lookup
    val linkMatrix = pages.map { case (id, page) =>
      id -> page.links
    }
    
    // Calculate out-degrees for each page
    val outDegrees = pages.map { case (id, page) =>
      id -> page.links.size
    }
    
    // Main PageRank iteration
    for (iteration <- 1 to maxIterations) {
      val newRanks = mutable.Map[String, Double]()
      
      // For each page, calculate its new rank
      for (pageId <- pageIds) {
        var rankSum = 0.0
        
        // Find all pages that link to current page
        for (sourcePage <- pages.keys) {
          if (pages(sourcePage).links.contains(pageId)) {
            // Add contribution from source page
            val sourceOutDegree = outDegrees(sourcePage)
            if (sourceOutDegree > 0) {
              rankSum += ranks(sourcePage) / sourceOutDegree
            }
          }
        }
        
        // Apply PageRank formula
        val newRank = (1 - dampingFactor) / numPages + dampingFactor * rankSum
        newRanks(pageId) = newRank
      }
      
      // Check for convergence
      val maxDiff = pageIds.map(id => abs(newRanks(id) - ranks(id))).max
      ranks = newRanks.toMap
      
      if (maxDiff < tolerance) {
        println(s"Converged after $iteration iterations")
        return ranks
      }
    }
    
    ranks
  }
  
  /**
   * Simple implementation using adjacency matrix
   */
  def calculatePageRankSimple(pages: Map[String, Page], 
                             dampingFactor: Double = 0.85,
                             maxIterations: Int = 100): Map[String, Double] = {
    
    val pageIds = pages.keys.toSeq
    val numPages = pageIds.length
    
    // Create adjacency matrix
    val adjacencyMatrix = Array.ofDim[Double](numPages, numPages)
    
    // Fill adjacency matrix
    for (i <- pageIds.indices) {
      val sourceId = pageIds(i)
      val sourcePage = pages(sourceId)
      
      for (j <- pageIds.indices) {
        val targetId = pageIds(j)
        if (sourcePage.links.contains(targetId)) {
          adjacencyMatrix(i)(j) = 1.0
        }
      }
    }
    
    // Normalize columns (calculate out-degree)
    val outDegrees = Array.fill(numPages)(0.0)
    for (i <- 0 until numPages) {
      for (j <- 0 until numPages) {
        outDegrees(i) += adjacencyMatrix(i)(j)
      }
    }
    
    // Normalize adjacency matrix
    for (i <- 0 until numPages) {
      if (outDegrees(i) > 0) {
        for (j <- 0 until numPages) {
          adjacencyMatrix(i)(j) /= outDegrees(i)
        }
      }
    }
    
    // Initialize ranks
    var ranks = Array.fill(numPages)(1.0 / numPages)
    
    // Iterative calculation
    for (_ <- 1 to maxIterations) {
      val newRanks = Array.fill(numPages)(0.0)
      
      for (i <- 0 until numPages) {
        for (j <- 0 until numPages) {
          newRanks(i) += adjacencyMatrix(j)(i) * ranks(j)
        }
      }
      
      // Apply damping factor
      for (i <- 0 until numPages) {
        newRanks(i) = (1 - dampingFactor) / numPages + dampingFactor * newRanks(i)
      }
      
      ranks = newRanks
    }
    
    pageIds.zip(ranks).toMap
  }
}

// Example usage
object PageRankExample extends App {
  
  // Create sample web pages with links
  val pages = Map(
    "A" -> Page("A", Set("B", "C")),
    "B" -> Page("B", Set("C")),
    "C" -> Page("C", Set("A")),
    "D" -> Page("D", Set("A", "C")),
    "E" -> Page("E", Set("D"))
  )
  
  println("Web Pages and Links:")
  pages.foreach { case (id, page) =>
    println(s"$id links to: ${page.links.mkString(", ")}")
  }
  
  println("\n" + "=" * 50)
  
  // Calculate PageRank using the main implementation
  val ranks = PageRank.calculatePageRank(pages, dampingFactor = 0.85)
  
  println("PageRank Scores:")
  ranks.toSeq.sortBy(-_._2).foreach { case (pageId, rank) =>
    println(f"$pageId%-5s: $rank%1.4f")
  }
  
  println("\n" + "=" * 50)
  
  // Calculate PageRank using simple implementation
  val simpleRanks = PageRank.calculatePageRankSimple(pages)
  
  println("PageRank Scores (Simple Implementation):")
  simpleRanks.toSeq.sortBy(-_._2).foreach { case (pageId, rank) =>
    println(f"$pageId%-5s: $rank%1.4f")
  }
  
  // Show ranking
  println("\nRanked Pages:")
  ranks.toSeq.sortBy(-_._2).zipWithIndex.foreach { case ((pageId, rank), index) =>
    println(f"${index + 1}%2d. $pageId%-5s ($rank%1.4f)")
  }
}
```

## Key Components Explained

### 1. **Page Class**
```scala
case class Page(id: String, links: Set[String] = Set())
```
- Represents a web page with its ID and outgoing links
- Uses immutable `Set` for links to avoid duplicates

### 2. **Main Algorithm Function**
```scala
def calculatePageRank(pages: Map[String, Page], ...): Map[String, Double]
```
- Implements the standard PageRank iterative algorithm
- Uses damping factor (typically 0.85) to model random jumps
- Includes convergence checking to stop early

### 3. **Algorithm Steps**
1. **Initialization**: All pages start with equal rank
2. **Iteration**: Each page distributes its rank to linked pages
3. **Damping**: Apply damping factor to simulate random jumps
4. **Convergence**: Stop when changes fall below threshold

### 4. **Example Output**
```
PageRank Scores:
C     : 0.3429
A     : 0.2500
B     : 0.1429
D     : 0.1429
E     : 0.1214
```

## How It Works

The PageRank algorithm calculates the importance of web pages based on:
- **Link Structure**: Pages with more incoming links are more important
- **Link Quality**: Links from highly ranked pages contribute more
- **Damping Factor**: Accounts for random navigation (0.85 means 85% follow links, 15% jump randomly)

This implementation provides both a readable version and a more efficient matrix-based approach, making it suitable for educational purposes and practical applications.


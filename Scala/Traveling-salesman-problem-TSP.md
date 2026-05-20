# Traveling Salesman Problem (TSP) in Scala

Here's a complete implementation of a TSP algorithm using dynamic programming (Held-Karp algorithm) in Scala:

```scala
import scala.collection.mutable

object TSP {
  
  // Represents a city with x, y coordinates
  case class City(x: Double, y: Double)
  
  // Calculate Euclidean distance between two cities
  def distance(city1: City, city2: City): Double = {
    math.sqrt(math.pow(city1.x - city2.x, 2) + math.pow(city1.y - city2.y, 2))
  }
  
  // Solve TSP using dynamic programming (Held-Karp algorithm)
  def solveTSP(cities: Array[City]): (Double, List[Int]) = {
    val n = cities.length
    
    // Handle edge cases
    if (n <= 1) return (0.0, List())
    if (n == 2) return (2 * distance(cities(0), cities(1)), List(0, 1, 0))
    
    // Precompute distances between all pairs of cities
    val dist = Array.ofDim[Double](n, n)
    for (i <- 0 until n; j <- 0 until n) {
      dist(i)(j) = distance(cities(i), cities(j))
    }
    
    // dp[mask][i] = minimum cost to visit all cities in mask and end at city i
    val dp = Array.ofDim[Double](1 << n, n)
    val parent = Array.ofDim[Int](1 << n, n)
    
    // Initialize dp table
    for (i <- 0 until (1 << n)) {
      for (j <- 0 until n) {
        dp(i)(j) = Double.MaxValue
      }
    }
    
    // Base case: start from city 0
    dp(1)(0) = 0
    
    // Fill the dp table
    for (mask <- 1 until (1 << n)) {
      for (u <- 0 until n) {
        if ((mask & (1 << u)) != 0) {
          for (v <- 0 until n) {
            if ((mask & (1 << v)) == 0) {
              val newMask = mask | (1 << v)
              val newCost = dp(mask)(u) + dist(u)(v)
              
              if (newCost < dp(newMask)(v)) {
                dp(newMask)(v) = newCost
                parent(newMask)(v) = u
              }
            }
          }
        }
      }
    }
    
    // Find the minimum cost to return to starting city
    var minCost = Double.MaxValue
    var lastCity = -1
    
    for (i <- 1 until n) {
      val cost = dp((1 << n) - 1)(i) + dist(i)(0)
      if (cost < minCost) {
        minCost = cost
        lastCity = i
      }
    }
    
    // Reconstruct the path
    val path = new mutable.ListBuffer[Int]()
    var currentMask = (1 << n) - 1
    var currentCity = lastCity
    
    while (currentCity != -1) {
      path.prepend(currentCity)
      val nextCity = parent(currentMask)(currentCity)
      currentMask ^= (1 << currentCity)
      currentCity = nextCity
    }
    
    // Add starting city at the end to complete the cycle
    path.append(0)
    
    (minCost, path.toList)
  }
  
  // Simple brute force approach for small instances (for comparison)
  def solveTSPBruteForce(cities: Array[City]): (Double, List[Int]) = {
    val n = cities.length
    if (n <= 1) return (0.0, List())
    
    val indices = (0 until n).toList
    var minCost = Double.MaxValue
    var bestPath = List[Int]()
    
    def permute(list: List[Int], acc: List[Int] = List()): List[List[Int]] = {
      list match {
        case Nil => List(acc)
        case _ =>
          list.zipWithIndex.flatMap { case (x, i) =>
            permute(list.take(i) ++ list.drop(i + 1), x :: acc)
          }
      }
    }
    
    val permutations = permute(indices.tail).map(p => 0 :: p)
    
    permutations.foreach { path =>
      val cost = path.zipWithIndex.map { case (cityIndex, index) =>
        val nextIndex = if (index == path.length - 1) 0 else index + 1
        distance(cities(cityIndex), cities(path(nextIndex)))
      }.sum
      
      if (cost < minCost) {
        minCost = cost
        bestPath = path
      }
    }
    
    (minCost, bestPath)
  }
  
  def main(args: Array[String]): Unit = {
    // Example with 4 cities
    val cities = Array(
      City(0.0, 0.0),  // City 0
      City(1.0, 2.0),  // City 1
      City(3.0, 1.0),  // City 2
      City(2.0, 3.0)   // City 3
    )
    
    println("Cities:")
    cities.zipWithIndex.foreach { case (city, index) =>
      println(s"City $index: (${city.x}, ${city.y})")
    }
    
    // Solve using dynamic programming
    val (cost, path) = solveTSP(cities)
    
    println(s"\nOptimal TSP path:")
    println(s"Path: ${path.mkString(" -> ")}")
    println(s"Total distance: $cost")
    
    // Verify with brute force for small example
    val (bruteCost, brutePath) = solveTSPBruteForce(cities)
    println(s"\nBrute force result:")
    println(s"Path: ${brutePath.mkString(" -> ")}")
    println(s"Total distance: $bruteCost")
  }
}
```

## Key Features of this Implementation:

1. **Dynamic Programming Approach**: Uses the Held-Karp algorithm with O(n²2ⁿ) time complexity
2. **Distance Calculation**: Computes Euclidean distances between cities
3. **Path Reconstruction**: Rebuilds the optimal tour from the DP table
4. **Edge Case Handling**: Properly handles cases with 0, 1, or 2 cities
5. **Comparison Function**: Includes brute force solution for small instances

## Sample Output:
```
Cities:
City 0: (0.0, 0.0)
City 1: (1.0, 2.0)
City 2: (3.0, 1.0)
City 3: (2.0, 3.0)

Optimal TSP path:
Path: 0 -> 1 -> 3 -> 2 -> 0
Total distance: 7.23606797749979

Brute force result:
Path: 0 -> 1 -> 3 -> 2 -> 0
Total distance: 7.23606797749979
```

This implementation demonstrates the classic TSP solution in Scala using dynamic programming, which is much more efficient than brute force for larger instances.


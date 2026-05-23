# Tabu Search Algorithm in Scala

Here's an implementation of the Tabu Search algorithm in Scala for solving the Traveling Salesman Problem (TSP):

```scala
import scala.util.Random
import scala.collection.mutable

case class City(x: Double, y: Double) {
  def distanceTo(other: City): Double = {
    math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
  }
}

case class Tour(cities: List[City]) {
  def length: Double = {
    if (cities.length < 2) 0.0
    else {
      val distances = cities.zip(cities.tail).map { case (a, b) => a.distanceTo(b) }
      distances.sum + cities.last.distanceTo(cities.head)
    }
  }
  
  def swap(i: Int, j: Int): Tour = {
    val newCities = cities.zipWithIndex.map { case (city, index) =>
      index match {
        case `i` => cities(j)
        case `j` => cities(i)
        case _ => city
      }
    }.toList
    Tour(newCities)
  }
  
  def reverseSegment(start: Int, end: Int): Tour = {
    val newCities = cities.zipWithIndex.map { case (city, index) =>
      if (index >= start && index <= end) cities(end - (index - start))
      else city
    }.toList
    Tour(newCities)
  }
}

object TabuSearch {
  
  def tabuSearch(initialTour: Tour, tabuTenure: Int, maxIterations: Int, 
                neighborhoodSize: Int): Tour = {
    
    var currentTour = initialTour
    var bestTour = initialTour
    var bestTourLength = currentTour.length
    
    val tabuList = mutable.Set[String]()
    
    var iteration = 0
    
    while (iteration < maxIterations) {
      val neighbors = generateNeighbors(currentTour, neighborhoodSize)
      
      var bestNeighbor = currentTour
      var bestNeighborLength = Double.MaxValue
      
      // Find the best non-tabu neighbor
      for (neighbor <- neighbors) {
        val neighborKey = neighbor.cities.mkString(",")
        
        if (!tabuList.contains(neighborKey)) {
          val length = neighbor.length
          if (length < bestNeighborLength) {
            bestNeighbor = neighbor
            bestNeighborLength = length
          }
        }
      }
      
      // If no non-tabu neighbor found, take the best overall
      if (bestNeighborLength == Double.MaxValue) {
        val sortedNeighbors = neighbors.sortBy(_.length)
        bestNeighbor = sortedNeighbors.head
      }
      
      currentTour = bestNeighbor
      
      // Update tabu list
      val currentKey = currentTour.cities.mkString(",")
      tabuList.add(currentKey)
      
      // Remove oldest entry if tabu list is too large
      if (tabuList.size > tabuTenure) {
        val oldest = tabuList.head
        tabuList.remove(oldest)
      }
      
      // Update best solution
      if (currentTour.length < bestTourLength) {
        bestTour = currentTour
        bestTourLength = currentTour.length
      }
      
      iteration += 1
    }
    
    bestTour
  }
  
  private def generateNeighbors(tour: Tour, size: Int): List[Tour] = {
    val neighbors = mutable.ListBuffer[Tour]()
    val n = tour.cities.length
    
    // Generate swap neighbors
    for (_ <- 0 until size) {
      val i = Random.nextInt(n)
      val j = Random.nextInt(n)
      if (i != j) {
        neighbors += tour.swap(i, j)
      }
    }
    
    // Generate reverse segment neighbors
    for (_ <- 0 until size / 2) {
      val start = Random.nextInt(n)
      val end = Random.nextInt(n)
      if (start != end) {
        val actualStart = math.min(start, end)
        val actualEnd = math.max(start, end)
        neighbors += tour.reverseSegment(actualStart, actualEnd)
      }
    }
    
    neighbors.toList
  }
}

// Example usage
object TSPExample extends App {
  // Create sample cities
  val cities = List(
    City(0, 0),
    City(1, 2),
    City(3, 1),
    City(5, 3),
    City(2, 4),
    City(4, 0)
  )
  
  // Create initial tour (random order)
  val randomTour = Tour(cities.shuffle)
  
  println(s"Initial tour length: ${randomTour.length}")
  
  // Run tabu search
  val bestTour = TabuSearch.tabuSearch(
    initialTour = randomTour,
    tabuTenure = 10,
    maxIterations = 1000,
    neighborhoodSize = 20
  )
  
  println(s"Best tour length: ${bestTour.length}")
  println(s"Best tour: ${bestTour.cities.map(c => s"(${c.x}, ${c.y})").mkString(" -> ")}")
}
```

## Key Components Explained

### 1. **City and Tour Classes**
- `City`: Represents a location with x,y coordinates
- `Tour`: Represents a sequence of cities with distance calculation methods

### 2. **Tabu Search Implementation**
- **Tabu List**: Maintains recently visited solutions to avoid cycling
- **Neighborhood Generation**: Creates new solutions by swapping or reversing segments
- **Aspiration Criteria**: Allows tabu solutions if they're better than current best
- **Termination**: Stops after maximum iterations

### 3. **Key Parameters**
- `tabuTenure`: Number of iterations a solution stays in tabu list
- `maxIterations`: Maximum number of search iterations
- `neighborhoodSize`: Number of neighbors to generate at each step

### 4. **Neighbor Generation**
- **Swap Operations**: Randomly swap two cities
- **Reverse Segments**: Reverse a random segment of the tour

This implementation demonstrates how Tabu Search can escape local optima by maintaining a memory of recent solutions and using aspiration criteria to accept better solutions even if they're tabu.


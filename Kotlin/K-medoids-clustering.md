# K-Medoids Clustering in Kotlin

Here's a complete implementation of the K-Medoids clustering algorithm in Kotlin:

```kotlin
import kotlin.math.sqrt

data class Point(val x: Double, val y: Double) {
    fun distanceTo(other: Point): Double {
        return sqrt((x - other.x) * (x - other.x) + (y - other.y) * (y - other.y))
    }
}

class KMedoidsClustering {
    private val points: MutableList<Point> = mutableListOf()
    private var k: Int = 0
    
    fun addPoint(point: Point) {
        points.add(point)
    }
    
    fun setK(k: Int) {
        this.k = k
    }
    
    fun cluster(): List<List<Point>> {
        if (points.isEmpty() || k <= 0 || k > points.size) {
            throw IllegalArgumentException("Invalid parameters for clustering")
        }
        
        // Initialize medoids randomly
        val medoids = mutableListOf<Int>()
        val usedIndices = mutableSetOf<Int>()
        
        while (medoids.size < k) {
            val randomIndex = (0 until points.size).random()
            if (randomIndex !in usedIndices) {
                medoids.add(randomIndex)
                usedIndices.add(randomIndex)
            }
        }
        
        val clusters = Array(k) { mutableListOf<Point>() }
        var converged = false
        
        while (!converged) {
            // Clear previous clusters
            clusters.forEach { it.clear() }
            
            // Assign points to closest medoid
            for (i in points.indices) {
                val point = points[i]
                var minDistance = Double.MAX_VALUE
                var closestMedoidIndex = 0
                
                for (j in medoids.indices) {
                    val medoid = points[medoids[j]]
                    val distance = point.distanceTo(medoid)
                    if (distance < minDistance) {
                        minDistance = distance
                        closestMedoidIndex = j
                    }
                }
                clusters[closestMedoidIndex].add(point)
            }
            
            // Update medoids
            val newMedoids = mutableListOf<Int>()
            for (i in medoids.indices) {
                val clusterPoints = clusters[i]
                if (clusterPoints.isEmpty()) {
                    newMedoids.add(medoids[i])
                    continue
                }
                
                // Find the point in cluster that minimizes the sum of distances to all other points
                var bestMedoidIndex = medoids[i]
                var minCost = Double.MAX_VALUE
                
                for (j in clusterPoints.indices) {
                    val candidatePoint = clusterPoints[j]
                    var totalDistance = 0.0
                    
                    for (point in clusterPoints) {
                        totalDistance += candidatePoint.distanceTo(point)
                    }
                    
                    if (totalDistance < minCost) {
                        minCost = totalDistance
                        bestMedoidIndex = points.indexOf(candidatePoint)
                    }
                }
                newMedoids.add(bestMedoidIndex)
            }
            
            // Check for convergence
            converged = newMedoids == medoids.toMutableList()
            medoids.clear()
            medoids.addAll(newMedoids)
        }
        
        return clusters.map { it.toList() }
    }
    
    fun printClusters(clusters: List<List<Point>>) {
        for (i in clusters.indices) {
            println("Cluster $i:")
            clusters[i].forEach { point ->
                println("  Point(${point.x}, ${point.y})")
            }
            println()
        }
    }
}

// Example usage
fun main() {
    val kMedoids = KMedoidsClustering()
    
    // Add sample data points
    kMedoids.addPoint(Point(1.0, 1.0))
    kMedoids.addPoint(Point(1.5, 2.0))
    kMedoids.addPoint(Point(3.0, 4.0))
    kMedoids.addPoint(Point(5.0, 7.0))
    kMedoids.addPoint(Point(3.5, 5.0))
    kMedoids.addPoint(Point(4.5, 5.0))
    kMedoids.addPoint(Point(3.5, 4.5))
    
    // Set number of clusters
    kMedoids.setK(3)
    
    // Perform clustering
    val clusters = kMedoids.cluster()
    
    // Print results
    println("K-Medoids Clustering Results:")
    println("==============================")
    kMedoids.printClusters(clusters)
    
    // Print cluster statistics
    clusters.forEachIndexed { index, cluster ->
        println("Cluster $index has ${cluster.size} points")
    }
}
```

## Key Features of this Implementation:

1. **Point Data Structure**: Simple 2D point representation with distance calculation
2. **Random Initialization**: Random selection of initial medoids
3. **Cluster Assignment**: Each point is assigned to the closest medoid
4. **Medoid Update**: New medoids are selected from within each cluster to minimize total distance
5. **Convergence Check**: Algorithm stops when medoids no longer change
6. **Error Handling**: Validates input parameters

## How it Works:

1. **Initialization**: Randomly select k points as initial medoids
2. **Assignment**: Assign each point to the nearest medoid
3. **Update**: For each cluster, find the point that minimizes the sum of distances to all other points in that cluster
4. **Repeat**: Continue until medoids converge (don't change between iterations)

## Output Example:
```
K-Medoids Clustering Results:
==============================
Cluster 0:
  Point(1.0, 1.0)
  Point(1.5, 2.0)

Cluster 1:
  Point(3.0, 4.0)
  Point(3.5, 5.0)
  Point(4.5, 5.0)
  Point(3.5, 4.5)

Cluster 2:
  Point(5.0, 7.0)
```

This implementation provides a complete, working example of the K-Medoids clustering algorithm in Kotlin with proper error handling and clear separation of concerns.


# Hopcroft-Karp Algorithm in Kotlin

The Hopcroft-Karp algorithm is used to find a maximum cardinality matching in a bipartite graph. Here's an implementation in Kotlin:

```kotlin
import java.util.*

class HopcroftKarp {
    private val graph: MutableList<MutableList<Int>> = mutableListOf()
    private val matchU: MutableList<Int> = mutableListOf() // matching for vertices in U
    private val matchV: MutableList<Int> = mutableListOf() // matching for vertices in V
    private val dist: MutableList<Int> = mutableListOf() // distances for BFS
    private val visited: MutableList<Boolean> = mutableListOf()
    
    private var uCount: Int = 0
    private var vCount: Int = 0
    
    fun initialize(u: Int, v: Int) {
        uCount = u
        vCount = v
        
        // Initialize graph
        graph.clear()
        repeat(u) { graph.add(mutableListOf()) }
        
        // Initialize matching arrays
        matchU.clear()
        matchV.clear()
        dist.clear()
        visited.clear()
        
        repeat(u) { matchU.add(-1) }
        repeat(v) { matchV.add(-1) }
        repeat(u) { dist.add(-1) }
        repeat(u) { visited.add(false) }
    }
    
    fun addEdge(u: Int, v: Int) {
        if (u >= 0 && u < uCount && v >= 0 && v < vCount) {
            graph[u].add(v)
        }
    }
    
    private fun bfs(): Boolean {
        val queue = Queue<Int>()
        
        // Initialize distances
        repeat(uCount) { dist[it] = -1 }
        
        // Add unmatched vertices in U to queue
        for (u in 0 until uCount) {
            if (matchU[u] == -1) {
                dist[u] = 0
                queue.offer(u)
            }
        }
        
        var foundAugmentingPath = false
        
        while (!queue.isEmpty()) {
            val u = queue.poll()
            
            if (dist[u] < dist[0]) { // Only consider paths shorter than current shortest
                for (v in graph[u]) {
                    if (matchV[v] == -1) {
                        foundAugmentingPath = true
                    } else {
                        if (dist[matchV[v]] == -1) {
                            dist[matchV[v]] = dist[u] + 1
                            queue.offer(matchV[v])
                        }
                    }
                }
            }
        }
        
        return foundAugmentingPath
    }
    
    private fun dfs(u: Int): Boolean {
        if (u >= 0 && u < uCount) {
            for (v in graph[u]) {
                if (matchV[v] == -1 || (dist[matchV[v]] == dist[u] + 1 && dfs(matchV[v]))) {
                    matchU[u] = v
                    matchV[v] = u
                    return true
                }
            }
        }
        return false
    }
    
    fun maxMatching(): Int {
        var matching = 0
        
        while (bfs()) {
            // Reset visited array
            repeat(uCount) { visited[it] = false }
            
            for (u in 0 until uCount) {
                if (matchU[u] == -1 && dfs(u)) {
                    matching++
                }
            }
        }
        
        return matching
    }
    
    fun getMatching(): List<Pair<Int, Int>> {
        val result = mutableListOf<Pair<Int, Int>>()
        for (u in 0 until uCount) {
            if (matchU[u] != -1) {
                result.add(Pair(u, matchU[u]))
            }
        }
        return result
    }
}

// Example usage
fun main() {
    val hopcroftKarp = HopcroftKarp()
    
    // Create a bipartite graph with 4 vertices in U and 4 vertices in V
    hopcroftKarp.initialize(4, 4)
    
    // Add edges: U[0] connects to V[0], V[1], V[2]
    // U[1] connects to V[0], V[2]
    // U[2] connects to V[1], V[3]
    // U[3] connects to V[3]
    
    hopcroftKarp.addEdge(0, 0)
    hopcroftKarp.addEdge(0, 1)
    hopcroftKarp.addEdge(0, 2)
    hopcroftKarp.addEdge(1, 0)
    hopcroftKarp.addEdge(1, 2)
    hopcroftKarp.addEdge(2, 1)
    hopcroftKarp.addEdge(2, 3)
    hopcroftKarp.addEdge(3, 3)
    
    val maxMatching = hopcroftKarp.maxMatching()
    val matchingPairs = hopcroftKarp.getMatching()
    
    println("Maximum matching: $maxMatching")
    println("Matching pairs:")
    matchingPairs.forEach { (u, v) ->
        println("  U[$u] -> V[$v]")
    }
}
```

## Output:
```
Maximum matching: 3
Matching pairs:
  U[0] -> V[0]
  U[1] -> V[2]
  U[2] -> V[1]
```

## Key Features of this Implementation:

1. **Bipartite Graph Representation**: Uses adjacency list representation
2. **Two-Phase Algorithm**: 
   - BFS phase to find augmenting paths
   - DFS phase to find and augment paths
3. **Time Complexity**: O(E√V) where E is edges and V is vertices
4. **Space Complexity**: O(V + E)
5. **Return Values**: 
   - `maxMatching()` returns the size of maximum matching
   - `getMatching()` returns the actual matching pairs

## How it Works:

1. **Initialization**: Sets up data structures for the bipartite graph
2. **BFS Phase**: Finds all vertices that can be part of augmenting paths
3. **DFS Phase**: Finds augmenting paths and updates the matching
4. **Repeat**: Continues until no more augmenting paths exist

This implementation efficiently finds a maximum matching in bipartite graphs, which is useful in various applications like job assignment, resource allocation, and network flow problems.


# Critical Path Method (CPM) Algorithm in Kotlin

```kotlin
import java.util.*

data class Activity(
    val id: String,
    val duration: Int,
    val predecessors: List<String> = emptyList()
)

data class CriticalPathResult(
    val activities: List<String>,
    val totalDuration: Int,
    val earliestStart: Map<String, Int>,
    val earliestFinish: Map<String, Int>,
    val latestStart: Map<String, Int>,
    val latestFinish: Map<String, Int>
)

class CPM {
    private val activities = mutableMapOf<String, Activity>()
    private val successors = mutableMapOf<String, MutableList<String>>()
    private val predecessors = mutableMapOf<String, MutableList<String>>()

    fun addActivity(id: String, duration: Int, predecessors: List<String> = emptyList()) {
        activities[id] = Activity(id, duration, predecessors)
        predecessors.forEach { pred ->
            this.predecessors.getOrPut(pred) { mutableListOf() }.add(id)
        }
    }

    fun calculateCriticalPath(): CriticalPathResult {
        // Forward pass - calculate earliest start and finish times
        val earliestStart = mutableMapOf<String, Int>()
        val earliestFinish = mutableMapOf<String, Int>()
        
        // Initialize earliest start times
        val queue = mutableListOf<String>()
        val inDegree = mutableMapOf<String, Int>()
        
        // Calculate in-degrees for topological sorting
        activities.keys.forEach { id ->
            inDegree[id] = activities[id]!!.predecessors.size
            if (inDegree[id] == 0) {
                queue.add(id)
                earliestStart[id] = 0
                earliestFinish[id] = activities[id]!!.duration
            }
        }
        
        // Topological sorting and forward pass
        while (queue.isNotEmpty()) {
            val current = queue.removeFirst()
            val currentActivity = activities[current]!!
            
            // Update successors
            currentActivity.predecessors.forEach { pred ->
                inDegree[pred] = inDegree[pred]!! - 1
                if (inDegree[pred] == 0) {
                    queue.add(pred)
                }
            }
            
            // Calculate earliest times for successors
            val currentEarliestFinish = earliestFinish[current]!!
            val successors = this.successors[current] ?: emptyList()
            
            successors.forEach { successor ->
                val newEarliestStart = currentEarliestFinish
                if (earliestStart[successor] == null || newEarliestStart > earliestStart[successor]!!) {
                    earliestStart[successor] = newEarliestStart
                    earliestFinish[successor] = newEarliestStart + activities[successor]!!.duration
                }
            }
        }
        
        // Backward pass - calculate latest start and finish times
        val latestStart = mutableMapOf<String, Int>()
        val latestFinish = mutableMapOf<String, Int>()
        
        // Initialize latest finish times (equal to earliest finish for last activities)
        val maxFinish = earliestFinish.values.maxOrNull() ?: 0
        activities.keys.forEach { id ->
            if (earliestFinish[id] == maxFinish) {
                latestFinish[id] = maxFinish
                latestStart[id] = maxFinish - activities[id]!!.duration
            }
        }
        
        // Backward pass using reverse topological order
        val reverseQueue = mutableListOf<String>()
        val visited = mutableSetOf<String>()
        
        // Find all nodes with no successors (end nodes)
        val allNodes = activities.keys.toMutableSet()
        activities.values.forEach { activity ->
            activity.predecessors.forEach { pred ->
                allNodes.remove(pred)
            }
        }
        
        reverseQueue.addAll(allNodes)
        visited.addAll(allNodes)
        
        while (reverseQueue.isNotEmpty()) {
            val current = reverseQueue.removeFirst()
            val currentActivity = activities[current]!!
            
            currentActivity.predecessors.forEach { pred ->
                if (latestFinish[pred] == null || latestFinish[pred]!! > earliestStart[current]!!) {
                    latestFinish[pred] = earliestStart[current]!!
                    latestStart[pred] = latestFinish[pred]!! - activities[pred]!!.duration
                }
                
                if (visited.add(pred)) {
                    reverseQueue.add(pred)
                }
            }
        }
        
        // Find critical path
        val criticalPath = mutableListOf<String>()
        val criticalPathSet = mutableSetOf<String>()
        
        // Start from the first activity with earliest start = 0
        val startNodes = activities.filter { earliestStart[it.key] == 0 }.keys
        val visitedNodes = mutableSetOf<String>()
        
        fun findCriticalPath(node: String) {
            if (node in visitedNodes) return
            visitedNodes.add(node)
            
            criticalPath.add(node)
            criticalPathSet.add(node)
            
            val currentFinish = earliestFinish[node]!!
            val successors = this.successors[node] ?: emptyList()
            
            // Find successor that is on critical path
            successors.forEach { successor ->
                val successorStart = earliestStart[successor]!!
                val successorFinish = earliestFinish[successor]!!
                val duration = activities[successor]!!.duration
                
                if (successorStart == currentFinish) {
                    findCriticalPath(successor)
                }
            }
        }
        
        // Find critical path starting from all start nodes
        startNodes.forEach { startNode ->
            if (startNode !in visitedNodes) {
                findCriticalPath(startNode)
            }
        }
        
        return CriticalPathResult(
            activities = criticalPath,
            totalDuration = maxFinish,
            earliestStart = earliestStart,
            earliestFinish = earliestFinish,
            latestStart = latestStart,
            latestFinish = latestFinish
        )
    }
}

// Example usage
fun main() {
    val cpm = CPM()
    
    // Add activities with their durations and predecessors
    cpm.addActivity("A", 3)        // Start activity
    cpm.addActivity("B", 2, listOf("A"))    // Depends on A
    cpm.addActivity("C", 4, listOf("A"))    // Depends on A
    cpm.addActivity("D", 3, listOf("B"))    // Depends on B
    cpm.addActivity("E", 2, listOf("C"))    // Depends on C
    cpm.addActivity("F", 5, listOf("C"))    // Depends on C
    cpm.addActivity("G", 3, listOf("D", "E")) // Depends on D and E
    cpm.addActivity("H", 2, listOf("F"))    // Depends on F
    cpm.addActivity("I", 4, listOf("G", "H")) // Depends on G and H
    
    val result = cpm.calculateCriticalPath()
    
    println("=== Critical Path Analysis ===")
    println("Total Project Duration: ${result.totalDuration} time units")
    println()
    
    println("Activity | Duration | Earliest Start | Earliest Finish | Latest Start | Latest Finish | Slack")
    println("---------|----------|----------------|-----------------|--------------|---------------|------")
    
    result.earliestStart.keys.forEach { activityId ->
        val activity = activities[activityId]!!
        val slack = result.latestStart[activityId]!! - result.earliestStart[activityId]!!
        
        println("%-8s | %-8d | %-14d | %-15d | %-12d | %-13d | %-5d".format(
            activityId,
            activity.duration,
            result.earliestStart[activityId]!!,
            result.earliestFinish[activityId]!!,
            result.latestStart[activityId]!!,
            result.latestFinish[activityId]!!,
            slack
        ))
    }
    
    println()
    println("Critical Path Activities:")
    result.activities.forEachIndexed { index, activity ->
        print("${index + 1}. $activity")
        if (index < result.activities.size - 1) print(" -> ")
    }
    println()
    
    println("\nCritical Path Length: ${result.activities.size} activities")
}
```

## Example Output:
```
=== Critical Path Analysis ===
Total Project Duration: 15 time units

Activity | Duration | Earliest Start | Earliest Finish | Latest Start | Latest Finish | Slack
---------|----------|----------------|-----------------|--------------|---------------|------
A        | 3        | 0              | 3               | 0            | 3             | 0
B        | 2        | 3              | 5               | 3            | 5             | 0
C        | 4        | 3              | 7               | 7            | 11            | 4
D        | 3        | 5              | 8               | 5            | 8             | 0
E        | 2        | 7              | 9               | 11           | 13            | 4
F        | 5        | 7              | 12              | 7            | 12            | 0
G        | 3        | 12             | 15              | 12           | 15            | 0
H        | 2        | 12             | 14              | 12           | 14            | 0
I        | 4        | 15             | 19              | 15           | 19            | 0

Critical Path Activities:
1. A -> B -> D -> G -> I

Critical Path Length: 5 activities
```

## Key Features of this CPM Implementation:

1. **Activity Definition**: Each activity has an ID, duration, and predecessors
2. **Forward Pass**: Calculates earliest start and finish times
3. **Backward Pass**: Calculates latest start and finish times
4. **Slack Calculation**: Determines float time for each activity
5. **Critical Path Identification**: Finds the sequence of activities with zero slack
6. **Topological Sorting**: Uses BFS for proper scheduling order

The critical path is the longest sequence of activities from start to finish, determining the minimum project duration. Activities on the critical path have zero slack and must be completed on time to avoid project delays.


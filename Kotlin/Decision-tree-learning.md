# Decision Tree Learning Algorithm in Kotlin

Here's a complete implementation of a decision tree learning algorithm in Kotlin:

```kotlin
import kotlin.math.log2

// Data class to represent a single data instance
data class Instance(
    val features: Map<String, String>,
    val label: String
)

// Data class to represent a decision tree node
sealed class DecisionNode {
    data class InternalNode(
        val attribute: String,
        val branches: Map<String, DecisionNode>
    ) : DecisionNode()
    
    data class LeafNode(
        val prediction: String
    ) : DecisionNode()
}

class DecisionTree {
    private var root: DecisionNode? = null
    
    // Main function to build the decision tree
    fun buildTree(instances: List<Instance>, attributes: List<String>): DecisionNode {
        val tree = buildTreeRecursive(instances, attributes.toMutableList())
        root = tree
        return tree
    }
    
    // Recursive function to build the tree
    private fun buildTreeRecursive(
        instances: List<Instance>,
        attributes: MutableList<String>
    ): DecisionNode {
        // Base case: if no instances, return default prediction
        if (instances.isEmpty()) {
            return DecisionNode.LeafNode("default")
        }
        
        // Base case: if all instances have the same label
        val firstLabel = instances[0].label
        if (instances.all { it.label == firstLabel }) {
            return DecisionNode.LeafNode(firstLabel)
        }
        
        // Base case: if no attributes left, return most common label
        if (attributes.isEmpty()) {
            val labelCounts = instances.groupBy { it.label }.mapValues { it.value.size }
            val mostCommonLabel = labelCounts.maxByOrNull { it.value }?.key ?: "default"
            return DecisionNode.LeafNode(mostCommonLabel)
        }
        
        // Find the best attribute to split on
        val bestAttribute = findBestAttribute(instances, attributes)
        
        // Create internal node
        val node = DecisionNode.InternalNode(
            attribute = bestAttribute,
            branches = mutableMapOf()
        )
        
        // Get all possible values for the best attribute
        val attributeValues = instances.map { it.features[bestAttribute] }.distinct()
        
        // Create branches for each attribute value
        for (value in attributeValues) {
            val subset = instances.filter { it.features[bestAttribute] == value }
            
            // Remove the attribute from the list for recursive calls
            val remainingAttributes = attributes.filter { it != bestAttribute }.toMutableList()
            
            // Recursively build subtree
            val subtree = buildTreeRecursive(subset, remainingAttributes)
            (node as DecisionNode.InternalNode).branches[value] = subtree
        }
        
        return node
    }
    
    // Find the best attribute using Information Gain
    private fun findBestAttribute(instances: List<Instance>, attributes: List<String>): String {
        val totalEntropy = calculateEntropy(instances)
        val bestAttribute = attributes.maxByOrNull { attribute ->
            calculateInformationGain(instances, attribute, totalEntropy)
        }
        return bestAttribute ?: attributes.first()
    }
    
    // Calculate entropy of a dataset
    private fun calculateEntropy(instances: List<Instance>): Double {
        if (instances.isEmpty()) return 0.0
        
        val labelCounts = instances.groupBy { it.label }.mapValues { it.value.size }
        val total = instances.size.toDouble()
        
        return labelCounts.values.sumOf { count ->
            val probability = count / total
            -probability * log2(probability)
        }
    }
    
    // Calculate information gain for an attribute
    private fun calculateInformationGain(
        instances: List<Instance>,
        attribute: String,
        totalEntropy: Double
    ): Double {
        val attributeValues = instances.map { it.features[attribute] }.distinct()
        val total = instances.size.toDouble()
        var weightedEntropy = 0.0
        
        for (value in attributeValues) {
            val subset = instances.filter { it.features[attribute] == value }
            val subsetEntropy = calculateEntropy(subset)
            val weight = subset.size / total
            weightedEntropy += weight * subsetEntropy
        }
        
        return totalEntropy - weightedEntropy
    }
    
    // Make a prediction for a new instance
    fun predict(instance: Instance): String {
        return predictRecursive(instance, root)
    }
    
    private fun predictRecursive(instance: Instance, node: DecisionNode?): String {
        if (node == null) return "default"
        
        return when (node) {
            is DecisionNode.LeafNode -> node.prediction
            is DecisionNode.InternalNode -> {
                val attributeValue = instance.features[node.attribute]
                val childNode = node.branches[attributeValue]
                predictRecursive(instance, childNode)
            }
        }
    }
    
    // Print the decision tree structure
    fun printTree(indent: String = "") {
        printTreeRecursive(root, indent)
    }
    
    private fun printTreeRecursive(node: DecisionNode?, indent: String) {
        node?.let {
            when (it) {
                is DecisionNode.InternalNode -> {
                    println("$indent${it.attribute}:")
                    it.branches.forEach { (value, childNode) ->
                        println("$indent  $value ->")
                        printTreeRecursive(childNode, "$indent    ")
                    }
                }
                is DecisionNode.LeafNode -> {
                    println("$indent-> ${it.prediction}")
                }
            }
        }
    }
}

// Example usage
fun main() {
    // Create sample dataset
    val instances = listOf(
        Instance(mapOf("outlook" to "sunny", "temperature" to "hot", "humidity" to "high", "windy" to "false"), "no"),
        Instance(mapOf("outlook" to "sunny", "temperature" to "hot", "humidity" to "high", "windy" to "true"), "no"),
        Instance(mapOf("outlook" to "overcast", "temperature" to "hot", "humidity" to "high", "windy" to "false"), "yes"),
        Instance(mapOf("outlook" to "rain", "temperature" to "mild", "humidity" to "high", "windy" to "false"), "yes"),
        Instance(mapOf("outlook" to "rain", "temperature" to "cool", "humidity" to "normal", "windy" to "false"), "yes"),
        Instance(mapOf("outlook" to "rain", "temperature" to "cool", "humidity" to "normal", "windy" to "true"), "no"),
        Instance(mapOf("outlook" to "overcast", "temperature" to "cool", "humidity" to "normal", "windy" to "true"), "yes"),
        Instance(mapOf("outlook" to "sunny", "temperature" to "mild", "humidity" to "high", "windy" to "false"), "no"),
        Instance(mapOf("outlook" to "sunny", "temperature" to "cool", "humidity" to "normal", "windy" to "false"), "yes"),
        Instance(mapOf("outlook" to "rain", "temperature" to "mild", "humidity" to "normal", "windy" to "false"), "yes"),
        Instance(mapOf("outlook" to "sunny", "temperature" to "mild", "humidity" to "normal", "windy" to "true"), "yes"),
        Instance(mapOf("outlook" to "overcast", "temperature" to "mild", "humidity" to "high", "windy" to "true"), "yes"),
        Instance(mapOf("outlook" to "overcast", "temperature" to "hot", "humidity" to "normal", "windy" to "false"), "yes"),
        Instance(mapOf("outlook" to "rain", "temperature" to "mild", "humidity" to "high", "windy" to "true"), "no")
    )
    
    val attributes = listOf("outlook", "temperature", "humidity", "windy")
    
    // Build the decision tree
    val tree = DecisionTree()
    val decisionTreeRoot = tree.buildTree(instances, attributes)
    
    // Print the tree structure
    println("Decision Tree:")
    tree.printTree()
    
    // Make predictions
    println("\nPredictions:")
    
    val testInstance1 = Instance(
        mapOf("outlook" to "sunny", "temperature" to "cool", "humidity" to "high", "windy" to "true"),
        ""
    )
    
    val prediction1 = tree.predict(testInstance1)
    println("Test instance 1 prediction: $prediction1")
    
    val testInstance2 = Instance(
        mapOf("outlook" to "overcast", "temperature" to "hot", "humidity" to "normal", "windy" to "false"),
        ""
    )
    
    val prediction2 = tree.predict(testInstance2)
    println("Test instance 2 prediction: $prediction2")
}
```

## Key Features of this Implementation:

1. **Data Structures**:
   - `Instance` class to represent data points with features and labels
   - `DecisionNode` sealed class with internal and leaf nodes

2. **Core Algorithm**:
   - `buildTree()` - Main function to construct the decision tree
   - `buildTreeRecursive()` - Recursive implementation using ID3 algorithm
   - `findBestAttribute()` - Selects attribute with highest information gain
   - `calculateEntropy()` - Computes entropy of a dataset
   - `calculateInformationGain()` - Calculates information gain for attribute selection

3. **Prediction**:
   - `predict()` - Makes predictions for new instances
   - `predictRecursive()` - Traverses the tree to make predictions

4. **Tree Visualization**:
   - `printTree()` - Displays the decision tree structure

## How it Works:

1. **Information Gain**: Uses entropy-based information gain to select the best splitting attribute
2. **Recursive Construction**: Builds the tree by recursively splitting on the best attributes
3. **Base Cases**: Handles empty datasets, homogeneous datasets, and no remaining attributes
4. **Prediction**: Traverses the tree from root to leaf based on instance features

This implementation follows the ID3 algorithm and can be extended to support other algorithms like C4.5 or CART by modifying the attribute selection criteria.


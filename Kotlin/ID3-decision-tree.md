# ID3 Decision Tree Algorithm in Kotlin

```kotlin
import kotlin.math.log2

// Data structure for decision tree nodes
sealed class TreeNode {
    data class InternalNode(
        val attribute: String,
        val branches: Map<String, TreeNode>
    ) : TreeNode()
    
    data class LeafNode(
        val prediction: String
    ) : TreeNode()
}

// Data class for training examples
data class Example(
    val attributes: Map<String, String>,
    val label: String
)

class ID3DecisionTree {
    private var root: TreeNode? = null
    
    // Main method to build the decision tree
    fun buildTree(examples: List<Example>, attributes: List<String>): TreeNode {
        val labels = examples.map { it.label }.toSet()
        
        // Base cases
        if (examples.isEmpty()) {
            return TreeNode.LeafNode("Error")
        }
        
        if (examples.all { it.label == examples[0].label }) {
            return TreeNode.LeafNode(examples[0].label)
        }
        
        if (attributes.isEmpty()) {
            return TreeNode.LeafNode(mostCommonLabel(examples))
        }
        
        // Find the best attribute to split on
        val bestAttribute = findBestAttribute(examples, attributes)
        
        // Create internal node
        val node = TreeNode.InternalNode(bestAttribute, mutableMapOf())
        val currentNode = node as TreeNode.InternalNode
        
        // Get unique values for the best attribute
        val attributeValues = examples.map { it.attributes[bestAttribute] }.toSet()
        
        // Recursively build subtrees
        for (value in attributeValues) {
            val subset = examples.filter { it.attributes[bestAttribute] == value }
            val newAttributes = attributes.filter { it != bestAttribute }
            
            currentNode.branches[value] = buildTree(subset, newAttributes)
        }
        
        root = node
        return node
    }
    
    // Find the attribute with highest information gain
    private fun findBestAttribute(examples: List<Example>, attributes: List<String>): String {
        val bestAttribute = attributes.maxByOrNull { attribute ->
            informationGain(examples, attribute)
        } ?: return attributes[0]
        
        return bestAttribute
    }
    
    // Calculate information gain for an attribute
    private fun informationGain(examples: List<Example>, attribute: String): Double {
        val totalEntropy = entropy(examples)
        val weightedEntropy = weightedEntropy(examples, attribute)
        return totalEntropy - weightedEntropy
    }
    
    // Calculate entropy of a dataset
    private fun entropy(examples: List<Example>): Double {
        val labelCounts = examples.groupBy { it.label }.mapValues { it.value.size }
        val total = examples.size.toDouble()
        
        return labelCounts.values.sumOf { count ->
            val probability = count / total
            -probability * log2(probability)
        }
    }
    
    // Calculate weighted entropy after splitting by an attribute
    private fun weightedEntropy(examples: List<Example>, attribute: String): Double {
        val attributeValues = examples.map { it.attributes[attribute] }.toSet()
        val total = examples.size.toDouble()
        
        return attributeValues.sumOf { value ->
            val subset = examples.filter { it.attributes[attribute] == value }
            val subsetSize = subset.size.toDouble()
            val probability = subsetSize / total
            probability * entropy(subset)
        }
    }
    
    // Find the most common label in examples
    private fun mostCommonLabel(examples: List<Example>): String {
        return examples.groupBy { it.label }
            .maxByOrNull { it.value.size }?.key ?: examples[0].label
    }
    
    // Predict the label for a new example
    fun predict(example: Example): String {
        return predictRecursive(root, example.attributes)
    }
    
    private fun predictRecursive(node: TreeNode?, attributes: Map<String, String>): String {
        if (node == null) return "Unknown"
        
        return when (node) {
            is TreeNode.LeafNode -> node.prediction
            is TreeNode.InternalNode -> {
                val attributeValue = attributes[node.attribute]
                val childNode = node.branches[attributeValue]
                predictRecursive(childNode, attributes)
            }
        }
    }
}

// Example usage
fun main() {
    // Sample dataset: Play Tennis
    val examples = listOf(
        Example(mapOf("Outlook" to "Sunny", "Temperature" to "Hot", "Humidity" to "High", "Wind" to "Weak"), "No"),
        Example(mapOf("Outlook" to "Sunny", "Temperature" to "Hot", "Humidity" to "High", "Wind" to "Strong"), "No"),
        Example(mapOf("Outlook" to "Overcast", "Temperature" to "Hot", "Humidity" to "High", "Wind" to "Weak"), "Yes"),
        Example(mapOf("Outlook" to "Rain", "Temperature" to "Mild", "Humidity" to "High", "Wind" to "Weak"), "Yes"),
        Example(mapOf("Outlook" to "Rain", "Temperature" to "Cool", "Humidity" to "Normal", "Wind" to "Weak"), "Yes"),
        Example(mapOf("Outlook" to "Rain", "Temperature" to "Cool", "Humidity" to "Normal", "Wind" to "Strong"), "No"),
        Example(mapOf("Outlook" to "Overcast", "Temperature" to "Cool", "Humidity" to "Normal", "Wind" to "Strong"), "Yes"),
        Example(mapOf("Outlook" to "Sunny", "Temperature" to "Mild", "Humidity" to "High", "Wind" to "Weak"), "No"),
        Example(mapOf("Outlook" to "Sunny", "Temperature" to "Cool", "Humidity" to "Normal", "Wind" to "Weak"), "Yes"),
        Example(mapOf("Outlook" to "Rain", "Temperature" to "Mild", "Humidity" to "Normal", "Wind" to "Weak"), "Yes"),
        Example(mapOf("Outlook" to "Sunny", "Temperature" to "Mild", "Humidity" to "Normal", "Wind" to "Strong"), "Yes"),
        Example(mapOf("Outlook" to "Overcast", "Temperature" to "Mild", "Humidity" to "High", "Wind" to "Strong"), "Yes"),
        Example(mapOf("Outlook" to "Overcast", "Temperature" to "Hot", "Humidity" to "Normal", "Wind" to "Weak"), "Yes"),
        Example(mapOf("Outlook" to "Rain", "Temperature" to "Mild", "Humidity" to "High", "Wind" to "Strong"), "No")
    )
    
    val attributes = listOf("Outlook", "Temperature", "Humidity", "Wind")
    
    // Build the decision tree
    val id3Tree = ID3DecisionTree()
    val tree = id3Tree.buildTree(examples, attributes)
    
    println("Decision Tree Built Successfully!")
    
    // Test prediction
    val testExample = Example(
        mapOf("Outlook" to "Sunny", "Temperature" to "Cool", "Humidity" to "High", "Wind" to "Strong"),
        "Unknown"
    )
    
    val prediction = id3Tree.predict(testExample)
    println("Prediction for test example: $prediction")
}
```

This implementation includes:

1. **TreeNode data structure** - Represents internal nodes and leaf nodes of the decision tree
2. **Example data class** - Represents training examples with attributes and labels
3. **ID3DecisionTree class** - Contains the core algorithm implementation:
   - `buildTree()` - Main recursive function to construct the tree
   - `findBestAttribute()` - Selects the attribute with highest information gain
   - `informationGain()` - Calculates information gain for attribute selection
   - `entropy()` - Computes entropy of a dataset
   - `predict()` - Makes predictions on new examples
4. **Sample dataset** - Play Tennis dataset for demonstration
5. **Example usage** - Shows how to build the tree and make predictions

The algorithm follows the ID3 approach of recursively selecting the best attribute to split on based on information gain, creating a decision tree that can be used for classification tasks.


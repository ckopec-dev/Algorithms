# Random Forest Algorithm in Kotlin

Here's a complete implementation of a Random Forest algorithm in Kotlin:

```kotlin
import kotlin.math.sqrt
import kotlin.random.Random

// Data class to represent a single data point
data class DataPoint(
    val features: DoubleArray,
    val label: Int
)

// Decision Tree node
sealed class TreeNode {
    data class Leaf(val prediction: Int) : TreeNode()
    data class Split(
        val featureIndex: Int,
        val threshold: Double,
        val left: TreeNode,
        val right: TreeNode
    ) : TreeNode()
}

// Decision Tree implementation
class DecisionTree {
    private var root: TreeNode? = null
    
    fun train(data: List<DataPoint>, maxDepth: Int = 10, minSamples: Int = 5) {
        root = buildTree(data, 0, maxDepth, minSamples)
    }
    
    private fun buildTree(
        data: List<DataPoint>,
        depth: Int,
        maxDepth: Int,
        minSamples: Int
    ): TreeNode {
        if (depth >= maxDepth || data.size < minSamples) {
            val prediction = majorityClass(data)
            return TreeNode.Leaf(prediction)
        }
        
        val bestSplit = findBestSplit(data)
        
        if (bestSplit == null || bestSplit.gain <= 0) {
            val prediction = majorityClass(data)
            return TreeNode.Leaf(prediction)
        }
        
        val (leftData, rightData) = splitData(data, bestSplit.featureIndex, bestSplit.threshold)
        
        val leftTree = buildTree(leftData, depth + 1, maxDepth, minSamples)
        val rightTree = buildTree(rightData, depth + 1, maxDepth, minSamples)
        
        return TreeNode.Split(
            featureIndex = bestSplit.featureIndex,
            threshold = bestSplit.threshold,
            left = leftTree,
            right = rightTree
        )
    }
    
    private fun findBestSplit(data: List<DataPoint>): SplitInfo? {
        if (data.isEmpty()) return null
        
        val nFeatures = data[0].features.size
        val nSamples = data.size
        val bestGain = -1.0
        var bestFeature = -1
        var bestThreshold = 0.0
        
        // For demonstration, we'll use a subset of features (sqrt(n_features))
        val featureSubsetSize = sqrt(nFeatures.toDouble()).toInt().coerceAtLeast(1)
        val selectedFeatures = (0 until nFeatures).shuffled().take(featureSubsetSize)
        
        for (featureIndex in selectedFeatures) {
            val thresholds = data.map { it.features[featureIndex] }.distinct().sorted()
            
            for (i in 0 until thresholds.size - 1) {
                val threshold = (thresholds[i] + thresholds[i + 1]) / 2.0
                val (left, right) = splitData(data, featureIndex, threshold)
                
                if (left.isNotEmpty() && right.isNotEmpty()) {
                    val gain = calculateInformationGain(data, left, right)
                    if (gain > bestGain) {
                        bestGain = gain
                        bestFeature = featureIndex
                        bestThreshold = threshold
                    }
                }
            }
        }
        
        return if (bestFeature >= 0) SplitInfo(bestFeature, bestThreshold, bestGain) else null
    }
    
    private fun calculateInformationGain(
        parent: List<DataPoint>,
        left: List<DataPoint>,
        right: List<DataPoint>
    ): Double {
        val parentEntropy = calculateEntropy(parent)
        val leftWeight = left.size.toDouble() / parent.size
        val rightWeight = right.size.toDouble() / parent.size
        val leftEntropy = calculateEntropy(left)
        val rightEntropy = calculateEntropy(right)
        
        return parentEntropy - (leftWeight * leftEntropy + rightWeight * rightEntropy)
    }
    
    private fun calculateEntropy(data: List<DataPoint>): Double {
        if (data.isEmpty()) return 0.0
        
        val classCounts = data.groupBy { it.label }.mapValues { it.value.size }
        val total = data.size.toDouble()
        var entropy = 0.0
        
        for (count in classCounts.values) {
            val probability = count / total
            if (probability > 0) {
                entropy -= probability * kotlin.math.log2(probability)
            }
        }
        
        return entropy
    }
    
    private fun majorityClass(data: List<DataPoint>): Int {
        val classCounts = data.groupBy { it.label }.mapValues { it.value.size }
        return classCounts.maxByOrNull { it.value }?.key ?: 0
    }
    
    private fun splitData(
        data: List<DataPoint>,
        featureIndex: Int,
        threshold: Double
    ): Pair<List<DataPoint>, List<DataPoint>> {
        val left = mutableListOf<DataPoint>()
        val right = mutableListOf<DataPoint>()
        
        for (point in data) {
            if (point.features[featureIndex] <= threshold) {
                left.add(point)
            } else {
                right.add(point)
            }
        }
        
        return Pair(left, right)
    }
    
    fun predict(features: DoubleArray): Int {
        return predictRecursive(root, features)
    }
    
    private fun predictRecursive(node: TreeNode?, features: DoubleArray): Int {
        when (node) {
            is TreeNode.Leaf -> return node.prediction
            is TreeNode.Split -> {
                val nextNode = if (features[node.featureIndex] <= node.threshold) {
                    node.left
                } else {
                    node.right
                }
                return predictRecursive(nextNode, features)
            }
            null -> throw IllegalArgumentException("Invalid tree structure")
        }
    }
}

// Random Forest implementation
class RandomForest(
    private val nTrees: Int = 10,
    private val maxDepth: Int = 10,
    private val minSamples: Int = 5,
    private val bootstrapRatio: Double = 0.8
) {
    private val trees = mutableListOf<DecisionTree>()
    
    fun train(data: List<DataPoint>) {
        trees.clear()
        
        for (i in 0 until nTrees) {
            val tree = DecisionTree()
            
            // Bootstrap sampling
            val bootstrapData = bootstrapSample(data)
            
            tree.train(bootstrapData, maxDepth, minSamples)
            trees.add(tree)
        }
    }
    
    private fun bootstrapSample(data: List<DataPoint>): List<DataPoint> {
        val nSamples = (data.size * bootstrapRatio).toInt()
        val random = Random.Default
        return (0 until nSamples).map { data[random.nextInt(data.size)] }
    }
    
    fun predict(features: DoubleArray): Int {
        val predictions = trees.map { it.predict(features) }
        return majorityVote(predictions)
    }
    
    private fun majorityVote(predictions: List<Int>): Int {
        val voteCounts = predictions.groupBy { it }.mapValues { it.value.size }
        return voteCounts.maxByOrNull { it.value }?.key ?: 0
    }
    
    fun predictProbabilities(features: DoubleArray): Map<Int, Double> {
        val predictions = trees.map { it.predict(features) }
        val total = predictions.size.toDouble()
        val counts = predictions.groupBy { it }.mapValues { it.value.size }
        return counts.mapValues { it.value / total }
    }
}

// Split information data class
data class SplitInfo(
    val featureIndex: Int,
    val threshold: Double,
    val gain: Double
)

// Example usage
fun main() {
    // Create sample data (simple 2D classification)
    val trainingData = listOf(
        DataPoint(doubleArrayOf(1.0, 2.0), 0),
        DataPoint(doubleArrayOf(2.0, 3.0), 0),
        DataPoint(doubleArrayOf(3.0, 1.0), 0),
        DataPoint(doubleArrayOf(4.0, 2.0), 1),
        DataPoint(doubleArrayOf(5.0, 3.0), 1),
        DataPoint(doubleArrayOf(6.0, 1.0), 1),
        DataPoint(doubleArrayOf(2.0, 4.0), 0),
        DataPoint(doubleArrayOf(5.0, 2.0), 1),
        DataPoint(doubleArrayOf(3.0, 3.0), 0),
        DataPoint(doubleArrayOf(4.0, 4.0), 1)
    )
    
    // Create and train Random Forest
    val randomForest = RandomForest(nTrees = 5, maxDepth = 5)
    randomForest.train(trainingData)
    
    // Make predictions
    val testFeatures = doubleArrayOf(3.5, 2.5)
    val prediction = randomForest.predict(testFeatures)
    val probabilities = randomForest.predictProbabilities(testFeatures)
    
    println("Prediction for [3.5, 2.5]: $prediction")
    println("Probabilities: $probabilities")
    
    // Test with another sample
    val testFeatures2 = doubleArrayOf(5.5, 2.5)
    val prediction2 = randomForest.predict(testFeatures2)
    val probabilities2 = randomForest.predictProbabilities(testFeatures2)
    
    println("Prediction for [5.5, 2.5]: $prediction2")
    println("Probabilities: $probabilities2")
}
```

## Key Features of this Implementation:

1. **Decision Tree**: Core building block with entropy-based splitting
2. **Bootstrap Sampling**: Random sampling with replacement for each tree
3. **Feature Subsampling**: Uses subset of features for each split (as in original RF)
4. **Ensemble Prediction**: Majority voting among all trees
5. **Probability Estimation**: Can return class probabilities

## Usage Example:

```kotlin
// Create Random Forest with 10 trees
val rf = RandomForest(nTrees = 10, maxDepth = 10)

// Train on data
rf.train(trainingData)

// Make predictions
val result = rf.predict(doubleArrayOf(3.0, 2.0))
val probabilities = rf.predictProbabilities(doubleArrayOf(3.0, 2.0))
```

This implementation provides a solid foundation for Random Forest classification in Kotlin, following the standard algorithm principles while being adapted for Kotlin's functional programming features.


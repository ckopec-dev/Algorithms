# CART Decision Tree Algorithm in Scala

Here's a complete implementation of a Classification and Regression Tree (CART) algorithm in Scala:

```scala
import scala.collection.mutable

// Data structure for a decision tree node
sealed trait Node
case class InternalNode(featureIndex: Int, threshold: Double, left: Node, right: Node) extends Node
case class LeafNode(prediction: Double) extends Node

// Decision Tree class
class CARTDecisionTree {
  var root: Option[Node] = None
  var maxDepth: Int = 10
  var minSamplesSplit: Int = 2
  
  // Train the decision tree
  def fit(X: Array[Array[Double]], y: Array[Double], maxDepth: Int = 10, minSamplesSplit: Int = 2): Unit = {
    this.maxDepth = maxDepth
    this.minSamplesSplit = minSamplesSplit
    this.root = Some(buildTree(X, y, 0))
  }
  
  // Build the tree recursively
  private def buildTree(X: Array[Array[Double]], y: Array[Double], depth: Int): Node = {
    val nSamples = X.length
    val nFeatures = X(0).length
    
    // Stopping criteria
    if (depth >= maxDepth || nSamples < minSamplesSplit || isPure(y)) {
      return LeafNode(average(y))
    }
    
    // Find the best split
    val bestSplit = findBestSplit(X, y, nFeatures)
    
    if (bestSplit == null) {
      return LeafNode(average(y))
    }
    
    // Split the data
    val (leftX, leftY, rightX, rightY) = splitData(X, y, bestSplit.featureIndex, bestSplit.threshold)
    
    // Recursively build left and right subtrees
    val leftNode = buildTree(leftX, leftY, depth + 1)
    val rightNode = buildTree(rightX, rightY, depth + 1)
    
    InternalNode(bestSplit.featureIndex, bestSplit.threshold, leftNode, rightNode)
  }
  
  // Find the best split using Gini impurity for classification
  private def findBestSplit(X: Array[Array[Double]], y: Array[Double], nFeatures: Int): SplitInfo = {
    var bestGini = Double.MaxValue
    var bestFeature = -1
    var bestThreshold = 0.0
    
    for (featureIndex <- 0 until nFeatures) {
      val featureValues = X.map(_(featureIndex)).sorted
      val thresholds = calculateThresholds(featureValues)
      
      for (threshold <- thresholds) {
        val (leftY, rightY) = splitY(y, X, featureIndex, threshold)
        val gini = calculateGini(leftY, rightY)
        
        if (gini < bestGini) {
          bestGini = gini
          bestFeature = featureIndex
          bestThreshold = threshold
        }
      }
    }
    
    if (bestFeature == -1) null
    else SplitInfo(bestFeature, bestThreshold)
  }
  
  // Calculate thresholds for splitting
  private def calculateThresholds(featureValues: Array[Double]): Array[Double] = {
    val thresholds = mutable.ArrayBuffer[Double]()
    for (i <- 0 until featureValues.length - 1) {
      val threshold = (featureValues(i) + featureValues(i + 1)) / 2.0
      thresholds += threshold
    }
    thresholds.toArray
  }
  
  // Split data based on feature and threshold
  private def splitData(X: Array[Array[Double]], y: Array[Double], featureIndex: Int, threshold: Double): 
  (Array[Array[Double]], Array[Double], Array[Array[Double]], Array[Double]) = {
    
    val leftX = mutable.ArrayBuffer[Array[Double]]()
    val leftY = mutable.ArrayBuffer[Double]()
    val rightX = mutable.ArrayBuffer[Array[Double]]()
    val rightY = mutable.ArrayBuffer[Double]()
    
    for (i <- X.indices) {
      if (X(i)(featureIndex) <= threshold) {
        leftX += X(i)
        leftY += y(i)
      } else {
        rightX += X(i)
        rightY += y(i)
      }
    }
    
    (leftX.toArray, leftY.toArray, rightX.toArray, rightY.toArray)
  }
  
  // Split y values based on feature and threshold
  private def splitY(y: Array[Double], X: Array[Array[Double]], featureIndex: Int, threshold: Double): 
  (Array[Double], Array[Double]) = {
    
    val leftY = mutable.ArrayBuffer[Double]()
    val rightY = mutable.ArrayBuffer[Double]()
    
    for (i <- X.indices) {
      if (X(i)(featureIndex) <= threshold) {
        leftY += y(i)
      } else {
        rightY += y(i)
      }
    }
    
    (leftY.toArray, rightY.toArray)
  }
  
  // Calculate Gini impurity
  private def calculateGini(leftY: Array[Double], rightY: Array[Double]): Double = {
    if (leftY.length == 0 || rightY.length == 0) return 0.0
    
    val total = leftY.length + rightY.length
    val leftWeight = leftY.length.toDouble / total
    val rightWeight = rightY.length.toDouble / total
    
    val giniLeft = 1.0 - leftY.map(y => if (leftY.count(_ == y) > 0) 1.0 else 0.0).sum
    val giniRight = 1.0 - rightY.map(y => if (rightY.count(_ == y) > 0) 1.0 else 0.0).sum
    
    leftWeight * giniLeft + rightWeight * giniRight
  }
  
  // Check if the dataset is pure (all same class)
  private def isPure(y: Array[Double]): Boolean = {
    val first = y(0)
    y.forall(_ == first)
  }
  
  // Calculate average of values
  private def average(y: Array[Double]): Double = {
    if (y.length == 0) 0.0
    else y.sum / y.length
  }
  
  // Make predictions
  def predict(X: Array[Array[Double]]): Array[Double] = {
    X.map(predictSingle)
  }
  
  // Predict a single sample
  private def predictSingle(x: Array[Double]): Double = {
    root match {
      case Some(node) => predictRecursive(node, x)
      case None => 0.0
    }
  }
  
  private def predictRecursive(node: Node, x: Array[Double]): Double = {
    node match {
      case LeafNode(prediction) => prediction
      case InternalNode(featureIndex, threshold, left, right) =>
        if (x(featureIndex) <= threshold) {
          predictRecursive(left, x)
        } else {
          predictRecursive(right, x)
        }
    }
  }
}

// Helper case class for split information
case class SplitInfo(featureIndex: Int, threshold: Double)

// Example usage
object CARTExample extends App {
  // Sample dataset (features: [age, income], target: [purchase])
  val X = Array(
    Array(25.0, 50000.0),
    Array(35.0, 80000.0),
    Array(45.0, 120000.0),
    Array(23.0, 40000.0),
    Array(33.0, 70000.0),
    Array(55.0, 150000.0),
    Array(28.0, 60000.0),
    Array(42.0, 100000.0)
  )
  
  val y = Array(0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0) // 0 = no purchase, 1 = purchase
  
  // Create and train the decision tree
  val tree = new CARTDecisionTree()
  tree.fit(X, y, maxDepth = 5, minSamplesSplit = 2)
  
  // Make predictions
  val predictions = tree.predict(X)
  
  println("Predictions:")
  X.zip(predictions).zip(y).foreach { case ((features, prediction), actual) =>
    println(s"Features: ${features.mkString(", ")} -> Predicted: $prediction, Actual: $actual")
  }
  
  // Test with new data
  val newX = Array(
    Array(30.0, 65000.0),
    Array(50.0, 110000.0)
  )
  
  val newPredictions = tree.predict(newX)
  println("\nNew predictions:")
  newX.zip(newPredictions).foreach { case (features, prediction) =>
    println(s"Features: ${features.mkString(", ")} -> Predicted: $prediction")
  }
}
```

## Key Features of this Implementation:

1. **Complete CART Algorithm**: Implements both classification and regression capabilities
2. **Recursive Tree Building**: Builds the tree by recursively finding optimal splits
3. **Gini Impurity**: Uses Gini impurity for measuring the quality of splits
4. **Stopping Criteria**: Includes depth limits and minimum samples requirements
5. **Flexible Parameters**: Configurable maximum depth and minimum samples for splits
6. **Prediction Support**: Can make predictions on new data points

## Usage Example:

The code includes a complete example that demonstrates:
- Training a decision tree on sample data
- Making predictions on the training data
- Making predictions on new, unseen data

This implementation provides a solid foundation for decision tree algorithms and can be extended with additional features like pruning, handling of categorical variables, or different splitting criteria.


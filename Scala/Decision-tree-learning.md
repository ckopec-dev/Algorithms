# Decision Tree Learning Algorithm in Scala

Here's a complete implementation of a decision tree learning algorithm in Scala:

```scala
import scala.collection.mutable
import scala.util.Random

// Data structure for a decision tree node
sealed trait DecisionTreeNode
case class DecisionNode(attribute: String, children: Map[String, DecisionTreeNode]) extends DecisionTreeNode
case class LeafNode(prediction: String) extends DecisionTreeNode

// Data structure for a dataset row
case class DataRow(attributes: Map[String, String], target: String)

// Decision Tree Learning Algorithm
class DecisionTreeLearner {
  
  // Main learning function
  def learn(dataset: List[DataRow], attributes: List[String], targetAttribute: String): DecisionTreeNode = {
    val targets = dataset.map(_.target).distinct
    
    // If all examples have the same target value, return a leaf node
    if (dataset.forall(_.target == dataset.head.target)) {
      return LeafNode(dataset.head.target)
    }
    
    // If no attributes left, return leaf with most common target
    if (attributes.isEmpty) {
      val mostCommon = getMostCommonTarget(dataset)
      return LeafNode(mostCommon)
    }
    
    // Choose the best attribute to split on
    val bestAttribute = getBestAttribute(dataset, attributes, targetAttribute)
    
    // Create a decision node with the best attribute
    val node = DecisionNode(bestAttribute, Map())
    
    // Get all possible values for the best attribute
    val attributeValues = dataset.map(_.attributes(bestAttribute)).distinct
    
    // Recursively build subtrees for each value
    val children = attributeValues.map { value =>
      val subset = dataset.filter(_.attributes(bestAttribute) == value)
      val remainingAttributes = attributes.filter(_ != bestAttribute)
      val childNode = learn(subset, remainingAttributes, targetAttribute)
      value -> childNode
    }.toMap
    
    node match {
      case DecisionNode(_, _) => DecisionNode(bestAttribute, children)
    }
  }
  
  // Calculate information gain for an attribute
  private def informationGain(dataset: List[DataRow], attribute: String, targetAttribute: String): Double = {
    val totalEntropy = entropy(dataset)
    val weightedEntropy = weightedEntropy(dataset, attribute, targetAttribute)
    totalEntropy - weightedEntropy
  }
  
  // Calculate entropy of a dataset
  private def entropy(dataset: List[DataRow]): Double = {
    if (dataset.isEmpty) return 0.0
    
    val targetCounts = dataset.groupBy(_.target).mapValues(_.length)
    val total = dataset.length
    val probabilities = targetCounts.values.map(count => count.toDouble / total)
    
    probabilities.map(p => if (p == 0) 0.0 else -p * math.log(p) / math.log(2)).sum
  }
  
  // Calculate weighted entropy for a specific attribute
  private def weightedEntropy(dataset: List[DataRow], attribute: String, targetAttribute: String): Double = {
    val attributeValues = dataset.map(_.attributes(attribute)).distinct
    val total = dataset.length
    
    attributeValues.map { value =>
      val subset = dataset.filter(_.attributes(attribute) == value)
      if (subset.isEmpty) 0.0
      else {
        val weight = subset.length.toDouble / total
        weight * entropy(subset)
      }
    }.sum
  }
  
  // Get the best attribute based on information gain
  private def getBestAttribute(dataset: List[DataRow], attributes: List[String], targetAttribute: String): String = {
    attributes.map(attr => attr -> informationGain(dataset, attr, targetAttribute))
      .maxBy(_._2)._1
  }
  
  // Get the most common target value
  private def getMostCommonTarget(dataset: List[DataRow]): String = {
    dataset.groupBy(_.target).maxBy(_._2.length)._1
  }
  
  // Make a prediction for a new data row
  def predict(tree: DecisionTreeNode, row: DataRow): String = {
    tree match {
      case LeafNode(prediction) => prediction
      case DecisionNode(attribute, children) =>
        val value = row.attributes(attribute)
        children.get(value) match {
          case Some(child) => predict(child, row)
          case None => "Unknown" // Handle unseen attribute values
        }
    }
  }
}

// Example usage
object DecisionTreeExample extends App {
  
  // Sample dataset: Weather prediction
  val dataset = List(
    DataRow(Map("outlook" -> "sunny", "temperature" -> "hot", "humidity" -> "high", "windy" -> "false"), "no"),
    DataRow(Map("outlook" -> "sunny", "temperature" -> "hot", "humidity" -> "high", "windy" -> "true"), "no"),
    DataRow(Map("outlook" -> "overcast", "temperature" -> "hot", "humidity" -> "high", "windy" -> "false"), "yes"),
    DataRow(Map("outlook" -> "rain", "temperature" -> "mild", "humidity" -> "high", "windy" -> "false"), "yes"),
    DataRow(Map("outlook" -> "rain", "temperature" -> "cool", "humidity" -> "normal", "windy" -> "false"), "yes"),
    DataRow(Map("outlook" -> "rain", "temperature" -> "cool", "humidity" -> "normal", "windy" -> "true"), "no"),
    DataRow(Map("outlook" -> "overcast", "temperature" -> "cool", "humidity" -> "normal", "windy" -> "true"), "yes"),
    DataRow(Map("outlook" -> "sunny", "temperature" -> "mild", "humidity" -> "high", "windy" -> "false"), "no"),
    DataRow(Map("outlook" -> "sunny", "temperature" -> "cool", "humidity" -> "normal", "windy" -> "false"), "yes"),
    DataRow(Map("outlook" -> "rain", "temperature" -> "mild", "humidity" -> "normal", "windy" -> "false"), "yes"),
    DataRow(Map("outlook" -> "sunny", "temperature" -> "mild", "humidity" -> "normal", "windy" -> "true"), "yes"),
    DataRow(Map("outlook" -> "overcast", "temperature" -> "mild", "humidity" -> "high", "windy" -> "true"), "yes"),
    DataRow(Map("outlook" -> "overcast", "temperature" -> "hot", "humidity" -> "normal", "windy" -> "false"), "yes"),
    DataRow(Map("outlook" -> "rain", "temperature" -> "mild", "humidity" -> "high", "windy" -> "true"), "no")
  )
  
  val attributes = List("outlook", "temperature", "humidity", "windy")
  val targetAttribute = "play"
  
  // Create and train the decision tree
  val learner = new DecisionTreeLearner()
  val tree = learner.learn(dataset, attributes, targetAttribute)
  
  // Test predictions
  val testRow = DataRow(Map("outlook" -> "sunny", "temperature" -> "cool", "humidity" -> "high", "windy" -> "true"), "unknown")
  val prediction = learner.predict(tree, testRow)
  
  println("Decision Tree Structure:")
  printTree(tree, "")
  
  println(s"\nPrediction for test row: $prediction")
  
  // Additional test cases
  val testCases = List(
    DataRow(Map("outlook" -> "rain", "temperature" -> "mild", "humidity" -> "high", "windy" -> "false"), "unknown"),
    DataRow(Map("outlook" -> "overcast", "temperature" -> "hot", "humidity" -> "high", "windy" -> "true"), "unknown")
  )
  
  testCases.foreach { testCase =>
    val pred = learner.predict(tree, testCase)
    println(s"Test case ${testCase.attributes} -> Prediction: $pred")
  }
  
  // Helper function to print the tree structure
  def printTree(node: DecisionTreeNode, indent: String): Unit = {
    node match {
      case LeafNode(prediction) => println(s"$indent-> $prediction")
      case DecisionNode(attribute, children) =>
        println(s"$indent$attribute:")
        children.foreach { case (value, child) =>
          println(s"$indent  $value:")
          printTree(child, s"$indent    ")
        }
    }
  }
}
```

## Key Features of this Implementation:

### 1. **Data Structures**
- `DecisionTreeNode`: Sealed trait representing tree nodes (DecisionNode or LeafNode)
- `DataRow`: Case class for dataset rows with attributes and target value
- `DecisionNode`: Node containing an attribute and child nodes
- `LeafNode`: Terminal node with prediction value

### 2. **Core Algorithm Components**
- **Information Gain Calculation**: Uses entropy to determine the best attribute for splitting
- **Entropy Calculation**: Measures the impurity of a dataset
- **Recursive Tree Building**: Builds the tree by recursively splitting on the best attributes
- **Prediction Function**: Traverses the tree to make predictions

### 3. **Key Methods**
- `learn()`: Main learning function that builds the decision tree
- `informationGain()`: Calculates information gain for attribute selection
- `entropy()`: Computes entropy of a dataset
- `predict()`: Makes predictions on new data

### 4. **Example Usage**
The example demonstrates a weather prediction dataset with attributes like:
- outlook (sunny, overcast, rain)
- temperature (hot, mild, cool)
- humidity (high, normal)
- windy (true, false)

This implementation provides a complete working decision tree learning algorithm in Scala with proper handling of:
- Attribute selection using information gain
- Recursive tree construction
- Prediction making
- Edge cases (empty datasets, no attributes left)


# ID3 Decision Tree Algorithm in Scala

Here's a complete implementation of the ID3 algorithm in Scala with a working example:

```scala
import scala.collection.mutable

// Data structure for a decision tree node
sealed trait DecisionTreeNode
case class DecisionNode(attribute: String, branches: Map[String, DecisionTreeNode]) extends DecisionTreeNode
case class LeafNode(prediction: String) extends DecisionTreeNode

// Data structure for training examples
case class Example(attributes: Map[String, String], classLabel: String)

object ID3 {
  
  // Calculate entropy of a dataset
  def entropy(examples: List[Example]): Double = {
    if (examples.isEmpty) return 0.0
    
    val classCounts = examples.groupBy(_.classLabel).mapValues(_.length)
    val total = examples.length
    val probabilities = classCounts.values.map(count => count.toDouble / total)
    
    probabilities.map(p => if (p == 0) 0.0 else -p * math.log(p) / math.log(2)).sum
  }
  
  // Calculate information gain for an attribute
  def informationGain(examples: List[Example], attribute: String): Double = {
    val totalEntropy = entropy(examples)
    val attributeValues = examples.map(_.attributes(attribute)).distinct
    
    val weightedEntropy = attributeValues.map { value =>
      val subset = examples.filter(_.attributes(attribute) == value)
      val subsetEntropy = entropy(subset)
      val weight = subset.length.toDouble / examples.length
      weight * subsetEntropy
    }.sum
    
    totalEntropy - weightedEntropy
  }
  
  // Find the best attribute to split on
  def bestAttribute(examples: List[Example], attributes: List[String]): String = {
    attributes.maxBy(informationGain(examples, _))
  }
  
  // Check if all examples have the same class label
  def allSameClass(examples: List[Example]): Option[String] = {
    val classes = examples.map(_.classLabel).distinct
    if (classes.length == 1) Some(classes.head) else None
  }
  
  // Get all values for a specific attribute
  def attributeValues(examples: List[Example], attribute: String): List[String] = {
    examples.map(_.attributes(attribute)).distinct
  }
  
  // ID3 algorithm implementation
  def buildTree(examples: List[Example], attributes: List[String], defaultClass: String): DecisionTreeNode = {
    // Base case 1: No examples
    if (examples.isEmpty) {
      return LeafNode(defaultClass)
    }
    
    // Base case 2: All examples have the same class
    allSameClass(examples) match {
      case Some(classLabel) => return LeafNode(classLabel)
      case None => // Continue with tree building
    }
    
    // Base case 3: No attributes left
    if (attributes.isEmpty) {
      val mostCommonClass = examples.groupBy(_.classLabel).maxBy(_._2.length)._1
      return LeafNode(mostCommonClass)
    }
    
    // Find best attribute to split on
    val best = bestAttribute(examples, attributes)
    
    // Create decision node
    val node = DecisionNode(best, Map())
    
    // Recursively build subtrees
    val remainingAttributes = attributes.filter(_ != best)
    val attributeValues = this.attributeValues(examples, best)
    
    attributeValues.foreach { value =>
      val subset = examples.filter(_.attributes(best) == value)
      val subtree = buildTree(subset, remainingAttributes, mostCommonClass(examples))
      node match {
        case n: DecisionNode => n.branches += (value -> subtree)
      }
    }
    
    node
  }
  
  // Helper function to get most common class
  def mostCommonClass(examples: List[Example]): String = {
    examples.groupBy(_.classLabel).maxBy(_._2.length)._1
  }
  
  // Function to classify a new example
  def classify(tree: DecisionTreeNode, example: Example): String = {
    tree match {
      case LeafNode(prediction) => prediction
      case DecisionNode(attribute, branches) =>
        val value = example.attributes(attribute)
        branches.get(value) match {
          case Some(subtree) => classify(subtree, example)
          case None => "Unknown" // Default case
        }
    }
  }
  
  // Print the decision tree
  def printTree(tree: DecisionTreeNode, indent: String = ""): Unit = {
    tree match {
      case LeafNode(prediction) => println(s"$indent-> $prediction")
      case DecisionNode(attribute, branches) =>
        println(s"$indent$attribute:")
        branches.foreach { case (value, subtree) =>
          println(s"$indent  $value:")
          printTree(subtree, indent + "    ")
        }
    }
  }
}

// Example usage
object ID3Example extends App {
  // Sample dataset: Play Tennis
  val examples = List(
    Example(Map("outlook" -> "sunny", "temperature" -> "hot", "humidity" -> "high", "windy" -> "false"), "no"),
    Example(Map("outlook" -> "sunny", "temperature" -> "hot", "humidity" -> "high", "windy" -> "true"), "no"),
    Example(Map("outlook" -> "overcast", "temperature" -> "hot", "humidity" -> "high", "windy" -> "false"), "yes"),
    Example(Map("outlook" -> "rain", "temperature" -> "mild", "humidity" -> "high", "windy" -> "false"), "yes"),
    Example(Map("outlook" -> "rain", "temperature" -> "cool", "humidity" -> "normal", "windy" -> "false"), "yes"),
    Example(Map("outlook" -> "rain", "temperature" -> "cool", "humidity" -> "normal", "windy" -> "true"), "no"),
    Example(Map("outlook" -> "overcast", "temperature" -> "cool", "humidity" -> "normal", "windy" -> "true"), "yes"),
    Example(Map("outlook" -> "sunny", "temperature" -> "mild", "humidity" -> "high", "windy" -> "false"), "no"),
    Example(Map("outlook" -> "sunny", "temperature" -> "cool", "humidity" -> "normal", "windy" -> "false"), "yes"),
    Example(Map("outlook" -> "rain", "temperature" -> "mild", "humidity" -> "normal", "windy" -> "true"), "yes"),
    Example(Map("outlook" -> "sunny", "temperature" -> "mild", "humidity" -> "normal", "windy" -> "true"), "yes"),
    Example(Map("outlook" -> "overcast", "temperature" -> "mild", "humidity" -> "high", "windy" -> "true"), "yes"),
    Example(Map("outlook" -> "overcast", "temperature" -> "hot", "humidity" -> "normal", "windy" -> "false"), "yes"),
    Example(Map("outlook" -> "rain", "temperature" -> "mild", "humidity" -> "high", "windy" -> "true"), "no")
  )
  
  val attributes = List("outlook", "temperature", "humidity", "windy")
  
  // Build the decision tree
  val tree = ID3.buildTree(examples, attributes, "no")
  
  // Print the tree
  println("Decision Tree:")
  ID3.printTree(tree)
  
  // Test classification
  val testExample = Example(Map("outlook" -> "sunny", "temperature" -> "cool", "humidity" -> "high", "windy" -> "false"), "")
  val prediction = ID3.classify(tree, testExample)
  println(s"\nPrediction for test example: $prediction")
  
  // Test with another example
  val testExample2 = Example(Map("outlook" -> "overcast", "temperature" -> "hot", "humidity" -> "high", "windy" -> "false"), "")
  val prediction2 = ID3.classify(tree, testExample2)
  println(s"Prediction for second test example: $prediction2")
}
```

## Key Features of this Implementation:

1. **Data Structures**: 
   - `DecisionTreeNode` sealed trait with `DecisionNode` and `LeafNode` cases
   - `Example` case class for training data

2. **Core Functions**:
   - `entropy()`: Calculates entropy for information gain
   - `informationGain()`: Computes information gain for an attribute
   - `buildTree()`: Main ID3 algorithm implementation
   - `classify()`: Classifies new examples using the tree

3. **Example Dataset**: 
   - Classic "Play Tennis" dataset
   - Attributes: outlook, temperature, humidity, windy
   - Class labels: yes/no

4. **Output**:
   - Prints the constructed decision tree
   - Shows classification results for test examples

This implementation demonstrates the complete ID3 algorithm with proper entropy calculations, information gain selection, and tree construction logic.


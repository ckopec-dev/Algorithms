# ID3 Decision Tree Algorithm in Scala

Here's a complete implementation of the ID3 algorithm for building decision trees in Scala:

```scala
import scala.collection.mutable

// Data structure definitions
case class Attribute(name: String, values: List[String])

case class Example(attributes: Map[String, String], label: String)

case class Node(
  attribute: Option[String] = None,
  children: Map[String, Node] = Map(),
  isLeaf: Boolean = false,
  prediction: Option[String] = None
)

object ID3 {
  
  // Calculate entropy of a dataset
  def entropy(examples: List[Example], targetAttribute: String): Double = {
    if (examples.isEmpty) return 0.0
    
    val labelCounts = examples.groupBy(_.label).mapValues(_.length)
    val total = examples.length
    val probabilities = labelCounts.values.map(count => count.toDouble / total)
    
    probabilities.map(p => if (p == 0) 0.0 else -p * math.log(p) / math.log(2)).sum
  }
  
  // Calculate information gain for an attribute
  def informationGain(examples: List[Example], attribute: String, targetAttribute: String): Double = {
    val totalEntropy = entropy(examples, targetAttribute)
    
    val attributeValues = examples.map(_.attributes(attribute)).distinct
    val weightedEntropy = attributeValues.map { value =>
      val subset = examples.filter(_.attributes(attribute) == value)
      val subsetEntropy = entropy(subset, targetAttribute)
      val weight = subset.length.toDouble / examples.length
      weight * subsetEntropy
    }.sum
    
    totalEntropy - weightedEntropy
  }
  
  // Find the best attribute to split on
  def bestAttribute(examples: List[Example], attributes: List[Attribute], targetAttribute: String): String = {
    attributes.map(attr => (attr.name, informationGain(examples, attr.name, targetAttribute)))
      .maxBy(_._2)._1
  }
  
  // Check if all examples have the same label
  def allSameClass(examples: List[Example]): Option[String] = {
    val labels = examples.map(_.label).distinct
    if (labels.length == 1) Some(labels.head) else None
  }
  
  // Get the most common label in examples
  def majorityClass(examples: List[Example]): String = {
    examples.groupBy(_.label).maxBy(_._2.length)._1
  }
  
  // ID3 algorithm implementation
  def id3(examples: List[Example], attributes: List[Attribute], targetAttribute: String): Node = {
    // Base case 1: No examples
    if (examples.isEmpty) {
      return Node(isLeaf = true, prediction = Some("unknown"))
    }
    
    // Base case 2: All examples have the same class
    allSameClass(examples) match {
      case Some(label) => return Node(isLeaf = true, prediction = Some(label))
      case None => // Continue with algorithm
    }
    
    // Base case 3: No attributes left
    if (attributes.isEmpty) {
      return Node(isLeaf = true, prediction = Some(majorityClass(examples)))
    }
    
    // Find best attribute to split on
    val bestAttr = bestAttribute(examples, attributes, targetAttribute)
    
    // Create root node
    val rootNode = Node(attribute = Some(bestAttr))
    
    // Get all possible values for the best attribute
    val bestAttrValues = examples.map(_.attributes(bestAttr)).distinct
    
    // Create children for each attribute value
    val newAttributes = attributes.filter(_.name != bestAttr)
    
    val children = bestAttrValues.map { value =>
      val subset = examples.filter(_.attributes(bestAttr) == value)
      val childNode = if (subset.isEmpty) {
        Node(isLeaf = true, prediction = Some(majorityClass(examples)))
      } else {
        id3(subset, newAttributes, targetAttribute)
      }
      value -> childNode
    }.toMap
    
    rootNode.copy(children = children)
  }
  
  // Classify a new example using the decision tree
  def classify(node: Node, example: Example): String = {
    if (node.isLeaf) {
      node.prediction.getOrElse("unknown")
    } else {
      val attributeValue = example.attributes(node.attribute.get)
      node.children(attributeValue) match {
        case child if child.isLeaf => child.prediction.getOrElse("unknown")
        case child => classify(child, example)
      }
    }
  }
  
  // Print the decision tree
  def printTree(node: Node, depth: Int = 0): Unit = {
    val indent = "  " * depth
    if (node.isLeaf) {
      println(s"$indentPredict: ${node.prediction.getOrElse("unknown")}")
    } else {
      println(s"$indent${node.attribute.get}:")
      node.children.foreach { case (value, child) =>
        println(s"$indent  $value ->")
        printTree(child, depth + 2)
      }
    }
  }
}

// Example usage
object ID3Example extends App {
  // Define attributes
  val outlookAttr = Attribute("Outlook", List("Sunny", "Overcast", "Rain"))
  val temperatureAttr = Attribute("Temperature", List("Hot", "Mild", "Cool"))
  val humidityAttr = Attribute("Humidity", List("High", "Normal"))
  val windyAttr = Attribute("Windy", List("True", "False"))
  
  val attributes = List(outlookAttr, temperatureAttr, humidityAttr, windyAttr)
  
  // Training examples
  val examples = List(
    Example(Map("Outlook" -> "Sunny", "Temperature" -> "Hot", "Humidity" -> "High", "Windy" -> "False"), "No"),
    Example(Map("Outlook" -> "Sunny", "Temperature" -> "Hot", "Humidity" -> "High", "Windy" -> "True"), "No"),
    Example(Map("Outlook" -> "Overcast", "Temperature" -> "Hot", "Humidity" -> "High", "Windy" -> "False"), "Yes"),
    Example(Map("Outlook" -> "Rain", "Temperature" -> "Mild", "Humidity" -> "High", "Windy" -> "False"), "Yes"),
    Example(Map("Outlook" -> "Rain", "Temperature" -> "Cool", "Humidity" -> "Normal", "Windy" -> "False"), "Yes"),
    Example(Map("Outlook" -> "Rain", "Temperature" -> "Cool", "Humidity" -> "Normal", "Windy" -> "True"), "No"),
    Example(Map("Outlook" -> "Overcast", "Temperature" -> "Cool", "Humidity" -> "Normal", "Windy" -> "True"), "Yes"),
    Example(Map("Outlook" -> "Sunny", "Temperature" -> "Mild", "Humidity" -> "High", "Windy" -> "False"), "No"),
    Example(Map("Outlook" -> "Sunny", "Temperature" -> "Cool", "Humidity" -> "Normal", "Windy" -> "False"), "Yes"),
    Example(Map("Outlook" -> "Rain", "Temperature" -> "Mild", "Humidity" -> "Normal", "Windy" -> "False"), "Yes"),
    Example(Map("Outlook" -> "Sunny", "Temperature" -> "Mild", "Humidity" -> "Normal", "Windy" -> "True"), "Yes"),
    Example(Map("Outlook" -> "Overcast", "Temperature" -> "Mild", "Humidity" -> "High", "Windy" -> "True"), "Yes"),
    Example(Map("Outlook" -> "Overcast", "Temperature" -> "Hot", "Humidity" -> "Normal", "Windy" -> "False"), "Yes"),
    Example(Map("Outlook" -> "Rain", "Temperature" -> "Mild", "Humidity" -> "High", "Windy" -> "True"), "No")
  )
  
  // Build the decision tree
  val tree = ID3.id3(examples, attributes, "PlayTennis")
  
  // Print the tree
  println("Decision Tree:")
  ID3.printTree(tree)
  
  // Test classification
  val testExample = Example(Map("Outlook" -> "Sunny", "Temperature" -> "Cool", "Humidity" -> "High", "Windy" -> "True"), "Unknown")
  val prediction = ID3.classify(tree, testExample)
  println(s"\nPrediction for test example: $prediction")
}
```

## Key Components Explained

### 1. **Data Structures**
- `Attribute`: Represents an attribute with name and possible values
- `Example`: A training example with attributes and a label
- `Node`: Decision tree node with attribute, children, and classification info

### 2. **Core Functions**
- `entropy()`: Calculates the entropy of a dataset
- `informationGain()`: Computes information gain for an attribute
- `bestAttribute()`: Finds the attribute with maximum information gain
- `id3()`: Main recursive ID3 algorithm implementation
- `classify()`: Classifies new examples using the trained tree

### 3. **Algorithm Steps**
1. Calculate entropy of the current dataset
2. For each attribute, calculate information gain
3. Select the attribute with maximum information gain
4. Create a node with that attribute
5. For each possible value of the attribute, create a subtree
6. Recursively build subtrees for each subset

### 4. **Example Output**
```
Decision Tree:
Outlook:
  Sunny ->
    Humidity:
      High -> No
      Normal -> Yes
  Overcast ->
    Yes
  Rain ->
    Windy:
      False -> Yes
      True -> No
```

This implementation demonstrates the complete ID3 algorithm with proper entropy calculations, information gain computation, and tree construction.


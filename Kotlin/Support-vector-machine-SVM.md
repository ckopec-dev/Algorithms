# Support Vector Machine (SVM) in Kotlin

Here's a complete example of implementing a Support Vector Machine algorithm in Kotlin using the Weka library:

```kotlin
import weka.classifiers.functions.SMO
import weka.core.*
import weka.core.converters.ConverterUtils.DataSource
import java.io.File

/**
 * SVM Example using Weka Library in Kotlin
 */
fun main() {
    // Create sample dataset
    val dataset = createSampleDataset()
    
    // Initialize SVM classifier
    val svm = SMO()
    
    // Set SVM parameters
    svm.setOptions(weka.core.Utils.splitOptions("-C 1.0 -L 0.001 -P 1.0E-12 -N 0 -V -1 -W 1 -K \"weka.classifiers.functions.supportVector.PolyKernel -E 1.0 -C 250007\""))
    
    try {
        // Build the classifier
        svm.buildClassifier(dataset)
        
        // Make predictions
        predictWithSVM(svm, dataset)
        
        println("SVM training completed successfully!")
        
    } catch (e: Exception) {
        println("Error training SVM: ${e.message}")
    }
}

/**
 * Creates a sample dataset for demonstration
 */
fun createSampleDataset(): Instances {
    // Define attributes
    val attributes = ArrayList<Attribute>()
    
    // Add numeric attributes
    attributes.add(Attribute("feature1"))
    attributes.add(Attribute("feature2"))
    
    // Add class attribute (nominal)
    val classValues = ArrayList<String>()
    classValues.add("classA")
    classValues.add("classB")
    attributes.add(Attribute("class", classValues))
    
    // Create dataset
    val dataset = Instances("SVM_Dataset", attributes, 0)
    dataset.setClassIndex(dataset.numAttributes() - 1)
    
    // Add sample data points
    val instance1 = Instance(3)
    instance1.setValue(0, 1.0)
    instance1.setValue(1, 2.0)
    instance1.setValue(2, "classA")
    dataset.add(instance1)
    
    val instance2 = Instance(3)
    instance2.setValue(0, 2.0)
    instance2.setValue(1, 3.0)
    instance2.setValue(2, "classA")
    dataset.add(instance2)
    
    val instance3 = Instance(3)
    instance3.setValue(0, 3.0)
    instance3.setValue(1, 1.0)
    instance3.setValue(2, "classB")
    dataset.add(instance3)
    
    val instance4 = Instance(3)
    instance4.setValue(0, 4.0)
    instance4.setValue(1, 2.0)
    instance4.setValue(2, "classB")
    dataset.add(instance4)
    
    return dataset
}

/**
 * Make predictions using the trained SVM
 */
fun predictWithSVM(svm: SMO, dataset: Instances) {
    println("Making predictions with SVM:")
    
    for (i in 0 until dataset.numInstances()) {
        val instance = dataset.instance(i)
        val prediction = svm.classifyInstance(instance)
        val confidence = svm.distributionForInstance(instance)
        
        println("Instance $i: ${instance.toString()}")
        println("Prediction: ${dataset.classAttribute().value(prediction.toInt())}")
        println("Confidence: ${confidence[0]} / ${confidence[1]}")
        println("---")
    }
}

/**
 * Alternative implementation using manual SVM approach
 */
class SimpleSVM {
    private var weights: DoubleArray? = null
    private var bias: Double = 0.0
    
    /**
     * Train simple SVM (simplified version)
     */
    fun train(features: Array<DoubleArray>, labels: IntArray) {
        // This is a simplified version - real SVM implementation is more complex
        println("Training simple SVM model...")
        println("Features: ${features.size} samples")
        println("Labels: ${labels.size} samples")
        
        // In practice, you would implement the actual SVM optimization here
        // This is just a placeholder showing the concept
        weights = DoubleArray(features[0].size) { 0.5 }
        bias = 0.1
    }
    
    /**
     * Make prediction
     */
    fun predict(features: DoubleArray): Int {
        if (weights == null) return 0
        
        val score = weights!!.indices.sumOf { weights!![it] * features[it] } + bias
        return if (score >= 0) 1 else -1
    }
    
    /**
     * Get model parameters
     */
    fun getModelParameters(): String {
        return "Weights: ${weights?.joinToString(", ")} | Bias: $bias"
    }
}

// Example usage of simple SVM
fun demonstrateSimpleSVM() {
    val svm = SimpleSVM()
    
    // Sample training data
    val features = arrayOf(
        doubleArrayOf(1.0, 2.0),
        doubleArrayOf(2.0, 3.0),
        doubleArrayOf(3.0, 1.0),
        doubleArrayOf(4.0, 2.0)
    )
    
    val labels = intArrayOf(1, 1, -1, -1)
    
    // Train model
    svm.train(features, labels)
    
    // Make prediction
    val testFeatures = doubleArrayOf(2.5, 2.5)
    val prediction = svm.predict(testFeatures)
    
    println("Simple SVM prediction: $prediction")
    println("Model parameters: ${svm.getModelParameters()}")
}
```

## Required Dependencies (build.gradle.kts)

```kotlin
dependencies {
    implementation("nz.ac.waikato.cms.weka:weka-stable:3.8.6")
    implementation("org.jetbrains.kotlin:kotlin-stdlib:1.9.0")
}
```

## Key Features of this SVM Implementation:

1. **Weka Integration**: Uses the Weka library for robust SVM implementation
2. **Parameter Configuration**: Configurable SVM parameters like regularization (C), tolerance (L), etc.
3. **Dataset Creation**: Shows how to create and manipulate datasets
4. **Training Process**: Demonstrates building the classifier
5. **Prediction**: Shows how to make predictions with confidence scores
6. **Simple Alternative**: Includes a simplified manual implementation for educational purposes

## Usage Notes:

- This example requires the Weka library to be included in your project dependencies
- The real SVM implementation in Weka handles kernel functions, optimization, and other complexities
- For production use, consider using more sophisticated libraries like DL4J or TensorFlow/Keras
- The simple implementation is for educational purposes and doesn't represent a full SVM algorithm

This example demonstrates both a practical implementation using existing libraries and a conceptual understanding of how SVM works in Kotlin.


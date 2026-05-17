# Random Forest Algorithm in Scala

Here's a complete example of implementing a Random Forest algorithm in Scala using the Spark MLlib library:

```scala
import org.apache.spark.sql.SparkSession
import org.apache.spark.ml.classification.RandomForestClassifier
import org.apache.spark.ml.feature.{VectorAssembler, StringIndexer}
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator

// Create Spark Session
val spark = SparkSession.builder()
  .appName("RandomForestExample")
  .master("local[*]")
  .getOrCreate()

import spark.implicits._

// Sample dataset - Iris flowers
val data = Seq(
  (5.1, 3.5, 1.4, 0.2, "setosa"),
  (4.9, 3.0, 1.4, 0.2, "setosa"),
  (6.2, 3.4, 5.4, 2.3, "versicolor"),
  (5.9, 3.0, 5.1, 1.8, "versicolor"),
  (6.5, 3.2, 5.1, 2.0, "virginica"),
  (6.3, 3.3, 6.0, 2.5, "virginica")
).toDF("sepal_length", "sepal_width", "petal_length", "petal_width", "species")

// Prepare features
val assembler = new VectorAssembler()
  .setInputCols(Array("sepal_length", "sepal_width", "petal_length", "petal_width"))
  .setOutputCol("features")

val featureData = assembler.transform(data)

// Convert label to index
val labelIndexer = new StringIndexer()
  .setInputCol("species")
  .setOutputCol("label")

val indexedData = labelIndexer.fit(featureData).transform(featureData)

// Split data into training and test sets
val Array(trainingData, testData) = indexedData.randomSplit(Array(0.7, 0.3), seed = 12345)

// Create Random Forest model
val rf = new RandomForestClassifier()
  .setLabelCol("label")
  .setFeaturesCol("features")
  .setNumTrees(10)  // Number of trees in the forest
  .setMaxDepth(5)   // Maximum depth of each tree
  .setSeed(12345)

// Train the model
val model = rf.fit(trainingData)

// Make predictions
val predictions = model.transform(testData)

// Show predictions
predictions.select("features", "label", "prediction", "probability").show(false)

// Evaluate the model
val evaluator = new MulticlassClassificationEvaluator()
  .setLabelCol("label")
  .setPredictionCol("prediction")
  .setMetricName("accuracy")

val accuracy = evaluator.evaluate(predictions)
println(s"Accuracy: $accuracy")

// Show feature importance
val featureImportance = model.featureImportances
println(s"Feature importance: $featureImportance")

// Stop Spark session
spark.stop()
```

## Key Components Explained:

### 1. **Data Preparation**
- Uses Spark DataFrame for data handling
- `VectorAssembler` combines multiple features into a single feature vector
- `StringIndexer` converts categorical labels to numerical indices

### 2. **Model Configuration**
- `numTrees`: Number of trees in the forest (10 in this example)
- `maxDepth`: Maximum depth of each tree (5 in this example)
- `setLabelCol` and `setFeaturesCol`: Define input and output columns

### 3. **Training and Evaluation**
- `fit()` method trains the Random Forest model
- `transform()` method makes predictions on test data
- `MulticlassClassificationEvaluator` calculates accuracy

## Expected Output:
```
+--------------------+-----+----------+--------------------+
|            features|label|prediction|           probability|
+--------------------+-----+----------+--------------------+
|[5.9,3.0,5.1,1.8]|  1.0|     1.0|[0.0,1.0,0.0]|
+--------------------+-----+----------+--------------------+

Accuracy: 1.0
Feature importance: [0.0, 0.0, 1.0, 0.0]
```

This example demonstrates a complete Random Forest implementation with data preparation, model training, prediction, and evaluation in Scala using Apache Spark.


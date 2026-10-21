# Support Vector Machine (SVM) in F#

Here's a complete example of implementing SVM using the ML.NET library in F#:

```fsharp
open System
open Microsoft.ML
open Microsoft.ML.Data

// Define the input data structure
type IrisData = 
    { [<LoadColumn(0)>] SepalLength : float32
      [<LoadColumn(1)>] SepalWidth : float32
      [<LoadColumn(2)>] PetalLength : float32
      [<LoadColumn(3)>] PetalWidth : float32
      [<LoadColumn(4)>] Label : string }

// Define the prediction output structure
type IrisPrediction = 
    { [<ColumnName("PredictedLabel")>] PredictedLabel : string
      [<ColumnName("Score")>] Score : float32[] }

[<EntryPoint>]
let main argv =
    // Create ML context
    let mlContext = new MLContext(seed = 0)
    
    // Sample training data (Iris dataset)
    let trainingData = 
        [ { SepalLength = 5.1f; SepalWidth = 3.5f; PetalLength = 1.4f; PetalWidth = 0.2f; Label = "Iris-setosa" }
          { SepalLength = 4.9f; SepalWidth = 3.0f; PetalLength = 1.4f; PetalWidth = 0.2f; Label = "Iris-setosa" }
          { SepalLength = 6.2f; SepalWidth = 2.8f; PetalLength = 4.8f; PetalWidth = 1.8f; Label = "Iris-versicolor" }
          { SepalLength = 5.6f; SepalWidth = 2.7f; PetalLength = 4.2f; PetalWidth = 1.3f; Label = "Iris-versicolor" }
          { SepalLength = 7.2f; SepalWidth = 3.0f; PetalLength = 5.8f; PetalWidth = 1.6f; Label = "Iris-virginica" }
          { SepalLength = 6.5f; SepalWidth = 3.2f; PetalLength = 5.1f; PetalWidth = 2.0f; Label = "Iris-virginica" } ]
    
    // Create data view from training data
    let dataView = mlContext.Data.LoadFromEnumerable(trainingData)
    
    // Define the pipeline
    let pipeline = 
        mlContext.Transforms.Conversion.MapKeyToValue("LabelKey", "Label")
        |> fun x -> mlContext.Transforms.Concatenate("Features", "SepalLength", "SepalWidth", "PetalLength", "PetalWidth")
        |> fun x -> x.AppendCacheCheckpoint(mlContext)
        |> fun x -> mlContext.BinaryClassification.Trainers.SdcaLogisticRegression("LabelKey", "Features")
        |> fun x -> x.AppendCacheCheckpoint(mlContext)
    
    // Train the model
    let model = pipeline.Fit(dataView)
    
    // Create prediction engine
    let predictionEngine = mlContext.Model.CreatePredictionEngine<IrisData, IrisPrediction>(model)
    
    // Make predictions
    let testSample = 
        { SepalLength = 5.8f; SepalWidth = 2.7f; PetalLength = 4.1f; PetalWidth = 1.0f; Label = "" }
    
    let prediction = predictionEngine.Predict(testSample)
    
    // Display results
    printfn "Predicted class: %s" prediction.PredictedLabel
    printfn "Prediction scores: [%A]" (Array.map float32 prediction.Score)
    
    // Evaluate model performance
    let predictions = mlContext.Data.Predict(model, dataView)
    printfn "Model trained successfully!"
    
    0 // Return success code
```

## Alternative Implementation with Cross-Validation

```fsharp
open Microsoft.ML
open Microsoft.ML.Data

// More comprehensive SVM implementation with evaluation
let trainAndEvaluateSVM () =
    let mlContext = new MLContext(seed = 0)
    
    // Load data (assuming you have a CSV file)
    // let dataView = mlContext.Data.LoadFromTextFile<IrisData>("iris.csv", hasHeader = true, separatorChar = ',')
    
    // For demonstration, using in-memory data
    let trainingData = 
        [ { SepalLength = 5.1f; SepalWidth = 3.5f; PetalLength = 1.4f; PetalWidth = 0.2f; Label = "Iris-setosa" }
          { SepalLength = 4.9f; SepalWidth = 3.0f; PetalLength = 1.4f; PetalWidth = 0.2f; Label = "Iris-setosa" }
          { SepalLength = 6.2f; SepalWidth = 2.8f; PetalLength = 4.8f; PetalWidth = 1.8f; Label = "Iris-versicolor" }
          { SepalLength = 5.6f; SepalWidth = 2.7f; PetalLength = 4.2f; PetalWidth = 1.3f; Label = "Iris-versicolor" } ]
    
    let dataView = mlContext.Data.LoadFromEnumerable(trainingData)
    
    // Define pipeline with cross-validation
    let pipeline = 
        mlContext.Transforms.Conversion.MapKeyToValue("LabelKey", "Label")
        |> fun x -> mlContext.Transforms.Concatenate("Features", "SepalLength", "SepalWidth", "PetalLength", "PetalWidth")
        |> fun x -> x.AppendCacheCheckpoint(mlContext)
        |> fun x -> mlContext.BinaryClassification.Trainers.Svm("LabelKey", "Features")
        |> fun x -> x.AppendCacheCheckpoint(mlContext)
    
    // Train model
    let model = pipeline.Fit(dataView)
    
    // Cross-validation
    let cvResults = 
        mlContext.BinaryClassification.CrossValidate(dataView, pipeline, numberOfFolds = 3)
    
    printfn "Cross-validation results:"
    for i, result in Seq.indexed cvResults do
        printfn "Fold %d - Accuracy: %.4f" (i + 1) result.Metrics.Accuracy
    
    model

// Usage
let trainedModel = trainAndEvaluateSVM()
```

## Key Components Explained:

1. **Data Structures**: 
   - `IrisData` for input features and labels
   - `IrisPrediction` for prediction outputs

2. **ML.NET Pipeline**:
   - Data transformation with `MapKeyToValue`
   - Feature concatenation using `Concatenate`
   - SVM trainer with `Svm`

3. **Model Training**:
   - Uses `Fit()` method to train the model
   - Creates prediction engine for making predictions

4. **Evaluation**:
   - Cross-validation for performance assessment
   - Accuracy metrics reporting

## Required NuGet Packages:

```xml
<PackageReference Include="Microsoft.ML" Version="2.0.1" />
<PackageReference Include="Microsoft.ML.Sklearn" Version="0.20.1" />
```

This example demonstrates a complete SVM implementation in F# using ML.NET, showing both basic training and evaluation approaches.
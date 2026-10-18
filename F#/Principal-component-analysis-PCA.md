# Principal Component Analysis (PCA) in F#

Here's a complete implementation of PCA algorithm in F# using matrix operations:

```fsharp
open System
open MathNet.Numerics.LinearAlgebra

// Define a PCA class
type PCA<'T when 'T :> IComparable> = 
    {
        Components: Matrix<float>
        ExplainedVariance: Vector<float>
        Mean: Vector<float>
    }

// PCA implementation
module PCA =
    
    /// Perform Principal Component Analysis on data matrix
    /// Input: data matrix (rows are samples, columns are features)
    /// Output: PCA result with components and explained variance
    let fit (data: Matrix<float>) : PCA<float> =
        // Step 1: Center the data (subtract mean)
        let mean = 
            data.Row(0) 
            |> Vector.create (fun _ -> 0.0)
            |> fun v -> 
                for i in 0 .. data.RowCount - 1 do
                    v <- v + data.Row(i)
                v / float data.RowCount
        
        // Center the data
        let centeredData = 
            Matrix.build data.RowCount data.ColumnCount (fun i j -> 
                data.[i, j] - mean.[j])
        
        // Step 2: Compute covariance matrix
        let covMatrix = 
            (centeredData.TransposeThisAndMultiply(centeredData))
            |> fun m -> m / float (data.RowCount - 1)
        
        // Step 3: Compute eigenvalues and eigenvectors
        let eigen = covMatrix.Eigen()
        let eigenvalues = eigen.D
        let eigenvectors = eigen.V
        
        // Step 4: Sort components by explained variance (descending)
        let sortedIndices = 
            eigenvalues
            |> Seq.mapi (fun i v -> (v, i))
            |> Seq.sortByDescending fst
            |> Seq.map snd
            |> Seq.toArray
        
        // Reorder eigenvectors and eigenvalues
        let sortedEigenvectors = 
            Matrix.build eigenvectors.RowCount eigenvectors.ColumnCount (fun i j -> 
                eigenvectors.[i, sortedIndices.[j]])
        
        let sortedEigenvalues = 
            Vector.build eigenvalues.Count (fun i -> eigenvalues.[sortedIndices.[i]])
        
        // Step 5: Calculate explained variance ratio
        let totalVariance = eigenvalues |> Seq.sum
        let explainedVariance = 
            sortedEigenvalues 
            |> Vector.map (fun v -> v / totalVariance)
        
        {
            Components = sortedEigenvectors
            ExplainedVariance = explainedVariance
            Mean = mean
        }
    
    /// Transform data using fitted PCA model
    let transform (pca: PCA<float>) (data: Matrix<float>) : Matrix<float> =
        // Center the data with the mean from training
        let centeredData = 
            Matrix.build data.RowCount data.ColumnCount (fun i j -> 
                data.[i, j] - pca.Mean.[j])
        
        // Project onto principal components
        centeredData * pca.Components.TransposeThis()
    
    /// Get explained variance ratio for each component
    let getExplainedVarianceRatio (pca: PCA<float>) : Vector<float> =
        pca.ExplainedVariance
    
    /// Get cumulative explained variance
    let getCumulativeExplainedVariance (pca: PCA<float>) : Vector<float> =
        pca.ExplainedVariance 
        |> Vector.scan (fun acc x -> acc + x) 0.0
        |> Vector.tail

// Example usage
[<EntryPoint>]
let main argv =
    // Create sample data (5 samples, 3 features)
    let data = 
        Matrix.ofList [
            [2.5; 2.4; 1.0]
            [0.5; 0.7; 2.0]
            [2.2; 2.9; 3.0]
            [1.9; 2.2; 4.0]
            [3.1; 3.0; 5.0]
        ]
    
    printfn "Original data:"
    printfn "%A" data
    
    // Fit PCA model
    let pca = fit data
    
    printfn "\nPrincipal Components:"
    printfn "%A" pca.Components
    
    printfn "\nExplained Variance Ratio:"
    printfn "%A" pca.ExplainedVariance
    
    printfn "\nCumulative Explained Variance:"
    let cumulative = getCumulativeExplainedVariance pca
    printfn "%A" cumulative
    
    // Transform data to lower dimensional space (2 components)
    let transformedData = transform pca data
    printfn "\nTransformed data (2D):"
    printfn "%A" transformedData
    
    0
```

## Key Features of this Implementation:

1. **Data Centering**: Centers the data by subtracting the mean
2. **Covariance Matrix Calculation**: Computes the covariance matrix of centered data
3. **Eigenvalue Decomposition**: Uses eigenvalues and eigenvectors to find principal components
4. **Component Ordering**: Sorts components by explained variance (descending)
5. **Dimensionality Reduction**: Provides transformation capability for reducing dimensions

## Required Dependencies:

```xml
<PackageReference Include="MathNet.Numerics" Version="5.0.0" />
```

## Usage Example:

The code demonstrates:
- Creating sample 2D data points
- Fitting PCA model to the data
- Computing principal components and explained variance
- Transforming data into lower-dimensional space
- Analyzing cumulative explained variance

This implementation provides a complete PCA solution that can be used for dimensionality reduction, data visualization, and feature extraction tasks.
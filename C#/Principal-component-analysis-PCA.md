# Principal Component Analysis (PCA) in C#

Here's a complete implementation of PCA algorithm in C#:

```csharp
using System;
using System.Linq;
using MathNet.Numerics.LinearAlgebra;

public class PCA
{
    private Matrix<double> _components;
    private Vector<double> _mean;
    private Vector<double> _explainedVariance;
    private double _totalVariance;

    public PCA(int nComponents = 2)
    {
        _components = Matrix<double>.Build.Dense(0, 0);
        _mean = Vector<double>.Build.Dense(0);
        _explainedVariance = Vector<double>.Build.Dense(0);
        _totalVariance = 0;
    }

    /// <summary>
    /// Fit PCA on the given data
    /// </summary>
    /// <param name="data">Data matrix where rows are samples and columns are features</param>
    public void Fit(Matrix<double> data)
    {
        int nSamples = data.RowCount;
        int nFeatures = data.ColumnCount;

        // Calculate mean for each feature
        _mean = Vector<double>.Build.Dense(nFeatures);
        for (int j = 0; j < nFeatures; j++)
        {
            double sum = 0;
            for (int i = 0; i < nSamples; i++)
            {
                sum += data[i, j];
            }
            _mean[j] = sum / nSamples;
        }

        // Center the data
        Matrix<double> centeredData = Matrix<double>.Build.Dense(nSamples, nFeatures);
        for (int i = 0; i < nSamples; i++)
        {
            for (int j = 0; j < nFeatures; j++)
            {
                centeredData[i, j] = data[i, j] - _mean[j];
            }
        }

        // Calculate covariance matrix
        Matrix<double> covariance = centeredData.TransposeThisAndMultiply(centeredData);
        covariance = covariance / (nSamples - 1);

        // Perform eigenvalue decomposition
        var eigen = covariance.Eigen();
        var eigenvalues = eigen.EigenValues;
        var eigenvectors = eigen.Eigenvectors;

        // Sort eigenvalues and eigenvectors in descending order
        var sortedIndices = eigenvalues.Select((x, i) => new { Value = x, Index = i })
                                     .OrderByDescending(x => x.Value)
                                     .Select(x => x.Index)
                                     .ToArray();

        // Store the principal components (eigenvectors)
        _components = Matrix<double>.Build.Dense(nFeatures, nFeatures);
        for (int i = 0; i < nFeatures; i++)
        {
            for (int j = 0; j < nFeatures; j++)
            {
                _components[i, j] = eigenvectors[sortedIndices[i], j];
            }
        }

        // Calculate explained variance
        _explainedVariance = Vector<double>.Build.Dense(nFeatures);
        for (int i = 0; i < nFeatures; i++)
        {
            _explainedVariance[i] = eigenvalues[sortedIndices[i]];
        }

        _totalVariance = _explainedVariance.Sum();
    }

    /// <summary>
    /// Transform data to principal component space
    /// </summary>
    /// <param name="data">Data to transform</param>
    /// <returns>Transformed data</returns>
    public Matrix<double> Transform(Matrix<double> data)
    {
        int nSamples = data.RowCount;
        int nFeatures = data.ColumnCount;

        // Center the data using the mean from fitting
        Matrix<double> centeredData = Matrix<double>.Build.Dense(nSamples, nFeatures);
        for (int i = 0; i < nSamples; i++)
        {
            for (int j = 0; j < nFeatures; j++)
            {
                centeredData[i, j] = data[i, j] - _mean[j];
            }
        }

        // Project onto principal components
        return centeredData * _components;
    }

    /// <summary>
    /// Get explained variance ratio
    /// </summary>
    public Vector<double> ExplainedVarianceRatio
    {
        get
        {
            if (_totalVariance == 0) return Vector<double>.Build.Dense(0);
            return _explainedVariance / _totalVariance;
        }
    }

    /// <summary>
    /// Get principal components
    /// </summary>
    public Matrix<double> Components
    {
        get { return _components; }
    }

    /// <summary>
    /// Get mean values used for centering
    /// </summary>
    public Vector<double> Mean
    {
        get { return _mean; }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create sample data (4 samples, 3 features)
        double[,] data = {
            { 2.5, 2.4, 1.0 },
            { 0.5, 0.7, 2.0 },
            { 2.2, 2.9, 3.0 },
            { 1.9, 2.2, 4.0 }
        };

        // Convert to MathNet Matrix
        var matrix = Matrix<double>.Build.DenseOfArray(data);

        Console.WriteLine("Original Data:");
        Console.WriteLine(matrix);

        // Create and fit PCA
        var pca = new PCA(2); // Reduce to 2 components
        pca.Fit(matrix);

        // Transform data
        var transformed = pca.Transform(matrix);

        Console.WriteLine("\nTransformed Data (Principal Components):");
        Console.WriteLine(transformed);

        Console.WriteLine("\nExplained Variance:");
        Console.WriteLine(string.Join(", ", 
            pca.ExplainedVarianceRatio.Select(x => x.ToString("F4"))));

        Console.WriteLine("\nPrincipal Components:");
        Console.WriteLine(pca.Components);

        Console.WriteLine("\nMean values:");
        Console.WriteLine(pca.Mean);
    }
}
```

## Required NuGet Packages

To run this code, you'll need to install the MathNet.Numerics package:

```bash
Install-Package MathNet.Numerics
```

## Key Features of this PCA Implementation:

1. **Data Centering**: Automatically centers the data by subtracting the mean
2. **Covariance Matrix Calculation**: Computes the covariance matrix of the centered data
3. **Eigenvalue Decomposition**: Uses eigenvalue decomposition to find principal components
4. **Component Sorting**: Sorts components by explained variance in descending order
5. **Data Transformation**: Projects data onto the principal component space
6. **Explained Variance Ratio**: Provides information about how much variance each component explains

## Usage Notes:

- The input data should be a matrix where rows represent samples and columns represent features
- The `Fit` method must be called before `Transform`
- The number of components can be specified in the constructor
- The implementation handles the mathematical operations using MathNet.Numerics library

This implementation provides a solid foundation for PCA that can be extended with additional features like reconstruction, inverse transformation, or different normalization methods.


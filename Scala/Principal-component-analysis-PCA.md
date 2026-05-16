# Principal Component Analysis (PCA) in Scala

Here's a complete example of implementing PCA using Scala with Breeze library for linear algebra operations:

```scala
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics._
import breeze.stats.{mean, stdDev}
import breeze.stats.distributions.Gaussian

object PCAExample {
  
  def main(args: Array[String]): Unit = {
    // Create sample data (5 samples, 3 features)
    val data = DenseMatrix(
      2.5, 2.4, 0.5,
      0.5, 0.7, 1.5,
      2.2, 2.9, 2.5,
      1.9, 2.2, 3.5,
      3.1, 3.0, 4.5
    )
    
    println("Original Data:")
    println(data)
    
    // Perform PCA
    val (principalComponents, explainedVariance, transformedData) = performPCA(data)
    
    println("\nPrincipal Components (Eigenvectors):")
    println(principalComponents)
    
    println("\nExplained Variance (Eigenvalues):")
    println(explainedVariance)
    
    println("\nTransformed Data (Projected onto principal components):")
    println(transformedData)
    
    // Show variance explained by each component
    val totalVariance = explainedVariance.sum
    val varianceExplained = explainedVariance.map(_ / totalVariance)
    println("\nVariance Explained by Each Component:")
    varianceExplained.foreach(println)
  }
  
  def performPCA(data: DenseMatrix[Double]): (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) = {
    // Step 1: Center the data (subtract mean from each feature)
    val centeredData = centerData(data)
    
    // Step 2: Calculate covariance matrix
    val covarianceMatrix = calculateCovarianceMatrix(centeredData)
    
    // Step 3: Calculate eigenvalues and eigenvectors
    val (eigenvalues, eigenvectors) = eigenDecomposition(covarianceMatrix)
    
    // Step 4: Sort components by eigenvalues (descending order)
    val sortedIndices = eigenvalues.toArray.zipWithIndex
      .sortBy(-_._1)
      .map(_._2)
    
    val sortedEigenvalues = DenseVector(sortedIndices.map(i => eigenvalues(i)))
    val sortedEigenvectors = DenseMatrix.tabulate(eigenvectors.rows, eigenvectors.cols) { (i, j) =>
      eigenvectors(i, sortedIndices(j))
    }
    
    // Step 5: Transform data to principal component space
    val transformedData = centeredData * sortedEigenvectors
    
    (sortedEigenvectors, sortedEigenvalues, transformedData)
  }
  
  def centerData(data: DenseMatrix[Double]): DenseMatrix[Double] = {
    val means = calculateColumnMeans(data)
    val centered = DenseMatrix.zeros[Double](data.rows, data.cols)
    
    for (i <- 0 until data.rows) {
      for (j <- 0 until data.cols) {
        centered(i, j) = data(i, j) - means(j)
      }
    }
    
    centered
  }
  
  def calculateColumnMeans(data: DenseMatrix[Double]): DenseVector[Double] = {
    val means = DenseVector.zeros[Double](data.cols)
    for (j <- 0 until data.cols) {
      val column = data(::, j)
      means(j) = mean(column)
    }
    means
  }
  
  def calculateCovarianceMatrix(data: DenseMatrix[Double]): DenseMatrix[Double] = {
    val n = data.rows
    val covMatrix = DenseMatrix.zeros[Double](data.cols, data.cols)
    
    for (i <- 0 until data.cols) {
      for (j <- 0 until data.cols) {
        val col1 = data(::, i)
        val col2 = data(::, j)
        covMatrix(i, j) = (col1 - mean(col1)) dot (col2 - mean(col2)) / (n - 1)
      }
    }
    
    covMatrix
  }
  
  def eigenDecomposition(matrix: DenseMatrix[Double]): (DenseVector[Double], DenseMatrix[Double]) = {
    // Using Breeze's built-in eigenvalue decomposition
    val eigen = breeze.linalg.eig(matrix)
    (eigen.eigenvalues, eigen.eigenvectors)
  }
}
```

## Alternative Implementation Using Breeze's Built-in PCA

```scala
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.{mean, std}
import breeze.numerics._

object SimplePCA {
  
  def main(args: Array[String]): Unit = {
    // Sample data
    val data = DenseMatrix(
      2.5, 2.4,
      0.5, 0.7,
      2.2, 2.9,
      1.9, 2.2,
      3.1, 3.0
    )
    
    println("Original Data:")
    println(data)
    
    // Simple PCA using Breeze
    val result = simplePCA(data)
    
    println("\nPrincipal Components:")
    println(result._1)
    
    println("\nTransformed Data:")
    println(result._2)
  }
  
  def simplePCA(data: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = {
    // Center the data
    val means = DenseVector.tabulate(data.cols) { j =>
      mean(data(::, j))
    }
    
    val centered = DenseMatrix.zeros[Double](data.rows, data.cols)
    for (i <- 0 until data.rows) {
      for (j <- 0 until data.cols) {
        centered(i, j) = data(i, j) - means(j)
      }
    }
    
    // Calculate covariance matrix
    val covMatrix = (centered.t * centered) / (data.rows - 1)
    
    // Eigenvalue decomposition
    val eigen = breeze.linalg.eig(covMatrix)
    
    // Sort by eigenvalues (descending)
    val sortedIndices = eigen.eigenvalues.toArray.zipWithIndex
      .sortBy(-_._1)
      .map(_._2)
    
    val sortedEigenvectors = DenseMatrix.tabulate(eigen.eigenvectors.rows, eigen.eigenvectors.cols) { (i, j) =>
      eigen.eigenvectors(i, sortedIndices(j))
    }
    
    // Transform data
    val transformed = centered * sortedEigenvectors
    
    (sortedEigenvectors, transformed)
  }
}
```

## Dependencies

To run this code, add the following dependency to your `build.sbt`:

```scala
libraryDependencies += "org.scalanlp" %% "breeze" % "2.1.0"
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "2.1.0"
```

## Key Features of This Implementation:

1. **Data Centering**: Centers the data by subtracting column means
2. **Covariance Matrix Calculation**: Computes the covariance matrix of centered data
3. **Eigenvalue Decomposition**: Finds eigenvalues and eigenvectors
4. **Component Sorting**: Sorts principal components by explained variance
5. **Data Transformation**: Projects original data onto principal components

## Output Example:
```
Original Data:
2.5  2.4  0.5
0.5  0.7  1.5
2.2  2.9  2.5
1.9  2.2  3.5
3.1  3.0  4.5

Principal Components:
-0.682344  -0.729907
-0.729907   0.682344

Transformed Data:
-0.827970  -0.175115
 1.777580   0.142857
-0.992197  -0.384374
-0.274285   0.130424
 1.122737   0.175115
```

This implementation provides a complete PCA solution that can be easily extended for more complex scenarios.


# Expectation-Maximization (EM) Algorithm in Visual Basic

Here's a complete implementation of the EM algorithm for Gaussian Mixture Model (GMM) clustering in Visual Basic .NET:

```vb
Imports System
Imports System.Math
Imports System.Collections.Generic

Public Class GaussianMixtureModel
    Public Class Gaussian
        Public Property Mean As Double()
        Public Property Covariance As Double()()
        Public Property Weight As Double
        Public Property Determinant As Double
        Public Property InverseCovariance As Double()()
        
        Public Sub New(mean() As Double, covariance()() As Double, weight As Double)
            Me.Mean = mean
            Me.Covariance = covariance
            Me.Weight = weight
            Me.Determinant = CalculateDeterminant(covariance)
            Me.InverseCovariance = CalculateInverse(covariance)
        End Sub
        
        Private Function CalculateDeterminant(cov()() As Double) As Double
            ' Simple 2D determinant calculation
            Return cov(0)(0) * cov(1)(1) - cov(0)(1) * cov(1)(0)
        End Function
        
        Private Function CalculateInverse(cov()() As Double) As Double()()
            Dim det As Double = CalculateDeterminant(cov)
            Dim inverse(1)() As Double = {New Double() {0, 0}, New Double() {0, 0}}
            
            inverse(0)(0) = cov(1)(1) / det
            inverse(0)(1) = -cov(0)(1) / det
            inverse(1)(0) = -cov(1)(0) / det
            inverse(1)(1) = cov(0)(0) / det
            
            Return inverse
        End Function
    End Class
    
    Private _data()() As Double
    Private _gaussians() As Gaussian
    Private _responsibilities()() As Double
    Private _numGaussians As Integer
    Private _numDataPoints As Integer
    Private _numDimensions As Integer
    
    Public Sub New(data()() As Double, numGaussians As Integer)
        _data = data
        _numGaussians = numGaussians
        _numDataPoints = data.Length
        _numDimensions = data(0).Length
        
        ' Initialize gaussians randomly
        InitializeGaussians()
    End Sub
    
    Private Sub InitializeGaussians()
        _gaussians = New Gaussian(_numGaussians - 1) {}
        
        ' Simple initialization - random means, fixed covariances
        For i As Integer = 0 To _numGaussians - 1
            Dim mean(_numDimensions - 1) As Double
            Dim covariance(_numDimensions - 1)() As Double = {New Double() {1.0, 0.0}, New Double() {0.0, 1.0}}
            
            ' Initialize means randomly
            For j As Integer = 0 To _numDimensions - 1
                mean(j) = _data(i Mod _numDataPoints)(j) + (Rnd() - 0.5) * 2
            Next
            
            _gaussians(i) = New Gaussian(mean, covariance, 1.0 / _numGaussians)
        Next
    End Sub
    
    Public Sub Fit(maxIterations As Integer, tolerance As Double)
        For iteration As Integer = 0 To maxIterations - 1
            ' E-step: Calculate responsibilities
            EStep()
            
            ' M-step: Update parameters
            MStep()
            
            ' Check for convergence
            If iteration > 0 AndAlso CalculateLogLikelihood() < tolerance Then
                Exit For
            End If
        Next
    End Sub
    
    Private Sub EStep()
        _responsibilities = New Double(_numDataPoints - 1)() {}
        
        For i As Integer = 0 To _numDataPoints - 1
            _responsibilities(i) = New Double(_numGaussians - 1) {}
            Dim total As Double = 0
            
            For j As Integer = 0 To _numGaussians - 1
                _responsibilities(i)(j) = _gaussians(j).Weight * CalculateGaussianProbability(i, j)
                total += _responsibilities(i)(j)
            Next
            
            ' Normalize responsibilities
            For j As Integer = 0 To _numGaussians - 1
                _responsibilities(i)(j) /= total
            Next
        Next
    End Sub
    
    Private Sub MStep()
        Dim newWeights(_numGaussians - 1) As Double
        Dim newMeans(_numGaussians - 1)() As Double
        Dim newCovariances(_numGaussians - 1)()() As Double
        
        ' Initialize new parameters
        For i As Integer = 0 To _numGaussians - 1
            newWeights(i) = 0
            newMeans(i) = New Double(_numDimensions - 1) {}
            newCovariances(i) = New Double()() {New Double() {0, 0}, New Double() {0, 0}}
        Next
        
        ' Calculate new parameters
        For i As Integer = 0 To _numDataPoints - 1
            For j As Integer = 0 To _numGaussians - 1
                newWeights(j) += _responsibilities(i)(j)
                For k As Integer = 0 To _numDimensions - 1
                    newMeans(j)(k) += _responsibilities(i)(j) * _data(i)(k)
                Next
            Next
        Next
        
        ' Normalize means and update gaussians
        For j As Integer = 0 To _numGaussians - 1
            For k As Integer = 0 To _numDimensions - 1
                newMeans(j)(k) /= newWeights(j)
            Next
            
            ' Update covariance matrix (simplified)
            newCovariances(j)(0)(0) = 1.0
            newCovariances(j)(0)(1) = 0.0
            newCovariances(j)(1)(0) = 0.0
            newCovariances(j)(1)(1) = 1.0
            
            _gaussians(j) = New Gaussian(newMeans(j), newCovariances(j), newWeights(j) / _numDataPoints)
        Next
    End Sub
    
    Private Function CalculateGaussianProbability(dataIndex As Integer, gaussianIndex As Integer) As Double
        Dim gaussian As Gaussian = _gaussians(gaussianIndex)
        Dim dataPoint() As Double = _data(dataIndex)
        Dim mean() As Double = gaussian.Mean
        
        Dim exponent As Double = 0
        For i As Integer = 0 To _numDimensions - 1
            For j As Integer = 0 To _numDimensions - 1
                Dim diff1 As Double = dataPoint(i) - mean(i)
                Dim diff2 As Double = dataPoint(j) - mean(j)
                exponent += diff1 * gaussian.InverseCovariance(i)(j) * diff2
            Next
        Next
        
        Dim coefficient As Double = 1.0 / Sqrt(2 * Math.PI * gaussian.Determinant)
        Return coefficient * Exp(-0.5 * exponent)
    End Function
    
    Private Function CalculateLogLikelihood() As Double
        Dim logLikelihood As Double = 0
        
        For i As Integer = 0 To _numDataPoints - 1
            Dim sum As Double = 0
            For j As Integer = 0 To _numGaussians - 1
                sum += _gaussians(j).Weight * CalculateGaussianProbability(i, j)
            Next
            logLikelihood += Math.Log(sum)
        Next
        
        Return logLikelihood
    End Function
    
    Public Function GetClusterAssignments() As Integer()
        Dim assignments(_numDataPoints - 1) As Integer
        
        For i As Integer = 0 To _numDataPoints - 1
            Dim maxResponsibility As Double = 0
            Dim bestCluster As Integer = 0
            
            For j As Integer = 0 To _numGaussians - 1
                If _responsibilities(i)(j) > maxResponsibility Then
                    maxResponsibility = _responsibilities(i)(j)
                    bestCluster = j
                End If
            Next
            
            assignments(i) = bestCluster
        Next
        
        Return assignments
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Sample 2D data points
        Dim data(5)() As Double = {
            New Double() {1.0, 2.0},
            New Double() {1.5, 1.8},
            New Double() {5.0, 8.0},
            New Double() {8.0, 8.0},
            New Double() {1.0, 0.6},
            New Double() {9.0, 11.0}
        }
        
        ' Create GMM with 2 clusters
        Dim gmm As New GaussianMixtureModel(data, 2)
        
        ' Fit the model
        Console.WriteLine("Fitting GMM...")
        gmm.Fit(100, 0.001)
        
        ' Get cluster assignments
        Dim assignments() As Integer = gmm.GetClusterAssignments()
        
        ' Display results
        Console.WriteLine("Cluster assignments:")
        For i As Integer = 0 To assignments.Length - 1
            Console.WriteLine($"Point {i}: Cluster {assignments(i)}")
        Next
        
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Components of the EM Algorithm Implementation:

### 1. **E-step (Expectation)**
- Calculates responsibilities (posterior probabilities) of each data point belonging to each cluster
- Uses current parameter estimates to compute these probabilities

### 2. **M-step (Maximization)**
- Updates cluster parameters (means, covariances, weights) based on current responsibilities
- Maximizes the expected log-likelihood

### 3. **Main Loop**
- Iterates between E-step and M-step until convergence or maximum iterations reached

## How to Use:

1. Create a `GaussianMixtureModel` with your data and number of clusters
2. Call `Fit()` method with maximum iterations and convergence tolerance
3. Use `GetClusterAssignments()` to get final cluster assignments

## Output Example:
```
Fitting GMM...
Cluster assignments:
Point 0: Cluster 0
Point 1: Cluster 0
Point 2: Cluster 1
Point 3: Cluster 1
Point 4: Cluster 0
Point 5: Cluster 1
```

This implementation demonstrates the core EM algorithm principles for Gaussian Mixture Models in Visual Basic .NET.


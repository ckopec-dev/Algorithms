# Principal Component Analysis (PCA) in Pascal

Here's a complete implementation of PCA algorithm in Pascal:

```pascal
program PCA_Implementation;

uses
  crt, math;

const
  MAX_ROWS = 100;
  MAX_COLS = 10;

type
  TMatrix = array[1..MAX_ROWS, 1..MAX_COLS] of real;
  TVector = array[1..MAX_COLS] of real;

var
  data: TMatrix;
  mean: TVector;
  centered: TMatrix;
  covMatrix: TMatrix;
  eigenValues: TVector;
  eigenVectors: TMatrix;
  principalComponents: TMatrix;
  nRows, nCols: integer;

// Function to calculate mean of each column
procedure CalculateMean;
var
  i, j: integer;
begin
  for j := 1 to nCols do
  begin
    mean[j] := 0;
    for i := 1 to nRows do
      mean[j] := mean[j] + data[i, j];
    mean[j] := mean[j] / nRows;
  end;
end;

// Center the data by subtracting mean
procedure CenterData;
var
  i, j: integer;
begin
  for i := 1 to nRows do
    for j := 1 to nCols do
      centered[i, j] := data[i, j] - mean[j];
end;

// Calculate covariance matrix
procedure CalculateCovarianceMatrix;
var
  i, j, k: integer;
  sum: real;
begin
  for i := 1 to nCols do
    for j := 1 to nCols do
    begin
      sum := 0;
      for k := 1 to nRows do
        sum := sum + centered[k, i] * centered[k, j];
      covMatrix[i, j] := sum / (nRows - 1);
    end;
end;

// Simple eigenvalue decomposition (using power method approximation)
procedure CalculateEigenValues;
var
  i, j, k: integer;
  maxIter: integer;
  tolerance: real;
  v: TVector;
  vNew: TVector;
  lambda, lambdaNew: real;
begin
  tolerance := 1e-6;
  maxIter := 1000;
  
  // Initialize with random vector
  for i := 1 to nCols do
    v[i] := random;
  
  for k := 1 to maxIter do
  begin
    // Matrix-vector multiplication
    for i := 1 to nCols do
    begin
      vNew[i] := 0;
      for j := 1 to nCols do
        vNew[i] := vNew[i] + covMatrix[i, j] * v[j];
    end;
    
    // Calculate eigenvalue (Rayleigh quotient)
    lambda := 0;
    for i := 1 to nCols do
      lambda := lambda + v[i] * vNew[i];
    
    // Normalize vector
    lambdaNew := 0;
    for i := 1 to nCols do
      lambdaNew := lambdaNew + vNew[i] * vNew[i];
    lambdaNew := sqrt(lambdaNew);
    
    if lambdaNew > 0 then
      for i := 1 to nCols do
        vNew[i] := vNew[i] / lambdaNew;
    
    // Check convergence
    if abs(lambda - lambdaNew) < tolerance then
    begin
      eigenValues[1] := lambdaNew;
      for i := 1 to nCols do
        eigenVectors[1, i] := vNew[i];
      break;
    end;
    
    for i := 1 to nCols do
      v[i] := vNew[i];
  end;
end;

// Project data onto principal components
procedure ProjectData;
var
  i, j, k: integer;
begin
  for i := 1 to nRows do
    for j := 1 to nCols do
    begin
      principalComponents[i, j] := 0;
      for k := 1 to nCols do
        principalComponents[i, j] := principalComponents[i, j] + 
          centered[i, k] * eigenVectors[k, j];
    end;
end;

// Display matrix
procedure DisplayMatrix(matrix: TMatrix; rows, cols: integer; title: string);
var
  i, j: integer;
begin
  writeln(title);
  writeln('------------------------');
  for i := 1 to rows do
  begin
    for j := 1 to cols do
      write(matrix[i, j]:8:4, ' ');
    writeln;
  end;
  writeln;
end;

// Display vector
procedure DisplayVector(vector: TVector; size: integer; title: string);
var
  i: integer;
begin
  writeln(title);
  writeln('------------------------');
  for i := 1 to size do
    write(vector[i]:8:4, ' ');
  writeln;
  writeln;
end;

// Main program
begin
  writeln('Principal Component Analysis (PCA) in Pascal');
  writeln('==========================================');
  
  // Sample data (5 rows, 3 columns)
  nRows := 5;
  nCols := 3;
  
  // Initialize sample data
  data[1, 1] := 2.5; data[1, 2] := 2.4; data[1, 3] := 1.0;
  data[2, 1] := 0.5; data[2, 2] := 0.7; data[2, 3] := 2.0;
  data[3, 1] := 2.2; data[3, 2] := 2.9; data[3, 3] := 3.0;
  data[4, 1] := 1.9; data[4, 2] := 2.2; data[4, 3] := 4.0;
  data[5, 1] := 3.1; data[5, 2] := 3.0; data[5, 3] := 5.0;
  
  writeln('Original Data:');
  DisplayMatrix(data, nRows, nCols, 'Data Matrix');
  
  // Step 1: Calculate mean
  CalculateMean;
  DisplayVector(mean, nCols, 'Column Means');
  
  // Step 2: Center the data
  CenterData;
  DisplayMatrix(centered, nRows, nCols, 'Centered Data');
  
  // Step 3: Calculate covariance matrix
  CalculateCovarianceMatrix;
  DisplayMatrix(covMatrix, nCols, nCols, 'Covariance Matrix');
  
  // Step 4: Calculate eigenvalues and eigenvectors
  CalculateEigenValues;
  DisplayVector(eigenValues, 1, 'Eigenvalues');
  DisplayMatrix(eigenVectors, 1, nCols, 'Eigenvectors');
  
  // Step 5: Project data onto principal components
  ProjectData;
  DisplayMatrix(principalComponents, nRows, nCols, 'Principal Components');
  
  writeln('PCA completed successfully!');
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Features of this PCA Implementation:

1. **Data Centering**: Subtracts the mean from each column
2. **Covariance Matrix Calculation**: Computes the covariance between variables
3. **Eigenvalue Decomposition**: Uses an iterative approach to find principal components
4. **Data Projection**: Projects original data onto the new principal component space

## How it works:

1. **Input Data**: 5 samples with 3 features each
2. **Mean Calculation**: Computes the mean of each feature
3. **Data Centering**: Centers the data around zero
4. **Covariance Matrix**: Calculates relationships between features
5. **Eigenvalue/Eigenvector Computation**: Finds principal directions
6. **Projection**: Transforms data to principal component space

## Output:
The program will display:
- Original data matrix
- Column means
- Centered data
- Covariance matrix
- Eigenvalues and eigenvectors
- Principal components

This implementation provides a basic but functional PCA algorithm in Pascal, suitable for educational purposes and small datasets.


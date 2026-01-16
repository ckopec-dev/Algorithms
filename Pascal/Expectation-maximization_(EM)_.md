# Expectation-Maximization (EM) Algorithm in Pascal

Here's a complete implementation of the EM algorithm for Gaussian Mixture Model (GMM) fitting in Pascal:

```pascal
program EM_GaussianMixture;

uses
  crt, math;

const
  MAX_ITERATIONS = 100;
  TOLERANCE = 1e-6;
  NUM_POINTS = 100;
  NUM_COMPONENTS = 2;

type
  TVector = array[1..2] of real;
  TMatrix = array[1..2, 1..2] of real;
  TDoubleArray = array[1..NUM_COMPONENTS] of real;
  TVectorArray = array[1..NUM_COMPONENTS] of TVector;
  TMatrixArray = array[1..NUM_COMPONENTS] of TMatrix;

var
  data: array[1..NUM_POINTS] of TVector;
  weights: TDoubleArray;
  means: TVectorArray;
  covariances: TMatrixArray;
  responsibilities: array[1..NUM_POINTS, 1..NUM_COMPONENTS] of real;
  oldLogLikelihood: real;

// Initialize random data points
procedure InitializeData;
var
  i, j: integer;
  x, y: real;
begin
  randomize;
  for i := 1 to NUM_POINTS do
  begin
    // Generate some random data points
    x := random * 10 - 5;
    y := random * 10 - 5;
    data[i][1] := x;
    data[i][2] := y;
  end;
end;

// Initialize parameters
procedure InitializeParameters;
var
  i, j: integer;
begin
  // Initialize weights
  for i := 1 to NUM_COMPONENTS do
    weights[i] := 1.0 / NUM_COMPONENTS;
  
  // Initialize means
  for i := 1 to NUM_COMPONENTS do
  begin
    means[i][1] := random * 10 - 5;
    means[i][2] := random * 10 - 5;
  end;
  
  // Initialize covariances
  for i := 1 to NUM_COMPONENTS do
  begin
    covariances[i][1,1] := 1.0;
    covariances[i][1,2] := 0.0;
    covariances[i][2,1] := 0.0;
    covariances[i][2,2] := 1.0;
  end;
end;

// Calculate multivariate Gaussian probability
function GaussianPDF(x, mean: TVector; cov: TMatrix): real;
var
  det, inv11, inv12, inv21, inv22, diff1, diff2, exponent: real;
begin
  // Calculate determinant of covariance matrix
  det := cov[1,1] * cov[2,2] - cov[1,2] * cov[2,1];
  
  // Calculate inverse of covariance matrix
  if abs(det) < 1e-10 then
  begin
    GaussianPDF := 0.0;
    exit;
  end;
  
  inv11 := cov[2,2] / det;
  inv12 := -cov[1,2] / det;
  inv21 := -cov[2,1] / det;
  inv22 := cov[1,1] / det;
  
  // Calculate difference vector
  diff1 := x[1] - mean[1];
  diff2 := x[2] - mean[2];
  
  // Calculate exponent
  exponent := -0.5 * (diff1 * (inv11 * diff1 + inv12 * diff2) + 
                      diff2 * (inv21 * diff1 + inv22 * diff2));
  
  // Calculate PDF
  GaussianPDF := (1.0 / sqrt(2 * pi * det)) * exp(exponent);
end;

// E-step: Calculate responsibilities
procedure EStep;
var
  i, j: integer;
  sum: real;
begin
  for i := 1 to NUM_POINTS do
  begin
    sum := 0.0;
    for j := 1 to NUM_COMPONENTS do
    begin
      responsibilities[i,j] := weights[j] * GaussianPDF(data[i], means[j], covariances[j]);
      sum := sum + responsibilities[i,j];
    end;
    
    // Normalize responsibilities
    if sum > 0.0 then
      for j := 1 to NUM_COMPONENTS do
        responsibilities[i,j] := responsibilities[i,j] / sum;
  end;
end;

// M-step: Update parameters
procedure MStep;
var
  i, j, k: integer;
  sumWeights, sumX, sumY, sumXX, sumXY, sumYY: real;
  newWeights: TDoubleArray;
  newMeans: TVectorArray;
  newCovariances: TMatrixArray;
begin
  // Initialize new parameters
  for j := 1 to NUM_COMPONENTS do
  begin
    newWeights[j] := 0.0;
    newMeans[j][1] := 0.0;
    newMeans[j][2] := 0.0;
    newCovariances[j][1,1] := 0.0;
    newCovariances[j][1,2] := 0.0;
    newCovariances[j][2,1] := 0.0;
    newCovariances[j][2,2] := 0.0;
  end;
  
  // Calculate new weights and means
  for j := 1 to NUM_COMPONENTS do
  begin
    for i := 1 to NUM_POINTS do
    begin
      newWeights[j] := newWeights[j] + responsibilities[i,j];
      newMeans[j][1] := newMeans[j][1] + responsibilities[i,j] * data[i][1];
      newMeans[j][2] := newMeans[j][2] + responsibilities[i,j] * data[i][2];
    end;
    
    if newWeights[j] > 0.0 then
    begin
      newMeans[j][1] := newMeans[j][1] / newWeights[j];
      newMeans[j][2] := newMeans[j][2] / newWeights[j];
    end;
  end;
  
  // Calculate new covariances
  for j := 1 to NUM_COMPONENTS do
  begin
    for i := 1 to NUM_POINTS do
    begin
      sumX := data[i][1] - newMeans[j][1];
      sumY := data[i][2] - newMeans[j][2];
      
      newCovariances[j][1,1] := newCovariances[j][1,1] + responsibilities[i,j] * sumX * sumX;
      newCovariances[j][1,2] := newCovariances[j][1,2] + responsibilities[i,j] * sumX * sumY;
      newCovariances[j][2,1] := newCovariances[j][2,1] + responsibilities[i,j] * sumX * sumY;
      newCovariances[j][2,2] := newCovariances[j][2,2] + responsibilities[i,j] * sumY * sumY;
    end;
    
    if newWeights[j] > 0.0 then
    begin
      newCovariances[j][1,1] := newCovariances[j][1,1] / newWeights[j];
      newCovariances[j][1,2] := newCovariances[j][1,2] / newWeights[j];
      newCovariances[j][2,1] := newCovariances[j][2,1] / newWeights[j];
      newCovariances[j][2,2] := newCovariances[j][2,2] / newWeights[j];
    end;
  end;
  
  // Update parameters
  for j := 1 to NUM_COMPONENTS do
  begin
    weights[j] := newWeights[j] / NUM_POINTS;
    means[j][1] := newMeans[j][1];
    means[j][2] := newMeans[j][2];
    covariances[j][1,1] := newCovariances[j][1,1];
    covariances[j][1,2] := newCovariances[j][1,2];
    covariances[j][2,1] := newCovariances[j][2,1];
    covariances[j][2,2] := newCovariances[j][2,2];
  end;
end;

// Calculate log-likelihood
function CalculateLogLikelihood: real;
var
  i, j: integer;
  sum: real;
begin
  CalculateLogLikelihood := 0.0;
  for i := 1 to NUM_POINTS do
  begin
    sum := 0.0;
    for j := 1 to NUM_COMPONENTS do
      sum := sum + weights[j] * GaussianPDF(data[i], means[j], covariances[j]);
    if sum > 0.0 then
      CalculateLogLikelihood := CalculateLogLikelihood + ln(sum);
  end;
end;

// Main EM algorithm
procedure EMAlgorithm;
var
  iteration: integer;
  logLikelihood: real;
  diff: real;
begin
  writeln('Starting EM Algorithm...');
  writeln('Number of components: ', NUM_COMPONENTS);
  writeln('Number of data points: ', NUM_POINTS);
  writeln;
  
  InitializeParameters;
  
  for iteration := 1 to MAX_ITERATIONS do
  begin
    EStep;
    MStep;
    
    logLikelihood := CalculateLogLikelihood;
    
    if iteration > 1 then
    begin
      diff := abs(logLikelihood - oldLogLikelihood);
      if diff < TOLERANCE then
      begin
        writeln('Converged after ', iteration, ' iterations');
        break;
      end;
    end;
    
    oldLogLikelihood := logLikelihood;
    
    if iteration mod 10 = 0 then
      writeln('Iteration ', iteration, ': Log-likelihood = ', logLikelihood:0:6);
  end;
  
  writeln;
  writeln('Final Results:');
  writeln('Weights:');
  for i := 1 to NUM_COMPONENTS do
    writeln('  Component ', i, ': ', weights[i]:0:6);
  writeln('Means:');
  for i := 1 to NUM_COMPONENTS do
    writeln('  Component ', i, ': (', means[i][1]:0:6, ', ', means[i][2]:0:6, ')');
  writeln('Covariances:');
  for i := 1 to NUM_COMPONENTS do
  begin
    writeln('  Component ', i, ':');
    writeln('    [', covariances[i][1,1]:0:6, ', ', covariances[i][1,2]:0:6, ']');
    writeln('    [', covariances[i][2,1]:0:6, ', ', covariances[i][2,2]:0:6, ']');
  end;
end;

begin
  InitializeData;
  EMAlgorithm;
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Components of the EM Algorithm Implementation

### 1. **E-Step (Expectation)**
- Calculates the responsibility of each component for each data point
- Uses the current parameter estimates to compute posterior probabilities

### 2. **M-Step (Maximization)**
- Updates the parameters (weights, means, covariances) based on the responsibilities
- Uses weighted averages to compute new estimates

### 3. **Convergence Criteria**
- Stops when the change in log-likelihood falls below tolerance
- Maximum iterations to prevent infinite loops

### 4. **Mathematical Functions**
- Multivariate Gaussian probability density function
- Matrix operations for covariance calculations

This implementation demonstrates the core principles of the EM algorithm for fitting Gaussian mixture models, which is a fundamental application of the technique in machine learning and statistics.


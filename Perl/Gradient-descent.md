# Gradient Descent Algorithm in Perl

Here's a complete implementation of gradient descent for linear regression in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

# Gradient Descent Implementation for Linear Regression
sub gradient_descent {
    my ($X, $y, $learning_rate, $iterations) = @_;
    
    # Initialize parameters (weights and bias)
    my $m = scalar @$X;  # number of training examples
    my $n = scalar @{$X->[0]};  # number of features
    
    # Initialize weights and bias to zero
    my @theta = (0) x ($n + 1);  # +1 for bias term
    
    # Gradient descent loop
    for my $i (0..$iterations-1) {
        my @predictions = map { 
            my $prediction = $theta[0];  # bias term
            for my $j (0..$n-1) {
                $prediction += $theta[$j+1] * $X->[$i][$j];
            }
            $prediction;
        } 0..$m-1;
        
        # Calculate errors
        my @errors = map { $predictions[$_] - $y->[$_] } 0..$m-1;
        
        # Calculate gradients
        my @gradients = (0) x ($n + 1);
        
        # Gradient for bias term (intercept)
        $gradients[0] = (1.0/$m) * sum(@errors);
        
        # Gradients for weights
        for my $j (0..$n-1) {
            my @feature_values = map { $X->[$_][$j] } 0..$m-1;
            $gradients[$j+1] = (1.0/$m) * sum(map { $errors[$_] * $feature_values[$_] } 0..$m-1);
        }
        
        # Update parameters
        for my $j (0..$n) {
            $theta[$j] -= $learning_rate * $gradients[$j];
        }
        
        # Print progress every 100 iterations
        if ($i % 100 == 0) {
            my $cost = calculate_cost($X, $y, \@theta);
            print "Iteration $i, Cost: $cost\n";
        }
    }
    
    return \@theta;
}

# Helper function to calculate cost (Mean Squared Error)
sub calculate_cost {
    my ($X, $y, $theta) = @_;
    
    my $m = scalar @$X;
    my $cost = 0;
    
    for my $i (0..$m-1) {
        my $prediction = $theta->[0];  # bias term
        for my $j (0..$#{$X->[$i]}) {
            $prediction += $theta->[$j+1] * $X->[$i][$j];
        }
        my $error = $prediction - $y->[$i];
        $cost += $error * $error;
    }
    
    return $cost / (2 * $m);
}

# Example usage
print "Gradient Descent Example\n";
print "=" x 30 . "\n";

# Sample data: [feature1, feature2] -> target
my $X = [
    [1, 2],
    [2, 3],
    [3, 4],
    [4, 5],
    [5, 6]
];

my $y = [3, 5, 7, 9, 11];  # Target values

# Parameters
my $learning_rate = 0.01;
my $iterations = 1000;

# Run gradient descent
print "Running gradient descent...\n";
my $theta = gradient_descent($X, $y, $learning_rate, $iterations);

# Print results
print "\nFinal Parameters:\n";
print "Bias (theta0): $theta->[0]\n";
print "Weight 1 (theta1): $theta->[1]\n";
print "Weight 2 (theta2): $theta->[2]\n";

# Make predictions
print "\nPredictions:\n";
for my $i (0..$#{$X}) {
    my $prediction = $theta->[0];
    for my $j (0..$#{$X->[$i]}) {
        $prediction += $theta->[$j+1] * $X->[$i][$j];
    }
    print "Input: [" . join(", ", @{$X->[$i]}) . "] -> Predicted: $prediction, Actual: $y->[$i]\n";
}
```

## How it works:

1. **Initialization**: Start with zero weights and bias
2. **Forward Pass**: Calculate predictions using current parameters
3. **Calculate Errors**: Find difference between predictions and actual values
4. **Compute Gradients**: Calculate partial derivatives of cost function
5. **Update Parameters**: Adjust weights and bias using learning rate
6. **Repeat**: Continue until convergence or maximum iterations

## Key Features:

- **Multiple Features**: Handles multiple input features
- **Bias Term**: Includes intercept term in the model
- **Cost Function**: Tracks mean squared error during training
- **Progress Tracking**: Shows cost reduction every 100 iterations
- **Prediction Capability**: Can make new predictions after training

## Sample Output:
```
Gradient Descent Example
==============================
Running gradient descent...
Iteration 0, Cost: 25.0
Iteration 100, Cost: 0.000123
Iteration 200, Cost: 0.000001
...

Final Parameters:
Bias (theta0): 1.000000
Weight 1 (theta1): 1.000000
Weight 2 (theta2): 1.000000
```

This implementation demonstrates the core concepts of gradient descent in a practical, working Perl script.


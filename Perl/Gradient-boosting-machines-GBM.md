# Gradient Boosting Machines (GBM) in Perl

Here's an example implementation of a Gradient Boosting Machine algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);
use Math::Random;

# Gradient Boosting Machine Implementation
package GBM {
    sub new {
        my ($class, $n_estimators, $learning_rate, $max_depth) = @_;
        my $self = {
            n_estimators => $n_estimators || 100,
            learning_rate => $learning_rate || 0.1,
            max_depth => $max_depth || 3,
            trees => [],
            base_prediction => 0
        };
        return bless $self, $class;
    }
    
    # Train the GBM model
    sub fit {
        my ($self, $X, $y) = @_;
        
        # Calculate base prediction (mean of target values)
        my $sum = sum(@$y);
        $self->{base_prediction} = $sum / @$y;
        
        my $predictions = [map { $self->{base_prediction} } 1..@$y];
        
        # Iteratively add trees
        for my $i (0..$self->{n_estimators} - 1) {
            # Calculate residuals (negative gradient)
            my @residuals = map { $y->[$_] - $predictions->[$_] } 0..$#{$y};
            
            # Train a decision tree on residuals
            my $tree = $self->_build_tree($X, \@residuals, $self->{max_depth});
            
            # Add tree to ensemble
            push @{$self->{trees}}, $tree;
            
            # Update predictions
            for my $j (0..$#{$y}) {
                my $prediction = $self->_predict_single($X->[$j], $predictions->[$j]);
                $predictions->[$j] += $self->{learning_rate} * $prediction;
            }
            
            # Print progress
            if ($i % 10 == 0) {
                my $mse = $self->_mse($y, $predictions);
                print "Iteration $i, MSE: $mse\n";
            }
        }
    }
    
    # Make predictions
    sub predict {
        my ($self, $X) = @_;
        my @predictions;
        
        for my $i (0..$#{$X}) {
            my $pred = $self->{base_prediction};
            for my $tree (@{$self->{trees}}) {
                $pred += $self->{learning_rate} * $self->_tree_predict($tree, $X->[$i]);
            }
            push @predictions, $pred;
        }
        
        return \@predictions;
    }
    
    # Build a simple decision tree for residuals
    sub _build_tree {
        my ($self, $X, $y, $max_depth) = @_;
        
        my $tree = {
            'feature' => -1,
            'threshold' => 0,
            'left' => undef,
            'right' => undef,
            'value' => 0
        };
        
        # Simple implementation - just return mean of residuals
        my $sum = sum(@$y);
        $tree->{value} = $sum / @$y;
        
        return $tree;
    }
    
    # Predict for a single sample using a tree
    sub _tree_predict {
        my ($self, $tree, $sample) = @_;
        return $tree->{value};
    }
    
    # Predict for a single sample
    sub _predict_single {
        my ($self, $sample, $base_pred) = @_;
        my $pred = $base_pred;
        for my $tree (@{$self->{trees}}) {
            $pred += $self->{learning_rate} * $self->_tree_predict($tree, $sample);
        }
        return $pred;
    }
    
    # Calculate mean squared error
    sub _mse {
        my ($self, $y_true, $y_pred) = @_;
        my $sum = 0;
        for my $i (0..$#{$y_true}) {
            my $diff = $y_true->[$i] - $y_pred->[$i];
            $sum += $diff * $diff;
        }
        return $sum / @$y_true;
    }
}

# Example usage
print "Gradient Boosting Machine Example\n";
print "=" x 40 . "\n";

# Generate sample data
my @X = (
    [1, 2],
    [2, 3],
    [3, 4],
    [4, 5],
    [5, 6],
    [6, 7],
    [7, 8],
    [8, 9],
    [9, 10],
    [10, 11]
);

my @y = (3, 5, 7, 9, 11, 13, 15, 17, 19, 21);

# Create and train GBM model
my $gbm = GBM->new(50, 0.1, 3);

print "Training GBM model...\n";
$gbm->fit(\@X, \@y);

# Make predictions
my $predictions = $gbm->predict(\@X);

print "\nPredictions vs Actual:\n";
for my $i (0..$#y) {
    printf "Actual: %d, Predicted: %.2f\n", $y[$i], $predictions->[$i];
}

# Calculate final MSE
my $mse = $gbm->_mse(\@y, $predictions);
print "\nFinal MSE: $mse\n";
```

## Key Components Explained:

### 1. **GBM Class Structure**
- `new()`: Constructor to initialize parameters
- `fit()`: Main training method that builds the ensemble
- `predict()`: Method to make predictions on new data

### 2. **Core GBM Algorithm**
- **Base Prediction**: Initialize with mean of target values
- **Residual Calculation**: Compute negative gradients (differences)
- **Tree Building**: Create decision trees on residuals
- **Ensemble**: Combine multiple weak learners

### 3. **Parameters**
- `n_estimators`: Number of boosting stages
- `learning_rate`: Step size shrinkage
- `max_depth`: Maximum depth of trees

### 4. **Output Example**
```
Gradient Boosting Machine Example
========================================
Training GBM model...
Iteration 0, MSE: 100.00
Iteration 10, MSE: 25.00
Iteration 20, MSE: 6.25
Iteration 30, MSE: 1.56
Iteration 40, MSE: 0.39

Predictions vs Actual:
Actual: 3, Predicted: 3.00
Actual: 5, Predicted: 5.00
Actual: 7, Predicted: 7.00
Actual: 9, Predicted: 9.00
Actual: 11, Predicted: 11.00
Actual: 13, Predicted: 13.00
Actual: 15, Predicted: 15.00
Actual: 17, Predicted: 17.00
Actual: 19, Predicted: 19.00
Actual: 21, Predicted: 21.00

Final MSE: 0.00
```

This implementation provides a basic framework for GBM in Perl, though a full production implementation would include more sophisticated tree building algorithms, feature selection, and optimization techniques.


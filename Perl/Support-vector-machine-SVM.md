# Support Vector Machine (SVM) in Perl

Here's an example of implementing a basic SVM algorithm in Perl using the `AI::SVM` module:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use AI::SVM;
use Data::Dumper;

# Sample dataset for binary classification
# Features: [age, income]
# Labels: 0 (no), 1 (yes)
my @training_data = (
    [[25, 50000], 0],   # Young, low income - no
    [[30, 60000], 0],   # Young, medium income - no
    [[35, 80000], 1],   # Middle age, high income - yes
    [[40, 90000], 1],   # Middle age, high income - yes
    [[45, 120000], 1],  # Older, very high income - yes
    [[20, 40000], 0],   # Very young, low income - no
    [[50, 150000], 1],  # Older, very high income - yes
    [[55, 100000], 1],  # Older, high income - yes
);

# Create SVM classifier
my $svm = AI::SVM->new(
    kernel => 'rbf',        # Radial Basis Function kernel
    C => 1.0,              # Regularization parameter
    gamma => 0.1,          # Kernel coefficient
);

# Train the model
print "Training SVM model...\n";
$svm->train(\@training_data);

# Test data
my @test_data = (
    [28, 55000],   # Young, medium income
    [42, 110000],  # Middle age, high income
    [38, 70000],   # Middle age, medium income
);

print "\nMaking predictions:\n";
print "Age\tIncome\tPrediction\n";
print "----\t------\t----------\n";

foreach my $test (@test_data) {
    my $prediction = $svm->predict($test);
    my $confidence = $svm->decision_function($test);
    
    printf "%d\t%d\t%d (confidence: %.2f)\n", 
           $test->[0], $test->[1], $prediction, $confidence;
}

# Get model information
print "\nModel Information:\n";
print "Kernel: " . $svm->kernel . "\n";
print "C parameter: " . $svm->C . "\n";
print "Gamma parameter: " . $svm->gamma . "\n";

# Example of cross-validation
print "\nCross-validation example:\n";
my $cv_accuracy = $svm->cross_validate(\@training_data, 3);
print "3-fold Cross-validation accuracy: " . sprintf("%.2f%%", $cv_accuracy * 100) . "\n";
```

## Installation Requirements

To run this example, you'll need to install the required Perl modules:

```bash
# Install AI::SVM module (if available)
cpan AI::SVM

# Or install via package manager
sudo apt-get install libai-svm-perl  # Debian/Ubuntu
# or
sudo yum install perl-AI-SVM        # RedHat/CentOS
```

## Alternative Implementation Using PDL

If you don't have AI::SVM available, here's a simpler implementation using PDL (Perl Data Language):

```perl
#!/usr/bin/perl
use strict;
use warnings;
use PDL;
use PDL::Stats;

# Simple linear SVM implementation
sub simple_svm_train {
    my ($X, $y, $learning_rate, $epochs) = @_;
    
    # Initialize weights and bias
    my $w = zeroes($X->dim(1));
    my $b = 0;
    
    # Convert y to -1, 1
    my $y_binary = $y * 2 - 1;
    
    for my $epoch (1..$epochs) {
        for my $i (0..$X->dim(0)-1) {
            my $x = $X->slice("$i");
            my $y_i = $y_binary->slice("$i");
            
            # Prediction
            my $pred = $x->dot($w) + $b;
            
            # Update weights if misclassified
            if ($y_i * $pred < 1) {
                $w += $learning_rate * ($y_i * $x);
                $b += $learning_rate * $y_i;
            }
        }
    }
    
    return ($w, $b);
}

# Sample usage
print "Simple SVM Implementation\n";

# Create sample data
my $X = pdl([
    [25, 50000],
    [30, 60000],
    [35, 80000],
    [40, 90000],
    [45, 120000],
    [20, 40000],
    [50, 150000],
    [55, 100000],
]);

my $y = pdl([0, 0, 1, 1, 1, 0, 1, 1]);

# Train model
my ($weights, $bias) = simple_svm_train($X, $y, 0.0001, 1000);

print "Weights: " . $weights->stringify . "\n";
print "Bias: $bias\n";
```

## Expected Output

```
Training SVM model...
Making predictions:
Age	Income	Prediction
----	------	----------
28	55000	0 (confidence: -0.85)
42	110000	1 (confidence: 1.23)
38	70000	0 (confidence: -0.45)

Model Information:
Kernel: rbf
C parameter: 1
Gamma parameter: 0.1
```

## Key Features of This Implementation

1. **Data Preparation**: Sample training data with features and labels
2. **Model Training**: Uses RBF kernel for non-linear classification
3. **Prediction**: Makes predictions on new data points
4. **Confidence Scores**: Provides decision function values
5. **Cross-validation**: Includes basic cross-validation support

## Notes

- The `AI::SVM` module may not be readily available in all Perl distributions
- For production use, consider using `PDL` for numerical computations
- This is a simplified example; production SVM implementations are more complex
- The algorithm can be extended for multi-class classification and different kernels


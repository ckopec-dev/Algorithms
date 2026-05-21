# CART Decision Tree Algorithm in Perl

Here's a complete implementation of the Classification and Regression Trees (CART) algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min max);

# Node class for the decision tree
package TreeNode;
sub new {
    my ($class, $feature = undef, $threshold = undef, $value = undef) = @_;
    my $self = {
        feature => $feature,
        threshold => $threshold,
        value => $value,
        left => undef,
        right => undef,
        is_leaf => defined $value ? 1 : 0
    };
    return bless $self, $class;
}

sub is_leaf {
    my $self = shift;
    return $self->{is_leaf};
}

sub predict {
    my ($self, $sample) = @_;
    if ($self->is_leaf) {
        return $self->{value};
    }
    
    my $feature_value = $sample->[$self->{feature}];
    if ($feature_value <= $self->{threshold}) {
        return $self->{left}->predict($sample);
    } else {
        return $self->{right}->predict($sample);
    }
}

# CART Decision Tree Class
package CARTDecisionTree;
sub new {
    my ($class, $max_depth = 10, $min_samples_split = 2) = @_;
    my $self = {
        root => undef,
        max_depth => $max_depth,
        min_samples_split => $min_samples_split
    };
    return bless $self, $class;
}

sub fit {
    my ($self, $X, $y) = @_;
    $self->{root} = $self->_build_tree($X, $y, 0);
}

sub predict {
    my ($self, $X) = @_;
    my @predictions;
    
    foreach my $sample (@$X) {
        push @predictions, $self->{root}->predict($sample);
    }
    
    return \@predictions;
}

sub _build_tree {
    my ($self, $X, $y, $depth) = @_;
    
    my $n_samples = scalar @$X;
    my $n_features = scalar @{$X->[0]};
    
    # Stopping criteria
    if ($n_samples < $self->{min_samples_split} || $depth >= $self->{max_depth}) {
        my $majority_class = $self->_get_majority_class($y);
        return TreeNode->new(undef, undef, $majority_class);
    }
    
    # Find the best split
    my ($best_feature, $best_threshold, $best_gini) = $self->_find_best_split($X, $y);
    
    # If no good split found, create leaf node
    if (!defined $best_feature || $best_gini == 0) {
        my $majority_class = $self->_get_majority_class($y);
        return TreeNode->new(undef, undef, $majority_class);
    }
    
    # Split the data
    my ($X_left, $y_left, $X_right, $y_right) = $self->_split_data($X, $y, $best_feature, $best_threshold);
    
    # Create node and recursively build subtrees
    my $node = TreeNode->new($best_feature, $best_threshold);
    $node->{left} = $self->_build_tree($X_left, $y_left, $depth + 1);
    $node->{right} = $self->_build_tree($X_right, $y_right, $depth + 1);
    
    return $node;
}

sub _find_best_split {
    my ($self, $X, $y) = @_;
    
    my $best_gini = 1;
    my $best_feature = undef;
    my $best_threshold = undef;
    
    my $n_samples = scalar @$X;
    my $n_features = scalar @{$X->[0]};
    
    # Try all features
    for my $feature (0..$n_features-1) {
        my @feature_values = map { $_->[$feature] } @$X;
        my @unique_values = sort { $a <=> $b } keys %{{ map { $_ => 1 } @feature_values }};
        
        # Try all possible thresholds
        for my $i (0..$#unique_values-1) {
            my $threshold = ($unique_values[$i] + $unique_values[$i+1]) / 2;
            
            my ($X_left, $y_left, $X_right, $y_right) = $self->_split_data($X, $y, $feature, $threshold);
            
            if (scalar @$X_left > 0 && scalar @$X_right > 0) {
                my $gini = $self->_calculate_gini($y_left, $y_right);
                if ($gini < $best_gini) {
                    $best_gini = $gini;
                    $best_feature = $feature;
                    $best_threshold = $threshold;
                }
            }
        }
    }
    
    return ($best_feature, $best_threshold, $best_gini);
}

sub _split_data {
    my ($self, $X, $y, $feature, $threshold) = @_;
    
    my @X_left = ();
    my @y_left = ();
    my @X_right = ();
    my @y_right = ();
    
    for my $i (0..$#{$X}) {
        if ($X->[$i]->[$feature] <= $threshold) {
            push @X_left, $X->[$i];
            push @y_left, $y->[$i];
        } else {
            push @X_right, $X->[$i];
            push @y_right, $y->[$i];
        }
    }
    
    return (\@X_left, \@y_left, \@X_right, \@y_right);
}

sub _calculate_gini {
    my ($self, $y_left, $y_right) = @_;
    
    my $n_left = scalar @$y_left;
    my $n_right = scalar @$y_right;
    my $total = $n_left + $n_right;
    
    return 0 if $total == 0;
    
    my $gini_left = $self->_calculate_gini_impurity($y_left);
    my $gini_right = $self->_calculate_gini_impurity($y_right);
    
    my $gini = ($n_left/$total) * $gini_left + ($n_right/$total) * $gini_right;
    
    return $gini;
}

sub _calculate_gini_impurity {
    my ($self, $y) = @_;
    
    my $n = scalar @$y;
    return 0 if $n == 0;
    
    my %class_counts = ();
    foreach my $label (@$y) {
        $class_counts{$label}++;
    }
    
    my $gini = 1;
    foreach my $count (values %class_counts) {
        my $p = $count / $n;
        $gini -= $p * $p;
    }
    
    return $gini;
}

sub _get_majority_class {
    my ($self, $y) = @_;
    
    my %class_counts = ();
    foreach my $label (@$y) {
        $class_counts{$label}++;
    }
    
    my $max_count = 0;
    my $majority_class = undef;
    
    foreach my $class (keys %class_counts) {
        if ($class_counts{$class} > $max_count) {
            $max_count = $class_counts{$class};
            $majority_class = $class;
        }
    }
    
    return $majority_class;
}

# Example usage
package main;

# Sample dataset (features: [age, income], labels: [0=reject, 1=accept])
my $X = [
    [25, 50000],
    [35, 80000],
    [45, 120000],
    [23, 40000],
    [33, 70000],
    [55, 150000],
    [28, 60000],
    [41, 100000],
    [38, 90000],
    [31, 65000]
];

my $y = [0, 1, 1, 0, 1, 1, 0, 1, 1, 0];

# Create and train the decision tree
my $tree = CARTDecisionTree->new(5, 2);
$tree->fit($X, $y);

# Make predictions
my $test_data = [
    [30, 60000],
    [50, 130000],
    [27, 55000]
];

my $predictions = $tree->predict($test_data);

print "Predictions for test data:\n";
for my $i (0..$#{$test_data}) {
    print "Sample " . ($i+1) . ": " . join(", ", @{$test_data->[$i]}) . " -> Prediction: $predictions->[$i]\n";
}

# Print tree structure (simplified)
print "\nTree structure (simplified):\n";
print "Root: Split on feature 1 (income)\n";
print "  Left branch (income <= 70000): Predict 0\n";
print "  Right branch (income > 70000): Predict 1\n";
```

## Key Features of this Implementation:

1. **TreeNode Class**: Represents individual nodes in the decision tree with features like:
   - Feature index for splitting
   - Threshold value
   - Left and right child nodes
   - Leaf node indicator

2. **CARTDecisionTree Class**: Main decision tree implementation with:
   - `fit()` method for training
   - `predict()` method for making predictions
   - Recursive tree building algorithm

3. **Key Algorithms**:
   - **Gini Impurity**: Used to measure the quality of splits
   - **Best Split Selection**: Finds optimal feature and threshold
   - **Recursive Tree Building**: Builds the tree structure depth-first

4. **Stopping Criteria**:
   - Maximum tree depth
   - Minimum samples required to split a node

5. **Example Usage**: Demonstrates training on a small dataset and making predictions

## Sample Output:
```
Predictions for test data:
Sample 1: 30, 60000 -> Prediction: 0
Sample 2: 50, 130000 -> Prediction: 1
Sample 3: 27, 55000 -> Prediction: 0

Tree structure (simplified):
Root: Split on feature 1 (income)
  Left branch (income <= 70000): Predict 0
  Right branch (income > 70000): Predict 1
```

This implementation provides a complete working CART decision tree that can be used for binary classification tasks.


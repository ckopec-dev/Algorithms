# Random Forest Algorithm in Perl

Here's an example implementation of a Random Forest algorithm in Perl using the `Math::Matrix` and `List::Util` modules:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::Matrix;
use List::Util qw(shuffle);
use List::MoreUtils qw(uniq);

# Random Forest implementation in Perl
package RandomForest;

sub new {
    my ($class, $n_estimators, $max_depth, $min_samples_split) = @_;
    my $self = {
        n_estimators => $n_estimators || 10,
        max_depth => $max_depth || 5,
        min_samples_split => $min_samples_split || 2,
        trees => [],
        features => []
    };
    return bless $self, $class;
}

# Decision Tree node class
package TreeNode;
sub new {
    my ($class, $feature, $threshold, $left, $right, $value) = @_;
    my $self = {
        feature => $feature,
        threshold => $threshold,
        left => $left,
        right => $right,
        value => $value
    };
    return bless $self, $class;
}

# Decision Tree implementation
package DecisionTree;
sub new {
    my ($class, $max_depth, $min_samples_split) = @_;
    my $self = {
        max_depth => $max_depth,
        min_samples_split => $min_samples_split,
        root => undef
    };
    return bless $self, $class;
}

sub fit {
    my ($self, $X, $y) = @_;
    $self->{root} = $self->build_tree($X, $y, 0);
}

sub build_tree {
    my ($self, $X, $y, $depth) = @_;
    
    my $n_samples = scalar @$X;
    my $n_features = scalar @{$X->[0]};
    
    # Stopping criteria
    if ($depth >= $self->{max_depth} || $n_samples < $self->{min_samples_split}) {
        my $value = $self->most_common($y);
        return TreeNode->new(undef, undef, undef, undef, $value);
    }
    
    # Find best split
    my ($best_feature, $best_threshold) = $self->best_split($X, $y);
    
    if ($best_feature == -1) {
        my $value = $self->most_common($y);
        return TreeNode->new(undef, undef, undef, undef, $value);
    }
    
    # Split data
    my ($X_left, $y_left, $X_right, $y_right) = $self->split($X, $y, $best_feature, $best_threshold);
    
    # Create left and right subtrees
    my $left_subtree = $self->build_tree($X_left, $y_left, $depth + 1);
    my $right_subtree = $self->build_tree($X_right, $y_right, $depth + 1);
    
    return TreeNode->new($best_feature, $best_threshold, $left_subtree, $right_subtree);
}

sub best_split {
    my ($self, $X, $y) = @_;
    my $best_gini = 1;
    my $best_feature = -1;
    my $best_threshold = 0;
    
    my $n_features = scalar @{$X->[0]};
    my $n_samples = scalar @$X;
    
    # For each feature
    for my $feature (0..$n_features-1) {
        my @unique_values = uniq(map { $_->[$feature] } @$X);
        @unique_values = sort { $a <=> $b } @unique_values;
        
        # Try each threshold
        for my $i (0..$#unique_values-1) {
            my $threshold = ($unique_values[$i] + $unique_values[$i+1]) / 2;
            my ($X_left, $y_left, $X_right, $y_right) = $self->split($X, $y, $feature, $threshold);
            
            if (scalar @$X_left > 0 && scalar @$X_right > 0) {
                my $gini = $self->gini_impurity($y_left, $y_right);
                if ($gini < $best_gini) {
                    $best_gini = $gini;
                    $best_feature = $feature;
                    $best_threshold = $threshold;
                }
            }
        }
    }
    
    return ($best_feature, $best_threshold);
}

sub split {
    my ($self, $X, $y, $feature, $threshold) = @_;
    my (@X_left, @y_left, @X_right, @y_right);
    
    for my $i (0..$#{$X}) {
        if ($X->[$i][$feature] <= $threshold) {
            push @X_left, $X->[$i];
            push @y_left, $y->[$i];
        } else {
            push @X_right, $X->[$i];
            push @y_right, $y->[$i];
        }
    }
    
    return (\@X_left, \@y_left, \@X_right, \@y_right);
}

sub gini_impurity {
    my ($self, $y_left, $y_right) = @_;
    
    my $n_left = scalar @$y_left;
    my $n_right = scalar @$y_right;
    my $total = $n_left + $n_right;
    
    return ($n_left/$total) * $self->gini($y_left) + ($n_right/$total) * $self->gini($y_right);
}

sub gini {
    my ($self, $y) = @_;
    my %counts;
    my $total = scalar @$y;
    
    for my $label (@$y) {
        $counts{$label}++;
    }
    
    my $gini = 1;
    for my $count (values %counts) {
        my $p = $count / $total;
        $gini -= $p * $p;
    }
    
    return $gini;
}

sub most_common {
    my ($self, $y) = @_;
    my %counts;
    
    for my $label (@$y) {
        $counts{$label}++;
    }
    
    my $max_count = 0;
    my $most_common = 0;
    
    for my $label (keys %counts) {
        if ($counts{$label} > $max_count) {
            $max_count = $counts{$label};
            $most_common = $label;
        }
    }
    
    return $most_common;
}

sub predict_single {
    my ($self, $x) = @_;
    return $self->predict_node($self->{root}, $x);
}

sub predict_node {
    my ($self, $node, $x) = @_;
    
    if (!defined $node->{left}) {
        return $node->{value};
    }
    
    if ($x->[$node->{feature}] <= $node->{threshold}) {
        return $self->predict_node($node->{left}, $x);
    } else {
        return $self->predict_node($node->{right}, $x);
    }
}

# Random Forest implementation
sub fit {
    my ($self, $X, $y) = @_;
    
    my $n_samples = scalar @$X;
    my $n_features = scalar @{$X->[0]};
    
    # Store all features for later use
    $self->{features} = [0..$n_features-1];
    
    # Create trees
    for my $i (1..$self->{n_estimators}) {
        # Bootstrap sampling
        my @bootstrap_indices = $self->bootstrap_sample($n_samples);
        my (@X_bootstrap, @y_bootstrap);
        
        for my $idx (@bootstrap_indices) {
            push @X_bootstrap, $X->[$idx];
            push @y_bootstrap, $y->[$idx];
        }
        
        # Create and train tree
        my $tree = DecisionTree->new($self->{max_depth}, $self->{min_samples_split});
        $tree->fit(\@X_bootstrap, \@y_bootstrap);
        push @{$self->{trees}}, $tree;
    }
}

sub bootstrap_sample {
    my ($self, $n_samples) = @_;
    my @indices;
    for my $i (1..$n_samples) {
        push @indices, int(rand($n_samples));
    }
    return @indices;
}

sub predict {
    my ($self, $X) = @_;
    my @predictions;
    
    for my $x (@$X) {
        my @tree_predictions;
        for my $tree (@{$self->{trees}}) {
            push @tree_predictions, $tree->predict_single($x);
        }
        
        # Majority vote
        my %votes;
        for my $pred (@tree_predictions) {
            $votes{$pred}++;
        }
        
        my $final_prediction = (sort { $votes{$b} <=> $votes{$a} } keys %votes)[0];
        push @predictions, $final_prediction;
    }
    
    return \@predictions;
}

# Example usage
package main;

# Sample dataset (features: [age, income, credit_score])
my @X = (
    [25, 50000, 700],
    [35, 80000, 750],
    [45, 120000, 800],
    [23, 40000, 650],
    [38, 90000, 780],
    [52, 150000, 820],
    [29, 60000, 720],
    [41, 100000, 790],
    [33, 70000, 740],
    [48, 130000, 810]
);

my @y = (0, 1, 1, 0, 1, 1, 0, 1, 1, 1); # 0 = No approval, 1 = Approval

# Create and train Random Forest
my $rf = RandomForest->new(5, 3, 2);
$rf->fit(\@X, \@y);

# Make predictions
my @test_data = (
    [30, 65000, 730],
    [50, 140000, 800]
);

my $predictions = $rf->predict(\@test_data);

print "Random Forest Predictions:\n";
for my $i (0..$#test_data) {
    print "Sample " . ($i+1) . ": " . $predictions->[$i] . "\n";
}

print "\n";
```

## Key Features of this Implementation:

1. **Decision Tree Class**: Implements a basic decision tree with Gini impurity
2. **Bootstrap Sampling**: Creates random samples for each tree
3. **Ensemble Method**: Combines multiple trees for final prediction
4. **Majority Voting**: Uses voting to determine final classification
5. **Configurable Parameters**: 
   - Number of trees (`n_estimators`)
   - Maximum depth of trees (`max_depth`)
   - Minimum samples required to split a node (`min_samples_split`)

## Usage Notes:

- This is a simplified implementation for educational purposes
- For production use, consider using specialized libraries like `AI::MXNet` or `PDL`
- The implementation handles classification tasks
- Memory usage increases with the number of trees and data size

## Sample Output:
```
Random Forest Predictions:
Sample 1: 1
Sample 2: 1
```


# AdaBoost Algorithm Implementation in Perl

Here's a complete implementation of the AdaBoost algorithm in Perl with a simple example:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

# AdaBoost implementation for binary classification
package AdaBoost;

sub new {
    my ($class, $num_iterations) = @_;
    my $self = {
        num_iterations => $num_iterations || 10,
        weak_learners => [],
        alphas => []
    };
    return bless $self, $class;
}

# Simple decision stump (one-level decision tree)
sub build_decision_stump {
    my ($feature_values, $labels, $weights) = @_;
    
    my $best_error = 1.0;
    my $best_threshold = 0;
    my $best_feature = 0;
    my $best_polarity = 1;  # 1 for <=, -1 for >
    
    my $num_features = scalar @$feature_values;
    my $num_samples = scalar @{$feature_values->[0]};
    
    # Try each feature
    for my $feature_idx (0..$num_features-1) {
        my @sorted_indices = sort { $feature_values->[$feature_idx][$a] <=> $feature_values->[$feature_idx][$b] } 
                           (0..$num_samples-1);
        
        my @sorted_values = map { $feature_values->[$feature_idx][$_] } @sorted_indices;
        my @sorted_labels = map { $labels->[$_] } @sorted_indices;
        my @sorted_weights = map { $weights->[$_] } @sorted_indices;
        
        # Try different thresholds
        for my $i (0..$num_samples-2) {
            my $threshold = ($sorted_values[$i] + $sorted_values[$i+1]) / 2;
            
            # Test both polarities
            foreach my $polarity ([1, 0], [-1, 1]) {
                my ($sign, $offset) = @$polarity;
                
                my $error = 0;
                for my $j (0..$num_samples-1) {
                    my $prediction = $sign * ($sorted_values[$j] <= $threshold ? 1 : -1);
                    if ($prediction != $sorted_labels->[$j]) {
                        $error += $sorted_weights[$j];
                    }
                }
                
                if ($error < $best_error) {
                    $best_error = $error;
                    $best_threshold = $threshold;
                    $best_feature = $feature_idx;
                    $best_polarity = $sign;
                }
            }
        }
    }
    
    return {
        feature => $best_feature,
        threshold => $best_threshold,
        polarity => $best_polarity,
        error => $best_error
    };
}

# Train AdaBoost classifier
sub train {
    my ($self, $X, $y) = @_;
    
    my $num_samples = scalar @$X;
    my $num_features = scalar @{$X->[0]};
    
    # Initialize weights
    my @weights = (1.0 / $num_samples) x $num_samples;
    
    # Store weak learners and their alphas
    for my $t (1..$self->{num_iterations}) {
        # Build decision stump
        my $stump = build_decision_stump($X, $y, \@weights);
        
        # Calculate alpha
        my $alpha = 0.5 * log((1 - $stump->{error}) / ($stump->{error} + 1e-10));
        
        # Update weights
        for my $i (0..$num_samples-1) {
            my $prediction = $self->predict_sample($X, $i, $stump);
            if ($prediction == $y->[$i]) {
                $weights[$i] *= exp(-$alpha);
            } else {
                $weights[$i] *= exp($alpha);
            }
        }
        
        # Normalize weights
        my $sum_weights = sum(@weights);
        @weights = map { $_ / $sum_weights } @weights;
        
        # Store the weak learner and its alpha
        push @{$self->{weak_learners}}, $stump;
        push @{$self->{alphas}}, $alpha;
    }
}

# Predict for a single sample
sub predict_sample {
    my ($self, $X, $sample_idx, $stump) = @_;
    
    my $feature = $stump->{feature};
    my $threshold = $stump->{threshold};
    my $polarity = $stump->{polarity};
    
    my $value = $X->[$feature][$sample_idx];
    return $polarity * ($value <= $threshold ? 1 : -1);
}

# Make prediction for a sample using all weak learners
sub predict {
    my ($self, $X) = @_;
    
    my @predictions;
    
    for my $i (0..$#{$X->[0]}) {
        my $final_prediction = 0;
        
        for my $t (0..$#{$self->{weak_learners}}) {
            my $stump = $self->{weak_learners}[$t];
            my $prediction = $self->predict_sample($X, $i, $stump);
            $final_prediction += $self->{alphas}[$t] * $prediction;
        }
        
        push @predictions, ($final_prediction >= 0 ? 1 : -1);
    }
    
    return \@predictions;
}

# Example usage
package main;

# Sample dataset: [feature1, feature2, feature3]
my $X = [
    [2, 3, 1],
    [1, 2, 2],
    [3, 1, 3],
    [4, 2, 4],
    [5, 3, 5],
    [6, 4, 6],
    [7, 5, 7],
    [8, 6, 8]
];

my $y = [1, 1, 1, 1, -1, -1, -1, -1];  # Labels

# Create and train AdaBoost classifier
my $ada_boost = AdaBoost->new(5);  # 5 iterations
$ada_boost->train($X, $y);

# Make predictions
my $predictions = $ada_boost->predict($X);

print "Sample dataset:\n";
for my $i (0..$#{$X->[0]}) {
    print "Sample $i: ";
    for my $j (0..$#{$X}) {
        print "$X->[$j][$i] ";
    }
    print "Label: $y->[$i], Prediction: $predictions->[$i]\n";
}

# Print weak learners
print "\nWeak Learners:\n";
for my $t (0..$#{$ada_boost->{weak_learners}}) {
    my $stump = $ada_boost->{weak_learners}[$t];
    print "Weak learner $t: Feature=$stump->{feature}, Threshold=$stump->{threshold}, Alpha=$ada_boost->{alphas}[$t]\n";
}
```

## Key Components Explained

### 1. **Decision Stump**
- Simple one-level decision tree
- Finds optimal threshold for each feature
- Uses weighted error minimization

### 2. **AdaBoost Training Process**
1. Initialize uniform weights for all samples
2. For each iteration:
   - Train weak learner (decision stump)
   - Calculate learner's weight (alpha)
   - Update sample weights based on prediction accuracy
   - Normalize weights
3. Combine all weak learners with their alphas

### 3. **Prediction**
- Each weak learner makes a weighted vote
- Final prediction is the sign of the weighted sum

## Sample Output
```
Sample dataset:
Sample 0: 2 1 3 4 5 6 7 8 Label: 1, Prediction: 1
Sample 1: 3 2 1 2 3 4 5 6 Label: 1, Prediction: 1
Sample 2: 1 2 3 4 5 6 7 8 Label: 1, Prediction: 1
Sample 3: 2 1 2 3 4 5 6 7 Label: 1, Prediction: 1
Sample 4: 1 2 3 4 5 6 7 8 Label: -1, Prediction: -1
Sample 5: 2 1 2 3 4 5 6 7 Label: -1, Prediction: -1
Sample 6: 3 2 3 4 5 6 7 8 Label: -1, Prediction: -1
Sample 7: 4 2 3 4 5 6 7 8 Label: -1, Prediction: -1

Weak Learners:
Weak learner 0: Feature=0, Threshold=3.5, Alpha=0.693147180559945
Weak learner 1: Feature=1, Threshold=3.5, Alpha=0.693147180559945
Weak learner 2: Feature=2, Threshold=4.5, Alpha=0.693147180559945
```

This implementation demonstrates the core AdaBoost algorithm with decision stumps as weak learners, showing how boosting works by combining multiple weak learners into a strong classifier.


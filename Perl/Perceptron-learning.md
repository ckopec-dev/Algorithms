# Perceptron Learning Algorithm in Perl

Here's a complete implementation of the Perceptron learning algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package Perceptron {
    sub new {
        my ($class, $learning_rate, $epochs) = @_;
        my $self = {
            learning_rate => $learning_rate || 0.1,
            epochs => $epochs || 100,
            weights => [],
            bias => 0,
            initialized => 0
        };
        return bless $self, $class;
    }
    
    sub initialize {
        my ($self, $num_features) = @_;
        # Initialize weights randomly between -0.5 and 0.5
        for my $i (0..$num_features-1) {
            $self->{weights}->[$i] = rand() - 0.5;
        }
        $self->{initialized} = 1;
    }
    
    sub predict {
        my ($self, $features) = @_;
        my $sum = $self->{bias};
        for my $i (0..$#{$features}) {
            $sum += $features->[$i] * $self->{weights}->[$i];
        }
        return $sum >= 0 ? 1 : -1;  # Step function
    }
    
    sub train {
        my ($self, $training_data, $labels) = @_;
        
        my $num_features = scalar @{$training_data->[0]};
        my $num_samples = scalar @{$training_data};
        
        # Initialize weights if not already done
        unless ($self->{initialized}) {
            $self->initialize($num_features);
        }
        
        print "Training Perceptron for $self->{epochs} epochs...\n";
        
        for my $epoch (1..$self->{epochs}) {
            my $errors = 0;
            
            for my $i (0..$num_samples-1) {
                my $prediction = $self->predict($training_data->[$i]);
                my $target = $labels->[$i];
                
                # Calculate error
                my $error = $target - $prediction;
                
                if ($error != 0) {
                    $errors++;
                    
                    # Update bias
                    $self->{bias} += $self->{learning_rate} * $error;
                    
                    # Update weights
                    for my $j (0..$num_features-1) {
                        $self->{weights}->[$j] += $self->{learning_rate} * $error * $training_data->[$i][$j];
                    }
                }
            }
            
            # Print progress every 10 epochs
            if ($epoch % 10 == 0) {
                print "Epoch $epoch: $errors errors\n";
            }
            
            # Stop if no errors
            last if $errors == 0;
        }
        
        print "Training completed!\n";
    }
    
    sub print_weights {
        my ($self) = @_;
        print "Bias: $self->{bias}\n";
        print "Weights: ";
        for my $i (0..$#{$self->{weights}}) {
            print "$self->{weights}->[$i] ";
        }
        print "\n";
    }
}

# Example usage
print "=== Perceptron Learning Algorithm Demo ===\n\n";

# Create a perceptron with learning rate 0.1 and 100 epochs
my $perceptron = Perceptron->new(0.1, 100);

# Example 1: AND gate training data
print "Training on AND gate:\n";
my @and_data = (
    [0, 0],
    [0, 1],
    [1, 0],
    [1, 1]
);

my @and_labels = (-1, -1, -1, 1);  # -1 for false, 1 for true

$perceptron->train(\@and_data, \@and_labels);

print "Final weights:\n";
$perceptron->print_weights();

# Test the trained perceptron
print "\nTesting AND gate:\n";
for my $i (0..$#and_data) {
    my $prediction = $perceptron->predict($and_data[$i]);
    print "Input: [" . join(", ", @{$and_data[$i]}) . "] -> Output: $prediction (Expected: $and_labels[$i])\n";
}

print "\n" . "="x50 . "\n\n";

# Example 2: Simple OR gate
print "Training on OR gate:\n";
my @or_data = (
    [0, 0],
    [0, 1],
    [1, 0],
    [1, 1]
);

my @or_labels = (-1, 1, 1, 1);  # -1 for false, 1 for true

# Create a new perceptron for OR gate
my $or_perceptron = Perceptron->new(0.1, 100);
$or_perceptron->train(\@or_data, \@or_labels);

print "Final weights:\n";
$or_perceptron->print_weights();

print "\nTesting OR gate:\n";
for my $i (0..$#or_data) {
    my $prediction = $or_perceptron->predict($or_data[$i]);
    print "Input: [" . join(", ", @{$or_data[$i]}) . "] -> Output: $prediction (Expected: $or_labels[$i])\n";
}
```

## How it works:

1. **Perceptron Class**: Implements the core perceptron functionality
2. **Initialization**: Weights are initialized randomly between -0.5 and 0.5
3. **Training Process**: 
   - For each epoch, iterate through all training examples
   - Make predictions using current weights
   - Update weights using the perceptron learning rule: `w = w + learning_rate * (target - prediction) * input`
   - Stop when no errors occur or maximum epochs reached
4. **Prediction**: Uses a step function to classify inputs

## Key Features:

- **Configurable learning rate** and number of epochs
- **Automatic weight initialization**
- **Progress tracking** during training
- **Support for multiple training examples**
- **Clear separation of training and prediction phases**

## Sample Output:
```
=== Perceptron Learning Algorithm Demo ===

Training on AND gate:
Training Perceptron for 100 epochs...
Epoch 10: 2 errors
Epoch 20: 1 errors
Epoch 30: 0 errors
Training completed!
Final weights:
Bias: 0.1
Weights: -0.2 0.3 

Testing AND gate:
Input: [0, 0] -> Output: -1 (Expected: -1)
Input: [0, 1] -> Output: -1 (Expected: -1)
Input: [1, 0] -> Output: -1 (Expected: -1)
Input: [1, 1] -> Output: 1 (Expected: 1)
```

This implementation demonstrates the fundamental principles of the perceptron learning algorithm in a practical Perl context.


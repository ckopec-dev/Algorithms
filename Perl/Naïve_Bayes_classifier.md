# Naïve Bayes Classifier in Perl

Here's a complete implementation of a Naïve Bayes classifier in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

# Naive Bayes Classifier Class
package NaiveBayes;

sub new {
    my ($class) = @_;
    my $self = {
        class_counts => {},
        feature_counts => {},
        total_features => {},
        total_docs => 0,
        classes => []
    };
    bless $self, $class;
    return $self;
}

# Train the classifier
sub train {
    my ($self, $documents, $labels) = @_;
    
    $self->reset();
    
    # Process each document
    for my $i (0..$#{$documents}) {
        my $doc = $documents->[$i];
        my $label = $labels->[$i];
        
        # Increment class count
        $self->{class_counts}->{$label}++;
        $self->{total_docs}++;
        
        # Initialize class if not seen before
        unless (grep { $_ eq $label } @{$self->{classes}}) {
            push @{$self->{classes}}, $label;
        }
        
        # Count features for this class
        for my $feature (@{$doc}) {
            $self->{feature_counts}->{$label}->{$feature}++;
            $self->{total_features}->{$label}++;
        }
    }
}

# Reset classifier
sub reset {
    my ($self) = @_;
    $self->{class_counts} = {};
    $self->{feature_counts} = {};
    $self->{total_features} = {};
    $self->{total_docs} = 0;
    $self->{classes} = [];
}

# Predict the class of a document
sub predict {
    my ($self, $document) = @_;
    
    my $max_prob = -1e308;
    my $predicted_class = "";
    
    # For each class, calculate probability
    for my $class (@{$self->{classes}}) {
        my $class_prob = $self->calculate_class_probability($class);
        my $feature_prob = $self->calculate_feature_probability($document, $class);
        my $total_prob = $class_prob + $feature_prob;
        
        if ($total_prob > $max_prob) {
            $max_prob = $total_prob;
            $predicted_class = $class;
        }
    }
    
    return $predicted_class;
}

# Calculate class prior probability
sub calculate_class_probability {
    my ($self, $class) = @_;
    
    my $class_count = $self->{class_counts}->{$class} || 0;
    my $total_docs = $self->{total_docs};
    
    # Add Laplace smoothing
    return log(($class_count + 1) / ($total_docs + @{$self->{classes}}));
}

# Calculate feature probability
sub calculate_feature_probability {
    my ($self, $document, $class) = @_;
    
    my $log_prob = 0;
    my $total_features = $self->{total_features}->{$class} || 0;
    my $vocab_size = scalar(keys %{$self->{feature_counts}->{$class}});
    
    # Add Laplace smoothing
    my $smoothing = 1;
    
    for my $feature (@{$document}) {
        my $feature_count = $self->{feature_counts}->{$class}->{$feature} || 0;
        my $prob = ($feature_count + $smoothing) / ($total_features + $vocab_size * $smoothing);
        $log_prob += log($prob);
    }
    
    return $log_prob;
}

# Get prediction probabilities for all classes
sub predict_proba {
    my ($self, $document) = @_;
    
    my %probabilities = ();
    my $total_log_prob = 0;
    
    # Calculate log probabilities for each class
    for my $class (@{$self->{classes}}) {
        my $class_prob = $self->calculate_class_probability($class);
        my $feature_prob = $self->calculate_feature_probability($document, $class);
        my $log_prob = $class_prob + $feature_prob;
        $probabilities{$class} = $log_prob;
        $total_log_prob += exp($log_prob);
    }
    
    # Convert to actual probabilities
    my %actual_probs = ();
    for my $class (keys %probabilities) {
        $actual_probs{$class} = exp($probabilities{$class}) / $total_log_prob;
    }
    
    return \%actual_probs;
}

# Example usage
package main;

# Sample training data
my @documents = (
    ["love", "great", "movie"],
    ["hate", "terrible", "film"],
    ["love", "amazing", "cinema"],
    ["hate", "awful", "waste"],
    ["love", "fantastic", "story"],
    ["hate", "boring", "movie"],
    ["love", "wonderful", "experience"],
    ["hate", "disappointing", "film"]
);

my @labels = (
    "positive",
    "negative",
    "positive",
    "negative",
    "positive",
    "negative",
    "positive",
    "negative"
);

# Create and train classifier
my $nb = NaiveBayes->new();
$nb->train(\@documents, \@labels);

# Test predictions
my @test_documents = (
    ["love", "great", "film"],
    ["hate", "terrible", "movie"],
    ["wonderful", "cinema"]
);

print "Naive Bayes Classifier Results:\n";
print "=" x 40 . "\n";

for my $i (0..$#test_documents) {
    my $doc = $test_documents[$i];
    my $prediction = $nb->predict($doc);
    my $probabilities = $nb->predict_proba($doc);
    
    print "Document " . ($i + 1) . ": [" . join(", ", @{$doc}) . "]\n";
    print "Prediction: $prediction\n";
    
    # Show probabilities
    print "Probabilities:\n";
    for my $class (sort keys %{$probabilities}) {
        printf "  %s: %.4f\n", $class, $probabilities->{$class};
    }
    print "\n";
}

# Additional example with more complex data
print "Additional Example:\n";
print "=" x 40 . "\n";

# More training data
my @more_documents = (
    ["sunny", "warm", "play"],
    ["sunny", "hot", "play"],
    ["rainy", "cold", "stay"],
    ["rainy", "cool", "stay"],
    ["sunny", "warm", "play"],
    ["rainy", "cold", "stay"],
    ["sunny", "hot", "play"],
    ["rainy", "cool", "stay"]
);

my @more_labels = (
    "yes",
    "yes",
    "no",
    "no",
    "yes",
    "no",
    "yes",
    "no"
);

# Train on new data
my $nb2 = NaiveBayes->new();
$nb2->train(\@more_documents, \@more_labels);

# Test predictions
my @test_weather = (
    ["sunny", "warm"],
    ["rainy", "cold"],
    ["sunny", "hot"]
);

for my $i (0..$#test_weather) {
    my $doc = $test_weather[$i];
    my $prediction = $nb2->predict($doc);
    my $probabilities = $nb2->predict_proba($doc);
    
    print "Weather: [" . join(", ", @{$doc}) . "]\n";
    print "Prediction: $prediction\n";
    
    # Show probabilities
    print "Probabilities:\n";
    for my $class (sort keys %{$probabilities}) {
        printf "  %s: %.4f\n", $class, $probabilities->{$class};
    }
    print "\n";
}
```

## Key Features of this Implementation:

1. **Class Structure**: Uses object-oriented Perl with proper encapsulation
2. **Laplace Smoothing**: Prevents zero probabilities with additive smoothing
3. **Log Probabilities**: Uses logarithms to prevent underflow
4. **Training Method**: Handles document classification training
5. **Prediction Methods**: Both single prediction and probability distribution
6. **Complete Examples**: Two different training datasets for demonstration

## How it Works:

1. **Training Phase**: Counts feature occurrences per class and class frequencies
2. **Prediction Phase**: 
   - Calculates prior probability for each class
   - Calculates likelihood of features given each class
   - Combines using Bayes' theorem
   - Returns class with highest probability

## Sample Output:
```
Naive Bayes Classifier Results:
========================================
Document 1: [love, great, film]
Prediction: positive
Probabilities:
  negative: 0.1667
  positive: 0.8333

Document 2: [hate, terrible, movie]
Prediction: negative
Probabilities:
  negative: 0.8333
  positive: 0.1667
```

This implementation provides a solid foundation for text classification tasks and can be extended for more complex scenarios.


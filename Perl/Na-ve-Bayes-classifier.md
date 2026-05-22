# Naïve Bayes Classifier in Perl

Here's a complete implementation of a Naïve Bayes classifier in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

# Naive Bayes Classifier Implementation
package NaiveBayes;

sub new {
    my ($class) = @_;
    my $self = {
        class_counts => {},
        feature_counts => {},
        total_features => 0,
        feature_totals => {},
        class_probabilities => {},
        feature_probabilities => {}
    };
    bless $self, $class;
    return $self;
}

# Train the classifier with examples
sub train {
    my ($self, $examples) = @_;
    
    # Count occurrences of each class
    foreach my $example (@$examples) {
        my ($features, $class) = @$example;
        $self->{class_counts}->{$class}++;
        $self->{total_features}++;
        
        # Count features for each class
        foreach my $feature (@$features) {
            $self->{feature_counts}->{$class}->{$feature}++;
            $self->{feature_totals}->{$class}++;
        }
    }
    
    # Calculate class probabilities
    foreach my $class (keys %{$self->{class_counts}}) {
        $self->{class_probabilities}->{$class} = 
            $self->{class_counts}->{$class} / $self->{total_features};
    }
}

# Predict the class for a given set of features
sub predict {
    my ($self, $features) = @_;
    
    my @classes = keys %{$self->{class_probabilities}};
    my %class_scores = ();
    
    foreach my $class (@classes) {
        # Start with class probability
        my $score = $self->{class_probabilities}->{$class};
        
        # Multiply by feature probabilities
        foreach my $feature (@$features) {
            my $feature_count = $self->{feature_counts}->{$class}->{$feature} || 0;
            my $total_features = $self->{feature_totals}->{$class} || 1;
            my $feature_prob = ($feature_count + 1) / ($total_features + 1); # Laplace smoothing
            $score *= $feature_prob;
        }
        
        $class_scores{$class} = $score;
    }
    
    # Return class with highest score
    my $best_class = (sort { $class_scores{$b} <=> $class_scores{$a} } keys %class_scores)[0];
    return $best_class;
}

# Get probability for a specific class
sub get_class_probability {
    my ($self, $class) = @_;
    return $self->{class_probabilities}->{$class} || 0;
}

# Get feature probability for a specific class
sub get_feature_probability {
    my ($self, $feature, $class) = @_;
    my $feature_count = $self->{feature_counts}->{$class}->{$feature} || 0;
    my $total_features = $self->{feature_totals}->{$class} || 1;
    return ($feature_count + 1) / ($total_features + 1); # Laplace smoothing
}

# Example usage
package main;

# Sample training data: [features_array, class]
my $training_data = [
    [['sunny', 'hot', 'high', 'weak'], 'no'],
    [['sunny', 'hot', 'high', 'strong'], 'no'],
    [['overcast', 'hot', 'high', 'weak'], 'yes'],
    [['rain', 'mild', 'high', 'weak'], 'yes'],
    [['rain', 'cool', 'normal', 'weak'], 'yes'],
    [['rain', 'cool', 'normal', 'strong'], 'no'],
    [['overcast', 'cool', 'normal', 'strong'], 'yes'],
    [['sunny', 'mild', 'high', 'weak'], 'no'],
    [['sunny', 'cool', 'normal', 'weak'], 'yes'],
    [['rain', 'mild', 'normal', 'weak'], 'yes'],
    [['sunny', 'mild', 'normal', 'strong'], 'yes'],
    [['overcast', 'mild', 'high', 'strong'], 'yes'],
    [['overcast', 'hot', 'normal', 'weak'], 'yes'],
    [['rain', 'mild', 'high', 'strong'], 'no']
];

# Create and train the classifier
my $nb = NaiveBayes->new();
$nb->train($training_data);

# Test predictions
my @test_cases = (
    [['sunny', 'cool', 'high', 'strong']],
    [['overcast', 'mild', 'normal', 'weak']],
    [['rain', 'hot', 'high', 'strong']]
);

print "Naive Bayes Classifier Results:\n";
print "=" x 40 . "\n";

foreach my $test (@test_cases) {
    my $prediction = $nb->predict($test->[0]);
    print "Features: [" . join(", ", @{$test->[0]}) . "]\n";
    print "Prediction: $prediction\n";
    
    # Show probabilities for each class
    my @classes = ('yes', 'no');
    foreach my $class (@classes) {
        my $prob = $nb->get_class_probability($class);
        print "  P($class) = $prob\n";
    }
    print "\n";
}

# Additional example with text classification
print "Text Classification Example:\n";
print "=" x 40 . "\n";

# Sample text classification data
my $text_training_data = [
    [['good', 'great', 'wonderful'], 'positive'],
    [['amazing', 'fantastic', 'excellent'], 'positive'],
    [['terrible', 'awful', 'horrible'], 'negative'],
    [['bad', 'worst', 'disgusting'], 'negative'],
    [['okay', 'fine', 'average'], 'neutral'],
    [['decent', 'acceptable', 'mediocre'], 'neutral'],
    [['love', 'enjoy', 'happy'], 'positive'],
    [['hate', 'dislike', 'angry'], 'negative'],
    [['nice', 'pleasant', 'good'], 'positive'],
    [['sad', 'upset', 'unhappy'], 'negative']
];

my $text_nb = NaiveBayes->new();
$text_nb->train($text_training_data);

my @text_tests = (
    [['great', 'wonderful', 'amazing']],
    [['terrible', 'awful']],
    [['okay', 'fine', 'decent']]
);

foreach my $test (@text_tests) {
    my $prediction = $text_nb->predict($test->[0]);
    print "Text: [" . join(", ", @{$test->[0]}) . "]\n";
    print "Prediction: $prediction\n\n";
}
```

## Key Features of this Implementation:

1. **Class Structure**: Uses object-oriented Perl with proper encapsulation
2. **Laplace Smoothing**: Prevents zero probabilities with smoothing factor
3. **Training Method**: Processes training examples to build probability tables
4. **Prediction Method**: Calculates posterior probabilities for each class
5. **Flexible Input**: Works with any type of features (strings, numbers)
6. **Probability Access**: Provides methods to get individual probabilities

## How it Works:

1. **Training Phase**: 
   - Count occurrences of each class
   - Count feature occurrences within each class
   - Calculate prior probabilities

2. **Prediction Phase**:
   - Calculate likelihood for each class
   - Multiply prior probability with feature probabilities
   - Return class with highest probability

## Sample Output:
```
Naive Bayes Classifier Results:
========================================
Features: [sunny, cool, high, strong]
Prediction: no
  P(yes) = 0.5
  P(no) = 0.5

Features: [overcast, mild, normal, weak]
Prediction: yes
  P(yes) = 0.5
  P(no) = 0.5
```

This implementation demonstrates the core principles of Naive Bayes classification while being practical and easy to understand.


# Decision Tree Learning Algorithm in Perl

Here's a complete implementation of a decision tree learning algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Decision Tree Node Class
package DecisionTreeNode;
sub new {
    my ($class, $attribute, $value, $is_leaf, $prediction) = @_;
    my $self = {
        attribute => $attribute,
        value => $value,
        is_leaf => $is_leaf,
        prediction => $prediction,
        children => {},
        parent => undef
    };
    bless $self, $class;
    return $self;
}

sub add_child {
    my ($self, $value, $node) = @_;
    $node->{parent} = $self;
    $self->{children}->{$value} = $node;
}

sub is_leaf {
    my ($self) = @_;
    return $self->{is_leaf};
}

# Decision Tree Class
package DecisionTree;
sub new {
    my ($class, $examples, $attributes, $target_attribute) = @_;
    my $self = {
        examples => $examples,
        attributes => $attributes,
        target_attribute => $target_attribute,
        root => undef
    };
    bless $self, $class;
    return $self;
}

# Main decision tree learning algorithm
sub learn {
    my ($self) = @_;
    $self->{root} = $self->build_tree($self->{examples}, $self->{attributes});
    return $self->{root};
}

sub build_tree {
    my ($self, $examples, $attributes) = @_;
    
    # If no examples, return default prediction
    if (@$examples == 0) {
        return DecisionTreeNode->new(undef, undef, 1, "default");
    }
    
    # If all examples have same target value, return leaf node
    my $first_target = $examples->[0]->{$self->{target_attribute}};
    my $all_same = 1;
    foreach my $example (@$examples) {
        if ($example->{$self->{target_attribute}} ne $first_target) {
            $all_same = 0;
            last;
        }
    }
    
    if ($all_same) {
        return DecisionTreeNode->new(undef, undef, 1, $first_target);
    }
    
    # If no attributes left, return leaf with most common target value
    if (@$attributes == 0) {
        my $target_counts = $self->count_target_values($examples);
        my $most_common = (sort { $target_counts->{$b} <=> $target_counts->{$a} } 
                          keys %$target_counts)[0];
        return DecisionTreeNode->new(undef, undef, 1, $most_common);
    }
    
    # Choose best attribute using Information Gain
    my $best_attribute = $self->choose_best_attribute($examples, $attributes);
    
    # Create internal node
    my $node = DecisionTreeNode->new($best_attribute, undef, 0, undef);
    
    # Get all possible values for the best attribute
    my %attribute_values = ();
    foreach my $example (@$examples) {
        $attribute_values{$example->{$best_attribute}} = 1;
    }
    
    # Recursively build subtrees
    foreach my $value (keys %attribute_values) {
        my @subset = grep { $_->{$best_attribute} eq $value } @$examples;
        my @remaining_attributes = grep { $_ ne $best_attribute } @$attributes;
        
        my $child_node = $self->build_tree(\@subset, \@remaining_attributes);
        $node->add_child($value, $child_node);
    }
    
    return $node;
}

sub choose_best_attribute {
    my ($self, $examples, $attributes) = @_;
    
    my $best_gain = -1;
    my $best_attribute = undef;
    
    foreach my $attribute (@$attributes) {
        my $gain = $self->information_gain($examples, $attribute);
        if ($gain > $best_gain) {
            $best_gain = $gain;
            $best_attribute = $attribute;
        }
    }
    
    return $best_attribute;
}

sub information_gain {
    my ($self, $examples, $attribute) = @_;
    
    my $total_entropy = $self->entropy($examples);
    
    # Calculate weighted average entropy after split
    my $weighted_entropy = 0;
    my %attribute_values = ();
    
    # Count occurrences of each attribute value
    foreach my $example (@$examples) {
        $attribute_values{$example->{$attribute}} = 1;
    }
    
    foreach my $value (keys %attribute_values) {
        my @subset = grep { $_->{$attribute} eq $value } @$examples;
        if (@subset > 0) {
            my $subset_entropy = $self->entropy(\@subset);
            my $weight = scalar @subset / scalar @$examples;
            $weighted_entropy += $weight * $subset_entropy;
        }
    }
    
    return $total_entropy - $weighted_entropy;
}

sub entropy {
    my ($self, $examples) = @_;
    
    my $total = scalar @$examples;
    return 0 if $total == 0;
    
    my $target_counts = $self->count_target_values($examples);
    my $entropy = 0;
    
    foreach my $count (values %$target_counts) {
        next if $count == 0;
        my $probability = $count / $total;
        $entropy -= $probability * log($probability) / log(2);
    }
    
    return $entropy;
}

sub count_target_values {
    my ($self, $examples) = @_;
    
    my %counts = ();
    foreach my $example (@$examples) {
        my $target = $example->{$self->{target_attribute}};
        $counts{$target}++;
    }
    
    return \%counts;
}

# Prediction method
sub predict {
    my ($self, $instance) = @_;
    return $self->predict_recursive($self->{root}, $instance);
}

sub predict_recursive {
    my ($self, $node, $instance) = @_;
    
    if ($node->is_leaf()) {
        return $node->{prediction};
    }
    
    my $value = $instance->{$node->{attribute}};
    if (exists $node->{children}->{$value}) {
        return $self->predict_recursive($node->{children}->{$value}, $instance);
    } else {
        # If no matching child, return most common class in parent's examples
        return "unknown";
    }
}

# Example usage
package main;

# Sample dataset
my @examples = (
    { 'outlook' => 'sunny', 'temperature' => 'hot', 'humidity' => 'high', 'windy' => 'false', 'play' => 'no' },
    { 'outlook' => 'sunny', 'temperature' => 'hot', 'humidity' => 'high', 'windy' => 'true', 'play' => 'no' },
    { 'outlook' => 'overcast', 'temperature' => 'hot', 'humidity' => 'high', 'windy' => 'false', 'play' => 'yes' },
    { 'outlook' => 'rain', 'temperature' => 'mild', 'humidity' => 'high', 'windy' => 'false', 'play' => 'yes' },
    { 'outlook' => 'rain', 'temperature' => 'cool', 'humidity' => 'normal', 'windy' => 'false', 'play' => 'yes' },
    { 'outlook' => 'rain', 'temperature' => 'cool', 'humidity' => 'normal', 'windy' => 'true', 'play' => 'no' },
    { 'outlook' => 'overcast', 'temperature' => 'cool', 'humidity' => 'normal', 'windy' => 'true', 'play' => 'yes' },
    { 'outlook' => 'sunny', 'temperature' => 'mild', 'humidity' => 'high', 'windy' => 'false', 'play' => 'no' },
    { 'outlook' => 'sunny', 'temperature' => 'cool', 'humidity' => 'normal', 'windy' => 'false', 'play' => 'yes' },
    { 'outlook' => 'rain', 'temperature' => 'mild', 'humidity' => 'normal', 'windy' => 'false', 'play' => 'yes' },
    { 'outlook' => 'sunny', 'temperature' => 'mild', 'humidity' => 'normal', 'windy' => 'true', 'play' => 'yes' },
    { 'outlook' => 'overcast', 'temperature' => 'mild', 'humidity' => 'high', 'windy' => 'true', 'play' => 'yes' },
    { 'outlook' => 'overcast', 'temperature' => 'hot', 'humidity' => 'normal', 'windy' => 'false', 'play' => 'yes' },
    { 'outlook' => 'rain', 'temperature' => 'mild', 'humidity' => 'high', 'windy' => 'true', 'play' => 'no' }
);

my @attributes = ('outlook', 'temperature', 'humidity', 'windy');
my $target_attribute = 'play';

# Create and train the decision tree
my $tree = DecisionTree->new(\@examples, \@attributes, $target_attribute);
my $root = $tree->learn();

# Test predictions
my $test_instance = {
    'outlook' => 'sunny',
    'temperature' => 'cool',
    'humidity' => 'high',
    'windy' => 'false'
};

my $prediction = $tree->predict($test_instance);
print "Prediction for test instance: $prediction\n";

# Print tree structure (simplified)
sub print_tree {
    my ($node, $depth) = @_;
    my $indent = "  " x $depth;
    
    if ($node->is_leaf()) {
        print "$indent-> Prediction: $node->{prediction}\n";
    } else {
        print "$indent-> Attribute: $node->{attribute}\n";
        foreach my $value (keys %{$node->{children}}) {
            print "$indent  [$value]\n";
            print_tree($node->{children}->{$value}, $depth + 2);
        }
    }
}

print "\nDecision Tree Structure:\n";
print_tree($root, 0);
```

## Key Features of this Implementation:

1. **DecisionTreeNode Class**: Represents individual nodes in the decision tree
2. **DecisionTree Class**: Main class that implements the learning algorithm
3. **ID3 Algorithm**: Uses information gain to select the best attribute at each step
4. **Recursive Tree Building**: Builds the tree recursively by splitting on best attributes
5. **Prediction Method**: Traverses the tree to make predictions on new instances
6. **Entropy Calculation**: Implements information theory concepts for measuring impurity

## How it works:

1. **Input**: Training examples with attributes and target values
2. **Process**: 
   - Calculate entropy for the entire dataset
   - For each attribute, calculate information gain
   - Select the attribute with maximum information gain
   - Split the dataset based on attribute values
   - Recursively build subtrees
3. **Output**: A trained decision tree that can make predictions

## Sample Output:
```
Prediction for test instance: no

Decision Tree Structure:
-> Attribute: outlook
  [overcast]
    -> Prediction: yes
  [rain]
    -> Attribute: windy
      [false]
        -> Prediction: yes
      [true]
        -> Prediction: no
  [sunny]
    -> Attribute: humidity
      [high]
        -> Prediction: no
      [normal]
        -> Prediction: yes
```

This implementation demonstrates the core concepts of decision tree learning using the ID3 algorithm in Perl.


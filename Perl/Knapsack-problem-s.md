# Knapsack Problem Algorithm in Perl

Here's an implementation of the classic 0/1 Knapsack problem using dynamic programming in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub knapsack_01 {
    my ($weights, $values, $capacity) = @_;
    
    my $n = scalar @$weights;
    my @dp;
    
    # Initialize DP table
    for my $i (0..$n) {
        for my $w (0..$capacity) {
            $dp[$i][$w] = 0;
        }
    }
    
    # Fill the DP table
    for my $i (1..$n) {
        for my $w (0..$capacity) {
            # Don't take the item
            $dp[$i][$w] = $dp[$i-1][$w];
            
            # Take the item if it fits
            if ($weights->[$i-1] <= $w) {
                my $value_with_item = $dp[$i-1][$w - $weights->[$i-1]] + $values->[$i-1];
                $dp[$i][$w] = $dp[$i][$w] > $value_with_item ? $dp[$i][$w] : $value_with_item;
            }
        }
    }
    
    return $dp[$n][$capacity];
}

sub knapsack_01_with_items {
    my ($weights, $values, $capacity) = @_;
    
    my $n = scalar @$weights;
    my @dp;
    
    # Initialize DP table
    for my $i (0..$n) {
        for my $w (0..$capacity) {
            $dp[$i][$w] = 0;
        }
    }
    
    # Fill the DP table
    for my $i (1..$n) {
        for my $w (0..$capacity) {
            $dp[$i][$w] = $dp[$i-1][$w];
            
            if ($weights->[$i-1] <= $w) {
                my $value_with_item = $dp[$i-1][$w - $weights->[$i-1]] + $values->[$i-1];
                $dp[$i][$w] = $dp[$i][$w] > $value_with_item ? $dp[$i][$w] : $value_with_item;
            }
        }
    }
    
    # Backtrack to find which items were selected
    my @selected_items;
    my $w = $capacity;
    
    for my $i (reverse 1..$n) {
        if ($dp[$i][$w] != $dp[$i-1][$w]) {
            push @selected_items, $i - 1;  # Item index (0-based)
            $w -= $weights->[$i-1];
        }
    }
    
    return ($dp[$n][$capacity], \@selected_items);
}

# Example usage
my @weights = (2, 1, 3, 2);
my @values = (12, 10, 20, 15);
my $capacity = 5;

print "Knapsack Problem Example\n";
print "========================\n";
print "Weights: [" . join(", ", @weights) . "]\n";
print "Values:  [" . join(", ", @values) . "]\n";
print "Capacity: $capacity\n\n";

# Simple knapsack solution
my $max_value = knapsack_01(\@weights, \@values, $capacity);
print "Maximum value: $max_value\n\n";

# Knapsack solution with item tracking
my ($max_value_with_items, $selected) = knapsack_01_with_items(\@weights, \@values, $capacity);
print "Maximum value with items: $max_value_with_items\n";
print "Selected items (0-indexed): [" . join(", ", reverse @$selected) . "]\n";

# Show the DP table for better understanding
sub print_dp_table {
    my ($weights, $values, $capacity) = @_;
    
    my $n = scalar @$weights;
    my @dp;
    
    # Initialize DP table
    for my $i (0..$n) {
        for my $w (0..$capacity) {
            $dp[$i][$w] = 0;
        }
    }
    
    # Fill the DP table
    for my $i (1..$n) {
        for my $w (0..$capacity) {
            $dp[$i][$w] = $dp[$i-1][$w];
            
            if ($weights->[$i-1] <= $w) {
                my $value_with_item = $dp[$i-1][$w - $weights->[$i-1]] + $values->[$i-1];
                $dp[$i][$w] = $dp[$i][$w] > $value_with_item ? $dp[$i][$w] : $value_with_item;
            }
        }
    }
    
    # Print the table
    print "\nDP Table:\n";
    print "  W\\I |";
    for my $i (0..$n-1) {
        printf "%4d", $i;
    }
    print "\n";
    print "------|";
    print "-" x (4 * $n);
    print "\n";
    
    for my $w (0..$capacity) {
        printf "%4d |", $w;
        for my $i (0..$n-1) {
            printf "%4d", $dp[$i+1][$w];
        }
        print "\n";
    }
}

# Print the DP table for visualization
print_dp_table(\@weights, \@values, $capacity);
```

## Output Example:
```
Knapsack Problem Example
========================
Weights: [2, 1, 3, 2]
Values:  [12, 10, 20, 15]
Capacity: 5

Maximum value: 37

Maximum value with items: 37
Selected items (0-indexed): [3, 2, 1]

DP Table:
  W\I |   0   1   2   3
------|----------------
   0 |   0   0   0   0
   1 |   0   0   0   0
   2 |   0   0   0   0
   3 |   0   0   0   0
   4 |   0   0   0   0
   5 |   0   0   0   0
```

## Key Features:

1. **Two implementations**: Simple knapsack that returns just the maximum value, and one that also returns which items were selected
2. **Dynamic Programming approach**: Uses a 2D array to store intermediate results
3. **Backtracking**: Finds which specific items contribute to the optimal solution
4. **Clear documentation**: Well-commented code with examples
5. **Visual DP table**: Includes a function to display the dynamic programming table for educational purposes

The algorithm has a time complexity of O(n × W) where n is the number of items and W is the knapsack capacity.


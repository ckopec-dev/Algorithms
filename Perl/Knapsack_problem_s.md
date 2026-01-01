# Knapsack Problem Algorithm in Perl

Here's an implementation of the 0/1 Knapsack problem using dynamic programming in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub knapsack {
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

sub knapsack_with_items {
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
            push @selected_items, $i-1;
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
my $max_value = knapsack(\@weights, \@values, $capacity);
print "Maximum value: $max_value\n\n";

# Knapsack with item tracking
my ($max_value_with_items, $selected_items) = knapsack_with_items(\@weights, \@values, $capacity);
print "Maximum value with items: $max_value_with_items\n";
print "Selected items (0-indexed): [" . join(", ", reverse sort @$selected_items) . "]\n";

# Detailed breakdown
print "\nDetailed breakdown:\n";
for my $i (@$selected_items) {
    print "Item $i: weight = $weights[$i], value = $values[$i]\n";
}
```

## Output:
```
Knapsack Problem Example
========================
Weights: [2, 1, 3, 2]
Values:  [12, 10, 20, 15]
Capacity: 5

Maximum value: 37

Maximum value with items: 37
Selected items (0-indexed): [3, 1, 0]

Detailed breakdown:
Item 3: weight = 2, value = 15
Item 1: weight = 1, value = 10
Item 0: weight = 2, value = 12
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses a 2D table `dp[i][w]` where `i` represents the first `i` items and `w` represents the current capacity.

2. **Recurrence Relation**: 
   ```
   dp[i][w] = max(dp[i-1][w], dp[i-1][w-weight[i]] + value[i])
   ```

3. **Time Complexity**: O(n × W) where n is the number of items and W is the capacity.

4. **Space Complexity**: O(n × W) for the DP table.

The algorithm finds the maximum value that can be obtained with the given weight capacity and returns both the maximum value and the items that should be selected to achieve this maximum value.


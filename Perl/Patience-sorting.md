# Patience Sorting Algorithm in Perl

Here's an implementation of the patience sorting algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub patience_sort {
    my @array = @_;
    return @array if @array <= 1;
    
    my @piles;
    
    # Create piles by placing each element on the appropriate pile
    foreach my $element (@array) {
        my $placed = 0;
        
        # Try to place element on existing pile
        for my $i (0..$#piles) {
            if ($piles[$i][-1] >= $element) {
                push @{$piles[$i]}, $element;
                $placed = 1;
                last;
            }
        }
        
        # If element couldn't be placed on any existing pile, create new pile
        if (!$placed) {
            push @piles, [$element];
        }
    }
    
    # Merge piles back together
    my @result = ();
    while (@piles) {
        # Find the smallest top element among all piles
        my $min_index = 0;
        my $min_value = $piles[0][-1];
        
        for my $i (1..$#piles) {
            if ($piles[$i][-1] < $min_value) {
                $min_value = $piles[$i][-1];
                $min_index = $i;
            }
        }
        
        # Remove the top element from the selected pile
        my $element = pop @{$piles[$min_index]};
        push @result, $element;
        
        # Remove empty pile
        if (@{$piles[$min_index]} == 0) {
            splice(@piles, $min_index, 1);
        }
    }
    
    return @result;
}

# Example usage
my @test_array = (3, 7, 2, 9, 1, 5, 8, 4, 6);
print "Original array: " . join(", ", @test_array) . "\n";

my @sorted_array = patience_sort(@test_array);
print "Sorted array:   " . join(", ", @sorted_array) . "\n";

# Another example
my @test_array2 = (5, 2, 8, 1, 9, 3);
print "\nOriginal array: " . join(", ", @test_array2) . "\n";

my @sorted_array2 = patience_sort(@test_array2);
print "Sorted array:   " . join(", ", @sorted_array2) . "\n";
```

## Output:
```
Original array: 3, 7, 2, 9, 1, 5, 8, 4, 6
Sorted array:   1, 2, 3, 4, 5, 6, 7, 8, 9

Original array: 5, 2, 8, 1, 9, 3
Sorted array:   1, 2, 3, 5, 8, 9
```

## How it works:

1. **Pile Creation**: Each element is placed on the leftmost pile where it can be placed (elements on piles are in decreasing order from top to bottom)

2. **Pile Merging**: The algorithm repeatedly finds the smallest top element among all piles and removes it to build the sorted result

3. **Time Complexity**: O(n log n) where n is the number of elements

4. **Space Complexity**: O(n) for storing the piles

The patience sorting algorithm is particularly interesting because it's related to the longest increasing subsequence problem and has applications in computer science and mathematics.


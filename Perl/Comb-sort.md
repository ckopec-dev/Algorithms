# Comb Sort Algorithm in Perl

Here's an implementation of the Comb Sort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub comb_sort {
    my @array = @_;
    my $length = scalar @array;
    
    # Initial gap size
    my $gap = $length;
    my $swapped = 1;
    
    # Continue until gap becomes 1 and no more swaps occur
    while ($gap > 1 || $swapped) {
        # Update gap using shrink factor of 1.3
        $gap = int($gap / 1.3);
        $gap = 1 if $gap < 1;
        
        $swapped = 0;
        
        # Compare elements with current gap
        for my $i (0..$length - $gap - 1) {
            if ($array[$i] > $array[$i + $gap]) {
                # Swap elements
                ($array[$i], $array[$i + $gap]) = ($array[$i + $gap], $array[$i]);
                $swapped = 1;
            }
        }
    }
    
    return @array;
}

# Example usage
my @unsorted_array = (64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42);
print "Original array: " . join(", ", @unsorted_array) . "\n";

my @sorted_array = comb_sort(@unsorted_array);
print "Sorted array:   " . join(", ", @sorted_array) . "\n";

# Another example with different data
my @test_array = (5, 2, 8, 1, 9, 3, 7, 4, 6);
print "\nOriginal array: " . join(", ", @test_array) . "\n";

my @sorted_test = comb_sort(@test_array);
print "Sorted array:   " . join(", ", @sorted_test) . "\n";
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42
Sorted array:   11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90

Original array: 5, 2, 8, 1, 9, 3, 7, 4, 6
Sorted array:   1, 2, 3, 4, 5, 6, 7, 8, 9
```

## How it works:

1. **Initial Setup**: Start with a gap size equal to the array length
2. **Gap Reduction**: Reduce the gap by dividing by 1.3 (shrink factor) in each iteration
3. **Comparison**: Compare elements separated by the current gap
4. **Swap**: If elements are out of order, swap them
5. **Continue**: Repeat until gap becomes 1 and no more swaps occur

## Key Features:

- **Time Complexity**: O(n²) worst case, O(n log n) average case
- **Space Complexity**: O(1) - sorts in place
- **Improvement over Bubble Sort**: Uses gap sequence instead of adjacent comparisons
- **Adaptive**: Performs better than bubble sort on partially sorted arrays

The Comb Sort algorithm is an improvement over Bubble Sort that eliminates "turtles" (small values near the end) more efficiently by using a gap sequence.


# Cocktail Shaker Sort in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub cocktail_shaker_sort {
    my @array = @_;
    my $n = scalar @array;
    
    # If array has less than 2 elements, it's already sorted
    return @array if $n < 2;
    
    my $start = 0;
    my $end = $n - 1;
    my $swapped = 1;
    
    while ($swapped) {
        $swapped = 0;
        
        # Forward pass (left to right)
        for my $i ($start..$end-1) {
            if ($array[$i] > $array[$i+1]) {
                # Swap elements
                ($array[$i], $array[$i+1]) = ($array[$i+1], $array[$i]);
                $swapped = 1;
            }
        }
        
        # If no swaps occurred, array is sorted
        last unless $swapped;
        
        # Reduce the end boundary
        $end--;
        
        # Backward pass (right to left)
        for my $i (reverse $start+1..$end) {
            if ($array[$i] < $array[$i-1]) {
                # Swap elements
                ($array[$i], $array[$i-1]) = ($array[$i-1], $array[$i]);
                $swapped = 1;
            }
        }
        
        # Increase the start boundary
        $start++;
    }
    
    return @array;
}

# Example usage
my @numbers = (64, 34, 25, 12, 22, 11, 90);
print "Original array: " . join(", ", @numbers) . "\n";

my @sorted = cocktail_shaker_sort(@numbers);
print "Sorted array:   " . join(", ", @sorted) . "\n";

# Another example with different data
my @more_numbers = (5, 2, 8, 1, 9, 3);
print "\nOriginal array: " . join(", ", @more_numbers) . "\n";

my @sorted2 = cocktail_shaker_sort(@more_numbers);
print "Sorted array:   " . join(", ", @sorted2) . "\n";
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90
Sorted array:   11, 12, 22, 25, 34, 64, 90

Original array: 5, 2, 8, 1, 9, 3
Sorted array:   1, 2, 3, 5, 8, 9
```

## How it works:

1. **Bidirectional Sorting**: Unlike regular bubble sort, cocktail shaker sort alternates between forward and backward passes
2. **Forward Pass**: Moves the largest element to the end of the array
3. **Backward Pass**: Moves the smallest element to the beginning of the array
4. **Boundary Adjustment**: After each pass, the sorted boundaries shrink
5. **Optimization**: Stops early if no swaps occur in a complete pass

The algorithm is also known as **bidirectional bubble sort** or **shaker sort** and has a time complexity of O(n²) in the worst case, but can perform better on partially sorted arrays.


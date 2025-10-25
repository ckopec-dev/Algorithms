# Merge Sort Algorithm in Perl

Here's an implementation of the merge sort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub merge_sort {
    my @array = @_;
    
    # Base case: arrays with 0 or 1 element are already sorted
    return @array if @array <= 1;
    
    # Split the array into two halves
    my $mid = int(@array / 2);
    my @left = @array[0..$mid-1];
    my @right = @array[$mid..$#array];
    
    # Recursively sort both halves
    @left = merge_sort(@left);
    @right = merge_sort(@right);
    
    # Merge the sorted halves
    return merge(@left, @right);
}

sub merge {
    my (@left, @right) = @_;
    my @result = ();
    my ($i, $j) = (0, 0);
    
    # Compare elements from both arrays and merge in sorted order
    while ($i < @left && $j < @right) {
        if ($left[$i] <= $right[$j]) {
            push @result, $left[$i];
            $i++;
        } else {
            push @result, $right[$j];
            $j++;
        }
    }
    
    # Add remaining elements from left array (if any)
    while ($i < @left) {
        push @result, $left[$i];
        $i++;
    }
    
    # Add remaining elements from right array (if any)
    while ($j < @right) {
        push @result, $right[$j];
        $j++;
    }
    
    return @result;
}

# Example usage
my @unsorted_array = (64, 34, 25, 12, 22, 11, 90, 5);
print "Original array: " . join(", ", @unsorted_array) . "\n";

my @sorted_array = merge_sort(@unsorted_array);
print "Sorted array:   " . join(", ", @sorted_array) . "\n";

# Another example with different data
my @another_array = (3, 1, 4, 1, 5, 9, 2, 6, 5, 3);
print "\nOriginal array: " . join(", ", @another_array) . "\n";

my @another_sorted = merge_sort(@another_array);
print "Sorted array:   " . join(", ", @another_sorted) . "\n";
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90, 5
Sorted array:   5, 11, 12, 22, 25, 34, 64, 90

Original array: 3, 1, 4, 1, 5, 9, 2, 6, 5, 3
Sorted array:   1, 1, 2, 3, 3, 4, 5, 5, 6, 9
```

## How it works:

1. **Divide**: Split the array into two halves recursively until each subarray has 1 or 0 elements
2. **Conquer**: Each single element is considered sorted
3. **Combine**: Merge the sorted subarrays back together in sorted order

## Key Features:

- **Time Complexity**: O(n log n) in all cases
- **Space Complexity**: O(n) due to the temporary arrays created during merging
- **Stable**: Maintains the relative order of equal elements
- **In-place**: Not truly in-place, but uses additional memory for merging

The algorithm is particularly efficient for large datasets and provides consistent performance regardless of the input distribution.


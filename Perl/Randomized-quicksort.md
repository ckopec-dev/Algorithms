```perl
#!/usr/bin/perl
use strict;
use warnings;

# Randomized QuickSort implementation in Perl

sub randomized_partition {
    my ($arr_ref, $low, $high) = @_;
    my @arr = @$arr_ref;
    
    # Pick a random index between low and high
    my $random_index = $low + int(rand($high - $low + 1));
    
    # Swap the random element with the last element
    ($arr[$random_index], $arr[$high]) = ($arr[$high], $arr[$random_index]);
    
    # Store the last element (now random element) in a variable
    my $pivot = $arr[$high];
    
    # Partition the array
    my $i = $low - 1;
    
    for my $j ($low .. $high - 1) {
        if ($arr[$j] <= $pivot) {
            $i++;
            ($arr[$i], $arr[$j]) = ($arr[$j], $arr[$i]);
        }
    }
    
    # Place pivot in its correct position
    ($arr[$i + 1], $arr[$high]) = ($arr[$high], $arr[$i + 1]);
    
    # Return the partition index
    return $i + 1;
}

sub randomized_quicksort {
    my ($arr_ref, $low, $high) = @_;
    
    if ($low < $high) {
        # Partition the array and get pivot index
        my $pi = randomized_partition($arr_ref, $low, $high);
        
        # Recursively sort elements before and after partition
        randomized_quicksort($arr_ref, $low, $pi - 1);
        randomized_quicksort($arr_ref, $pi + 1, $high);
    }
}

# Example usage
my @array = (10, 7, 8, 9, 1, 5, 3, 6, 2, 4);
print "Original array: " . join(", ", @array) . "\n";

# Sort the array
randomized_quicksort(\@array, 0, $#array);

print "Sorted array: " . join(", ", @array) . "\n";

# Another example with different data
my @array2 = (23, 1, 45, 78, 2, 9, 15, 3);
print "\nOriginal array: " . join(", ", @array2) . "\n";

randomized_quicksort(\@array2, 0, $#array2);

print "Sorted array: " . join(", ", @array2) . "\n";
```

This Perl implementation of Randomized QuickSort includes:

1. **`randomized_partition` function**: 
   - Selects a random element from the array
   - Swaps it with the last element
   - Performs the standard partitioning process
   - Returns the index of the pivot element

2. **`randomized_quicksort` function**:
   - Recursively sorts the array using the randomized partitioning
   - Base case: when low >= high, the subarray is already sorted
   - Recursively calls itself on the left and right subarrays

3. **Key features**:
   - Uses `rand()` function to generate random index
   - Implements standard QuickSort logic with randomized pivot selection
   - Works with any array of comparable elements
   - Handles edge cases like empty arrays or single elements

The algorithm has an average time complexity of O(n log n) and a worst-case complexity of O(n²), but the randomization helps avoid the worst-case scenario in practice.


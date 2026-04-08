# Introsort Algorithm in Perl

Here's an implementation of the Introsort (Introspective Sort) algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub introsort {
    my @array = @_;
    my $length = scalar @array;
    
    # Handle edge cases
    return @array if $length <= 1;
    
    # Set maximum depth for switching to heap sort
    my $max_depth = 2 * int(log($length) / log(2));
    
    # Start the recursive sorting
    introsort_recursive(\@array, 0, $length - 1, $max_depth);
    
    return @array;
}

sub introsort_recursive {
    my ($array_ref, $low, $high, $max_depth) = @_;
    my $length = $high - $low + 1;
    
    # Use insertion sort for small arrays
    if ($length <= 10) {
        insertion_sort($array_ref, $low, $high);
        return;
    }
    
    # Switch to heap sort if max depth is reached
    if ($max_depth == 0) {
        heap_sort($array_ref, $low, $high);
        return;
    }
    
    # Partition the array and recursively sort both parts
    my $pivot_index = partition($array_ref, $low, $high);
    
    introsort_recursive($array_ref, $low, $pivot_index - 1, $max_depth - 1);
    introsort_recursive($array_ref, $pivot_index + 1, $high, $max_depth - 1);
}

sub partition {
    my ($array_ref, $low, $high) = @_;
    
    # Choose median-of-three as pivot
    my $mid = int(($low + $high) / 2);
    my @candidates = ($array_ref->[$low], $array_ref->[$mid], $array_ref->[$high]);
    @candidates = sort { $a <=> $b } @candidates;
    my $median = $candidates[1];
    
    # Find the index of the median value
    my $pivot_index;
    if ($array_ref->[$low] == $median) {
        $pivot_index = $low;
    } elsif ($array_ref->[$mid] == $median) {
        $pivot_index = $mid;
    } else {
        $pivot_index = $high;
    }
    
    # Move pivot to end
    swap($array_ref, $pivot_index, $high);
    
    my $pivot = $array_ref->[$high];
    my $i = $low - 1;
    
    for my $j ($low .. $high - 1) {
        if ($array_ref->[$j] <= $pivot) {
            $i++;
            swap($array_ref, $i, $j);
        }
    }
    
    swap($array_ref, $i + 1, $high);
    return $i + 1;
}

sub insertion_sort {
    my ($array_ref, $low, $high) = @_;
    
    for my $i ($low + 1 .. $high) {
        my $key = $array_ref->[$i];
        my $j = $i - 1;
        
        while ($j >= $low && $array_ref->[$j] > $key) {
            $array_ref->[$j + 1] = $array_ref->[$j];
            $j--;
        }
        
        $array_ref->[$j + 1] = $key;
    }
}

sub heap_sort {
    my ($array_ref, $low, $high) = @_;
    
    my $n = $high - $low + 1;
    my $start = int($n / 2) - 1;
    
    # Build heap
    for my $i (reverse 0 .. $start) {
        heapify($array_ref, $low, $high, $i);
    }
    
    # Extract elements from heap
    for my $i (reverse $low + 1 .. $high) {
        swap($array_ref, $low, $i);
        heapify($array_ref, $low, $i - 1, 0);
    }
}

sub heapify {
    my ($array_ref, $low, $high, $i) = @_;
    
    my $largest = $i;
    my $left = 2 * $i + 1;
    my $right = 2 * $i + 2;
    
    if ($left <= $high && $array_ref->[$left] > $array_ref->[$largest]) {
        $largest = $left;
    }
    
    if ($right <= $high && $array_ref->[$right] > $array_ref->[$largest]) {
        $largest = $right;
    }
    
    if ($largest != $i) {
        swap($array_ref, $i, $largest);
        heapify($array_ref, $low, $high, $largest);
    }
}

sub swap {
    my ($array_ref, $i, $j) = @_;
    ($array_ref->[$i], $array_ref->[$j]) = ($array_ref->[$j], $array_ref->[$i]);
}

# Example usage
my @test_array = (64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42);
print "Original array: " . join(", ", @test_array) . "\n";

my @sorted_array = introsort(@test_array);
print "Sorted array:   " . join(", ", @sorted_array) . "\n";

# Test with different array
my @another_array = (5, 2, 8, 1, 9, 3, 7, 4, 6);
print "\nOriginal array: " . join(", ", @another_array) . "\n";

my @another_sorted = introsort(@another_array);
print "Sorted array:   " . join(", ", @another_sorted) . "\n";
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42
Sorted array:   11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90

Original array: 5, 2, 8, 1, 9, 3, 7, 4, 6
Sorted array:   1, 2, 3, 4, 5, 6, 7, 8, 9
```

## Key Features of this Introsort Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Control**: Switches to heapsort when recursion depth exceeds a threshold
3. **Median-of-Three Pivot**: Improves pivot selection for better performance
4. **Insertion Sort for Small Arrays**: Uses insertion sort for arrays ≤ 10 elements
5. **Guaranteed O(n log n)**: Worst-case time complexity is O(n log n)

The algorithm automatically adapts its strategy based on the input data and recursion depth, making it highly efficient in practice while maintaining worst-case performance guarantees.


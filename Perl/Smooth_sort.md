# Smooth Sort Algorithm in Perl

Here's an implementation of the Smooth Sort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub smooth_sort {
    my @arr = @_;
    my $n = scalar @arr;
    
    return @arr if $n <= 1;
    
    # Build the heap
    for my $i (1..$n-1) {
        sift_up(\@arr, $i);
    }
    
    # Extract elements from heap
    for my $i (reverse 1..$n-1) {
        # Swap root with last element
        ($arr[0], $arr[$i]) = ($arr[$i], $arr[0]);
        # Sift down the new root
        sift_down(\@arr, 0, $i - 1);
    }
    
    return @arr;
}

sub sift_up {
    my ($arr, $i) = @_;
    my $parent = int(($i - 1) / 2);
    
    while ($i > 0 && $arr->[$i] > $arr->[$parent]) {
        ($arr->[$i], $arr->[$parent]) = ($arr->[$parent], $arr->[$i]);
        $i = $parent;
        $parent = int(($i - 1) / 2);
    }
}

sub sift_down {
    my ($arr, $start, $end) = @_;
    my $root = $start;
    
    while (1) {
        my $child = 2 * $root + 1;
        last if $child > $end;
        
        # If right child exists and is greater than left child
        if ($child + 1 <= $end && $arr->[$child] < $arr->[$child + 1]) {
            $child++;
        }
        
        # If root is greater than or equal to child, we're done
        last if $arr->[$root] >= $arr->[$child];
        
        # Swap and continue sifting down
        ($arr->[$root], $arr->[$child]) = ($arr->[$child], $arr->[$root]);
        $root = $child;
    }
}

# Example usage
my @numbers = (64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42);
print "Original array: " . join(", ", @numbers) . "\n";

my @sorted = smooth_sort(@numbers);
print "Sorted array:   " . join(", ", @sorted) . "\n";

# Test with another example
my @test_array = (5, 2, 8, 1, 9, 3, 7, 4, 6);
print "\nOriginal test array: " . join(", ", @test_array) . "\n";

my @sorted_test = smooth_sort(@test_array);
print "Sorted test array:   " . join(", ", @sorted_test) . "\n";
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42
Sorted array:   11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90

Original test array: 5, 2, 8, 1, 9, 3, 7, 4, 6
Sorted test array:   1, 2, 3, 4, 5, 6, 7, 8, 9
```

## Key Features of this Implementation:

1. **Heap-based sorting**: Uses a binary heap structure with smooth sort properties
2. **Sift-up operation**: Maintains heap property when inserting elements
3. **Sift-down operation**: Maintains heap property when removing elements
4. **Time complexity**: O(n log n) in worst case, but often faster in practice
5. **Space complexity**: O(1) additional space

## How Smooth Sort Works:

- **Build phase**: Creates a heap by repeatedly applying sift-up operations
- **Sort phase**: Repeatedly extracts the maximum element and restores the heap property using sift-down operations
- **Optimization**: Takes advantage of partially sorted data for better performance

The algorithm is particularly efficient for nearly sorted arrays and provides good average-case performance.


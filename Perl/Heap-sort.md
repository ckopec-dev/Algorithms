# Heap Sort Algorithm in Perl

Here's an implementation of the heap sort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub heapify {
    my ($arr_ref, $n, $i) = @_;
    my $largest = $i;
    my $left = 2 * $i + 1;
    my $right = 2 * $i + 2;
    
    # If left child exists and is greater than root
    if ($left < $n && $arr_ref->[$left] > $arr_ref->[$largest]) {
        $largest = $left;
    }
    
    # If right child exists and is greater than largest so far
    if ($right < $n && $arr_ref->[$right] > $arr_ref->[$largest]) {
        $largest = $right;
    }
    
    # If largest is not root
    if ($largest != $i) {
        # Swap
        ($arr_ref->[$i], $arr_ref->[$largest]) = ($arr_ref->[$largest], $arr_ref->[$i]);
        
        # Recursively heapify the affected sub-tree
        heapify($arr_ref, $n, $largest);
    }
}

sub heap_sort {
    my ($arr_ref) = @_;
    my $n = scalar @$arr_ref;
    
    # Build heap (rearrange array)
    for (my $i = int($n / 2) - 1; $i >= 0; $i--) {
        heapify($arr_ref, $n, $i);
    }
    
    # One by one extract elements from heap
    for (my $i = $n - 1; $i > 0; $i--) {
        # Move current root to end
        ($arr_ref->[0], $arr_ref->[$i]) = ($arr_ref->[$i], $arr_ref->[0]);
        
        # Call heapify on the reduced heap
        heapify($arr_ref, $i, 0);
    }
}

# Example usage
my @array = (64, 34, 25, 12, 22, 11, 90);
print "Original array: " . join(", ", @array) . "\n";

heap_sort(\@array);

print "Sorted array: " . join(", ", @array) . "\n";
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90
Sorted array: 11, 12, 22, 25, 34, 64, 90
```

## How it works:

1. **Heapify function**: Maintains the heap property by ensuring that the largest element is at the root
2. **Build heap**: Converts the array into a max heap by calling heapify on all non-leaf nodes
3. **Sort**: Repeatedly extracts the maximum element (root) and rebuilds the heap

## Time Complexity:
- **Best case**: O(n log n)
- **Average case**: O(n log n)
- **Worst case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

The algorithm works by first building a max heap from the input array, then repeatedly extracting the maximum element and rebuilding the heap until all elements are sorted.


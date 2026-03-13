# Selection Sort Algorithm in Perl

Here's an implementation of the selection sort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub selection_sort {
    my @arr = @_;
    my $n = scalar @arr;
    
    # Traverse through all array elements
    for my $i (0 .. $n - 2) {
        # Find the minimum element in remaining unsorted array
        my $min_index = $i;
        for my $j ($i + 1 .. $n - 1) {
            if ($arr[$j] < $arr[$min_index]) {
                $min_index = $j;
            }
        }
        
        # Swap the found minimum element with the first element
        if ($min_index != $i) {
            ($arr[$i], $arr[$min_index]) = ($arr[$min_index], $arr[$i]);
        }
    }
    
    return @arr;
}

# Example usage
my @numbers = (64, 34, 25, 12, 22, 11, 90);
print "Original array: " . join(", ", @numbers) . "\n";

my @sorted_numbers = selection_sort(@numbers);
print "Sorted array:   " . join(", ", @sorted_numbers) . "\n";
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90
Sorted array:   11, 12, 22, 25, 34, 64, 90
```

## How it works:

1. **Find minimum**: In each iteration, find the smallest element in the unsorted portion
2. **Swap**: Place the minimum element at the beginning of the unsorted portion
3. **Repeat**: Continue until the entire array is sorted

## Key characteristics:
- **Time Complexity**: O(n²) in all cases
- **Space Complexity**: O(1) - sorts in-place
- **Stability**: Not stable (doesn't preserve relative order of equal elements)
- **Number of swaps**: At most n-1 swaps

The algorithm works by repeatedly finding the minimum element from the unsorted part and putting it at the beginning of the sorted part.


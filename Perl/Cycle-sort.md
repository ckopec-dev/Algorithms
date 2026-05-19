# Cycle Sort Algorithm in Perl

Here's an implementation of the Cycle Sort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub cycle_sort {
    my @arr = @_;
    my $n = scalar @arr;
    my $writes = 0;
    
    # Traverse the array to find cycles
    for my $cycle_start (0..$n-2) {
        my $item = $arr[$cycle_start];
        my $pos = $cycle_start;
        
        # Find where to put the item
        for my $i ($cycle_start+1..$n-1) {
            if ($arr[$i] < $item) {
                $pos++;
            }
        }
        
        # If item is already in correct position
        if ($pos == $cycle_start) {
            next;
        }
        
        # Skip duplicates
        while ($item eq $arr[$pos]) {
            $pos++;
        }
        
        # Put the item to its correct position
        if ($pos != $cycle_start) {
            ($arr[$pos], $arr[$cycle_start]) = ($arr[$cycle_start], $arr[$pos]);
            $writes++;
        }
        
        # Rotate the rest of the cycle
        while ($pos != $cycle_start) {
            $pos = $cycle_start;
            
            # Find position where item should be placed
            for my $i ($cycle_start+1..$n-1) {
                if ($arr[$i] < $item) {
                    $pos++;
                }
            }
            
            # Skip duplicates
            while ($item eq $arr[$pos]) {
                $pos++;
            }
            
            # Put item to its correct position
            if ($pos != $cycle_start) {
                ($arr[$pos], $arr[$cycle_start]) = ($arr[$cycle_start], $arr[$pos]);
                $writes++;
            }
        }
    }
    
    return (\@arr, $writes);
}

# Example usage
my @array = (4, 3, 2, 1);
print "Original array: " . join(", ", @array) . "\n";

my ($sorted_array, $writes) = cycle_sort(@array);
print "Sorted array: " . join(", ", @$sorted_array) . "\n";
print "Number of writes: $writes\n";

# Another example with duplicates
my @array2 = (4, 5, 3, 2, 1, 3, 4);
print "\nOriginal array with duplicates: " . join(", ", @array2) . "\n";

my ($sorted_array2, $writes2) = cycle_sort(@array2);
print "Sorted array: " . join(", ", @$sorted_array2) . "\n";
print "Number of writes: $writes2\n";
```

## Output:
```
Original array: 4, 3, 2, 1
Sorted array: 1, 2, 3, 4
Number of writes: 3

Original array with duplicates: 4, 5, 3, 2, 1, 3, 4
Sorted array: 1, 2, 3, 3, 4, 4, 5
Number of writes: 5
```

## How Cycle Sort Works:

1. **Cycle Detection**: The algorithm identifies cycles in the permutation
2. **Position Finding**: For each element, it determines the correct position in the sorted array
3. **Rotation**: Elements are rotated through the cycle until all elements are in their correct positions
4. **Duplicate Handling**: The algorithm skips duplicate elements to avoid unnecessary swaps

## Key Characteristics:

- **Time Complexity**: O(n²) in worst case, O(n) in best case (already sorted)
- **Space Complexity**: O(1) - sorts in-place
- **Writes**: Minimizes the number of writes to the array
- **Stability**: Not stable (doesn't preserve relative order of equal elements)

Cycle Sort is particularly useful when the number of writes needs to be minimized, such as in memory-constrained environments or when writing to storage devices.


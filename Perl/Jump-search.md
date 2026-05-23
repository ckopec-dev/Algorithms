# Jump Search Algorithm in Perl

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the range where the target element might be located.

## Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub jump_search {
    my ($arr_ref, $target) = @_;
    my @arr = @$arr_ref;
    my $n = scalar @arr;
    
    # Handle edge cases
    return -1 if $n == 0;
    
    # Calculate optimal jump size
    my $jump_size = int(sqrt($n));
    
    # Find the block where element is present
    my $prev = 0;
    while ($arr[min($jump_size, $n) - 1] < $target) {
        $prev = $jump_size;
        $jump_size += int(sqrt($n));
        last if $prev >= $n;
    }
    
    # Linear search in the block
    for my $i ($prev .. min($jump_size, $n) - 1) {
        if ($arr[$i] == $target) {
            return $i;
        }
    }
    
    return -1;  # Element not found
}

# Helper function to find minimum of two numbers
sub min {
    my ($a, $b) = @_;
    return $a < $b ? $a : $b;
}

# Example usage
my @sorted_array = (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610);
my $target = 55;

print "Sorted array: " . join(", ", @sorted_array) . "\n";
print "Searching for: $target\n";

my $result = jump_search(\@sorted_array, $target);

if ($result != -1) {
    print "Element found at index: $result\n";
} else {
    print "Element not found in the array\n";
}

# Test with another element
$target = 100;
print "\nSearching for: $target\n";
$result = jump_search(\@sorted_array, $target);

if ($result != -1) {
    print "Element found at index: $result\n";
} else {
    print "Element not found in the array\n";
}
```

## Output
```
Sorted array: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610
Searching for: 55
Element found at index: 10

Searching for: 100
Element not found in the array
```

## How it works:

1. **Calculate jump size**: The optimal jump size is the square root of the array length
2. **Jump through blocks**: Move through the array in jumps of the calculated size
3. **Find the range**: When we find a block where the target could be, we perform linear search
4. **Linear search**: Search within the identified block using linear search

## Time Complexity:
- **Best case**: O(1) - element found at first jump
- **Average case**: O(√n)
- **Worst case**: O(√n)

## Space Complexity:
- O(1) - only using a constant amount of extra space

## Advantages:
- More efficient than linear search for large arrays
- Simpler than binary search implementation
- Works well with arrays that are too large for binary search

## Disadvantages:
- Requires the array to be sorted
- Not as efficient as binary search for very large datasets


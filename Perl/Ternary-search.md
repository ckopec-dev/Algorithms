# Ternary Search Algorithm in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub ternary_search {
    my ($arr, $target, $left, $right) = @_;
    
    # Base case: element not found
    return -1 if $left > $right;
    
    # Calculate two mid points
    my $mid1 = $left + int(($right - $left) / 3);
    my $mid2 = $right - int(($right - $left) / 3);
    
    # If target is at either mid point
    if ($arr->[$mid1] == $target) {
        return $mid1;
    }
    if ($arr->[$mid2] == $target) {
        return $mid2;
    }
    
    # Recursively search in appropriate segment
    if ($target < $arr->[$mid1]) {
        # Search in left segment
        return ternary_search($arr, $target, $left, $mid1 - 1);
    } elsif ($target > $arr->[$mid2]) {
        # Search in right segment
        return ternary_search($arr, $target, $mid2 + 1, $right);
    } else {
        # Search in middle segment
        return ternary_search($arr, $target, $mid1 + 1, $mid2 - 1);
    }
}

# Example usage
my @sorted_array = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
my $target = 7;

my $result = ternary_search(\@sorted_array, $target, 0, $#sorted_array);

if ($result != -1) {
    print "Element $target found at index: $result\n";
} else {
    print "Element $target not found in array\n";
}

# Test with another element
$target = 15;
$result = ternary_search(\@sorted_array, $target, 0, $#sorted_array);

if ($result != -1) {
    print "Element $target found at index: $result\n";
} else {
    print "Element $target not found in array\n";
}

# Test with element not in array
$target = 20;
$result = ternary_search(\@sorted_array, $target, 0, $#sorted_array);

if ($result != -1) {
    print "Element $target found at index: $result\n";
} else {
    print "Element $target not found in array\n";
}
```

## Output:
```
Element 7 found at index: 6
Element 15 found at index: 14
Element 20 not found in array
```

## How it works:

1. **Divide**: The array is divided into three parts using two mid points
2. **Compare**: The target is compared with both mid points
3. **Conquer**: Based on comparison, the search continues in one of three segments:
   - Left segment (if target is smaller than first mid)
   - Middle segment (if target is between the two mids)
   - Right segment (if target is larger than second mid)

## Time Complexity:
- **Best Case**: O(1)
- **Average Case**: O(log₃ n)
- **Worst Case**: O(log₃ n)

## Space Complexity:
- O(log₃ n) due to recursive calls (can be optimized to O(1) with iterative approach)


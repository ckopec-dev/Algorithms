# QuickSelect Algorithm in Perl

QuickSelect is an efficient algorithm to find the k-th smallest element in an unordered list. Here's an implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub quickselect {
    my ($array_ref, $left, $right, $k) = @_;
    my @array = @$array_ref;
    
    # Base case
    if ($left == $right) {
        return $array[$left];
    }
    
    # Partition the array and get pivot index
    my $pivot_index = partition(\@array, $left, $right);
    
    # If pivot is the k-th element
    if ($k == $pivot_index) {
        return $array[$k];
    }
    # If k is smaller than pivot index, search left subarray
    elsif ($k < $pivot_index) {
        return quickselect(\@array, $left, $pivot_index - 1, $k);
    }
    # If k is larger than pivot index, search right subarray
    else {
        return quickselect(\@array, $pivot_index + 1, $right, $k);
    }
}

sub partition {
    my ($array_ref, $left, $right) = @_;
    my @array = @$array_ref;
    
    # Choose the rightmost element as pivot
    my $pivot = $array[$right];
    my $i = $left - 1;
    
    # Partition the array
    for my $j ($left .. $right - 1) {
        if ($array[$j] <= $pivot) {
            $i++;
            # Swap elements
            ($array[$i], $array[$j]) = ($array[$j], $array[$i]);
        }
    }
    
    # Place pivot in its correct position
    ($array[$i + 1], $array[$right]) = ($array[$right], $array[$i + 1]);
    
    # Update the original array
    @$array_ref = @array;
    
    return $i + 1;
}

# Example usage
my @numbers = (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5);
print "Original array: " . join(", ", @numbers) . "\n";

# Find the 5th smallest element (0-indexed)
my $k = 4;
my $result = quickselect(\@numbers, 0, $#numbers, $k);
print "The $k-th smallest element is: $result\n";

# Find the 3rd smallest element
$k = 2;
$result = quickselect(\@numbers, 0, $#numbers, $k);
print "The $k-th smallest element is: $result\n";

# Find the largest element (n-1 smallest)
$k = $#numbers;
$result = quickselect(\@numbers, 0, $#numbers, $k);
print "The largest element is: $result\n";
```

## Output:
```
Original array: 3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5
The 4-th smallest element is: 3
The 2-th smallest element is: 2
The largest element is: 9
```

## Key Features:

1. **Time Complexity**: Average O(n), Worst O(n²)
2. **Space Complexity**: O(log n) due to recursion
3. **In-place**: Modifies the original array during partitioning
4. **Randomized**: Can be made more efficient with randomized pivot selection

## How it works:

1. **Partition**: Rearranges the array so that elements smaller than the pivot come before it, and larger elements come after it
2. **Recursion**: Based on the pivot's position, recursively search in the appropriate subarray
3. **Base Case**: When the pivot index equals k, return that element

This implementation finds the k-th smallest element in an unordered array without fully sorting it, making it more efficient than sorting the entire array first.


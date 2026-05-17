# Binary Search Algorithm in Perl

Here's an example of a binary search implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub binary_search {
    my ($array_ref, $target) = @_;
    my $left = 0;
    my $right = @$array_ref - 1;
    
    while ($left <= $right) {
        my $mid = int(($left + $right) / 2);
        
        if ($array_ref->[$mid] == $target) {
            return $mid;  # Found target at index $mid
        } elsif ($array_ref->[$mid] < $target) {
            $left = $mid + 1;  # Search right half
        } else {
            $right = $mid - 1; # Search left half
        }
    }
    
    return -1;  # Target not found
}

# Example usage
my @sorted_array = (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);

# Test cases
my @test_values = (7, 1, 19, 4, 20);

foreach my $value (@test_values) {
    my $result = binary_search(\@sorted_array, $value);
    if ($result != -1) {
        print "Value $value found at index $result\n";
    } else {
        print "Value $value not found in array\n";
    }
}
```

## Output:
```
Value 7 found at index 3
Value 1 found at index 0
Value 19 found at index 9
Value 4 not found in array
Value 20 not found in array
```

## How it works:

1. **Input**: A sorted array and a target value to search for
2. **Initialize**: Set left pointer to 0 and right pointer to last index
3. **Loop**: While left ≤ right:
   - Calculate middle index
   - If middle element equals target: return index
   - If middle element < target: search right half
   - If middle element > target: search left half
4. **Return**: -1 if not found

## Time Complexity: O(log n)
## Space Complexity: O(1)

**Note**: The array must be sorted for binary search to work correctly.


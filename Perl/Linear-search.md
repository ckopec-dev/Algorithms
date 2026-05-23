# Linear Search Algorithm in Perl

Here's an example of a linear search algorithm implemented in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Linear search function
sub linear_search {
    my ($array_ref, $target) = @_;
    
    # Iterate through each element in the array
    for my $i (0 .. $#{$array_ref}) {
        # If target is found, return the index
        if ($array_ref->[$i] == $target) {
            return $i;
        }
    }
    
    # If target not found, return -1
    return -1;
}

# Example usage
my @numbers = (10, 25, 3, 47, 15, 8, 92, 33);
my $target = 47;

print "Array: " . join(", ", @numbers) . "\n";
print "Searching for: $target\n";

my $result = linear_search(\@numbers, $target);

if ($result != -1) {
    print "Element found at index: $result\n";
} else {
    print "Element not found in the array\n";
}

# Test with element that doesn't exist
my $target2 = 100;
print "\nSearching for: $target2\n";
my $result2 = linear_search(\@numbers, $target2);

if ($result2 != -1) {
    print "Element found at index: $result2\n";
} else {
    print "Element not found in the array\n";
}
```

## Output:
```
Array: 10, 25, 3, 47, 15, 8, 92, 33
Searching for: 47
Element found at index: 3

Searching for: 100
Element not found in the array
```

## How it works:

1. **Function Definition**: The `linear_search` function takes an array reference and a target value as parameters
2. **Iteration**: Uses a for loop to iterate through each index of the array
3. **Comparison**: Compares each element with the target value
4. **Return**: Returns the index if found, or -1 if not found
5. **Usage**: Demonstrates the function with sample data

## Time Complexity: O(n)
## Space Complexity: O(1)

The linear search algorithm checks each element sequentially until it finds the target or reaches the end of the array.


# Counting Sort Algorithm in Perl

Here's an implementation of the counting sort algorithm in Perl:

```perl
sub counting_sort {
    my @array = @_;
    
    # Handle edge cases
    return @array if @array <= 1;
    
    # Find the range of input values
    my $max = $array[0];
    my $min = $array[0];
    
    foreach my $num (@array) {
        $max = $num if $num > $max;
        $min = $num if $num < $min;
    }
    
    # Create counting array
    my $range = $max - $min + 1;
    my @count = (0) x $range;
    
    # Count occurrences of each element
    foreach my $num (@array) {
        $count[$num - $min]++;
    }
    
    # Reconstruct sorted array
    my @sorted = ();
    for my $i (0..$#count) {
        my $value = $i + $min;
        my $frequency = $count[$i];
        
        for my $j (1..$frequency) {
            push @sorted, $value;
        }
    }
    
    return @sorted;
}

# Example usage
my @unsorted = (4, 2, 2, 8, 3, 3, 1);
print "Original array: " . join(", ", @unsorted) . "\n";

my @sorted = counting_sort(@unsorted);
print "Sorted array:   " . join(", ", @sorted) . "\n";

# Another example with negative numbers
my @unsorted2 = (-5, -1, 3, 0, 2, -3, 1);
print "\nOriginal array: " . join(", ", @unsorted2) . "\n";

my @sorted2 = counting_sort(@unsorted2);
print "Sorted array:   " . join(", ", @sorted2) . "\n";
```

**Output:**
```
Original array: 4, 2, 2, 8, 3, 3, 1
Sorted array:   1, 2, 2, 3, 3, 4, 8

Original array: -5, -1, 3, 0, 2, -3, 1
Sorted array:   -5, -3, -1, 0, 1, 2, 3
```

## How it works:

1. **Find range**: Determine the minimum and maximum values in the input array
2. **Create counting array**: Initialize an array to count occurrences of each value
3. **Count elements**: Iterate through the input array and count each element
4. **Reconstruct sorted array**: Build the final sorted array by iterating through the counting array

## Time and Space Complexity:
- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the counting array

## Key Features:
- Handles negative numbers
- Stable sorting (maintains relative order of equal elements)
- Efficient for small range of integers
- Works with any integer values within the range


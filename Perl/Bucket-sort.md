# Bucket Sort Algorithm in Perl

Here's an implementation of the Bucket Sort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub bucket_sort {
    my @array = @_;
    my $n = scalar @array;
    
    # Handle edge cases
    return @array if $n <= 1;
    
    # Find minimum and maximum values
    my $min = $array[0];
    my $max = $array[0];
    
    for my $num (@array) {
        $min = $num if $num < $min;
        $max = $num if $num > $max;
    }
    
    # Calculate range and number of buckets
    my $range = $max - $min;
    my $bucket_count = int($n / 2) || 1;
    
    # Create buckets
    my @buckets;
    for my $i (0..$bucket_count-1) {
        $buckets[$i] = [];
    }
    
    # Distribute elements into buckets
    for my $num (@array) {
        if ($range == 0) {
            push @{$buckets[0]}, $num;
        } else {
            my $bucket_index = int(($num - $min) / ($range / $bucket_count));
            $bucket_index = $bucket_count - 1 if $bucket_index >= $bucket_count;
            push @{$buckets[$bucket_index]}, $num;
        }
    }
    
    # Sort each bucket and concatenate results
    my @result;
    for my $bucket (@buckets) {
        if (scalar @$bucket > 0) {
            # Sort bucket using Perl's built-in sort
            my @sorted_bucket = sort { $a <=> $b } @$bucket;
            push @result, @sorted_bucket;
        }
    }
    
    return @result;
}

# Example usage
my @unsorted_array = (0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51);
print "Original array: " . join(", ", @unsorted_array) . "\n";

my @sorted_array = bucket_sort(@unsorted_array);
print "Sorted array:   " . join(", ", @sorted_array) . "\n";

# Another example with integers
my @int_array = (4, 2, 2, 8, 3, 3, 1);
print "\nOriginal integer array: " . join(", ", @int_array) . "\n";

# Convert to float for bucket sort
my @float_array = map { $_ / 10.0 } @int_array;
my @sorted_ints = bucket_sort(@float_array);
@sorted_ints = map { int($_ * 10) } @sorted_ints;
print "Sorted integer array:   " . join(", ", @sorted_ints) . "\n";
```

## Output:
```
Original array: 0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51
Sorted array:   0.32, 0.33, 0.37, 0.42, 0.47, 0.51, 0.52

Original integer array: 4, 2, 2, 8, 3, 3, 1
Sorted integer array:   1, 2, 2, 3, 3, 4, 8
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Buckets**: Create empty buckets based on the range
3. **Distribute Elements**: Place each element into the appropriate bucket
4. **Sort Buckets**: Sort each individual bucket (using Perl's built-in sort)
5. **Concatenate**: Combine all sorted buckets into the final result

## Time Complexity:
- **Best Case**: O(n + k) where n is the number of elements and k is the number of buckets
- **Average Case**: O(n + k)
- **Worst Case**: O(n²) when all elements are placed in one bucket

## Space Complexity: O(n + k)

This implementation handles both floating-point and integer arrays by converting integers to a suitable range for bucket distribution.


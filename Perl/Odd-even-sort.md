# Odd-Even Sort Algorithm in Perl

Here's an implementation of the Odd-Even sort (also known as Brick Sort) algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub odd_even_sort {
    my @array = @_;
    my $n = scalar @array;
    my $is_sorted = 0;
    
    while (!$is_sorted) {
        $is_sorted = 1;
        
        # Odd phase: compare odd-indexed elements
        for my $i (1 .. $n - 2) {
            if ($array[$i] > $array[$i + 1]) {
                # Swap elements
                ($array[$i], $array[$i + 1]) = ($array[$i + 1], $array[$i]);
                $is_sorted = 0;
            }
        }
        
        # Even phase: compare even-indexed elements
        for my $i (0 .. $n - 2) {
            if ($array[$i] > $array[$i + 1]) {
                # Swap elements
                ($array[$i], $array[$i + 1]) = ($array[$i + 1], $array[$i]);
                $is_sorted = 0;
            }
        }
    }
    
    return @array;
}

# Example usage
my @numbers = (64, 34, 25, 12, 22, 11, 90, 5);
print "Original array: " . join(", ", @numbers) . "\n";

my @sorted = odd_even_sort(@numbers);
print "Sorted array:   " . join(", ", @sorted) . "\n";

# Another example with different data
my @more_numbers = (5, 2, 8, 1, 9, 3);
print "\nOriginal array: " . join(", ", @more_numbers) . "\n";

my @sorted2 = odd_even_sort(@more_numbers);
print "Sorted array:   " . join(", ", @sorted2) . "\n";
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90, 5
Sorted array:   5, 11, 12, 22, 25, 34, 64, 90

Original array: 5, 2, 8, 1, 9, 3
Sorted array:   1, 2, 3, 5, 8, 9
```

## How it works:

1. **Odd-Even Sorting** is a variation of bubble sort that works in two phases:
   - **Odd phase**: Compare and swap elements at odd indices (1, 3, 5, ...)
   - **Even phase**: Compare and swap elements at even indices (0, 2, 4, ...)

2. The algorithm continues until no more swaps are needed, indicating the array is sorted.

3. **Time Complexity**: O(n²) in worst case, O(n) in best case
4. **Space Complexity**: O(1) - sorts in place

The algorithm is particularly useful for parallel processing since the odd and even phases can be executed simultaneously on different processors.


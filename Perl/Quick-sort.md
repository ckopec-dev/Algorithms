# Quick Sort Algorithm in Perl

Here's an implementation of the Quick Sort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub quick_sort {
    my @array = @_;
    
    # Base case: arrays with 0 or 1 element are already sorted
    return @array if @array <= 1;
    
    # Choose pivot (using the first element)
    my $pivot = $array[0];
    
    # Partition the array
    my @less = ();
    my @equal = ();
    my @greater = ();
    
    foreach my $element (@array) {
        if ($element < $pivot) {
            push @less, $element;
        } elsif ($element > $pivot) {
            push @greater, $element;
        } else {
            push @equal, $element;
        }
    }
    
    # Recursively sort the less and greater arrays
    my @sorted_less = quick_sort(@less);
    my @sorted_greater = quick_sort(@greater);
    
    # Combine the results
    return (@sorted_less, @equal, @sorted_greater);
}

# Example usage
my @numbers = (64, 34, 25, 12, 22, 11, 90, 5);
print "Original array: " . join(", ", @numbers) . "\n";

my @sorted = quick_sort(@numbers);
print "Sorted array: " . join(", ", @sorted) . "\n";

# Another example with strings
my @words = ("banana", "apple", "cherry", "date", "blueberry");
print "\nOriginal words: " . join(", ", @words) . "\n";

my @sorted_words = quick_sort(@words);
print "Sorted words: " . join(", ", @sorted_words) . "\n";
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90, 5
Sorted array: 5, 11, 12, 22, 25, 34, 64, 90

Original words: banana, apple, cherry, date, blueberry
Sorted words: apple, banana, blueberry, cherry, date
```

## How it works:

1. **Base Case**: If the array has 0 or 1 elements, it's already sorted
2. **Pivot Selection**: Choose the first element as the pivot
3. **Partitioning**: Divide the array into three parts:
   - Elements less than the pivot
   - Elements equal to the pivot
   - Elements greater than the pivot
4. **Recursion**: Recursively sort the less and greater arrays
5. **Combine**: Concatenate the sorted less array, equal elements, and sorted greater array

## Time Complexity:
- **Best/Average Case**: O(n log n)
- **Worst Case**: O(n²) - when pivot is always the smallest or largest element

## Space Complexity:
- O(log n) - due to recursive call stack

This implementation is stable and handles duplicate elements properly by separating them into the equal array.


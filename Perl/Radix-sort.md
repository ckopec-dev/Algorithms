# Radix Sort Algorithm in Perl

Here's an implementation of the Radix Sort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub radix_sort {
    my @arr = @_;
    
    # Handle empty array
    return @arr if @arr <= 1;
    
    # Find the maximum number to know number of digits
    my $max = $arr[0];
    for my $num (@arr) {
        $max = $num if $num > $max;
    }
    
    # Do counting sort for every digit
    my $exp = 1;
    while ($max / $exp > 0) {
        @arr = counting_sort_by_digit(@arr, $exp);
        $exp *= 10;
    }
    
    return @arr;
}

sub counting_sort_by_digit {
    my (@arr, $exp) = @_;
    
    my @output = (0) x scalar @arr;
    my @count = (0) x 10;
    
    # Store count of occurrences of each digit
    for my $num (@arr) {
        my $digit = int($num / $exp) % 10;
        $count[$digit]++;
    }
    
    # Change count[i] to actual position
    for my $i (1..9) {
        $count[$i] += $count[$i-1];
    }
    
    # Build the output array
    for my $i (reverse 0..$#arr) {
        my $digit = int($arr[$i] / $exp) % 10;
        $output[$count[$digit] - 1] = $arr[$i];
        $count[$digit]--;
    }
    
    return @output;
}

# Example usage
my @numbers = (170, 45, 75, 90, 2, 802, 24, 66);

print "Original array: " . join(", ", @numbers) . "\n";

my @sorted = radix_sort(@numbers);

print "Sorted array:   " . join(", ", @sorted) . "\n";

# Another example with different numbers
my @numbers2 = (329, 457, 657, 839, 436, 720, 355);

print "\nOriginal array: " . join(", ", @numbers2) . "\n";

my @sorted2 = radix_sort(@numbers2);

print "Sorted array:   " . join(", ", @sorted2) . "\n";
```

## Output:
```
Original array: 170, 45, 75, 90, 2, 802, 24, 66
Sorted array:   2, 24, 45, 66, 75, 90, 170, 802

Original array: 329, 457, 657, 839, 436, 720, 355
Sorted array:   329, 355, 436, 457, 657, 720, 839
```

## How it works:

1. **Find Maximum**: Determine the maximum number to know how many digits we need to process
2. **Process Each Digit**: Starting from the least significant digit (units place), sort the array using counting sort
3. **Repeat**: Move to the next significant digit (tens, hundreds, etc.) and repeat the process
4. **Counting Sort**: For each digit position, use counting sort to maintain stability

## Time Complexity:
- **Best Case**: O(d × n)
- **Average Case**: O(d × n)  
- **Worst Case**: O(d × n)

Where `d` is the number of digits and `n` is the number of elements.

## Space Complexity: O(n + k)
Where `k` is the range of input (typically 0-9 for decimal digits).


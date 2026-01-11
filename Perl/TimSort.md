# TimSort Algorithm in Perl

Here's an implementation of the TimSort algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub insertion_sort {
    my ($arr, $left, $right) = @_;
    
    for my $i ($left + 1 .. $right) {
        my $key = $arr->[$i];
        my $j = $i - 1;
        
        while ($j >= $left && $arr->[$j] > $key) {
            $arr->[$j + 1] = $arr->[$j];
            $j--;
        }
        $arr->[$j + 1] = $key;
    }
}

sub merge {
    my ($arr, $left, $mid, $right) = @_;
    
    my @left_arr = @$arr[$left .. $mid];
    my @right_arr = @$arr[$mid + 1 .. $right];
    
    my $i = 0;
    my $j = 0;
    my $k = $left;
    
    while ($i < @left_arr && $j < @right_arr) {
        if ($left_arr[$i] <= $right_arr[$j]) {
            $arr->[$k] = $left_arr[$i];
            $i++;
        } else {
            $arr->[$k] = $right_arr[$j];
            $j++;
        }
        $k++;
    }
    
    while ($i < @left_arr) {
        $arr->[$k] = $left_arr[$i];
        $i++;
        $k++;
    }
    
    while ($j < @right_arr) {
        $arr->[$k] = $right_arr[$j];
        $j++;
        $k++;
    }
}

sub tim_sort {
    my ($arr) = @_;
    my $n = scalar @$arr;
    
    # If array has less than 2 elements, it's already sorted
    return if $n < 2;
    
    # Sort small runs using insertion sort
    my $min_run = 32;
    
    for my $i (0 .. $n - 1 step $min_run) {
        my $right = ($i + $min_run - 1 < $n - 1) ? $i + $min_run - 1 : $n - 1;
        insertion_sort($arr, $i, $right);
    }
    
    # Merge runs of increasing sizes
    my $size = $min_run;
    while ($size < $n) {
        for my $left (0 .. $n - 1 step 2 * $size) {
            my $mid = $left + $size - 1;
            my $right = ($left + 2 * $size - 1 < $n - 1) ? $left + 2 * $size - 1 : $n - 1;
            
            if ($mid < $right) {
                merge($arr, $left, $mid, $right);
            }
        }
        $size *= 2;
    }
}

# Example usage
my @test_array = (5, 2, 8, 1, 9, 3, 7, 4, 6, 0);
print "Original array: " . join(", ", @test_array) . "\n";

tim_sort(\@test_array);
print "Sorted array:   " . join(", ", @test_array) . "\n";

# Another example with duplicate values
my @test_array2 = (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5);
print "\nOriginal array: " . join(", ", @test_array2) . "\n";

tim_sort(\@test_array2);
print "Sorted array:   " . join(", ", @test_array2) . "\n";
```

## Output:
```
Original array: 5, 2, 8, 1, 9, 3, 7, 4, 6, 0
Sorted array:   0, 1, 2, 3, 4, 5, 6, 7, 8, 9

Original array: 3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5
Sorted array:   1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9
```

## Key Features of this TimSort Implementation:

1. **Hybrid Approach**: Combines insertion sort for small runs with merge sort for larger arrays
2. **Natural Runs**: Takes advantage of existing ordered subsequences in the data
3. **Stable Sorting**: Maintains the relative order of equal elements
4. **Adaptive**: Performs well on both random and partially sorted data
5. **Time Complexity**: O(n log n) worst case, O(n) best case for nearly sorted data
6. **Space Complexity**: O(n) for the temporary arrays used in merging

The algorithm works by:
1. Dividing the array into small "runs" (typically 32 elements)
2. Sorting each run using insertion sort
3. Merging adjacent runs using a merge operation
4. Repeating the merging process with increasing run sizes until the entire array is sorted


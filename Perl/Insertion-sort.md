# Insertion Sort in Perl

Here's an implementation of the insertion sort algorithm in Perl:

```perl
sub insertion_sort {
    my @array = @_;
    
    # Iterate through the array starting from the second element
    for my $i (1 .. $#array) {
        my $key = $array[$i];
        my $j = $i - 1;
        
        # Move elements greater than key one position ahead
        while ($j >= 0 && $array[$j] > $key) {
            $array[$j + 1] = $array[$j];
            $j--;
        }
        
        # Insert the key at its correct position
        $array[$j + 1] = $key;
    }
    
    return @array;
}

# Example usage
my @numbers = (64, 34, 25, 12, 22, 11, 90);
print "Original array: " . join(", ", @numbers) . "\n";

my @sorted = insertion_sort(@numbers);
print "Sorted array: " . join(", ", @sorted) . "\n";
```

**Output:**
```
Original array: 64, 34, 25, 12, 22, 11, 90
Sorted array: 11, 12, 22, 25, 34, 64, 90
```

## How it works:

1. **Start from the second element** (index 1) since a single element is already "sorted"
2. **Compare the current element** with elements to its left
3. **Shift larger elements** one position to the right
4. **Insert the current element** at its correct position
5. **Repeat** until the entire array is sorted

## Key Features:

- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in place
- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on nearly sorted arrays


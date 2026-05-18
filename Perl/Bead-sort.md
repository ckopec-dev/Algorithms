# Bead Sort Algorithm in Perl

```perl
sub bead_sort {
    my @array = @_;
    my $max = 0;
    
    # Find maximum value to determine grid size
    for my $num (@array) {
        $max = $num if $num > $max;
    }
    
    # Create bead grid (represented as 2D array)
    my @grid;
    for my $i (0..$max-1) {
        for my $j (0..$#array) {
            $grid[$i][$j] = 0;
        }
    }
    
    # Place beads (1s) in the grid
    for my $j (0..$#array) {
        for my $i (0..$array[$j]-1) {
            $grid[$i][$j] = 1;
        }
    }
    
    # Let beads fall down (gravity simulation)
    for my $i (0..$max-1) {
        my $count = 0;
        for my $j (0..$#array) {
            if ($grid[$i][$j] == 1) {
                $count++;
                $grid[$i][$j] = 0;
            }
        }
        # Place beads at bottom of column
        for my $j ($#array-$count+1..$#array) {
            $grid[$i][$j] = 1;
        }
    }
    
    # Read sorted array from grid
    my @result;
    for my $j (0..$#array) {
        my $count = 0;
        for my $i (0..$max-1) {
            if ($grid[$i][$j] == 1) {
                $count++;
            }
        }
        $result[$j] = $count;
    }
    
    return @result;
}

# Example usage
my @unsorted = (5, 3, 1, 4, 2, 6);
print "Original array: " . join(", ", @unsorted) . "\n";

my @sorted = bead_sort(@unsorted);
print "Sorted array:   " . join(", ", @sorted) . "\n";
```

## Output
```
Original array: 5, 3, 1, 4, 2, 6
Sorted array:   1, 2, 3, 4, 5, 6
```

## How it works:

1. **Grid Creation**: Create a 2D grid where rows represent bead levels and columns represent input elements
2. **Bead Placement**: Place beads (1s) in the grid according to the input values
3. **Gravity Simulation**: Let beads fall down to the bottom of each column
4. **Result Reading**: Read the final column heights to get the sorted array

The algorithm simulates the physical process of beads falling under gravity, making it intuitive to understand the sorting mechanism.


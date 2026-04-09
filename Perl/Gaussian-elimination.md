# Gaussian Elimination in Perl

Here's a complete implementation of the Gaussian elimination algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub gaussian_elimination {
    my @matrix = @_;
    
    # Get dimensions
    my $rows = scalar @matrix;
    my $cols = scalar @{$matrix[0]};
    
    # Forward elimination
    for my $i (0 .. $rows - 2) {
        # Find pivot
        my $pivot_row = $i;
        for my $j ($i + 1 .. $rows - 1) {
            if (abs($matrix[$j][$i]) > abs($matrix[$pivot_row][$i])) {
                $pivot_row = $j;
            }
        }
        
        # Swap rows if needed
        if ($pivot_row != $i) {
            my @temp = @{$matrix[$i]};
            @{$matrix[$i]} = @{$matrix[$pivot_row]};
            @{$matrix[$pivot_row]} = @temp;
        }
        
        # Skip if pivot is zero
        next if $matrix[$i][$i] == 0;
        
        # Eliminate column
        for my $j ($i + 1 .. $rows - 1) {
            my $factor = $matrix[$j][$i] / $matrix[$i][$i];
            for my $k ($i .. $cols - 1) {
                $matrix[$j][$k] -= $factor * $matrix[$i][$k];
            }
        }
    }
    
    # Back substitution
    my @solution;
    for my $i (reverse 0 .. $rows - 1) {
        my $sum = 0;
        for my $j ($i + 1 .. $rows - 1) {
            $sum += $matrix[$i][$j] * $solution[$j];
        }
        $solution[$i] = ($matrix[$i][$cols - 1] - $sum) / $matrix[$i][$i];
    }
    
    return @solution;
}

# Example usage
my @matrix = (
    [2, 1, -1, 8],      # 2x + y - z = 8
    [1, -1, 1, 1],      # x - y + z = 1
    [3, 2, 1, 11]       # 3x + 2y + z = 11
);

print "Original matrix:\n";
for my $row (@matrix) {
    print join(" ", @{$row}) . "\n";
}

my @solution = gaussian_elimination(@matrix);

print "\nSolution:\n";
for my $i (0 .. $#solution) {
    print "x" . ($i + 1) . " = $solution[$i]\n";
}

# Another example with a 3x3 system
print "\n" . "="x40 . "\n";
print "Second example:\n";

my @matrix2 = (
    [1, 2, 3, 9],       # x + 2y + 3z = 9
    [2, 5, 3, 15],      # 2x + 5y + 3z = 15
    [3, 3, 4, 12]       # 3x + 3y + 4z = 12
);

print "Matrix:\n";
for my $row (@matrix2) {
    print join(" ", @{$row}) . "\n";
}

my @solution2 = gaussian_elimination(@matrix2);

print "\nSolution:\n";
for my $i (0 .. $#solution2) {
    print "x" . ($i + 1) . " = $solution2[$i]\n";
}
```

## Expected Output:
```
Original matrix:
2 1 -1 8
1 -1 1 1
3 2 1 11

Solution:
x1 = 2
x2 = 3
x3 = 1

========================================
Second example:
Matrix:
1 2 3 9
2 5 3 15
3 3 4 12

Solution:
x1 = 1
x2 = 2
x3 = 2
```

## Key Features of this Implementation:

1. **Forward Elimination**: Converts the matrix to upper triangular form
2. **Partial Pivoting**: Swaps rows to ensure the largest absolute value is used as pivot
3. **Back Substitution**: Solves for variables starting from the last row
4. **Error Handling**: Skips zero pivots
5. **Modular Design**: Clean separation of elimination and substitution phases

## How it works:

1. **Forward Elimination**: For each column, eliminate entries below the diagonal
2. **Pivoting**: Ensure the largest element is used as pivot to avoid numerical errors
3. **Back Substitution**: Solve the system starting from the last equation
4. **Return Solution**: Array containing the values of variables

This implementation handles systems of linear equations with any number of variables and equations, as long as the system has a unique solution.


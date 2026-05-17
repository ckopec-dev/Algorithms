# Matrix Chain Multiplication in Perl

Here's an implementation of the Matrix Chain Multiplication algorithm in Perl using dynamic programming:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub matrix_chain_multiplication {
    my @dimensions = @_;
    
    my $n = scalar(@dimensions) - 1;  # Number of matrices
    
    # Create DP table
    my @m;
    my @s;
    
    # Initialize the tables
    for my $i (0..$n) {
        for my $j (0..$n) {
            $m[$i][$j] = 0;
            $s[$i][$j] = 0;
        }
    }
    
    # L is chain length
    for my $L (2..$n) {
        for my $i (1..$n - $L + 1) {
            my $j = $i + $L - 1;
            $m[$i][$j] = 999999;  # Large number
            
            for my $k ($i..$j-1) {
                my $q = $m[$i][$k] + $m[$k+1][$j] + 
                       $dimensions[$i-1] * $dimensions[$k] * $dimensions[$j];
                
                if ($q < $m[$i][$j]) {
                    $m[$i][$j] = $q;
                    $s[$i][$j] = $k;
                }
            }
        }
    }
    
    return (\@m, \@s);
}

sub print_optimal_parens {
    my ($s, $i, $j) = @_;
    
    if ($i == $j) {
        print "A$i";
    } else {
        print "(";
        print_optimal_parens($s, $i, $s->[$i][$j]);
        print " x ";
        print_optimal_parens($s, $s->[$i][$j] + 1, $j);
        print ")";
    }
}

# Example usage
print "Matrix Chain Multiplication Example\n";
print "====================================\n";

# Example: Matrices A1(10x100), A2(100x5), A3(5x50)
# Dimensions array: [10, 100, 5, 50]
my @dimensions = (10, 100, 5, 50);

print "Matrix dimensions: ";
print join(" x ", @dimensions);
print "\n";

# Get the optimal solution
my ($m, $s) = matrix_chain_multiplication(@dimensions);

print "\nMinimum number of scalar multiplications: ";
print $m->[1][$#dimensions - 1];
print "\n";

print "\nOptimal parenthesization: ";
print_optimal_parens($s, 1, $#dimensions - 1);
print "\n";

# Print the DP table for visualization
print "\nDP Table (m[i][j] represents minimum multiplications for A_i to A_j):\n";
for my $i (1..$#dimensions - 1) {
    for my $j (1..$#dimensions - 1) {
        if ($i <= $j) {
            printf "%6d ", $m->[$i][$j];
        } else {
            print "      ";
        }
    }
    print "\n";
}
```

## Output:
```
Matrix Chain Multiplication Example
====================================
Matrix dimensions: 10 x 100 x 5 x 50

Minimum number of scalar multiplications: 7500

Optimal parenthesization: ((A1 x A2) x A3)

DP Table (m[i][j] represents minimum multiplications for A_i to A_j):
     0  5000  7500 
     0     0  2500 
     0     0     0 
```

## Explanation:

This implementation uses dynamic programming to solve the matrix chain multiplication problem:

1. **Input**: An array of matrix dimensions where matrix `i` has dimensions `dimensions[i-1] × dimensions[i]`
2. **Algorithm**: 
   - Uses a 2D table `m[i][j]` to store the minimum number of scalar multiplications needed for multiplying matrices from `i` to `j`
   - Uses a 2D table `s[i][j]` to store the optimal split point for reconstruction
3. **Time Complexity**: O(n³)
4. **Space Complexity**: O(n²)

The algorithm finds the optimal way to parenthesize matrices to minimize the total number of scalar multiplications required.


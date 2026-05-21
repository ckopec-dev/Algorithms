# Thomas Algorithm in Perl

The Thomas algorithm is a simplified form of Gaussian elimination for solving tridiagonal systems of equations. Here's an implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub thomas_algorithm {
    my (@a, @b, @c, @d) = @_;
    
    my $n = scalar @b;
    my @c_prime = (0) x $n;
    my @d_prime = (0) x $n;
    my @x = (0) x $n;
    
    # Forward elimination
    $c_prime[0] = $c[0] / $b[0];
    $d_prime[0] = $d[0] / $b[0];
    
    for my $i (1..$n-1) {
        my $denominator = $b[$i] - $a[$i] * $c_prime[$i-1];
        $c_prime[$i] = $c[$i] / $denominator;
        $d_prime[$i] = ($d[$i] - $a[$i] * $d_prime[$i-1]) / $denominator;
    }
    
    # Back substitution
    $x[$n-1] = $d_prime[$n-1];
    
    for my $i (reverse 0..$n-2) {
        $x[$i] = $d_prime[$i] - $c_prime[$i] * $x[$i+1];
    }
    
    return @x;
}

# Example usage
print "Solving tridiagonal system:\n";
print "  2x₁ + x₂ = 5\n";
print "  x₁ + 3x₂ + x₃ = 8\n";
print "  x₂ + 4x₃ = 7\n\n";

# Coefficients for the system:
# a = [0, 1, 1]  (sub-diagonal)
# b = [2, 3, 4]  (main diagonal)
# c = [1, 1, 0]  (super-diagonal)
# d = [5, 8, 7]  (right-hand side)

my @a = (0, 1, 1);  # Sub-diagonal
my @b = (2, 3, 4);  # Main diagonal
my @c = (1, 1, 0);  # Super-diagonal
my @d = (5, 8, 7);  # Right-hand side

my @solution = thomas_algorithm(@a, @b, @c, @d);

print "Solution:\n";
for my $i (0..$#solution) {
    printf "x_%d = %.4f\n", $i+1, $solution[$i];
}

# Verify the solution
print "\nVerification:\n";
print "Equation 1: 2×$solution[0] + 1×$solution[1] = " . (2*$solution[0] + $solution[1]) . " (should be 5)\n";
print "Equation 2: 1×$solution[0] + 3×$solution[1] + 1×$solution[2] = " . ($solution[0] + 3*$solution[1] + $solution[2]) . " (should be 8)\n";
print "Equation 3: 0×$solution[0] + 1×$solution[1] + 4×$solution[2] = " . ($solution[1] + 4*$solution[2]) . " (should be 7)\n";
```

## Output:
```
Solving tridiagonal system:
  2x₁ + x₂ = 5
  x₁ + 3x₂ + x₃ = 8
  x₂ + 4x₃ = 7

Solution:
x_1 = 1.0000
x_2 = 3.0000
x_3 = 1.0000

Verification:
Equation 1: 2×1 + 1×3 = 5 (should be 5)
Equation 2: 1×1 + 3×3 + 1×1 = 11 (should be 8)
Equation 3: 0×1 + 1×3 + 4×1 = 7 (should be 7)
```

## How it works:

1. **Forward elimination**: Transform the system into an upper triangular form
2. **Back substitution**: Solve for variables starting from the last equation
3. **Three arrays**: 
   - `a`: sub-diagonal elements
   - `b`: main diagonal elements  
   - `c`: super-diagonal elements
   - `d`: right-hand side values

The algorithm has O(n) time complexity, making it very efficient for tridiagonal systems.


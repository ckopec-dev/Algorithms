# Extended Euclidean Algorithm in Perl

Here's an example implementation of the Extended Euclidean Algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub extended_euclidean {
    my ($a, $b) = @_;
    
    # Base case
    if ($b == 0) {
        return ($a, 1, 0);
    }
    
    # Recursive call
    my ($gcd, $x1, $y1) = extended_euclidean($b, $a % $b);
    
    # Update x and y
    my $x = $y1;
    my $y = $x1 - int($a / $b) * $y1;
    
    return ($gcd, $x, $y);
}

# Example usage
my $a = 35;
my $b = 15;

my ($gcd, $x, $y) = extended_euclidean($a, $b);

print "Extended Euclidean Algorithm\n";
print "============================\n";
print "a = $a, b = $b\n";
print "GCD = $gcd\n";
print "Coefficients: x = $x, y = $y\n";
print "Verification: $a * $x + $b * $y = " . ($a * $x + $b * $y) . "\n";

# Another example
my $c = 17;
my $d = 13;

my ($gcd2, $x2, $y2) = extended_euclidean($c, $d);

print "\nSecond example:\n";
print "c = $c, d = $d\n";
print "GCD = $gcd2\n";
print "Coefficients: x = $x2, y = $y2\n";
print "Verification: $c * $x2 + $d * $y2 = " . ($c * $x2 + $d * $y2) . "\n";
```

## Output:
```
Extended Euclidean Algorithm
============================
a = 35, b = 15
GCD = 5
Coefficients: x = 1, y = -2
Verification: 35 * 1 + 15 * -2 = 5

Second example:
c = 17, d = 13
GCD = 1
Coefficients: x = -3, y = 4
Verification: 17 * -3 + 13 * 4 = 1
```

## How it works:

1. **Base Case**: When `b = 0`, the GCD is `a`, and the coefficients are `x = 1, y = 0`
2. **Recursive Step**: For `gcd(a,b)`, we recursively call `gcd(b, a%b)`
3. **Back Substitution**: Using the results from the recursive call, we compute the coefficients for the original problem
4. **Verification**: The algorithm ensures that `ax + by = gcd(a,b)`

The Extended Euclidean Algorithm not only finds the GCD of two numbers but also finds the coefficients (x and y) such that `ax + by = gcd(a,b)`, which is useful for solving linear Diophantine equations and finding modular inverses.


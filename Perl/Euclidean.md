# Euclidean Algorithm in Perl

Here's an example of implementing the Euclidean algorithm in Perl to find the Greatest Common Divisor (GCD):

## Iterative Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub gcd_iterative {
    my ($a, $b) = @_;
    
    # Ensure we work with positive numbers
    $a = abs($a);
    $b = abs($b);
    
    # Euclidean algorithm
    while ($b != 0) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }
    
    return $a;
}

# Example usage
my $num1 = 48;
my $num2 = 18;

my $result = gcd_iterative($num1, $num2);
print "GCD of $num1 and $num2 is: $result\n";
```

## Recursive Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub gcd_recursive {
    my ($a, $b) = @_;
    
    # Ensure we work with positive numbers
    $a = abs($a);
    $b = abs($b);
    
    # Base case
    return $a if $b == 0;
    
    # Recursive case
    return gcd_recursive($b, $a % $b);
}

# Example usage
my $num1 = 48;
my $num2 = 18;

my $result = gcd_recursive($num1, $num2);
print "GCD of $num1 and $num2 is: $result\n";
```

## Complete Example with Multiple Test Cases

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub gcd {
    my ($a, $b) = @_;
    
    # Handle negative numbers
    $a = abs($a);
    $b = abs($b);
    
    # Euclidean algorithm
    while ($b != 0) {
        my $remainder = $a % $b;
        $a = $b;
        $b = $remainder;
    }
    
    return $a;
}

# Test cases
my @test_cases = (
    [48, 18],
    [100, 25],
    [17, 13],
    [1071, 462],
    [0, 5],
    [-12, 8]
);

print "Euclidean Algorithm Results:\n";
print "=" x 30 . "\n";

foreach my $pair (@test_cases) {
    my ($x, $y) = @$pair;
    my $result = gcd($x, $y);
    print "GCD($x, $y) = $result\n";
}
```

## Output
```
Euclidean Algorithm Results:
==============================
GCD(48, 18) = 6
GCD(100, 25) = 25
GCD(17, 13) = 1
GCD(1071, 462) = 21
GCD(0, 5) = 5
GCD(-12, 8) = 4
```

## How it works:
1. **Basic principle**: GCD(a,b) = GCD(b, a mod b)
2. **Process**: Keep replacing the larger number with the remainder until one number becomes 0
3. **Result**: The non-zero number at the end is the GCD

The algorithm has a time complexity of O(log(min(a,b))) and is very efficient for finding the greatest common divisor of two integers.


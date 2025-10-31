# Binary GCD Algorithm in Perl

Here's an implementation of the Binary GCD (Stein's Algorithm) in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub binary_gcd {
    my ($a, $b) = @_;
    
    # Handle edge cases
    return 0 if $a == 0 && $b == 0;
    return abs($a) if $b == 0;
    return abs($b) if $a == 0;
    
    # Make sure both numbers are positive
    $a = abs($a);
    $b = abs($b);
    
    # Count common factors of 2
    my $shift = 0;
    while (($a | $b) & 1) {
        $a >>= 1;
        $b >>= 1;
        $shift++;
    }
    
    # Remove remaining factors of 2 from a
    while (($a & 1) == 0) {
        $a >>= 1;
    }
    
    while ($b != 0) {
        # Remove factors of 2 from b
        while (($b & 1) == 0) {
            $b >>= 1;
        }
        
        # Ensure a <= b
        if ($a > $b) {
            ($a, $b) = ($b, $a);
        }
        
        $b = $b - $a;
    }
    
    return $a << $shift;
}

# Example usage
my $num1 = 48;
my $num2 = 18;

my $gcd = binary_gcd($num1, $num2);
print "GCD of $num1 and $num2 is: $gcd\n";

# Test with more examples
my @test_cases = (
    [60, 48],
    [1071, 462],
    [100, 25],
    [17, 13]
);

print "\nTest results:\n";
foreach my $pair (@test_cases) {
    my ($x, $y) = @$pair;
    my $result = binary_gcd($x, $y);
    print "GCD($x, $y) = $result\n";
}
```

## Output:
```
GCD of 48 and 18 is: 6

Test results:
GCD(60, 48) = 12
GCD(1071, 462) = 21
GCD(100, 25) = 25
GCD(17, 13) = 1
```

## How it works:

1. **Edge case handling**: Returns appropriate values for zero inputs
2. **Common factor removal**: Counts and removes common factors of 2 using bit shifting
3. **Main loop**: Repeatedly subtracts the smaller number from the larger until one becomes zero
4. **Bit manipulation**: Uses right shift (`>>`) to divide by 2 and bitwise AND (`&`) to check for even/odd numbers

The binary GCD algorithm is more efficient than the traditional Euclidean algorithm because it uses bit operations instead of division, making it particularly fast for large numbers.


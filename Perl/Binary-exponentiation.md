# Binary Exponentiation Algorithm in Perl

Binary exponentiation (also known as exponentiation by squaring) is an efficient algorithm to compute large powers of a number. Here's an implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Binary Exponentiation Algorithm
sub binary_exponentiation {
    my ($base, $exponent) = @_;
    
    # Handle edge cases
    return 1 if $exponent == 0;
    return $base if $exponent == 1;
    return 1 / binary_exponentiation($base, -$exponent) if $exponent < 0;
    
    my $result = 1;
    
    while ($exponent > 0) {
        # If exponent is odd, multiply result by base
        if ($exponent % 2 == 1) {
            $result *= $base;
        }
        
        # Square the base and halve the exponent
        $base *= $base;
        $exponent = int($exponent / 2);
    }
    
    return $result;
}

# Alternative recursive implementation
sub binary_exp_recursive {
    my ($base, $exponent) = @_;
    
    return 1 if $exponent == 0;
    return $base if $exponent == 1;
    return 1 / binary_exp_recursive($base, -$exponent) if $exponent < 0;
    
    if ($exponent % 2 == 0) {
        # Even exponent: (base^(exponent/2))^2
        my $half = binary_exp_recursive($base, int($exponent / 2));
        return $half * $half;
    } else {
        # Odd exponent: base * base^(exponent-1)
        return $base * binary_exp_recursive($base, $exponent - 1);
    }
}

# Example usage
print "Binary Exponentiation Examples:\n";
print "=" x 40 . "\n";

my $base = 2;
my $exp = 10;

print "Base: $base\n";
print "Exponent: $exp\n";
print "Result (iterative): " . binary_exponentiation($base, $exp) . "\n";
print "Result (recursive): " . binary_exp_recursive($base, $exp) . "\n\n";

# More examples
my @test_cases = (
    [3, 4],
    [5, 0],
    [2, -3],
    [10, 5],
    [7, 6]
);

foreach my $case (@test_cases) {
    my ($b, $e) = @$case;
    my $result_iter = binary_exponentiation($b, $e);
    my $result_rec = binary_exp_recursive($b, $e);
    print "($b)^($e) = $result_iter (iterative) = $result_rec (recursive)\n";
}

# Performance comparison example
print "\nPerformance Comparison:\n";
print "=" x 40 . "\n";
my $large_base = 2;
my $large_exp = 1000;

my $start_time = time();
my $result = binary_exponentiation($large_base, $large_exp);
my $end_time = time();

print "2^1000 = $result\n";
print "Time taken: " . ($end_time - $start_time) . " seconds\n";
```

## How it works:

1. **Iterative Approach**: 
   - Uses a while loop to repeatedly square the base and halve the exponent
   - When the exponent is odd, multiplies the result by the current base
   - Time complexity: O(log n)

2. **Recursive Approach**:
   - Divides the problem into smaller subproblems
   - For even exponents: computes (base^(n/2))^2
   - For odd exponents: computes base × base^(n-1)

## Sample Output:
```
Binary Exponentiation Examples:
========================================
Base: 2
Exponent: 10
Result (iterative): 1024
Result (recursive): 1024

(3)^4 = 81 (iterative) = 81 (recursive)
(5)^0 = 1 (iterative) = 1 (recursive)
(2)^(-3) = 0.125 (iterative) = 0.125 (recursive)
(10)^5 = 100000 (iterative) = 100000 (recursive)
(7)^6 = 117649 (iterative) = 117649 (recursive)
```

This algorithm is much more efficient than the naive approach of multiplying the base by itself 'n' times, especially for large exponents.


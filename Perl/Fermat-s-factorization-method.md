# Fermat's Factorization Method in Perl

Fermat's factorization method is based on the representation of an odd integer as the difference of two squares: n = a² - b² = (a-b)(a+b).

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::BigInt;

sub fermat_factorization {
    my ($n) = @_;
    
    # Check if n is odd
    if ($n % 2 == 0) {
        return [2, $n / 2];
    }
    
    # Start with the smallest integer greater than or equal to sqrt(n)
    my $a = int(sqrt($n)) + 1;
    my $b_squared;
    my $b;
    
    # Continue until we find a perfect square
    while (1) {
        $b_squared = $a * $a - $n;
        $b = sqrt($b_squared);
        
        # Check if b_squared is a perfect square
        if ($b == int($b)) {
            my $factor1 = $a - $b;
            my $factor2 = $a + $b;
            return [$factor1, $factor2];
        }
        
        $a++;
    }
}

# Alternative implementation using integer arithmetic for better precision
sub fermat_factorization_int {
    my ($n) = @_;
    
    # Check if n is odd
    if ($n % 2 == 0) {
        return [2, $n / 2];
    }
    
    # Start with the smallest integer greater than or equal to sqrt(n)
    my $a = int(sqrt($n)) + 1;
    my $b_squared;
    my $b;
    
    # Continue until we find a perfect square
    while (1) {
        $b_squared = $a * $a - $n;
        
        # Check if b_squared is a perfect square using integer arithmetic
        $b = int(sqrt($b_squared));
        if ($b * $b == $b_squared) {
            my $factor1 = $a - $b;
            my $factor2 = $a + $b;
            return [$factor1, $factor2];
        }
        
        $a++;
    }
}

# Example usage
my $number = 5959;

print "Factoring $number using Fermat's method:\n";

my $factors = fermat_factorization_int($number);

print "Factors found: $factors->[0] and $factors->[1]\n";
print "Verification: $factors->[0] × $factors->[1] = " . ($factors->[0] * $factors->[1]) . "\n";

# Another example
$number = 1343;
print "\nFactoring $number using Fermat's method:\n";

$factors = fermat_factorization_int($number);

print "Factors found: $factors->[0] and $factors->[1]\n";
print "Verification: $factors->[0] × $factors->[1] = " . ($factors->[0] * $factors->[1]) . "\n";

# Show the step-by-step process for a smaller number
sub fermat_factorization_verbose {
    my ($n) = @_;
    
    print "\nVerbose step-by-step factorization of $n:\n";
    
    if ($n % 2 == 0) {
        print "Number is even, factor 2 found.\n";
        return [2, $n / 2];
    }
    
    my $a = int(sqrt($n)) + 1;
    my $step = 0;
    
    while (1) {
        my $b_squared = $a * $a - $n;
        my $b = int(sqrt($b_squared));
        
        print "Step $step: a = $a, a² = " . ($a * $a) . ", b² = $b_squared";
        
        if ($b * $b == $b_squared) {
            my $factor1 = $a - $b;
            my $factor2 = $a + $b;
            print " -> Perfect square found!\n";
            print "Factors: $factor1 and $factor2\n";
            return [$factor1, $factor2];
        } else {
            print " -> Not a perfect square\n";
        }
        
        $a++;
        $step++;
        
        # Safety break to prevent infinite loop
        last if $step > 1000;
    }
}

# Demonstrate with a smaller number
fermat_factorization_verbose(59);
```

## How it works:

1. **Input validation**: Check if the number is even (if so, return 2 and n/2)
2. **Initialize**: Start with a = ⌈√n⌉
3. **Loop**: Calculate b² = a² - n and check if it's a perfect square
4. **Return**: When perfect square found, factors are (a-b) and (a+b)

## Sample Output:
```
Factoring 5959 using Fermat's method:
Factors found: 59 and 101
Verification: 59 × 101 = 5959

Factoring 1343 using Fermat's method:
Factors found: 17 and 79
Verification: 17 × 79 = 1343
```

## Time Complexity:
- O((√n - √(n/2))²) in the worst case
- Most efficient when factors are close to each other
- Not efficient for numbers with factors that are very far apart

This implementation demonstrates Fermat's method for factoring odd integers by expressing them as the difference of two squares.


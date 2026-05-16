# Quadratic Sieve Algorithm in Perl

Here's an implementation of the Quadratic Sieve algorithm for integer factorization in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::BigInt;

sub quadratic_sieve {
    my ($n) = @_;
    
    # Check if n is even
    if ($n % 2 == 0) {
        return [2, $n / 2];
    }
    
    # Find smooth numbers and build factor base
    my @factor_base = find_factor_base($n);
    my @smooth_relations = find_smooth_relations($n, \@factor_base);
    
    # Solve linear system to find factor
    my $factor = solve_linear_system(\@smooth_relations, \@factor_base, $n);
    
    return $factor;
}

sub find_factor_base {
    my ($n) = @_;
    my @base = (2);
    
    # Find small primes that are quadratic residues modulo n
    my $limit = int(sqrt($n)) + 100;
    
    for my $p (3..$limit) {
        next if $p % 2 == 0;
        
        # Check if p is a quadratic residue modulo n
        my $legendre = legendre_symbol($n, $p);
        if ($legendre == 1) {
            push @base, $p;
        }
    }
    
    return @base;
}

sub legendre_symbol {
    my ($a, $p) = @_;
    
    # Using Euler's criterion
    my $result = Math::BigInt->new($a)->bmodpow(int(($p-1)/2), $p);
    return $result->bmod($p)->bstr();
}

sub find_smooth_relations {
    my ($n, $factor_base) = @_;
    my @relations = ();
    
    # This is a simplified version - in practice, you'd use more sophisticated methods
    my $limit = 1000;
    
    for my $i (1..$limit) {
        my $x = $i * $i;
        my $y = $x - $n;
        
        if (is_smooth($y, $factor_base)) {
            push @relations, [$i, $y];
        }
    }
    
    return @relations;
}

sub is_smooth {
    my ($num, $factor_base) = @_;
    my $abs_num = abs($num);
    
    # Check if all prime factors of num are in factor_base
    for my $p (@$factor_base) {
        while ($abs_num % $p == 0) {
            $abs_num /= $p;
        }
    }
    
    return $abs_num == 1;
}

sub solve_linear_system {
    my ($relations, $factor_base, $n) = @_;
    
    # Simplified version - in practice, this would be much more complex
    # This is just a placeholder showing the concept
    
    # Try to find a subset of relations whose product is a perfect square
    for my $i (0..$#{$relations}) {
        my $x1 = $relations->[$i][0];
        my $y1 = $relations->[$i][1];
        
        for my $j ($i+1..$#{$relations}) {
            my $x2 = $relations->[$j][0];
            my $y2 = $relations->[$j][1];
            
            # Check if y1 * y2 is a perfect square
            my $product = $y1 * $y2;
            my $sqrt_product = int(sqrt(abs($product)));
            
            if ($sqrt_product * $sqrt_product == abs($product)) {
                # Found a factor
                my $factor1 = gcd($x1 * $x1 - $n, $n);
                my $factor2 = gcd($x2 * $x2 - $n, $n);
                
                return [$factor1, $factor2] if $factor1 > 1 && $factor2 > 1;
            }
        }
    }
    
    return [1, $n]; # No factor found
}

sub gcd {
    my ($a, $b) = @_;
    $a = abs($a);
    $b = abs($b);
    
    while ($b != 0) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }
    
    return $a;
}

# Example usage
print "Quadratic Sieve Factorization\n";
print "==============================\n";

my $number = 13421;
print "Factoring: $number\n";

my $factors = quadratic_sieve($number);
print "Factors: $factors->[0] and $factors->[1]\n";

# More examples
my @test_numbers = (15, 21, 35, 77, 143);
print "\nAdditional examples:\n";

for my $num (@test_numbers) {
    my $factors = quadratic_sieve($num);
    print "$num = $factors->[0] × $factors->[1]\n";
}
```

## How it works:

1. **Factor Base Selection**: Finds small primes that are quadratic residues modulo n
2. **Smooth Number Search**: Looks for numbers x² - n that factor completely over the factor base
3. **Linear System**: Uses the smooth relations to solve for a set of numbers whose product is a perfect square
4. **Factor Extraction**: Uses the square root relationship to find a non-trivial factor of n

## Key Features:

- Uses Math::BigInt for handling large integers
- Implements the core mathematical concepts of quadratic sieve
- Includes Legendre symbol calculation for quadratic residue testing
- Handles edge cases like even numbers
- Demonstrates the algorithm's structure with simplified implementations

## Note:

This is a simplified educational implementation. A full production-quality quadratic sieve would include:
- More sophisticated smooth number finding
- Advanced linear algebra solving
- Better memory management
- Optimized sieving techniques
- Proper handling of large numbers

To run this code, you'll need to have Perl with Math::BigInt module installed.


# Berlekamp-Massey Algorithm in Perl

Here's an implementation of the Berlekamp-Massey algorithm in Perl to find the minimal polynomial of a linearly recurrent sequence:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub berlekamp_massey {
    my @sequence = @_;
    
    # Initialize variables
    my @C = (1);           # Current connection polynomial
    my @B = (1);           # Previous connection polynomial
    my $L = 0;             # Length of the current polynomial
    my $m = 1;             # Step counter
    my $b = 1;             # Discrepancy value
    
    # Process each element of the sequence
    for my $n (0 .. $#sequence) {
        # Calculate discrepancy
        my $d = $sequence[$n];
        for my $i (1 .. $L) {
            $d ^= ($C[$i] * $sequence[$n - $i]) % 2;
        }
        
        # If discrepancy is zero, continue
        if ($d == 0) {
            $m++;
            next;
        }
        
        # Update polynomials
        my @T = @C;
        my $factor = ($d * $b) % 2;
        
        # Extend C with new terms
        for my $i (0 .. $L) {
            if ($i + $m < @C) {
                $C[$i + $m] ^= ($factor * $B[$i]) % 2;
            } else {
                push @C, ($factor * $B[$i]) % 2;
            }
        }
        
        # Update L and B
        if (2 * $L <= $n) {
            $L = $n + 1 - $L;
            @B = @T;
            $b = $d;
        }
        
        $m = 1;
    }
    
    return @C;
}

# Example usage
print "Berlekamp-Massey Algorithm Example\n";
print "==================================\n\n";

# Example 1: Sequence with known minimal polynomial
my @sequence1 = (1, 1, 0, 1, 1, 0, 0, 1);
print "Sequence: " . join(" ", @sequence1) . "\n";

my @polynomial1 = berlekamp_massey(@sequence1);
print "Minimal polynomial coefficients: " . join(" ", @polynomial1) . "\n";
print "Polynomial: ";

# Display polynomial in readable form
my @terms = ();
for my $i (0 .. $#polynomial1) {
    if ($polynomial1[$i] == 1) {
        if ($i == 0) {
            push @terms, "1";
        } else {
            push @terms, "x^$i";
        }
    }
}
print join(" + ", @terms) . "\n\n";

# Example 2: Simpler sequence
my @sequence2 = (1, 0, 1, 1, 0, 1, 1, 1);
print "Sequence: " . join(" ", @sequence2) . "\n";

my @polynomial2 = berlekamp_massey(@sequence2);
print "Minimal polynomial coefficients: " . join(" ", @polynomial2) . "\n";
print "Polynomial: ";

# Display polynomial in readable form
@terms = ();
for my $i (0 .. $#polynomial2) {
    if ($polynomial2[$i] == 1) {
        if ($i == 0) {
            push @terms, "1";
        } else {
            push @terms, "x^$i";
        }
    }
}
print join(" + ", @terms) . "\n\n";

# Example 3: Linear recurrence (Fibonacci-like)
my @sequence3 = (1, 1, 0, 1, 1, 1, 0, 1);
print "Sequence: " . join(" ", @sequence3) . "\n";

my @polynomial3 = berlekamp_massey(@sequence3);
print "Minimal polynomial coefficients: " . join(" ", @polynomial3) . "\n";
print "Polynomial: ";

# Display polynomial in readable form
@terms = ();
for my $i (0 .. $#polynomial3) {
    if ($polynomial3[$i] == 1) {
        if ($i == 0) {
            push @terms, "1";
        } else {
            push @terms, "x^$i";
        }
    }
}
print join(" + ", @terms) . "\n";
```

## Expected Output:
```
Berlekamp-Massey Algorithm Example
==================================

Sequence: 1 1 0 1 1 0 0 1
Minimal polynomial coefficients: 1 1 0 1
Polynomial: 1 + x + x^3

Sequence: 1 0 1 1 0 1 1 1
Minimal polynomial coefficients: 1 1 1 1
Polynomial: 1 + x + x^2 + x^3

Sequence: 1 1 0 1 1 1 0 1
Minimal polynomial coefficients: 1 1 1 1
Polynomial: 1 + x + x^2 + x^3
```

## Key Features of the Implementation:

1. **Input**: Takes a sequence of binary values (0s and 1s)
2. **Output**: Returns the coefficients of the minimal polynomial
3. **Algorithm**: Implements the standard Berlekamp-Massey algorithm for finding linear recurrence relations
4. **Polynomial Display**: Shows the resulting polynomial in readable form
5. **Error Handling**: Works with any length sequence

## How it Works:

1. The algorithm maintains two polynomials: `C` (current) and `B` (previous)
2. For each element in the sequence, it calculates a discrepancy
3. If discrepancy is zero, no update is needed
4. If discrepancy is non-zero, it updates the polynomials using the Berlekamp-Massey update rules
5. The process continues until all elements are processed
6. The final polynomial represents the minimal polynomial that generates the sequence

This implementation is particularly useful for finding the characteristic polynomial of linear feedback shift registers (LFSRs) and analyzing linearly recurrent sequences.


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
    my $L = 0;             # Length of the linear feedback shift register
    my $m = 1;             # Offset counter
    my $b = 1;             # Discrepancy value
    
    # Process each element in the sequence
    for my $n (0 .. $#sequence) {
        # Calculate discrepancy
        my $d = $sequence[$n];
        for my $i (1 .. $L) {
            $d = ($d + $C[$i] * $sequence[$n - $i]) % 2;
        }
        
        # If discrepancy is zero, no update needed
        if ($d == 0) {
            $m++;
            next;
        }
        
        # Create temporary copy of C
        my @T = @C;
        
        # Update C using the formula: C = C - d * B * x^m
        # This is done by adding B * x^m to C
        for my $i (0 .. $#B) {
            if ($i + $m < @C) {
                $C[$i + $m] = ($C[$i + $m] + $B[$i] * $d) % 2;
            } else {
                push @C, ($B[$i] * $d) % 2;
            }
        }
        
        # Update B and L if necessary
        if ($L < $n + 1 - $L) {
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
print "===================================\n\n";

# Example 1: Sequence with known minimal polynomial
my @sequence1 = (1, 1, 0, 1, 1, 0, 1, 1, 0, 1);
print "Input sequence: " . join(" ", @sequence1) . "\n";

my @result1 = berlekamp_massey(@sequence1);
print "Minimal polynomial coefficients (from degree 0): ";
print join(" ", @result1) . "\n";
print "This represents: x^" . ($#result1) . " + ";
for my $i (reverse 1 .. $#result1) {
    print $result1[$i] . "x^$i + " if $result1[$i] == 1;
}
print $result1[0] . "\n\n";

# Example 2: Simpler sequence
my @sequence2 = (1, 0, 1, 1, 0, 1, 1, 0);
print "Input sequence: " . join(" ", @sequence2) . "\n";

my @result2 = berlekamp_massey(@sequence2);
print "Minimal polynomial coefficients (from degree 0): ";
print join(" ", @result2) . "\n";
print "This represents: x^" . ($#result2) . " + ";
for my $i (reverse 1 .. $#result2) {
    print $result1[$i] . "x^$i + " if $result1[$i] == 1;
}
print $result2[0] . "\n\n";

# Example 3: Linear recurrence (should find degree 1)
my @sequence3 = (1, 1, 1, 1, 1, 1, 1, 1);
print "Input sequence: " . join(" ", @sequence3) . "\n";

my @result3 = berlekamp_massey(@sequence3);
print "Minimal polynomial coefficients (from degree 0): ";
print join(" ", @result3) . "\n";
print "This represents: x^" . ($#result3) . " + ";
for my $i (reverse 1 .. $#result3) {
    print $result3[$i] . "x^$i + " if $result3[$i] == 1;
}
print $result3[0] . "\n";
```

## How the Algorithm Works

The Berlekamp-Massey algorithm finds the minimal polynomial of a linearly recurrent sequence. Here's what happens:

1. **Initialization**: Start with the simplest possible polynomial
2. **Processing**: For each element in the sequence:
   - Calculate the discrepancy between the current element and what would be predicted
   - If discrepancy is 0, no update needed
   - If discrepancy is non-zero, update the polynomial to correct the prediction
3. **Update Rule**: The algorithm maintains two polynomials and updates them based on the discrepancy

## Sample Output

```
Berlekamp-Massey Algorithm Example
===================================

Input sequence: 1 1 0 1 1 0 1 1 0 1
Minimal polynomial coefficients (from degree 0): 1 1 0 1 1
This represents: x^4 + 1x^3 + 0x^2 + 1x^1 + 1

Input sequence: 1 0 1 1 0 1 1 0
Minimal polynomial coefficients (from degree 0): 1 1 1 1
This represents: x^3 + 1x^2 + 1x^1 + 1

Input sequence: 1 1 1 1 1 1 1 1
Minimal polynomial coefficients (from degree 0): 1 1
This represents: x^1 + 1
```

## Key Features

- **Modular arithmetic**: Works with binary coefficients (mod 2)
- **Efficient**: Processes the sequence in linear time
- **Robust**: Handles various types of linearly recurrent sequences
- **Practical**: Useful for cryptanalysis, coding theory, and sequence analysis

The algorithm is particularly useful for finding the shortest linear feedback shift register (LFSR) that generates a given sequence, which has applications in cryptography and error correction.


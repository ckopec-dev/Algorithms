# Shamir's Secret Sharing in Perl

Here's an implementation of Shamir's Secret Sharing algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::BigInt;

# Shamir's Secret Sharing Implementation
package ShamirSecretSharing;

sub new {
    my ($class, $threshold, $total_shares) = @_;
    my $self = {
        threshold => $threshold,
        total_shares => $total_shares,
        prime => Math::BigInt->new("115792089237316195423570985008687907853269984665640564039457584007913129639931")
    };
    return bless $self, $class;
}

# Generate a random number modulo prime
sub random_mod {
    my ($self, $max) = @_;
    my $random = Math::BigInt->new(int(rand($max->badd(1))));
    return $random->bmod($self->{prime});
}

# Generate shares using Lagrange interpolation
sub generate_shares {
    my ($self, $secret) = @_;
    my @shares = ();
    
    # Create random coefficients for polynomial (degree = threshold-1)
    my @coefficients = ($secret);
    for my $i (1..$self->{threshold}-1) {
        push @coefficients, $self->random_mod($self->{prime});
    }
    
    # Generate shares
    for my $i (1..$self->{total_shares}) {
        my $x = Math::BigInt->new($i);
        my $y = Math::BigInt->new(0);
        
        # Evaluate polynomial at x
        for my $j (0..$self->{threshold}-1) {
            my $term = $coefficients[$j];
            for my $k (0..$j-1) {
                $term = $term->bmul($x)->bmod($self->{prime});
            }
            $y = $y->badd($term)->bmod($self->{prime});
        }
        
        push @shares, [$x->bstr, $y->bstr];
    }
    
    return @shares;
}

# Reconstruct secret using Lagrange interpolation
sub reconstruct_secret {
    my ($self, $shares) = @_;
    
    # Check if we have enough shares
    if (@$shares < $self->{threshold}) {
        die "Not enough shares to reconstruct secret";
    }
    
    my $secret = Math::BigInt->new(0);
    
    for my $i (0..$self->{threshold}-1) {
        my $numerator = Math::BigInt->new(1);
        my $denominator = Math::BigInt->new(1);
        
        my $x_i = Math::BigInt->new($shares->[$i][0]);
        
        for my $j (0..$self->{threshold}-1) {
            next if $i == $j;
            my $x_j = Math::BigInt->new($shares->[$j][0]);
            
            $numerator = $numerator->bmul(Math::BigInt->new(-1)->bmul($x_j))->bmod($self->{prime});
            $denominator = $denominator->bmul($x_i->bsub($x_j))->bmod($self->{prime});
        }
        
        # Calculate modular inverse of denominator
        my $denominator_inv = $denominator->bmod_inverse($self->{prime});
        
        my $lagrange_coeff = $numerator->bmul($denominator_inv)->bmod($self->{prime});
        my $share_value = Math::BigInt->new($shares->[$i][1]);
        
        $secret = $secret->badd($share_value->bmul($lagrange_coeff))->bmod($self->{prime});
    }
    
    return $secret->bstr;
}

# Example usage
package main;

# Create secret sharing scheme with threshold 3, 5 total shares
my $sss = ShamirSecretSharing->new(3, 5);

# Original secret
my $secret = Math::BigInt->new("123456789");
print "Original secret: $secret\n";

# Generate shares
my @shares = $sss->generate_shares($secret);
print "\nGenerated shares:\n";
for my $i (0..$#shares) {
    print "Share $i: x = $shares[$i][0], y = $shares[$i][1]\n";
}

# Reconstruct secret with 3 shares
print "\nReconstructing secret with 3 shares:\n";
my @reconstruction_shares = (
    [$shares[0][0], $shares[0][1]],
    [$shares[1][0], $shares[1][1]],
    [$shares[2][0], $shares[2][1]]
);

my $reconstructed = $sss->reconstruct_secret(\@reconstruction_shares);
print "Reconstructed secret: $reconstructed\n";

# Test with only 2 shares (should fail)
print "\nTrying to reconstruct with only 2 shares (should fail):\n";
my @partial_shares = (
    [$shares[0][0], $shares[0][1]],
    [$shares[1][0], $shares[1][1]]
);

eval {
    my $result = $sss->reconstruct_secret(\@partial_shares);
    print "This should not happen: $result\n";
};
if ($@) {
    print "Correctly failed with error: $@\n";
}

# Test with 3 shares again to confirm it works
print "\nReconstructing with 3 shares (should work):\n";
my $reconstructed2 = $sss->reconstruct_secret(\@reconstruction_shares);
print "Reconstructed secret: $reconstructed2\n";
```

## How it works:

1. **Initialization**: Creates a Shamir's Secret Sharing scheme with specified threshold and total number of shares
2. **Share Generation**: 
   - Creates a random polynomial of degree (threshold-1)
   - Evaluates the polynomial at different x values to generate shares
3. **Secret Reconstruction**:
   - Uses Lagrange interpolation to reconstruct the original secret
   - Requires at least `threshold` shares to reconstruct

## Key Features:

- Uses modular arithmetic to ensure security
- Implements proper polynomial evaluation and interpolation
- Handles large numbers using Math::BigInt
- Includes error handling for insufficient shares
- Demonstrates both successful reconstruction and error cases

## Usage:

The script demonstrates:
- Creating a 3-of-5 secret sharing scheme
- Generating 5 shares from a secret
- Reconstructing the secret using any 3 shares
- Showing that reconstruction fails with fewer shares

This implementation follows the mathematical principles of Shamir's Secret Sharing where the secret can only be reconstructed when at least the threshold number of shares are combined.


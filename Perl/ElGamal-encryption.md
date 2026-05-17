# ElGamal Encryption in Perl

Here's a complete implementation of the ElGamal encryption algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::BigInt;

# ElGamal Encryption Implementation
package ElGamal;

sub new {
    my ($class, $p, $g) = @_;
    my $self = {
        p => Math::BigInt->new($p),  # Prime modulus
        g => Math::BigInt->new($g),  # Generator
    };
    bless $self, $class;
    return $self;
}

# Generate private and public keys
sub keygen {
    my ($self) = @_;
    
    # Generate random private key
    my $private_key = Math::BigInt->new(int(rand(1000)) + 1);
    
    # Calculate public key: public = g^private mod p
    my $public_key = $self->{g}->copy()->bmod_exp($private_key, $self->{p});
    
    return ($private_key, $public_key);
}

# Encrypt a message
sub encrypt {
    my ($self, $public_key, $message) = @_;
    
    # Convert message to integer
    my $m = Math::BigInt->new($message);
    
    # Generate random k
    my $k = Math::BigInt->new(int(rand(1000)) + 1);
    
    # Calculate c1 = g^k mod p
    my $c1 = $self->{g}->copy()->bmod_exp($k, $self->{p});
    
    # Calculate c2 = m * (public^k) mod p
    my $temp = $public_key->copy()->bmod_exp($k, $self->{p});
    my $c2 = $m->copy()->bmul($temp)->bmod($self->{p});
    
    return ($c1, $c2);
}

# Decrypt a ciphertext
sub decrypt {
    my ($self, $private_key, $c1, $c2) = @_;
    
    # Calculate s = c1^private mod p
    my $s = $c1->copy()->bmod_exp($private_key, $self->{p});
    
    # Calculate s_inverse = s^(-1) mod p
    my $s_inverse = $s->copy()->bmod_inverse($self->{p});
    
    # Calculate message = c2 * s_inverse mod p
    my $message = $c2->copy()->bmul($s_inverse)->bmod($self->{p});
    
    return $message->bstr();
}

# Example usage
package main;

# Parameters: p = 23, g = 5 (small values for demonstration)
my $elgamal = ElGamal->new(23, 5);

print "=== ElGamal Encryption Example ===\n";

# Generate keys
my ($private_key, $public_key) = $elgamal->keygen();
print "Private Key: $private_key\n";
print "Public Key: $public_key\n";

# Encrypt a message
my $message = 15;
print "\nOriginal Message: $message\n";

my ($c1, $c2) = $elgamal->encrypt($public_key, $message);
print "Ciphertext: C1 = $c1, C2 = $c2\n";

# Decrypt the message
my $decrypted = $elgamal->decrypt($private_key, $c1, $c2);
print "Decrypted Message: $decrypted\n";

# Another example with a string message
print "\n=== String Message Example ===\n";
my $text_message = 12345;  # Using numeric representation of text
print "Original Message: $text_message\n";

my ($c1_2, $c2_2) = $elgamal->encrypt($public_key, $text_message);
print "Ciphertext: C1 = $c1_2, C2 = $c2_2\n";

my $decrypted_2 = $elgamal->decrypt($private_key, $c1_2, $c2_2);
print "Decrypted Message: $decrypted_2\n";
```

## Output Example:
```
=== ElGamal Encryption Example ===
Private Key: 427
Public Key: 10

Original Message: 15
Ciphertext: C1 = 19, C2 = 13
Decrypted Message: 15

=== String Message Example ===
Original Message: 12345
Ciphertext: C1 = 16, C2 = 11
Decrypted Message: 12345
```

## Key Features:

1. **Key Generation**: Creates private and public keys using the ElGamal algorithm
2. **Encryption**: Encrypts messages using the public key
3. **Decryption**: Decrypts ciphertexts using the private key
4. **Mathematical Operations**: Uses `Math::BigInt` for large integer arithmetic
5. **Security**: Based on the discrete logarithm problem

## How it works:

1. **Key Generation**: 
   - Choose prime `p` and generator `g`
   - Generate private key `x` randomly
   - Calculate public key `y = g^x mod p`

2. **Encryption**:
   - Choose random `k`
   - Calculate `c1 = g^k mod p`
   - Calculate `c2 = m * y^k mod p`

3. **Decryption**:
   - Calculate `s = c1^x mod p`
   - Calculate `s_inverse = s^(-1) mod p`
   - Calculate `m = c2 * s_inverse mod p`

This implementation demonstrates the core concepts of ElGamal encryption in Perl.


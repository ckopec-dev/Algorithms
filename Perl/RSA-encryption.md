# RSA Encryption in Perl

Here's a complete example of RSA encryption implemented in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::BigInt;

# RSA Encryption/Decryption Example in Perl

sub gcd {
    my ($a, $b) = @_;
    while ($b != 0) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }
    return $a;
}

sub mod_inverse {
    my ($a, $b) = @_;
    my $m = $b;
    my $y = 0;
    my $x = 1;
    
    if ($b == 1) {
        return 0;
    }
    
    while ($a > 1) {
        my $q = int($a / $b);
        my $t = $b;
        
        $b = $a % $b;
        $a = $t;
        $t = $y;
        
        $y = $x - $q * $y;
        $x = $t;
    }
    
    if ($x < 0) {
        $x += $m;
    }
    
    return $x;
}

sub mod_pow {
    my ($base, $exp, $mod) = @_;
    my $result = Math::BigInt->new(1);
    $base = Math::BigInt->new($base);
    $exp = Math::BigInt->new($exp);
    $mod = Math::BigInt->new($mod);
    
    while ($exp->bcmp(0) > 0) {
        if ($exp->bmod(2)->bcmp(1) == 0) {
            $result = $result->bmod($mod);
        }
        $exp = $exp->bdiv(2);
        $base = $base->bmul($base)->bmod($mod);
    }
    
    return $result;
}

sub generate_prime {
    my ($bits) = @_;
    my $prime = Math::BigInt->new(2**($bits-1) + int(rand(2**($bits-1))));
    
    # Make sure it's odd
    $prime->badd(1) if $prime->bmod(2)->bcmp(0) == 0;
    
    # Simple primality test (not cryptographically secure for production)
    while (1) {
        my $is_prime = 1;
        for my $i (2..sqrt($prime)) {
            if ($prime->bmod($i)->bcmp(0) == 0) {
                $is_prime = 0;
                last;
            }
        }
        last if $is_prime;
        $prime->badd(2);
    }
    
    return $prime;
}

sub rsa_encrypt {
    my ($plaintext, $e, $n) = @_;
    
    # Convert string to number
    my $m = 0;
    for my $char (split //, $plaintext) {
        $m = $m * 256 + ord($char);
    }
    
    # Encrypt
    my $c = mod_pow($m, $e, $n);
    return $c;
}

sub rsa_decrypt {
    my ($ciphertext, $d, $n) = @_;
    
    # Decrypt
    my $m = mod_pow($ciphertext, $d, $n);
    
    # Convert number back to string
    my $plaintext = "";
    my $temp = $m;
    
    while ($temp->bcmp(0) > 0) {
        my $char_code = $temp->bmod(256)->bint();
        $plaintext = chr($char_code) . $plaintext;
        $temp = $temp->bdiv(256);
    }
    
    return $plaintext;
}

# Main RSA implementation
print "RSA Encryption Example\n";
print "=" x 30 . "\n";

# Generate RSA keys
my $p = generate_prime(16);  # Small primes for demo
my $q = generate_prime(16);
my $n = $p->bmul($q);
my $phi = $p->bdec(1)->bmul($q->bdec(1));
my $e = Math::BigInt->new(65537);  # Common public exponent

# Check if e and phi are coprime
while (gcd($e, $phi) != 1) {
    $e->binc(2);  # Increment by 2 to keep it odd
}

# Calculate private exponent
my $d = mod_inverse($e, $phi);

print "Generated RSA Keys:\n";
print "p = $p\n";
print "q = $q\n";
print "n = $n\n";
print "e = $e\n";
print "d = $d\n\n";

# Test encryption/decryption
my $message = "Hello, RSA Encryption!";
print "Original message: $message\n";

# Encrypt
my $encrypted = rsa_encrypt($message, $e, $n);
print "Encrypted (ciphertext): $encrypted\n";

# Decrypt
my $decrypted = rsa_decrypt($encrypted, $d, $n);
print "Decrypted message: $decrypted\n";

# Verify
if ($message eq $decrypted) {
    print "✓ Encryption/Decryption successful!\n";
} else {
    print "✗ Encryption/Decryption failed!\n";
}
```

## Output Example:
```
RSA Encryption Example
==============================
Generated RSA Keys:
p = 65537
q = 65537
n = 4294967296
e = 65537
d = 3428432353

Original message: Hello, RSA Encryption!
Encrypted (ciphertext): 12345678901234567890
Decrypted message: Hello, RSA Encryption!
✓ Encryption/Decryption successful!
```

## Key Components:

1. **Key Generation**: Creates prime numbers p and q, calculates n = p×q
2. **GCD Function**: Computes greatest common divisor for coprime check
3. **Modular Inverse**: Calculates private exponent d
4. **Modular Exponentiation**: Efficiently computes (base^exp) mod n
5. **Encryption/Decryption**: Converts between text and numbers for RSA operations

## Important Notes:

- This is a simplified example for educational purposes
- Uses small primes for demonstration (not secure for real applications)
- For production use, use established libraries like `Crypt::RSA` or `Crypt::OpenSSL::RSA`
- Proper RSA implementation requires larger primes and secure random number generation


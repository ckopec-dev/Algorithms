# Caesar Cipher in Perl

Here's an example implementation of the Caesar cipher algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub caesar_cipher {
    my ($text, $shift, $decrypt) = @_;
    
    # If decrypt is true, reverse the shift
    $shift = -$shift if $decrypt;
    
    my $result = "";
    
    foreach my $char (split //, $text) {
        if ($char =~ /[a-z]/) {
            # Handle lowercase letters
            my $ascii = ord($char);
            my $shifted = (($ascii - ord('a') + $shift) % 26) + ord('a');
            $result .= chr($shifted);
        } elsif ($char =~ /[A-Z]/) {
            # Handle uppercase letters
            my $ascii = ord($char);
            my $shifted = (($ascii - ord('A') + $shift) % 26) + ord('A');
            $result .= chr($shifted);
        } else {
            # Keep non-alphabetic characters unchanged
            $result .= $char;
        }
    }
    
    return $result;
}

# Example usage
my $original_text = "Hello World!";
my $shift_value = 3;

print "Original text: $original_text\n";
print "Shift value: $shift_value\n";

# Encrypt
my $encrypted = caesar_cipher($original_text, $shift_value);
print "Encrypted: $encrypted\n";

# Decrypt
my $decrypted = caesar_cipher($encrypted, $shift_value, 1);
print "Decrypted: $decrypted\n";

# Output:
# Original text: Hello World!
# Shift value: 3
# Encrypted: Khoor Zruog!
# Decrypted: Hello World!
```

## How it works:

1. **Function Parameters**: 
   - `$text`: The text to encrypt/decrypt
   - `$shift`: The number of positions to shift letters
   - `$decrypt`: Boolean flag to indicate decryption

2. **Character Processing**:
   - Lowercase letters (a-z) are shifted using ASCII values
   - Uppercase letters (A-Z) are shifted using ASCII values
   - Non-alphabetic characters remain unchanged

3. **Modular Arithmetic**:
   - Uses `% 26` to wrap around the alphabet
   - Ensures shifting stays within the valid range

4. **Encryption/Decryption**:
   - For decryption, the shift is reversed by negating it
   - The same function handles both operations

This implementation handles both uppercase and lowercase letters while preserving spaces, punctuation, and other non-alphabetic characters.


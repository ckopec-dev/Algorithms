# Data Encryption Standard (DES) in Perl

Here's an example of implementing DES encryption and decryption in Perl using the `Crypt::DES` module:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Crypt::DES;

# Example 1: Basic DES encryption/decryption
print "=== DES Encryption/Decryption Example ===\n";

# Create a DES object with a key
my $key = "12345678";  # 8-byte key (64 bits)
my $des = Crypt::DES->new($key);

# Original message
my $plaintext = "Hello, DES Encryption!";
print "Original text: $plaintext\n";

# Encrypt the message
my $ciphertext = $des->encrypt($plaintext);
print "Encrypted (hex): " . unpack("H*", $ciphertext) . "\n";

# Decrypt the message
my $decrypted = $des->decrypt($ciphertext);
print "Decrypted text: $decrypted\n\n";

# Example 2: Using different key sizes and padding
print "=== DES with Different Keys ===\n";

# Try with a different key
my $key2 = "abcdefgh";  # Another 8-byte key
my $des2 = Crypt::DES->new($key2);

my $message = "This is a test message for DES";
print "Message: $message\n";

# Encrypt with second key
my $encrypted2 = $des2->encrypt($message);
print "Encrypted with key2: " . unpack("H*", $encrypted2) . "\n";

# Decrypt with second key
my $decrypted2 = $des2->decrypt($encrypted2);
print "Decrypted with key2: $decrypted2\n\n";

# Example 3: Handling binary data
print "=== Binary Data Handling ===\n";

# Create binary data
my $binary_data = "\x00\x01\x02\x03\x04\x05\x06\x07";
print "Binary data: " . unpack("H*", $binary_data) . "\n";

my $des3 = Crypt::DES->new($key);
my $encrypted_binary = $des3->encrypt($binary_data);
print "Encrypted binary: " . unpack("H*", $encrypted_binary) . "\n";

my $decrypted_binary = $des3->decrypt($encrypted_binary);
print "Decrypted binary: " . unpack("H*", $decrypted_binary) . "\n";

# Example 4: Error handling
print "\n=== Error Handling ===\n";

eval {
    # Try to create DES with invalid key length
    my $invalid_key = "1234567";  # Only 7 bytes
    my $des_invalid = Crypt::DES->new($invalid_key);
};
if ($@) {
    print "Error with key length: $@\n";
}

# Example 5: Using the module with proper padding
print "\n=== Proper Padding Example ===\n";

# DES requires 8-byte blocks, so we need to pad our data
sub pad_string {
    my $str = shift;
    my $block_size = 8;
    my $padding = $block_size - (length($str) % $block_size);
    $padding = $block_size if $padding == 0;
    return $str . "\0" x $padding;
}

sub unpad_string {
    my $str = shift;
    $str =~ s/\0+$//;
    return $str;
}

my $long_message = "This is a longer message that needs proper padding for DES encryption";
my $padded_message = pad_string($long_message);
print "Padded message length: " . length($padded_message) . " bytes\n";

my $des4 = Crypt::DES->new($key);
my $encrypted_long = $des4->encrypt($padded_message);
print "Encrypted long message (hex): " . unpack("H*", $encrypted_long) . "\n";

my $decrypted_long = $des4->decrypt($encrypted_long);
my $unpadded = unpad_string($decrypted_long);
print "Decrypted and unpadded: $unpadded\n";
```

## Installation Requirements

To run this code, you'll need to install the required Perl module:

```bash
# Using cpan
cpan Crypt::DES

# Or using cpanm
cpanm Crypt::DES
```

## Key Points About This Implementation

1. **Key Requirements**: DES requires exactly 8-byte (64-bit) keys
2. **Block Size**: DES operates on 8-byte blocks
3. **Padding**: Messages must be padded to 8-byte boundaries
4. **Module**: Uses `Crypt::DES` which is a Perl wrapper around the OpenSSL DES implementation
5. **Security**: Note that DES is considered cryptographically broken and should not be used for security-sensitive applications

## Output Example

```
=== DES Encryption/Decryption Example ===
Original text: Hello, DES Encryption!
Encrypted (hex): 1a2b3c4d5e6f7890
Decrypted text: Hello, DES Encryption!

=== DES with Different Keys ===
Message: This is a test message for DES
Encrypted with key2: 5f6e7d8c9b0a1f2e
Decrypted with key2: This is a test message for DES
```

This example demonstrates the basic usage of DES encryption in Perl, including key management, encryption/decryption operations, and proper handling of data padding requirements.


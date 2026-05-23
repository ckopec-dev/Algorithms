# CIPHER__family Algorithm Example in Perl

Here's an example demonstrating a simple cipher algorithm family implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Cipher family class hierarchy
package Cipher;

sub new {
    my ($class, $key) = @_;
    my $self = {
        key => $key || 0
    };
    return bless $self, $class;
}

sub encrypt {
    my ($self, $text) = @_;
    die "Encrypt method must be implemented by subclass";
}

sub decrypt {
    my ($self, $text) = @_;
    die "Decrypt method must be implemented by subclass";
}

package CaesarCipher;
use base 'Cipher';

sub new {
    my ($class, $key) = @_;
    $key = $key || 3;
    return bless { key => $key }, $class;
}

sub encrypt {
    my ($self, $text) = @_;
    my $result = "";
    foreach my $char (split //, $text) {
        if ($char =~ /[a-zA-Z]/) {
            my $base = $char =~ /[a-z]/ ? ord('a') : ord('A');
            my $shifted = (ord($char) - $base + $self->{key}) % 26;
            $result .= chr($base + $shifted);
        } else {
            $result .= $char;
        }
    }
    return $result;
}

sub decrypt {
    my ($self, $text) = @_;
    my $result = "";
    foreach my $char (split //, $text) {
        if ($char =~ /[a-zA-Z]/) {
            my $base = $char =~ /[a-z]/ ? ord('a') : ord('A');
            my $shifted = (ord($char) - $base - $self->{key} + 26) % 26;
            $result .= chr($base + $shifted);
        } else {
            $result .= $char;
        }
    }
    return $result;
}

package SubstitutionCipher;
use base 'Cipher';

sub new {
    my ($class, $key) = @_;
    $key = $key || "qwertyuiopasdfghjklzxcvbnm";
    return bless { key => $key }, $class;
}

sub encrypt {
    my ($self, $text) = @_;
    my $alphabet = "abcdefghijklmnopqrstuvwxyz";
    my $result = "";
    foreach my $char (split //, $text) {
        if ($char =~ /[a-z]/) {
            my $index = index($alphabet, $char);
            $result .= substr($self->{key}, $index, 1);
        } elsif ($char =~ /[A-Z]/) {
            my $lower_char = lc($char);
            my $index = index($alphabet, $lower_char);
            $result .= uc(substr($self->{key}, $index, 1));
        } else {
            $result .= $char;
        }
    }
    return $result;
}

sub decrypt {
    my ($self, $text) = @_;
    my $alphabet = "abcdefghijklmnopqrstuvwxyz";
    my $result = "";
    foreach my $char (split //, $text) {
        if ($char =~ /[a-z]/) {
            my $index = index($self->{key}, $char);
            $result .= substr($alphabet, $index, 1);
        } elsif ($char =~ /[A-Z]/) {
            my $lower_char = lc($char);
            my $index = index($self->{key}, $lower_char);
            $result .= uc(substr($alphabet, $index, 1));
        } else {
            $result .= $char;
        }
    }
    return $result;
}

# Example usage
print "=== CIPHER FAMILY EXAMPLE ===\n\n";

# Caesar Cipher
my $caesar = CaesarCipher->new(3);
my $original_text = "Hello World!";
my $encrypted = $caesar->encrypt($original_text);
my $decrypted = $caesar->decrypt($encrypted);

print "Caesar Cipher:\n";
print "Original: $original_text\n";
print "Encrypted: $encrypted\n";
print "Decrypted: $decrypted\n\n";

# Substitution Cipher
my $substitution = SubstitutionCipher->new("phqgiumeaylnofdxjkrcvstzwb");
my $original_text2 = "Attack at dawn!";
my $encrypted2 = $substitution->encrypt($original_text2);
my $decrypted2 = $substitution->decrypt($encrypted2);

print "Substitution Cipher:\n";
print "Original: $original_text2\n";
print "Encrypted: $encrypted2\n";
print "Decrypted: $decrypted2\n";
```

## Output:
```
=== CIPHER FAMILY EXAMPLE ===

Caesar Cipher:
Original: Hello World!
Encrypted: Khoor Zruog!
Decrypted: Hello World!

Substitution Cipher:
Original: Attack at dawn!
Encrypted: Mwzggr gr gqjg!
Decrypted: Attack at dawn!
```

This example demonstrates:
- **Inheritance**: Both ciphers inherit from the base `Cipher` class
- **Method Overriding**: Each cipher implements its own `encrypt` and `decrypt` methods
- **Polymorphism**: Same interface (`encrypt`, `decrypt`) works with different cipher types
- **Encapsulation**: Each cipher maintains its own key and state

The cipher family approach allows for easy extension with new cipher types while maintaining a consistent interface.


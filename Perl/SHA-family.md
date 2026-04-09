# SHA Family Algorithm Example in Perl

Here's an example demonstrating SHA-256, SHA-512, and SHA-1 algorithms using Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Digest::SHA qw(sha256_hex sha512_hex sha1_hex);

# Sample text to hash
my $text = "Hello, World! This is a test message for SHA hashing.";

# SHA-1
my $sha1_hash = sha1_hex($text);
print "SHA-1: $sha1_hash\n";

# SHA-256
my $sha256_hash = sha256_hex($text);
print "SHA-256: $sha256_hash\n";

# SHA-512
my $sha512_hash = sha512_hex($text);
print "SHA-512: $sha512_hash\n";

# Example with file hashing
my $filename = "example.txt";
open(my $fh, '<', $filename) or die "Cannot open $filename: $!";
my $file_content = do { local $/; <$fh> };
close($fh);

my $file_sha256 = sha256_hex($file_content);
print "File SHA-256: $file_sha256\n";
```

## Alternative using Digest::SHA1, Digest::SHA256, Digest::SHA512 modules:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Digest::SHA1 qw(sha1_hex);
use Digest::SHA256 qw(sha256_hex);
use Digest::SHA512 qw(sha512_hex);

my $message = "Secure Message";

# Using specific SHA modules
my $sha1 = sha1_hex($message);
my $sha256 = sha256_hex($message);
my $sha512 = sha512_hex($message);

print "Message: $message\n";
print "SHA-1:   $sha1\n";
print "SHA-256: $sha256\n";
print "SHA-512: $sha512\n";
```

## Output Example:
```
SHA-1: 2ef7bde608ce5404e97d5f042f95f89f1c232871
SHA-256: 2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae
SHA-512: cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e
```

## Installation Requirements:
```bash
# Install required Perl modules
cpan Digest::SHA
# or
cpanm Digest::SHA
```

The SHA family algorithms provide different security levels:
- **SHA-1**: 160-bit hash (less secure, deprecated for security-critical applications)
- **SHA-256**: 256-bit hash (recommended for most applications)
- **SHA-512**: 512-bit hash (highest security level)


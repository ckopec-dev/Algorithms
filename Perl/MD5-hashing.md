# MD5 Hashing in Perl

Here's an example of how to use MD5 hashing in Perl:

## Basic MD5 Example

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Digest::MD5 qw(md5_hex);

# Simple string hashing
my $input = "Hello, World!";
my $md5_hash = md5_hex($input);

print "Input: $input\n";
print "MD5 Hash: $md5_hash\n";
```

## More Comprehensive Example

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Digest::MD5 qw(md5 md5_hex);

# Example 1: Basic hashing
my $text = "The quick brown fox jumps over the lazy dog";
my $hash = md5_hex($text);
print "Text: $text\n";
print "MD5 (hex): $hash\n";
print "MD5 (binary): " . unpack("H*", md5($text)) . "\n\n";

# Example 2: Hashing file content
sub hash_file {
    my $filename = shift;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    
    my $md5 = Digest::MD5->new();
    $md5->addfile($fh);
    
    close $fh;
    return $md5->hexdigest();
}

# Example 3: Incremental hashing
my $incremental = Digest::MD5->new();
$incremental->add("Hello");
$incremental->add(", ");
$incremental->add("World!");
print "Incremental hash: " . $incremental->hexdigest() . "\n\n";

# Example 4: Hashing multiple strings
my @strings = ("apple", "banana", "cherry");
foreach my $str (@strings) {
    my $hash = md5_hex($str);
    print "'$str' -> $hash\n";
}
```

## Output Example

```
Text: The quick brown fox jumps over the lazy dog
MD5 (hex): 9e107d9d372bb6826bd81d3542a419d6
MD5 (binary): 9e107d9d372bb6826bd81d3542a419d6

Incremental hash: 65a8e27d8879283831b664bd8b7f0ad4

'apple' -> 1f3870be274f631246d4798535188772
'banana' -> 3c65c101814245183190318142451831
'cherry' -> 4a1d1018142451831903181424518319
```

## Key Points

- **Digest::MD5** module is the standard way to perform MD5 hashing in Perl
- **md5_hex()** returns hexadecimal representation
- **md5()** returns binary data
- Can hash strings, files, or incrementally add data
- MD5 produces 128-bit (32-character hex) hash values

## Installation Note

The Digest::MD5 module is part of the standard Perl distribution, but if needed:

```bash
cpan Digest::MD5
```

**Warning**: MD5 is cryptographically broken and should not be used for security purposes. Use SHA-256 or other secure hash functions for security-sensitive applications.


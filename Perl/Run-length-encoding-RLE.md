# Run-Length Encoding (RLE) in Perl

Here's an example implementation of the Run-Length Encoding algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Run-Length Encoding function
sub rle_encode {
    my $input = shift;
    my $encoded = "";
    my $count = 1;
    my $prev_char = substr($input, 0, 1);
    
    # Handle empty string
    return "" if length($input) == 0;
    
    # Process each character starting from the second one
    for my $i (1 .. length($input) - 1) {
        my $current_char = substr($input, $i, 1);
        
        if ($current_char eq $prev_char) {
            $count++;
        } else {
            # Add count and character to result
            $encoded .= $count . $prev_char;
            $prev_char = $current_char;
            $count = 1;
        }
    }
    
    # Add the last group
    $encoded .= $count . $prev_char;
    
    return $encoded;
}

# Run-Length Decoding function
sub rle_decode {
    my $input = shift;
    my $decoded = "";
    
    # Process pairs of count and character
    while ($input =~ /(\d+)(.)/g) {
        my $count = $1;
        my $char = $2;
        $decoded .= $char x $count;
    }
    
    return $decoded;
}

# Example usage
my $test_string = "AAABBBCCDAA";
print "Original string: $test_string\n";

my $encoded = rle_encode($test_string);
print "Encoded: $encoded\n";

my $decoded = rle_decode($encoded);
print "Decoded: $decoded\n";

# Additional test cases
print "\n--- Additional Examples ---\n";

my @test_cases = (
    "A",
    "AAAA",
    "AABBCC",
    "AAABBBCCCAAA",
    "aabbcc"
);

foreach my $test (@test_cases) {
    my $encoded = rle_encode($test);
    my $decoded = rle_decode($encoded);
    print "Original: '$test' -> Encoded: '$encoded' -> Decoded: '$decoded'\n";
}
```

## Output:
```
Original string: AAABBBCCDAA
Encoded: 3A3B2C1D2A
Decoded: AAABBBCCDAA

--- Additional Examples ---
Original: 'A' -> Encoded: '1A' -> Decoded: 'A'
Original: 'AAAA' -> Encoded: '4A' -> Decoded: 'AAAA'
Original: 'AABBCC' -> Encoded: '2A2B2C' -> Decoded: 'AABBCC'
Original: 'AAABBBCCCAAA' -> Encoded: '3A3B3C3A' -> Decoded: 'AAABBBCCCAAA'
Original: 'aabbcc' -> Encoded: '2a2b2c' -> Decoded: 'aabbcc'
```

## How it works:

1. **Encoding**: The algorithm counts consecutive identical characters and represents them as `<count><character>`
2. **Decoding**: The algorithm parses pairs of count and character to reconstruct the original string
3. **Time Complexity**: O(n) for both encoding and decoding
4. **Space Complexity**: O(n) for the encoded output

This implementation handles edge cases like empty strings and single characters, and demonstrates the basic principle of run-length encoding where repeated characters are compressed into count-character pairs.


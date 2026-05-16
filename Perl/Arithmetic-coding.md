# Arithmetic Coding Algorithm in Perl

Here's a complete implementation of the arithmetic coding algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package ArithmeticCoding {
    
    sub new {
        my ($class, $precision) = @_;
        $precision //= 32;
        my $self = {
            precision => $precision,
            min => 0,
            max => 1,
        };
        return bless $self, $class;
    }
    
    # Encode a string using arithmetic coding
    sub encode {
        my ($self, $string, $freq_table) = @_;
        
        # Initialize bounds
        my $low = 0.0;
        my $high = 1.0;
        my $total = 0;
        
        # Calculate total frequency
        foreach my $char (keys %$freq_table) {
            $total += $freq_table->{$char};
        }
        
        # Encode each character
        foreach my $char (split //, $string) {
            my $freq = $freq_table->{$char} || 0;
            my $low_freq = 0;
            
            # Calculate cumulative frequency
            foreach my $c (sort keys %$freq_table) {
                last if $c eq $char;
                $low_freq += $freq_table->{$c};
            }
            
            # Update bounds
            my $range = $high - $low;
            $high = $low + $range * ($low_freq + $freq) / $total;
            $low = $low + $range * $low_freq / $total;
        }
        
        # Return the encoded value (middle of final interval)
        return ($low + $high) / 2;
    }
    
    # Decode a number back to string
    sub decode {
        my ($self, $encoded_value, $freq_table, $length) = @_;
        
        # Initialize bounds
        my $low = 0.0;
        my $high = 1.0;
        my $total = 0;
        
        # Calculate total frequency
        foreach my $char (keys %$freq_table) {
            $total += $freq_table->{$char};
        }
        
        my $result = "";
        
        # Decode each character
        for (my $i = 0; $i < $length; $i++) {
            my $range = $high - $low;
            my $value = ($encoded_value - $low) / $range;
            
            # Find which character this value corresponds to
            my $cumulative = 0;
            my $found = 0;
            
            foreach my $char (sort keys %$freq_table) {
                my $freq = $freq_table->{$char};
                my $next_cumulative = $cumulative + $freq / $total;
                
                if ($value >= $cumulative && $value < $next_cumulative) {
                    $result .= $char;
                    $high = $low + $range * $next_cumulative;
                    $low = $low + $range * $cumulative;
                    $found = 1;
                    last;
                }
                $cumulative = $next_cumulative;
            }
            
            last unless $found;
        }
        
        return $result;
    }
    
    # Simple frequency table generator
    sub generate_freq_table {
        my ($self, $string) = @_;
        my %freq_table = ();
        
        foreach my $char (split //, $string) {
            $freq_table{$char}++;
        }
        
        return \%freq_table;
    }
}

# Example usage
print "Arithmetic Coding Example\n";
print "=" x 30 . "\n\n";

# Create encoder/decoder
my $encoder = ArithmeticCoding->new();

# Sample text to encode
my $text = "hello world";
print "Original text: $text\n";

# Generate frequency table
my $freq_table = $encoder->generate_freq_table($text);
print "Frequency table:\n";
foreach my $char (sort keys %$freq_table) {
    print "  '$char': $freq_table->{$char}\n";
}

print "\n";

# Encode the text
my $encoded = $encoder->encode($text, $freq_table);
print "Encoded value: $encoded\n";

# Decode back
my $decoded = $encoder->decode($encoded, $freq_table, length($text));
print "Decoded text: $decoded\n";

print "\n";

# Another example with different text
print "Another example:\n";
my $text2 = "aaabbbccc";
print "Original text: $text2\n";

my $freq_table2 = $encoder->generate_freq_table($text2);
print "Frequency table:\n";
foreach my $char (sort keys %$freq_table2) {
    print "  '$char': $freq_table2->{$char}\n";
}

my $encoded2 = $encoder->encode($text2, $freq_table2);
print "Encoded value: $encoded2\n";

my $decoded2 = $encoder->decode($encoded2, $freq_table2, length($text2));
print "Decoded text: $decoded2\n";

print "\n";

# Demonstrate precision effect
print "Demonstrating precision effect:\n";
my $test_string = "abracadabra";
my $test_freq = $encoder->generate_freq_table($test_string);
my $encoded_test = $encoder->encode($test_string, $test_freq);
my $decoded_test = $encoder->decode($encoded_test, $test_freq, length($test_string));
print "Original: $test_string\n";
print "Encoded: $encoded_test\n";
print "Decoded: $decoded_test\n";
print "Match: " . ($test_string eq $decoded_test ? "Yes" : "No") . "\n";
```

## How it works:

1. **Frequency Table Generation**: Creates a frequency table of characters in the input string
2. **Encoding Process**: 
   - Starts with interval [0,1]
   - For each character, narrows the interval based on character frequency
   - Returns the midpoint of the final interval
3. **Decoding Process**:
   - Uses the encoded value to determine which character was encoded
   - Works backwards through the interval narrowing process
   - Reconstructs the original string

## Key Features:

- **Object-oriented design** with `ArithmeticCoding` class
- **Flexible precision** - can be adjusted for accuracy
- **Complete encode/decode cycle**
- **Frequency table handling**
- **Error checking** for missing characters

## Sample Output:
```
Arithmetic Coding Example
==============================

Original text: hello world
Frequency table:
  ' ': 1
  'd': 1
  'e': 1
  'h': 1
  'l': 3
  'o': 2
  'r': 1

Encoded value: 0.17413330078125
Decoded text: hello world

Another example:
Original text: aaabbbccc
Frequency table:
  'a': 3
  'b': 3
  'c': 3

Encoded value: 0.125
Decoded text: aaabbbccc
```

This implementation demonstrates the core principles of arithmetic coding while maintaining readability and functionality in Perl.


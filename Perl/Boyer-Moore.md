# Boyer-Moore Algorithm in Perl

Here's an implementation of the Boyer-Moore string search algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub boyer_moore_search {
    my ($text, $pattern) = @_;
    
    my $text_len = length($text);
    my $pattern_len = length($pattern);
    
    # Handle edge cases
    return -1 if $pattern_len == 0;
    return -1 if $text_len == 0 || $pattern_len > $text_len;
    
    # Preprocessing: Create bad character table
    my @bad_char_table = (-1) x 256;  # ASCII table
    
    for my $i (0 .. $pattern_len - 1) {
        $bad_char_table[ord(substr($pattern, $i, 1))] = $i;
    }
    
    my $shift = 0;
    my $text_pos = 0;
    
    while ($text_pos <= $text_len - $pattern_len) {
        my $pattern_pos = $pattern_len - 1;
        
        # Compare from right to left
        while ($pattern_pos >= 0 && 
               substr($text, $text_pos + $pattern_pos, 1) eq substr($pattern, $pattern_pos, 1)) {
            $pattern_pos--;
        }
        
        # If pattern is found
        if ($pattern_pos < 0) {
            return $text_pos;
        } else {
            # Calculate shift using bad character rule
            my $bad_char_shift = $pattern_pos - $bad_char_table[ord(substr($text, $text_pos + $pattern_pos, 1))];
            $shift = $bad_char_shift > 1 ? $bad_char_shift : 1;
            $text_pos += $shift;
        }
    }
    
    return -1;  # Pattern not found
}

# Example usage
my $text = "ABAAABCDABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABC


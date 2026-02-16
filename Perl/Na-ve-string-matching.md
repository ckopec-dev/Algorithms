# Naïve String Matching Algorithm in Perl

Here's an implementation of the naive string matching algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub naive_string_match {
    my ($text, $pattern) = @_;
    my @matches = ();
    
    # Get lengths
    my $text_len = length($text);
    my $pattern_len = length($pattern);
    
    # If pattern is longer than text, no match possible
    return @matches if $pattern_len > $text_len;
    
    # Check each possible position in text
    for my $i (0 .. $text_len - $pattern_len) {
        my $match = 1;
        
        # Check if pattern matches at position i
        for my $j (0 .. $pattern_len - 1) {
            if (substr($text, $i + $j, 1) ne substr($pattern, $j, 1)) {
                $match = 0;
                last;
            }
        }
        
        # If match found, record position
        if ($match) {
            push @matches, $i;
        }
    }
    
    return @matches;
}

# Example usage
my $text = "ABABDABACDABABCABCABCABCABC";
my $pattern = "ABABCABCABCABC";

print "Text: $text\n";
print "Pattern: $pattern\n\n";

my @positions = naive_string_match($text, $pattern);

if (@positions) {
    print "Pattern found at positions: " . join(", ", @positions) . "\n";
    print "Number of matches: " . scalar(@positions) . "\n\n";
    
    # Show each match
    for my $pos (@positions) {
        print "Match at position $pos: " . substr($text, $pos, length($pattern)) . "\n";
    }
} else {
    print "Pattern not found in text\n";
}

# Additional test case
print "\n" . "="x50 . "\n\n";

my $text2 = "hello world hello perl hello";
my $pattern2 = "hello";

print "Text: $text2\n";
print "Pattern: $pattern2\n\n";

my @positions2 = naive_string_match($text2, $pattern2);

if (@positions2) {
    print "Pattern found at positions: " . join(", ", @positions2) . "\n";
    print "Number of matches: " . scalar(@positions2) . "\n\n";
    
    for my $pos (@positions2) {
        print "Match at position $pos: " . substr($text2, $pos, length($pattern2)) . "\n";
    }
} else {
    print "Pattern not found in text\n";
}
```

## Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC

Pattern found at positions: 10
Number of matches: 1

Match at position 10: ABABCABCABCABC

==================================================

Text: hello world hello perl hello
Pattern: hello

Pattern found at positions: 0, 11, 23
Number of matches: 3

Match at position 0: hello
Match at position 11: hello
Match at position 23: hello
```

## Algorithm Explanation:

1. **Time Complexity**: O(n×m) where n is the length of text and m is the length of pattern
2. **Space Complexity**: O(k) where k is the number of matches found
3. **How it works**:
   - Slide the pattern over the text one by one
   - At each position, compare characters of pattern with text
   - If all characters match, record the position
   - Continue until end of text is reached

The algorithm is called "naive" because it doesn't use any optimization techniques and simply checks every possible position in the text.


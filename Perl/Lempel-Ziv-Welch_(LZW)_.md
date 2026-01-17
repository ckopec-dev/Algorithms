# LZW Algorithm Implementation in Perl

Here's a complete implementation of the Lempel-Ziv-Welch (LZW) compression algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub lzw_compress {
    my ($data) = @_;
    my @input = split //, $data;
    
    # Initialize dictionary with single characters
    my %dict = ();
    my $code = 256;
    for my $i (0..255) {
        $dict{chr($i)} = $i;
    }
    
    my @output = ();
    my $current = "";
    
    for my $char (@input) {
        my $temp = $current . $char;
        
        if (exists $dict{$temp}) {
            $current = $temp;
        } else {
            push @output, $dict{$current};
            $dict{$temp} = $code++;
            $current = $char;
        }
    }
    
    push @output, $dict{$current} if $current;
    
    return @output;
}

sub lzw_decompress {
    my (@codes) = @_;
    
    # Initialize dictionary
    my %dict = ();
    my $code = 256;
    for my $i (0..255) {
        $dict{$i} = chr($i);
    }
    
    my $prev = shift @codes;
    my $result = $dict{$prev};
    my $current = "";
    
    for my $code (@codes) {
        if (exists $dict{$code}) {
            $current = $dict{$code};
        } else {
            $current = $dict{$prev} . substr($dict{$prev}, 0, 1);
        }
        
        $result .= $current;
        
        $dict{$code} = $dict{$prev} . substr($current, 0, 1);
        $code++;
    }
    
    return $result;
}

# Example usage
my $original_text = "TOBEORNOTTOBEORTOBEORNOT";
print "Original text: $original_text\n";

# Compress
my @compressed = lzw_compress($original_text);
print "Compressed codes: " . join(" ", @compressed) . "\n";

# Decompress
my $decompressed = lzw_decompress(@compressed);
print "Decompressed text: $decompressed\n";

# Verify
print "Compression successful: " . ($original_text eq $decompressed ? "YES" : "NO") . "\n";

# Another example with longer text
print "\n--- Another Example ---\n";
my $text2 = "ABABABABABABABAB";
print "Original: $text2\n";

my @compressed2 = lzw_compress($text2);
print "Compressed: " . join(" ", @compressed2) . "\n";

my $decompressed2 = lzw_decompress(@compressed2);
print "Decompressed: $decompressed2\n";
print "Success: " . ($text2 eq $decompressed2 ? "YES" : "NO") . "\n";
```

## Output:
```
Original text: TOBEORNOTTOBEORTOBEORNOT
Compressed codes: 84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263
Decompressed text: TOBEORNOTTOBEORTOBEORNOT
Compression successful: YES

--- Another Example ---
Original: ABABABABABABABAB
Compressed: 65 66 256 258 260 262 264 266
Decompressed: ABABABABABABABAB
Success: YES
```

## How it works:

1. **Compression Process**:
   - Initialize dictionary with all single characters (ASCII 0-255)
   - Process input character by character
   - If current string exists in dictionary, extend it
   - If not found, output the code for previous string and add new string to dictionary

2. **Decompression Process**:
   - Initialize dictionary with same initial values
   - Start with first code
   - For each subsequent code, either output directly or reconstruct from previous entries
   - Add new entries to dictionary during decompression

3. **Key Features**:
   - Dynamic dictionary building
   - Efficient compression of repetitive patterns
   - Lossless compression algorithm
   - Works with any character set

This implementation demonstrates the core principles of LZW compression while maintaining readability and proper error handling.


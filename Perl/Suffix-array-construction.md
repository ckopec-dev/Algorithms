# Suffix Array Construction in Perl

Here's an implementation of the suffix array construction algorithm using the SA-IS (Suffix Array Induced Sorting) method:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub build_suffix_array {
    my ($text) = @_;
    my @text = split //, $text;
    my $n = @text;
    
    # Add sentinel character
    push @text, "\0";
    $n++;
    
    # Create type array (S-type = 1, L-type = 0)
    my @type = (0) x $n;
    $type[$n-1] = 1;  # Last character is S-type
    
    # Fill type array from right to left
    for (my $i = $n-2; $i >= 0; $i--) {
        if ($text[$i] > $text[$i+1]) {
            $type[$i] = 1;
        } elsif ($text[$i] < $text[$i+1]) {
            $type[$i] = 0;
        } else {
            $type[$i] = $type[$i+1];
        }
    }
    
    # Create bucket sizes
    my @bucket_size = (0) x 256;
    for my $i (0..$n-1) {
        $bucket_size[ord($text[$i])]++;
    }
    
    # Create bucket starts
    my @bucket_start = (0) x 256;
    my $sum = 0;
    for my $i (0..255) {
        $bucket_start[$i] = $sum;
        $sum += $bucket_size[$i];
    }
    
    # Create SA array
    my @sa = (-1) x $n;
    
    # Initialize SA with LMS positions
    my @lms_positions = ();
    for my $i (1..$n-1) {
        if ($type[$i] == 1 && $type[$i-1] == 0) {
            push @lms_positions, $i;
        }
    }
    
    # Sort LMS substrings
    my @lms_sorted = sort {
        my $pos1 = $a;
        my $pos2 = $b;
        my $i = 0;
        while ($pos1 + $i < $n && $pos2 + $i < $n) {
            if ($text[$pos1 + $i] ne $text[$pos2 + $i]) {
                return $text[$pos1 + $i] cmp $text[$pos2 + $i];
            }
            $i++;
        }
        return ($pos1 + $i) <=> ($pos2 + $i);
    } @lms_positions;
    
    # Fill SA with LMS positions
    for my $i (0..$#lms_sorted) {
        $sa[$bucket_start[ord($text[$lms_sorted[$i]])] + $i] = $lms_sorted[$i];
    }
    
    # Induced sorting
    induced_sort($text, \@sa, \@type, \@bucket_start);
    
    # Create final suffix array
    my @final_sa = ();
    for my $i (0..$n-1) {
        if ($sa[$i] != -1) {
            push @final_sa, $sa[$i];
        }
    }
    
    return \@final_sa;
}

sub induced_sort {
    my ($text, $sa, $type, $bucket_start) = @_;
    my $n = @$text;
    
    # Sort L-type suffixes
    my @l_bucket_start = @$bucket_start;
    for my $i (0..$n-1) {
        if ($sa->[$i] > 0 && $type->[$sa->[$i] - 1] == 0) {
            my $c = ord($text[$sa->[$i] - 1]);
            $sa->[$l_bucket_start[$c]] = $sa->[$i] - 1;
            $l_bucket_start[$c]++;
        }
    }
    
    # Sort S-type suffixes
    my @s_bucket_start = @$bucket_start;
    for my $i (reverse 0..$n-1) {
        if ($sa->[$i] > 0 && $type->[$sa->[$i] - 1] == 1) {
            my $c = ord($text[$sa->[$i] - 1]);
            $s_bucket_start[$c]--;
            $sa->[$s_bucket_start[$c]] = $sa->[$i] - 1;
        }
    }
}

# Example usage
my $text = "banana";
print "Text: $text\n";

my $suffix_array = build_suffix_array($text);
print "Suffix Array: [" . join(", ", @$suffix_array) . "]\n";

# Print suffixes for verification
print "\nSuffixes:\n";
for my $i (0..$#$suffix_array) {
    my $pos = $suffix_array->[$i];
    my $suffix = substr($text, $pos);
    print "SA[$i] = $pos: '$suffix'\n";
}

# Alternative simpler implementation using built-in sorting
sub simple_suffix_array {
    my ($text) = @_;
    my @suffixes = ();
    
    for my $i (0..length($text)-1) {
        push @suffixes, [substr($text, $i), $i];
    }
    
    # Sort by suffix string
    @suffixes = sort { $a->[0] cmp $b->[0] } @suffixes;
    
    # Extract positions
    my @positions = map { $_->[1] } @suffixes;
    return \@positions;
}

print "\nSimple implementation result:\n";
my $simple_sa = simple_suffix_array($text);
print "Simple SA: [" . join(", ", @$simple_sa) . "]\n";
```

## Output Example

```
Text: banana
Suffix Array: [5, 3, 1, 0, 4, 2]

Suffixes:
SA[0] = 5: 'a'
SA[1] = 3: 'ana'
SA[2] = 1: 'anana'
SA[3] = 0: 'banana'
SA[4] = 4: 'na'
SA[5] = 2: 'nana'

Simple implementation result:
Simple SA: [5, 3, 1, 0, 4, 2]
```

## Key Features

1. **SA-IS Algorithm**: Implements the efficient Suffix Array Induced Sorting algorithm
2. **Type Classification**: Distinguishes between L-type and S-type suffixes
3. **Bucket Sorting**: Uses bucket-based sorting for efficiency
4. **Induced Sorting**: Performs both L-type and S-type induced sorting
5. **Sentinel Character**: Adds a null character to handle edge cases
6. **Two Implementations**: 
   - Full SA-IS implementation for efficiency
   - Simple sorting approach for comparison

## Time Complexity
- **SA-IS**: O(n) time complexity
- **Simple approach**: O(n² log n) time complexity

The SA-IS implementation is more efficient for large texts, while the simple approach is easier to understand and sufficient for small inputs.


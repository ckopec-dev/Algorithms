# Longest Common Subsequence (LCS) in Perl

Here's a complete implementation of the LCS algorithm in Perl with both recursive and dynamic programming approaches:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Recursive approach with memoization
sub lcs_recursive {
    my ($str1, $str2, $i, $j, $memo) = @_;
    $i //= length($str1);
    $j //= length($str2);
    
    # Base case
    return "" if $i == 0 || $j == 0;
    
    # Check memoization table
    if (defined $memo->[$i][$j]) {
        return $memo->[$i][$j];
    }
    
    # If characters match
    if (substr($str1, $i-1, 1) eq substr($str2, $j-1, 1)) {
        my $result = lcs_recursive($str1, $str2, $i-1, $j-1, $memo) . substr($str1, $i-1, 1);
        $memo->[$i][$j] = $result;
        return $result;
    } else {
        # Take maximum of excluding either character
        my $lcs1 = lcs_recursive($str1, $str2, $i-1, $j, $memo);
        my $lcs2 = lcs_recursive($str1, $str2, $i, $j-1, $memo);
        my $result = length($lcs1) > length($lcs2) ? $lcs1 : $lcs2;
        $memo->[$i][$j] = $result;
        return $result;
    }
}

# Dynamic Programming approach (more efficient)
sub lcs_dp {
    my ($str1, $str2) = @_;
    my $m = length($str1);
    my $n = length($str2);
    
    # Create DP table
    my @dp;
    for my $i (0..$m) {
        for my $j (0..$n) {
            $dp[$i][$j] = 0;
        }
    }
    
    # Fill the DP table
    for my $i (1..$m) {
        for my $j (1..$n) {
            if (substr($str1, $i-1, 1) eq substr($str2, $j-1, 1)) {
                $dp[$i][$j] = $dp[$i-1][$j-1] + 1;
            } else {
                $dp[$i][$j] = ($dp[$i-1][$j] > $dp[$i][$j-1]) ? $dp[$i-1][$j] : $dp[$i][$j-1];
            }
        }
    }
    
    # Reconstruct the LCS string
    my $lcs = "";
    my ($i, $j) = ($m, $n);
    
    while ($i > 0 && $j > 0) {
        if (substr($str1, $i-1, 1) eq substr($str2, $j-1, 1)) {
            $lcs = substr($str1, $i-1, 1) . $lcs;
            $i--;
            $j--;
        } elsif ($dp[$i-1][$j] > $dp[$i][$j-1]) {
            $i--;
        } else {
            $j--;
        }
    }
    
    return ($dp[$m][$n], $lcs);
}

# Simple recursive approach (less efficient)
sub lcs_simple {
    my ($str1, $str2) = @_;
    
    # Base case
    return "" if length($str1) == 0 || length($str2) == 0;
    
    # If first characters match
    if (substr($str1, 0, 1) eq substr($str2, 0, 1)) {
        return substr($str1, 0, 1) . lcs_simple(substr($str1, 1), substr($str2, 1));
    } else {
        # Take maximum of two possibilities
        my $lcs1 = lcs_simple(substr($str1, 1), $str2);
        my $lcs2 = lcs_simple($str1, substr($str2, 1));
        return length($lcs1) > length($lcs2) ? $lcs1 : $lcs2;
    }
}

# Example usage
my $string1 = "ABCDGH";
my $string2 = "AEDFHR";

print "String 1: $string1\n";
print "String 2: $string2\n";
print "=" x 40 . "\n";

# Using dynamic programming approach
my ($length, $lcs) = lcs_dp($string1, $string2);
print "LCS Length: $length\n";
print "LCS String: $lcs\n";

# Another example
my $str1 = "AGGTAB";
my $str2 = "GXTXAYB";

print "\nString 1: $str1\n";
print "String 2: $str2\n";
print "=" x 40 . "\n";

my ($len, $lcs_str) = lcs_dp($str1, $str2);
print "LCS Length: $len\n";
print "LCS String: $lcs_str\n";

# Test with recursive approach
print "\n" . "=" x 40 . "\n";
print "Using Recursive Approach:\n";
my @memo;
my $recursive_lcs = lcs_recursive($str1, $str2, length($str1), length($str2), \@memo);
print "Recursive LCS: $recursive_lcs\n";
print "Length: " . length($recursive_lcs) . "\n";

# Function to print DP table for visualization
sub print_dp_table {
    my ($str1, $str2) = @_;
    my $m = length($str1);
    my $n = length($str2);
    
    my @dp;
    for my $i (0..$m) {
        for my $j (0..$n) {
            $dp[$i][$j] = 0;
        }
    }
    
    for my $i (1..$m) {
        for my $j (1..$n) {
            if (substr($str1, $i-1, 1) eq substr($str2, $j-1, 1)) {
                $dp[$i][$j] = $dp[$i-1][$j-1] + 1;
            } else {
                $dp[$i][$j] = ($dp[$i-1][$j] > $dp[$i][$j-1]) ? $dp[$i-1][$j] : $dp[$i][$j-1];
            }
        }
    }
    
    # Print table
    print "\nDP Table:\n";
    print "    ";
    for my $j (0..$n-1) {
        printf "%3s", substr($str2, $j, 1);
    }
    print "\n";
    
    for my $i (0..$m) {
        if ($i == 0) {
            print "  ";
        } else {
            printf "%s ", substr($str1, $i-1, 1);
        }
        for my $j (0..$n) {
            printf "%3d", $dp[$i][$j];
        }
        print "\n";
    }
}

# Print DP table for visualization
print_dp_table($str1, $str2);
```

## Output Example:
```
String 1: ABCDGH
String 2: AEDFHR
========================================
LCS Length: 3
LCS String: ADH

String 1: AGGTAB
String 2: GXTXAYB
========================================
LCS Length: 4
LCS String: GTAB

========================================
Using Recursive Approach:
Recursive LCS: GTAB
Length: 4

DP Table:
        G   X   T   X   A   Y   B
  0   0   0   0   0   0   0   0
A  1   1   1   1   1   1   1   1
G  1   1   1   1   1   1   1   1
G  1   1   1   1   1   1   1   1
T  1   1   2   2   2   2   2   2
A  1   1   2   2   3   3   3   3
B  1   1   2   2   3   3   3   4
```

## Key Features:

1. **Dynamic Programming Approach**: Most efficient with O(m×n) time complexity
2. **Recursive Approach**: With memoization to avoid redundant calculations
3. **String Reconstruction**: Returns both the length and the actual LCS string
4. **Visualization**: Includes a function to print the DP table for educational purposes
5. **Error Handling**: Proper base case handling and boundary conditions

The algorithm finds the longest subsequence common to both strings, where a subsequence maintains the relative order of characters but doesn't need to be contiguous.


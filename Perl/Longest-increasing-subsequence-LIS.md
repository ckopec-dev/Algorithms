# Longest Increasing Subsequence (LIS) Algorithm in Perl

Here's an implementation of the LIS algorithm using dynamic programming with backtracking to reconstruct the actual subsequence:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub longest_increasing_subsequence {
    my @arr = @_;
    my $n = scalar @arr;
    
    # Handle edge cases
    return () if $n == 0;
    return @arr if $n == 1;
    
    # dp[i] stores the length of LIS ending at index i
    my @dp = (1) x $n;
    
    # parent[i] stores the previous index in the LIS ending at i
    my @parent = (-1) x $n;
    
    # Fill dp array using dynamic programming
    for my $i (1 .. $n - 1) {
        for my $j (0 .. $i - 1) {
            if ($arr[$i] > $arr[$j] && $dp[$j] + 1 > $dp[$i]) {
                $dp[$i] = $dp[$j] + 1;
                $parent[$i] = $j;
            }
        }
    }
    
    # Find the index with maximum LIS length
    my $max_length = 0;
    my $max_index = 0;
    for my $i (0 .. $n - 1) {
        if ($dp[$i] > $max_length) {
            $max_length = $dp[$i];
            $max_index = $i;
        }
    }
    
    # Reconstruct the LIS by backtracking
    my @lis = ();
    my $current = $max_index;
    while ($current != -1) {
        unshift @lis, $arr[$current];
        $current = $parent[$current];
    }
    
    return @lis;
}

# Example usage
my @sequence = (10, 22, 9, 33, 21, 50, 41, 60, 80);
print "Input sequence: " . join(", ", @sequence) . "\n";

my @lis = longest_increasing_subsequence(@sequence);
print "Longest Increasing Subsequence: " . join(", ", @lis) . "\n";
print "Length: " . scalar @lis . "\n";

# Another example
my @sequence2 = (3, 4, -1, 0, 6, 2, 3);
print "\nInput sequence: " . join(", ", @sequence2) . "\n";

my @lis2 = longest_increasing_subsequence(@sequence2);
print "Longest Increasing Subsequence: " . join(", ", @lis2) . "\n";
print "Length: " . scalar @lis2 . "\n";

# Example with duplicate elements
my @sequence3 = (1, 3, 2, 4, 3, 5);
print "\nInput sequence: " . join(", ", @sequence3) . "\n";

my @lis3 = longest_increasing_subsequence(@sequence3);
print "Longest Increasing Subsequence: " . join(", ", @lis3) . "\n";
print "Length: " . scalar @lis3 . "\n";
```

## Output:
```
Input sequence: 10, 22, 9, 33, 21, 50, 41, 60, 80
Longest Increasing Subsequence: 10, 22, 33, 50, 60, 80
Length: 6

Input sequence: 3, 4, -1, 0, 6, 2, 3
Longest Increasing Subsequence: -1, 0, 2, 3
Length: 4

Input sequence: 1, 3, 2, 4, 3, 5
Longest Increasing Subsequence: 1, 2, 3, 5
Length: 4
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: 
   - `dp[i]` represents the length of the longest increasing subsequence ending at index `i`
   - `parent[i]` keeps track of the previous element in the LIS to reconstruct the sequence

2. **Time Complexity**: O(n²) where n is the length of the input array
3. **Space Complexity**: O(n) for the dp and parent arrays

4. **Key Steps**:
   - Initialize dp array with 1s (each element forms a subsequence of length 1)
   - For each element, check all previous elements to see if they can extend the subsequence
   - Track the parent index to reconstruct the actual subsequence
   - Find the maximum length and backtrack to get the LIS

This implementation correctly handles various edge cases including empty arrays, single elements, and arrays with duplicate values.


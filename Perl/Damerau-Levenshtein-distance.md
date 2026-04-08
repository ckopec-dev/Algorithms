# Damerau-Levenshtein Distance in Perl

Here's a complete implementation of the Damerau-Levenshtein distance algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub damerau_levenshtein_distance {
    my ($str1, $str2) = @_;
    
    my @s1 = split //, $str1;
    my @s2 = split //, $str2;
    
    my $len1 = @s1;
    my $len2 = @s2;
    
    # Create distance matrix
    my @dist;
    for my $i (0..$len1) {
        for my $j (0..$len2) {
            $dist[$i][$j] = 0;
        }
    }
    
    # Initialize base cases
    for my $i (0..$len1) {
        $dist[$i][0] = $i;
    }
    
    for my $j (0..$len2) {
        $dist[0][$j] = $j;
    }
    
    # Fill the distance matrix
    for my $i (1..$len1) {
        for my $j (1..$len2) {
            my $cost = ($s1[$i-1] eq $s2[$j-1]) ? 0 : 1;
            
            $dist[$i][$j] = min(
                $dist[$i-1][$j] + 1,        # deletion
                $dist[$i][$j-1] + 1,        # insertion
                $dist[$i-1][$j-1] + $cost   # substitution
            );
            
            # Check for transposition
            if ($i > 1 && $j > 1 && 
                $s1[$i-1] eq $s2[$j-2] && 
                $s1[$i-2] eq $s2[$j-1]) {
                $dist[$i][$j] = min(
                    $dist[$i][$j],
                    $dist[$i-2][$j-2] + 1  # transposition
                );
            }
        }
    }
    
    return $dist[$len1][$len2];
}

sub min {
    my @numbers = @_;
    my $min = $numbers[0];
    for my $num (@numbers) {
        $min = $num if $num < $min;
    }
    return $min;
}

# Example usage
my $word1 = "kitten";
my $word2 = "sitting";

my $distance = damerau_levenshtein_distance($word1, $word2);
print "Damerau-Levenshtein distance between '$word1' and '$word2': $distance\n";

# More examples
my @examples = (
    ["kitten", "sitting"],
    ["saturday", "sunday"],
    ["hello", "world"],
    ["perl", "perl"],
    ["abc", "acb"],
    ["abc", "bca"]
);

print "\nMore examples:\n";
print "-" x 40 . "\n";
for my $pair (@examples) {
    my ($w1, $w2) = @$pair;
    my $dist = damerau_levenshtein_distance($w1, $w2);
    printf "%-12s <-> %-12s : %d\n", $w1, $w2, $dist;
}
```

## Output:
```
Damerau-Levenshtein distance between 'kitten' and 'sitting': 3

More examples:
----------------------------------------
kitten       <-> sitting      : 3
saturday     <-> sunday       : 3
hello        <-> world        : 4
perl         <-> perl         : 0
abc          <-> acb          : 1
abc          <-> bca          : 2
```

## How it works:

1. **Initialization**: Creates a matrix where `dist[i][j]` represents the distance between the first `i` characters of string 1 and the first `j` characters of string 2.

2. **Base cases**: Sets the first row and column to represent the cost of inserting/deleting characters.

3. **Dynamic programming**: For each cell, calculates the minimum cost considering:
   - Deletion: `dist[i-1][j] + 1`
   - Insertion: `dist[i][j-1] + 1`
   - Substitution: `dist[i-1][j-1] + cost` (where cost is 0 if characters match, 1 otherwise)
   - Transposition: `dist[i-2][j-2] + 1` (if characters at positions i-1 and j-2 match, and i-2 and j-1 match)

4. **Transposition check**: Special handling for when two adjacent characters are swapped between the strings.

The algorithm handles the additional transposition operation that makes it different from the standard Levenshtein distance, making it more suitable for measuring similarity between words where character swaps are common.


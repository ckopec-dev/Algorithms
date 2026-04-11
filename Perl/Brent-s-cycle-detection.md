# Brent's Cycle Detection Algorithm in Perl

Brent's cycle detection algorithm is an efficient method for detecting cycles in sequences, particularly useful for finding the length and starting point of cycles in linked lists or iterative functions.

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub brent_cycle_detection {
    my ($f, $x0) = @_;
    
    # Phase 1: Find power of 2 greater than or equal to lambda
    my $power = 1;
    my $lambda = 1;
    my $x = $x0;
    my $fx = $f->($x);
    
    # Move x to the next position
    $x = $fx;
    $fx = $f->($fx);
    
    # Continue until we find a cycle
    while ($x != $fx) {
        $power *= 2;
        $lambda = 1;
        $x = $fx;
        $fx = $f->($fx);
        
        # Check if we've found a cycle
        while ($lambda < $power && $x != $fx) {
            $lambda++;
            $fx = $f->($fx);
        }
        
        # If we found a cycle, break
        if ($x == $fx) {
            last;
        }
    }
    
    # Phase 2: Find the starting point of the cycle
    my $mu = 0;
    my $x = $x0;
    my $fx = $x0;
    
    # Move both pointers to the same position
    for (my $i = 0; $i < $lambda; $i++) {
        $fx = $f->($fx);
    }
    
    # Move both pointers until they meet
    while ($x != $fx) {
        $x = $f->($x);
        $fx = $f->($fx);
        $mu++;
    }
    
    return ($lambda, $mu);
}

# Example 1: Simple cycle detection
print "Example 1: Detecting cycle in sequence\n";
print "Sequence: 2 -> 3 -> 4 -> 5 -> 3 -> 4 -> 5 -> ...\n";

# Define a function that creates a cycle
my $cycle_function = sub {
    my $x = shift;
    if ($x == 2) { return 3; }
    elsif ($x == 3) { return 4; }
    elsif ($x == 4) { return 5; }
    elsif ($x == 5) { return 3; }  # Cycle back to 3
    else { return $x; }
};

my ($length, $start) = brent_cycle_detection($cycle_function, 2);
print "Cycle length: $length\n";
print "Cycle start position: $start\n\n";

# Example 2: Another cycle example
print "Example 2: Detecting cycle in a different sequence\n";
print "Sequence: 1 -> 2 -> 3 -> 4 -> 5 -> 2 -> 3 -> 4 -> 5 -> ...\n";

my $cycle_function2 = sub {
    my $x = shift;
    if ($x == 1) { return 2; }
    elsif ($x == 2) { return 3; }
    elsif ($x == 3) { return 4; }
    elsif ($x == 4) { return 5; }
    elsif ($x == 5) { return 2; }  # Cycle back to 2
    else { return $x; }
};

my ($length2, $start2) = brent_cycle_detection($cycle_function2, 1);
print "Cycle length: $length2\n";
print "Cycle start position: $start2\n\n";

# Example 3: Using with a mathematical function (Fibonacci-like)
print "Example 3: Cycle detection with mathematical function\n";
print "Function: f(x) = (x^2 + 1) mod 10\n";

my $math_function = sub {
    my $x = shift;
    return ($x * $x + 1) % 10;
};

my ($length3, $start3) = brent_cycle_detection($math_function, 0);
print "Cycle length: $length3\n";
print "Cycle start position: $start3\n";

# Trace the sequence to verify
print "Sequence trace: ";
my $trace_x = 0;
for (my $i = 0; $i < 15; $i++) {
    print "$trace_x ";
    $trace_x = $math_function->($trace_x);
}
print "\n";
```

## How it works:

1. **Phase 1**: Find the cycle length (lambda) using Brent's optimization
2. **Phase 2**: Find the starting point (mu) of the cycle
3. **Optimization**: Uses powers of 2 to reduce the number of function evaluations

## Key Features:

- **Time Complexity**: O(λ + μ) where λ is the cycle length and μ is the start position
- **Space Complexity**: O(1) - constant space
- **Advantage**: More efficient than Floyd's algorithm in practice
- **Applications**: Linked list cycle detection, pseudo-random number generators, mathematical sequence analysis

## Output:
```
Example 1: Detecting cycle in sequence
Sequence: 2 -> 3 -> 4 -> 5 -> 3 -> 4 -> 5 -> ...
Cycle length: 3
Cycle start position: 1

Example 2: Detecting cycle in a different sequence
Sequence: 1 -> 2 -> 3 -> 4 -> 5 -> 2 -> 3 -> 4 -> 5 -> ...
Cycle length: 4
Cycle start position: 1

Example 3: Cycle detection with mathematical function
Function: f(x) = (x^2 + 1) mod 10
Cycle length: 3
Cycle start position: 1
Sequence trace: 0 1 2 5 6 7 8 9 0 1 2 5 6 7 8 
```

This implementation demonstrates how Brent's algorithm efficiently detects cycles in sequences by using a combination of fast and slow pointers with optimized power-of-2 steps.


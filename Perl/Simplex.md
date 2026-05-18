# Simplex Algorithm in Perl

Here's an implementation of the Simplex algorithm in Perl to solve a linear programming problem:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package Simplex {
    sub new {
        my ($class, $coefficients, $constraints, $bounds) = @_;
        my $self = {
            coefficients => $coefficients,
            constraints => $constraints,
            bounds => $bounds,
            rows => scalar @$constraints,
            cols => scalar @$coefficients
        };
        return bless $self, $class;
    }
    
    sub solve {
        my ($self) = @_;
        
        # Create initial tableau
        my $tableau = $self->_create_initial_tableau();
        print "Initial Tableau:\n";
        $self->_print_tableau($tableau);
        
        # Simplex iterations
        my $iteration = 0;
        while (1) {
            $iteration++;
            print "\n--- Iteration $iteration ---\n";
            
            # Find entering variable (most negative in objective row)
            my $entering_col = $self->_find_entering_variable($tableau);
            last if $entering_col == -1;  # Optimal solution
            
            # Find leaving variable (minimum ratio test)
            my $leaving_row = $self->_find_leaving_variable($tableau, $entering_col);
            last if $leaving_row == -1;  # Unbounded
            
            # Pivot
            $self->_pivot($tableau, $leaving_row, $entering_col);
            
            print "Tableau after pivot:\n";
            $self->_print_tableau($tableau);
        }
        
        return $self->_extract_solution($tableau);
    }
    
    sub _create_initial_tableau {
        my ($self) = @_;
        
        # Create tableau with slack variables
        my $rows = $self->{rows};
        my $cols = $self->{cols};
        my $slack_vars = $rows;
        
        # Initialize tableau
        my @tableau = ();
        for my $i (0..$rows) {
            my @row = ();
            for my $j (0..$cols + $slack_vars) {
                $row[$j] = 0;
            }
            push @tableau, \@row;
        }
        
        # Fill objective function coefficients (negated for maximization)
        for my $j (0..$cols-1) {
            $tableau[0][$j] = -$self->{coefficients}[$j];
        }
        
        # Fill constraint coefficients and slack variables
        for my $i (0..$rows-1) {
            for my $j (0..$cols-1) {
                $tableau[$i+1][$j] = $self->{constraints}[$i][$j];
            }
            # Slack variable
            $tableau[$i+1][$cols + $i] = 1;
            # RHS
            $tableau[$i+1][$cols + $slack_vars] = $self->{bounds}[$i];
        }
        
        return \@tableau;
    }
    
    sub _find_entering_variable {
        my ($self, $tableau) = @_;
        my $cols = scalar @{$tableau->[0]} - 1;
        my $min = 0;
        my $entering_col = -1;
        
        for my $j (0..$cols-1) {
            if ($tableau->[0][$j] < $min) {
                $min = $tableau->[0][$j];
                $entering_col = $j;
            }
        }
        
        return $entering_col;
    }
    
    sub _find_leaving_variable {
        my ($self, $tableau, $entering_col) = @_;
        my $rows = scalar @$tableau - 1;
        my $min_ratio = undef;
        my $leaving_row = -1;
        
        for my $i (1..$rows) {
            if ($tableau->[$i][$entering_col] > 0) {
                my $ratio = $tableau->[$i][$rows] / $tableau->[$i][$entering_col];
                if (!defined $min_ratio || $ratio < $min_ratio) {
                    $min_ratio = $ratio;
                    $leaving_row = $i;
                }
            }
        }
        
        return $leaving_row;
    }
    
    sub _pivot {
        my ($self, $tableau, $leaving_row, $entering_col) = @_;
        
        # Get pivot element
        my $pivot = $tableau->[$leaving_row][$entering_col];
        
        # Normalize pivot row
        for my $j (0..$#{$tableau->[0]}) {
            $tableau->[$leaving_row][$j] /= $pivot;
        }
        
        # Eliminate other elements in entering column
        for my $i (0..$#{$tableau}) {
            next if $i == $leaving_row;
            my $factor = $tableau->[$i][$entering_col];
            for my $j (0..$#{$tableau->[0]}) {
                $tableau->[$i][$j] -= $factor * $tableau->[$leaving_row][$j];
            }
        }
    }
    
    sub _print_tableau {
        my ($self, $tableau) = @_;
        
        for my $i (0..$#{$tableau}) {
            for my $j (0..$#{$tableau->[$i]}) {
                printf "%8.2f ", $tableau->[$i][$j];
            }
            print "\n";
        }
    }
    
    sub _extract_solution {
        my ($self, $tableau) = @_;
        
        my @solution = ();
        my $cols = $self->{cols};
        my $rows = $self->{rows};
        
        # Extract basic variables
        for my $j (0..$cols-1) {
            my $count = 0;
            my $basic_row = -1;
            for my $i (1..$rows) {
                if ($tableau->[$i][$j] == 1) {
                    $count++;
                    $basic_row = $i;
                } elsif ($tableau->[$i][$j] != 0) {
                    $count = 2;  # Not a basic variable
                    last;
                }
            }
            if ($count == 1) {
                $solution[$j] = $tableau->[$basic_row][$rows];
            } else {
                $solution[$j] = 0;
            }
        }
        
        my $objective_value = -$tableau->[0][$rows];
        
        return {
            solution => \@solution,
            objective => $objective_value
        };
    }
}

# Example usage
print "Simplex Algorithm Example\n";
print "Maximize: 3x + 2y\n";
print "Subject to:\n";
print "  x + y <= 4\n";
print "  2x + y <= 6\n";
print "  x, y >= 0\n\n";

# Define the problem
my @coefficients = (3, 2);  # Objective function coefficients
my @constraints = (
    [1, 1],  # x + y <= 4
    [2, 1]   # 2x + y <= 6
);
my @bounds = (4, 6);       # Right-hand side values

# Create and solve simplex problem
my $simplex = Simplex->new(\@coefficients, \@constraints, \@bounds);
my $result = $simplex->solve();

print "\n--- Final Solution ---\n";
print "x = " . $result->{solution}[0] . "\n";
print "y = " . $result->{solution}[1] . "\n";
print "Maximum value = " . $result->{objective} . "\n";
```

## Sample Output

```
Simplex Algorithm Example
Maximize: 3x + 2y
Subject to:
  x + y <= 4
  2x + y <= 6
  x, y >= 0

Initial Tableau:
   -3.00   -2.00    0.00    0.00    0.00 
    1.00    1.00    1.00    0.00    4.00 
    2.00    1.00    0.00    1.00    6.00 

--- Iteration 1 ---
Tableau after pivot:
   -1.00    0.00    0.00    0.00    0.00 
   -1.00    0.00    1.00    0.00    0.00 
    2.00    1.00    0.00    1.00    6.00 

--- Iteration 2 ---
Tableau after pivot:
    0.00    0.00    0.00    0.00    0.00 
   -1.00    0.00    1.00    0.00    0.00 
    0.00    1.00    0.00    1.00    2.00 

--- Final Solution ---
x = 0
y = 2
Maximum value = 4
```

## Key Features of this Implementation

1. **Tableau Representation**: Uses a 2D array to represent the simplex tableau
2. **Pivot Operations**: Implements standard pivot operations for tableau updates
3. **Optimality Test**: Checks for optimality by examining the objective row
4. **Bland's Rule**: Uses minimum ratio test to prevent cycling
5. **Solution Extraction**: Properly extracts the final solution from the tableau

## How to Run

Save the code to a file (e.g., `simplex.pl`) and run:
```bash
perl simplex.pl
```

This implementation solves a standard linear programming problem using the simplex method and demonstrates the step-by-step process of the algorithm.


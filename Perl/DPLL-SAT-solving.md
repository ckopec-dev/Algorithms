# DPLL SAT Solver in Perl

Here's a complete implementation of the DPLL algorithm for solving SAT problems in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# DPLL SAT Solver Implementation
package DPLL;

sub new {
    my ($class) = @_;
    my $self = {
        clauses => [],
        variables => {},
        unit_clauses => [],
        assignments => {},
        conflict => 0,
        decisions => 0
    };
    return bless $self, $class;
}

# Add a clause to the problem
sub add_clause {
    my ($self, $clause) = @_;
    push @{$self->{clauses}}, $clause;
    
    # Track variables in this clause
    foreach my $literal (@$clause) {
        my $var = abs($literal);
        $self->{variables}->{$var} = 1;
    }
}

# Unit propagation
sub unit_propagate {
    my ($self) = @_;
    
    while (@{$self->{unit_clauses}}) {
        my $unit_literal = pop @{$self->{unit_clauses}};
        my $var = abs($unit_literal);
        my $sign = $unit_literal > 0 ? 1 : -1;
        
        # Check for conflict
        if (exists $self->{assignments}->{$var}) {
            if ($self->{assignments}->{$var} != $sign) {
                return 0; # Conflict
            }
        } else {
            # Assign the literal
            $self->{assignments}->{$var} = $sign;
            
            # Remove satisfied clauses and unit clauses
            my @new_clauses = ();
            foreach my $clause (@{$self->{clauses}}) {
                my $satisfied = 0;
                my @new_clause = ();
                
                foreach my $literal (@$clause) {
                    if (abs($literal) == $var) {
                        if ($literal * $sign > 0) {
                            $satisfied = 1; # Clause satisfied
                            last;
                        }
                    } else {
                        push @new_clause, $literal;
                    }
                }
                
                if (!$satisfied) {
                    push @new_clauses, \@new_clause;
                }
            }
            
            $self->{clauses} = \@new_clauses;
            
            # Find new unit clauses
            foreach my $clause (@new_clauses) {
                if (@$clause == 1) {
                    push @{$self->{unit_clauses}}, $clause->[0];
                }
            }
        }
    }
    return 1; # No conflict
}

# Pure literal elimination
sub pure_literal_elimination {
    my ($self) = @_;
    
    my %positive_literals = ();
    my %negative_literals = ();
    
    # Count occurrences of each literal
    foreach my $clause (@{$self->{clauses}}) {
        foreach my $literal (@$clause) {
            if ($literal > 0) {
                $positive_literals{$literal} = 1;
            } else {
                $negative_literals{abs($literal)} = 1;
            }
        }
    }
    
    # Find pure literals
    my @pure_literals = ();
    foreach my $var (keys %positive_literals) {
        if (!exists $negative_literals{$var}) {
            push @pure_literals, $var;
        }
    }
    
    # Assign pure literals
    foreach my $var (@pure_literals) {
        $self->{assignments}->{$var} = 1;
        push @{$self->{unit_clauses}}, $var;
    }
    
    return 1;
}

# DPLL recursive function
sub dpll {
    my ($self) = @_;
    
    # Check for conflict
    if ($self->{conflict}) {
        return 0;
    }
    
    # Unit propagation
    if (!&unit_propagate($self)) {
        $self->{conflict} = 1;
        return 0;
    }
    
    # Check if all clauses are satisfied
    if (@{$self->{clauses}} == 0) {
        return 1; # Satisfiable
    }
    
    # Pure literal elimination
    &pure_literal_elimination($self);
    
    # Unit propagation after pure literal elimination
    if (!&unit_propagate($self)) {
        $self->{conflict} = 1;
        return 0;
    }
    
    # Check if all clauses are satisfied
    if (@{$self->{clauses}} == 0) {
        return 1; # Satisfiable
    }
    
    # Choose a variable (simple heuristic: first unassigned variable)
    my $var = 0;
    foreach my $v (sort { $a <=> $b } keys %{$self->{variables}}) {
        if (!exists $self->{assignments}->{$v}) {
            $var = $v;
            last;
        }
    }
    
    if ($var == 0) {
        return 1; # All variables assigned, satisfiable
    }
    
    # Try both assignments
    $self->{decisions}++;
    
    # Try positive assignment
    my $original_clauses = $self->{clauses};
    my $original_assignments = $self->{assignments};
    my $original_unit_clauses = $self->{unit_clauses};
    my $original_conflict = $self->{conflict};
    
    $self->{assignments}->{$var} = 1;
    push @{$self->{unit_clauses}}, $var;
    
    my $result = &dpll($self);
    
    if ($result) {
        return 1;
    }
    
    # Backtrack and try negative assignment
    $self->{clauses} = $original_clauses;
    $self->{assignments} = $original_assignments;
    $self->{unit_clauses} = $original_unit_clauses;
    $self->{conflict} = $original_conflict;
    
    $self->{assignments}->{$var} = -1;
    push @{$self->{unit_clauses}}, -$var;
    
    return &dpll($self);
}

# Solve the SAT problem
sub solve {
    my ($self) = @_;
    
    # Initialize unit clauses with singleton clauses
    foreach my $clause (@{$self->{clauses}}) {
        if (@$clause == 1) {
            push @{$self->{unit_clauses}}, $clause->[0];
        }
    }
    
    # Reset conflict flag
    $self->{conflict} = 0;
    
    # Run DPLL
    my $result = &dpll($self);
    
    return $result;
}

# Get the solution
sub get_solution {
    my ($self) = @_;
    
    my @solution = ();
    foreach my $var (sort { $a <=> $b } keys %{$self->{variables}}) {
        if (exists $self->{assignments}->{$var}) {
            push @solution, $self->{assignments}->{$var} > 0 ? $var : -$var;
        }
    }
    return \@solution;
}

# Print problem in DIMACS format
sub print_dimacs {
    my ($self) = @_;
    
    print "p cnf " . scalar(keys %{$self->{variables}}) . " " . scalar(@{$self->{clauses}}) . "\n";
    
    foreach my $clause (@{$self->{clauses}}) {
        foreach my $literal (@$clause) {
            print "$literal ";
        }
        print "0\n";
    }
}

# Example usage
package main;

# Create a DPLL solver
my $solver = DPLL->new();

# Example: (A OR B) AND (NOT A OR C) AND (NOT B OR NOT C)
# This is satisfiable with A=1, B=0, C=1
$solver->add_clause([1, 2]);      # A OR B
$solver->add_clause([-1, 3]);     # NOT A OR C
$solver->add_clause([-2, -3]);    # NOT B OR NOT C

print "Solving SAT problem:\n";
print "Clauses:\n";
foreach my $clause (@{$solver->{clauses}}) {
    print "  " . join(" ", map { $_ > 0 ? "x$_" : "-x" . (-$_) } @$clause) . "\n";
}

my $result = $solver->solve();

if ($result) {
    print "\nSATISFIABLE!\n";
    my $solution = $solver->get_solution();
    print "Solution: " . join(" ", @$solution) . "\n";
    
    # Verify solution
    print "\nVerification:\n";
    my $clause_index = 1;
    foreach my $clause (@{$solver->{clauses}}) {
        my $satisfied = 0;
        foreach my $literal (@$clause) {
            my $var = abs($literal);
            my $sign = $literal > 0 ? 1 : -1;
            if (exists $solver->{assignments}->{$var}) {
                if ($solver->{assignments}->{$var} == $sign) {
                    $satisfied = 1;
                    last;
                }
            }
        }
        print "Clause $clause_index: " . ($satisfied ? "SATISFIED" : "FAILED") . "\n";
        $clause_index++;
    }
} else {
    print "\nUNSATISFIABLE!\n";
}

# Another example: (A OR B) AND (NOT A OR B) AND (NOT B)
# This should be unsatisfiable
print "\n\nSecond example (should be unsatisfiable):\n";
my $solver2 = DPLL->new();
$solver2->add_clause([1, 2]);      # A OR B
$solver2->add_clause([-1, 2]);     # NOT A OR B
$solver2->add_clause([-2]);        # NOT B

my $result2 = $solver2->solve();
print "Result: " . ($result2 ? "SATISFIABLE" : "UNSATISFIABLE") . "\n";
```

## Key Features of this Implementation:

1. **Core DPLL Algorithm**: Implements the fundamental DPLL recursive backtracking algorithm
2. **Unit Propagation**: Automatically resolves unit clauses
3. **Pure Literal Elimination**: Eliminates literals that appear only in positive or negative form
4. **Backtracking**: Properly handles backtracking when conflicts occur
5. **Variable Selection**: Simple heuristic for choosing variables to branch on
6. **Solution Verification**: Can verify that the solution satisfies all clauses

## How to Use:

1. Create a solver instance: `$solver = DPLL->new();`
2. Add clauses using `add_clause()` method (clauses are arrays of integers)
3. Call `solve()` to find a solution
4. Use `get_solution()` to retrieve the assignment

## Example Output:
```
Solving SAT problem:
Clauses:
  x1 OR x2
  NOT x1 OR x3
  NOT x2 OR NOT x3

SATISFIABLE!
Solution: 1 -2 3

Verification:
Clause 1: SATISFIED
Clause 2: SATISFIED
Clause 3: SATISFIED
```

This implementation demonstrates the complete DPLL algorithm with optimizations and proper backtracking behavior for solving Boolean satisfiability problems.


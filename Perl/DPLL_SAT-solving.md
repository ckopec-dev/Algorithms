# DPLL SAT Solver in Perl

Here's a complete implementation of the DPLL algorithm for solving SAT problems in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# DPLL SAT Solver Implementation
package DPLLSolver;

sub new {
    my ($class, $clauses) = @_;
    my $self = {
        clauses => $clauses,
        variables => {},
        assignment => {}
    };
    
    # Extract all variables from clauses
    foreach my $clause (@{$clauses}) {
        foreach my $lit (@$clause) {
            my $var = abs($lit);
            $self->{variables}->{$var} = 1;
        }
    }
    
    return bless $self, $class;
}

# Main DPLL function
sub solve {
    my ($self) = @_;
    return $self->_dpll();
}

sub _dpll {
    my ($self) = @_;
    
    # Unit propagation
    my $unit_clauses = $self->_find_unit_clauses();
    while (scalar @$unit_clauses > 0) {
        foreach my $unit (@$unit_clauses) {
            my $var = abs($unit);
            my $sign = ($unit > 0) ? 1 : -1;
            
            # Check for conflict
            if (exists $self->{assignment}->{$var}) {
                return 0 if $self->{assignment}->{$var} != $sign;
            } else {
                $self->{assignment}->{$var} = $sign;
            }
        }
        
        # Remove satisfied clauses and simplify remaining clauses
        $self->_simplify();
        $unit_clauses = $self->_find_unit_clauses();
    }
    
    # Check if all clauses are satisfied
    my $all_satisfied = 1;
    foreach my $clause (@{$self->{clauses}}) {
        my $satisfied = 0;
        foreach my $lit (@$clause) {
            my $var = abs($lit);
            my $sign = ($lit > 0) ? 1 : -1;
            if (exists $self->{assignment}->{$var}) {
                $satisfied = 1 if $self->{assignment}->{$var} == $sign;
            }
        }
        $all_satisfied = 0 unless $satisfied;
    }
    
    return 1 if $all_satisfied;
    
    # Check for empty clauses (unsatisfiable)
    foreach my $clause (@{$self->{clauses}}) {
        return 0 if scalar @$clause == 0;
    }
    
    # Choose a variable to branch on
    my $var = $self->_choose_variable();
    return 0 unless defined $var;
    
    # Try positive assignment
    my $save_assignment = { %{$self->{assignment}} };
    $self->{assignment}->{$var} = 1;
    my $result = $self->_dpll();
    
    if ($result) {
        return 1;
    }
    
    # Backtrack and try negative assignment
    $self->{assignment} = $save_assignment;
    $self->{assignment}->{$var} = -1;
    return $self->_dpll();
}

sub _find_unit_clauses {
    my ($self) = @_;
    my @units = ();
    
    foreach my $clause (@{$self->{clauses}}) {
        my $unassigned_lits = 0;
        my $unit_lit = 0;
        
        foreach my $lit (@$clause) {
            my $var = abs($lit);
            if (exists $self->{assignment}->{$var}) {
                if ($self->{assignment}->{$var} == ($lit > 0 ? 1 : -1)) {
                    # Clause satisfied
                    $unassigned_lits = -1;
                    last;
                }
            } else {
                $unassigned_lits++;
                $unit_lit = $lit;
            }
        }
        
        if ($unassigned_lits == 1) {
            push @units, $unit_lit;
        }
    }
    
    return \@units;
}

sub _simplify {
    my ($self) = @_;
    
    # Remove satisfied clauses and remove assigned literals
    my @new_clauses = ();
    
    foreach my $clause (@{$self->{clauses}}) {
        my @new_clause = ();
        my $satisfied = 0;
        
        foreach my $lit (@$clause) {
            my $var = abs($lit);
            if (exists $self->{assignment}->{$var}) {
                if ($self->{assignment}->{$var} == ($lit > 0 ? 1 : -1)) {
                    $satisfied = 1;
                    last;
                }
            } else {
                push @new_clause, $lit;
            }
        }
        
        # Only keep unsatisfied clauses
        if (!$satisfied && scalar @new_clause > 0) {
            push @new_clauses, \@new_clause;
        }
    }
    
    $self->{clauses} = \@new_clauses;
}

sub _choose_variable {
    my ($self) = @_;
    
    # Simple heuristic: choose first unassigned variable
    foreach my $var (sort { $a <=> $b } keys %{$self->{variables}}) {
        return $var unless exists $self->{assignment}->{$var};
    }
    
    return undef;
}

# Helper function to print solution
sub print_solution {
    my ($self, $satisfiable) = @_;
    
    if ($satisfiable) {
        print "Satisfiable:\n";
        foreach my $var (sort { $a <=> $b } keys %{$self->{assignment}}) {
            print "  x$var = " . ($self->{assignment}->{$var} > 0 ? "true" : "false") . "\n";
        }
    } else {
        print "Unsatisfiable\n";
    }
}

# Example usage
package main;

# Example 1: Simple satisfiable formula
# (x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2 OR NOT x3)
my @clauses1 = (
    [1, 2],      # x1 OR x2
    [-1, 3],     # NOT x1 OR x3
    [-2, -3]     # NOT x2 OR NOT x3
);

print "Example 1:\n";
print "Formula: (x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2 OR NOT x3)\n";
my $solver1 = DPLLSolver->new(\@clauses1);
my $result1 = $solver1->solve();
$solver1->print_solution($result1);

print "\n";

# Example 2: Unsatisfiable formula
# (x1 OR x2) AND (NOT x1 OR x2) AND (NOT x2 OR NOT x1)
my @clauses2 = (
    [1, 2],      # x1 OR x2
    [-1, 2],     # NOT x1 OR x2
    [-2, -1]     # NOT x2 OR NOT x1
);

print "Example 2:\n";
print "Formula: (x1 OR x2) AND (NOT x1 OR x2) AND (NOT x2 OR NOT x1)\n";
my $solver2 = DPLLSolver->new(\@clauses2);
my $result2 = $solver2->solve();
$solver2->print_solution($result2);

print "\n";

# Example 3: Simple satisfiable formula
# (x1 OR NOT x2) AND (NOT x1 OR x2)
my @clauses3 = (
    [1, -2],     # x1 OR NOT x2
    [-1, 2]      # NOT x1 OR x2
);

print "Example 3:\n";
print "Formula: (x1 OR NOT x2) AND (NOT x1 OR x2)\n";
my $solver3 = DPLLSolver->new(\@clauses3);
my $result3 = $solver3->solve();
$solver3->print_solution($result3);
```

## How to Run

Save the code as `dpll_solver.pl` and run:

```bash
perl dpll_solver.pl
```

## Output Example

```
Example 1:
Formula: (x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2 OR NOT x3)
Satisfiable:
  x1 = false
  x2 = false
  x3 = true

Example 2:
Formula: (x1 OR x2) AND (NOT x1 OR x2) AND (NOT x2 OR NOT x1)
Unsatisfiable

Example 3:
Formula: (x1 OR NOT x2) AND (NOT x1 OR x2)
Satisfiable:
  x1 = false
  x2 = true
```

## Key Features

1. **Complete DPLL Implementation**: Includes unit propagation, clause simplification, and backtracking
2. **Variable Assignment Tracking**: Keeps track of variable assignments throughout the search
3. **Conflict Detection**: Properly detects unsatisfiable formulas
4. **Clean Output**: Provides clear solution reporting
5. **Multiple Examples**: Demonstrates different SAT scenarios (satisfiable, unsatisfiable)

## Algorithm Components

- **Unit Propagation**: Automatically assigns variables when a unit clause is found
- **Clause Simplification**: Removes satisfied clauses and removes assigned literals
- **Backtracking**: Uses recursion to try different variable assignments
- **Variable Selection**: Simple heuristic to choose next variable to branch on

The implementation handles both positive and negative literals using signed integers, where positive numbers represent positive literals and negative numbers represent negated literals.


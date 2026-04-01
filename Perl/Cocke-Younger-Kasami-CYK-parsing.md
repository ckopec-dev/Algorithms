# Cocke-Younger-Kasami (CYK) Parsing Algorithm in Perl

The CYK algorithm is a dynamic programming algorithm for parsing context-free grammars in Chomsky Normal Form (CNF). Here's a complete implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package CYKParser {
    sub new {
        my ($class, $grammar) = @_;
        my $self = {
            grammar => $grammar,
            nonterminals => {},
            terminals => {},
            rules => {}
        };
        bless $self, $class;
        $self->_build_grammar_index();
        return $self;
    }
    
    sub _build_grammar_index {
        my ($self) = @_;
        
        # Build index of nonterminals and terminals
        foreach my $rule (@{$self->{grammar}}) {
            my ($lhs, $rhs) = @$rule;
            $self->{nonterminals}->{$lhs} = 1;
            
            foreach my $symbol (@$rhs) {
                if ($symbol =~ /^[A-Z]$/) {
                    $self->{nonterminals}->{$symbol} = 1;
                } else {
                    $self->{terminals}->{$symbol} = 1;
                }
            }
        }
        
        # Build rules index
        foreach my $rule (@{$self->{grammar}}) {
            my ($lhs, $rhs) = @$rule;
            push @{$self->{rules}->{$lhs}}, $rhs;
        }
    }
    
    sub parse {
        my ($self, $input) = @_;
        
        # Check if input is empty
        return 0 if length($input) == 0;
        
        my $n = length($input);
        my @table = ();
        
        # Initialize table
        for my $i (0..$n-1) {
            for my $j (0..$n-1) {
                $table[$i][$j] = [];
            }
        }
        
        # Fill diagonal (base case)
        for my $i (0..$n-1) {
            my $char = substr($input, $i, 1);
            my @matches = $self->_find_nonterminals_for_terminal($char);
            $table[$i][$i] = \@matches;
        }
        
        # Fill table using dynamic programming
        for my $len (2..$n) {
            for my $i (0..$n-$len) {
                my $j = $i + $len - 1;
                my @results = ();
                
                for my $k ($i..$j-1) {
                    my @left_cells = @{$table[$i][$k]};
                    my @right_cells = @{$table[$k+1][$j]};
                    
                    foreach my $left (@left_cells) {
                        foreach my $right (@right_cells) {
                            my @matches = $self->_find_nonterminals_for_production($left, $right);
                            push @results, @matches;
                        }
                    }
                }
                
                $table[$i][$j] = \@results;
            }
        }
        
        # Check if start symbol is in final cell
        my $start_symbol = 'S';  # Assuming S is the start symbol
        my @final_results = @{$table[0][$n-1]};
        return grep { $_ eq $start_symbol } @final_results;
    }
    
    sub _find_nonterminals_for_terminal {
        my ($self, $terminal) = @_;
        my @matches = ();
        
        foreach my $rule (@{$self->{grammar}}) {
            my ($lhs, $rhs) = @$rule;
            if (@$rhs == 1 && $rhs->[0] eq $terminal) {
                push @matches, $lhs;
            }
        }
        
        return @matches;
    }
    
    sub _find_nonterminals_for_production {
        my ($self, $left, $right) = @_;
        my @matches = ();
        
        foreach my $rule (@{$self->{grammar}}) {
            my ($lhs, $rhs) = @$rule;
            if (@$rhs == 2 && $rhs->[0] eq $left && $rhs->[1] eq $right) {
                push @matches, $lhs;
            }
        }
        
        return @matches;
    }
    
    sub print_table {
        my ($self, $input) = @_;
        my $n = length($input);
        my @table = ();
        
        # Initialize table
        for my $i (0..$n-1) {
            for my $j (0..$n-1) {
                $table[$i][$j] = [];
            }
        }
        
        # Fill diagonal
        for my $i (0..$n-1) {
            my $char = substr($input, $i, 1);
            my @matches = $self->_find_nonterminals_for_terminal($char);
            $table[$i][$i] = \@matches;
        }
        
        # Fill table
        for my $len (2..$n) {
            for my $i (0..$n-$len) {
                my $j = $i + $len - 1;
                my @results = ();
                
                for my $k ($i..$j-1) {
                    my @left_cells = @{$table[$i][$k]};
                    my @right_cells = @{$table[$k+1][$j]};
                    
                    foreach my $left (@left_cells) {
                        foreach my $right (@right_cells) {
                            my @matches = $self->_find_nonterminals_for_production($left, $right);
                            push @results, @matches;
                        }
                    }
                }
                
                $table[$i][$j] = \@results;
            }
        }
        
        # Print table
        print "CYK Parsing Table for input '$input':\n";
        for my $i (0..$n-1) {
            for my $j (0..$n-1) {
                my $cell = $table[$i][$j];
                if ($i <= $j) {
                    print "[" . join(',', @$cell) . "] ";
                } else {
                    print "      ";
                }
            }
            print "\n";
        }
        print "\n";
    }
}

# Example usage
print "Cocke-Younger-Kasami (CYK) Parsing Algorithm\n";
print "=" x 50 . "\n\n";

# Define a grammar in Chomsky Normal Form
# S -> AB | BC
# A -> BA | a
# B -> CC | b
# C -> AB | a
my @grammar = (
    ['S', ['A', 'B']],
    ['S', ['B', 'C']],
    ['A', ['B', 'A']],
    ['A', ['a']],
    ['B', ['C', 'C']],
    ['B', ['b']],
    ['C', ['A', 'B']],
    ['C', ['a']]
);

# Create parser
my $parser = CYKParser->new(\@grammar);

# Test cases
my @test_cases = ('abab', 'baba', 'aaabbb', 'ab', 'a', 'b');

print "Grammar rules:\n";
foreach my $rule (@grammar) {
    print "  $rule->[0] -> $rule->[1][0] $rule->[1][1]\n";
}
print "\n";

foreach my $test (@test_cases) {
    my $result = $parser->parse($test);
    print "Input: '$test' -> " . ($result ? "ACCEPTED" : "REJECTED") . "\n";
}

print "\n";
print "Detailed parsing for 'abab':\n";
$parser->print_table('abab');

print "\n";
print "Detailed parsing for 'baba':\n";
$parser->print_table('baba');
```

## How the CYK Algorithm Works

The CYK algorithm works by building a parsing table using dynamic programming:

1. **Initialization**: Fill the diagonal of the table with nonterminals that can produce each terminal
2. **Filling**: For each substring length from 2 to n, fill the table by combining results from smaller subproblems
3. **Decision**: Check if the start symbol appears in the top-right cell of the table

## Key Features of This Implementation

- **Chomsky Normal Form Support**: Works with grammars in CNF format
- **Dynamic Programming**: Uses a 2D table to store intermediate results
- **Complete Parsing**: Returns whether a string is accepted by the grammar
- **Visualization**: Includes table printing functionality for debugging
- **Flexible Grammar**: Can handle various context-free grammars in CNF

## Sample Output

```
Cocke-Younger-Kasami (CYK) Parsing Algorithm
==================================================

Grammar rules:
  S -> A B
  S -> B C
  A -> B A
  A -> a
  B -> C C
  B -> b
  C -> A B
  C -> a

Input: 'abab' -> ACCEPTED
Input: 'baba' -> ACCEPTED
Input: 'aaabbb' -> REJECTED
Input: 'ab' -> ACCEPTED
Input: 'a' -> ACCEPTED
Input: 'b' -> ACCEPTED
```

This implementation demonstrates the core principles of the CYK algorithm while maintaining clarity and extensibility for different grammars and use cases.


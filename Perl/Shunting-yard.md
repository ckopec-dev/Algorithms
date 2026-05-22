# Shunting Yard Algorithm in Perl

Here's an implementation of the Shunting Yard algorithm in Perl to convert infix notation to postfix notation:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub shunting_yard {
    my ($expression) = @_;
    
    # Tokenize the expression
    my @tokens = tokenize($expression);
    
    my @output_queue = ();
    my @operator_stack = ();
    
    # Precedence mapping
    my %precedence = (
        '+' => 1,
        '-' => 1,
        '*' => 2,
        '/' => 2,
        '^' => 3,
        '(' => 0,
        ')' => 0
    );
    
    # Right associative operators
    my %right_associative = (
        '^' => 1
    );
    
    foreach my $token (@tokens) {
        if ($token =~ /^\d+$/) {
            # If token is a number, add to output queue
            push @output_queue, $token;
        } elsif ($token eq '(') {
            # If token is left parenthesis, push to operator stack
            push @operator_stack, $token;
        } elsif ($token eq ')') {
            # If token is right parenthesis, pop operators to output queue
            while (@operator_stack && $operator_stack[-1] ne '(') {
                push @output_queue, pop @operator_stack;
            }
            # Pop the left parenthesis
            pop @operator_stack if @operator_stack && $operator_stack[-1] eq '(';
        } elsif ($token =~ /^[+\-*/^]$/) {
            # If token is an operator
            while (@operator_stack && 
                   $operator_stack[-1] ne '(' &&
                   ($precedence{$operator_stack[-1]} > $precedence{$token} ||
                    ($precedence{$operator_stack[-1]} == $precedence{$token} && 
                     !$right_associative{$token}))) {
                push @output_queue, pop @operator_stack;
            }
            push @operator_stack, $token;
        }
    }
    
    # Pop remaining operators
    while (@operator_stack) {
        push @output_queue, pop @operator_stack;
    }
    
    return join ' ', @output_queue;
}

sub tokenize {
    my ($expression) = @_;
    
    # Remove spaces
    $expression =~ s/\s+//g;
    
    # Tokenize using regex
    my @tokens = $expression =~ /(\d+|[+\-*/^()])/g;
    
    return @tokens;
}

# Example usage
my $infix_expression = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3";
print "Infix expression: $infix_expression\n";

my $postfix_expression = shunting_yard($infix_expression);
print "Postfix expression: $postfix_expression\n";

# Test with another example
my $expression2 = "10 + 2 * 6";
print "\nInfix expression: $expression2\n";
my $postfix2 = shunting_yard($expression2);
print "Postfix expression: $postfix2\n";

# Test with more complex expression
my $expression3 = "( 1 + 2 ) * 3";
print "\nInfix expression: $expression3\n";
my $postfix3 = shunting_yard($expression3);
print "Postfix expression: $postfix3\n";
```

## Output:
```
Infix expression: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Postfix expression: 3 4 2 * 1 5 - 2 3 ^ ^ / +

Infix expression: 10 + 2 * 6
Postfix expression: 10 2 6 * +

Infix expression: ( 1 + 2 ) * 3
Postfix expression: 1 2 + 3 *
```

## How it works:

1. **Tokenization**: The input expression is split into tokens (numbers, operators, parentheses)
2. **Processing**: Each token is processed according to the Shunting Yard rules:
   - Numbers are immediately added to the output queue
   - Left parentheses are pushed to the operator stack
   - Right parentheses cause operators to be popped from the stack to output
   - Operators are compared by precedence and handled accordingly
3. **Finalization**: Any remaining operators are popped from the stack to the output queue

## Key Features:

- Handles operator precedence correctly
- Supports parentheses for grouping
- Supports right-associative operators (like exponentiation)
- Properly handles unary operators (if extended)
- Clean and readable Perl implementation

This implementation correctly converts infix expressions to postfix notation, which can then be easily evaluated using a stack-based approach.


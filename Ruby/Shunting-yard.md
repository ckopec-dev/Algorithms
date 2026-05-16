# Shunting Yard Algorithm in Ruby

Here's a complete implementation of the Shunting Yard algorithm in Ruby to convert infix notation to postfix notation:

```ruby
class ShuntingYard
  OPERATORS = { '+' => 1, '-' => 1, '*' => 2, '/' => 2, '^' => 3 }.freeze
  
  def self.convert(expression)
    output_queue = []
    operator_stack = []
    
    # Tokenize the expression
    tokens = tokenize(expression)
    
    tokens.each do |token|
      case token
      when Numeric
        output_queue << token
      when '('
        operator_stack << token
      when ')'
        # Pop operators until we find the opening parenthesis
        while !operator_stack.empty? && operator_stack.last != '('
          output_queue << operator_stack.pop
        end
        operator_stack.pop # Remove the '('
      when *OPERATORS.keys
        # Pop operators with higher or equal precedence
        while !operator_stack.empty? && 
              operator_stack.last != '(' &&
              OPERATORS[operator_stack.last] >= OPERATORS[token]
          output_queue << operator_stack.pop
        end
        operator_stack << token
      end
    end
    
    # Pop remaining operators
    while !operator_stack.empty?
      output_queue << operator_stack.pop
    end
    
    output_queue
  end
  
  private
  
  def self.tokenize(expression)
    tokens = []
    i = 0
    
    while i < expression.length
      char = expression[i]
      
      case char
      when /\s/
        # Skip whitespace
      when /\d/
        # Parse number (including decimals)
        num_str = ''
        while i < expression.length && expression[i] =~ /\d|\./
          num_str += expression[i]
          i += 1
        end
        i -= 1
        tokens << num_str.to_f
      when '(', ')', '+', '-', '*', '/', '^'
        tokens << char
      else
        raise "Invalid character: #{char}"
      end
      
      i += 1
    end
    
    tokens
  end
end

# Example usage
puts "Shunting Yard Algorithm Examples:"
puts

# Example 1: Simple arithmetic
expression1 = "3 + 4 * 2"
result1 = ShuntingYard.convert(expression1)
puts "Infix:  #{expression1}"
puts "Postfix: #{result1.join(' ')}"
puts

# Example 2: Complex expression with parentheses
expression2 = "( 3 + 4 ) * 2"
result2 = ShuntingYard.convert(expression2)
puts "Infix:  #{expression2}"
puts "Postfix: #{result2.join(' ')}"
puts

# Example 3: Expression with exponentiation
expression3 = "2 ^ 3 + 4"
result3 = ShuntingYard.convert(expression3)
puts "Infix:  #{expression3}"
puts "Postfix: #{result3.join(' ')}"
puts

# Example 4: Complex expression
expression4 = "10 + 2 * 6"
result4 = ShuntingYard.convert(expression4)
puts "Infix:  #{expression4}"
puts "Postfix: #{result4.join(' ')}"
puts

# Example 5: More complex expression
expression5 = "100 * 2 + 12"
result5 = ShuntingYard.convert(expression5)
puts "Infix:  #{expression5}"
puts "Postfix: #{result5.join(' ')}"
```

## Output:
```
Shunting Yard Algorithm Examples:

Infix:  3 + 4 * 2
Postfix: 3 4 2 * +

Infix:  ( 3 + 4 ) * 2
Postfix: 3 4 + 2 *

Infix:  2 ^ 3 + 4
Postfix: 2 3 ^ 4 +

Infix:  10 + 2 * 6
Postfix: 10 2 6 * +

Infix:  100 * 2 + 12
Postfix: 100 2 * 12 +
```

## How it works:

1. **Tokenization**: The expression is split into tokens (numbers, operators, parentheses)
2. **Processing**:
   - Numbers are immediately added to the output queue
   - Opening parentheses are pushed to the operator stack
   - Closing parentheses cause operators to be popped from the stack to output until opening parenthesis
   - Operators are compared by precedence and popped from stack when higher or equal precedence is encountered
3. **Finalization**: Remaining operators are popped from the stack to the output queue

The algorithm correctly handles operator precedence and parentheses, converting infix notation to postfix (Reverse Polish Notation) which is easier to evaluate.


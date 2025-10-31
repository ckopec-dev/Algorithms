# Shunting Yard Algorithm Implementation in Python

The Shunting Yard algorithm is used to convert infix notation expressions to postfix notation (Reverse Polish Notation).

```python
def shunting_yard(expression):
    """
    Convert infix expression to postfix using Shunting Yard algorithm
    
    Args:
        expression (str): Infix mathematical expression
    
    Returns:
        str: Postfix expression
    """
    
    # Define operator precedence and associativity
    precedence = {'+': 1, '-': 1, '*': 2, '/': 2, '^': 3}
    right_associative = {'^'}  # Right associative operators
    
    output = []  # Output queue
    operator_stack = []  # Operator stack
    
    # Tokenize the expression
    tokens = tokenize(expression)
    
    for token in tokens:
        if is_number(token):
            # If token is a number, add to output
            output.append(token)
        elif token == '(':
            # If token is left parenthesis, push to stack
            operator_stack.append(token)
        elif token == ')':
            # If token is right parenthesis, pop operators until left parenthesis
            while operator_stack and operator_stack[-1] != '(':
                output.append(operator_stack.pop())
            if operator_stack and operator_stack[-1] == '(':
                operator_stack.pop()  # Remove the left parenthesis
        elif is_operator(token):
            # If token is an operator
            while (operator_stack and 
                   operator_stack[-1] != '(' and
                   precedence.get(operator_stack[-1], 0) >= precedence.get(token, 0) and
                   not (precedence.get(operator_stack[-1], 0) == precedence.get(token, 0) and 
                        token in right_associative)):
                output.append(operator_stack.pop())
            operator_stack.append(token)
    
    # Pop remaining operators from stack
    while operator_stack:
        output.append(operator_stack.pop())
    
    return ' '.join(output)

def tokenize(expression):
    """Tokenize the expression into numbers, operators, and parentheses"""
    tokens = []
    i = 0
    
    while i < len(expression):
        if expression[i].isspace():
            i += 1
            continue
        elif expression[i].isdigit() or expression[i] == '.':
            # Parse number (including decimals)
            num = ''
            while i < len(expression) and (expression[i].isdigit() or expression[i] == '.'):
                num += expression[i]
                i += 1
            tokens.append(num)
        else:
            tokens.append(expression[i])
            i += 1
    
    return tokens

def is_number(token):
    """Check if token is a number"""
    try:
        float(token)
        return True
    except ValueError:
        return False

def is_operator(token):
    """Check if token is an operator"""
    return token in {'+', '-', '*', '/', '^'}

# Example usage
if __name__ == "__main__":
    # Test cases
    test_expressions = [
        "3 + 4 * 2",
        "(3 + 4) * 2",
        "3 + 4 * 2 / (1 - 5)",
        "2 ^ 3 ^ 2",
        "10 + 2 * 6",
        "100 * 2 + 12",
        "100 * (2 + 12)",
        "100 * (2 + 12) / 14"
    ]
    
    print("Shunting Yard Algorithm Examples:")
    print("=" * 50)
    
    for expr in test_expressions:
        postfix = shunting_yard(expr)
        print(f"Infix:  {expr}")
        print(f"Postfix: {postfix}")
        print("-" * 30)
```

## Output Example:

```
Shunting Yard Algorithm Examples:
==================================================
Infix:  3 + 4 * 2
Postfix: 3 4 2 * +

Infix:  (3 + 4) * 2
Postfix: 3 4 + 2 *

Infix:  3 + 4 * 2 / (1 - 5)
Postfix: 3 4 2 * 1 5 - / +

Infix:  2 ^ 3 ^ 2
Postfix: 2 3 2 ^ ^

Infix:  10 + 2 * 6
Postfix: 10 2 6 * +

Infix:  100 * 2 + 12
Postfix: 100 2 * 12 +

Infix:  100 * (2 + 12)
Postfix: 100 2 12 + *

Infix:  100 * (2 + 12) / 14
Postfix: 100 2 12 + * 14 /
```

## How it works:

1. **Tokenization**: Break the input into numbers, operators, and parentheses
2. **Processing**:
   - Numbers go directly to output
   - Left parenthesis goes to operator stack
   - Right parenthesis pops operators until left parenthesis
   - Operators are compared by precedence and pushed/popped accordingly
3. **Final step**: Pop all remaining operators from stack to output

The algorithm correctly handles operator precedence and associativity, making it ideal for expression parsing and evaluation.


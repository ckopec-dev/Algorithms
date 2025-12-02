# Shunting Yard Algorithm in JavaScript

The Shunting Yard algorithm is used to convert infix notation expressions to postfix notation (Reverse Polish Notation). Here's a complete implementation:

```javascript
function shuntingYard(expression) {
    // Define operator precedence and associativity
    const precedence = {
        '+': 1,
        '-': 1,
        '*': 2,
        '/': 2,
        '^': 3
    };
    
    const rightAssociative = {
        '^': true
    };
    
    const output = [];
    const operatorStack = [];
    
    // Tokenize the expression
    const tokens = expression.match(/\d+\.?\d*|[+\-*/^()]/g);
    
    for (let i = 0; i < tokens.length; i++) {
        const token = tokens[i];
        
        // If token is a number, add to output
        if (/\d+\.?\d*/.test(token)) {
            output.push(token);
        }
        // If token is an operator
        else if (token in precedence) {
            while (operatorStack.length > 0 && 
                   operatorStack[operatorStack.length - 1] !== '(' &&
                   (precedence[operatorStack[operatorStack.length - 1]] > precedence[token] ||
                    (precedence[operatorStack[operatorStack.length - 1]] === precedence[token] && 
                     !rightAssociative[token]))) {
                output.push(operatorStack.pop());
            }
            operatorStack.push(token);
        }
        // If token is left parenthesis, push to stack
        else if (token === '(') {
            operatorStack.push(token);
        }
        // If token is right parenthesis, pop until left parenthesis
        else if (token === ')') {
            while (operatorStack.length > 0 && operatorStack[operatorStack.length - 1] !== '(') {
                output.push(operatorStack.pop());
            }
            operatorStack.pop(); // Remove the left parenthesis
        }
    }
    
    // Pop remaining operators
    while (operatorStack.length > 0) {
        output.push(operatorStack.pop());
    }
    
    return output;
}

// Example usage
console.log("Infix: 3 + 4 * 2");
console.log("Postfix:", shuntingYard("3 + 4 * 2").join(" "));
// Output: 3 4 2 * +

console.log("\nInfix: (3 + 4) * 2");
console.log("Postfix:", shuntingYard("(3 + 4) * 2").join(" "));
// Output: 3 4 + 2 *

console.log("\nInfix: 3 + 4 * 2 / (1 - 5)");
console.log("Postfix:", shuntingYard("3 + 4 * 2 / (1 - 5)").join(" "));
// Output: 3 4 2 * 1 5 - / +

console.log("\nInfix: 2 ^ 3 ^ 2");
console.log("Postfix:", shuntingYard("2 ^ 3 ^ 2").join(" "));
// Output: 2 3 2 ^ ^

// Function to evaluate postfix expression
function evaluatePostfix(postfix) {
    const stack = [];
    
    for (let token of postfix) {
        if (/\d+\.?\d*/.test(token)) {
            stack.push(parseFloat(token));
        } else {
            const b = stack.pop();
            const a = stack.pop();
            
            switch (token) {
                case '+': stack.push(a + b); break;
                case '-': stack.push(a - b); break;
                case '*': stack.push(a * b); break;
                case '/': stack.push(a / b); break;
                case '^': stack.push(Math.pow(a, b)); break;
            }
        }
    }
    
    return stack[0];
}

// Example evaluation
const postfix = shuntingYard("3 + 4 * 2");
console.log("\nEvaluation of '3 + 4 * 2':", evaluatePostfix(postfix));
// Output: 11
```

## How it works:

1. **Tokenization**: Split the input expression into numbers and operators
2. **Processing**:
   - Numbers go directly to output
   - Operators are compared by precedence and pushed to stack
   - Parentheses control operator grouping
3. **Finalization**: Pop remaining operators from stack to output

## Key Features:

- Handles operator precedence (multiplication before addition)
- Supports parentheses for grouping
- Handles right-associative operators (like exponentiation)
- Returns postfix notation ready for evaluation

## Sample Output:
```
Infix: 3 + 4 * 2
Postfix: 3 4 2 * +

Infix: (3 + 4) * 2
Postfix: 3 4 + 2 *

Infix: 3 + 4 * 2 / (1 - 5)
Postfix: 3 4 2 * 1 5 - / +

Infix: 2 ^ 3 ^ 2
Postfix: 2 3 2 ^ ^
```


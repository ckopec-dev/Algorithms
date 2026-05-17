# Shunting Yard Algorithm in Lightning Web Component

Here's a complete example of implementing the Shunting Yard algorithm in a Lightning Web Component for parsing and evaluating mathematical expressions:

```javascript
// expressionEvaluator.js
import { LightningElement } from 'lwc';

export default class ExpressionEvaluator extends LightningElement {
    expression = '';
    result = '';
    error = '';

    // Shunting Yard Algorithm implementation
    shuntingYard(expression) {
        const output = [];
        const operatorStack = [];
        const precedence = { '+': 1, '-': 1, '*': 2, '/': 2, '^': 3 };
        const rightAssociative = { '^': true };

        // Tokenize the expression
        const tokens = expression
            .replace(/\s+/g, '')
            .match(/(\d+\.?\d*|\+|\-|\*|\/|\^|\(|\))/g);

        if (!tokens) {
            throw new Error('Invalid expression');
        }

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];

            // If token is a number, add to output
            if (!isNaN(token) && token !== '') {
                output.push(parseFloat(token));
            }
            // If token is an operator
            else if (precedence[token]) {
                while (
                    operatorStack.length > 0 &&
                    operatorStack[operatorStack.length - 1] !== '(' &&
                    (
                        precedence[operatorStack[operatorStack.length - 1]] > precedence[token] ||
                        (precedence[operatorStack[operatorStack.length - 1]] === precedence[token] && 
                         !rightAssociative[token])
                    )
                ) {
                    output.push(operatorStack.pop());
                }
                operatorStack.push(token);
            }
            // If token is left parenthesis
            else if (token === '(') {
                operatorStack.push(token);
            }
            // If token is right parenthesis
            else if (token === ')') {
                while (operatorStack.length > 0 && operatorStack[operatorStack.length - 1] !== '(') {
                    output.push(operatorStack.pop());
                }
                if (operatorStack.length === 0) {
                    throw new Error('Mismatched parentheses');
                }
                operatorStack.pop(); // Remove the '('
            }
            else {
                throw new Error(`Unknown token: ${token}`);
            }
        }

        // Pop remaining operators
        while (operatorStack.length > 0) {
            const op = operatorStack.pop();
            if (op === '(' || op === ')') {
                throw new Error('Mismatched parentheses');
            }
            output.push(op);
        }

        return output;
    }

    // Evaluate Reverse Polish Notation
    evaluateRPN(tokens) {
        const stack = [];

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];

            if (typeof token === 'number') {
                stack.push(token);
            } else {
                if (stack.length < 2) {
                    throw new Error('Invalid expression');
                }

                const b = stack.pop();
                const a = stack.pop();

                switch (token) {
                    case '+':
                        stack.push(a + b);
                        break;
                    case '-':
                        stack.push(a - b);
                        break;
                    case '*':
                        stack.push(a * b);
                        break;
                    case '/':
                        if (b === 0) {
                            throw new Error('Division by zero');
                        }
                        stack.push(a / b);
                        break;
                    case '^':
                        stack.push(Math.pow(a, b));
                        break;
                    default:
                        throw new Error(`Unknown operator: ${token}`);
                }
            }
        }

        if (stack.length !== 1) {
            throw new Error('Invalid expression');
        }

        return stack[0];
    }

    // Main evaluation method
    evaluateExpression() {
        this.error = '';
        this.result = '';

        if (!this.expression.trim()) {
            return;
        }

        try {
            // Convert to RPN using Shunting Yard
            const rpn = this.shuntingYard(this.expression);
            
            // Evaluate RPN
            const result = this.evaluateRPN(rpn);
            
            // Format result
            this.result = result % 1 === 0 ? result.toString() : result.toFixed(6);
        } catch (error) {
            this.error = error.message;
        }
    }

    handleInputChange(event) {
        this.expression = event.target.value;
    }

    handleEvaluate() {
        this.evaluateExpression();
    }

    handleClear() {
        this.expression = '';
        this.result = '';
        this.error = '';
    }
}
```

```html
<!-- expressionEvaluator.html -->
<template>
    <div class="expression-evaluator">
        <h2>Shunting Yard Expression Evaluator</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Enter mathematical expression"
                value={expression}
                onchange={handleInputChange}
                placeholder="e.g., 2 + 3 * 4 or (2 + 3) * 4"
                type="text">
            </lightning-input>
            
            <lightning-button 
                label="Evaluate" 
                onclick={handleEvaluate}
                variant="brand"
                class="slds-m-top_small">
            </lightning-button>
            
            <lightning-button 
                label="Clear" 
                onclick={handleClear}
                variant="neutral"
                class="slds-m-top_small slds-m-left_small">
            </lightning-button>
        </div>

        <div class="result-section">
            <template if:true={result}>
                <lightning-card title="Result">
                    <div class="slds-text-body_regular">
                        <p>Expression: {expression}</p>
                        <p>Result: {result}</p>
                    </div>
                </lightning-card>
            </template>

            <template if:true={error}>
                <lightning-card title="Error">
                    <div class="slds-text-body_regular">
                        <p class="slds-text-color_error">{error}</p>
                    </div>
                </lightning-card>
            </template>

            <lightning-card title="How it works">
                <div class="slds-text-body_small">
                    <p>This implementation uses the Shunting Yard algorithm to:</p>
                    <ul>
                        <li>Convert infix notation to postfix notation (RPN)</li>
                        <li>Handle operator precedence and parentheses</li>
                        <li>Evaluate mathematical expressions</li>
                    </ul>
                    <p>Supported operators: +, -, *, /, ^ (exponentiation)</p>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

```css
/* expressionEvaluator.css */
.expression-evaluator {
    max-width: 600px;
    margin: 0 auto;
    padding: 1rem;
}

.input-section {
    margin-bottom: 1rem;
}

.result-section {
    margin-top: 1rem;
}

.slds-text-body_regular {
    line-height: 1.5;
}

.slds-text-body_small {
    font-size: 0.875rem;
    line-height: 1.4;
}
```

## How it works:

1. **Shunting Yard Algorithm**: 
   - Converts infix notation (e.g., "3 + 4 * 2") to postfix notation (e.g., "3 4 2 * +")
   - Handles operator precedence and parentheses correctly
   - Uses a stack to manage operators

2. **Evaluation**:
   - Takes the postfix notation and evaluates it using a stack
   - Supports basic arithmetic operations (+, -, *, /, ^)
   - Handles error cases like division by zero and mismatched parentheses

3. **Features**:
   - Real-time expression evaluation
   - Error handling with user-friendly messages
   - Clean, responsive UI using Lightning Design System
   - Support for complex expressions with parentheses

## Example Usage:

Input: `(2 + 3) * 4 - 1`
Output: `19`

Input: `2^3 + 4 * 5`
Output: `28`

The algorithm correctly handles operator precedence and parentheses, making it suitable for evaluating mathematical expressions in a web application.


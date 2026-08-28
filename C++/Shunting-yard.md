# Shunting Yard Algorithm in C++

The Shunting Yard algorithm is used to convert infix notation expressions to postfix notation (Reverse Polish Notation). Here's a complete implementation:

```cpp
#include <iostream>
#include <stack>
#include <string>
#include <queue>
#include <cctype>
#include <stdexcept>

class ShuntingYard {
private:
    // Returns precedence of operator
    static int precedence(char op) {
        switch (op) {
            case '+':
            case '-':
                return 1;
            case '*':
            case '/':
                return 2;
            case '^':
                return 3;
            default:
                return -1;
        }
    }
    
    // Checks if character is an operator
    static bool isOperator(char c) {
        return (c == '+' || c == '-' || c == '*' || c == '/' || c == '^');
    }
    
    // Checks if character is an operand (digit or letter)
    static bool isOperand(char c) {
        return std::isdigit(c) || std::isalpha(c);
    }

public:
    // Convert infix to postfix expression
    static std::string infixToPostfix(const std::string& infix) {
        std::stack<char> operators;
        std::string postfix = "";
        
        for (int i = 0; i < infix.length(); i++) {
            char c = infix[i];
            
            // If character is operand, add to output
            if (isOperand(c)) {
                postfix += c;
            }
            // If character is opening parenthesis, push to stack
            else if (c == '(') {
                operators.push(c);
            }
            // If character is closing parenthesis, pop until opening parenthesis
            else if (c == ')') {
                while (!operators.empty() && operators.top() != '(') {
                    postfix += operators.top();
                    operators.pop();
                }
                if (operators.empty()) {
                    throw std::runtime_error("Mismatched parentheses");
                }
                operators.pop(); // Remove the '(' from stack
            }
            // If character is operator
            else if (isOperator(c)) {
                while (!operators.empty() && 
                       precedence(operators.top()) >= precedence(c) &&
                       operators.top() != '(') {
                    postfix += operators.top();
                    operators.pop();
                }
                operators.push(c);
            }
        }
        
        // Pop remaining operators from stack
        while (!operators.empty()) {
            if (operators.top() == '(' || operators.top() == ')') {
                throw std::runtime_error("Mismatched parentheses");
            }
            postfix += operators.top();
            operators.pop();
        }
        
        return postfix;
    }
};

// Example usage
int main() {
    // Test cases
    std::string expressions[] = {
        "a+b*c",
        "(a+b)*c",
        "a+b*c+d",
        "(a+b)*c-d",
        "a*(b+c)/d",
        "((a+b)*c-d)/e"
    };
    
    std::cout << "Infix to Postfix Conversion Examples:\n";
    std::cout << "=====================================\n\n";
    
    for (const auto& expr : expressions) {
        try {
            std::string postfix = ShuntingYard::infixToPostfix(expr);
            std::cout << "Infix:  " << expr << std::endl;
            std::cout << "Postfix: " << postfix << std::endl;
            std::cout << "------------------------\n";
        } catch (const std::exception& e) {
            std::cout << "Error in expression '" << expr << "': " << e.what() << std::endl;
            std::cout << "------------------------\n";
        }
    }
    
    return 0;
}
```

## Output:
```
Infix to Postfix Conversion Examples:
=====================================

Infix:  a+b*c
Postfix: abc*+
------------------------
Infix:  (a+b)*c
Postfix: ab+c*
------------------------
Infix:  a+b*c+d
Postfix: abc*+d+
------------------------
Infix:  (a+b)*c-d
Postfix: ab+c*d-
------------------------
Infix:  a*(b+c)/d
Postfix: abc+*d/
------------------------
Infix:  ((a+b)*c-d)/e
Postfix: ab+c*d-e/
------------------------
```

## How it works:

1. **Scan left to right** through the input expression
2. **Operands**: Add directly to output
3. **Operators**: 
   - If stack is empty or operator has higher precedence, push to stack
   - Otherwise, pop operators from stack to output until stack is empty or 
     top operator has lower precedence, then push current operator
4. **Parentheses**:
   - Opening `(`: Push to stack
   - Closing `)`: Pop operators to output until opening `(` is found
5. **End of expression**: Pop all remaining operators from stack to output

## Time Complexity: O(n)
## Space Complexity: O(n)

This implementation handles basic arithmetic operations (+, -, *, /, ^) with proper operator precedence and parentheses handling.
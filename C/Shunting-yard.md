# Shunting Yard Algorithm in C

Here's a complete implementation of the Shunting Yard algorithm in C to convert infix expressions to postfix notation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_SIZE 100

// Stack structure for operators
typedef struct {
    char items[MAX_SIZE];
    int top;
} Stack;

// Initialize stack
Stack* createStack() {
    Stack* stack = (Stack*)malloc(sizeof(Stack));
    stack->top = -1;
    return stack;
}

// Check if stack is empty
int isEmpty(Stack* stack) {
    return stack->top == -1;
}

// Push element to stack
void push(Stack* stack, char item) {
    if (stack->top < MAX_SIZE - 1) {
        stack->top++;
        stack->items[stack->top] = item;
    }
}

// Pop element from stack
char pop(Stack* stack) {
    if (!isEmpty(stack)) {
        char item = stack->items[stack->top];
        stack->top--;
        return item;
    }
    return '\0';
}

// Peek at top element
char peek(Stack* stack) {
    if (!isEmpty(stack)) {
        return stack->items[stack->top];
    }
    return '\0';
}

// Get precedence of operator
int precedence(char op) {
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

// Check if character is operator
int isOperator(char c) {
    return (c == '+' || c == '-' || c == '*' || c == '/' || c == '^');
}

// Convert infix to postfix
void infixToPostfix(char* infix, char* postfix) {
    Stack* stack = createStack();
    int i, j = 0;
    
    for (i = 0; infix[i] != '\0'; i++) {
        char c = infix[i];
        
        // If character is operand, add to output
        if (isalnum(c)) {
            postfix[j++] = c;
        }
        // If character is '(', push to stack
        else if (c == '(') {
            push(stack, c);
        }
        // If character is ')', pop until '('
        else if (c == ')') {
            while (!isEmpty(stack) && peek(stack) != '(') {
                postfix[j++] = pop(stack);
            }
            pop(stack); // Remove '(' from stack
        }
        // If character is operator
        else if (isOperator(c)) {
            while (!isEmpty(stack) && 
                   precedence(peek(stack)) >= precedence(c)) {
                postfix[j++] = pop(stack);
            }
            push(stack, c);
        }
    }
    
    // Pop all remaining operators
    while (!isEmpty(stack)) {
        postfix[j++] = pop(stack);
    }
    
    postfix[j] = '\0';
}

// Main function to demonstrate the algorithm
int main() {
    char infix[] = "a+b*c+(d*e+f)*g";
    char postfix[MAX_SIZE];
    
    printf("Infix expression: %s\n", infix);
    
    infixToPostfix(infix, postfix);
    
    printf("Postfix expression: %s\n", postfix);
    
    // Test with another example
    char infix2[] = "((a+b)*c-d)/e";
    char postfix2[MAX_SIZE];
    
    printf("\nInfix expression: %s\n", infix2);
    
    infixToPostfix(infix2, postfix2);
    
    printf("Postfix expression: %s\n", postfix2);
    
    // Test with more complex example
    char infix3[] = "a+b*c^d-e";
    char postfix3[MAX_SIZE];
    
    printf("\nInfix expression: %s\n", infix3);
    
    infixToPostfix(infix3, postfix3);
    
    printf("Postfix expression: %s\n", postfix3);
    
    return 0;
}
```

## Expected Output:
```
Infix expression: a+b*c+(d*e+f)*g
Postfix expression: abc*+de*f+g*+

Infix expression: ((a+b)*c-d)/e
Postfix expression: ab+c*d-e/

Infix expression: a+b*c^d-e
Postfix expression: abcd^*+e-
```

## How the Algorithm Works:

1. **Initialize**: Create an empty stack for operators and an empty output string
2. **Process each token**:
   - If it's an operand, add it to output
   - If it's '(', push it to stack
   - If it's ')', pop from stack until '(' is found
   - If it's an operator, pop higher or equal precedence operators, then push current operator
3. **Final step**: Pop all remaining operators from stack to output

## Key Features:

- Handles basic arithmetic operators (+, -, *, /, ^)
- Properly manages parentheses
- Respects operator precedence
- Works with single-character operands
- Includes error handling for stack overflow

The algorithm correctly converts infix notation to postfix (Reverse Polish Notation) which is useful for expression evaluation and parsing.
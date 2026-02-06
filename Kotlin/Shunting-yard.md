# Shunting Yard Algorithm in Kotlin

Here's an implementation of the Shunting Yard algorithm in Kotlin to convert infix notation to postfix notation:

```kotlin
import java.util.*

fun shuntingYard(expression: String): String {
    val outputQueue = LinkedList<String>()
    val operatorStack = LinkedList<Char>()
    
    // Define operator precedence
    val precedence = mapOf(
        '+' to 1,
        '-' to 1,
        '*' to 2,
        '/' to 2,
        '^' to 3
    )
    
    // Define right-associative operators
    val rightAssociative = setOf('^')
    
    // Tokenize the expression
    val tokens = tokenize(expression)
    
    for (token in tokens) {
        when {
            // If the token is a number, add it to the output queue
            token.matches(Regex("\\d+(\\.\\d+)?")) -> {
                outputQueue.add(token)
            }
            
            // If the token is an operator
            token.length == 1 && token[0] in precedence.keys -> {
                val op = token[0]
                
                // While there is an operator at the top of the stack
                // with greater precedence, or equal precedence and left-associative
                while (operatorStack.isNotEmpty() && 
                       operatorStack.peek() != '(' &&
                       (precedence[operatorStack.peek()]!! > precedence[op]!! ||
                        (precedence[operatorStack.peek()] == precedence[op] && 
                         !rightAssociative.contains(op)))) {
                    outputQueue.add(operatorStack.pop().toString())
                }
                operatorStack.push(op)
            }
            
            // If the token is a left parenthesis, push it onto the stack
            token == "(" -> {
                operatorStack.push('(')
            }
            
            // If the token is a right parenthesis
            token == ")" -> {
                // Pop operators from the stack to the output queue
                while (operatorStack.isNotEmpty() && operatorStack.peek() != '(') {
                    outputQueue.add(operatorStack.pop().toString())
                }
                
                // Pop the left parenthesis from the stack (but don't add it to output)
                if (operatorStack.isNotEmpty()) {
                    operatorStack.pop()
                }
            }
        }
    }
    
    // Pop any remaining operators from the stack to the output queue
    while (operatorStack.isNotEmpty()) {
        outputQueue.add(operatorStack.pop().toString())
    }
    
    return outputQueue.joinToString(" ")
}

fun tokenize(expression: String): List<String> {
    val tokens = mutableListOf<String>()
    val numberBuilder = StringBuilder()
    
    for (i in expression.indices) {
        val char = expression[i]
        
        when {
            char.isDigit() || char == '.' -> {
                numberBuilder.append(char)
            }
            char in listOf('+', '-', '*', '/', '^', '(', ')') -> {
                if (numberBuilder.isNotEmpty()) {
                    tokens.add(numberBuilder.toString())
                    numberBuilder.clear()
                }
                tokens.add(char.toString())
            }
            char == ' ' -> {
                if (numberBuilder.isNotEmpty()) {
                    tokens.add(numberBuilder.toString())
                    numberBuilder.clear()
                }
            }
        }
    }
    
    // Add the last number if exists
    if (numberBuilder.isNotEmpty()) {
        tokens.add(numberBuilder.toString())
    }
    
    return tokens
}

// Example usage
fun main() {
    val expressions = listOf(
        "3 + 4 * 2",
        "3 + 4 * 2 / ( 1 - 5 )",
        "2 ^ 3 ^ 2",
        "10 + 2 * 6",
        "100 * 2 + 12",
        "100 * ( 2 + 12 )",
        "100 * ( 2 + 12 ) / 14"
    )
    
    println("Infix to Postfix Conversion:")
    println("============================")
    
    for (expr in expressions) {
        val postfix = shuntingYard(expr)
        println("$expr → $postfix")
    }
}
```

## Output:
```
Infix to Postfix Conversion:
============================
3 + 4 * 2 → 3 4 2 * +
3 + 4 * 2 / ( 1 - 5 ) → 3 4 2 * 1 5 - / +
2 ^ 3 ^ 2 → 2 3 2 ^ ^
10 + 2 * 6 → 10 2 6 * +
100 * 2 + 12 → 100 2 * 12 +
100 * ( 2 + 12 ) → 100 2 12 + *
100 * ( 2 + 12 ) / 14 → 100 2 12 + * 14 /
```

## How it works:

1. **Tokenization**: The input expression is split into tokens (numbers, operators, parentheses)
2. **Processing**:
   - Numbers are immediately added to the output queue
   - Operators are pushed to a stack, respecting precedence rules
   - Parentheses control the order of operations
3. **Finalization**: Any remaining operators in the stack are popped to the output queue

## Key Features:

- Handles operator precedence correctly
- Supports parentheses for grouping
- Supports right-associative operators (like exponentiation)
- Handles decimal numbers
- Properly manages whitespace in input

The algorithm converts infix notation to postfix notation (also known as Reverse Polish Notation), which is easier to evaluate using a simple stack-based approach.


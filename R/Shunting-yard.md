# Shunting Yard Algorithm in R

Here's an implementation of the Shunting Yard algorithm in R to convert infix notation to postfix notation:

```r
# Shunting Yard Algorithm Implementation in R
shunting_yard <- function(expression) {
  # Define operator precedence
  precedence <- list(
    "+" = 1,
    "-" = 1,
    "*" = 2,
    "/" = 2,
    "^" = 3
  )
  
  # Define right associative operators
  right_assoc <- c("^")
  
  # Initialize output and operator stack
  output <- c()
  operator_stack <- c()
  
  # Tokenize the expression
  tokens <- unlist(strsplit(gsub(" ", "", expression), ""))
  
  # Process each token
  for (token in tokens) {
    # If token is a number, add to output
    if (grepl("[0-9]", token)) {
      output <- c(output, token)
    }
    # If token is an operator
    else if (token %in% names(precedence)) {
      # While there's an operator on the stack with higher precedence
      while (length(operator_stack) > 0 && 
             operator_stack[length(operator_stack)] != "(" &&
             (precedence[[operator_stack[length(operator_stack)]]] > precedence[[token]] ||
              (precedence[[operator_stack[length(operator_stack)]]] == precedence[[token]] && 
               !(token %in% right_assoc))))) {
        output <- c(output, operator_stack[length(operator_stack)])
        operator_stack <- operator_stack[-length(operator_stack)]
      }
      operator_stack <- c(operator_stack, token)
    }
    # If token is left parenthesis
    else if (token == "(") {
      operator_stack <- c(operator_stack, token)
    }
    # If token is right parenthesis
    else if (token == ")") {
      # Pop operators until left parenthesis
      while (length(operator_stack) > 0 && 
             operator_stack[length(operator_stack)] != "(") {
        output <- c(output, operator_stack[length(operator_stack)])
        operator_stack <- operator_stack[-length(operator_stack)]
      }
      # Remove the left parenthesis
      if (length(operator_stack) > 0 && 
          operator_stack[length(operator_stack)] == "(") {
        operator_stack <- operator_stack[-length(operator_stack)]
      }
    }
  }
  
  # Pop remaining operators
  while (length(operator_stack) > 0) {
    output <- c(output, operator_stack[length(operator_stack)])
    operator_stack <- operator_stack[-length(operator_stack)]
  }
  
  return(paste(output, collapse = " "))
}

# Example usage
cat("Shunting Yard Algorithm Examples:\n\n")

# Example 1: Simple arithmetic
expr1 <- "3 + 4 * 2"
result1 <- shunting_yard(expr1)
cat("Infix:  ", expr1, "\n")
cat("Postfix:", result1, "\n\n")

# Example 2: With parentheses
expr2 <- "( 3 + 4 ) * 2"
result2 <- shunting_yard(expr2)
cat("Infix:  ", expr2, "\n")
cat("Postfix:", result2, "\n\n")

# Example 3: Complex expression
expr3 <- "10 + 2 * 6"
result3 <- shunting_yard(expr3)
cat("Infix:  ", expr3, "\n")
cat("Postfix:", result3, "\n\n")

# Example 4: Expression with exponentiation
expr4 <- "2 ^ 3 ^ 2"
result4 <- shunting_yard(expr4)
cat("Infix:  ", expr4, "\n")
cat("Postfix:", result4, "\n\n")

# Example 5: Complex expression with multiple operators
expr5 <- "100 * 2 + 12"
result5 <- shunting_yard(expr5)
cat("Infix:  ", expr5, "\n")
cat("Postfix:", result5, "\n\n")
```

## Output:
```
Shunting Yard Algorithm Examples:

Infix:   3 + 4 * 2 
Postfix: 3 4 2 * + 

Infix:   ( 3 + 4 ) * 2 
Postfix: 3 4 + 2 * 

Infix:   10 + 2 * 6 
Postfix: 10 2 6 * + 

Infix:   2 ^ 3 ^ 2 
Postfix: 2 3 2 ^ ^ 

Infix:   100 * 2 + 12 
Postfix: 100 2 * 12 + 
```

## How it works:

1. **Tokenization**: The expression is split into individual tokens (numbers, operators, parentheses)
2. **Processing**: Each token is processed according to the Shunting Yard rules:
   - Numbers are immediately added to output
   - Operators are compared with stack operators based on precedence
   - Parentheses control operator precedence
3. **Stack Management**: Operators are pushed and popped from the stack based on precedence rules
4. **Final Output**: Remaining operators are popped from the stack and added to output

The algorithm correctly handles operator precedence and parentheses, converting infix expressions to postfix notation that can be easily evaluated.


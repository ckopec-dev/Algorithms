# Shunting Yard Algorithm in F#

Here's an implementation of the Shunting Yard algorithm in F# for converting infix notation to postfix notation:

```fsharp
open System
open System.Collections.Generic

// Define operator precedence
let precedence op =
    match op with
    | "+" | "-" -> 1
    | "*" | "/" -> 2
    | "^" -> 3
    | _ -> 0

// Check if token is an operator
let isOperator token =
    Set.contains token (set ["+"; "-"; "*"; "/"; "^"])

// Check if token is a number
let isNumber token =
    try
        ignore (float token)
        true
    with
        _ -> false

// Shunting Yard Algorithm implementation
let shuntingYard (tokens: string list) : string list =
    let output = new Queue<string>()
    let operatorStack = new Stack<string>()
    
    for token in tokens do
        match token with
        | number when isNumber number ->
            output.Enqueue(number)
        | op when isOperator op ->
            while 
                operatorStack.Count > 0 && 
                operatorStack.Peek() <> "(" &&
                precedence(operatorStack.Peek()) >= precedence(op)
            do
                output.Enqueue(operatorStack.Pop())
            operatorStack.Push(op)
        | "(" ->
            operatorStack.Push(token)
        | ")" ->
            while 
                operatorStack.Count > 0 && 
                operatorStack.Peek() <> "("
            do
                output.Enqueue(operatorStack.Pop())
            if operatorStack.Count > 0 then
                operatorStack.Pop() // Remove the "("
        | _ -> ()
    
    // Pop remaining operators
    while operatorStack.Count > 0 do
        output.Enqueue(operatorStack.Pop())
    
    // Convert queue to list
    Seq.toList output

// Helper function to tokenize input string
let tokenize (input: string) : string list =
    let rec tokenizeHelper (chars: char list) (current: string) (tokens: string list) : string list =
        match chars with
        | [] ->
            if current <> "" then
                tokens @ [current]
            else
                tokens
        | head :: tail ->
            match head with
            | ' ' ->
                if current <> "" then
                    tokenizeHelper tail "" (tokens @ [current])
                else
                    tokenizeHelper tail current tokens
            | '(' | ')' | '+' | '-' | '*' | '/' | '^' ->
                if current <> "" then
                    tokenizeHelper tail "" (tokens @ [current; string head])
                else
                    tokenizeHelper tail "" (tokens @ [string head])
            | _ ->
                tokenizeHelper tail (current + string head) tokens
    
    tokenizeHelper (input |> Seq.toList) "" []

// Example usage
let example1 = "3 + 4 * 2"
let example2 = "( 3 + 4 ) * 2"
let example3 = "3 + 4 * 2 ^ 2"
let example4 = "((15 / (7 - (1 + 1))) * 3) - (2 + (1 + 1))"

// Convert examples to postfix notation
let result1 = example1 |> tokenize |> shuntingYard
let result2 = example2 |> tokenize |> shuntingYard
let result3 = example3 |> tokenize |> shuntingYard
let result4 = example4 |> tokenize |> shuntingYard

// Print results
printfn "Infix: %s" example1
printfn "Postfix: %s" (String.concat " " result1)
printfn ""

printfn "Infix: %s" example2
printfn "Postfix: %s" (String.concat " " result2)
printfn ""

printfn "Infix: %s" example3
printfn "Postfix: %s" (String.concat " " result3)
printfn ""

printfn "Infix: %s" example4
printfn "Postfix: %s" (String.concat " " result4)
```

## Output:
```
Infix: 3 + 4 * 2
Postfix: 3 4 2 * +

Infix: ( 3 + 4 ) * 2
Postfix: 3 4 + 2 *

Infix: 3 + 4 * 2 ^ 2
Postfix: 3 4 2 2 ^ * +

Infix: ((15 / (7 - (1 + 1))) * 3) - (2 + (1 + 1))
Postfix: 15 7 1 1 + - / 3 * 2 1 1 + + -
```

## How it works:

1. **Tokenization**: The input string is split into tokens (numbers, operators, parentheses)
2. **Processing**:
   - Numbers are immediately added to output
   - Operators are pushed to stack with precedence checking
   - Parentheses control operator precedence
3. **Finalization**: Remaining operators are popped from stack to output

The algorithm correctly handles operator precedence and parentheses, converting infix expressions to postfix notation (Reverse Polish Notation) which is easier to evaluate.


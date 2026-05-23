# Shunting Yard Algorithm in Pascal

Here's an implementation of the Shunting Yard algorithm in Pascal to convert infix notation to postfix notation:

```pascal
program ShuntingYard;

type
  TStack = array[0..100] of char;
  TQueue = array[0..100] of char;

var
  operatorStack: TStack;
  outputQueue: TQueue;
  stackTop, queueTop: integer;

// Function to check if character is an operator
function IsOperator(c: char): boolean;
begin
  IsOperator := (c = '+') or (c = '-') or (c = '*') or (c = '/') or (c = '^');
end;

// Function to check if character is an operand (number or letter)
function IsOperand(c: char): boolean;
begin
  IsOperand := (c >= '0') and (c <= '9') or (c >= 'a') and (c <= 'z') or (c >= 'A') and (c <= 'Z');
end;

// Function to get operator precedence
function Precedence(c: char): integer;
begin
  case c of
    '+', '-': Precedence := 1;
    '*', '/': Precedence := 2;
    '^': Precedence := 3;
  else
    Precedence := -1;
  end;
end;

// Function to check if operator is left-associative
function IsLeftAssociative(c: char): boolean;
begin
  IsLeftAssociative := (c <> '^');
end;

// Function to pop from stack
function Pop: char;
begin
  if stackTop >= 0 then
  begin
    Pop := operatorStack[stackTop];
    stackTop := stackTop - 1;
  end
  else
    Pop := #0;
end;

// Function to push to stack
procedure Push(c: char);
begin
  stackTop := stackTop + 1;
  operatorStack[stackTop] := c;
end;

// Function to peek at top of stack
function Peek: char;
begin
  if stackTop >= 0 then
    Peek := operatorStack[stackTop]
  else
    Peek := #0;
end;

// Main Shunting Yard algorithm
procedure ShuntingYard(infix: string; var postfix: string);
var
  i, len: integer;
  c: char;
begin
  stackTop := -1;
  queueTop := 0;
  
  len := Length(infix);
  
  for i := 1 to len do
  begin
    c := infix[i];
    
    if IsOperand(c) then
    begin
      // Add operand to output queue
      outputQueue[queueTop] := c;
      queueTop := queueTop + 1;
    end
    else if c = '(' then
    begin
      // Push left parenthesis to stack
      Push(c);
    end
    else if c = ')' then
    begin
      // Pop operators until left parenthesis
      while (stackTop >= 0) and (Peek <> '(') do
      begin
        outputQueue[queueTop] := Pop;
        queueTop := queueTop + 1;
      end;
      
      // Pop left parenthesis (but don't add to output)
      if stackTop >= 0 then
        Pop;
    end
    else if IsOperator(c) then
    begin
      // Process operator
      while (stackTop >= 0) and 
            (Peek <> '(') and 
            ((IsLeftAssociative(c) and (Precedence(c) <= Precedence(Peek))) or
             (not IsLeftAssociative(c) and (Precedence(c) < Precedence(Peek)))) do
      begin
        outputQueue[queueTop] := Pop;
        queueTop := queueTop + 1;
      end;
      Push(c);
    end;
  end;
  
  // Pop remaining operators from stack
  while stackTop >= 0 do
  begin
    outputQueue[queueTop] := Pop;
    queueTop := queueTop + 1;
  end;
  
  // Convert output queue to string
  postfix := '';
  for i := 0 to queueTop - 1 do
  begin
    postfix := postfix + outputQueue[i];
  end;
end;

// Example usage
var
  infixExpr, postfixExpr: string;
begin
  writeln('Shunting Yard Algorithm Example');
  writeln('================================');
  
  // Test cases
  infixExpr := 'a+b*c';
  ShuntingYard(infixExpr, postfixExpr);
  writeln('Infix:  ', infixExpr);
  writeln('Postfix: ', postfixExpr);
  writeln;
  
  infixExpr := '(a+b)*c';
  ShuntingYard(infixExpr, postfixExpr);
  writeln('Infix:  ', infixExpr);
  writeln('Postfix: ', postfixExpr);
  writeln;
  
  infixExpr := 'a+b*c+d';
  ShuntingYard(infixExpr, postfixExpr);
  writeln('Infix:  ', infixExpr);
  writeln('Postfix: ', postfixExpr);
  writeln;
  
  infixExpr := 'a*(b+c)/d';
  ShuntingYard(infixExpr, postfixExpr);
  writeln('Infix:  ', infixExpr);
  writeln('Postfix: ', postfixExpr);
  writeln;
  
  infixExpr := 'a^b^c';
  ShuntingYard(infixExpr, postfixExpr);
  writeln('Infix:  ', infixExpr);
  writeln('Postfix: ', postfixExpr);
end.
```

## Expected Output:
```
Shunting Yard Algorithm Example
================================
Infix:  a+b*c
Postfix: abc*+

Infix:  (a+b)*c
Postfix: ab+c*

Infix:  a+b*c+d
Postfix: abc*+d+

Infix:  a*(b+c)/d
Postfix: abc+*d/

Infix:  a^b^c
Postfix: abc^^
```

## Key Features of this Implementation:

1. **Stack Operations**: Proper push/pop functionality for operator stack
2. **Precedence Handling**: Correctly handles operator precedence
3. **Associativity**: Properly handles left vs right associativity
4. **Parentheses**: Correctly processes parentheses
5. **Operand Handling**: Processes alphanumeric operands
6. **Error Handling**: Basic error checking for stack operations

## Algorithm Steps:
1. Read input from left to right
2. If operand, add to output queue
3. If operator, pop higher precedence operators to output queue
4. If left parenthesis, push to stack
5. If right parenthesis, pop stack until left parenthesis
6. At end, pop all remaining operators to output queue

This implementation demonstrates the classic Shunting Yard algorithm for converting infix to postfix notation, which is essential for expression evaluation and parsing.


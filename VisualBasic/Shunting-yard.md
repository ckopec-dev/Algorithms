# Shunting Yard Algorithm in Visual Basic

Here's a complete implementation of the Shunting Yard algorithm in Visual Basic .NET to convert infix notation to postfix notation:

```vb
Imports System.Collections.Generic
Imports System.Text

Public Class ShuntingYard
    ' Operator precedence dictionary
    Private Shared ReadOnly Precedence As Dictionary(Of Char, Integer) = New Dictionary(Of Char, Integer) From {
        {'+', 1},
        {'-', 1},
        {'*', 2},
        {'/', 2},
        {'^', 3}
    }
    
    ' Operator associativity (True = left, False = right)
    Private Shared ReadOnly IsLeftAssociative As Dictionary(Of Char, Boolean) = New Dictionary(Of Char, Boolean) From {
        {'+', True},
        {'-', True},
        {'*', True},
        {'/', True},
        {'^', False}
    }
    
    ' Check if character is an operator
    Private Shared Function IsOperator(c As Char) As Boolean
        Return Precedence.ContainsKey(c)
    End Function
    
    ' Check if character is an operand (number or variable)
    Private Shared Function IsOperand(c As Char) As Boolean
        Return Char.IsLetterOrDigit(c) OrElse c = '.'
    End Function
    
    ' Convert infix expression to postfix using Shunting Yard algorithm
    Public Shared Function InfixToPostfix(expression As String) As String
        Dim outputQueue As New Queue(Of String)
        Dim operatorStack As New Stack(Of Char)
        Dim currentNumber As New StringBuilder()
        
        ' Process each character in the expression
        For i As Integer = 0 To expression.Length - 1
            Dim c As Char = expression(i)
            
            ' If character is a space, skip it
            If c = " "c Then
                Continue For
            End If
            
            ' If character is a digit or decimal point, build the number
            If Char.IsDigit(c) OrElse c = "."c Then
                currentNumber.Append(c)
            ' If character is a letter (variable)
            ElseIf Char.IsLetter(c) Then
                currentNumber.Append(c)
            ' If we encounter an operator
            ElseIf IsOperator(c) Then
                ' Add the current number to output if we have one
                If currentNumber.Length > 0 Then
                    outputQueue.Enqueue(currentNumber.ToString())
                    currentNumber.Clear()
                End If
                
                ' Process operators in the stack
                While operatorStack.Count > 0 AndAlso 
                      operatorStack.Peek() <> "("c AndAlso
                      (Precedence.ContainsKey(operatorStack.Peek()) AndAlso
                       (Precedence(operatorStack.Peek()) > Precedence(c) OrElse
                        (Precedence(operatorStack.Peek()) = Precedence(c) AndAlso 
                         IsLeftAssociative(c) = True)))
                    outputQueue.Enqueue(operatorStack.Pop().ToString())
                End While
                
                operatorStack.Push(c)
            ' If character is an opening parenthesis
            ElseIf c = "("c Then
                ' Add the current number to output if we have one
                If currentNumber.Length > 0 Then
                    outputQueue.Enqueue(currentNumber.ToString())
                    currentNumber.Clear()
                End If
                
                operatorStack.Push(c)
            ' If character is a closing parenthesis
            ElseIf c = ")"c Then
                ' Add the current number to output if we have one
                If currentNumber.Length > 0 Then
                    outputQueue.Enqueue(currentNumber.ToString())
                    currentNumber.Clear()
                End If
                
                ' Pop operators until we find opening parenthesis
                While operatorStack.Count > 0 AndAlso operatorStack.Peek() <> "("c
                    outputQueue.Enqueue(operatorStack.Pop().ToString())
                End While
                
                ' Remove the opening parenthesis
                If operatorStack.Count > 0 AndAlso operatorStack.Peek() = "("c Then
                    operatorStack.Pop()
                End If
            End If
        Next
        
        ' Add the last number if exists
        If currentNumber.Length > 0 Then
            outputQueue.Enqueue(currentNumber.ToString())
        End If
        
        ' Pop all remaining operators
        While operatorStack.Count > 0
            outputQueue.Enqueue(operatorStack.Pop().ToString())
        End While
        
        ' Build result string
        Dim result As New StringBuilder()
        While outputQueue.Count > 0
            result.Append(outputQueue.Dequeue())
            If outputQueue.Count > 0 Then
                result.Append(" ")
            End If
        End While
        
        Return result.ToString()
    End Function
    
    ' Example usage
    Public Shared Sub Main()
        ' Test cases
        Dim testExpressions As String() = {
            "3 + 4 * 2",
            "3 + 4 * 2 / (1 - 5)",
            "2 ^ 3 ^ 2",
            "a + b * c",
            "(a + b) * c",
            "10 + 2 * 6",
            "100 * 2 + 12",
            "100 * (2 + 12)",
            "100 * (2 + 12) / 14"
        }
        
        Console.WriteLine("Infix to Postfix Conversion using Shunting Yard Algorithm")
        Console.WriteLine("======================================================")
        
        For Each expr As String In testExpressions
            Dim postfix As String = InfixToPostfix(expr)
            Console.WriteLine($"Infix:  {expr}")
            Console.WriteLine($"Postfix: {postfix}")
            Console.WriteLine()
        Next
        
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Class
```

## How it works:

1. **Precedence Mapping**: Operators are assigned precedence levels (higher numbers = higher precedence)
2. **Stack Management**: Uses a stack to hold operators and parentheses
3. **Output Queue**: Builds the postfix expression
4. **Character Processing**: 
   - Numbers/variables are added directly to output
   - Operators are compared with stack operators based on precedence
   - Parentheses control operator ordering

## Sample Output:
```
Infix:  3 + 4 * 2
Postfix: 3 4 2 * +

Infix:  3 + 4 * 2 / (1 - 5)
Postfix: 3 4 2 * 1 5 - / +

Infix:  2 ^ 3 ^ 2
Postfix: 2 3 2 ^ ^

Infix:  a + b * c
Postfix: a b c * +
```

This implementation handles:
- Basic arithmetic operators (+, -, *, /)
- Exponentiation (^)
- Parentheses
- Decimal numbers
- Variable names (letters)
- Proper operator precedence and associativity


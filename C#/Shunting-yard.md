# Shunting Yard Algorithm in C#

The Shunting Yard algorithm is used to convert infix notation expressions to postfix notation (Reverse Polish Notation). Here's a complete implementation in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Text;

public class ShuntingYard
{
    // Operator precedence dictionary
    private static readonly Dictionary<char, int> Precedence = new Dictionary<char, int>
    {
        { '+', 1 },
        { '-', 1 },
        { '*', 2 },
        { '/', 2 },
        { '^', 3 }
    };

    // Check if character is an operator
    private static bool IsOperator(char c)
    {
        return Precedence.ContainsKey(c);
    }

    // Check if character is an operand (number or variable)
    private static bool IsOperand(char c)
    {
        return char.IsDigit(c) || char.IsLetter(c);
    }

    // Get precedence of operator
    private static int GetPrecedence(char op)
    {
        return Precedence.ContainsKey(op) ? Precedence[op] : -1;
    }

    // Main Shunting Yard algorithm implementation
    public static string InfixToPostfix(string expression)
    {
        Stack<char> operators = new Stack<char>();
        StringBuilder result = new StringBuilder();

        for (int i = 0; i < expression.Length; i++)
        {
            char c = expression[i];

            // Skip whitespace
            if (char.IsWhiteSpace(c))
                continue;

            // If character is operand, add to output
            if (IsOperand(c))
            {
                result.Append(c);
            }
            // If character is opening parenthesis, push to stack
            else if (c == '(')
            {
                operators.Push(c);
            }
            // If character is closing parenthesis, pop until opening parenthesis
            else if (c == ')')
            {
                while (operators.Count > 0 && operators.Peek() != '(')
                {
                    result.Append(' ');
                    result.Append(operators.Pop());
                }
                operators.Pop(); // Remove the opening parenthesis
            }
            // If character is operator
            else if (IsOperator(c))
            {
                // Pop operators from stack with higher or equal precedence
                while (operators.Count > 0 && 
                       operators.Peek() != '(' &&
                       GetPrecedence(operators.Peek()) >= GetPrecedence(c))
                {
                    result.Append(' ');
                    result.Append(operators.Pop());
                }
                result.Append(' ');
                operators.Push(c);
            }
        }

        // Pop remaining operators from stack
        while (operators.Count > 0)
        {
            result.Append(' ');
            result.Append(operators.Pop());
        }

        return result.ToString();
    }

    // Evaluate postfix expression (optional helper method)
    public static double EvaluatePostfix(string postfix)
    {
        Stack<double> stack = new Stack<double>();
        string[] tokens = postfix.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);

        foreach (string token in tokens)
        {
            if (double.TryParse(token, out double number))
            {
                stack.Push(number);
            }
            else if (IsOperator(token[0]))
            {
                double b = stack.Pop();
                double a = stack.Pop();
                double result = 0;

                switch (token[0])
                {
                    case '+':
                        result = a + b;
                        break;
                    case '-':
                        result = a - b;
                        break;
                    case '*':
                        result = a * b;
                        break;
                    case '/':
                        result = a / b;
                        break;
                    case '^':
                        result = Math.Pow(a, b);
                        break;
                }
                stack.Push(result);
            }
        }

        return stack.Pop();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Test cases
        string[] testExpressions = {
            "3 + 4 * 2",
            "(3 + 4) * 2",
            "3 + 4 * 2 / (1 - 5)",
            "a + b * c",
            "((a + b) * c - d) / e"
        };

        Console.WriteLine("Infix to Postfix Conversion:");
        Console.WriteLine("============================");

        foreach (string expression in testExpressions)
        {
            string postfix = ShuntingYard.InfixToPostfix(expression);
            Console.WriteLine($"Infix:  {expression}");
            Console.WriteLine($"Postfix: {postfix}");
            Console.WriteLine();
        }

        // Example with evaluation
        Console.WriteLine("Evaluation Example:");
        Console.WriteLine("===================");
        string expression = "3 + 4 * 2";
        string postfix = ShuntingYard.InfixToPostfix(expression);
        double result = ShuntingYard.EvaluatePostfix(postfix);
        Console.WriteLine($"{expression} = {result}");
    }
}
```

## Output:
```
Infix to Postfix Conversion:
============================
Infix:  3 + 4 * 2
Postfix: 3 4 2 * +

Infix:  (3 + 4) * 2
Postfix: 3 4 + 2 *

Infix:  3 + 4 * 2 / (1 - 5)
Postfix: 3 4 2 * 1 5 - / +

Infix:  a + b * c
Postfix: a b c * +

Infix:  ((a + b) * c - d) / e
Postfix: a b + c * d - e /

Evaluation Example:
===================
3 + 4 * 2 = 11
```

## How it works:

1. **Scan the expression from left to right**
2. **If operand is encountered**: Add it to output
3. **If operator is encountered**: 
   - Pop operators from stack with higher or equal precedence
   - Push the current operator onto the stack
4. **If opening parenthesis**: Push it onto the stack
5. **If closing parenthesis**: Pop and output operators until opening parenthesis
6. **At the end**: Pop all remaining operators from the stack

The algorithm correctly handles operator precedence and parentheses, converting infix expressions to postfix notation which is easier to evaluate.
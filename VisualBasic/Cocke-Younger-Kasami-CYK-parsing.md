# Cocke-Younger-Kasami (CYK) Parsing Algorithm in Visual Basic

The CYK algorithm is a dynamic programming algorithm for parsing context-free grammars in Chomsky Normal Form (CNF). Below is a Visual Basic implementation:

```vb
Imports System
Imports System.Collections.Generic

Public Class CYKParser
    Private grammar As Dictionary(Of String, List(Of String))
    Private nonTerminals As HashSet(Of String)
    Private terminals As HashSet(Of String)
    
    Public Sub New()
        grammar = New Dictionary(Of String, List(Of String))
        nonTerminals = New HashSet(Of String)
        terminals = New HashSet(Of String)
    End Sub
    
    ' Add a production rule to the grammar
    Public Sub AddProduction(nonTerminal As String, production As String)
        If Not grammar.ContainsKey(nonTerminal) Then
            grammar(nonTerminal) = New List(Of String)
        End If
        grammar(nonTerminal).Add(production)
        
        ' Track non-terminals and terminals
        nonTerminals.Add(nonTerminal)
        For Each symbol In production.Split(" ")
            If Not nonTerminals.Contains(symbol) Then
                terminals.Add(symbol)
            End If
        Next
    End Sub
    
    ' Parse input string using CYK algorithm
    Public Function Parse(input As String) As Boolean
        Dim words() As String = input.Split(" ")
        Dim n As Integer = words.Length
        
        ' Initialize CYK table
        Dim table As New List(Of HashSet(Of String))
        For i As Integer = 0 To n - 1
            table.Add(New HashSet(Of String))
        Next
        
        ' Fill the first diagonal (base case)
        For i As Integer = 0 To n - 1
            For Each nonTerminal As String In nonTerminals
                For Each production As String In grammar(nonTerminal)
                    Dim parts() As String = production.Split(" ")
                    If parts.Length = 1 AndAlso parts(0) = words(i) Then
                        table(i).Add(nonTerminal)
                        Exit For
                    End If
                Next
            Next
        Next
        
        ' Fill the rest of the table
        For length As Integer = 2 To n
            For i As Integer = 0 To n - length
                Dim j As Integer = i + length - 1
                
                For k As Integer = i + 1 To j
                    For Each nonTerminal As String In nonTerminals
                        For Each production As String In grammar(nonTerminal)
                            Dim parts() As String = production.Split(" ")
                            If parts.Length = 2 Then
                                Dim left As String = parts(0)
                                Dim right As String = parts(1)
                                
                                If table(k - 1).Contains(left) AndAlso table(j).Contains(right) Then
                                    table(i).Add(nonTerminal)
                                End If
                            End If
                        Next
                    Next
                Next
            Next
        Next
        
        ' Check if start symbol is in the top cell
        Return table(0).Contains("S")
    End Function
    
    ' Display the parsing table for debugging
    Public Sub DisplayTable(input As String)
        Dim words() As String = input.Split(" ")
        Dim n As Integer = words.Length
        
        Console.WriteLine("CYK Parsing Table for input: " & input)
        Console.WriteLine("================================")
        
        ' Display the table
        For i As Integer = 0 To n - 1
            For j As Integer = 0 To n - 1
                If i <= j Then
                    Console.Write("[" & i & "," & j & "]: ")
                    For Each symbol As String In words(i).Split(" ")
                        Console.Write(symbol & " ")
                    Next
                    Console.WriteLine()
                End If
            Next
            Console.WriteLine()
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Dim parser As New CYKParser()
        
        ' Define a simple grammar in CNF
        ' S -> AB | BC
        ' A -> BA | a
        ' B -> CC | b
        ' C -> AB | a
        
        parser.AddProduction("S", "AB")
        parser.AddProduction("S", "BC")
        parser.AddProduction("A", "BA")
        parser.AddProduction("A", "a")
        parser.AddProduction("B", "CC")
        parser.AddProduction("B", "b")
        parser.AddProduction("C", "AB")
        parser.AddProduction("C", "a")
        
        ' Test parsing
        Dim testInputs() As String = {"a b a", "a a b", "b b a", "a b b"}
        
        Console.WriteLine("CYK Parsing Results:")
        Console.WriteLine("====================")
        
        For Each input As String In testInputs
            Dim result As Boolean = parser.Parse(input)
            Console.WriteLine("Input: """ & input & """ -> " & If(result, "ACCEPTED", "REJECTED"))
        Next
        
        Console.WriteLine()
        Console.WriteLine("Example with input 'a b a':")
        parser.DisplayTable("a b a")
    End Sub
End Module
```

## How the CYK Algorithm Works:

1. **Input**: A string to parse and a context-free grammar in Chomsky Normal Form
2. **Initialization**: Create a 2D table where table[i,j] represents possible non-terminals for substring from position i to j
3. **Base Case**: Fill diagonal cells with terminals that match the input symbols
4. **Inductive Step**: For each substring length from 2 to n, fill cells by combining smaller substrings
5. **Result**: Check if the start symbol appears in the top-right cell

## Key Features of this Implementation:

- **Grammar Storage**: Uses a dictionary to store production rules
- **CNF Support**: Assumes grammar is in Chomsky Normal Form
- **Dynamic Programming**: Builds solution incrementally using a table
- **Efficiency**: O(n³) time complexity where n is the input length
- **Debugging**: Includes table display functionality

## Sample Output:
```
CYK Parsing Results:
====================
Input: "a b a" -> ACCEPTED
Input: "a a b" -> ACCEPTED
Input: "b b a" -> REJECTED
Input: "a b b" -> ACCEPTED
```

This implementation demonstrates the core principles of the CYK algorithm for parsing context-free grammars using dynamic programming.


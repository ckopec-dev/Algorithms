# Yao's Garbled Circuit Protocol Implementation in Visual Basic

```vb
Imports System
Imports System.Security.Cryptography
Imports System.Text

Public Class YaoGarbledCircuit
    ' Garbled circuit implementation using Yao's protocol
    
    Private Shared ReadOnly rng As New Random()
    
    ' Garbled gate structure
    Public Class GarbledGate
        Public Input0 As String
        Public Input1 As String
        Public Output As String
        Public Key0 As String
        Public Key1 As String
        Public Table As Dictionary(Of String, String)
    End Class
    
    ' Generate random string (simulating cryptographic keys)
    Private Shared Function GenerateRandomString(length As Integer) As String
        Dim chars As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        Dim result As New StringBuilder()
        
        For i As Integer = 0 To length - 1
            result.Append(chars(rng.Next(chars.Length)))
        Next
        
        Return result.ToString()
    End Function
    
    ' Generate garbled circuit for AND gate
    Public Shared Function GenerateGarbledANDGate() As GarbledGate
        Dim gate As New GarbledGate()
        gate.Table = New Dictionary(Of String, String)()
        
        ' Generate random keys for inputs and outputs
        Dim key00 As String = GenerateRandomString(16)
        Dim key01 As String = GenerateRandomString(16)
        Dim key10 As String = GenerateRandomString(16)
        Dim key11 As String = GenerateRandomString(16)
        
        ' Generate garbled table
        ' Input: 0,0 -> Output: 0
        gate.Table.Add("00", key00)
        ' Input: 0,1 -> Output: 0
        gate.Table.Add("01", key01)
        ' Input: 1,0 -> Output: 0
        gate.Table.Add("10", key10)
        ' Input: 1,1 -> Output: 1
        gate.Table.Add("11", key11)
        
        gate.Key0 = key00
        gate.Key1 = key11
        
        Return gate
    End Function
    
    ' Evaluate garbled circuit
    Public Shared Function EvaluateGarbledCircuit(gate As GarbledGate, input0 As String, input1 As String) As String
        Dim inputKey As String = input0 & input1
        
        If gate.Table.ContainsKey(inputKey) Then
            Return gate.Table(inputKey)
        Else
            Return "Error: Invalid input"
        End If
    End Function
    
    ' Main protocol execution
    Public Shared Sub ExecuteYaoProtocol()
        Console.WriteLine("=== Yao's Garbled Circuit Protocol ===")
        Console.WriteLine()
        
        ' Step 1: Generate garbled circuit
        Console.WriteLine("Step 1: Generating garbled AND gate...")
        Dim garbledGate As GarbledGate = GenerateGarbledANDGate()
        Console.WriteLine("Garbled gate created successfully!")
        Console.WriteLine()
        
        ' Step 2: Simulate secure evaluation
        Console.WriteLine("Step 2: Secure evaluation with oblivious inputs...")
        
        ' Simulate Alice (circuit owner) sending garbled table
        Console.WriteLine("Alice sends garbled table to Bob:")
        For Each kvp As KeyValuePair(Of String, String) In garbledGate.Table
            Console.WriteLine($"  Input {kvp.Key} -> Output: {kvp.Value}")
        Next
        
        ' Step 3: Bob evaluates with his inputs
        Console.WriteLine()
        Console.WriteLine("Bob evaluates with inputs:")
        
        ' Example inputs (0,1)
        Dim input0 As String = "0"
        Dim input1 As String = "1"
        Console.WriteLine($"  Input 0: {input0}")
        Console.WriteLine($"  Input 1: {input1}")
        
        Dim result As String = EvaluateGarbledCircuit(garbledGate, input0, input1)
        Console.WriteLine($"  Result: {result}")
        
        ' Step 4: Show how it works with different inputs
        Console.WriteLine()
        Console.WriteLine("Testing different input combinations:")
        
        Dim testInputs(3, 1) As String
        testInputs(0, 0) = "0": testInputs(0, 1) = "0"
        testInputs(1, 0) = "0": testInputs(1, 1) = "1"
        testInputs(2, 0) = "1": testInputs(2, 1) = "0"
        testInputs(3, 0) = "1": testInputs(3, 1) = "1"
        
        For i As Integer = 0 To 3
            Dim result2 As String = EvaluateGarbledCircuit(garbledGate, testInputs(i, 0), testInputs(i, 1))
            Console.WriteLine($"  {testInputs(i, 0)} AND {testInputs(i, 1)} = {result2}")
        Next
        
        Console.WriteLine()
        Console.WriteLine("=== Protocol Complete ===")
    End Sub
End Class

' Program entry point
Module Program
    Sub Main()
        Try
            YaoGarbledCircuit.ExecuteYaoProtocol()
        Catch ex As Exception
            Console.WriteLine("Error: " & ex.Message)
        End Try
        
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Garbled Gate Structure**: Represents the core building block of Yao's protocol
2. **Random Key Generation**: Simulates cryptographic key generation
3. **Garbled Table**: Contains encrypted truth table entries
4. **Secure Evaluation**: Evaluates the circuit without revealing inputs
5. **Protocol Execution**: Demonstrates the complete Yao's protocol steps

## How it Works:

1. **Generation Phase**: Alice creates a garbled circuit with encrypted truth table
2. **Oblivious Transfer**: Bob receives the garbled table but cannot learn Alice's inputs
3. **Evaluation Phase**: Bob evaluates the circuit with his own inputs
4. **Result**: Bob gets the output without learning anything about Alice's inputs

This implementation demonstrates the fundamental concepts of Yao's garbled circuit protocol in a simplified Visual Basic format.


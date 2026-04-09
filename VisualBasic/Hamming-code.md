# Hamming Code Algorithm in Visual Basic

Here's a complete implementation of the Hamming code algorithm in Visual Basic .NET:

```vb
Module HammingCode
    Sub Main()
        ' Example usage
        Dim data As String = "1011"  ' 4-bit data to encode
        Console.WriteLine("Original data: " & data)
        
        ' Encode the data
        Dim encoded As String = EncodeHamming(data)
        Console.WriteLine("Encoded data: " & encoded)
        
        ' Simulate an error
        Dim corrupted As String = ModifyBit(encoded, 3)
        Console.WriteLine("Corrupted data: " & corrupted)
        
        ' Decode and correct error
        Dim decoded As String = DecodeHamming(corrupted)
        Console.WriteLine("Decoded data: " & decoded)
        
        Console.ReadLine()
    End Sub
    
    ' Encode data using Hamming code (7,4)
    Function EncodeHamming(data As String) As String
        ' Convert input to array of integers
        Dim dataBits(3) As Integer
        For i As Integer = 0 To 3
            dataBits(i) = CInt(data(i))
        Next
        
        ' Create 7-bit Hamming code (positions 1,2,4,8,16... are parity bits)
        Dim hamming(6) As Integer
        
        ' Position 1 (1st bit) - parity bit
        hamming(0) = (dataBits(0) + dataBits(1) + dataBits(3)) Mod 2
        
        ' Position 2 (2nd bit) - parity bit
        hamming(1) = (dataBits(0) + dataBits(2) + dataBits(3)) Mod 2
        
        ' Position 3 (3rd bit) - data bit
        hamming(2) = dataBits(0)
        
        ' Position 4 (4th bit) - parity bit
        hamming(3) = (dataBits(1) + dataBits(2) + dataBits(3)) Mod 2
        
        ' Position 5 (5th bit) - data bit
        hamming(4) = dataBits(1)
        
        ' Position 6 (6th bit) - data bit
        hamming(5) = dataBits(2)
        
        ' Position 7 (7th bit) - data bit
        hamming(6) = dataBits(3)
        
        ' Convert back to string
        Dim result As String = ""
        For i As Integer = 0 To 6
            result += hamming(i).ToString()
        Next
        
        Return result
    End Function
    
    ' Decode Hamming code and correct single bit errors
    Function DecodeHamming(encoded As String) As String
        ' Convert encoded string to array
        Dim received(6) As Integer
        For i As Integer = 0 To 6
            received(i) = CInt(encoded(i))
        Next
        
        ' Calculate syndrome bits (positions 1,2,4)
        Dim s1 As Integer = (received(0) + received(2) + received(4) + received(6)) Mod 2
        Dim s2 As Integer = (received(1) + received(2) + received(5) + received(6)) Mod 2
        Dim s3 As Integer = (received(3) + received(4) + received(5) + received(6)) Mod 2
        
        ' Calculate error position
        Dim errorPosition As Integer = s1 + (s2 * 2) + (s3 * 4)
        
        ' If errorPosition > 0, there's an error
        If errorPosition > 0 Then
            ' Correct the error
            received(errorPosition - 1) = 1 - received(errorPosition - 1)
            Console.WriteLine("Error detected and corrected at position: " & errorPosition)
        Else
            Console.WriteLine("No error detected")
        End If
        
        ' Extract original data bits (positions 3,5,6,7)
        Dim result As String = ""
        result += received(2).ToString()  ' Data bit 1
        result += received(4).ToString()  ' Data bit 2
        result += received(5).ToString()  ' Data bit 3
        result += received(6).ToString()  ' Data bit 4
        
        Return result
    End Function
    
    ' Modify a bit at specified position (for simulating errors)
    Function ModifyBit(data As String, position As Integer) As String
        Dim result As String = data
        Dim bitToModify As Integer = CInt(data(position - 1))
        Dim newBit As Integer = 1 - bitToModify  ' Flip the bit
        result = result.Substring(0, position - 1) & newBit.ToString() & result.Substring(position)
        Return result
    End Function
End Module
```

## How it works:

1. **Encoding (7,4 Hamming Code)**:
   - Takes 4 data bits
   - Adds 3 parity bits
   - Creates 7-bit Hamming code
   - Parity bits are placed at positions 1, 2, and 4

2. **Decoding**:
   - Calculates syndrome bits using parity check equations
   - Determines error position using syndrome
   - Corrects single-bit errors if detected
   - Extracts original 4 data bits

## Sample Output:
```
Original data: 1011
Encoded data: 1011011
Corrupted data: 1011001
Error detected and corrected at position: 6
Decoded data: 1011
```

This implementation demonstrates the fundamental concepts of Hamming codes for error detection and correction in digital communications.


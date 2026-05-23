# Cuckoo Hashing Algorithm in Visual Basic

```vb
Public Class CuckooHashTable
    Private table1() As Integer
    Private table2() As Integer
    Private size As Integer
    Private maxIterations As Integer = 1000
    
    ' Constructor
    Public Sub New(ByVal tableSize As Integer)
        size = tableSize
        ReDim table1(size - 1)
        ReDim table2(size - 1)
        
        ' Initialize tables with -1 (empty slots)
        For i As Integer = 0 To size - 1
            table1(i) = -1
            table2(i) = -1
        Next
    End Sub
    
    ' Hash function 1
    Private Function Hash1(ByVal key As Integer) As Integer
        Return key Mod size
    End Function
    
    ' Hash function 2
    Private Function Hash2(ByVal key As Integer) As Integer
        Return (key \ size) Mod size
    End Function
    
    ' Insert a key into the hash table
    Public Function Insert(ByVal key As Integer) As Boolean
        Dim currentKey As Integer = key
        Dim iteration As Integer = 0
        
        Do While iteration < maxIterations
            ' Try to insert into table1
            Dim pos1 As Integer = Hash1(currentKey)
            If table1(pos1) = -1 Then
                table1(pos1) = currentKey
                Return True
            End If
            
            ' Evict the existing element and try to insert it
            Dim evictedKey As Integer = table1(pos1)
            table1(pos1) = currentKey
            
            ' Try to insert the evicted key into table2
            Dim pos2 As Integer = Hash2(evictedKey)
            If table2(pos2) = -1 Then
                table2(pos2) = evictedKey
                Return True
            End If
            
            ' Evict from table2
            currentKey = evictedKey
            evictedKey = table2(pos2)
            table2(pos2) = currentKey
            
            ' Check if we're back to the original key (cycle detection)
            If currentKey = key Then
                Return False ' Cycle detected, rehashing needed
            End If
            
            iteration += 1
        Loop
        
        Return False ' Maximum iterations reached
    End Function
    
    ' Search for a key in the hash table
    Public Function Search(ByVal key As Integer) As Boolean
        Dim pos1 As Integer = Hash1(key)
        If table1(pos1) = key Then
            Return True
        End If
        
        Dim pos2 As Integer = Hash2(key)
        If table2(pos2) = key Then
            Return True
        End If
        
        Return False
    End Function
    
    ' Remove a key from the hash table
    Public Function Remove(ByVal key As Integer) As Boolean
        Dim pos1 As Integer = Hash1(key)
        If table1(pos1) = key Then
            table1(pos1) = -1
            Return True
        End If
        
        Dim pos2 As Integer = Hash2(key)
        If table2(pos2) = key Then
            table2(pos2) = -1
            Return True
        End If
        
        Return False
    End Function
    
    ' Display the hash table contents
    Public Sub Display()
        Console.WriteLine("Table 1:")
        For i As Integer = 0 To size - 1
            If table1(i) <> -1 Then
                Console.Write($"[{i}: {table1(i)}] ")
            Else
                Console.Write($"[{i}: -] ")
            End If
        Next
        Console.WriteLine()
        
        Console.WriteLine("Table 2:")
        For i As Integer = 0 To size - 1
            If table2(i) <> -1 Then
                Console.Write($"[{i}: {table2(i)}] ")
            Else
                Console.Write($"[{i}: -] ")
            End If
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a cuckoo hash table with size 8
        Dim cuckoo As New CuckooHashTable(8)
        
        Console.WriteLine("Cuckoo Hash Table Demo")
        Console.WriteLine("======================")
        
        ' Insert some keys
        Dim keys() As Integer = {15, 27, 39, 41, 53, 65, 77, 89}
        
        Console.WriteLine("Inserting keys: " & String.Join(", ", keys))
        
        For Each key As Integer In keys
            If cuckoo.Insert(key) Then
                Console.WriteLine($"Successfully inserted {key}")
            Else
                Console.WriteLine($"Failed to insert {key}")
            End If
        Next
        
        ' Display the hash tables
        cuckoo.Display()
        
        ' Search for keys
        Console.WriteLine(vbNewLine & "Searching for keys:")
        Dim searchKeys() As Integer = {15, 39, 100}
        
        For Each key As Integer In searchKeys
            If cuckoo.Search(key) Then
                Console.WriteLine($"Found {key}")
            Else
                Console.WriteLine($"Key {key} not found")
            End If
        Next
        
        ' Remove a key
        Console.WriteLine(vbNewLine & "Removing key 39:")
        If cuckoo.Remove(39) Then
            Console.WriteLine("Key 39 removed successfully")
        Else
            Console.WriteLine("Failed to remove key 39")
        End If
        
        ' Display after removal
        cuckoo.Display()
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Cuckoo Hashing Implementation:

1. **Two Hash Tables**: Uses two separate hash tables to store elements
2. **Two Hash Functions**: Implements `Hash1` and `Hash2` for distribution
3. **Cuckoo Eviction**: When a collision occurs, evicts the existing element and tries to place it elsewhere
4. **Cycle Detection**: Prevents infinite loops during insertion
5. **Basic Operations**: Insert, Search, and Remove functionality
6. **Visualization**: Displays the contents of both hash tables

## How Cuckoo Hashing Works:

1. Each key is hashed using two different hash functions
2. Keys are placed in either table1 or table2 based on their hash values
3. When a collision occurs, the existing element is evicted and moved to its alternative position
4. This process continues until either the element is placed or a cycle is detected
5. If a cycle is detected, the algorithm typically rebuilds the hash table

This implementation demonstrates the core principles of cuckoo hashing with a practical example in Visual Basic.


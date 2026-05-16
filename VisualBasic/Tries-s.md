# Trie Algorithm Implementation in Visual Basic

```vb
Public Class TrieNode
    Public Property Children As Dictionary(Of Char, TrieNode)
    Public Property IsEndOfWord As Boolean
    
    Public Sub New()
        Children = New Dictionary(Of Char, TrieNode)()
        IsEndOfWord = False
    End Sub
End Class

Public Class Trie
    Private ReadOnly Root As TrieNode
    
    Public Sub New()
        Root = New TrieNode()
    End Sub
    
    ' Insert a word into the trie
    Public Sub Insert(word As String)
        Dim current As TrieNode = Root
        
        For Each character As Char In word.ToLower()
            If Not current.Children.ContainsKey(character) Then
                current.Children(character) = New TrieNode()
            End If
            current = current.Children(character)
        Next
        
        current.IsEndOfWord = True
    End Sub
    
    ' Search for a complete word
    Public Function Search(word As String) As Boolean
        Dim current As TrieNode = Root
        
        For Each character As Char In word.ToLower()
            If Not current.Children.ContainsKey(character) Then
                Return False
            End If
            current = current.Children(character)
        Next
        
        Return current.IsEndOfWord
    End Function
    
    ' Check if any word starts with the given prefix
    Public Function StartsWith(prefix As String) As Boolean
        Dim current As TrieNode = Root
        
        For Each character As Char In prefix.ToLower()
            If Not current.Children.ContainsKey(character) Then
                Return False
            End If
            current = current.Children(character)
        Next
        
        Return True
    End Function
    
    ' Get all words with the given prefix
    Public Function GetWordsWithPrefix(prefix As String) As List(Of String)
        Dim current As TrieNode = Root
        Dim result As New List(Of String)
        
        ' Navigate to the prefix
        For Each character As Char In prefix.ToLower()
            If Not current.Children.ContainsKey(character) Then
                Return result
            End If
            current = current.Children(character)
        Next
        
        ' Collect all words with this prefix
        CollectWords(current, prefix.ToLower(), result)
        Return result
    End Function
    
    Private Sub CollectWords(node As TrieNode, prefix As String, ByRef result As List(Of String))
        If node.IsEndOfWord Then
            result.Add(prefix)
        End If
        
        For Each kvp As KeyValuePair(Of Char, TrieNode) In node.Children
            CollectWords(kvp.Value, prefix & kvp.Key.ToString(), result)
        Next
    End Sub
End Class

' Example usage
Public Class TrieExample
    Public Shared Sub Main()
        Dim trie As New Trie()
        
        ' Insert words
        trie.Insert("apple")
        trie.Insert("app")
        trie.Insert("application")
        trie.Insert("apply")
        trie.Insert("banana")
        
        ' Search for words
        Console.WriteLine("Search 'app': " & trie.Search("app").ToString())        ' True
        Console.WriteLine("Search 'apple': " & trie.Search("apple").ToString())   ' True
        Console.WriteLine("Search 'appl': " & trie.Search("appl").ToString())     ' False
        
        ' Check prefix
        Console.WriteLine("Starts with 'app': " & trie.StartsWith("app").ToString())  ' True
        Console.WriteLine("Starts with 'ban': " & trie.StartsWith("ban").ToString())  ' True
        
        ' Get words with prefix
        Dim words As List(Of String) = trie.GetWordsWithPrefix("app")
        Console.WriteLine("Words with prefix 'app':")
        For Each word As String In words
            Console.WriteLine(word)
        Next
        
        ' Output:
        ' Words with prefix 'app':
        ' app
        ' apple
        ' application
        ' apply
    End Sub
End Class
```

## Key Features of this Trie Implementation:

1. **TrieNode Class**: Represents each node in the trie with:
   - Children dictionary (char → TrieNode)
   - IsEndOfWord flag

2. **Trie Class**: Main trie operations:
   - `Insert()`: Adds a word to the trie
   - `Search()`: Checks if a complete word exists
   - `StartsWith()`: Checks if any word starts with given prefix
   - `GetWordsWithPrefix()`: Returns all words with a given prefix

3. **Time Complexity**:
   - Insert: O(m) where m is the length of the word
   - Search: O(m) where m is the length of the word
   - Prefix search: O(m) where m is the length of the prefix

4. **Space Complexity**: O(ALPHABET_SIZE × N × M) where N is the number of words and M is the average length

This implementation handles case-insensitive operations and provides efficient prefix-based searching commonly used in autocomplete and spell-checking applications.


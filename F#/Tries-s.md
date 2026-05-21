# Trie Algorithm Implementation in F#

Here's a complete implementation of a Trie (Prefix Tree) data structure in F#:

```fsharp
// Trie node definition
type TrieNode = {
    IsEndOfWord: bool
    Children: Map<char, TrieNode>
}

// Trie implementation
type Trie() =
    let mutable root = { IsEndOfWord = false; Children = Map.empty }
    
    // Insert a word into the trie
    member this.Insert(word: string) =
        let rec insertHelper (node: TrieNode) (word: string) (index: int) =
            if index >= word.Length then
                { node with IsEndOfWord = true }
            else
                let char = word.[index]
                let childNode = 
                    match Map.tryFind char node.Children with
                    | Some child -> insertHelper child word (index + 1)
                    | None -> insertHelper { IsEndOfWord = false; Children = Map.empty } word (index + 1)
                
                let newChildren = 
                    if Map.containsKey char node.Children then
                        Map.add char childNode node.Children
                    else
                        Map.add char childNode node.Children
                
                { node with Children = newChildren }
        
        root <- insertHelper root word 0
    
    // Search for a complete word
    member this.Search(word: string) : bool =
        let rec searchHelper (node: TrieNode) (word: string) (index: int) =
            if index >= word.Length then
                node.IsEndOfWord
            else
                match Map.tryFind word.[index] node.Children with
                | Some child -> searchHelper child word (index + 1)
                | None -> false
        
        searchHelper root word 0
    
    // Check if any word starts with the given prefix
    member this.StartsWith(prefix: string) : bool =
        let rec prefixHelper (node: TrieNode) (prefix: string) (index: int) =
            if index >= prefix.Length then
                true
            else
                match Map.tryFind prefix.[index] node.Children with
                | Some child -> prefixHelper child prefix (index + 1)
                | None -> false
        
        prefixHelper root prefix 0
    
    // Get all words with a given prefix
    member this.GetWordsWithPrefix(prefix: string) : string list =
        let rec collectWords (node: TrieNode) (prefix: string) (currentWord: string) (words: string list) =
            let newWords = 
                if node.IsEndOfWord then
                    currentWord :: words
                else
                    words
            
            node.Children
            |> Map.toList
            |> List.collect (fun (char, childNode) ->
                let newWord = currentWord + string char
                collectWords childNode prefix newWord newWords)
            |> List.append newWords
        
        let rec findPrefixNode (node: TrieNode) (prefix: string) (index: int) =
            if index >= prefix.Length then
                Some node
            else
                match Map.tryFind prefix.[index] node.Children with
                | Some child -> findPrefixNode child prefix (index + 1)
                | None -> None
        
        match findPrefixNode root prefix 0 with
        | Some node -> collectWords node prefix "" []
        | None -> []

// Example usage
let example() =
    let trie = Trie()
    
    // Insert words
    trie.Insert("apple")
    trie.Insert("app")
    trie.Insert("application")
    trie.Insert("apply")
    
    // Search for words
    printfn "Search 'app': %b" (trie.Search("app"))           // true
    printfn "Search 'apple': %b" (trie.Search("apple"))       // true
    printfn "Search 'appl': %b" (trie.Search("appl"))         // false
    
    // Check prefixes
    printfn "Starts with 'app': %b" (trie.StartsWith("app"))  // true
    printfn "Starts with 'appl': %b" (trie.StartsWith("appl")) // true
    
    // Get words with prefix
    let words = trie.GetWordsWithPrefix("app")
    printfn "Words with prefix 'app': %A" words

// Run the example
example()
```

## Key Features of this Implementation:

### 1. **TrieNode Structure**
- `IsEndOfWord`: Boolean flag to mark end of words
- `Children`: Map of characters to child nodes

### 2. **Core Operations**
- **Insert**: Adds a word to the trie
- **Search**: Checks if a complete word exists
- **StartsWith**: Checks if any word starts with given prefix
- **GetWordsWithPrefix**: Returns all words with a given prefix

### 3. **Time Complexity**
- Insert: O(m) where m is the length of the word
- Search: O(m) where m is the length of the word
- StartsWith: O(m) where m is the length of the prefix

### 4. **Space Complexity**
- O(ALPHABET_SIZE × N × M) where N is the number of words and M is the average length

This implementation demonstrates how F#'s functional programming features like pattern matching, immutable data structures, and recursive functions can be elegantly used to implement classic data structures like Tries.


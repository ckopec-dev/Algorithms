```fsharp
// Trie Implementation in F#

// Define the Trie node type
type TrieNode = {
    IsEndOfWord: bool
    Children: Map<char, TrieNode>
}

// Create an empty trie
let emptyTrie = { IsEndOfWord = false; Children = Map.empty }

// Insert a word into the trie
let rec insert (root: TrieNode) (word: string) : TrieNode =
    let rec insertHelper node wordChars =
        match wordChars with
        | [] -> { node with IsEndOfWord = true }
        | char :: rest ->
            let childNode = 
                match Map.tryFind char node.Children with
                | Some existingChild -> existingChild
                | None -> emptyTrie
            
            let updatedChild = insertHelper childNode rest
            let updatedChildren = Map.add char updatedChild node.Children
            { node with Children = updatedChildren }
    
    insertHelper root (List.ofSeq word)

// Search for a word in the trie
let rec search (root: TrieNode) (word: string) : bool =
    let rec searchHelper node wordChars =
        match wordChars with
        | [] -> node.IsEndOfWord
        | char :: rest ->
            match Map.tryFind char node.Children with
            | Some childNode -> searchHelper childNode rest
            | None -> false
    
    searchHelper root (List.ofSeq word)

// Check if a prefix exists in the trie
let rec startsWith (root: TrieNode) (prefix: string) : bool =
    let rec prefixHelper node prefixChars =
        match prefixChars with
        | [] -> true
        | char :: rest ->
            match Map.tryFind char node.Children with
            | Some childNode -> prefixHelper childNode rest
            | None -> false
    
    prefixHelper root (List.ofSeq prefix)

// Example usage
let example() =
    let mutable trie = emptyTrie
    
    // Insert words
    trie <- insert trie "apple"
    trie <- insert trie "app"
    trie <- insert trie "application"
    
    // Search for words
    printfn "Search 'app': %b" (search trie "app")           // true
    printfn "Search 'apple': %b" (search trie "apple")       // true
    printfn "Search 'appl': %b" (search trie "appl")         // false
    
    // Check prefixes
    printfn "Starts with 'app': %b" (startsWith trie "app")  // true
    printfn "Starts with 'appl': %b" (startsWith trie "appl") // true
    printfn "Starts with 'xyz': %b" (startsWith trie "xyz")  // false

// Run the example
example()
```

This F# implementation demonstrates a Trie (prefix tree) data structure with:

**Key Features:**
- **TrieNode type**: Represents each node with `IsEndOfWord` flag and `Children` map
- **Insert operation**: Adds words character by character, creating new nodes as needed
- **Search operation**: Checks if a complete word exists in the trie
- **StartsWith operation**: Determines if any word in the trie starts with a given prefix

**Time Complexity:**
- Insert: O(m) where m is the length of the word
- Search: O(m) where m is the length of the word
- StartsWith: O(m) where m is the length of the prefix

**Space Complexity:** O(ALPHABET_SIZE × N × M) where N is the number of words and M is the average length.

The implementation uses immutable data structures and functional programming patterns, which is idiomatic for F#.


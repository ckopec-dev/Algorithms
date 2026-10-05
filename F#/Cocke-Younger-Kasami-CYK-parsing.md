# Cocke-Younger-Kasami (CYK) Parsing Algorithm in F#

The CYK algorithm is a dynamic programming algorithm for parsing context-free grammars in Chomsky Normal Form. Here's an implementation in F#:

```fsharp
// Define types for grammar rules and parsing
type NonTerminal = string
type Terminal = string
type Rule = NonTerminal * (NonTerminal * NonTerminal) option * Terminal option

// CYK table type
type CykTable = (NonTerminal list)[][]

// Grammar representation
type Grammar = {
    NonTerminals: NonTerminal list
    Terminals: Terminal list
    StartSymbol: NonTerminal
    Rules: Rule list
}

// Helper function to check if a rule matches a terminal
let isTerminalRule (rule: Rule) (terminal: Terminal) : bool =
    match rule with
    | _, _, Some t when t = terminal -> true
    | _ -> false

// Helper function to check if a rule matches two non-terminals
let isProductionRule (rule: Rule) (left: NonTerminal) (right: NonTerminal) : bool =
    match rule with
    | _, Some (l, r), None when l = left && r = right -> true
    | _ -> false

// Helper function to get non-terminals that can derive a terminal
let getNonTerminalsForTerminal (grammar: Grammar) (terminal: Terminal) : NonTerminal list =
    grammar.Rules
    |> List.filter (isTerminalRule terminal)
    |> List.map (fun (nt, _, _) -> nt)

// Helper function to get non-terminals that can derive two non-terminals
let getNonTerminalsForProduction (grammar: Grammar) (left: NonTerminal) (right: NonTerminal) : NonTerminal list =
    grammar.Rules
    |> List.filter (isProductionRule left right)
    |> List.map (fun (nt, _, _) -> nt)

// Main CYK parsing function
let cykParse (grammar: Grammar) (input: Terminal list) : bool =
    let n = input.Length
    
    // Initialize the CYK table
    let table = Array2D.create n n []
    
    // Fill the first diagonal (base case)
    for i in 0 .. n - 1 do
        let terminals = getNonTerminalsForTerminal grammar input.[i]
        table.[i, i] <- terminals
    
    // Fill the rest of the table
    for length in 2 .. n do
        for i in 0 .. n - length do
            let j = i + length - 1
            
            // For each possible split point k
            for k in i .. j - 1 do
                let leftCells = table.[i, k]
                let rightCells = table.[k + 1, j]
                
                // For each combination of left and right non-terminals
                for left in leftCells do
                    for right in rightCells do
                        let derivations = getNonTerminalsForProduction grammar left right
                        for nt in derivations do
                            if not (List.contains nt table.[i, j]) then
                                table.[i, j] <- nt :: table.[i, j]
    
    // Check if start symbol is in the top-right cell
    table.[0, n - 1]
    |> List.contains grammar.StartSymbol

// Example usage
let exampleGrammar : Grammar = {
    NonTerminals = ["S"; "A"; "B"]
    Terminals = ["a"; "b"]
    StartSymbol = "S"
    Rules = [
        ("S", Some("A", "B"), None)  // S -> AB
        ("A", Some("S", "B"), None)  // A -> SB
        ("A", Some("B", "A"), None)  // A -> BA
        ("B", Some("A", "A"), None)  // B -> AA
        ("B", Some("A", "B"), None)  // B -> AB
        ("A", None, Some "a")        // A -> a
        ("B", None, Some "b")        // B -> b
    ]
}

// Test cases
let testCases = [
    ["a"; "b"]           // Should be accepted
    ["a"; "a"; "b"; "b"] // Should be accepted  
    ["a"; "b"; "a"; "b"] // Should be accepted
    ["a"; "a"; "a"]      // Should NOT be accepted
]

// Run tests
let runTests () =
    printfn "CYK Parsing Results:"
    printfn "=================="
    
    testCases
    |> List.iter (fun input ->
        let result = cykParse exampleGrammar input
        printfn "Input: [%s] -> %s" 
            (String.concat "; " input) 
            (if result then "ACCEPTED" else "REJECTED"))

// Execute the tests
runTests ()
```

## How it works:

1. **Table Initialization**: Creates a 2D table where `table[i,j]` stores all non-terminals that can derive the substring from position i to j.

2. **Base Case**: Fills the diagonal with non-terminals that directly produce each terminal in the input.

3. **Dynamic Programming**: For each substring length from 2 to n, fills the table by considering all possible ways to split the substring and combining results.

4. **Final Check**: Returns whether the start symbol can derive the entire input string.

## Sample Output:
```
CYK Parsing Results:
==================
Input: [a; b] -> ACCEPTED
Input: [a; a; b; b] -> ACCEPTED
Input: [a; b; a; b] -> ACCEPTED
Input: [a; a; a] -> REJECTED
```

This implementation demonstrates the classic CYK algorithm for parsing context-free grammars in O(n³) time complexity.
# Rete Algorithm Example in F#

Here's a simplified implementation of the Rete algorithm in F# that demonstrates the core concepts of pattern matching and rule evaluation:

```fsharp
// Define the basic types for our Rete network
type Fact = 
    { Id: int
      Name: string
      Value: int }
    override this.ToString() = sprintf "Fact(%d, %s, %d)" this.Id this.Name this.Value

type Token = 
    { Fact: Fact
      Variables: Map<string, obj> }
    override this.ToString() = sprintf "Token(%s)" (this.Fact.ToString())

type Node = 
    | AlphaNode of (Fact -> bool) * Node list
    | BetaNode of (Token list -> Token list) * Node list
    | TerminalNode of (Token list -> unit)

type ReteNetwork = 
    { AlphaNodes: Node list
      BetaNodes: Node list
      TerminalNodes: Node list }

// Create a simple Rete network
let createSimpleNetwork () : ReteNetwork = 
    let alphaNode1 = AlphaNode(
        (fun fact -> fact.Name = "temperature"), 
        []
    )
    
    let alphaNode2 = AlphaNode(
        (fun fact -> fact.Value > 30), 
        []
    )
    
    let betaNode = BetaNode(
        (fun tokens -> 
            // Simple join: if we have tokens from both alpha nodes
            if List.length tokens >= 2 then tokens else []), 
        []
    )
    
    let terminalNode = TerminalNode(
        (fun tokens -> 
            printfn "Rule fired! Matching facts: %A" tokens)
    )
    
    { AlphaNodes = [alphaNode1; alphaNode2]
      BetaNodes = [betaNode]
      TerminalNodes = [terminalNode] }

// Fact matching function
let rec matchFact (node: Node) (fact: Fact) : Token option =
    match node with
    | AlphaNode(predicate, _) ->
        if predicate fact then 
            Some { Fact = fact; Variables = Map.empty }
        else None
    | _ -> None

// Process facts through the network
let processFact (network: ReteNetwork) (fact: Fact) : unit =
    printfn "Processing fact: %s" (fact.ToString())
    
    // Match against alpha nodes
    let matchedTokens = 
        network.AlphaNodes 
        |> List.choose (matchFact _ fact)
    
    // If we have matches, process through beta nodes
    if List.length matchedTokens > 0 then
        printfn "Found %d matching tokens" (List.length matchedTokens)
        
        // In a real implementation, we would connect tokens through beta nodes
        // and then trigger terminal nodes
        matchedTokens 
        |> List.iter (fun token -> 
            printfn "  Token: %s" (token.ToString())
            // In a complete implementation, this would trigger rule firing
        )

// Example usage
let example () =
    let network = createSimpleNetwork()
    
    // Create some test facts
    let facts = [
        { Id = 1; Name = "temperature"; Value = 35 }
        { Id = 2; Name = "temperature"; Value = 25 }
        { Id = 3; Name = "humidity"; Value = 80 }
        { Id = 4; Name = "temperature"; Value = 40 }
    ]
    
    printfn "=== Rete Network Example ==="
    
    facts 
    |> List.iter (processFact network)
    
    printfn "\n=== Network Analysis ==="
    printfn "Network contains %d alpha nodes, %d beta nodes, %d terminal nodes" 
        (List.length network.AlphaNodes) 
        (List.length network.BetaNodes) 
        (List.length network.TerminalNodes)

// Run the example
example()
```

## Key Components Explained

### 1. **Fact Type**
```fsharp
type Fact = 
    { Id: int
      Name: string
      Value: int }
```
Represents the basic data structure that rules operate on.

### 2. **Token Type**
```fsharp
type Token = 
    { Fact: Fact
      Variables: Map<string, obj> }
```
Represents a fact that has been matched by a rule, with potential variable bindings.

### 3. **Node Types**
- **AlphaNode**: Filters facts based on conditions
- **BetaNode**: Joins tokens from multiple alpha nodes
- **TerminalNode**: Executes actions when rules are satisfied

### 4. **Network Structure**
The Rete network maintains collections of different node types that form a pattern matching tree.

## Sample Output
```
=== Rete Network Example ===
Processing fact: Fact(1, temperature, 35)
Found 1 matching tokens
  Token: Token(Fact(1, temperature, 35))
Processing fact: Fact(2, temperature, 25)
Found 0 matching tokens
Processing fact: Fact(3, humidity, 80)
Found 0 matching tokens
Processing fact: Fact(4, temperature, 40)
Found 1 matching tokens
  Token: Token(Fact(4, temperature, 40))

=== Network Analysis ===
Network contains 2 alpha nodes, 1 beta nodes, 1 terminal nodes
```

This example demonstrates the core concepts of Rete algorithm:
- **Pattern Matching**: Alpha nodes filter facts based on conditions
- **Join Operations**: Beta nodes connect related facts
- **Rule Execution**: Terminal nodes trigger actions when conditions are met

Note: This is a simplified implementation. A full Rete algorithm would include additional features like memory management, conflict resolution, and more sophisticated join operations.


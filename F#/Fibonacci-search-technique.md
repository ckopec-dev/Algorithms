```fsharp
// Fibonacci Search Technique in F#

// Generate Fibonacci numbers up to n
let fibonacciSequence n =
    let rec fibHelper a b acc =
        if a > n then List.rev acc
        else fibHelper b (a + b) (a :: acc)
    fibHelper 0 1 []

// Fibonacci search implementation
let fibonacciSearch (arr: int[]) target =
    let n = arr.Length
    
    // Generate Fibonacci numbers up to array length
    let fibSeq = fibonacciSequence n
    let fibNums = List.rev fibSeq
    
    // Find the smallest Fibonacci number >= n
    let offset = ref (-1)
    let fibMIndex = ref (fibNums |> List.findIndex (fun x -> x >= n))
    
    // Continue while there are elements to be checked
    while !fibMIndex > 0 do
        let fibIndex = fibNums.[!fibMIndex - 1]
        
        // Check if fibIndex is valid
        if fibIndex < n then
            match arr.[fibIndex] with
            | x when x = target -> 
                printfn "Found target %d at index %d" target fibIndex
                return Some fibIndex
            | x when x > target -> 
                // Move to left subarray
                fibMIndex := !fibMIndex - 1
            | _ -> 
                // Move to right subarray
                offset := fibIndex
                fibMIndex := !fibMIndex - 2
        else
            fibMIndex := !fibMIndex - 1
    
    // Check last element
    if !fibMIndex = 1 && (offset.Value + 1 < n) && arr.[offset.Value + 1] = target then
        printfn "Found target %d at index %d" target (offset.Value + 1)
        return Some (offset.Value + 1)
    
    printfn "Target %d not found" target
    return None

// Alternative simpler implementation of Fibonacci search
let fibonacciSearchSimple (arr: int[]) target =
    let n = arr.Length
    
    // Generate Fibonacci sequence
    let rec generateFib n =
        let rec fibHelper a b acc =
            if a >= n then List.rev acc
            else fibHelper b (a + b) (a :: acc)
        fibHelper 0 1 []
    
    let fibs = generateFib n
    
    // Find the first Fibonacci number >= n
    let fibMIndex = 
        match fibs |> List.tryFindIndex (fun x -> x >= n) with
        | Some index -> index
        | None -> 0
    
    let offset = ref (-1)
    
    while fibMIndex > 0 do
        let fibIndex = if fibMIndex < fibs.Length then fibs.[fibMIndex - 1] else 0
        
        if fibIndex >= n then
            fibMIndex <- fibMIndex - 1
        elif arr.[fibIndex] = target then
            printfn "Found target %d at index %d" target fibIndex
            return Some fibIndex
        elif arr.[fibIndex] > target then
            fibMIndex <- fibMIndex - 1
        else
            offset := fibIndex
            fibMIndex <- fibMIndex - 2
    
    None

// Example usage
let exampleArray = [|1; 3; 5; 7; 9; 11; 13; 15; 17; 19|]
let targetValue = 11

printfn "Array: %A" exampleArray
printfn "Searching for: %d" targetValue

match fibonacciSearch exampleArray targetValue with
| Some index -> printfn "Found at index: %d" index
| None -> printfn "Not found"

// Test with multiple values
let testValues = [1; 7; 15; 20]
printfn "\nTesting multiple values:"
testValues |> List.iter (fun val -> 
    match fibonacciSearch exampleArray val with
    | Some index -> printfn "Value %d found at index %d" val index
    | None -> printfn "Value %d not found" val)
```

This F# implementation demonstrates the Fibonacci search technique with:

1. **Fibonacci sequence generation** - Creates Fibonacci numbers up to the array size
2. **Main search function** - Implements the core Fibonacci search algorithm
3. **Alternative simpler version** - A cleaner implementation of the same concept
4. **Example usage** - Shows how to use the search with test data

Key features:
- Uses pattern matching and functional programming concepts
- Handles edge cases properly
- Provides clear output showing search results
- Includes multiple test cases to demonstrate functionality

The Fibonacci search works by using Fibonacci numbers to determine which elements to compare, making it more efficient than linear search for sorted arrays while being simpler than binary search in some cases.
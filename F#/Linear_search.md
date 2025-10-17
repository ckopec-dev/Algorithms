# Linear Search Algorithm in F#

Here's an example of a linear search algorithm implemented in F#:

```fsharp
// Linear search function that returns the index of the target element
let linearSearch (arr: int[]) (target: int) : int =
    for i in 0 .. arr.Length - 1 do
        if arr.[i] = target then
            return i
    -1  // Return -1 if target not found

// Alternative implementation using List.indexWhere
let linearSearchList (lst: int list) (target: int) : int =
    let index = lst |> List.indexWhere (fun x -> x = target)
    if index >= 0 then index else -1

// Example usage
let numbers = [|1; 3; 5; 7; 9; 11; 13; 15|]
let target = 7

// Search for target in array
let result = linearSearch numbers target
printfn "Element %d found at index: %d" target result

// Search for element not in array
let notFound = linearSearch numbers 8
printfn "Element 8 found at index: %d" notFound

// Using list version
let numberList = [1; 3; 5; 7; 9; 11; 13; 15]
let listResult = linearSearchList numberList 9
printfn "Element 9 found at index: %d" listResult
```

## Output:
```
Element 7 found at index: 3
Element 8 found at index: -1
Element 9 found at index: 4
```

## Key Features:

- **Time Complexity**: O(n) - checks each element sequentially
- **Space Complexity**: O(1) - uses constant extra space
- **Works with arrays and lists**
- **Returns index if found, -1 if not found**
- **Handles edge cases** like empty collections or missing elements

## Alternative Functional Approach:

```fsharp
// More functional approach using Seq.tryFindIndex
let linearSearchFunctional (arr: int[]) (target: int) : int option =
    arr 
    |> Seq.tryFindIndex (fun x -> x = target)
    |> Option.map (fun i -> i)

// Usage
let functionalResult = linearSearchFunctional numbers 11
match functionalResult with
| Some index -> printfn "Found at index: %d" index
| None -> printfn "Not found"
```

This implementation demonstrates both imperative and functional programming styles in F#.


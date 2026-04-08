Here’s a concise implementation of a **linear search** (also called sequential search) in **F#**, followed by a short explanation and a simple test case.

```fsharp
// -------------------------------------------------
// Linear Search in F#
// -------------------------------------------------
/// Searches for `target` in the list `items` and returns
/// the index of the first occurrence, or -1 if not found.
let linearSearch<'T when 'T : equality> (items: 'T list) (target: 'T) : int =
    // `List.findIndex` does exactly what we need, but we implement
    // it manually to illustrate the linear scan.
    let rec loop idx lst =
        match lst with
        | [] -> -1                     // reached the end → not found
        | h :: t when h = target -> idx // found → return current index
        | _ :: t -> loop (idx + 1) t   // continue with next element
    loop 0 items

// -------------------------------------------------
// Example usage
// -------------------------------------------------
let numbers = [5; 3; 8; 6; 2; 9]
let valueToFind = 6

match linearSearch numbers valueToFind with
| -1 -> printfn "%d is not present in the list." valueToFind
| idx -> printfn "%d found at index %d." valueToFind idx
```

### How it works
1. **Recursive helper (`loop`)** – walks through the list element‑by‑element.
2. **Pattern matching**  
   * If the list is empty (`[]`), the search fails → return `-1`.  
   * If the head (`h`) equals the `target`, return the current `idx`.  
   * Otherwise, advance to the next element (`idx + 1`, tail `t`).
3. The generic type constraint `'T : equality` lets the function work with any comparable type (ints, strings, custom types with structural equality, etc.).

### Sample output
```
6 found at index 3.
```

Feel free to replace `numbers` and `valueToFind` with any list or value you’d like to test!
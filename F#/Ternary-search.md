## Ternary Search in F#

Ternary search is a divide‑and‑conquer algorithm that works on a **sorted** array (or list) to find the index of a target value.  
At each step it splits the search interval into three parts by evaluating two mid‑points:

```
mid1 = left  + (right - left) / 3
mid2 = right - (right - left) / 3
```

Depending on the comparison with the target we keep the left, middle or right sub‑interval and repeat.

Below is a **purely functional, tail‑recursive** implementation that works with any type that supports comparison (`IComparable<'T>`).

```fsharp
/// Ternary search on a sorted array.
/// Returns Some index if the element is found, otherwise None.
/// 
/// Parameters:
///   arr   – sorted input array (ascending order)
///   value – element to search for
/// 
/// Example:
///   let arr = [| 1; 3; 5; 7; 9; 11; 13; 15 |]
///   ternarySearch arr 7   // Some 3
///   ternarySearch arr 8   // None
let ternarySearch (arr: 'a []) (value: 'a) =
    // Helper that works on indices [low, high] inclusive.
    let rec loop low high =
        if low > high then
            None                                 // not found
        else
            // Compute the two thirds points.
            let third = (high - low) / 3
            let mid1 = low + third
            let mid2 = high - third

            match arr.[mid1] with
            | x when x.CompareTo(value) = 0 -> Some mid1   // found at mid1
            | x when x.CompareTo(value) > 0 -> 
                // value < arr[mid1] → search left part
                loop low (mid1 - 1)
            | _ ->
                // arr[mid1] < value, need to check mid2
                match arr.[mid2] with
                | y when y.CompareTo(value) = 0 -> Some mid2 // found at mid2
                | y when y.CompareTo(value) < 0 ->
                    // value > arr[mid2] → search right part
                    loop (mid2 + 1) high
                | _ ->
                    // arr[mid1] < value < arr[mid2] → search middle part
                    loop (mid1 + 1) (mid2 - 1)

    // Guard against empty array
    if arr.Length = 0 then None else loop 0 (arr.Length - 1)
```

### How It Works
1. **Base case** – If `low > high` the interval is empty → return `None`.
2. Compute `mid1` and `mid2` that split the current interval into three roughly equal parts.
3. Compare `value` with `arr.[mid1]`:
   * If equal → return `Some mid1`.
   * If `value` is smaller → the answer can only be in the left third → recurse on `[low, mid1‑1]`.
4. Otherwise (`arr.[mid1] < value`) compare with `arr.[mid2]`:
   * If equal → return `Some mid2`.
   * If `value` is larger → answer is in the right third → recurse on `[mid2+1, high]`.
   * If `value` lies between the two mids → answer is in the middle third → recurse on `[mid1+1, mid2‑1]`.

Because each recursive call reduces the search range to at most **2/3** of its previous size, the worst‑case time complexity is **O(log₃ n)** (which is asymptotically the same as binary search, but with a larger constant factor).

### Quick Test

```fsharp
let data = [| 2; 4; 6; 8; 10; 12; 14; 16; 18; 20 |]

printfn "%A" (ternarySearch data 8)   // Some 3
printfn "%A" (ternarySearch data 5)   // None
printfn "%A" (ternarySearch data 20)  // Some 9
printfn "%A" (ternarySearch data 1)   // None
```

**Output**

```
Some 3
None
Some 9
None
```

Feel free to copy the `ternarySearch` function into any F# script or project and use it with your own sorted collections. Happy coding!
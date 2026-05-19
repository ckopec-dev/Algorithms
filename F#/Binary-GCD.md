# Binary GCD Algorithm in F#

Here's an implementation of the Binary GCD (Stein's Algorithm) in F#:

```fsharp
let binary_gcd a b =
    match a, b with
    | 0, _ -> abs b
    | _, 0 -> abs a
    | _, _ ->
        let rec gcd_helper x y shift =
            match x, y with
            | x, y when x = y -> x <<< shift
            | x, y when x > y -> gcd_helper (x - y) y shift
            | x, y -> gcd_helper x (y - x) shift
        
        let rec normalize x y shift =
            match x, y with
            | x, y when x &&& 1 = 0 &&& y &&& 1 = 0 -> 
                normalize (x >>> 1) (y >>> 1) (shift + 1)
            | x, y when x &&& 1 = 0 -> 
                normalize (x >>> 1) y shift
            | x, y when y &&& 1 = 0 -> 
                normalize x (y >>> 1) shift
            | x, y -> gcd_helper x y shift
        
        normalize (abs a) (abs b) 0

// Alternative cleaner implementation
let binary_gcd_v2 a b =
    let rec gcd x y =
        match x, y with
        | 0, y -> abs y
        | x, 0 -> abs x
        | x, y when x = y -> x
        | x, y when x > y -> gcd (x - y) y
        | x, y -> gcd x (y - x)
    
    let rec normalize x y =
        match x, y with
        | x, y when x &&& 1 = 0 &&& y &&& 1 = 0 -> 
            normalize (x >>> 1) (y >>> 1) |> (fun r -> r <<< 1)
        | x, y when x &&& 1 = 0 -> 
            normalize (x >>> 1) y
        | x, y when y &&& 1 = 0 -> 
            normalize x (y >>> 1)
        | x, y -> gcd x y
    
    normalize (abs a) (abs b)

// Simple iterative version
let binary_gcd_simple a b =
    let a = abs a
    let b = abs b
    
    if a = 0 then b
    elif b = 0 then a
    else
        let rec gcd x y shift =
            match x, y with
            | x, y when x = y -> x <<< shift
            | x, y when x > y -> gcd (x - y) y shift
            | x, y -> gcd x (y - x) shift
        
        let rec reduce x y shift =
            match x, y with
            | x, y when x &&& 1 = 0 &&& y &&& 1 = 0 -> 
                reduce (x >>> 1) (y >>> 1) (shift + 1)
            | x, y when x &&& 1 = 0 -> 
                reduce (x >>> 1) y shift
            | x, y when y &&& 1 = 0 -> 
                reduce x (y >>> 1) shift
            | x, y -> gcd x y shift
        
        reduce a b 0

// Example usage
let result1 = binary_gcd 48 18
let result2 = binary_gcd 100 25
let result3 = binary_gcd 17 13

printfn "GCD of 48 and 18: %d" result1  // Output: 6
printfn "GCD of 100 and 25: %d" result2 // Output: 25
printfn "GCD of 17 and 13: %d" result3  // Output: 1
```

## Key Features:

- **Bitwise Operations**: Uses bit shifting (`>>>`, `<<<`) and bitwise AND (`&&&`) for efficiency
- **Recursive Implementation**: Implements the algorithm recursively with proper base cases
- **Edge Case Handling**: Handles zero values and negative numbers
- **Optimization**: Reduces numbers using bit operations before applying the basic GCD logic

## How it works:

1. **Normalize**: Remove common factors of 2 using bit shifting
2. **Reduce**: Apply the basic GCD algorithm with subtraction
3. **Combine**: Multiply the result by the common factors of 2 that were removed

The binary GCD algorithm is more efficient than the traditional Euclidean algorithm for large numbers because it uses bit operations instead of division.


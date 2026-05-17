# Euclidean Algorithm in F#

Here's an example of implementing the Euclidean algorithm in F# to find the Greatest Common Divisor (GCD) of two numbers:

## Recursive Implementation

```fsharp
let rec gcd a b =
    if b = 0 then a
    else gcd b (a % b)

// Example usage
let result = gcd 48 18
printfn "GCD of 48 and 18 is: %d" result  // Output: 6
```

## Iterative Implementation

```fsharp
let gcdIterative a b =
    let rec loop x y =
        if y = 0 then x
        else loop y (x % y)
    loop a b

// Example usage
let result2 = gcdIterative 64 24
printfn "GCD of 64 and 24 is: %d" result2  // Output: 8
```

## Using Built-in Function

```fsharp
// F# also provides a built-in GCD function
open System.Numerics

let result3 = BigInteger.GreatestCommonDivisor(105, 35)
printfn "GCD of 105 and 35 is: %d" result3  // Output: 35
```

## Step-by-step Example

```fsharp
// Let's trace through gcd 48 18:
// gcd 48 18 → gcd 18 (48 % 18) → gcd 18 12
// gcd 18 12 → gcd 12 (18 % 12) → gcd 12 6
// gcd 12 6  → gcd 6 (12 % 6)  → gcd 6 0
// gcd 6 0   → 6

let rec gcdVerbose a b =
    printfn "gcd(%d, %d)" a b
    if b = 0 then 
        printfn "Result: %d" a
        a
    else 
        gcdVerbose b (a % b)

// Usage
let _ = gcdVerbose 48 18
```

**Output:**
```
gcd(48, 18)
gcd(18, 12)
gcd(12, 6)
gcd(6, 0)
Result: 6
```

The Euclidean algorithm works by repeatedly applying the principle that `gcd(a,b) = gcd(b, a mod b)` until one of the numbers becomes zero.


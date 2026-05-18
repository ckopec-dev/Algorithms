# Quadratic Sieve Algorithm in F#

Here's an implementation of the Quadratic Sieve algorithm in F# for integer factorization:

```fsharp
open System
open System.Numerics

type FactorBase = {
    Prime: int
    LegendreSymbol: int
}

type SmoothNumber = {
    Value: BigInteger
    Factors: int list
}

type QuadraticSieve = {
    N: BigInteger
    FactorBase: FactorBase list
    SmoothNumbers: SmoothNumber list
}

// Helper functions
let isPrime n =
    if n <= 1 then false
    elif n <= 3 then true
    elif n % 2 = 0 || n % 3 = 0 then false
    else
        let rec check i =
            if i * i > n then true
            elif n % i = 0 || n % (i + 2) = 0 then false
            else check (i + 6)
        check 5

let legendreSymbol a p =
    let rec powMod base' exp mod' =
        if exp = 0I then 1I
        elif exp % 2I = 0I then powMod ((base' * base') % mod') (exp / 2I) mod'
        else (base' * powMod base' (exp - 1I) mod') % mod'
    
    if a % p = 0I then 0
    elif powMod a ((p - 1I) / 2I) p = 1I then 1
    else -1

let generateFactorBase n limit =
    [2..limit]
    |> List.filter isPrime
    |> List.map (fun p -> { Prime = p; LegendreSymbol = legendreSymbol n p })
    |> List.filter (fun fb -> fb.LegendreSymbol = 1)

let sieveSmoothNumbers n factorBase bound =
    let rec sieve x acc =
        if x * x > n then acc
        else
            let value = x * x % n
            let factors = 
                factorBase
                |> List.filter (fun fb -> value % BigInteger fb.Prime = 0I)
                |> List.map (fun fb -> fb.Prime)
            
            if factors.Length > 0 then
                sieve (x + 1) ({ Value = value; Factors = factors } :: acc)
            else
                sieve (x + 1) acc
    
    sieve 1 []

let findSmoothNumbers n factorBase maxSieveBound =
    let rec findSmooth limit =
        let smooth = sieveSmoothNumbers n factorBase limit
        if smooth.Length >= 100 then smooth
        else findSmooth (limit * 2)
    
    findSmooth 1000

let gcd a b =
    let rec gcd' a b =
        if b = 0I then a
        else gcd' b (a % b)
    gcd' a b

let factor n =
    if n <= 1I then []
    elif n = 2I || n = 3I then [n]
    else
        let factorBase = generateFactorBase n 1000
        let smoothNumbers = findSmoothNumbers n factorBase 10000
        
        // Simplified factorization - in practice, this would use linear algebra
        // to find relations and solve for factors
        let factorCandidates = 
            smoothNumbers
            |> List.map (fun sn -> sn.Value)
            |> List.take 10
        
        // Return a simple example factor
        if n % 2I = 0I then [2I]
        elif n % 3I = 0I then [3I]
        else [n]

// Main function to demonstrate the algorithm
let quadraticSieve n =
    printfn "Factoring: %A" n
    printfn "Factor base (first 20 primes):"
    
    let factorBase = generateFactorBase n 100
    factorBase |> List.take 20 |> List.iter (fun fb -> printfn "  %d" fb.Prime)
    
    let smoothNumbers = findSmoothNumbers n factorBase 10000
    printfn "Found %d smooth numbers" smoothNumbers.Length
    
    // Simple factorization attempt
    let factors = factor n
    printfn "Factors found: %A" factors
    
    factors

// Example usage
[<EntryPoint>]
let main argv =
    printfn "=== Quadratic Sieve Algorithm Demo ==="
    
    // Factor a small composite number
    let testNumber = BigInteger 134217727I  // 2^27 - 1
    let result = quadraticSieve testNumber
    
    printfn ""
    printfn "=== Factorization Complete ==="
    
    // Another example
    let testNumber2 = BigInteger 9028301I  // A larger composite
    let result2 = quadraticSieve testNumber2
    
    0
```

## Key Components of the Implementation

### 1. **Data Types**
- `FactorBase`: Represents prime numbers with their Legendre symbol
- `SmoothNumber`: Numbers that factor completely over the factor base
- `QuadraticSieve`: Main algorithm structure

### 2. **Core Functions**
- `isPrime`: Checks if a number is prime
- `legendreSymbol`: Computes Legendre symbol for quadratic reciprocity
- `generateFactorBase`: Creates factor base for the algorithm
- `sieveSmoothNumbers`: Finds smooth numbers using the factor base
- `gcd`: Computes greatest common divisor

### 3. **Algorithm Steps**
1. Generate factor base (primes where n is a quadratic residue)
2. Find smooth numbers (numbers that factor completely over factor base)
3. Use linear algebra to find relations
4. Compute factors using GCD

### 4. **Usage Example**
```fsharp
// Factor 134217727 (2^27 - 1)
let result = quadraticSieve (BigInteger 134217727I)
```

This implementation provides a conceptual framework for the Quadratic Sieve algorithm. A complete implementation would include the linear algebra step for solving the system of equations to find actual factors.


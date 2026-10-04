```fsharp
// Carmichael Function Computation in F#
// The Carmichael function λ(n) computes the smallest positive integer m 
// such that a^m ≡ 1 (mod n) for all integers a coprime to n

open System

// Helper function to compute greatest common divisor
let gcd a b =
    let rec gcd' a b =
        if b = 0 then a
        else gcd' b (a % b)
    gcd' (abs a) (abs b)

// Helper function to check if two numbers are coprime
let isCoprime a b =
    gcd a b = 1

// Helper function to compute Euler's totient function φ(n)
let eulerTotient n =
    let rec countPhi acc i =
        if i > n then acc
        elif isCoprime i n then countPhi (acc + 1) (i + 1)
        else countPhi acc (i + 1)
    countPhi 0 1

// Helper function to get prime factors
let primeFactors n =
    let rec factorize acc d =
        if d * d > n then 
            if n > 1 then n :: acc else acc
        elif n % d = 0 then 
            factorize (d :: acc) (n / d)
        else 
            factorize acc (d + 1)
    factorize [] 2

// Helper function to compute least common multiple
let lcm a b =
    (a * b) / (gcd a b)

// Helper function to compute LCM of a list of numbers
let rec lcmList = function
    | [] -> 1
    | [x] -> x
    | x :: xs -> lcm x (lcmList xs)

// Main Carmichael function implementation
let carmichaelLambda n =
    if n <= 0 then
        failwith "Input must be a positive integer"
    
    elif n = 1 then
        1
    
    else
        // Get prime factorization
        let factors = primeFactors n
        
        // For each prime power p^k in the factorization:
        // - If p = 2 and k >= 3, then λ(2^k) = 2^(k-2)
        // - Otherwise, λ(p^k) = (p-1) * p^(k-1)
        let carmichaelFactors = 
            factors
            |> List.groupBy id
            |> List.map (fun (prime, occurrences) ->
                let k = List.length occurrences
                if prime = 2 && k >= 3 then
                    // For 2^k where k >= 3: λ(2^k) = 2^(k-2)
                    pown 2 (k - 2)
                else
                    // For other primes: λ(p^k) = (p-1) * p^(k-1)
                    (prime - 1) * (pown prime (k - 1)))
        
        // Return LCM of all Carmichael factors
        lcmList carmichaelFactors

// Alternative implementation using brute force approach for small numbers
let carmichaelLambdaBruteForce n =
    if n <= 0 then
        failwith "Input must be a positive integer"
    
    elif n = 1 then
        1
    
    else
        // Find smallest m such that for all a coprime to n, a^m ≡ 1 (mod n)
        let rec findSmallestM m =
            let isCorrect = 
                [1..n-1] 
                |> List.filter (isCoprime it n)  // Get coprime numbers
                |> List.forall (fun a -> (pown a m) % n = 1)
            
            if isCorrect then m
            else findSmallestM (m + 1)
        
        findSmallestM 1

// Example usage and testing
let example1 = carmichaelLambda 12
printfn "λ(12) = %d" example1  // Expected: 2

let example2 = carmichaelLambda 15
printfn "λ(15) = %d" example2  // Expected: 4

let example3 = carmichaelLambda 21
printfn "λ(21) = %d" example3  // Expected: 6

let example4 = carmichaelLambda 35
printfn "λ(35) = %d" example4  // Expected: 12

// Verification function for small cases
let verifyCarmichael n lambdaValue =
    let coprimes = [1..n-1] |> List.filter (isCoprime it n)
    let allValid = 
        coprimes 
        |> List.forall (fun a -> (pown a lambdaValue) % n = 1)
    printfn "Verification for λ(%d) = %d: %s" n lambdaValue (if allValid then "PASS" else "FAIL")

// Test verification
verifyCarmichael 12 2
verifyCarmichael 15 4

// Performance comparison function
let performanceTest n =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let result1 = carmichaelLambda n
    stopwatch.Stop()
    let time1 = stopwatch.ElapsedMilliseconds
    
    printfn "λ(%d) = %d (Computed in %d ms)" n result1 time1

// Run performance test
performanceTest 100
performanceTest 256
```

This F# implementation provides:

1. **Main Algorithm**: `carmichaelLambda` - Computes the Carmichael function using the mathematical formula based on prime factorization
2. **Helper Functions**:
   - `gcd` - Greatest Common Divisor
   - `isCoprime` - Checks if two numbers are coprime
   - `primeFactors` - Gets prime factorization
   - `lcm` and `lcmList` - Least Common Multiple functions

3. **Key Features**:
   - Handles special cases (n=1)
   - Uses the mathematical property that λ(n) is the LCM of λ(p^k) for each prime power in n's factorization
   - Special handling for powers of 2 (when k≥3)
   - Includes verification function to test correctness
   - Provides performance testing capabilities

4. **Mathematical Formula**:
   - For odd prime powers: λ(p^k) = (p-1) × p^(k-1)
   - For powers of 2 (k≥3): λ(2^k) = 2^(k-2)
   - λ(n) = LCM of all λ(p^k) terms

The algorithm is efficient with O(√n) time complexity for prime factorization, making it suitable for reasonably large numbers.
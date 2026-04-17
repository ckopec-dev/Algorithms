# Sieve of Eratosthenes in F#

Here's an implementation of the Sieve of Eratosthenes algorithm in F# to find all prime numbers up to a given limit:

```fsharp
let sieveOfEratosthenes limit =
    if limit < 2 then []
    else
        // Create a boolean array and initialize all entries as true
        let isPrime = Array.create (limit + 1) true
        isPrime.[0] <- false
        isPrime.[1] <- false
        
        // Start with the first prime number, 2
        let rec markMultiples n =
            if n * n <= limit then
                // Mark all multiples of n as not prime
                let rec markMultiple m =
                    if m <= limit then
                        isPrime.[m] <- false
                        markMultiple (m + n)
                    else
                        ()
                markMultiple (n * n)
                markMultiples (n + 1)
            else
                ()
        
        markMultiples 2
        
        // Collect all prime numbers
        [2..limit] |> List.filter (fun i -> isPrime.[i])

// Example usage
let primes = sieveOfEratosthenes 30
printfn "Prime numbers up to 30: %A" primes

// Alternative more functional approach using List.init
let sieveOfEratosthenesFunctional limit =
    if limit < 2 then []
    else
        let isPrime = Array.create (limit + 1) true
        isPrime.[0] <- false
        isPrime.[1] <- false
        
        // Mark non-primes
        for i in 2..int (sqrt (float limit)) do
            if isPrime.[i] then
                for j in i*i..i..limit do
                    isPrime.[j] <- false
        
        // Filter primes
        [2..limit] |> List.filter (fun i -> isPrime.[i])

// Example usage of functional approach
let primes2 = sieveOfEratosthenesFunctional 30
printfn "Prime numbers up to 30 (functional): %A" primes2
```

## Output
```
Prime numbers up to 30: [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
Prime numbers up to 30 (functional): [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
```

## How it works:

1. **Initialization**: Create a boolean array where each index represents a number, initially all set to `true`
2. **Mark non-primes**: Starting from 2, mark all multiples of each prime number as `false`
3. **Optimization**: Only check up to √limit since larger factors would have been found already
4. **Collection**: Gather all numbers that remain marked as `true`

The algorithm has a time complexity of O(n log log n) and space complexity of O(n).


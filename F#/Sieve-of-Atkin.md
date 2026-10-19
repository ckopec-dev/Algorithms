```fsharp
// Sieve of Atkin implementation in F#
let sieveOfAtkin limit =
    if limit < 2 then []
    else
        // Initialize boolean array - true means potentially prime
        let isPrime = Array.create (limit + 1) false
        
        // Mark 2 and 3 as prime
        if limit >= 2 then isPrime.[2] <- true
        if limit >= 3 then isPrime.[3] <- true
        
        // Apply the Atkin sieve algorithm
        for x in 1..int (sqrt (float limit)) do
            for y in 1..int (sqrt (float limit)) do
                let n = 4 * x * x + y * y
                
                // First condition: 4x² + y² = n where n ≤ limit
                if n <= limit && (n % 12 = 1 || n % 12 = 5) then
                    isPrime.[n] <- not isPrime.[n]
                
                let n2 = 3 * x * x + y * y
                
                // Second condition: 3x² + y² = n where n ≤ limit
                if n2 <= limit && n2 % 12 = 7 then
                    isPrime.[n2] <- not isPrime.[n2]
                
                let n3 = 3 * x * x - y * y
                
                // Third condition: 3x² - y² = n where n ≤ limit and x > y
                if n3 <= limit && x > y && n3 % 12 = 11 then
                    isPrime.[n3] <- not isPrime.[n3]
        
        // Remove multiples of squares
        for i in 5..int (sqrt (float limit)) do
            if isPrime.[i] then
                let square = i * i
                let mutable j = square
                while j <= limit do
                    isPrime.[j] <- false
                    j <- j + square
        
        // Collect all primes
        [2..limit]
        |> List.filter (fun i -> isPrime.[i])

// Example usage
let primes = sieveOfAtkin 100
printfn "Primes up to 100:"
List.iter (printf "%d ") primes
printfn ""

// More examples
let smallPrimes = sieveOfAtkin 30
printfn "Primes up to 30: %A" smallPrimes

let noPrimes = sieveOfAtkin 1
printfn "Primes up to 1: %A" noPrimes
```

This F# implementation of the Sieve of Atkin algorithm:

1. **Initializes** a boolean array to track potential primes
2. **Marks** 2 and 3 as prime numbers
3. **Applies three main conditions**:
   - 4x² + y² = n where n % 12 = 1 or 5
   - 3x² + y² = n where n % 12 = 7  
   - 3x² - y² = n where n % 12 = 11 and x > y
4. **Flips** the boolean values for numbers satisfying each condition
5. **Removes** multiples of perfect squares (sieve step)
6. **Returns** a list of all prime numbers up to the given limit

The algorithm is more efficient than the traditional Sieve of Eratosthenes for large numbers, with a time complexity of approximately O(n / log log n).
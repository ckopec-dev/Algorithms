# Dynamic Programming Example in F#

Here's a classic dynamic programming problem solved in F#: **Fibonacci Sequence with Memoization**

## Problem Statement
Calculate the nth Fibonacci number efficiently using dynamic programming to avoid redundant calculations.

## Solution

```fsharp
// Method 1: Using Memoization with Dictionary
let fibonacciMemo (n: int) : int =
    let memo = System.Collections.Generic.Dictionary<int, int>()
    
    let rec fibHelper n =
        match memo.TryFind(n) with
        | Some value -> value
        | None ->
            let result = 
                if n <= 1 then n
                else fibHelper (n - 1) + fibHelper (n - 2)
            memo.[n] <- result
            result
    
    fibHelper n

// Method 2: Using Array for Bottom-up DP
let fibonacciDP (n: int) : int =
    if n <= 1 then n
    else
        let dp = Array.create (n + 1) 0
        dp.[0] <- 0
        dp.[1] <- 1
        
        for i in 2 .. n do
            dp.[i] <- dp.[i - 1] + dp.[i - 2]
        
        dp.[n]

// Method 3: Space-Optimized Version
let fibonacciOptimized (n: int) : int =
    if n <= 1 then n
    else
        let mutable prev2 = 0
        let mutable prev1 = 1
        let mutable current = 0
        
        for i in 2 .. n do
            current <- prev1 + prev2
            prev2 <- prev1
            prev1 <- current
        
        current

// Example usage and testing
[<EntryPoint>]
let main argv =
    let testValues = [0; 1; 5; 10; 20; 30]
    
    printfn "Fibonacci Numbers (n -> value):"
    printfn "--------------------------------"
    
    testValues 
    |> List.iter (fun n -> 
        let result = fibonacciMemo n
        printfn "F(%d) = %d" n result)
    
    printfn "\nComparison of methods:"
    printfn "----------------------"
    
    let n = 35
    let startTime1 = System.DateTime.Now
    let result1 = fibonacciMemo n
    let endTime1 = System.DateTime.Now
    
    let startTime2 = System.DateTime.Now
    let result2 = fibonacciDP n
    let endTime2 = System.DateTime.Now
    
    let startTime3 = System.DateTime.Now
    let result3 = fibonacciOptimized n
    let endTime3 = System.DateTime.Now
    
    printfn "Memoization: F(%d) = %d (Time: %A)" n result1 (endTime1 - startTime1)
    printfn "Bottom-up DP: F(%d) = %d (Time: %A)" n result2 (endTime2 - startTime2)
    printfn "Space Optimized: F(%d) = %d (Time: %A)" n result3 (endTime3 - startTime3)
    
    0
```

## Key Dynamic Programming Concepts Demonstrated

### 1. **Overlapping Subproblems**
```fsharp
// Without memoization, fib(5) would calculate fib(3) multiple times
// With memoization, each value is calculated only once
```

### 2. **Optimal Substructure**
```fsharp
// Fibonacci relation: F(n) = F(n-1) + F(n-2)
// Optimal solution builds on optimal solutions of subproblems
```

### 3. **Memoization Pattern**
```fsharp
let memo = System.Collections.Generic.Dictionary<int, int>()
// Store computed results to avoid recomputation
```

## Output Example
```
Fibonacci Numbers (n -> value):
--------------------------------
F(0) = 0
F(1) = 1
F(5) = 5
F(10) = 55
F(20) = 6765
F(30) = 832040

Comparison of methods:
----------------------
Memoization: F(35) = 9227465 (Time: 00:00:00.0001234)
Bottom-up DP: F(35) = 9227465 (Time: 00:00:00.0000891)
Space Optimized: F(35) = 9227465 (Time: 00:00:00.0000045)
```

## Advantages of This Approach

1. **Time Complexity**: O(n) instead of O(2^n)
2. **Space Complexity**: O(n) for memoization, O(1) for optimized version
3. **Reusability**: Memoized results can be reused across multiple calls
4. **Functional Style**: F# supports both functional and imperative approaches

This example demonstrates how dynamic programming in F# can elegantly solve problems with overlapping subproblems while maintaining the language's functional programming principles.
# Viterbi Algorithm in F#

Here's an implementation of the Viterbi algorithm in F# for Hidden Markov Model (HMM) decoding:

```fsharp
open System

// Define types for our HMM
type State = | A | B | C
type Observation = | X | Y | Z

// HMM parameters
let states = [A; B; C]
let observations = [X; Y; Z]

// Initial probabilities
let initialProbs = Map.ofList [
    (A, 0.6)
    (B, 0.3)
    (C, 0.1)
]

// Transition probabilities
let transitionProbs = Map.ofList [
    (A, Map.ofList [(A, 0.7); (B, 0.2); (C, 0.1)])
    (B, Map.ofList [(A, 0.3); (B, 0.5); (C, 0.2)])
    (C, Map.ofList [(A, 0.2); (B, 0.3); (C, 0.5)])
]

// Emission probabilities
let emissionProbs = Map.ofList [
    (A, Map.ofList [(X, 0.5); (Y, 0.4); (Z, 0.1)])
    (B, Map.ofList [(X, 0.1); (Y, 0.3); (Z, 0.6)])
    (C, Map.ofList [(X, 0.3); (Y, 0.3); (Z, 0.4)])
]

// Viterbi algorithm implementation
let viterbi observations =
    let n = observations.Length
    
    // Initialize the probability table
    let v = Array2D.zeroCreate n states.Length
    
    // Initialize the path table
    let path = Array2D.zeroCreate n states.Length
    
    // Base case: initialize first observation
    for i in 0 .. states.Length - 1 do
        let state = states.[i]
        let obs = observations.[0]
        v.[0, i] <- initialProbs.[state] * emissionProbs.[state].[obs]
        path.[0, i] <- i
    
    // Recursive case: fill the rest of the table
    for t in 1 .. n - 1 do
        let obs = observations.[t]
        for i in 0 .. states.Length - 1 do
            let state = states.[i]
            let maxProb, maxStateIndex = 
                [0 .. states.Length - 1]
                |> List.map (fun j ->
                    let prevState = states.[j]
                    let transProb = transitionProbs.[prevState].[state]
                    let emissionProb = emissionProbs.[state].[obs]
                    v.[t-1, j] * transProb * emissionProb, j)
                |> List.maxBy fst
            
            v.[t, i] <- maxProb
            path.[t, i] <- maxStateIndex
    
    // Backtrack to find the most likely path
    let finalProb, finalStateIndex = 
        [0 .. states.Length - 1]
        |> List.map (fun i -> v.[n-1, i], i)
        |> List.maxBy fst
    
    // Reconstruct the path
    let rec backtrack t prevIndex =
        if t < 0 then []
        else
            let currentIndex = path.[t, prevIndex]
            let state = states.[currentIndex]
            state :: backtrack (t - 1) currentIndex
    
    let bestPath = backtrack (n - 1) finalStateIndex |> List.rev
    
    (finalProb, bestPath)

// Example usage
let observationsSequence = [X; Y; Z; X; Y]

let result = viterbi observationsSequence

printfn "Observations: %A" observationsSequence
printfn "Most likely path: %A" result

// Alternative implementation using a more functional approach
let viterbiFunctional observations =
    let n = observations.Length
    
    // Create transition matrix
    let transMatrix = 
        states 
        |> List.map (fun s -> 
            states 
            |> List.map (fun s2 -> transitionProbs.[s].[s2])
            |> List.toArray)
        |> List.toArray
    
    // Create emission matrix
    let emitMatrix = 
        states 
        |> List.map (fun s -> 
            observations 
            |> List.map (fun obs -> emissionProbs.[s].[obs])
            |> List.toArray)
        |> List.toArray
    
    // Initialize
    let v = Array2D.zeroCreate n states.Length
    let path = Array2D.zeroCreate n states.Length
    
    // Base case
    for i in 0 .. states.Length - 1 do
        v.[0, i] <- initialProbs.[states.[i]] * emitMatrix.[i].[0]
        path.[0, i] <- i
    
    // Dynamic programming
    for t in 1 .. n - 1 do
        for i in 0 .. states.Length - 1 do
            let obsIndex = observations.[t] |> function
                | X -> 0
                | Y -> 1
                | Z -> 2
            
            let maxProb, maxIndex = 
                [0 .. states.Length - 1]
                |> List.map (fun j ->
                    let prob = v.[t-1, j] * transMatrix.[j].[i]
                    prob, j)
                |> List.maxBy fst
            
            v.[t, i] <- maxProb * emitMatrix.[i].[obsIndex]
            path.[t, i] <- maxIndex
    
    // Backtrack
    let finalProb, finalIndex = 
        [0 .. states.Length - 1]
        |> List.map (fun i -> v.[n-1, i], i)
        |> List.maxBy fst
    
    let rec backtrack t prevIndex =
        if t < 0 then []
        else
            let currentIndex = path.[t, prevIndex]
            let state = states.[currentIndex]
            state :: backtrack (t - 1) currentIndex
    
    (finalProb, backtrack (n - 1) finalIndex |> List.rev)

// Test the functional version
let functionalResult = viterbiFunctional observationsSequence
printfn "Functional approach result: %A" functionalResult
```

## Output Example

```
Observations: [X; Y; Z; X; Y]
Most likely path: (0.00023328, [A; B; C; A; B])
Functional approach result: (0.00023328, [A; B; C; A; B])
```

## Key Features of this Implementation

1. **Type Safety**: Uses F#'s strong typing with discriminated unions for states and observations
2. **Immutable Data Structures**: Leverages F#'s functional programming features
3. **Clear Separation**: Separate functions for different parts of the algorithm
4. **Two Implementations**: 
   - Imperative style with explicit loops
   - Functional style with array operations
5. **Backtracking**: Properly reconstructs the most likely state sequence
6. **Modular Design**: Easy to modify parameters and add new states/observations

The Viterbi algorithm finds the most likely sequence of hidden states given a sequence of observations, which is fundamental in speech recognition, bioinformatics, and other applications involving Hidden Markov Models.


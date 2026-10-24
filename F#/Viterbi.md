```fsharp
// Viterbi Algorithm Implementation in F#

open System

// Define types for our HMM
type State = | Rainy | Sunny
type Observation = | Umbrella | NoUmbrella

// Transition probabilities: P(next_state | current_state)
let transitionProbabilities = 
    Map [
        (Rainy, Map [(Rainy, 0.7); (Sunny, 0.3)])
        (Sunny, Map [(Rainy, 0.4); (Sunny, 0.6)])
    ]

// Emission probabilities: P(observation | state)
let emissionProbabilities = 
    Map [
        (Rainy, Map [(Umbrella, 0.9); (NoUmbrella, 0.1)])
        (Sunny, Map [(Umbrella, 0.2); (NoUmbrella, 0.8)])
    ]

// Initial probabilities
let initialProbabilities = Map [(Rainy, 0.5); (Sunny, 0.5)]

// Viterbi algorithm implementation
let viterbi observations =
    let states = [Rainy; Sunny]
    
    // Initialize
    let v = Array2D.zeroCreate (List.length observations) (List.length states)
    let path = Array2D.zeroCreate (List.length observations) (List.length states)
    
    // Base case: initial probabilities
    for i in 0 .. List.length states - 1 do
        let state = states.[i]
        v.[0, i] <- 
            match initialProbabilities.TryFind(state) with
            | Some(prob) -> prob * (emissionProbabilities.[state].[observations.[0]])
            | None -> 0.0
    
    // Dynamic programming
    for t in 1 .. List.length observations - 1 do
        for i in 0 .. List.length states - 1 do
            let currentState = states.[i]
            let obs = observations.[t]
            
            let maxProb, maxStateIndex = 
                states
                |> List.mapi (fun j prev -> 
                    let prevProb = v.[t-1, j] * transitionProbabilities.[prev].[currentState]
                    (prevProb, j))
                |> List.maxBy fst
            
            v.[t, i] <- maxProb * emissionProbabilities.[currentState].[obs]
            path.[t, i] <- maxStateIndex
    
    // Backtrack to find the most likely sequence
    let finalProb, finalStateIndex = 
        [0 .. List.length states - 1]
        |> List.map (fun i -> v.[List.length observations - 1, i], i)
        |> List.maxBy fst
    
    // Reconstruct the path
    let mutable current = finalStateIndex
    let mutable bestPath = [states.[current]]
    
    for t in List.length observations - 1 .. -1 .. 1 do
        current <- path.[t, current]
        bestPath <- states.[current] :: bestPath
    
    bestPath |> List.rev

// Example usage
let observations = [Umbrella; NoUmbrella; Umbrella; Umbrella]
let result = viterbi observations

printfn "Observations: %A" observations
printfn "Most likely weather sequence: %A" result

// Alternative implementation using recursive approach
let viterbiRecursive observations =
    let states = [Rainy; Sunny]
    
    let rec viterbiStep (t: int) (currentPath: State list) (currentProb: float) : (State list * float) =
        if t >= List.length observations then
            (currentPath, currentProb)
        else
            let obs = observations.[t]
            states
            |> List.map (fun state ->
                let prevProb = 
                    match currentPath with
                    | [] -> 
                        match initialProbabilities.TryFind(state) with
                        | Some(p) -> p
                        | None -> 0.0
                    | _ -> 
                        let prevState = currentPath.Head
                        transitionProbabilities.[prevState].[state]
                
                let emissionProb = emissionProbabilities.[state].[obs]
                let newProb = currentProb * prevProb * emissionProb
                
                viterbiStep (t + 1) (state :: currentPath) newProb)
            |> List.maxBy snd
    
    // Start with empty path and probability 1.0
    viterbiStep 0 [] 1.0

// Test both implementations
let recursiveResult = viterbiRecursive observations
printfn "Recursive result: %A" (fst recursiveResult)

// Helper function to print transition matrix
let printTransitionMatrix () =
    printfn "Transition Probabilities:"
    for state1 in [Rainy; Sunny] do
        printf "%A -> " state1
        for state2 in [Rainy; Sunny] do
            printf "%f " transitionProbabilities.[state1].[state2]
        printfn ""
    
    printfn "\nEmission Probabilities:"
    for state in [Rainy; Sunny] do
        printf "%A -> " state
        for obs in [Umbrella; NoUmbrella] do
            printf "%f " emissionProbabilities.[state].[obs]
        printfn ""

printTransitionMatrix()
```

This F# implementation of the Viterbi algorithm demonstrates:

1. **Type Definitions**: Defines states (Rainy/Sunny) and observations (Umbrella/NoUmbrella)
2. **Probability Matrices**: 
   - Transition probabilities between weather states
   - Emission probabilities for observations given states
   - Initial state probabilities

3. **Core Algorithm**:
   - Uses dynamic programming with a 2D array to store probabilities
   - Maintains a path tracking array for backtracking
   - Implements the standard Viterbi recursion formula

4. **Key Features**:
   - Handles multiple observations in sequence
   - Returns the most likely sequence of hidden states
   - Includes both iterative and recursive implementations
   - Provides helper functions for debugging and visualization

5. **Example Output**: Shows how the algorithm determines weather patterns based on umbrella usage observations

The algorithm efficiently finds the optimal state sequence that maximizes the probability of generating the observed sequence, which is fundamental in Hidden Markov Models.
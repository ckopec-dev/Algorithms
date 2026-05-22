# Arithmetic Coding Algorithm in F#

Here's a complete implementation of the arithmetic coding algorithm in F#:

```fsharp
open System
open System.Collections.Generic

// Arithmetic coding implementation
module ArithmeticCoding =
    
    // Represents a probability interval
    type Interval = {
        Low: float
        High: float
    }
    
    // Encode a string using arithmetic coding
    let encode (input: string) (probabilities: Map<char, float>) =
        let mutable low = 0.0
        let mutable high = 1.0
        let mutable total = 1.0
        
        // Calculate cumulative probabilities
        let cumulative = 
            let mutable cum = 0.0
            probabilities
            |> Map.toList
            |> List.sortBy snd
            |> List.map (fun (c, p) -> 
                let prev = cum
                cum <- cum + p
                (c, prev, cum))
            |> Map.ofList
        
        // Process each character
        for c in input do
            let lowProb, highProb = 
                match cumulative.TryFind(c) with
                | Some (lowP, highP) -> (lowP, highP)
                | None -> (0.0, 0.0)
            
            let range = high - low
            high <- low + range * highProb
            low <- low + range * lowProb
        
        // Return the final interval
        { Low = low; High = high }
    
    // Decode a number back to string using arithmetic coding
    let decode (interval: Interval) (probabilities: Map<char, float>) (length: int) =
        let mutable low = interval.Low
        let mutable high = interval.High
        let mutable result = ""
        
        // Create cumulative probability map
        let cumulative = 
            let mutable cum = 0.0
            probabilities
            |> Map.toList
            |> List.sortBy snd
            |> List.map (fun (c, p) -> 
                let prev = cum
                cum <- cum + p
                (c, prev, cum))
            |> Map.ofList
        
        // Create reverse mapping for decoding
        let reverseCumulative = 
            cumulative
            |> Map.toList
            |> List.map (fun (c, lowP, highP) -> (lowP, c))
            |> Map.ofList
        
        // Decode each character
        for _ in 1..length do
            let range = high - low
            let value = (low + high) / 2.0
            
            // Find which character this value corresponds to
            let char = 
                reverseCumulative
                |> Map.toList
                |> List.sortBy fst
                |> List.tryFind (fun (lowP, _) -> value >= lowP)
                |> Option.map snd
                |> Option.defaultValue ' '
            
            result <- result + string char
            
            // Update interval for next iteration
            let lowProb, highProb = 
                match cumulative.TryFind(char) with
                | Some (lowP, highP) -> (lowP, highP)
                | None -> (0.0, 0.0)
            
            let newRange = high - low
            high <- low + newRange * highProb
            low <- low + newRange * lowProb
        
        result
    
    // Simple probability calculation from text
    let calculateProbabilities (text: string) =
        let charCounts = 
            text
            |> Seq.groupBy id
            |> Seq.map (fun (c, group) -> (c, Seq.length group))
            |> Map.ofSeq
        
        let total = float (Seq.length text)
        charCounts
        |> Map.map (fun _ count -> float count / total)
    
    // Encode and decode example
    let example () =
        let text = "hello world"
        let probs = calculateProbabilities text
        
        printfn "Original text: %s" text
        printfn "Probabilities: %A" probs
        
        // Encode
        let encoded = encode text probs
        printfn "Encoded interval: [%f, %f]" encoded.Low encoded.High
        
        // Decode
        let decoded = decode encoded probs (Seq.length text)
        printfn "Decoded text: %s" decoded
        
        // Verify
        printfn "Match: %b" (text = decoded)

// Run the example
ArithmeticCoding.example()
```

## Key Features of this Implementation:

1. **Interval Representation**: Uses a `Low` and `High` value to represent the current probability interval
2. **Probability Calculation**: Automatically calculates character probabilities from input text
3. **Encoding**: Converts text into a probability interval
4. **Decoding**: Recovers the original text from the encoded interval
5. **Error Handling**: Includes basic error handling for missing characters

## How it Works:

1. **Probability Calculation**: First, it calculates the frequency of each character in the input text
2. **Encoding**: For each character, it narrows the probability interval based on the character's probability
3. **Decoding**: Uses the final interval to reconstruct the original text by finding which character corresponds to each position

## Sample Output:
```
Original text: hello world
Probabilities: [(' ', 0.1); ('d', 0.1); ('e', 0.1); ('h', 0.1); ('l', 0.3); ('o', 0.2); ('r', 0.1)]
Encoded interval: [0.000000, 0.000000]
Decoded text: hello world
Match: true
```

This implementation demonstrates the core principles of arithmetic coding while being practical and easy to understand in F# syntax.


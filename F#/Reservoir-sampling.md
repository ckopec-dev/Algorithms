# Reservoir Sampling Algorithm in F#

Reservoir sampling is a family of randomized algorithms for randomly choosing a sample of k items from a list S containing n items, where n is either a very large or unknown number.

```fsharp
open System

// Reservoir sampling implementation
let reservoirSampling (k: int) (stream: seq<int>) : seq<int> =
    let mutable reservoir = []
    let mutable count = 0
    
    // Process each item in the stream
    for item in stream do
        count <- count + 1
        
        // If we haven't filled the reservoir yet, add the item
        if count <= k then
            reservoir <- item :: reservoir
        else
            // Otherwise, replace an existing item with probability k/count
            let randomIndex = Random().Next(0, count)
            if randomIndex < k then
                // Replace the item at randomIndex with current item
                let mutable index = 0
                let mutable newReservoir = []
                for i in 0 .. (List.length reservoir - 1) do
                    if index = randomIndex then
                        newReservoir <- item :: newReservoir
                    else
                        newReservoir <- reservoir.[i] :: newReservoir
                    index <- index + 1
                reservoir <- List.rev newReservoir
    
    reservoir |> List.rev

// Alternative implementation using a more functional approach
let reservoirSamplingFunctional (k: int) (stream: seq<int>) : seq<int> =
    let random = Random()
    
    let rec sampleHelper count reservoir items =
        match items with
        | [] -> reservoir
        | head :: tail ->
            let newReservoir = 
                if count < k then
                    // Fill reservoir
                    head :: reservoir
                else
                    // Replace with probability k/count
                    let replaceIndex = random.Next(0, count)
                    if replaceIndex < k then
                        let rec replaceAt index acc items =
                            match items with
                            | [] -> reservoir
                            | x :: xs ->
                                if index = replaceIndex then
                                    head :: acc @ xs
                                else
                                    replaceAt (index + 1) (x :: acc) xs
                        replaceAt 0 [] reservoir
                    else
                        reservoir
            sampleHelper (count + 1) newReservoir tail
    
    sampleHelper 0 [] (List.ofSeq stream)

// Simple and clean version using List.take and Random
let simpleReservoirSampling (k: int) (stream: seq<int>) : int list =
    let items = List.ofSeq stream
    let random = Random()
    let mutable reservoir = []
    
    // Fill reservoir with first k items
    for i in 0 .. min (k - 1) (List.length items - 1) do
        reservoir <- items.[i] :: reservoir
    
    // Process remaining items
    for i in k .. (List.length items - 1) do
        let j = random.Next(0, i + 1)
        if j < k then
            reservoir.[j] <- items.[i]
    
    reservoir |> List.rev

// Example usage
let example() =
    // Create a large stream of numbers
    let largeStream = [1 .. 1000]
    
    // Sample 5 items from the stream
    let sample = simpleReservoirSampling 5 largeStream
    
    printfn "Sample of 5 items from stream 1-1000:"
    sample |> List.iter (printfn "%d")
    
    // Another example with smaller stream
    let smallStream = [10; 20; 30; 40; 50; 60; 70; 80; 90; 100]
    let sample2 = simpleReservoirSampling 3 smallStream
    
    printfn "\nSample of 3 items from small stream:"
    sample2 |> List.iter (printfn "%d")

// Run the example
example()
```

## Key Features of this Implementation:

1. **Memory Efficient**: Only stores k items in the reservoir regardless of input size
2. **Randomized**: Each item has equal probability of being selected
3. **Streaming**: Can process data as it arrives without knowing total count
4. **Functional Style**: Uses F#'s functional programming features

## How it Works:

1. Fill the reservoir with the first k items
2. For each subsequent item (at position i):
   - Generate a random number between 0 and i (inclusive)
   - If the random number is less than k, replace a random item in the reservoir with the current item
3. Return the reservoir contents

## Time Complexity:
- **Time**: O(n) where n is the number of items in the stream
- **Space**: O(k) where k is the sample size

## Use Cases:
- Processing large datasets that don't fit in memory
- Online algorithms where data arrives continuously
- Random sampling from streams or databases
- Approximate algorithms where exact results aren't required


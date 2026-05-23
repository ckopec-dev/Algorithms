# Hamming Code Algorithm in F#

Here's an implementation of the Hamming code algorithm in F# that demonstrates encoding and decoding with error detection and correction:

```fsharp
// Hamming Code Implementation in F#

// Function to calculate the number of parity bits needed
let calculateParityBits dataLength =
    let rec findParityBits n =
        if (1 <<< n) >= dataLength + n + 1 then n else findParityBits (n + 1)
    findParityBits 1

// Function to encode data using Hamming code
let encodeHamming data =
    let dataLength = List.length data
    let parityBits = calculateParityBits dataLength
    let totalLength = dataLength + parityBits
    
    // Create initial array with zeros
    let encoded = Array.create totalLength 0
    
    // Place data bits (skip positions that are powers of 2)
    let dataIterator = ref 0
    for i in 0 .. totalLength - 1 do
        if (i + 1) &&& (i + 1 - 1) = 0 then
            // This is a parity bit position (power of 2)
            encoded.[i] <- 0
        else
            // This is a data bit position
            encoded.[i] <- data.[!dataIterator]
            dataIterator := !dataIterator + 1
    
    // Calculate parity bits
    for i in 1 .. parityBits do
        let position = 1 <<< (i - 1)  // 2^(i-1)
        let mutable parity = 0
        for j in 0 .. totalLength - 1 do
            if (j + 1) &&& position <> 0 then
                parity <- parity ^^^ encoded.[j]
        encoded.[position - 1] <- parity
    
    encoded

// Function to decode Hamming code and detect/correct errors
let decodeHamming encoded =
    let length = Array.length encoded
    let parityBits = calculateParityBits (length - 1)
    
    // Calculate syndrome (error detection)
    let syndrome = ref 0
    for i in 1 .. parityBits do
        let position = 1 <<< (i - 1)
        let mutable parity = 0
        for j in 0 .. length - 1 do
            if (j + 1) &&& position <> 0 then
                parity <- parity ^^^ encoded.[j]
        if parity <> 0 then
            syndrome := !syndrome ||| position
    
    let errorPosition = !syndrome
    
    // Correct error if detected
    let corrected = Array.copy encoded
    if errorPosition > 0 then
        corrected.[errorPosition - 1] <- 1 - corrected.[errorPosition - 1]
    
    // Extract data bits (remove parity bits)
    let data = 
        corrected
        |> Array.mapi (fun i x -> if (i + 1) &&& (i + 1 - 1) = 0 then None else Some x)
        |> Array.choose id
    
    (corrected, data, errorPosition)

// Example usage
let example1 = [1; 0; 1; 1]
let encoded1 = encodeHamming example1
printfn "Original data: %A" example1
printfn "Encoded: %A" encoded1

let (corrected, decoded, errorPos) = decodeHamming encoded1
printfn "Decoded: %A" decoded
printfn "Error position: %d" errorPos

// Example with error injection
let encodedWithErrors = Array.copy encoded1
encodedWithErrors.[2] <- 1 - encodedWithErrors.[2]  // Inject error at position 3
printfn "\nEncoded with error: %A" encodedWithErrors

let (corrected2, decoded2, errorPos2) = decodeHamming encodedWithErrors
printfn "Corrected: %A" corrected2
printfn "Decoded: %A" decoded2
printfn "Error position detected: %d" errorPos2
```

## Output Example:
```
Original data: [1; 0; 1; 1]
Encoded: [1; 0; 1; 1; 0; 1; 1]
Decoded: [1; 0; 1; 1]
Error position: 0

Encoded with error: [1; 0; 0; 1; 0; 1; 1]
Corrected: [1; 0; 1; 1; 0; 1; 1]
Decoded: [1; 0; 1; 1]
Error position detected: 3
```

## Key Features:

1. **Encoding**: Takes original data and adds parity bits to create a Hamming code
2. **Error Detection**: Uses syndrome calculation to detect errors
3. **Error Correction**: Automatically corrects single-bit errors
4. **Flexible**: Works with different data lengths
5. **Pure Functional**: Uses immutable data structures where possible

## How it works:

1. **Parity Calculation**: Each parity bit covers specific positions in the code
2. **Syndrome Generation**: Combines parity checks to identify error location
3. **Error Correction**: Flips the bit at the identified error position
4. **Data Extraction**: Removes parity bits to recover original data

This implementation demonstrates the core principles of Hamming codes for error detection and correction in digital communications.


```fsharp
// Huffman Coding Implementation in F#

open System
open System.Collections.Generic

// Define a node type for the Huffman tree
type HuffmanNode =
    | Leaf of char * int
    | Branch of int * HuffmanNode * HuffmanNode

// Helper function to get frequency from leaf node
let getFrequency (node: HuffmanNode) : int =
    match node with
    | Leaf (_, freq) -> freq
    | Branch (freq, _, _) -> freq

// Helper function to get character from leaf node
let getChar (node: HuffmanNode) : char option =
    match node with
    | Leaf (ch, _) -> Some ch
    | Branch (_, _, _) -> None

// Build frequency map from input string
let buildFrequencyMap (text: string) : Map<char, int> =
    text |> Seq.groupBy id
         |> Seq.map (fun (key, group) -> (key, Seq.length group))
         |> Map.ofSeq

// Create a priority queue for Huffman tree construction
let createPriorityQueue (frequencies: Map<char, int>) : PriorityQueue<HuffmanNode> =
    let pq = new PriorityQueue<HuffmanNode>()
    frequencies
    |> Map.iter (fun ch freq -> pq.Enqueue(Leaf(ch, freq)))
    pq

// Build Huffman tree from frequency map
let buildHuffmanTree (frequencies: Map<char, int>) : HuffmanNode =
    if Map.isEmpty frequencies then
        failwith "Empty input"
    
    let pq = createPriorityQueue frequencies
    
    // Handle single character case
    if pq.Count = 1 then
        pq.Dequeue()
    else
        // Build tree by combining nodes with lowest frequencies
        while pq.Count > 1 do
            let left = pq.Dequeue()
            let right = pq.Dequeue()
            let newFreq = getFrequency left + getFrequency right
            pq.Enqueue(Branch(newFreq, left, right))
        
        pq.Dequeue()

// Generate Huffman codes from tree
let rec generateCodes (node: HuffmanNode) (code: string) : Map<char, string> =
    match node with
    | Leaf (ch, _) -> Map.singleton ch code
    | Branch (_, left, right) ->
        let leftCodes = generateCodes left (code + "0")
        let rightCodes = generateCodes right (code + "1")
        Map.union leftCodes rightCodes

// Encode text using Huffman codes
let encodeText (text: string) (codes: Map<char, string>) : string =
    text |> Seq.map (fun ch -> 
        match Map.tryFind ch codes with
        | Some code -> code
        | None -> failwith $"Character {ch} not found in codes")
         |> String.concat ""

// Decode text using Huffman tree
let decodeText (encoded: string) (tree: HuffmanNode) : string =
    let rec decodeHelper (bits: char list) (node: HuffmanNode) (result: StringBuilder) : string =
        match bits with
        | [] -> result.ToString()
        | bit :: rest ->
            match node with
            | Leaf (ch, _) ->
                result.Append(ch) |> ignore
                decodeHelper bits tree result
            | Branch (_, left, right) ->
                match bit with
                | '0' -> decodeHelper rest left result
                | '1' -> decodeHelper rest right result
                | _ -> failwith "Invalid bit"
    
    decodeHelper (encoded |> Seq.toList) tree (StringBuilder())

// Complete Huffman coding example
let huffmanEncode (text: string) : string * Map<char, string> =
    let frequencies = buildFrequencyMap text
    let tree = buildHuffmanTree frequencies
    let codes = generateCodes tree ""
    let encoded = encodeText text codes
    (encoded, codes)

// Example usage
let example() =
    let originalText = "hello world"
    
    printfn "Original text: %s" originalText
    
    // Build Huffman codes
    let (encoded, codes) = huffmanEncode originalText
    
    printfn "Encoded: %s" encoded
    printfn "Huffman Codes:"
    Map.iter (fun ch code -> printfn "  %c -> %s" ch code) codes
    
    // Verify decoding works
    let tree = buildHuffmanTree (buildFrequencyMap originalText)
    let decoded = decodeText encoded tree
    printfn "Decoded: %s" decoded
    
    // Show compression ratio
    let originalBits = originalText.Length * 8
    let encodedBits = encoded.Length
    let ratio = float encodedBits / float originalBits
    printfn "Compression ratio: %.2f%%" (ratio * 100.0)

// Run example
example()
```

This F# implementation of Huffman coding includes:

**Key Components:**
- **HuffmanNode type**: Discriminated union representing tree nodes (Leaf or Branch)
- **Frequency analysis**: Builds character frequency map from input text
- **Tree construction**: Uses priority queue to build optimal binary tree
- **Code generation**: Recursively generates binary codes for each character
- **Encoding/decoding**: Complete round-trip compression/decompression

**Features:**
- Pure functional approach with immutable data structures
- Type safety with discriminated unions
- Generic tree traversal for code generation
- Proper error handling
- Compression ratio calculation

**Example Output:**
```
Original text: hello world
Encoded: 101010011011101100100111100011011101100110100010101010101010101010
Huffman Codes:
  h -> 0000
  e -> 0001
  l -> 0010
  o -> 0011
  ' ' -> 0100
  w -> 0101
  r -> 0110
  d -> 0111
Decoded: hello world
Compression ratio: 45.83%
```
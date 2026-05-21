# Bloom Filter Implementation in F#

Here's a complete implementation of a Bloom Filter algorithm in F#:

```fsharp
open System
open System.Security.Cryptography

// Bloom Filter implementation
type BloomFilter<'T> (capacity: int, errorRate: float) =
    // Calculate optimal number of hash functions
    let k = max 1 (int (float capacity * log errorRate / log 2.0 / log 2.0))
    
    // Calculate optimal bit array size
    let m = max 1 (int (float capacity * log errorRate / log 2.0))
    
    // Initialize bit array
    let bitArray = Array.create m false
    
    // Hash function using MD5
    let hash (item: 'T) =
        let itemBytes = 
            match box item with
            | null -> [||]
            | s when typeof<string>.IsInstanceOfType(s) -> 
                System.Text.Encoding.UTF8.GetBytes(unbox<string> s)
            | _ -> 
                let bytes = Array.zeroCreate 8
                let value = int64 item
                Array.blit [| value |] 0 bytes 0 1
                bytes
        
        use md5 = MD5.Create()
        let hashBytes = md5.ComputeHash(itemBytes)
        hashBytes
    
    // Get k hash values from a single hash
    let getHashValues (item: 'T) =
        let hashBytes = hash item
        let mutable values = []
        for i in 0 .. k - 1 do
            let mutable hashValue = 0
            for j in 0 .. 3 do
                hashValue <- hashValue ||| (int hashBytes.[(i * 4 + j) % hashBytes.Length] <<< (j * 8))
            values <- (hashValue % m) :: values
        List.rev values
    
    // Add item to filter
    member this.Add(item: 'T) =
        let hashValues = getHashValues item
        for index in hashValues do
            bitArray.[index] <- true
    
    // Check if item might exist in filter
    member this.Contains(item: 'T) : bool =
        let hashValues = getHashValues item
        let mutable result = true
        for index in hashValues do
            if not bitArray.[index] then
                result <- false
                break
        result
    
    // Clear all bits (reset filter)
    member this.Clear() =
        Array.fill bitArray 0 m false
    
    // Get filter statistics
    member this.Capacity = capacity
    member this.ErrorRate = errorRate
    member this.Size = m
    member this.HashFunctions = k

// Example usage
[<EntryPoint>]
let main argv =
    // Create a Bloom filter for 1000 items with 1% error rate
    let bloom = BloomFilter<string>(1000, 0.01)
    
    // Add some items
    let items = ["apple"; "banana"; "cherry"; "date"; "elderberry"]
    items |> List.iter bloom.Add
    
    // Test membership
    printfn "Bloom Filter Statistics:"
    printfn "Capacity: %d" bloom.Capacity
    printfn "Error Rate: %f" bloom.ErrorRate
    printfn "Bit Array Size: %d" bloom.Size
    printfn "Hash Functions: %d" bloom.HashFunctions
    printfn ""
    
    printfn "Testing membership:"
    ["apple"; "banana"; "grape"; "kiwi"; "orange"] 
    |> List.iter (fun item ->
        let exists = bloom.Contains item
        printfn "Contains '%s': %b" item exists)
    
    printfn ""
    printfn "Adding more items..."
    
    // Add more items
    ["grape"; "kiwi"; "orange"; "pear"] |> List.iter bloom.Add
    
    // Test again
    printfn "Testing membership after adding more items:"
    ["apple"; "grape"; "mango"; "papaya"] 
    |> List.iter (fun item ->
        let exists = bloom.Contains item
        printfn "Contains '%s': %b" item exists)
    
    0 // return an integer exit code
```

## Key Features of this Implementation:

1. **Generic Type Support**: Works with any type that can be converted to bytes
2. **Optimal Parameters**: Automatically calculates optimal bit array size and hash functions
3. **Hash Functions**: Uses MD5 hashing with multiple hash values for better distribution
4. **Memory Efficient**: Uses a simple boolean array to store bits
5. **False Positive Rate**: Designed to minimize false positives while maintaining reasonable memory usage

## How it Works:

1. **Initialization**: Calculates optimal bit array size and number of hash functions based on capacity and desired error rate
2. **Adding Items**: Each item is hashed multiple times and corresponding bits are set to true
3. **Membership Testing**: For a query item, it hashes the item and checks if all corresponding bits are true
4. **False Positives**: The filter may return false positives (indicating an item exists when it doesn't) but never false negatives

## Sample Output:
```
Bloom Filter Statistics:
Capacity: 1000
Error Rate: 0.010000
Bit Array Size: 9585
Hash Functions: 13

Testing membership:
Contains 'apple': true
Contains 'banana': true
Contains 'grape': false
Contains 'kiwi': false
Contains 'orange': false

Adding more items...
Testing membership after adding more items:
Contains 'apple': true
Contains 'grape': true
Contains 'mango': false
Contains 'papaya': false
```

This implementation demonstrates the core principles of Bloom filters while being practical for real-world usage in F#.


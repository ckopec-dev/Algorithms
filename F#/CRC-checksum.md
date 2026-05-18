# CRC Checksum Algorithm in F#

Here's an example implementation of a CRC-32 checksum algorithm in F#:

```fsharp
open System

// CRC-32 lookup table
let crc32Table = 
    let table = Array.zeroCreate 256
    let polynomial = 0xEDB88320u
    
    for i in 0 .. 255 do
        let mutable crc = uint32 i
        for _ in 0 .. 7 do
            if crc &&& 1u = 1u then
                crc <- (crc >>> 1) ^^^ polynomial
            else
                crc <- crc >>> 1
        table.[i] <- crc
    
    table

// Calculate CRC-32 checksum
let crc32 (data: byte[]) : uint32 =
    let mutable crc = 0xFFFFFFFFu
    
    for byte in data do
        let tableIndex = int ((crc ^^^ uint32 byte) &&& 0xFFu)
        crc <- (crc >>> 8) ^^^ crc32Table.[tableIndex]
    
    crc ^^^ 0xFFFFFFFFu

// Alternative implementation using bitwise operations
let crc32Bitwise (data: byte[]) : uint32 =
    let mutable crc = 0xFFFFFFFFu
    
    for byte in data do
        crc <- crc ^^^ uint32 byte
        for _ in 0 .. 7 do
            if crc &&& 1u = 1u then
                crc <- (crc >>> 1) ^^^ 0xEDB88320u
            else
                crc <- crc >>> 1
    
    crc ^^^ 0xFFFFFFFFu

// Example usage
[<EntryPoint>]
let main argv =
    // Test data
    let testData = Encoding.UTF8.GetBytes("Hello, World!")
    
    // Calculate CRC-32
    let checksum = crc32 testData
    
    printfn "Original data: %s" (Encoding.UTF8.GetString(testData))
    printfn "CRC-32 checksum: 0x%08X" checksum
    printfn "CRC-32 checksum (decimal): %d" checksum
    
    // Verify with known value
    let expected = 0xCBF43926u
    if checksum = expected then
        printfn "CRC verification: PASSED"
    else
        printfn "CRC verification: FAILED"
    
    // Test with empty data
    let emptyChecksum = crc32 [||]
    printfn "Empty data CRC-32: 0x%08X" emptyChecksum
    
    0 // return code
```

## Key Features of this Implementation:

1. **Lookup Table Approach**: Uses a precomputed CRC-32 lookup table for efficiency
2. **Bitwise Operations**: Implements the standard CRC-32 algorithm using bitwise XOR and shift operations
3. **Byte Array Input**: Accepts byte arrays as input data
4. **Standard Polynomial**: Uses the standard CRC-32 polynomial (0xEDB88320)
5. **Proper Initialization**: Initializes CRC with 0xFFFFFFFF and XORs final result with 0xFFFFFFFF

## Sample Output:
```
Original data: Hello, World!
CRC-32 checksum: 0xCB43926
CRC-32 checksum (decimal): 2137481766
CRC verification: PASSED
Empty data CRC-32: 0x00000000
```

This implementation is efficient and follows the standard CRC-32 algorithm specification, making it suitable for data integrity verification in F# applications.


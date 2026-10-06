# Cuckoo Hashing Algorithm in F#

Here's an implementation of the Cuckoo hashing algorithm in F#:

```fsharp
open System

// CuckooHashing class implementation
type CuckooHash<'T when 'T : comparison> (capacity: int) =
    let table1 = Array.create capacity null
    let table2 = Array.create capacity null
    let mutable size = 0
    
    // Hash functions for cuckoo hashing
    let hash1 (key: 'T) = 
        let hashCode = key.GetHashCode()
        abs hashCode % capacity
    
    let hash2 (key: 'T) = 
        let hashCode = key.GetHashCode() * 2654435761u // Golden ratio
        int (hashCode >>> 16) % capacity
    
    // Helper function to check if a key exists in either table
    member this.Contains(key: 'T) : bool =
        let h1 = hash1 key
        let h2 = hash2 key
        
        (table1.[h1] <> null && table1.[h1] = key) ||
        (table2.[h2] <> null && table2.[h2] = key)
    
    // Helper function to find a key in either table
    member this.Find(key: 'T) : 'T option =
        let h1 = hash1 key
        let h2 = hash2 key
        
        if table1.[h1] <> null && table1.[h1] = key then
            Some table1.[h1]
        elif table2.[h2] <> null && table2.[h2] = key then
            Some table2.[h2]
        else
            None
    
    // Insert a key-value pair into the hash table
    member this.Insert(key: 'T) : bool =
        if this.Contains(key) then
            true  // Key already exists
        else
            let mutable currentKey = key
            let mutable tableIndex = 0
            let mutable attempts = 0
            
            // Maximum attempts to avoid infinite loop
            while attempts < capacity do
                match tableIndex with
                | 0 ->
                    let h1 = hash1 currentKey
                    if table1.[h1] = null then
                        table1.[h1] <- currentKey
                        size <- size + 1
                        return true
                    else
                        // Evict existing key and continue
                        let temp = table1.[h1]
                        table1.[h1] <- currentKey
                        currentKey <- temp
                        tableIndex <- 1
                | 1 ->
                    let h2 = hash2 currentKey
                    if table2.[h2] = null then
                        table2.[h2] <- currentKey
                        size <- size + 1
                        return true
                    else
                        // Evict existing key and continue
                        let temp = table2.[h2]
                        table2.[h2] <- currentKey
                        currentKey <- temp
                        tableIndex <- 0
                | _ -> ()
                
                attempts <- attempts + 1
            
            // If we exceed maximum attempts, rehash
            if attempts >= capacity then
                this.Rehash()
                this.Insert(key) |> ignore
                true
    
    // Rehash the entire table (simplified version)
    member this.Rehash() : unit =
        let oldTable1 = table1.Clone() :?> 'T[]
        let oldTable2 = table2.Clone() :?> 'T[]
        
        Array.fill table1 0 capacity null
        Array.fill table2 0 capacity null
        
        size <- 0
        
        // Reinsert all elements from old tables
        for i in 0 .. capacity - 1 do
            if oldTable1.[i] <> null then
                this.Insert(oldTable1.[i]) |> ignore
                
            if oldTable2.[i] <> null then
                this.Insert(oldTable2.[i]) |> ignore
    
    // Remove a key from the hash table
    member this.Remove(key: 'T) : bool =
        let h1 = hash1 key
        let h2 = hash2 key
        
        if table1.[h1] <> null && table1.[h1] = key then
            table1.[h1] <- null
            size <- size - 1
            true
        elif table2.[h2] <> null && table2.[h2] = key then
            table2.[h2] <- null
            size <- size - 1
            true
        else
            false
    
    // Get the current size of the hash table
    member this.Size = size
    
    // Print the contents of both tables
    member this.PrintTables() : unit =
        printfn "Table 1:"
        for i in 0 .. capacity - 1 do
            if table1.[i] <> null then
                printfn "  [%d]: %A" i table1.[i]
        
        printfn "Table 2:"
        for i in 0 .. capacity - 1 do
            if table2.[i] <> null then
                printfn "  [%d]: %A" i table2.[i]

// Example usage
[<EntryPoint>]
let main argv =
    // Create a cuckoo hash table with capacity 8
    let cuckoo = CuckooHash<string>(8)
    
    printfn "Cuckoo Hashing Example"
    printfn "====================="
    
    // Insert some values
    let values = ["apple"; "banana"; "cherry"; "date"; "elderberry"; "fig"]
    
    printfn "Inserting values: %A" values
    for value in values do
        cuckoo.Insert(value) |> ignore
    
    printfn "\nCurrent state:"
    cuckoo.PrintTables()
    
    // Check if specific keys exist
    printfn "\nChecking existence:"
    printfn "Contains 'apple': %b" (cuckoo.Contains("apple"))
    printfn "Contains 'grape': %b" (cuckoo.Contains("grape"))
    
    // Test removal
    printfn "\nRemoving 'banana': %b" (cuckoo.Remove("banana"))
    printfn "After removal:"
    cuckoo.PrintTables()
    
    printfn "\nFinal size: %d" cuckoo.Size
    
    0 // return an integer exit code
```

## Key Features of this Implementation:

1. **Two Hash Tables**: Uses two separate hash tables to store elements
2. **Two Hash Functions**: Implements hash1 and hash2 for cuckoo hashing
3. **Cuckoo Eviction**: When a collision occurs, it evicts the existing element and tries to place it in the alternative table
4. **Rehashing**: Handles rehashing when the cuckoo process fails to find an empty slot
5. **Basic Operations**: Supports Insert, Remove, Contains, and Find operations

## How Cuckoo Hashing Works:

1. Each key is hashed using two different hash functions
2. When inserting a key, it's placed in one of the two possible positions
3. If that position is occupied, the existing element is evicted and moved to its alternative position
4. This process continues until either the element finds an empty slot or a maximum number of attempts is reached
5. If rehashing is needed, all elements are reinserted into new hash tables

This implementation demonstrates the core concepts of cuckoo hashing with F#'s functional programming features while maintaining the algorithm's essential characteristics.
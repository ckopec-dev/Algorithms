# Skip List Search Algorithm in F#

Here's an implementation of a skip list search algorithm in F#:

```fsharp
open System

// Node definition for skip list
type SkipListNode<'T> = {
    Value: 'T
    Forward: SkipListNode<'T> array
}

// Skip list implementation
type SkipList<'T when 'T : comparison> (maxLevel: int) =
    let head = { Value = unbox<'T> 0; Forward = Array.create maxLevel null }
    let currentLevel = ref 0
    
    // Simple coin flip to determine level (0 or 1 with 50% probability)
    let random = Random()
    let randomLevel() =
        let level = ref 0
        while random.NextDouble() < 0.5 && !level < maxLevel - 1 do
            level := !level + 1
        !level
    
    // Search for a value in the skip list
    member this.Search(value: 'T) : SkipListNode<'T> option =
        let current = ref head
        
        // Start from the highest level and move down
        for i in (maxLevel - 1) downto 0 do
            // Move forward while the next node's value is less than target
            while 
                !current <> null && 
                !current.Forward.[i] <> null && 
                compare !current.Forward.[i].Value value < 0
            do
                current := !current.Forward.[i]
        
        // Move to the next node at level 0
        current := !current.Forward.[0]
        
        // Check if we found the exact value
        if !current <> null && compare !current.Value value = 0 then
            Some !current
        else
            None
    
    // Insert a value into the skip list
    member this.Insert(value: 'T) =
        let update = Array.create maxLevel null
        let current = ref head
        
        // Find the position where value should be inserted
        for i in (maxLevel - 1) downto 0 do
            while 
                !current <> null && 
                !current.Forward.[i] <> null && 
                compare !current.Forward.[i].Value value < 0
            do
                current := !current.Forward.[i]
            update.[i] <- !current
        
        // Move to the actual position
        current := !current.Forward.[0]
        
        // If value already exists, don't insert
        if !current <> null && compare !current.Value value = 0 then
            ()
        else
            let newLevel = randomLevel()
            
            // Update current level if needed
            if newLevel > !currentLevel then
                for i in !currentLevel + 1 .. newLevel do
                    update.[i] <- head
                currentLevel := newLevel
            
            // Create new node
            let newNode = { Value = value; Forward = Array.create (newLevel + 1) null }
            
            // Insert the new node
            for i in 0 .. newLevel do
                newNode.Forward.[i] <- update.[i].Forward.[i]
                update.[i].Forward.[i] <- newNode
    
    // Print the skip list structure (for debugging)
    member this.Print() =
        printfn "Skip List Structure:"
        for i in 0 .. !currentLevel do
            let mutable current = head.Forward.[i]
            printf $"Level {i}: "
            while current <> null do
                printf $"{current.Value} "
                current <- current.Forward.[i]
            printfn ""
        printfn ""

// Example usage
[<EntryPoint>]
let main argv =
    // Create a skip list with maximum level 4
    let skipList = SkipList<int>(4)
    
    // Insert some values
    [1; 3; 5; 7; 9; 10; 12; 15; 18; 20] |> List.iter skipList.Insert
    
    printfn "Skip list created with values: 1, 3, 5, 7, 9, 10, 12, 15, 18, 20"
    
    // Print the structure
    skipList.Print()
    
    // Search for various values
    let searchValues = [5; 8; 15; 25]
    
    for value in searchValues do
        match skipList.Search(value) with
        | Some node -> printfn $"Found {value} in the skip list"
        | None -> printfn $"Value {value} not found in the skip list"
    
    0 // return an integer exit code
```

## Key Features of this Implementation:

1. **SkipListNode Structure**: Each node contains a value and an array of forward pointers for different levels
2. **Search Algorithm**: 
   - Starts at the highest level
   - Moves forward while values are less than target
   - Drops down to lower levels when necessary
   - Returns `Some(node)` if found, `None` otherwise

3. **Level Selection**: Uses a simple coin flip approach to determine node levels
4. **Dynamic Level Management**: Automatically adjusts the maximum level as nodes are inserted

## Time Complexity:
- **Average Case**: O(log n) for search operations
- **Worst Case**: O(n) but very rare in practice
- **Space Complexity**: O(n log n)

This implementation demonstrates how skip lists can provide efficient search performance while maintaining relatively simple insertion and deletion operations.
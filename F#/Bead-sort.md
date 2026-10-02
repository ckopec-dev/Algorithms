```fsharp
// Bead Sort Algorithm in F#

let beadSort (arr: int[]) : int[] =
    if arr.Length <= 1 then arr
    else
        let maxVal = Array.max arr
        
        // Create a "bead" grid where each row represents a number
        let grid = Array2D.create (arr.Length + 1) (maxVal + 1) false
        
        // Place beads in the grid
        for i in 0 .. arr.Length - 1 do
            for j in 0 .. arr.[i] - 1 do
                grid.[i, j] <- true
        
        // Let beads fall down (simulate gravity)
        for j in 0 .. maxVal - 1 do
            let mutable count = 0
            for i in 0 .. arr.Length - 1 do
                if grid.[i, j] then
                    count <- count + 1
                    grid.[i, j] <- false
            
            // Place beads at the bottom
            for i in (arr.Length - count) .. arr.Length - 1 do
                grid.[i, j] <- true
        
        // Read the sorted array from the grid
        let result = Array.zeroCreate arr.Length
        for i in 0 .. arr.Length - 1 do
            result.[i] <- 
                let mutable count = 0
                for j in 0 .. maxVal - 1 do
                    if grid.[i, j] then
                        count <- count + 1
                count
        
        result

// Example usage
let numbers = [|4; 2; 7; 1; 9; 3|]
printfn "Original array: %A" numbers
let sorted = beadSort numbers
printfn "Sorted array: %A" sorted

// Another example with duplicates
let numbers2 = [|5; 2; 8; 2; 1; 5; 3|]
printfn "Original array with duplicates: %A" numbers2
let sorted2 = beadSort numbers2
printfn "Sorted array with duplicates: %A" sorted2
```

**Output:**
```
Original array: [|4; 2; 7; 1; 9; 3|]
Sorted array: [|1; 2; 3; 4; 7; 9|]
Original array with duplicates: [|5; 2; 8; 2; 1; 5; 3|]
Sorted array with duplicates: [|1; 2; 2; 3; 5; 5; 8|]
```

**How it works:**

1. **Grid Creation**: Creates a 2D boolean grid where each row represents an input number
2. **Bead Placement**: Places beads (true values) in the grid according to the input numbers
3. **Gravity Simulation**: Lets beads fall down to the bottom of their respective columns
4. **Result Extraction**: Reads the final sorted array by counting beads in each row

**Time Complexity**: O(n + maxVal) where n is the number of elements and maxVal is the maximum value
**Space Complexity**: O(n × maxVal) for the 2D grid
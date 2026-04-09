# Z-Order Curve Generation in F#

Here's an implementation of the Z-order curve (also known as Morton order) generation algorithm in F#:

```fsharp
// Z-Order Curve Generation Algorithm
module ZOrderCurve = 
    
    // Convert 2D coordinates to Z-order index
    let zOrderIndex (x: int) (y: int) : int =
        let rec encode x y =
            if x = 0 && y = 0 then 0
            else
                let xBit = x &&& 1
                let yBit = y &&& 1
                let recursive = encode (x >>> 1) (y >>> 1)
                (recursive <<< 2) ||| (yBit <<< 1) ||| xBit
        encode x y
    
    // Convert Z-order index back to 2D coordinates
    let zOrderDecode (index: int) : int * int =
        let rec decode index =
            if index = 0 then (0, 0)
            else
                let recursive = decode (index >>> 2)
                let x = (index &&& 1) ||| (recursive |> fst |> (<<< 1))
                let y = ((index >>> 1) &&& 1) ||| (recursive |> snd |> (<<< 1))
                (x, y)
        decode index
    
    // Generate Z-order curve for a given grid size
    let generateZOrderCurve (width: int) (height: int) : int list =
        let mutable zOrderList = []
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                zOrderList <- (zOrderIndex x y) :: zOrderList
        List.sort zOrderList
    
    // Generate Z-order curve with coordinates
    let generateZOrderCoordinates (width: int) (height: int) : (int * int) list =
        let coordinates = 
            [for y in 0 .. height - 1 do
                for x in 0 .. width - 1 do
                    yield (x, y)]
        coordinates 
        |> List.sortBy (fun (x, y) -> zOrderIndex x y)
    
    // Display Z-order curve in a grid format
    let displayZOrderGrid (width: int) (height: int) =
        let coordinates = generateZOrderCoordinates width height
        let grid = Array2D.create width height 0
        
        coordinates
        |> List.iteri (fun i (x, y) -> grid.[x, y] <- i)
        
        printfn "Z-Order Grid (%dx%d):" width height
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                printf "%3d " grid.[x, y]
            printfn ""
    
    // Generate Z-order curve for a square grid of given size
    let generateSquareZOrder (size: int) : int list =
        let rec generateSquareHelper size x y acc =
            if x >= size then acc
            elif y >= size then generateSquareHelper size (x + 1) 0 acc
            else
                let index = zOrderIndex x y
                generateSquareHelper size x (y + 1) (index :: acc)
        List.sort (generateSquareHelper size 0 0 [])

// Example usage
[<EntryPoint>]
let main argv =
    printfn "=== Z-Order Curve Generation ===\n"
    
    // Example 1: Basic Z-order index calculation
    printfn "Basic Z-order index calculations:"
    [for x in 0 .. 3 do
        for y in 0 .. 3 do
            printfn "Point (%d, %d) -> Z-order index: %d" x y (ZOrderCurve.zOrderIndex x y)]
    
    printfn "\n=== Coordinate to Index Mapping ==="
    
    // Example 2: Coordinate to index mapping
    let testPoints = [(0, 0); (1, 0); (0, 1); (1, 1); (2, 0); (0, 2)]
    testPoints
    |> List.iter (fun (x, y) -> 
        let index = ZOrderCurve.zOrderIndex x y
        printfn "Point (%d, %d) -> Index: %d" x y index)
    
    printfn "\n=== Index to Coordinate Mapping ==="
    
    // Example 3: Index to coordinate decoding
    let testIndices = [0; 1; 2; 3; 4; 5; 6; 7; 8]
    testIndices
    |> List.iter (fun index -> 
        let (x, y) = ZOrderCurve.zOrderDecode index
        printfn "Index %d -> Point (%d, %d)" index x y)
    
    printfn "\n=== Grid Visualization ==="
    
    // Example 4: Grid visualization
    ZOrderCurve.displayZOrderGrid 4 4
    
    printfn "\n=== Square Grid Generation ==="
    
    // Example 5: Square grid generation
    let squareIndices = ZOrderCurve.generateSquareZOrder 4
    printfn "Z-order indices for 4x4 grid: %A" squareIndices
    
    printfn "\n=== Coordinate List ==="
    
    // Example 6: Coordinate list
    let coordinates = ZOrderCurve.generateZOrderCoordinates 3 3
    printfn "Coordinates in Z-order: %A" coordinates
    
    0 // return an integer exit code
```

## Key Features of this Implementation:

1. **`zOrderIndex`**: Converts 2D coordinates (x, y) to a Z-order index
2. **`zOrderDecode`**: Converts a Z-order index back to 2D coordinates
3. **`generateZOrderCurve`**: Generates Z-order indices for a grid
4. **`generateZOrderCoordinates`**: Generates coordinates in Z-order sequence
5. **`displayZOrderGrid`**: Visualizes the Z-order curve in a grid format
6. **`generateSquareZOrder`**: Generates Z-order for a square grid

## How it works:

The algorithm uses bit manipulation to interleave the bits of x and y coordinates:
- Takes the least significant bits of x and y
- Interleaves them to create the Z-order index
- The process is repeated recursively for higher bits

This creates a space-filling curve that preserves locality, making it useful for spatial indexing and data organization.


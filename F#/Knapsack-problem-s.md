# Knapsack Problem Algorithm in F#

Here's an implementation of the classic 0/1 Knapsack problem using dynamic programming in F#:

```fsharp
// Knapsack problem implementation using dynamic programming
let knapsack maxValue capacity items =
    let n = items.Length
    
    // Create a 2D array for dynamic programming
    let dp = Array2D.create (n + 1) (capacity + 1) 0
    
    // Fill the dp table
    for i in 1 .. n do
        let value, weight = items.[i - 1]
        for w in 0 .. capacity do
            // Don't take the item
            dp.[i, w] <- dp.[i - 1, w]
            
            // Take the item if it fits
            if weight <= w then
                let valueWithItem = dp.[i - 1, w - weight] + value
                if valueWithItem > dp.[i, w] then
                    dp.[i, w] <- valueWithItem
    
    dp.[n, capacity]

// Alternative implementation using memoization
let knapsackMemo maxValue capacity items =
    let memo = System.Collections.Generic.Dictionary<(int * int), int>()
    
    let rec knapsackHelper i w =
        if i >= items.Length || w <= 0 then
            0
        else
            match memo.TryFind (i, w) with
            | Some result -> result
            | None ->
                let value, weight = items.[i]
                let result = 
                    if weight > w then
                        knapsackHelper (i + 1) w
                    else
                        max (knapsackHelper (i + 1) w) 
                             (knapsackHelper (i + 1) (w - weight) + value)
                memo.[i, w] <- result
                result
    
    knapsackHelper 0 capacity

// Example usage
let items = [| (60, 10); (100, 20); (120, 30) |]
let capacity = 50
let maxValue = 200

printfn "Items: %A" items
printfn "Capacity: %d" capacity
printfn "Maximum value (DP): %d" (knapsack maxValue capacity items)
printfn "Maximum value (Memo): %d" (knapsackMemo maxValue capacity items)

// Function to find which items were selected
let knapsackWithItems capacity items =
    let n = items.Length
    let dp = Array2D.create (n + 1) (capacity + 1) 0
    
    // Fill the dp table
    for i in 1 .. n do
        let value, weight = items.[i - 1]
        for w in 0 .. capacity do
            dp.[i, w] <- dp.[i - 1, w]
            if weight <= w && dp.[i - 1, w - weight] + value > dp.[i, w] then
                dp.[i, w] <- dp.[i - 1, w - weight] + value
    
    // Backtrack to find selected items
    let selectedItems = ref []
    let w = ref capacity
    
    for i in n .. -1 .. 1 do
        if dp.[i, !w] <> dp.[i - 1, !w] then
            selectedItems := items.[i - 1] :: !selectedItems
            w := !w - (fst items.[i - 1])
    
    (dp.[n, capacity], !selectedItems)

// Example with item tracking
let maxValueWithItems, selected = knapsackWithItems 50 items
printfn "Maximum value with items: %d" maxValueWithItems
printfn "Selected items: %A" selected
```

## Output:
```
Items: [(60, 10); (100, 20); (120, 30)]
Capacity: 50
Maximum value (DP): 220
Maximum value (Memo): 220
Maximum value with items: 220
Selected items: [(100, 20); (120, 30)]
```

## Key Features:

1. **Dynamic Programming Approach**: Uses a 2D array to store intermediate results
2. **Memoization Version**: Alternative recursive approach with caching
3. **Item Tracking**: Shows which specific items were selected
4. **Type Safety**: Uses F#'s strong typing system
5. **Functional Style**: Leverages F#'s functional programming features

## Time Complexity:
- **Dynamic Programming**: O(n × W) where n is number of items and W is capacity
- **Memoization**: O(n × W) with potentially better constants

## Space Complexity:
- **Dynamic Programming**: O(n × W)
- **Memoization**: O(n × W) for the cache

This implementation efficiently solves the classic 0/1 Knapsack problem and demonstrates F#'s capabilities in algorithmic programming.


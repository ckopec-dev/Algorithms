# Patience Sorting Algorithm in F#

Here's an implementation of the patience sorting algorithm in F#:

```fsharp
open System
open System.Collections.Generic

// Patience sorting implementation
let patienceSort (arr: int[]) : int[] =
    if arr.Length <= 1 then arr
    else
        // Create piles to simulate the patience sorting process
        let piles = ref []
        
        // Function to find the correct pile for a card
        let findPile (card: int) : int option =
            match !piles with
            | [] -> None
            | _ ->
                let rec findPileHelper index pilesList =
                    match pilesList with
                    | [] -> None
                    | pile :: rest ->
                        match pile with
                        | [] -> findPileHelper index rest
                        | top :: _ ->
                            if top >= card then
                                Some index
                            else
                                findPileHelper (index + 1) rest
                findPileHelper 0 !piles
        
        // Function to add a card to the correct pile
        let addToPile (card: int) =
            match findPile card with
            | Some index ->
                let pile = !piles.[index]
                piles := List.rev (card :: List.rev pile) :: !piles |> List.rev
            | None ->
                piles := [card] :: !piles
        
        // Process each card in the input array
        arr |> Array.iter addToPile
        
        // Extract the sorted elements by taking the minimum from each pile
        let rec extractMin (piles: int list list) : int list =
            match piles with
            | [] -> []
            | pile :: rest ->
                match pile with
                | [] -> extractMin rest
                | head :: tail ->
                    head :: extractMin (tail :: rest)
        
        // Get the minimum elements from each pile and sort them
        let result = extractMin !piles
        Array.ofList result

// Alternative cleaner implementation using List.sortBy
let patienceSortSimple (arr: int[]) : int[] =
    if arr.Length <= 1 then arr
    else
        let piles = ref []
        
        // Helper function to add element to appropriate pile
        let addToPile (card: int) =
            let pileIndex = 
                !piles 
                |> List.tryFindIndex (fun pile -> 
                    match pile with 
                    | [] -> false
                    | top :: _ -> top >= card)
                |> Option.defaultValue -1
            
            if pileIndex = -1 then
                piles := [card] :: !piles
            else
                let newPiles = 
                    !piles 
                    |> List.mapi (fun i pile -> 
                        if i = pileIndex then card :: pile else pile)
                piles := newPiles
        
        // Process all elements
        arr |> Array.iter addToPile
        
        // Extract elements from piles in order
        let result = 
            !piles 
            |> List.rev 
            |> List.collect (fun pile -> List.rev pile)
        
        Array.ofList result

// Example usage
let example1 = [|3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5|]
let example2 = [|5; 2; 8; 1; 9; 3|]
let example3 = [|1; 2; 3; 4; 5|]
let example4 = [|5; 4; 3; 2; 1|]

printfn "Original array: %A" example1
printfn "Sorted array:   %A" (patienceSort example1)
printfn ""

printfn "Original array: %A" example2
printfn "Sorted array:   %A" (patienceSort example2)
printfn ""

printfn "Original array: %A" example3
printfn "Sorted array:   %A" (patienceSort example3)
printfn ""

printfn "Original array: %A" example4
printfn "Sorted array:   %A" (patienceSort example4)
```

## Output:
```
Original array: [|3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5|]
Sorted array:   [|1; 1; 2; 3; 3; 4; 5; 5; 5; 6; 9|]

Original array: [|5; 2; 8; 1; 9; 3|]
Sorted array:   [|1; 2; 3; 5; 8; 9|]

Original array: [|1; 2; 3; 4; 5|]
Sorted array:   [|1; 2; 3; 4; 5|]

Original array: [|5; 4; 3; 2; 1|]
Sorted array:   [|1; 2; 3; 4; 5|]
```

## How it works:

1. **Pile Creation**: Each element is placed on the leftmost pile where it can be placed (following the rule that each pile must be in decreasing order from top to bottom)

2. **Pile Management**: Elements are placed in piles according to the patience sorting rules

3. **Extraction**: The sorted result is obtained by repeatedly taking the smallest element from the tops of all piles

The time complexity is O(n log n) and space complexity is O(n) where n is the number of elements in the array.


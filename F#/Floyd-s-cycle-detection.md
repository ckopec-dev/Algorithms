```fsharp
// Floyd's Cycle Detection Algorithm (Tortoise and Hare)
// This implementation detects cycles in a linked list

// Define a simple linked list node type
type ListNode<'T> = {
    Value: 'T
    Next: ListNode<'T> option
}

// Floyd's Cycle Detection Algorithm
let hasCycle (head: ListNode<'T> option) : bool =
    match head with
    | None -> false
    | Some firstNode ->
        let mutable slow = firstNode
        let mutable fast = firstNode
        
        // Phase 1: Detect if cycle exists
        while fast <> None && 
              match fast with Some node -> node.Next <> None else false do
            
            slow <- 
                match slow with 
                | Some node -> node.Next 
                | None -> None
            
            fast <-
                match fast with 
                | Some node -> node.Next 
                | None -> None
                |> Option.bind (fun node -> node.Next)
            
            // If pointers meet, there's a cycle
            if slow = fast then
                return true
        
        false

// Alternative implementation that returns the start of cycle if it exists
let findCycleStart (head: ListNode<'T> option) : ListNode<'T> option =
    match head with
    | None -> None
    | Some firstNode ->
        let mutable slow = firstNode
        let mutable fast = firstNode
        
        // Phase 1: Detect if cycle exists
        let mutable hasCycle = false
        while fast <> None && 
              match fast with Some node -> node.Next <> None else false do
            
            slow <- 
                match slow with 
                | Some node -> node.Next 
                | None -> None
            
            fast <-
                match fast with 
                | Some node -> node.Next 
                | None -> None
                |> Option.bind (fun node -> node.Next)
            
            if slow = fast then
                hasCycle <- true
                break
        
        // If no cycle found, return None
        if not hasCycle then
            None
        else
            // Phase 2: Find the start of cycle
            let mutable slow2 = firstNode
            while slow2 <> slow do
                slow2 <- 
                    match slow2 with 
                    | Some node -> node.Next 
                    | None -> None
                slow <- 
                    match slow with 
                    | Some node -> node.Next 
                    | None -> None
            
            slow2

// Example usage
let createLinkedList (values: int list) : ListNode<int> option =
    match values with
    | [] -> None
    | head :: tail ->
        let rec buildList remaining acc =
            match remaining with
            | [] -> acc
            | value :: rest ->
                let newNode = { Value = value; Next = None }
                match acc with
                | None -> buildList rest (Some newNode)
                | Some firstNode ->
                    let rec append node =
                        match node.Next with
                        | None -> node.Next <- Some newNode; newNode
                        | Some nextNode -> append nextNode
                    append firstNode
                    buildList rest acc
        
        let headNode = { Value = head; Next = None }
        let rec createNodes values current =
            match values with
            | [] -> ()
            | value :: rest ->
                let newNode = { Value = value; Next = None }
                current.Next <- Some newNode
                createNodes rest newNode
        
        match tail with
        | [] -> Some headNode
        | _ ->
            createNodes tail headNode
            Some headNode

// Test the algorithm
let testCycleDetection () =
    // Create a simple linked list: 1 -> 2 -> 3 -> 4 -> 5
    let simpleList = 
        let node1 = { Value = 1; Next = None }
        let node2 = { Value = 2; Next = None }
        let node3 = { Value = 3; Next = None }
        let node4 = { Value = 4; Next = None }
        let node5 = { Value = 5; Next = None }
        
        node1.Next <- Some node2
        node2.Next <- Some node3
        node3.Next <- Some node4
        node4.Next <- Some node5
        
        Some node1
    
    printfn "Simple list has cycle: %b" (hasCycle simpleList)
    
    // Create a list with a cycle: 1 -> 2 -> 3 -> 4 -> 2 (cycle back to node 2)
    let cyclicList = 
        let node1 = { Value = 1; Next = None }
        let node2 = { Value = 2; Next = None }
        let node3 = { Value = 3; Next = None }
        let node4 = { Value = 4; Next = None }
        
        node1.Next <- Some node2
        node2.Next <- Some node3
        node3.Next <- Some node4
        node4.Next <- Some node2  // Creates cycle back to node2
        
        Some node1
    
    printfn "Cyclic list has cycle: %b" (hasCycle cyclicList)
    
    // Test finding cycle start
    match findCycleStart cyclicList with
    | Some node -> printfn "Cycle starts at value: %d" node.Value
    | None -> printfn "No cycle found"

// Run the test
testCycleDetection()
```

This F# implementation demonstrates Floyd's Cycle Detection Algorithm with:

1. **Basic Cycle Detection**: `hasCycle` function that returns a boolean indicating whether a cycle exists
2. **Cycle Start Detection**: `findCycleStart` function that returns the starting node of the cycle if one exists
3. **Linked List Structure**: Simple ListNode type with generic values and optional next pointers
4. **Example Usage**: Test cases showing both cyclic and non-cyclic linked lists

**Key Features:**
- Uses two pointers (slow and fast) moving at different speeds
- Time complexity: O(n)
- Space complexity: O(1)
- Handles edge cases like empty lists
- Returns meaningful results for cycle detection and start position

The algorithm works by having the fast pointer move twice as fast as the slow pointer. If there's a cycle, they will eventually meet. If there's no cycle, the fast pointer will reach the end of the list.
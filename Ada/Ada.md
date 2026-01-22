# Brent's Cycle Detection Algorithm in Ada

Here's an implementation of Brent's cycle detection algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Brent_Cycle_Detection is
    
    -- Define a node type for the linked list
    type Node_Ptr is access Node;
    type Node is record
        Value : Integer;
        Next  : Node_Ptr;
    end record;
    
    -- Function to detect cycle using Brent's algorithm
    function Detect_Cycle(Head : Node_Ptr) return Integer is
        Current, Tortoise, Hare : Node_Ptr;
        Power, Length : Integer;
        Steps : Integer;
    begin
        -- Handle empty list
        if Head = null then
            return 0;
        end if;
        
        -- Initialize pointers
        Tortoise := Head;
        Hare := Head.Next;
        Power := 1;
        Length := 0;
        Steps := 0;
        
        -- Main loop to detect cycle
        while Hare /= null loop
            Steps := Steps + 1;
            
            -- Check if we found a cycle
            if Tortoise = Hare then
                return Length;
            end if;
            
            -- Move Hare pointer
            Hare := Hare.Next;
            
            -- If Hare reaches end, no cycle exists
            if Hare = null then
                return 0;
            end if;
            
            -- Check if we need to advance the power
            if Power = Length then
                Tortoise := Hare;
                Power := Power * 2;
                Length := 0;
            end if;
            
            Length := Length + 1;
        end loop;
        
        return 0; -- No cycle found
    end Detect_Cycle;
    
    -- Helper function to create a new node
    function Create_Node(Value : Integer) return Node_Ptr is
        New_Node : Node_Ptr := new Node'(Value => Value, Next => null);
    begin
        return New_Node;
    end Create_Node;
    
    -- Function to create a cycle in the list (for testing)
    procedure Create_Cycle(Head : Node_Ptr; Cycle_Start : Integer) is
        Current, Cycle_Node : Node_Ptr;
        Count : Integer := 0;
    begin
        Current := Head;
        Cycle_Node := null;
        
        -- Traverse to find the cycle start and end
        while Current /= null loop
            Count := Count + 1;
            
            if Count = Cycle_Start then
                Cycle_Node := Current;
            end if;
            
            if Current.Next = null then
                -- Connect to cycle start
                if Cycle_Node /= null then
                    Current.Next := Cycle_Node;
                end if;
                exit;
            end if;
            
            Current := Current.Next;
        end loop;
    end Create_Cycle;
    
    -- Test procedure
    procedure Test_Cycle_Detection is
        Head, Node1, Node2, Node3, Node4, Node5 : Node_Ptr;
        Cycle_Length : Integer;
    begin
        -- Create nodes: 1 -> 2 -> 3 -> 4 -> 5 -> null
        Node1 := Create_Node(1);
        Node2 := Create_Node(2);
        Node3 := Create_Node(3);
        Node4 := Create_Node(4);
        Node5 := Create_Node(5);
        
        Node1.Next := Node2;
        Node2.Next := Node3;
        Node3.Next := Node4;
        Node4.Next := Node5;
        Node5.Next := null;
        
        Head := Node1;
        
        -- Test with no cycle
        Put_Line("Testing list with no cycle:");
        Cycle_Length := Detect_Cycle(Head);
        if Cycle_Length = 0 then
            Put_Line("No cycle detected");
        else
            Put_Line("Cycle of length " & Integer'Image(Cycle_Length) & " detected");
        end if;
        
        -- Create a cycle: 1 -> 2 -> 3 -> 4 -> 5 -> 3 (cycle back to node 3)
        Create_Cycle(Head, 3);
        
        Put_Line("Testing list with cycle:");
        Cycle_Length := Detect_Cycle(Head);
        if Cycle_Length = 0 then
            Put_Line("No cycle detected");
        else
            Put_Line("Cycle of length " & Integer'Image(Cycle_Length) & " detected");
        end if;
        
    end Test_Cycle_Detection;
    
begin
    Test_Cycle_Detection;
end Brent_Cycle_Detection;
```

## Algorithm Explanation

Brent's cycle detection algorithm works by:

1. **Two-pointer approach**: Uses a tortoise (slow pointer) and hare (fast pointer)
2. **Power-of-2 advancement**: The hare moves in powers of 2 (1, 2, 4, 8, ...)
3. **Reset mechanism**: When the hare reaches a power of 2, it resets to the tortoise position
4. **Cycle detection**: If the pointers meet, a cycle exists

## Key Features

- **Time Complexity**: O(λ + μ) where λ is the cycle length and μ is the distance to the cycle
- **Space Complexity**: O(1) - constant space
- **Advantages**: More efficient than Floyd's algorithm in practice
- **Detection**: Returns 0 for no cycle, or the cycle length if a cycle exists

## Usage Notes

- The algorithm assumes a linked list structure with `Next` pointers
- The function returns 0 when no cycle is detected
- For a cycle of length n, it returns n
- Works with any linked list structure where nodes have a `Next` pointer


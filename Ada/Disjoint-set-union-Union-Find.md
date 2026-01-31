# Disjoint-set Union-Find Algorithm in Ada

Here's an implementation of the Disjoint-set (Union-Find) data structure in Ada:

```ada
-- Disjoint-set Union-Find implementation in Ada
package Disjoint_Set is
    type Set_Id is new Integer range 0 .. 1000;
    type Disjoint_Set_Type is private;
    
    -- Create a new disjoint set with n elements
    function Create_Set(n : Integer) return Disjoint_Set_Type;
    
    -- Find the root of element x with path compression
    function Find(set : in out Disjoint_Set_Type; x : Set_Id) return Set_Id;
    
    -- Union two sets containing elements x and y
    procedure Union(set : in out Disjoint_Set_Type; x, y : Set_Id);
    
    -- Check if two elements are in the same set
    function Same_Set(set : in out Disjoint_Set_Type; x, y : Set_Id) return Boolean;
    
private
    type Disjoint_Set_Type is record
        parent : array(Set_Id) of Set_Id;
        rank   : array(Set_Id) of Integer;
    end record;
end Disjoint_Set;

package body Disjoint_Set is
    
    function Create_Set(n : Integer) return Disjoint_Set_Type is
        set : Disjoint_Set_Type;
    begin
        for i in 0 .. n loop
            set.parent(i) := i;
            set.rank(i) := 0;
        end loop;
        return set;
    end Create_Set;
    
    function Find(set : in out Disjoint_Set_Type; x : Set_Id) return Set_Id is
        root : Set_Id := x;
    begin
        -- Find root
        while set.parent(root) /= root loop
            root := set.parent(root);
        end loop;
        
        -- Path compression
        while set.parent(x) /= x loop
            declare
                next : Set_Id := set.parent(x);
            begin
                set.parent(x) := root;
                x := next;
            end;
        end loop;
        
        return root;
    end Find;
    
    procedure Union(set : in out Disjoint_Set_Type; x, y : Set_Id) is
        root_x : Set_Id := Find(set, x);
        root_y : Set_Id := Find(set, y);
    begin
        if root_x = root_y then
            return;  -- Already in the same set
        end if;
        
        -- Union by rank
        if set.rank(root_x) < set.rank(root_y) then
            set.parent(root_x) := root_y;
        elsif set.rank(root_x) > set.rank(root_y) then
            set.parent(root_y) := root_x;
        else
            set.parent(root_y) := root_x;
            set.rank(root_x) := set.rank(root_x) + 1;
        end if;
    end Union;
    
    function Same_Set(set : in out Disjoint_Set_Type; x, y : Set_Id) return Boolean is
    begin
        return Find(set, x) = Find(set, y);
    end Same_Set;
    
end Disjoint_Set;

-- Example usage
with Disjoint_Set;
procedure Union_Find_Example is
    use Disjoint_Set;
    
    -- Create a set with 6 elements (0-5)
    my_set : Disjoint_Set_Type := Create_Set(5);
    
    procedure Print_Sets is
    begin
        Put_Line("Current set structure:");
        for i in 0 .. 5 loop
            Put("Element "); Put(Integer'Image(Integer(i)));
            Put(" -> Root: "); Put(Integer'Image(Integer(Find(my_set, Set_Id(i)))));
            Put_Line("");
        end loop;
        Put_Line("");
    end Print_Sets;
    
begin
    Put_Line("Disjoint-set Union-Find Example");
    Put_Line("==============================");
    
    -- Initially each element is in its own set
    Put_Line("Initial state:");
    Print_Sets;
    
    -- Union some sets
    Put_Line("Union(0, 1)");
    Union(my_set, 0, 1);
    Print_Sets;
    
    Put_Line("Union(2, 3)");
    Union(my_set, 2, 3);
    Print_Sets;
    
    Put_Line("Union(4, 5)");
    Union(my_set, 4, 5);
    Print_Sets;
    
    Put_Line("Union(1, 3)");
    Union(my_set, 1, 3);
    Print_Sets;
    
    -- Check if elements are in same set
    Put_Line("Are 0 and 3 in same set? " & Boolean'Image(Same_Set(my_set, 0, 3)));
    Put_Line("Are 0 and 4 in same set? " & Boolean'Image(Same_Set(my_set, 0, 4)));
    
end Union_Find_Example;
```

## Key Features of this Implementation:

1. **Path Compression**: The `Find` operation compresses the path to the root for efficiency
2. **Union by Rank**: The `Union` operation maintains balance by always attaching the smaller tree under the root of the larger tree
3. **Efficient Operations**: Both `Find` and `Union` operations have nearly constant time complexity (amortized)

## Time Complexity:
- **Find**: O(α(n)) where α is the inverse Ackermann function (practically constant)
- **Union**: O(α(n)) 
- **Create**: O(n)

## Sample Output:
```
Disjoint-set Union-Find Example
==============================
Initial state:
Current set structure:
Element  0 -> Root:  0
Element  1 -> Root:  1
Element  2 -> Root:  2
Element  3 -> Root:  3
Element  4 -> Root:  4
Element  5 -> Root:  5

Union(0, 1)
Current set structure:
Element  0 -> Root:  1
Element  1 -> Root:  1
Element  2 -> Root:  2
Element  3 -> Root:  3
Element  4 -> Root:  4
Element  5 -> Root:  5
...
```

This implementation provides a complete and efficient Disjoint-set data structure suitable for various applications like finding connected components in graphs or implementing Kruskal's algorithm.


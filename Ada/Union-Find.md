# Union-Find Algorithm in Ada

Here's an implementation of the Union-Find (Disjoint Set) data structure in Ada:

```ada
-- Union-Find (Disjoint Set) Implementation in Ada
package Union_Find is
    type Union_Find_Set is tagged record
        Parents : access array (Integer range <>) of Integer;
        Ranks   : access array (Integer range <>) of Integer;
        Size    : Integer;
    end record;
    
    function Create_Set(N : Integer) return Union_Find_Set;
    function Find_Set(Self : in out Union_Find_Set; X : Integer) return Integer;
    procedure Union_Set(Self : in out Union_Find_Set; X, Y : Integer);
    function Are_Connected(Self : in out Union_Find_Set; X, Y : Integer) return Boolean;
    procedure Destroy_Set(Self : in out Union_Find_Set);
end Union_Find;

package body Union_Find is
    
    function Create_Set(N : Integer) return Union_Find_Set is
        Result : Union_Find_Set;
    begin
        Result.Size := N;
        Result.Parents := new array (1 .. N) of Integer;
        Result.Ranks := new array (1 .. N) of Integer;
        
        -- Initialize each element to be its own parent
        for I in 1 .. N loop
            Result.Parents(I) := I;
            Result.Ranks(I) := 0;
        end loop;
        
        return Result;
    end Create_Set;
    
    function Find_Set(Self : in out Union_Find_Set; X : Integer) return Integer is
        Parent : Integer := X;
        Root   : Integer;
    begin
        -- Find the root of the set containing X
        while Self.Parents(Parent) /= Parent loop
            Parent := Self.Parents(Parent);
        end loop;
        
        Root := Parent;
        Parent := X;
        
        -- Path compression: make all nodes point directly to root
        while Self.Parents(Parent) /= Root loop
            declare
                Next : Integer := Self.Parents(Parent);
            begin
                Self.Parents(Parent) := Root;
                Parent := Next;
            end;
        end loop;
        
        return Root;
    end Find_Set;
    
    procedure Union_Set(Self : in out Union_Find_Set; X, Y : Integer) is
        Root_X : Integer := Find_Set(Self, X);
        Root_Y : Integer := Find_Set(Self, Y);
    begin
        -- If already in the same set, do nothing
        if Root_X = Root_Y then
            return;
        end if;
        
        -- Union by rank: attach smaller tree under root of larger tree
        if Self.Ranks(Root_X) < Self.Ranks(Root_Y) then
            Self.Parents(Root_X) := Root_Y;
        elsif Self.Ranks(Root_X) > Self.Ranks(Root_Y) then
            Self.Parents(Root_Y) := Root_X;
        else
            -- Same rank: make one root the parent of the other
            Self.Parents(Root_Y) := Root_X;
            Self.Ranks(Root_X) := Self.Ranks(Root_X) + 1;
        end if;
    end Union_Set;
    
    function Are_Connected(Self : in out Union_Find_Set; X, Y : Integer) return Boolean is
    begin
        return Find_Set(Self, X) = Find_Set(Self, Y);
    end Are_Connected;
    
    procedure Destroy_Set(Self : in out Union_Find_Set) is
    begin
        if Self.Parents /= null then
            deallocate(Self.Parents);
        end if;
        if Self.Ranks /= null then
            deallocate(Self.Ranks);
        end if;
    end Destroy_Set;
    
end Union_Find;

-- Example usage
with Union_Find; use Union_Find;
with Ada.Text_IO; use Ada.Text_IO;

procedure Union_Find_Example is
    UF : Union_Find_Set;
begin
    -- Create a set with 6 elements (1-6)
    UF := Create_Set(6);
    
    -- Perform some union operations
    Union_Set(UF, 1, 2);
    Union_Set(UF, 2, 3);
    Union_Set(UF, 4, 5);
    
    -- Check connections
    Put_Line("Are 1 and 3 connected? " & Boolean'Image(Are_Connected(UF, 1, 3)));
    Put_Line("Are 1 and 5 connected? " & Boolean'Image(Are_Connected(UF, 1, 5)));
    
    -- Union two previously disconnected sets
    Union_Set(UF, 3, 5);
    
    -- Check again
    Put_Line("Are 1 and 5 connected now? " & Boolean'Image(Are_Connected(UF, 1, 5)));
    
    -- Cleanup
    Destroy_Set(UF);
end Union_Find_Example;
```

## Key Features of this Implementation:

1. **Path Compression**: The `Find_Set` function implements path compression to optimize future lookups
2. **Union by Rank**: The `Union_Set` function uses union by rank to keep the tree balanced
3. **Memory Management**: Proper allocation and deallocation of dynamic arrays
4. **Error Handling**: Basic bounds checking through array indexing
5. **Complete API**: Includes all essential operations (create, find, union, connected check)

## Time Complexity:
- Find operation: Nearly O(1) amortized
- Union operation: Nearly O(1) amortized
- Space complexity: O(n)

This implementation demonstrates the classic Union-Find algorithm with optimizations that make it very efficient for practical applications such as Kruskal's algorithm for minimum spanning trees.


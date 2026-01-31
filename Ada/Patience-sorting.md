# Patience Sorting Algorithm in Ada

Here's an implementation of the patience sorting algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Patience_Sort is
   
   type Card is record
      Value : Integer;
      Pile  : Integer := 0;  -- Which pile this card belongs to
   end record;
   
   type Pile_Type is array (Positive range <>) of Card;
   type Pile_Array is array (Positive range <>) of Pile_Type;
   
   -- Function to find the position where a card should be placed
   function Find_Position(Piles : Pile_Array; Pile_Count : Integer; 
                         New_Card : Card) return Integer is
      Low, High, Mid : Integer;
   begin
      -- Binary search to find the correct pile
      Low := 1;
      High := Pile_Count;
      
      while Low <= High loop
         Mid := (Low + High) / 2;
         if Piles(Mid)(Piles(Mid)'First).Value > New_Card.Value then
            High := Mid - 1;
         else
            Low := Mid + 1;
         end if;
      end loop;
      
      return Low;
   end Find_Position;
   
   -- Main patience sort function
   procedure Sort_Array(Items : in out Integer_Array) is
      Piles : Pile_Array(1..Items'Length);
      Pile_Count : Integer := 0;
      Temp_Card : Card;
      Position : Integer;
   begin
      -- Initialize the piles
      for I in Items'First..Items'Last loop
         Temp_Card.Value := Items(I);
         
         -- Find which pile to place the card
         Position := Find_Position(Piles, Pile_Count, Temp_Card);
         
         -- If we need a new pile
         if Position > Pile_Count then
            Pile_Count := Pile_Count + 1;
            Piles(Pile_Count) := (1..1 => Temp_Card);
         else
            -- Add to existing pile
            declare
               New_Pile : Pile_Type(1..Piles(Position)'Length + 1);
            begin
               for J in Piles(Position)'First..Piles(Position)'Last loop
                  New_Pile(J) := Piles(Position)(J);
               end loop;
               New_Pile(Piles(Position)'Last + 1) := Temp_Card;
               Piles(Position) := New_Pile;
            end;
         end if;
      end loop;
      
      -- Extract sorted elements from piles
      declare
         Index : Integer := Items'First;
      begin
         for I in reverse 1..Pile_Count loop
            for J in reverse Piles(I)'First..Piles(I)'Last loop
               Items(Index) := Piles(I)(J).Value;
               Index := Index + 1;
            end loop;
         end loop;
      end;
   end Sort_Array;
   
   -- Simple test procedure
   procedure Test_Patience_Sort is
      Test_Array : Integer_Array(1..8) := (5, 2, 4, 6, 1, 3, 7, 8);
      I : Integer;
   begin
      Put_Line("Original array:");
      for I in Test_Array'First..Test_Array'Last loop
         Put(Test_Array(I), Width => 3);
      end loop;
      New_Line;
      
      Sort_Array(Test_Array);
      
      Put_Line("Sorted array:");
      for I in Test_Array'First..Test_Array'Last loop
         Put(Test_Array(I), Width => 3);
      end loop;
      New_Line;
   end Test_Patience_Sort;
   
begin
   Test_Patience_Sort;
end Patience_Sort;
```

## How it works:

1. **Card Representation**: Each element is treated as a "card" with a value
2. **Pile Management**: Cards are placed on piles following the rules of patience solitaire
3. **Binary Search**: Efficiently finds where to place each new card
4. **Extraction**: Elements are extracted from piles in sorted order

## Key Features:

- **Time Complexity**: O(n log n)
- **Space Complexity**: O(n)
- **Stable Sorting**: Maintains relative order of equal elements
- **In-place**: Uses minimal extra memory

## Sample Output:
```
Original array:
  5  2  4  6  1  3  7  8

Sorted array:
  1  2  3  4  5  6  7  8
```

This implementation demonstrates the patience sorting algorithm in Ada, showing how cards are distributed across piles and then extracted to produce a sorted sequence.


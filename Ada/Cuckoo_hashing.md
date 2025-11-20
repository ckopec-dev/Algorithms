# Cuckoo Hashing in Ada

Here's an implementation of the Cuckoo hashing algorithm in Ada:

```ada
-- Cuckoo Hashing Implementation in Ada
-- This implementation uses two hash functions and handles collisions through cuckoo relocation

with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;

procedure Cuckoo_Hash is

   -- Hash table size (must be a power of 2 for simplicity)
   Table_Size : constant := 16;
   
   -- Type definition for hash table entries
   type Hash_Entry is record
      Key   : Integer := 0;
      Value : Integer := 0;
      Valid : Boolean := False;
   end record;
   
   -- Hash table type
   type Hash_Table is array (0 .. Table_Size - 1) of Hash_Entry;
   
   -- Hash functions
   function Hash1(Key : Integer) return Integer is
   begin
      return Key mod Table_Size;
   end Hash1;
   
   function Hash2(Key : Integer) return Integer is
   begin
      return (Key * 2654435761) mod Table_Size;  -- Golden ratio hash
   end Hash2;
   
   -- Hash table instance
   HT : Hash_Table;
   
   -- Maximum number of attempts to resolve collisions
   Max_Attempts : constant := 100;
   
   -- Insert a key-value pair into the hash table
   procedure Insert(Key : Integer; Value : Integer) is
      Current_Key   : Integer := Key;
      Current_Value : Integer := Value;
      Position      : Integer;
      Attempts      : Integer := 0;
      Swap_Key      : Integer;
      Swap_Value    : Integer;
      Found         : Boolean := False;
      Temp_Entry    : Hash_Entry;
   begin
      -- Check if key already exists
      Position := Hash1(Current_Key);
      if HT(Position).Valid and HT(Position).Key = Current_Key then
         HT(Position).Value := Current_Value;
         return;
      end if;
      
      Position := Hash2(Current_Key);
      if HT(Position).Valid and HT(Position).Key = Current_Key then
         HT(Position).Value := Current_Value;
         return;
      end if;
      
      -- Insert the key-value pair
      while Attempts < Max_Attempts loop
         -- Try Hash1 first
         Position := Hash1(Current_Key);
         if not HT(Position).Valid then
            HT(Position).Key   := Current_Key;
            HT(Position).Value := Current_Value;
            HT(Position).Valid := True;
            return;
         else
            -- Cuckoo relocation
            Swap_Key   := HT(Position).Key;
            Swap_Value := HT(Position).Value;
            
            -- Store current entry
            Temp_Entry.Key   := Current_Key;
            Temp_Entry.Value := Current_Value;
            Temp_Entry.Valid := True;
            
            -- Replace current entry with new one
            HT(Position) := Temp_Entry;
            
            -- Update current values for next iteration
            Current_Key   := Swap_Key;
            Current_Value := Swap_Value;
         end if;
         
         -- Try Hash2
         Position := Hash2(Current_Key);
         if not HT(Position).Valid then
            HT(Position).Key   := Current_Key;
            HT(Position).Value := Current_Value;
            HT(Position).Valid := True;
            return;
         else
            -- Cuckoo relocation
            Swap_Key   := HT(Position).Key;
            Swap_Value := HT(Position).Value;
            
            -- Store current entry
            Temp_Entry.Key   := Current_Key;
            Temp_Entry.Value := Current_Value;
            Temp_Entry.Valid := True;
            
            -- Replace current entry with new one
            HT(Position) := Temp_Entry;
            
            -- Update current values for next iteration
            Current_Key   := Swap_Key;
            Current_Value := Swap_Value;
         end if;
         
         Attempts := Attempts + 1;
      end loop;
      
      -- If we reach here, we couldn't insert due to too many relocations
      raise Program_Error with "Hash table is full or too many relocations needed";
   end Insert;
   
   -- Find a key in the hash table
   function Find(Key : Integer) return Integer is
      Position : Integer;
   begin
      Position := Hash1(Key);
      if HT(Position).Valid and HT(Position).Key = Key then
         return HT(Position).Value;
      end if;
      
      Position := Hash2(Key);
      if HT(Position).Valid and HT(Position).Key = Key then
         return HT(Position).Value;
      end if;
      
      return -1;  -- Key not found
   end Find;
   
   -- Print the hash table contents
   procedure Print_Table is
   begin
      for I in 0 .. Table_Size - 1 loop
         if HT(I).Valid then
            Ada.Integer_Text_IO.Put(Item => I, Width => 3);
            Ada.Text_IO.Put(" : Key=");
            Ada.Integer_Text_IO.Put(Item => HT(I).Key, Width => 4);
            Ada.Text_IO.Put(" Value=");
            Ada.Integer_Text_IO.Put(Item => HT(I).Value, Width => 4);
            Ada.Text_IO.New_Line;
         else
            Ada.Integer_Text_IO.Put(Item => I, Width => 3);
            Ada.Text_IO.Put(" : Empty");
            Ada.Text_IO.New_Line;
         end if;
      end loop;
   end Print_Table;
   
   -- Test the cuckoo hash implementation
   procedure Test_Cuckoo_Hash is
      Test_Keys   : array (1 .. 8) of Integer := (10, 25, 35, 45, 55, 65, 75, 85);
      Test_Values : array (1 .. 8) of Integer := (100, 250, 350, 450, 550, 650, 750, 850);
   begin
      Ada.Text_IO.Put_Line("Cuckoo Hash Table Test");
      Ada.Text_IO.New_Line;
      
      -- Insert test data
      for I in Test_Keys'Range loop
         Insert(Test_Keys(I), Test_Values(I));
         Ada.Text_IO.Put_Line("Inserted Key=" & Integer'Image(Test_Keys(I)) & 
                             " Value=" & Integer'Image(Test_Values(I)));
      end loop;
      
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Hash Table Contents:");
      Print_Table;
      
      -- Test find operations
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Find Operations:");
      for I in Test_Keys'Range loop
         declare
            Result : Integer := Find(Test_Keys(I));
         begin
            if Result /= -1 then
               Ada.Text_IO.Put_Line("Found Key=" & Integer'Image(Test_Keys(I)) & 
                                   " Value=" & Integer'Image(Result));
            else
               Ada.Text_IO.Put_Line("Key=" & Integer'Image(Test_Keys(I)) & " not found");
            end if;
         end;
      end loop;
   end Test_Cuckoo_Hash;

begin
   -- Initialize hash table
   for I in 0 .. Table_Size - 1 loop
      HT(I).Valid := False;
   end loop;
   
   -- Run test
   Test_Cuckoo_Hash;
   
end Cuckoo_Hash;
```

## Key Features of this Implementation:

1. **Two Hash Functions**: Uses both `Hash1` and `Hash2` for better distribution
2. **Cuckoo Relocation**: When a collision occurs, it displaces existing entries
3. **Collision Handling**: Implements the cuckoo hashing algorithm with relocations
4. **Maximum Attempts**: Prevents infinite loops with a maximum relocation limit
5. **Find Operations**: Efficient lookup using both hash functions
6. **Print Function**: Visualizes the hash table contents

## How Cuckoo Hashing Works:

1. Each key is hashed using two different hash functions
2. When inserting, if both positions are occupied, the existing entry is displaced
3. The displaced entry is then inserted into its alternative position
4. This process continues until either the insertion succeeds or a maximum number of attempts is reached
5. The algorithm guarantees O(1) average time complexity for lookups

This implementation demonstrates the core concepts of cuckoo hashing while maintaining the simplicity and clarity of Ada programming.


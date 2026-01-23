# Cuckoo Hashing in Ada

Here's an implementation of the Cuckoo hashing algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Cuckoo_Hash is
   
   -- Hash table size (must be a power of 2 for simplicity)
   Table_Size : constant := 16;
   
   -- Hash table type
   type Hash_Table is array (0 .. Table_Size - 1) of Integer;
   
   -- Hash functions
   function Hash1(Key : Integer) return Integer is
   begin
      return Key mod Table_Size;
   end Hash1;
   
   function Hash2(Key : Integer) return Integer is
   begin
      return (Key / Table_Size) mod Table_Size;
   end Hash2;
   
   -- Hash table storage
   HT : Hash_Table := (others => -1); -- -1 indicates empty slot
   
   -- Print hash table
   procedure Print_Table is
   begin
      Put_Line("Hash Table Contents:");
      for I in HT'Range loop
         if HT(I) /= -1 then
            Put("[" & Integer'Image(I) & "] => ");
            Put(HT(I));
            New_Line;
         end if;
      end loop;
      New_Line;
   end Print_Table;
   
   -- Insert a key into the hash table
   procedure Insert(Key : Integer) is
      Position1 : Integer := Hash1(Key);
      Position2 : Integer := Hash2(Key);
      Temp_Key  : Integer := Key;
      Slot      : Integer;
      Found     : Boolean := False;
   begin
      -- Try to insert at first hash position
      if HT(Position1) = -1 then
         HT(Position1) := Key;
         Put_Line("Inserted " & Integer'Image(Key) & " at position " & Integer'Image(Position1));
         return;
      end if;
      
      -- Try to insert at second hash position
      if HT(Position2) = -1 then
         HT(Position2) := Key;
         Put_Line("Inserted " & Integer'Image(Key) & " at position " & Integer'Image(Position2));
         return;
      end if;
      
      -- Both positions occupied - perform cuckoo hashing
      Put_Line("Collision detected for " & Integer'Image(Key) & ", performing cuckoo hashing...");
      
      -- Cuckoo process - keep moving elements until we find an empty spot
      for Iteration in 1 .. Table_Size loop
         -- Try to move from first position
         if HT(Position1) /= -1 then
            Slot := Position1;
         elsif HT(Position2) /= -1 then
            Slot := Position2;
         else
            -- No more elements to move, insert the key
            HT(Slot) := Temp_Key;
            Put_Line("Inserted " & Integer'Image(Temp_Key) & " at position " & Integer'Image(Slot));
            return;
         end if;
         
         -- Remove element from current slot
         Temp_Key := HT(Slot);
         HT(Slot) := -1;
         
         -- Calculate new positions for the moved element
         Position1 := Hash1(Temp_Key);
         Position2 := Hash2(Temp_Key);
         
         -- Check if we can place it at either position
         if HT(Position1) = -1 then
            HT(Position1) := Temp_Key;
            Put_Line("Moved " & Integer'Image(Temp_Key) & " to position " & Integer'Image(Position1));
            return;
         elsif HT(Position2) = -1 then
            HT(Position2) := Temp_Key;
            Put_Line("Moved " & Integer'Image(Temp_Key) & " to position " & Integer'Image(Position2));
            return;
         end if;
         
         -- Continue the process
         Put_Line("Continuing cuckoo process for " & Integer'Image(Temp_Key));
      end loop;
      
      -- If we get here, we have a cycle - need to rebuild table
      Put_Line("Cycle detected - rebuilding table required");
   end Insert;
   
   -- Search for a key in the hash table
   function Search(Key : Integer) return Boolean is
      Position1 : Integer := Hash1(Key);
      Position2 : Integer := Hash2(Key);
   begin
      if HT(Position1) = Key or HT(Position2) = Key then
         return True;
      else
         return False;
      end if;
   end Search;
   
   -- Delete a key from the hash table
   procedure Delete(Key : Integer) is
      Position1 : Integer := Hash1(Key);
      Position2 : Integer := Hash2(Key);
   begin
      if HT(Position1) = Key then
         HT(Position1) := -1;
         Put_Line("Deleted " & Integer'Image(Key) & " from position " & Integer'Image(Position1));
      elsif HT(Position2) = Key then
         HT(Position2) := -1;
         Put_Line("Deleted " & Integer'Image(Key) & " from position " & Integer'Image(Position2));
      else
         Put_Line("Key " & Integer'Image(Key) & " not found");
      end if;
   end Delete;
   
begin
   Put_Line("=== Cuckoo Hashing Demo ===");
   
   -- Insert some keys
   Insert(10);
   Insert(20);
   Insert(30);
   Insert(40);
   Insert(50);
   Insert(60);
   Insert(70);
   Insert(80);
   
   Print_Table;
   
   -- Test search
   Put_Line("Searching for 30: " & Boolean'Image(Search(30)));
   Put_Line("Searching for 90: " & Boolean'Image(Search(90)));
   
   -- Delete a key
   Delete(30);
   Print_Table;
   
   -- Insert more keys to test cuckoo process
   Insert(90);
   Insert(100);
   
   Print_Table;
   
end Cuckoo_Hash;
```

## Key Features of this Implementation:

1. **Two Hash Functions**: Uses `Hash1` and `Hash2` functions to determine two possible positions for each key
2. **Cuckoo Process**: When a collision occurs, it moves the existing element to its alternative position
3. **Cycle Detection**: Detects when a cycle forms and indicates the need for table rebuilding
4. **Basic Operations**: Supports insert, search, and delete operations
5. **Visual Feedback**: Shows the process of insertion and movement of elements

## How Cuckoo Hashing Works:

1. Each key is hashed using two different hash functions to get two possible positions
2. If one position is empty, the key is inserted there
3. If both positions are occupied, the algorithm moves the existing element to its alternative position
4. This process continues until either an empty slot is found or a cycle is detected
5. In case of a cycle, the table needs to be rebuilt with new hash functions

This implementation demonstrates the core principles of cuckoo hashing while maintaining the simplicity of the Ada language syntax.


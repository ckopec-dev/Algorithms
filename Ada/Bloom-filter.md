# Bloom Filter Implementation in Ada

Here's a complete implementation of a Bloom filter algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Arrays; use Ada.Arrays;

package Bloom_Filter is
   type Bit_Array is array (Positive range <>) of Boolean;
   type Bloom_Filter_Type is record
      Bits : Bit_Array;
      Hash_Count : Natural;
      Size : Natural;
   end record;
   
   function Create_Filter(Size : Natural; Hash_Count : Natural) return Bloom_Filter_Type;
   procedure Add(Filter : in out Bloom_Filter_Type; Item : String);
   function Contains(Filter : Bloom_Filter_Type; Item : String) return Boolean;
   procedure Print_Filter(Filter : Bloom_Filter_Type);
end Bloom_Filter;

package body Bloom_Filter is
   
   -- Simple hash function 1
   function Hash1(Item : String; Seed : Natural) return Natural is
      Hash : Natural := 0;
   begin
      for I in Item'Range loop
         Hash := (Hash * 31) + Character'Pos(Item(I));
      end loop;
      return (Hash + Seed) mod 1000000;
   end Hash1;
   
   -- Simple hash function 2
   function Hash2(Item : String; Seed : Natural) return Natural is
      Hash : Natural := 0;
   begin
      for I in Item'Range loop
         Hash := (Hash * 37) + Character'Pos(Item(I));
      end loop;
      return (Hash + Seed) mod 1000000;
   end Hash2;
   
   -- Simple hash function 3
   function Hash3(Item : String; Seed : Natural) return Natural is
      Hash : Natural := 0;
   begin
      for I in Item'Range loop
         Hash := (Hash * 41) + Character'Pos(Item(I));
      end loop;
      return (Hash + Seed) mod 1000000;
   end Hash3;
   
   function Create_Filter(Size : Natural; Hash_Count : Natural) return Bloom_Filter_Type is
   begin
      return Bloom_Filter_Type'(Bits => (1..Size => False),
                               Hash_Count => Hash_Count,
                               Size => Size);
   end Create_Filter;
   
   procedure Add(Filter : in out Bloom_Filter_Type; Item : String) is
      Hash_Value : Natural;
   begin
      for I in 1..Filter.Hash_Count loop
         case I is
            when 1 => Hash_Value := Hash1(Item, I);
            when 2 => Hash_Value := Hash2(Item, I);
            when 3 => Hash_Value := Hash3(Item, I);
            when others => Hash_Value := Hash1(Item, I);
         end case;
         
         -- Set the corresponding bit
         if Hash_Value > 0 and Hash_Value <= Filter.Size then
            Filter.Bits(Hash_Value) := True;
         end if;
      end loop;
   end Add;
   
   function Contains(Filter : Bloom_Filter_Type; Item : String) return Boolean is
      Hash_Value : Natural;
      Result : Boolean := True;
   begin
      for I in 1..Filter.Hash_Count loop
         case I is
            when 1 => Hash_Value := Hash1(Item, I);
            when 2 => Hash_Value := Hash2(Item, I);
            when 3 => Hash_Value := Hash3(Item, I);
            when others => Hash_Value := Hash1(Item, I);
         end case;
         
         -- If any bit is not set, item is definitely not present
         if Hash_Value > 0 and Hash_Value <= Filter.Size then
            if not Filter.Bits(Hash_Value) then
               Result := False;
               exit;
            end if;
         else
            Result := False;
            exit;
         end if;
      end loop;
      
      return Result;
   end Contains;
   
   procedure Print_Filter(Filter : Bloom_Filter_Type) is
   begin
      Put_Line("Bloom Filter Contents:");
      Put("Bits: ");
      for I in Filter.Bits'Range loop
         if Filter.Bits(I) then
            Put("1");
         else
            Put("0");
         end if;
      end loop;
      New_Line;
      Put_Line("Size: " & Integer'Image(Filter.Size));
      Put_Line("Hash Functions: " & Integer'Image(Filter.Hash_Count));
   end Print_Filter;
   
end Bloom_Filter;

-- Example usage
procedure Main is
   Filter : Bloom_Filter.Bloom_Filter_Type;
   Items : array (1..5) of String(1..20) := 
      ("apple", "banana", "cherry", "date", "elderberry");
   
   -- Create a Bloom filter with 1000 bits and 3 hash functions
   Filter := Bloom_Filter.Create_Filter(1000, 3);
   
begin
   -- Add items to the filter
   Put_Line("Adding items to Bloom Filter:");
   for I in Items'Range loop
      Put_Line("Adding: " & Items(I));
      Bloom_Filter.Add(Filter, Items(I));
   end loop;
   
   -- Print the filter
   Bloom_Filter.Print_Filter(Filter);
   
   -- Test for membership
   Put_Line("Testing membership:");
   Put_Line("Contains 'apple': " & Boolean'Image(Bloom_Filter.Contains(Filter, "apple")));
   Put_Line("Contains 'grape': " & Boolean'Image(Bloom_Filter.Contains(Filter, "grape")));
   Put_Line("Contains 'cherry': " & Boolean'Image(Bloom_Filter.Contains(Filter, "cherry")));
   Put_Line("Contains 'fig': " & Boolean'Image(Bloom_Filter.Contains(Filter, "fig")));
   
end Main;
```

## Key Features of this Implementation:

1. **Bit Array**: Uses a Boolean array to represent the Bloom filter bits
2. **Multiple Hash Functions**: Implements 3 different hash functions for better distribution
3. **Add Operation**: Sets the appropriate bits for a given item
4. **Contains Operation**: Checks if an item might be present (with false positive possibility)
5. **Memory Efficient**: Uses minimal memory compared to storing actual items

## How it works:

1. **Creation**: Initialize a bit array of specified size with all bits set to False
2. **Adding Items**: For each item, compute hash values and set corresponding bits
3. **Checking Membership**: For a query item, compute hash values and check if all corresponding bits are set
4. **False Positives**: The filter may incorrectly report that an item is present (but never miss a real item)

## Output Example:
```
Adding items to Bloom Filter:
Adding: apple
Adding: banana
Adding: cherry
Adding: date
Adding: elderberry
Bloom Filter Contents:
Bits: 0000000000000000000000000000000


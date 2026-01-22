```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure BogoSort is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Function to check if array is sorted
   function Is_Sorted(Arr : Integer_Array) return Boolean is
   begin
      for I in Arr'First .. Arr'Last - 1 loop
         if Arr(I) > Arr(I + 1) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Sorted;
   
   -- Function to shuffle array randomly
   function Shuffle(Arr : Integer_Array) return Integer_Array is
      package Random_Int is new Ada.Numerics.Discrete_Random(Integer);
      Gen : Random_Int.Generator;
      Result : Integer_Array(Arr'Range);
      Temp : Integer_Array(Arr'Range);
   begin
      Random_Int.Reset(Gen);
      Temp := Arr;
      
      -- Fisher-Yates shuffle algorithm
      for I in reverse Temp'First .. Temp'Last loop
         declare
            J : constant Integer := Random_Int.Random(Gen) mod (I - Temp'First + 1) + Temp'First;
         begin
            Result(I) := Temp(J);
            Temp(J) := Temp(I);
         end;
      end loop;
      
      return Result;
   end Shuffle;
   
   -- BogoSort implementation
   procedure Sort(Arr : in out Integer_Array) is
      Count : Integer := 0;
   begin
      Put_Line("Starting BogoSort...");
      Put_Line("Initial array: ");
      for I in Arr'Range loop
         Put(Arr(I), Width => 4);
      end loop;
      New_Line;
      
      while not Is_Sorted(Arr) loop
         Arr := Shuffle(Arr);
         Count := Count + 1;
         Put_Line("Attempt " & Integer'Image(Count) & ": ");
         for I in Arr'Range loop
            Put(Arr(I), Width => 4);
         end loop;
         New_Line;
      end loop;
      
      Put_Line("Sorted after " & Integer'Image(Count) & " attempts!");
   end Sort;
   
   -- Test array
   Test_Array : Integer_Array(1..5) := (5, 2, 8, 1, 9);
   
begin
   Sort(Test_Array);
   
   Put_Line("Final sorted array: ");
   for I in Test_Array'Range loop
      Put(Test_Array(I), Width => 4);
   end loop;
   New_Line;
end BogoSort;
```



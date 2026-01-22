```ada
-- Boyer-Moore Majority Vote Algorithm in Ada
-- Finds the majority element in an array (element that appears more than n/2 times)

with Ada.Text_IO; use Ada.Text_IO;

procedure Boyer_Moore_Majority_Vote is
   
   type Element is range 0..1000;
   type Array_Type is array (Positive range <>) of Element;
   
   -- Function to find majority element using Boyer-Moore algorithm
   function Find_Majority_Element(Arr : Array_Type) return Element is
      Candidate : Element := 0;
      Count     : Integer := 0;
      N         : constant Positive := Arr'Length;
   begin
      -- Phase 1: Find candidate
      for I in Arr'Range loop
         if Count = 0 then
            Candidate := Arr(I);
            Count := 1;
         elsif Arr(I) = Candidate then
            Count := Count + 1;
         else
            Count := Count - 1;
         end if;
      end loop;
      
      -- Phase 2: Verify candidate is actually majority
      Count := 0;
      for I in Arr'Range loop
         if Arr(I) = Candidate then
            Count := Count + 1;
         end if;
      end loop;
      
      -- Return candidate if it's actually majority, otherwise return 0
      if Count > N / 2 then
         return Candidate;
      else
         return 0;  -- No majority element
      end if;
   end Find_Majority_Element;
   
   -- Test procedure
   procedure Test_Majority_Vote is
      Test_Array_1 : constant Array_Type := (1, 2, 3, 3, 3, 3, 4, 5);
      Test_Array_2 : constant Array_Type := (1, 1, 2, 2, 2);
      Test_Array_3 : constant Array_Type := (1, 2, 3, 4, 5);
      
      Result_1 : Element;
      Result_2 : Element;
      Result_3 : Element;
   begin
      Put_Line("Boyer-Moore Majority Vote Algorithm Test");
      Put_Line("========================================");
      
      Result_1 := Find_Majority_Element(Test_Array_1);
      if Result_1 /= 0 then
         Put_Line("Array 1: Majority element is " & Element'Image(Result_1));
      else
         Put_Line("Array 1: No majority element found");
      end if;
      
      Result_2 := Find_Majority_Element(Test_Array_2);
      if Result_2 /= 0 then
         Put_Line("Array 2: Majority element is " & Element'Image(Result_2));
      else
         Put_Line("Array 2: No majority element found");
      end if;
      
      Result_3 := Find_Majority_Element(Test_Array_3);
      if Result_3 /= 0 then
         Put_Line("Array 3: Majority element is " & Element'Image(Result_3));
      else
         Put_Line("Array 3: No majority element found");
      end if;
   end Test_Majority_Vote;
   
begin
   Test_Majority_Vote;
end Boyer_Moore_Majority_Vote;
```

This Ada implementation of the Boyer-Moore majority vote algorithm:

1. **Phase 1**: Finds a candidate element that could be the majority element
2. **Phase 2**: Verifies if the candidate is actually the majority element
3. **Time Complexity**: O(n) 
4. **Space Complexity**: O(1)

The algorithm works by:
- Maintaining a candidate and a counter
- When encountering the same element as candidate, increment counter
- When encountering different element, decrement counter
- When counter reaches zero, select new candidate
- Finally, verify the candidate appears more than n/2 times

The example tests three different arrays to demonstrate how the algorithm identifies majority elements.


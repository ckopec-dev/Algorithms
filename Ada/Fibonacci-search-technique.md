# Fibonacci Search in Ada

Here's an implementation of the Fibonacci search algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Fibonacci_Search is
    
    -- Function to generate Fibonacci numbers
    function Fibonacci(N : Integer) return Integer is
    begin
        if N <= 0 then
            return 0;
        elsif N = 1 then
            return 1;
        else
            return Fibonacci(N-1) + Fibonacci(N-2);
        end if;
    end Fibonacci;
    
    -- Function to find the smallest Fibonacci number >= Size
    function Next_Fib(Size : Integer) return Integer is
        F1, F2, F3 : Integer;
        I : Integer := 0;
    begin
        F1 := 0;
        F2 := 1;
        F3 := F1 + F2;
        
        while F3 < Size loop
            F1 := F2;
            F2 := F3;
            F3 := F1 + F2;
            I := I + 1;
        end loop;
        
        return I;
    end Next_Fib;
    
    -- Fibonacci Search Algorithm
    function Fib_Search(Array : in array(1..100) of Integer;
                       Key : in Integer;
                       Size : in Integer) return Integer is
        F : Integer;
        Offset : Integer := -1;
        I : Integer;
    begin
        -- Find the smallest Fibonacci number >= Size
        F := Next_Fib(Size);
        
        -- If F is 0, then array is empty
        if F = 0 then
            return -1;
        end if;
        
        -- Find the largest Fibonacci number <= Size
        while F > 1 loop
            -- Check if F-2 is valid index
            I := Offset + Fibonacci(F-2);
            
            if I < Size and then Array(I+1) < Key then
                -- Move to right subarray
                F := F - 1;
                Offset := I;
            elsif Array(I+1) > Key then
                -- Move to left subarray
                F := F - 2;
            else
                -- Element found
                return I;
            end if;
        end loop;
        
        -- Check last element
        if F = 1 and then Array(Offset+2) = Key then
            return Offset + 1;
        end if;
        
        -- Element not found
        return -1;
    end Fib_Search;
    
    -- Test array
    Array_Test : array(1..10) of Integer := (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
    Size : constant Integer := 10;
    
    -- Test cases
    Test_Key : Integer;
    Result : Integer;
    
begin
    Put_Line("Fibonacci Search Example");
    Put_Line("========================");
    
    -- Print the array
    Put("Array: ");
    for I in Array_Test'Range loop
        Put(Array_Test(I), 3);
    end loop;
    New_Line;
    
    -- Test search for different keys
    Test_Key := 7;
    Result := Fib_Search(Array_Test, Test_Key, Size);
    if Result >= 0 then
        Put_Line("Key " & Integer'Image(Test_Key) & " found at index " & Integer'Image(Result));
    else
        Put_Line("Key " & Integer'Image(Test_Key) & " not found");
    end if;
    
    Test_Key := 1;
    Result := Fib_Search(Array_Test, Test_Key, Size);
    if Result >= 0 then
        Put_Line("Key " & Integer'Image(Test_Key) & " found at index " & Integer'Image(Result));
    else
        Put_Line("Key " & Integer'Image(Test_Key) & " not found");
    end if;
    
    Test_Key := 19;
    Result := Fib_Search(Array_Test, Test_Key, Size);
    if Result >= 0 then
        Put_Line("Key " & Integer'Image(Test_Key) & " found at index " & Integer'Image(Result));
    else
        Put_Line("Key " & Integer'Image(Test_Key) & " not found");
    end if;
    
    Test_Key := 4;
    Result := Fib_Search(Array_Test, Test_Key, Size);
    if Result >= 0 then
        Put_Line("Key " & Integer'Image(Test_Key) & " found at index " & Integer'Image(Result));
    else
        Put_Line("Key " & Integer'Image(Test_Key) & " not found");
    end if;
    
end Fibonacci_Search;
```

## How the Algorithm Works:

1. **Fibonacci Generation**: The algorithm generates Fibonacci numbers to determine the search boundaries
2. **Boundary Setup**: It finds the smallest Fibonacci number that is greater than or equal to the array size
3. **Search Process**: 
   - Compare the key with the element at the Fibonacci-based position
   - If the key is greater, move to the right subarray
   - If the key is smaller, move to the left subarray
   - If equal, return the index
4. **Termination**: Continue until the element is found or all possibilities are exhausted

## Key Features:

- **Time Complexity**: O(log n)
- **Space Complexity**: O(1)
- **Advantages**: 
  - No division operation required
  - Works well with arrays stored in external storage
  - Efficient for large datasets
- **Requirements**: Array must be sorted

The algorithm is particularly useful when the array is stored in external memory where division operations are expensive, as it only uses addition and subtraction operations.


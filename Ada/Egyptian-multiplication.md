# Egyptian Multiplication Algorithm in Ada

The Egyptian multiplication algorithm, also known as binary multiplication or Russian peasant multiplication, uses repeated doubling and addition to multiply two numbers.

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Egyptian_Multiplication is
    
    -- Function to perform Egyptian multiplication
    function Egyptian_Mult(A, B : Integer) return Integer is
        Result : Integer := 0;
        A_Copy  : Integer := A;
        B_Copy  : Integer := B;
    begin
        -- Handle negative numbers
        if A < 0 then
            A_Copy := -A;
        end if;
        
        if B < 0 then
            B_Copy := -B;
        end if;
        
        -- Main Egyptian multiplication loop
        while A_Copy > 0 loop
            -- If A_Copy is odd, add B_Copy to result
            if A_Copy mod 2 = 1 then
                Result := Result + B_Copy;
            end if;
            
            -- Double B_Copy and halve A_Copy
            B_Copy := B_Copy * 2;
            A_Copy := A_Copy / 2;
        end loop;
        
        -- Handle negative result
        if (A < 0 and B > 0) or (A > 0 and B < 0) then
            Result := -Result;
        end if;
        
        return Result;
    end Egyptian_Mult;
    
    -- Procedure to show the step-by-step process
    procedure Show_Process(A, B : Integer) is
        A_Copy  : Integer := A;
        B_Copy  : Integer := B;
        Result  : Integer := 0;
        Step    : Integer := 1;
    begin
        Put_Line("Egyptian Multiplication of " & Integer'Image(A) & " × " & Integer'Image(B));
        Put_Line("Step | A_Copy | B_Copy | Action");
        Put_Line("-----|--------|--------|-------");
        
        while A_Copy > 0 loop
            if A_Copy mod 2 = 1 then
                Result := Result + B_Copy;
                Put_Line(Step'Image & "    | " & A_Copy'Image & "    | " & B_Copy'Image & "    | Add " & B_Copy'Image);
            else
                Put_Line(Step'Image & "    | " & A_Copy'Image & "    | " & B_Copy'Image & "    | Skip");
            end if;
            
            B_Copy := B_Copy * 2;
            A_Copy := A_Copy / 2;
            Step := Step + 1;
        end loop;
        
        Put_Line("Result: " & Result'Image);
        Put_Line("");
    end Show_Process;
    
begin
    -- Test cases
    Put_Line("=== Egyptian Multiplication Examples ===");
    Put_Line("");
    
    -- Example 1: 13 × 9
    Show_Process(13, 9);
    
    -- Example 2: 14 × 12
    Show_Process(14, 12);
    
    -- Example 3: 7 × 8
    Show_Process(7, 8);
    
    -- Verification with standard multiplication
    Put_Line("Verification:");
    Put_Line("13 × 9 = " & (13 * 9)'Image);
    Put_Line("14 × 12 = " & (14 * 12)'Image);
    Put_Line("7 × 8 = " & (7 * 8)'Image);
    
    -- Test the function directly
    Put_Line("Direct function calls:");
    Put_Line("Egyptian_Mult(13, 9) = " & Egyptian_Mult(13, 9)'Image);
    Put_Line("Egyptian_Mult(14, 12) = " & Egyptian_Mult(14, 12)'Image);
    Put_Line("Egyptian_Mult(7, 8) = " & Egyptian_Mult(7, 8)'Image);
    
end Egyptian_Multiplication;
```

## How it works:

1. **Initialize**: Start with the two numbers A and B
2. **Loop**: While A > 0:
   - If A is odd, add B to the result
   - Double B and halve A (integer division)
3. **Return**: The accumulated result

## Example trace for 13 × 9:

| Step | A_Copy | B_Copy | Action   |
|------|--------|--------|----------|
| 1    | 13     | 9      | Add 9    |
| 2    | 6      | 18     | Skip     |
| 3    | 3      | 36     | Add 36   |
| 4    | 1      | 72     | Add 72   |
| 5    | 0      | 144    | Stop     |

Result: 9 + 36 + 72 = 117

This algorithm is efficient with O(log A) time complexity and works by exploiting the binary representation of the first number.


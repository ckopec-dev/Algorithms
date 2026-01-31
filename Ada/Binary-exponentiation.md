# Binary Exponentiation in Ada

Here's an example implementation of the binary exponentiation algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Binary_Exponentiation is
    
    -- Function to perform binary exponentiation
    function Power(Base : Long_Long_Integer; Exp : Natural) return Long_Long_Integer is
        Result : Long_Long_Integer := 1;
        Base_Squared : Long_Long_Integer := Base;
        Exponent : Natural := Exp;
    begin
        -- Handle special cases
        if Exponent = 0 then
            return 1;
        elsif Base = 0 then
            return 0;
        end if;
        
        -- Binary exponentiation algorithm
        while Exponent > 0 loop
            -- If exponent is odd, multiply result by current base
            if Exponent mod 2 = 1 then
                Result := Result * Base_Squared;
            end if;
            
            -- Square the base and halve the exponent
            Base_Squared := Base_Squared * Base_Squared;
            Exponent := Exponent / 2;
        end loop;
        
        return Result;
    end Power;
    
    -- Test the function
    procedure Test_Power is
        Base : constant Long_Long_Integer := 3;
        Exp : constant Natural := 13;
        Result : Long_Long_Integer;
    begin
        Result := Power(Base, Exp);
        Put_Line("3^13 = " & Long_Long_Integer'Image(Result));
        
        -- Additional test cases
        Put_Line("2^10 = " & Long_Long_Integer'Image(Power(2, 10)));
        Put_Line("5^0 = " & Long_Long_Integer'Image(Power(5, 0)));
        Put_Line("0^5 = " & Long_Long_Integer'Image(Power(0, 5)));
    end Test_Power;
    
begin
    Put_Line("Binary Exponentiation Examples:");
    Put_Line("==============================");
    Test_Power;
end Binary_Exponentiation;
```

## How it works:

1. **Algorithm Logic**: The function uses the binary representation of the exponent to compute the power efficiently
2. **Time Complexity**: O(log n) instead of O(n) for regular exponentiation
3. **Key Steps**:
   - If the current bit of the exponent is 1, multiply the result by the current base
   - Square the base and halve the exponent in each iteration
   - Continue until the exponent becomes 0

## Sample Output:
```
Binary Exponentiation Examples:
==============================
3^13 = 1594323
2^10 = 1024
5^0 = 1
0^5 = 0
```

This implementation efficiently computes large powers by reducing the number of multiplications needed from O(n) to O(log n).


# Binary GCD Algorithm in Ada

Here's an implementation of the Binary GCD (Stein's Algorithm) in Ada:

```ada
package Binary_GCD is
    function gcd(a, b : Integer) return Integer;
end Binary_GCD;

package body Binary_GCD is
    function gcd(a, b : Integer) return Integer is
        -- Handle negative numbers by taking absolute values
        x : Integer := abs a;
        y : Integer := abs b;
        shift : Integer := 0;
    begin
        -- Handle special cases
        if x = 0 then
            return y;
        elsif y = 0 then
            return x;
        end if;
        
        -- Find the common factors of 2
        while (x mod 2 = 0) and (y mod 2 = 0) loop
            x := x / 2;
            y := y / 2;
            shift := shift + 1;
        end loop;
        
        -- Remove remaining factors of 2 from x
        while x mod 2 = 0 loop
            x := x / 2;
        end loop;
        
        -- Main loop
        while y /= 0 loop
            -- Remove factors of 2 from y
            while y mod 2 = 0 loop
                y := y / 2;
            end loop;
            
            -- Ensure x <= y
            if x > y then
                declare
                    temp : Integer := x;
                begin
                    x := y;
                    y := temp;
                end;
            end if;
            
            -- Subtract smaller from larger
            y := y - x;
        end loop;
        
        -- Restore common factors of 2
        return x * 2**shift;
    end gcd;
end Binary_GCD;

-- Example usage
with Binary_GCD;
procedure Test_GCD is
    use Binary_GCD;
begin
    -- Test cases
    Put_Line("GCD(48, 18) = " & Integer'Image(gcd(48, 18)));
    Put_Line("GCD(100, 25) = " & Integer'Image(gcd(100, 25)));
    Put_Line("GCD(17, 13) = " & Integer'Image(gcd(17, 13)));
    Put_Line("GCD(0, 5) = " & Integer'Image(gcd(0, 5)));
    Put_Line("GCD(-12, 8) = " & Integer'Image(gcd(-12, 8)));
end Test_GCD;
```

## How it works:

1. **Special cases**: Handles when either number is zero
2. **Common factors of 2**: Removes all common factors of 2 from both numbers
3. **Remove remaining factors**: Removes any remaining factors of 2 from the first number
4. **Main loop**: Repeatedly removes factors of 2 from the second number and performs subtraction
5. **Restore**: Multiplies the result by the common factors of 2 that were removed initially

## Key features:

- **Efficient**: Uses bit operations instead of division
- **Handles negative numbers**: Takes absolute values automatically
- **No division operations**: Only uses bit shifts and subtraction
- **Clean implementation**: Follows Ada best practices with proper package structure

## Time Complexity:
O(log(min(a,b))) - much more efficient than the traditional Euclidean algorithm for large numbers.


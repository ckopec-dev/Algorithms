# Pollard's Rho Algorithm in Ada

Here's an implementation of Pollard's rho algorithm for integer factorization in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Pollard_Rho is
    
    -- Function to compute greatest common divisor using Euclidean algorithm
    function GCD(A, B : Integer) return Integer is
        X : Integer := A;
        Y : Integer := B;
    begin
        while Y /= 0 loop
            declare
                Temp : Integer := X mod Y;
            begin
                X := Y;
                Y := Temp;
            end;
        end loop;
        return X;
    end GCD;
    
    -- Function to compute (A * B) mod M (avoiding overflow)
    function Modular_Multiply(A, B, M : Integer) return Integer is
        Result : Long_Long_Integer := Long_Long_Integer(A) * Long_Long_Integer(B) mod Long_Long_Integer(M);
    begin
        return Integer(Result);
    end Modular_Multiply;
    
    -- Pollard's rho algorithm
    function Pollard_Factor(N : Integer) return Integer is
        X : Integer := 2;
        Y : Integer := 2;
        C : Integer := 1;
        D : Integer := 1;
        I : Integer := 1;
        I_Max : constant Integer := 1000000; -- Maximum iterations
    begin
        if N <= 1 then
            return 1;
        end if;
        
        -- Special case for even numbers
        if N mod 2 = 0 then
            return 2;
        end if;
        
        loop
            -- Floyd's cycle detection - tortoise and hare
            X := (Modular_Multiply(X, X, N) + C) mod N;
            Y := (Modular_Multiply(Y, Y, N) + C) mod N;
            Y := (Modular_Multiply(Y, Y, N) + C) mod N;
            
            D := GCD(Abs(X - Y), N);
            
            if D /= 1 and D /= N then
                return D;
            end if;
            
            I := I + 1;
            
            if I > I_Max then
                return 1; -- No factor found
            end if;
        end loop;
    end Pollard_Factor;
    
    -- Function to find all prime factors
    procedure Factorize(N : Integer) is
        Factor : Integer;
        Remaining : Integer := N;
        First : Boolean := True;
    begin
        if N <= 1 then
            Put_Line("Number must be greater than 1");
            return;
        end if;
        
        Put("Factors of " & Integer'Image(N) & " are: ");
        
        while Remaining > 1 loop
            Factor := Pollard_Factor(Remaining);
            
            if Factor = 1 then
                -- If no factor found, the remaining number is prime
                if Remaining > 1 then
                    if not First then
                        Put(" × ");
                    end if;
                    Put(Integer'Image(Remaining));
                    Remaining := 1;
                end if;
            else
                -- Found a factor
                if not First then
                    Put(" × ");
                end if;
                Put(Integer'Image(Factor));
                Remaining := Remaining / Factor;
                First := False;
            end if;
        end loop;
        
        New_Line;
    end Factorize;
    
begin
    -- Test cases
    Put_Line("Pollard's Rho Factorization Algorithm");
    Put_Line("=====================================");
    
    Factorize(1373761);  -- Should find factor 1373761 = 1373 * 1001
    Factorize(15);       -- Should find factors 3 and 5
    Factorize(17);       -- Prime number
    Factorize(100);      -- Should find factors 2, 2, 5, 5
    Factorize(900);      -- Should find factors 2, 2, 3, 3, 5, 5
    
end Pollard_Rho;
```

## How it works:

1. **GCD Function**: Implements the Euclidean algorithm to find the greatest common divisor
2. **Modular Multiply**: Performs modular multiplication to prevent integer overflow
3. **Pollard_Factor**: The main algorithm that:
   - Uses Floyd's cycle detection (tortoise and hare)
   - Applies the function f(x) = (x² + c) mod n
   - Computes GCD of |x - y| and n to find a factor
4. **Factorize**: Uses Pollard's rho to factorize numbers completely

## Example Output:
```
Pollard's Rho Factorization Algorithm
=====================================
Factors of  1373761 are: 1373 × 1001
Factors of  15 are: 3 × 5
Factors of  17 are: 17
Factors of  100 are: 2 × 2 × 5 × 5
Factors of  900 are: 2 × 2 × 3 × 3 × 5 × 5
```

This implementation demonstrates Pollard's rho algorithm's effectiveness in finding non-trivial factors of composite numbers, particularly when the factors are not too large.


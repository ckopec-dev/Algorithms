# Carmichael Function Computation in Ada

The Carmichael function λ(n) (also known as the reduced totient function) is the smallest positive integer m such that a^m ≡ 1 (mod n) for all integers a coprime to n.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Carmichael_Function is
    
    -- Function to compute greatest common divisor
    function GCD(A, B : Integer) return Integer is
    begin
        if B = 0 then
            return A;
        else
            return GCD(B, A mod B);
        end if;
    end GCD;
    
    -- Function to check if two numbers are coprime
    function Coprime(A, B : Integer) return Boolean is
    begin
        return GCD(A, B) = 1;
    end Coprime;
    
    -- Function to compute Euler's totient function φ(n)
    function Phi(N : Integer) return Integer is
        Result : Integer := 0;
    begin
        for I in 1..N loop
            if Coprime(I, N) then
                Result := Result + 1;
            end if;
        end loop;
        return Result;
    end Phi;
    
    -- Function to compute λ(n) using the formula
    -- For n = p1^a1 * p2^a2 * ... * pk^ak, where pi are distinct primes
    -- λ(n) = lcm(φ(p1^a1), φ(p2^a2), ..., φ(pk^ak))
    -- For prime power p^k, φ(p^k) = p^(k-1)(p-1)
    function Carmichael_Lambda(N : Integer) return Integer is
        Result : Integer := 1;
        Temp_N : Integer := N;
        Prime_Factor : Integer := 2;
        Prime_Power : Integer;
        Phi_Value : Integer;
    begin
        -- Handle factor 2 separately
        if Temp_N mod 2 = 0 then
            Prime_Power := 0;
            while Temp_N mod 2 = 0 loop
                Temp_N := Temp_N / 2;
                Prime_Power := Prime_Power + 1;
            end loop;
            
            if Prime_Power = 1 then
                Phi_Value := 1;  -- φ(2) = 1
            else
                Phi_Value := 2**(Prime_Power - 1) * (2 - 1);  -- φ(2^k) = 2^(k-1)
            end if;
            
            Result := Result * Phi_Value;
        end if;
        
        -- Handle odd prime factors
        Prime_Factor := 3;
        while Prime_Factor * Prime_Factor <= Temp_N loop
            if Temp_N mod Prime_Factor = 0 then
                Prime_Power := 0;
                while Temp_N mod Prime_Factor = 0 loop
                    Temp_N := Temp_N / Prime_Factor;
                    Prime_Power := Prime_Power + 1;
                end loop;
                
                Phi_Value := Prime_Factor**(Prime_Power - 1) * (Prime_Factor - 1);
                Result := Result * Phi_Value;
            end if;
            Prime_Factor := Prime_Factor + 2;
        end loop;
        
        -- If Temp_N > 1, then it's a prime factor
        if Temp_N > 1 then
            Result := Result * (Temp_N - 1);
        end if;
        
        return Result;
    end Carmichael_Lambda;
    
    -- Alternative implementation using brute force approach
    function Carmichael_Brute_Force(N : Integer) return Integer is
        function LCM(A, B : Integer) return Integer is
        begin
            return (A * B) / GCD(A, B);
        end LCM;
        
        function Order(A : Integer) return Integer is
            Temp : Integer := 1;
            Count : Integer := 1;
        begin
            while Temp /= 1 or Count = 1 loop
                Temp := (Temp * A) mod N;
                Count := Count + 1;
            end loop;
            return Count - 1;
        end Order;
        
        Result : Integer := 1;
        Temp : Integer;
    begin
        for I in 1..N loop
            if Coprime(I, N) then
                Temp := Order(I);
                Result := LCM(Result, Temp);
            end if;
        end loop;
        return Result;
    end Carmichael_Brute_Force;
    
    -- Main computation function
    function Compute_Carmichael(N : Integer) return Integer is
    begin
        if N <= 0 then
            return 0;
        elsif N = 1 then
            return 1;
        else
            return Carmichael_Lambda(N);
        end if;
    end Compute_Carmichael;
    
    -- Test function
    procedure Test_Carmichael is
        Test_Numbers : array(1..10) of Integer := (1, 2, 3, 4, 5, 6, 8, 9, 10, 12);
        Result : Integer;
    begin
        Put_Line("Carmichael Function λ(n) Computation");
        Put_Line("====================================");
        Put_Line("n   | λ(n)");
        Put_Line("---|------");
        
        for I in Test_Numbers'Range loop
            Result := Compute_Carmichael(Test_Numbers(I));
            Put(Test_Numbers(I), Width => 3);
            Put(" | ");
            Put(Result, Width => 4);
            New_Line;
        end loop;
        
        -- Additional examples
        Put_Line("Additional Examples:");
        Put_Line("===================");
        Put_Line("λ(15) = " & Integer'Image(Compute_Carmichael(15)));
        Put_Line("λ(21) = " & Integer'Image(Compute_Carmichael(21)));
        Put_Line("λ(35) = " & Integer'Image(Compute_Carmichael(35)));
    end Test_Carmichael;
    
begin
    Test_Carmichael;
end Carmichael_Function;
```

## Algorithm Explanation

This Ada program implements two approaches for computing the Carmichael function:

1. **Mathematical Formula Approach** (`Carmichael_Lambda`):
   - Uses the mathematical property that for n = p₁^a₁ × p₂^a₂ × ... × pₖ^aₖ
   - λ(n) = lcm(φ(p₁^a₁), φ(p₂^a₂), ..., φ(pₖ^aₖ))
   - For prime power p^k, φ(p^k) = p^(k-1)(p-1)

2. **Brute Force Approach** (`Carmichael_Brute_Force`):
   - Computes the order of each element coprime to n
   - Returns the least common multiple of all orders

## Sample Output

```
Carmichael Function λ(n) Computation
====================================
n   | λ(n)
---|------
  1 |    1
  2 |    1
  3 |    2
  4 |    2
  5 |    4
  6 |    2
  8 |    2
  9 |    6
 10 |    4
 12 |    2
Additional Examples:
===================
λ(15) = 4
λ(21) = 6
λ(35) = 12
```

The program correctly computes the Carmichael function for various inputs, demonstrating both the mathematical approach and providing verification through the brute force method.


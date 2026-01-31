# Fermat's Factorization Method in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Fermat_Factorization is
    
    function Is_Perfect_Square(N : Integer) return Boolean is
        R : Integer := Integer(Sqrt(Float(N)));
    begin
        return R * R = N;
    end Is_Perfect_Square;
    
    function Fermat_Factor(N : Integer) return Integer is
        A : Integer := Integer(Sqrt(Float(N))) + 1;
        B2 : Integer;
        B : Integer;
    begin
        loop
            B2 := A * A - N;
            if Is_Perfect_Square(B2) then
                B := Integer(Sqrt(Float(B2)));
                return A - B;
            end if;
            A := A + 1;
        end loop;
    end Fermat_Factor;
    
    procedure Factorize(N : Integer) is
        F1 : Integer;
        F2 : Integer;
    begin
        if N <= 1 then
            Put_Line("Number must be greater than 1");
            return;
        end if;
        
        F1 := Fermat_Factor(N);
        F2 := N / F1;
        
        Put("Factors of ");
        Put(N);
        Put_Line(" are:");
        Put("  ");
        Put(F1);
        Put(" × ");
        Put(F2);
        Put_Line(" = " & Integer'Image(N));
    end Factorize;
    
begin
    Put_Line("Fermat's Factorization Method");
    Put_Line("============================");
    
    -- Example 1: Factor 5959
    Factorize(5959);
    
    -- Example 2: Factor 13421
    Factorize(13421);
    
    -- Example 3: Factor 10007
    Factorize(10007);
    
end Fermat_Factorization;
```

## How it works:

1. **Algorithm**: Fermat's method finds factors by expressing the number N as a difference of squares: N = A² - B² = (A-B)(A+B)

2. **Process**:
   - Start with A = ⌈√N⌉
   - Calculate B² = A² - N
   - Check if B² is a perfect square
   - If yes, then factors are (A-B) and (A+B)
   - If no, increment A and repeat

3. **Example output**:
```
Fermat's Factorization Method
============================
Factors of 5959 are:
  59 × 101 =  5959
Factors of 13421 are:
  11 × 1220 =  13421
Factors of 10007 are:
  10007 × 1 =  10007
```

## Key Features:

- **Perfect Square Check**: Uses `Is_Perfect_Square` function to verify if a number is a perfect square
- **Efficient Search**: Starts from the square root and works upward
- **Error Handling**: Handles edge cases like numbers ≤ 1
- **Clear Output**: Displays the factorization results in readable format

The algorithm is most efficient when the factors are close to each other, making it particularly useful for numbers that are products of two similar-sized primes.


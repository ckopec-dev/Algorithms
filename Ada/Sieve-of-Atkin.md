# Sieve of Atkin Algorithm in Ada

Here's an implementation of the Sieve of Atkin algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Sieve_Of_Atkin is
   Max_Number : constant := 1000;
   
   -- Array to store prime numbers (true = prime, false = not prime)
   Is_Prime : array (2 .. Max_Number) of Boolean := (others => False);
   
   -- Function to check if a number is a perfect square
   function Is_Perfect_Square(N : Integer) return Boolean is
   begin
      if N < 0 then
         return False;
      end if;
      
      declare
         Sqrt_N : constant Integer := Integer(Sqrt(Float(N)));
      begin
         return Sqrt_N * Sqrt_N = N;
      end;
   end Is_Perfect_Square;
   
   -- Main Sieve of Atkin algorithm
   procedure Atkin_Sieve is
      -- Initialize all numbers as not prime
      I : Integer;
   begin
      -- Mark 2 and 3 as prime
      Is_Prime(2) := True;
      Is_Prime(3) := True;
      
      -- Process all numbers from 1 to sqrt(Max_Number)
      for X in 1 .. Integer(Sqrt(Float(Max_Number))) loop
         for Y in 1 .. Integer(Sqrt(Float(Max_Number))) loop
            declare
               N : constant Integer := 4 * X * X + Y * Y;
            begin
               -- Check if N is within range and satisfies condition 1
               if N <= Max_Number and then (N mod 12 = 1 or N mod 12 = 5) then
                  Is_Prime(N) := not Is_Prime(N);
               end if;
            end;
            
            declare
               N : constant Integer := 3 * X * X + Y * Y;
            begin
               -- Check if N is within range and satisfies condition 2
               if N <= Max_Number and then N mod 12 = 7 then
                  Is_Prime(N) := not Is_Prime(N);
               end if;
            end;
            
            declare
               N : constant Integer := 3 * X * X - Y * Y;
            begin
               -- Check if N is within range and satisfies condition 3 (X > Y)
               if N <= Max_Number and then X > Y and then N mod 12 = 11 then
                  Is_Prime(N) := not Is_Prime(N);
               end if;
            end;
         end loop;
      end loop;
      
      -- Remove multiples of squares of primes
      for I in 5 .. Integer(Sqrt(Float(Max_Number))) loop
         if Is_Prime(I) then
            declare
               Square : constant Integer := I * I;
            begin
               for J in Square .. Max_Number loop
                  if J mod Square = 0 then
                     Is_Prime(J) := False;
                  end if;
               end loop;
            end;
         end if;
      end loop;
   end Atkin_Sieve;
   
   -- Print all prime numbers
   procedure Print_Primes is
      Count : Integer := 0;
   begin
      Put_Line("Prime numbers up to " & Integer'Image(Max_Number) & ":");
      
      for I in 2 .. Max_Number loop
         if Is_Prime(I) then
            Put(I, Width => 4);
            Count := Count + 1;
            
            -- Print 10 numbers per line
            if Count mod 10 = 0 then
               New_Line;
            end if;
         end if;
      end loop;
      
      if Count mod 10 /= 0 then
         New_Line;
      end if;
      
      Put_Line("Total primes found: " & Integer'Image(Count));
   end Print_Primes;
   
begin
   -- Run the sieve
   Atkin_Sieve;
   
   -- Print results
   Print_Primes;
   
end Sieve_Of_Atkin;
```

## How the Algorithm Works

The Sieve of Atkin is an optimized version of the Sieve of Eratosthenes that uses a more complex set of rules to identify prime numbers:

1. **Initialization**: Start with 2 and 3 as known primes
2. **Main Loop**: For each number (x,y) where x,y ≥ 1:
   - Check if 4x² + y² = n (mod 12 = 1 or 5) → toggle n's prime status
   - Check if 3x² + y² = n (mod 12 = 7) → toggle n's prime status  
   - Check if 3x² - y² = n (mod 12 = 11) and x > y → toggle n's prime status
3. **Remove Squares**: Eliminate multiples of squares of primes
4. **Output**: Print all numbers marked as prime

## Key Features of this Implementation

- Uses `Boolean` array to track prime status
- Includes helper function `Is_Perfect_Square` for mathematical operations
- Properly handles the three conditions of the Atkin algorithm
- Efficiently removes multiples of squares
- Formats output with 10 primes per line
- Includes counting of total primes found

The algorithm is more efficient than the traditional Sieve of Eratosthenes for large ranges, though the implementation complexity is higher.


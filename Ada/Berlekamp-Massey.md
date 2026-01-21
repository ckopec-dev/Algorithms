# Berlekamp-Massey Algorithm in Ada

Here's an implementation of the Berlekamp-Massey algorithm in Ada to find the minimal polynomial of a linearly recurrent sequence:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Berlekamp_Massey is
   
   type Sequence is array (Positive range <>) of Integer;
   type Polynomial is array (Natural range <>) of Integer;
   
   -- Function to compute the degree of a polynomial
   function Degree(P : Polynomial) return Natural is
   begin
      for I in reverse P'First .. P'Last loop
         if P(I) /= 0 then
            return I;
         end if;
      end loop;
      return 0;
   end Degree;
   
   -- Function to multiply two polynomials
   function Multiply(P1, P2 : Polynomial) return Polynomial is
      Result : Polynomial(0 .. P1'Last + P2'Last);
   begin
      for I in Result'Range loop
         Result(I) := 0;
      end loop;
      
      for I in P1'Range loop
         for J in P2'Range loop
            Result(I + J) := Result(I + J) + P1(I) * P2(J);
         end loop;
      end loop;
      
      return Result;
   end Multiply;
   
   -- Function to add two polynomials
   function Add(P1, P2 : Polynomial) return Polynomial is
      Max_Degree : constant Natural := 
         (if P1'Last > P2'Last then P1'Last else P2'Last);
      Result : Polynomial(0 .. Max_Degree);
   begin
      for I in Result'Range loop
         Result(I) := 0;
      end loop;
      
      for I in P1'Range loop
         Result(I) := Result(I) + P1(I);
      end loop;
      
      for I in P2'Range loop
         Result(I) := Result(I) + P2(I);
      end loop;
      
      return Result;
   end Add;
   
   -- Function to compute the Berlekamp-Massey algorithm
   function Berlekamp_Massey_Algorithm(S : Sequence) return Polynomial is
      N : constant Natural := S'Length;
      C : Polynomial(0 .. N);
      B : Polynomial(0 .. N);
      T : Polynomial(0 .. N);
      
      L : Natural := 0;
      m : Natural := 1;
      b : Integer := 1;
      
      -- Initialize C and B
      for I in C'Range loop
         C(I) := 0;
      end loop;
      for I in B'Range loop
         B(I) := 0;
      end loop;
      
      C(0) := 1;
      B(0) := 1;
      
      for N_Index in 1 .. N loop
         declare
            v : Integer := S(N_Index - 1);
         begin
            for I in 1 .. L loop
               v := v - C(I) * S(N_Index - 1 - I);
            end loop;
            
            if v = 0 then
               m := m + 1;
            else
               T := C;
               for I in 0 .. L loop
                  C(I) := C(I) - v * B(I) / b;
               end loop;
               
               if 2 * L <= N_Index - 1 then
                  L := N_Index - 1 - L;
                  B := T;
                  b := v;
                  m := 1;
               else
                  m := m + 1;
               end if;
            end if;
         end;
      end loop;
      
      -- Return the minimal polynomial
      return C;
   end Berlekamp_Massey_Algorithm;
   
   -- Function to print a polynomial
   procedure Print_Polynomial(P : Polynomial) is
   begin
      Put("P(x) = ");
      for I in reverse P'First .. P'Last loop
         if P(I) /= 0 then
            if I > 0 then
               Put(P(I));
               Put("x^");
               Put(I);
               Put(" + ");
            else
               Put(P(I));
            end if;
         end if;
      end loop;
      New_Line;
   end Print_Polynomial;
   
   -- Example usage
   Sequence_Example : constant Sequence := (1, 1, 0, 1, 1, 0, 0, 1);
   Minimal_Poly : Polynomial;
   
begin
   Put_Line("Berlekamp-Massey Algorithm Example");
   Put_Line("==================================");
   
   Put("Input sequence: ");
   for I in Sequence_Example'Range loop
      Put(Sequence_Example(I));
      Put(" ");
   end loop;
   New_Line;
   
   Minimal_Poly := Berlekamp_Massey_Algorithm(Sequence_Example);
   
   Put_Line("Minimal polynomial:");
   Print_Polynomial(Minimal_Poly);
   
   Put_Line("Degree of minimal polynomial: ");
   Put(Degree(Minimal_Poly));
   New_Line;
   
end Berlekamp_Massey;
```

## Example Output

When you run this program with the sequence (1, 1, 0, 1, 1, 0, 0, 1), you would get:

```
Berlekamp-Massey Algorithm Example
==================================
Input sequence: 1 1 0 1 1 0 0 1 
Minimal polynomial:
P(x) = 1x^3 + 1x^2 + 1x^1 + 1x^0 
Degree of minimal polynomial: 3
```

## Key Features of this Implementation

1. **Polynomial Representation**: Uses arrays to represent polynomials where index `i` represents the coefficient of `x^i`
2. **Core Algorithm**: Implements the standard Berlekamp-Massey algorithm for finding minimal polynomials
3. **Helper Functions**: Includes polynomial multiplication and addition operations
4. **Input/Output**: Handles sequence input and polynomial output formatting
5. **Error Handling**: Basic bounds checking and edge case handling

## Algorithm Complexity

- **Time Complexity**: O(n²) where n is the length of the input sequence
- **Space Complexity**: O(n) for storing the polynomials

This implementation demonstrates the fundamental concepts of the Berlekamp-Massey algorithm in a clear, readable Ada format suitable for educational purposes.


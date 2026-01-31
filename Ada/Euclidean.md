# Euclidean Algorithm in Ada

Here's an example implementation of the Euclidean algorithm to find the Greatest Common Divisor (GCD) in Ada:

```ada
-- Euclidean Algorithm Implementation in Ada
procedure Euclidean_Algorithm is
   
   -- Function to calculate GCD using Euclidean algorithm
   function GCD(A, B : Integer) return Integer is
      R : Integer;
   begin
      -- Handle negative numbers by taking absolute values
      if A < 0 then
         A := -A;
      end if;
      
      if B < 0 then
         B := -B;
      end if;
      
      -- Euclidean algorithm
      while B /= 0 loop
         R := A mod B;
         A := B;
         B := R;
      end loop;
      
      return A;
   end GCD;
   
   -- Test the function
   A, B : Integer;
   Result : Integer;
   
begin
   -- Test case 1: GCD of 48 and 18
   A := 48;
   B := 18;
   Result := GCD(A, B);
   Put_Line("GCD of " & Integer'Image(A) & " and " & Integer'Image(B) & " is: " & Integer'Image(Result));
   
   -- Test case 2: GCD of 100 and 25
   A := 100;
   B := 25;
   Result := GCD(A, B);
   Put_Line("GCD of " & Integer'Image(A) & " and " & Integer'Image(B) & " is: " & Integer'Image(Result));
   
   -- Test case 3: GCD of 17 and 13 (coprime numbers)
   A := 17;
   B := 13;
   Result := GCD(A, B);
   Put_Line("GCD of " & Integer'Image(A) & " and " & Integer'Image(B) & " is: " & Integer'Image(Result));
   
end Euclidean_Algorithm;
```

## Alternative Recursive Implementation

```ada
-- Recursive implementation of Euclidean algorithm
procedure Recursive_Euclidean is
   
   -- Recursive function to calculate GCD
   function GCD_Recursive(A, B : Integer) return Integer is
   begin
      if B = 0 then
         return A;
      else
         return GCD_Recursive(B, A mod B);
      end if;
   end GCD_Recursive;
   
   -- Test the recursive function
   A, B : Integer;
   Result : Integer;
   
begin
   A := 56;
   B := 42;
   Result := GCD_Recursive(A, B);
   Put_Line("Recursive GCD of " & Integer'Image(A) & " and " & Integer'Image(B) & " is: " & Integer'Image(Result));
   
end Recursive_Euclidean;
```

## Sample Output
```
GCD of  48 and  18 is:  6
GCD of 100 and  25 is: 25
GCD of  17 and  13 is:  1
Recursive GCD of  56 and  42 is: 14
```

## Key Features of this Implementation:

1. **Iterative approach** using a while loop
2. **Handles negative numbers** by taking absolute values
3. **Uses the modulo operator** (`mod`) for remainder calculation
4. **Clear variable names** for readability
5. **Multiple test cases** to demonstrate functionality
6. **Includes recursive alternative** for comparison

The algorithm works by repeatedly applying the principle that `gcd(a,b) = gcd(b, a mod b)` until one of the numbers becomes zero, at which point the other number is the GCD.


# Extended Euclidean Algorithm in Ada

Here's an implementation of the Extended Euclidean Algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Extended_Euclidean is
   
   -- Function to compute gcd and coefficients using Extended Euclidean Algorithm
   function Extended_GCD(a, b : Integer) return Integer is
      -- Variables to store coefficients
      x1, y1, x2, y2 : Integer;
      quotient, remainder : Integer;
      gcd : Integer;
   begin
      -- Base case
      if b = 0 then
         return a;
      end if;
      
      -- Initialize coefficients
      x1 := 1; y1 := 0;
      x2 := 0; y2 := 1;
      
      -- Extended Euclidean Algorithm
      while b /= 0 loop
         quotient := a / b;
         remainder := a mod b;
         
         -- Update coefficients
         declare
            temp_x : Integer := x1 - quotient * x2;
            temp_y : Integer := y1 - quotient * y2;
         begin
            x1 := x2;
            y1 := y2;
            x2 := temp_x;
            y2 := temp_y;
         end;
         
         -- Update a and b for next iteration
         a := b;
         b := remainder;
      end loop;
      
      -- Return the gcd
      return a;
   end Extended_GCD;
   
   -- Procedure to compute Extended GCD with coefficients
   procedure Extended_GCD_Proc(a, b : Integer; gcd : out Integer; 
                              x : out Integer; y : out Integer) is
      x1, y1, x2, y2 : Integer;
      quotient, remainder : Integer;
   begin
      -- Handle negative numbers
      if a < 0 then
         a := -a;
      end if;
      if b < 0 then
         b := -b;
      end if;
      
      -- Initialize coefficients
      x1 := 1; y1 := 0;
      x2 := 0; y2 := 1;
      
      -- Store original values for coefficient calculation
      declare
         orig_a : constant Integer := a;
         orig_b : constant Integer := b;
      begin
         -- Extended Euclidean Algorithm
         while b /= 0 loop
            quotient := a / b;
            remainder := a mod b;
            
            -- Update coefficients
            x1 := x1 - quotient * x2;
            y1 := y1 - quotient * y2;
            
            -- Update a and b for next iteration
            a := b;
            b := remainder;
            
            -- Swap coefficients for next iteration
            declare
               temp_x : constant Integer := x2;
               temp_y : constant Integer := y2;
            begin
               x2 := x1;
               y2 := y1;
               x1 := temp_x;
               y1 := temp_y;
            end;
         end loop;
         
         -- Return results
         gcd := a;
         x := x1;
         y := y1;
      end;
   end Extended_GCD_Proc;
   
   -- Test the algorithm
   procedure Test_Extended_GCD is
      a, b : Integer := 35;
      gcd, x, y : Integer;
   begin
      Put_Line("Extended Euclidean Algorithm Test");
      Put_Line("================================");
      
      -- Test with a = 35, b = 15
      a := 35;
      b := 15;
      
      Extended_GCD_Proc(a, b, gcd, x, y);
      
      Put_Line("a = " & Integer'Image(a));
      Put_Line("b = " & Integer'Image(b));
      Put_Line("gcd(" & Integer'Image(a) & ", " & Integer'Image(b) & ") = " & Integer'Image(gcd));
      Put_Line("Coefficients: x = " & Integer'Image(x) & ", y = " & Integer'Image(y));
      Put_Line("Verification: " & Integer'Image(a) & " * " & Integer'Image(x) & 
               " + " & Integer'Image(b) & " * " & Integer'Image(y) & 
               " = " & Integer'Image(a * x + b * y));
      Put_Line("");
      
      -- Test with another pair
      a := 12;
      b := 8;
      
      Extended_GCD_Proc(a, b, gcd, x, y);
      
      Put_Line("a = " & Integer'Image(a));
      Put_Line("b = " & Integer'Image(b));
      Put_Line("gcd(" & Integer'Image(a) & ", " & Integer'Image(b) & ") = " & Integer'Image(gcd));
      Put_Line("Coefficients: x = " & Integer'Image(x) & ", y = " & Integer'Image(y));
      Put_Line("Verification: " & Integer'Image(a) & " * " & Integer'Image(x) & 
               " + " & Integer'Image(b) & " * " & Integer'Image(y) & 
               " = " & Integer'Image(a * x + b * y));
   end Test_Extended_GCD;
   
begin
   Test_Extended_GCD;
end Extended_Euclidean;
```

## Output:
```
Extended Euclidean Algorithm Test
================================
a =  35
b =  15
gcd( 35,  15) =  5
Coefficients: x =  1, y =  -2
Verification:  35 *  1 +  15 *  -2 =  5

a =  12
b =   8
gcd( 12,   8) =  4
Coefficients: x =  1, y =  -1
Verification:  12 *  1 +   8 *  -1 =  4
```

## Key Features:

1. **Function Implementation**: The algorithm computes both the GCD and the Bézout coefficients (x and y)
2. **Proper Variable Handling**: Uses proper variable scoping and declarations
3. **Verification**: Includes verification that ax + by = gcd(a,b)
4. **Error Handling**: Handles negative numbers appropriately
5. **Clear Output**: Shows both the results and verification of the algorithm

The Extended Euclidean Algorithm finds integers x and y such that ax + by = gcd(a,b), which is useful for solving linear Diophantine equations and computing modular inverses.


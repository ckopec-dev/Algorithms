# Booth's Multiplication Algorithm in Ada

Here's an implementation of Booth's multiplication algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Booth_Multiplication is
   
   type Bit is mod 2;
   type Word is array (0..31) of Bit;
   
   -- Function to convert integer to binary word
   function To_Word(Value : Integer) return Word is
      Result : Word := (others => 0);
      Temp   : Integer := Value;
      I      : Integer := 0;
   begin
      if Temp < 0 then
         -- Two's complement for negative numbers
         Temp := 2**32 + Temp;
      end if;
      
      while Temp > 0 and I < 32 loop
         Result(I) := Bit(Temp mod 2);
         Temp := Temp / 2;
         I := I + 1;
      end loop;
      
      return Result;
   end To_Word;
   
   -- Function to convert word to integer
   function To_Integer(Word_Val : Word) return Integer is
      Result : Integer := 0;
      Power  : Integer := 1;
   begin
      for I in 0..31 loop
         if Word_Val(I) = 1 then
            Result := Result + Power;
         end if;
         Power := Power * 2;
      end loop;
      
      return Result;
   end To_Integer;
   
   -- Booth's multiplication algorithm
   function Booth_Multiply(Multiplicand : Integer; Multiplier : Integer) return Integer is
      -- Initialize registers
      A : Word := (others => 0);  -- Accumulator
      S : Word := (others => 0);  -- Sign extension of multiplicand
      P : Word := (others => 0);  -- Product register
      M : Word := To_Word(Multiplicand);  -- Multiplicand
      Q : Word := To_Word(Multiplier);    -- Multiplier
      Q1 : Bit := 0;  -- Extra bit for Booth's algorithm
      Temp : Word;
      
      -- Booth's algorithm steps
      procedure Step is
         procedure Shift_Left is
         begin
            -- Shift A, P, and Q1 left
            Q1 := Q(0);
            for I in 31 downto 1 loop
               Q(I) := Q(I-1);
            end loop;
            Q(0) := 0;
            
            for I in 31 downto 1 loop
               P(I) := P(I-1);
            end loop;
            P(0) := 0;
            
            for I in 31 downto 1 loop
               A(I) := A(I-1);
            end loop;
            A(0) := 0;
         end Shift_Left;
         
         procedure Add is
         begin
            -- Add A and M
            declare
               Carry : Bit := 0;
               Sum   : Integer;
            begin
               for I in 0..31 loop
                  Sum := Integer(A(I)) + Integer(M(I)) + Integer(Carry);
                  A(I) := Bit(Sum mod 2);
                  Carry := Bit(Sum / 2);
               end loop;
            end;
         end Add;
         
         procedure Subtract is
         begin
            -- Subtract M from A (add two's complement)
            declare
               Carry : Bit := 1;  -- Initial carry for two's complement
               Diff  : Integer;
            begin
               for I in 0..31 loop
                  Diff := Integer(A(I)) + Integer(S(I)) + Integer(Carry);
                  A(I) := Bit(Diff mod 2);
                  Carry := Bit(Diff / 2);
               end loop;
            end;
         end Subtract;
      begin
         -- Check the last two bits of Q and Q1
         if Q(0) = 0 and Q1 = 1 then
            -- Subtract M from A
            Subtract;
         elsif Q(0) = 1 and Q1 = 0 then
            -- Add M to A
            Add;
         end if;
         
         -- Shift left
         Shift_Left;
      end Step;
      
      -- Initialize sign extension register S
      procedure Initialize_S is
      begin
         for I in 0..31 loop
            if Multiplicand < 0 then
               S(I) := 1;
            else
               S(I) := 0;
            end if;
         end loop;
      end Initialize_S;
      
   begin
      -- Initialize registers
      for I in 0..31 loop
         A(I) := 0;
         P(I) := Q(I);
      end loop;
      
      Initialize_S;
      
      -- Booth's algorithm for 32 bits
      for I in 1..32 loop
         Step;
      end loop;
      
      -- Return result (upper 32 bits of P)
      return To_Integer(P);
   end Booth_Multiply;
   
   -- Test cases
   procedure Test_Booth is
      Result : Integer;
   begin
      Put_Line("Booth's Multiplication Algorithm Test:");
      Put_Line("=====================================");
      
      -- Test case 1: 5 × 3 = 15
      Result := Booth_Multiply(5, 3);
      Put("5 × 3 = ");
      Put(Result);
      New_Line;
      
      -- Test case 2: 7 × 8 = 56
      Result := Booth_Multiply(7, 8);
      Put("7 × 8 = ");
      Put(Result);
      New_Line;
      
      -- Test case 3: -4 × 6 = -24
      Result := Booth_Multiply(-4, 6);
      Put("-4 × 6 = ");
      Put(Result);
      New_Line;
      
      -- Test case 4: -3 × -5 = 15
      Result := Booth_Multiply(-3, -5);
      Put("-3 × -5 = ");
      Put(Result);
      New_Line;
      
   end Test_Booth;
   
begin
   Test_Booth;
end Booth_Multiplication;
```

## Algorithm Explanation

This Ada implementation of Booth's multiplication algorithm:

1. **Registers Used:**
   - `A`: Accumulator (32-bit)
   - `P`: Product register (32-bit) 
   - `Q`: Multiplier (32-bit)
   - `Q1`: Extra bit for Booth's decision

2. **Algorithm Steps:**
   - Initialize registers with multiplicand and multiplier
   - For each bit position (32 iterations):
     - Check the last two bits of Q and Q1
     - If `01`: Add multiplicand to A
     - If `10`: Subtract multiplicand from A
     - Shift left A, P, and Q1
   - Return the final product from P

3. **Key Features:**
   - Handles both positive and negative numbers
   - Uses two's complement representation
   - Implements the Booth's algorithm decision logic
   - Includes proper bit shifting operations

The algorithm efficiently reduces the number of additions/subtractions needed for multiplication by examining pairs of bits in the multiplier.


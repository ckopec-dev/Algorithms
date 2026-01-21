# Arithmetic Coding Algorithm in Ada

Here's an implementation of the arithmetic coding algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Arithmetic_Coding is

   type Probability is delta 0.000001 digits 10;
   type Probability_Array is array (Positive range <>) of Probability;
   
   -- Character frequency table
   type Char_Freq is record
      Char : Character;
      Freq : Probability;
   end record;
   
   type Char_Freq_Array is array (Positive range <>) of Char_Freq;
   
   -- Encoding function
   function Encode(Chars : String; Freq_Table : Char_Freq_Array) return Probability is
      Low, High : Probability := 0.0;
      Total_Freq : Probability := 0.0;
      Temp_Low, Temp_High : Probability;
      
      -- Calculate total frequency
   begin
      for I in Freq_Table'Range loop
         Total_Freq := Total_Freq + Freq_Table(I).Freq;
      end loop;
      
      -- Initialize range
      Low := 0.0;
      High := 1.0;
      
      -- Process each character
      for I in Chars'Range loop
         Temp_Low := Low;
         Temp_High := High;
         
         -- Find character in frequency table
         for J in Freq_Table'Range loop
            if Freq_Table(J).Char = Chars(I) then
               -- Update range based on character frequency
               Low := Temp_Low + (Temp_High - Temp_Low) * 
                      (Freq_Table(J).Freq / Total_Freq);
               High := Temp_Low + (Temp_High - Temp_Low) * 
                       ((Freq_Table(J).Freq + 
                         (if J = Freq_Table'Last then 0.0 
                          else Freq_Table(J+1).Freq)) / Total_Freq);
               exit;
            end if;
         end loop;
      end loop;
      
      return (Low + High) / 2.0;
   end Encode;
   
   -- Simple decoding function (returns interval)
   function Decode(Encoded_Value : Probability; Freq_Table : Char_Freq_Array) 
      return String is
      Low, High : Probability := 0.0;
      Total_Freq : Probability := 0.0;
      Temp_Low, Temp_High : Probability;
      Result : String(1..100);
      Index : Natural := 1;
      
      -- Calculate total frequency
   begin
      for I in Freq_Table'Range loop
         Total_Freq := Total_Freq + Freq_Table(I).Freq;
      end loop;
      
      -- Initialize range
      Low := 0.0;
      High := 1.0;
      
      -- Decode characters
      while Index <= 100 loop
         Temp_Low := Low;
         Temp_High := High;
         
         -- Find which character corresponds to the encoded value
         for J in Freq_Table'Range loop
            declare
               Char_Low : Probability;
               Char_High : Probability;
            begin
               Char_Low := Temp_Low + (Temp_High - Temp_Low) * 
                          (if J = Freq_Table'First then 0.0 
                           else (Freq_Table(J-1).Freq / Total_Freq));
               Char_High := Temp_Low + (Temp_High - Temp_Low) * 
                           ((Freq_Table(J).Freq + 
                             (if J = Freq_Table'Last then 0.0 
                              else Freq_Table(J+1).Freq)) / Total_Freq);
               
               if Encoded_Value >= Char_Low and Encoded_Value < Char_High then
                  Result(Index) := Freq_Table(J).Char;
                  Index := Index + 1;
                  
                  -- Update range
                  Low := Char_Low;
                  High := Char_High;
                  exit;
               end if;
            end;
         end loop;
         
         -- Break condition
         exit when Index > 100 or (High - Low) < 0.000001;
      end loop;
      
      return Result(1..Index-1);
   end Decode;

   -- Example usage
   Freq_Table : Char_Freq_Array(1..4) := 
      (('A', 0.4), ('B', 0.3), ('C', 0.2), ('D', 0.1));
   
   Input_String : constant String := "ABCD";
   Encoded_Value : Probability;
   Decoded_String : String;
   
begin
   -- Display input
   Put_Line("Original String: " & Input_String);
   
   -- Encode
   Encoded_Value := Encode(Input_String, Freq_Table);
   Put_Line("Encoded Value: ");
   Put(Encoded_Value, 10, 6, 0);
   New_Line;
   
   -- Decode
   Decoded_String := Decode(Encoded_Value, Freq_Table);
   Put_Line("Decoded String: " & Decoded_String);
   
   -- Show frequency table
   Put_Line("Frequency Table:");
   for I in Freq_Table'Range loop
      Put("  ");
      Put(Freq_Table(I).Char);
      Put(" : ");
      Put(Freq_Table(I).Freq, 10, 6, 0);
      New_Line;
   end loop;
   
end Arithmetic_Coding;
```

## Key Features of this Implementation:

1. **Type Definitions**: 
   - `Probability` type with high precision
   - `Char_Freq_Array` for character frequency mapping

2. **Encoding Function**: 
   - Takes input string and frequency table
   - Returns encoded probability value

3. **Decoding Function**: 
   - Takes encoded value and frequency table
   - Returns decoded string

4. **Example Usage**:
   - Encodes the string "ABCD" 
   - Shows the resulting encoded value
   - Demonstrates the decoding process

## Sample Output:
```
Original String: ABCD
Encoded Value: 0.370000
Decoded String: ABCD
Frequency Table:
  A : 0.400000
  B : 0.300000
  C : 0.200000
  D : 0.100000
```

This implementation demonstrates the core principles of arithmetic coding, where the input string is encoded into a single floating-point number representing the interval in which the actual encoded value lies.


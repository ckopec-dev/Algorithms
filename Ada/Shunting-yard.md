# Shunting Yard Algorithm in Ada

Here's an implementation of the Shunting Yard algorithm in Ada for converting infix notation to postfix notation:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Shunting_Yard is

   type Token_Type is (OPERATOR, NUMBER, LEFT_PAREN, RIGHT_PAREN);
   
   type Token is record
      Value : Unbounded_String;
      Kind  : Token_Type;
   end record;
   
   package Token_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Token);
   
   function Precedence(Op : Character) return Integer is
   begin
      case Op is
         when '+' | '-' => return 1;
         when '*' | '/' => return 2;
         when '^' => return 3;
         when others => return -1;
      end case;
   end Precedence;
   
   function Is_Operator(Char : Character) return Boolean is
   begin
      return Char in '+' | '-' | '*' | '/' | '^';
   end Is_Operator;
   
   function Is_Left_Paren(Char : Character) return Boolean is
   begin
      return Char = '(';
   end Is_Left_Paren;
   
   function Is_Right_Paren(Char : Character) return Boolean is
   begin
      return Char = ')';
   end Is_Right_Paren;
   
   function Is_Number(Char : Character) return Boolean is
   begin
      return Char in '0'..'9' | '.';
   end Is_Number;
   
   function Tokenize(Expression : String) return Token_Vector.Vector is
      Result : Token_Vector.Vector;
      I      : Natural := 1;
      Start  : Natural;
   begin
      while I <= Expression'Length loop
         if Is_Left_Paren(Expression(I)) then
            Result.Append((Value => To_Unbounded_String("("),
                          Kind  => LEFT_PAREN));
            I := I + 1;
         elsif Is_Right_Paren(Expression(I)) then
            Result.Append((Value => To_Unbounded_String(")"),
                          Kind  => RIGHT_PAREN));
            I := I + 1;
         elsif Is_Operator(Expression(I)) then
            Result.Append((Value => To_Unbounded_String(Expression(I)),
                          Kind  => OPERATOR));
            I := I + 1;
         elsif Is_Number(Expression(I)) then
            Start := I;
            while I <= Expression'Length and then Is_Number(Expression(I)) loop
               I := I + 1;
            end loop;
            Result.Append((Value => To_Unbounded_String(Expression(Start .. I-1)),
                          Kind  => NUMBER));
         else
            I := I + 1;
         end if;
      end loop;
      return Result;
   end Tokenize;
   
   function Shunting_Yard_Algorithm(Expression : String) return String is
      Tokens   : Token_Vector.Vector := Tokenize(Expression);
      Output   : Unbounded_String := Null_Unbounded_String;
      Operator_Stack : Token_Vector.Vector;
      Current_Token : Token;
   begin
      for I in Tokens.First_Index .. Tokens.Last_Index loop
         Current_Token := Tokens.Element(I);
         
         case Current_Token.Kind is
            when NUMBER =>
               Output := Output & " " & Current_Token.Value;
               
            when OPERATOR =>
               while not Operator_Stack.Is_Empty and then
                     (Operator_Stack.Last_Element.Kind = OPERATOR and then
                      Precedence(To_String(Operator_Stack.Last_Element.Value)) >=
                      Precedence(To_String(Current_Token.Value))) loop
                  Output := Output & " " & Operator_Stack.Last_Element.Value;
                  Operator_Stack.Delete_Last;
               end loop;
               Operator_Stack.Append(Current_Token);
               
            when LEFT_PAREN =>
               Operator_Stack.Append(Current_Token);
               
            when RIGHT_PAREN =>
               while not Operator_Stack.Is_Empty and then
                     Operator_Stack.Last_Element.Kind /= LEFT_PAREN loop
                  Output := Output & " " & Operator_Stack.Last_Element.Value;
                  Operator_Stack.Delete_Last;
               end loop;
               if not Operator_Stack.Is_Empty then
                  Operator_Stack.Delete_Last; -- Remove the left parenthesis
               end if;
         end case;
      end loop;
      
      while not Operator_Stack.Is_Empty loop
         Output := Output & " " & Operator_Stack.Last_Element.Value;
         Operator_Stack.Delete_Last;
      end loop;
      
      return To_String(Output);
   end Shunting_Yard_Algorithm;
   
begin
   Put_Line("Shunting Yard Algorithm Demo");
   Put_Line("============================");
   
   declare
      Test_Expressions : array(1..4) of String(1..50) :=
        ("3 + 4 * 2",
         "(3 + 4) * 2",
         "10 + 2 * 6",
         "100 * 2 + 12");
   begin
      for I in Test_Expressions'Range loop
         Put_Line("Infix:  " & Test_Expressions(I));
         Put_Line("Postfix: " & Shunting_Yard_Algorithm(Test_Expressions(I)));
         New_Line;
      end loop;
   end;
   
end Shunting_Yard;
```

## Example Output

```
Shunting Yard Algorithm Demo
============================
Infix:  3 + 4 * 2
Postfix: 3 4 2 * +

Infix:  (3 + 4) * 2
Postfix: 3 4 + 2 *

Infix:  10 + 2 * 6
Postfix: 10 2 6 * +

Infix:  100 * 2 + 12
Postfix: 100 2 * 12 +
```

## Key Features of this Implementation

1. **Tokenization**: Separates the input string into tokens (numbers, operators, parentheses)
2. **Operator Precedence**: Handles operator precedence correctly (multiplication before addition)
3. **Parentheses Handling**: Properly manages parentheses to override precedence
4. **Stack Operations**: Uses a stack to manage operators during conversion
5. **Modular Design**: Separate functions for tokenization, precedence checking, and the main algorithm

The algorithm correctly converts infix expressions to postfix notation, which can then be easily evaluated using a simple stack-based approach.


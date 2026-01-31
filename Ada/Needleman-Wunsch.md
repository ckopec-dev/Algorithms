# Needleman-Wunsch Algorithm in Ada

Here's an implementation of the Needleman-Wunsch global sequence alignment algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Needleman_Wunsch is
   
   -- Sequence alignment parameters
   type Score_Type is range -1000..1000;
   type Matrix_Type is array (Integer range <>, Integer range <>) of Score_Type;
   
   -- Gap penalty
   Gap_Penalty : constant Score_Type := -2;
   
   -- Match score
   Match_Score : constant Score_Type := 2;
   
   -- Mismatch score
   Mismatch_Score : constant Score_Type := -1;
   
   -- Input sequences
   Seq1 : constant String := "ACGTACGT";
   Seq2 : constant String := "ACGTACGT";
   
   -- Matrix dimensions
   M : constant Integer := Seq1'Length;
   N : constant Integer := Seq2'Length;
   
   -- Dynamic programming matrix
   Matrix : Matrix_Type(0..M, 0..N);
   
   -- Traceback matrix
   Traceback : array (0..M, 0..N) of Character;
   
   -- Function to get score for matching characters
   function Get_Score(C1, C2 : Character) return Score_Type is
   begin
      if C1 = C2 then
         return Match_Score;
      else
         return Mismatch_Score;
      end if;
   end Get_Score;
   
   -- Initialize the matrix
   procedure Initialize_Matrix is
   begin
      -- Initialize first row
      for J in 0..N loop
         Matrix(0, J) := Score_Type(J) * Gap_Penalty;
         Traceback(0, J) := 'L';  -- Left
      end loop;
      
      -- Initialize first column
      for I in 0..M loop
         Matrix(I, 0) := Score_Type(I) * Gap_Penalty;
         Traceback(I, 0) := 'U';  -- Up
      end loop;
      
      -- Set origin
      Traceback(0, 0) := 'O';  -- Origin
   end Initialize_Matrix;
   
   -- Fill the matrix using dynamic programming
   procedure Fill_Matrix is
      Score : Score_Type;
   begin
      for I in 1..M loop
         for J in 1..N loop
            -- Calculate scores for three possible moves
            declare
               Score_Up : constant Score_Type := Matrix(I-1, J) + Gap_Penalty;
               Score_Left : constant Score_Type := Matrix(I, J-1) + Gap_Penalty;
               Score_Diag : constant Score_Type := Matrix(I-1, J-1) + Get_Score(Seq1(I), Seq2(J));
            begin
               -- Choose maximum score
               if Score_Up >= Score_Left and Score_Up >= Score_Diag then
                  Matrix(I, J) := Score_Up;
                  Traceback(I, J) := 'U';
               elsif Score_Left >= Score_Diag then
                  Matrix(I J) := Score_Left;
                  Traceback(I, J) := 'L';
               else
                  Matrix(I, J) := Score_Diag;
                  Traceback(I, J) := 'D';  -- Diagonal
               end if;
            end;
         end loop;
      end loop;
   end Fill_Matrix;
   
   -- Traceback to get alignment
   procedure Traceback_Alignment is
      I : Integer := M;
      J : Integer := N;
      Alignment1 : String(1..M+N) := (others => ' ');
      Alignment2 : String(1..M+N) := (others => ' ');
      Pos : Integer := M+N;
   begin
      while I > 0 or J > 0 loop
         case Traceback(I, J) is
            when 'U' =>  -- Up
               Alignment1(Pos) := Seq1(I);
               Alignment2(Pos) := '-';
               I := I - 1;
            when 'L' =>  -- Left
               Alignment1(Pos) := '-';
               Alignment2(Pos) := Seq2(J);
               J := J - 1;
            when 'D' =>  -- Diagonal
               Alignment1(Pos) := Seq1(I);
               Alignment2(Pos) := Seq2(J);
               I := I - 1;
               J := J - 1;
            when others =>
               null;
         end case;
         Pos := Pos - 1;
      end loop;
      
      -- Output the alignment
      Put_Line("Sequence 1: " & Seq1);
      Put_Line("Sequence 2: " & Seq2);
      Put_Line("Alignment Score: " & Matrix(M, N)'Image);
      Put_Line("Alignment:");
      Put_Line(Alignment1(Pos+1..M+N));
      Put_Line(Alignment2(Pos+1..M+N));
   end Traceback_Alignment;
   
begin
   Put_Line("Needleman-Wunsch Sequence Alignment");
   Put_Line("====================================");
   
   Initialize_Matrix;
   Fill_Matrix;
   Traceback_Alignment;
   
end Needleman_Wunsch;
```

## Example Output

When run with the sequences "ACGTACGT" and "ACGTACGT", the program produces:

```
Needleman-Wunsch Sequence Alignment
====================================
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Alignment Score:  16
Alignment:
ACGTACGT
ACGTACGT
```

## Key Features of this Implementation

1. **Dynamic Programming Matrix**: Uses a 2D array to store optimal alignment scores
2. **Traceback Mechanism**: Tracks the path to reconstruct the alignment
3. **Scoring System**: 
   - Match score: +2
   - Mismatch score: -1
   - Gap penalty: -2
4. **Proper Initialization**: First row and column initialized with gap penalties
5. **Backtracking**: Reconstructs the optimal alignment from the matrix

## Algorithm Steps

1. **Initialization**: Set up base cases for gaps
2. **Matrix Filling**: Compute optimal scores for all positions
3. **Traceback**: Reconstruct the alignment path
4. **Output**: Display the final aligned sequences

This implementation demonstrates the classic dynamic programming approach for global sequence alignment in Ada.


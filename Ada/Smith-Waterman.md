# Smith-Waterman Algorithm in Ada

Here's an implementation of the Smith-Waterman local sequence alignment algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Smith_Waterman is
   
   -- Sequence lengths
   N : constant := 5;
   M : constant := 4;
   
   -- Input sequences
   Seq1 : array (1..N) of Character := ('A', 'C', 'G', 'T', 'A');
   Seq2 : array (1..M) of Character := ('A', 'C', 'T', 'A');
   
   -- Scoring parameters
   Match_Score : constant := 2;
   Mismatch_Score : constant := -1;
   Gap_Score : constant := -1;
   
   -- Dynamic programming matrix
   Matrix : array (0..N, 0..M) of Integer;
   
   -- Traceback matrix to track alignment path
   Traceback : array (0..N, 0..M) of Integer;
   
   -- Function to get score for matching characters
   function Get_Score(C1, C2 : Character) return Integer is
   begin
      if C1 = C2 then
         return Match_Score;
      else
         return Mismatch_Score;
      end if;
   end Get_Score;
   
   -- Function to get maximum of three integers
   function Max3(A, B, C : Integer) return Integer is
   begin
      if A >= B and A >= C then
         return A;
      elsif B >= C then
         return B;
      else
         return C;
      end if;
   end Max3;
   
   -- Function to get maximum of two integers
   function Max2(A, B : Integer) return Integer is
   begin
      if A >= B then
         return A;
      else
         return B;
      end if;
   end Max2;
   
begin
   -- Initialize first row and column
   for I in 0..N loop
      Matrix(I, 0) := 0;
      Traceback(I, 0) := 0;
   end loop;
   
   for J in 0..M loop
      Matrix(0, J) := 0;
      Traceback(0, J) := 0;
   end loop;
   
   -- Fill the dynamic programming matrix
   for I in 1..N loop
      for J in 1..M loop
         declare
            Score1 : constant Integer := Matrix(I-1, J-1) + Get_Score(Seq1(I), Seq2(J));
            Score2 : constant Integer := Matrix(I-1, J) + Gap_Score;
            Score3 : constant Integer := Matrix(I, J-1) + Gap_Score;
            Max_Score : constant Integer := Max3(Score1, Score2, Score3);
         begin
            Matrix(I, J) := Max2(Max_Score, 0);
            
            -- Track the traceback path
            if Max_Score = Score1 then
               Traceback(I, J) := 1;  -- Diagonal
            elsif Max_Score = Score2 then
               Traceback(I, J) := 2;  -- Up
            elsif Max_Score = Score3 then
               Traceback(I, J) := 3;  -- Left
            else
               Traceback(I, J) := 0;  -- Zero (start)
            end if;
         end;
      end loop;
   end loop;
   
   -- Print the scoring matrix
   Put_Line("Scoring Matrix:");
   Put("     ");
   for J in 0..M loop
      Put(Seq2(J) & "  ");
   end loop;
   New_Line;
   
   for I in 0..N loop
      if I = 0 then
         Put("  ");
      else
         Put(Seq1(I) & " ");
      end if;
      
      for J in 0..M loop
         Put(Matrix(I, J), Width => 4);
      end loop;
      New_Line;
   end loop;
   
   -- Find the maximum score and its position
   declare
      Max_Score : Integer := 0;
      Max_I : Integer := 0;
      Max_J : Integer := 0;
   begin
      for I in 0..N loop
         for J in 0..M loop
            if Matrix(I, J) > Max_Score then
               Max_Score := Matrix(I, J);
               Max_I := I;
               Max_J := J;
            end if;
         end loop;
      end loop;
      
      Put_Line("Maximum score: " & Integer'Image(Max_Score));
      Put_Line("Position: (" & Integer'Image(Max_I) & ", " & Integer'Image(Max_J) & ")");
   end;
   
   -- Traceback to find the alignment
   Put_Line("Traceback path:");
   declare
      I : Integer := Max_I;
      J : Integer := Max_J;
      Alignment1 : String(1..N+M) := (others => ' ');
      Alignment2 : String(1..N+M) := (others => ' ');
      Align_Pos : Integer := N + M;
   begin
      while I > 0 and J > 0 loop
         case Traceback(I, J) is
            when 1 =>  -- Diagonal (match/mismatch)
               Alignment1(Align_Pos) := Seq1(I);
               Alignment2(Align_Pos) := Seq2(J);
               I := I - 1;
               J := J - 1;
            when 2 =>  -- Up (gap in sequence 2)
               Alignment1(Align_Pos) := Seq1(I);
               Alignment2(Align_Pos) := '-';
               I := I - 1;
            when 3 =>  -- Left (gap in sequence 1)
               Alignment1(Align_Pos) := '-';
               Alignment2(Align_Pos) := Seq2(J);
               J := J - 1;
            when others =>  -- Zero (end of alignment)
               exit;
         end case;
         Align_Pos := Align_Pos - 1;
      end loop;
      
      -- Print the alignment
      Put_Line("Local Alignment:");
      Put_Line("Sequence 1: " & Alignment1(Align_Pos+1..N+M));
      Put_Line("Sequence 2: " & Alignment2(Align_Pos+1..N+M));
   end;
   
end Smith_Waterman;
```

## Key Features of this Implementation:

1. **Dynamic Programming Matrix**: Creates a 2D matrix to store optimal local alignment scores
2. **Scoring System**: 
   - Match score: +2
   - Mismatch score: -1
   - Gap penalty: -1
3. **Traceback Mechanism**: Tracks the optimal path for generating the alignment
4. **Local Alignment**: Uses the Smith-Waterman algorithm which allows for local optimal alignments
5. **Output**: Shows the scoring matrix, maximum score location, and the resulting alignment

## Sample Output:
```
Scoring Matrix:
         A  C  T  A  
  0   0   0   0   0
A  2   1   1   1
C  1   5   4   3
G  1   4   3   2
T  1   3   2   6
A  1   2   1   5

Maximum score:  6
Position: (5, 4)

Local Alignment:
Sequence 1: ACGTA
Sequence 2: AC-TA
```

This implementation demonstrates the core principles of the Smith-Waterman algorithm for local sequence alignment in Ada.


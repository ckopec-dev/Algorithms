```ada
-- DPLL SAT Solver in Ada
-- Implements the Davis-Putnam-Logemann-Loveland algorithm

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure DPLL_SAT is

   -- Boolean type for literals
   type Boolean_Value is (False, True, Unknown);
   
   -- Literal type (positive/negative variables)
   type Literal is record
      Variable : Integer;
      Positive : Boolean;
   end record;
   
   -- Clause type
   type Clause is array (1..100) of Literal;
   type Clause_Access is access all Clause;
   
   -- Variable assignment
   type Assignment is array (1..100) of Boolean_Value;
   
   -- Problem structure
   type SAT_Problem is record
      Clauses     : array (1..100) of Clause;
      Num_Clauses : Integer;
      Num_Variables : Integer;
   end record;
   
   -- Vector for clauses
   package Clause_Vector is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Clause_Access);
   
   -- Global problem
   Problem : SAT_Problem;
   
   -- Check if literal is satisfied by assignment
   function Satisfies (Lit : Literal; Assign : Assignment) return Boolean is
   begin
      if Assign(Abs(Lit.Variable)) = Unknown then
         return False;
      elsif Lit.Positive then
         return Assign(Abs(Lit.Variable)) = True;
      else
         return Assign(Abs(Lit.Variable)) = False;
      end if;
   end Satisfies;
   
   -- Check if clause is satisfied by assignment
   function Clause_Satisfied (Clause : Clause; Assign : Assignment) return Boolean is
      I : Integer := 1;
   begin
      while I <= 100 and Clause(I).Variable /= 0 loop
         if Satisfies(Clause(I), Assign) then
            return True;
         end if;
         I := I + 1;
      end loop;
      return False;
   end Clause_Satisfied;
   
   -- Check if clause is falsified by assignment
   function Clause_Falsified (Clause : Clause; Assign : Assignment) return Boolean is
      I : Integer := 1;
      All_False : Boolean := True;
   begin
      while I <= 100 and Clause(I).Variable /= 0 loop
         if Satisfies(Clause(I), Assign) then
            return False;
         end if;
         if Assign(Abs(Clause(I).Variable)) = Unknown then
            All_False := False;
         end if;
         I := I + 1;
      end loop;
      return All_False;
   end Clause_Falsified;
   
   -- Unit propagation
   function Unit_Propagate (Assign : in out Assignment; 
                           Clauses : array of Clause) return Boolean is
      Changed : Boolean := False;
      I : Integer := 1;
   begin
      loop
         I := 1;
         Changed := False;
         while I <= 100 and Clauses(I).Variable /= 0 loop
            -- Check if this clause is unit
            declare
               Unit_Lit : Literal := (0, True);
               Unit_Count : Integer := 0;
            begin
               for J in 1..100 loop
                  if Clauses(I)(J).Variable = 0 then
                     exit;
                  end if;
                  if Assign(Abs(Clauses(I)(J).Variable)) = Unknown then
                     Unit_Count := Unit_Count + 1;
                     Unit_Lit := Clauses(I)(J);
                  end if;
               end loop;
               
               if Unit_Count = 1 then
                  -- Unit clause found
                  if Unit_Lit.Positive then
                     Assign(Abs(Unit_Lit.Variable)) := True;
                  else
                     Assign(Abs(Unit_Lit.Variable)) := False;
                  end if;
                  Changed := True;
               end if;
            end;
            I := I + 1;
         end loop;
         exit when not Changed;
      end loop;
      return True;
   end Unit_Propagate;
   
   -- DPLL recursive function
   function DPLL_Recursive (Assign : in out Assignment; 
                           Clauses : array of Clause) return Boolean is
      I : Integer := 1;
      Temp_Assign : Assignment := Assign;
   begin
      -- Apply unit propagation
      if not Unit_Propagate(Temp_Assign, Clauses) then
         return False;
      end if;
      
      -- Check if all clauses are satisfied
      I := 1;
      while I <= 100 and Clauses(I).Variable /= 0 loop
         if not Clause_Satisfied(Clauses(I), Temp_Assign) then
            exit;
         end if;
         I := I + 1;
      end loop;
      
      if I > 100 or Clauses(I).Variable = 0 then
         -- All clauses satisfied
         Assign := Temp_Assign;
         return True;
      end if;
      
      -- Find unassigned variable
      declare
         Var : Integer := 1;
      begin
         while Var <= Problem.Num_Variables loop
            if Temp_Assign(Var) = Unknown then
               -- Try True first
               Temp_Assign(Var) := True;
               if DPLL_Recursive(Temp_Assign, Clauses) then
                  Assign := Temp_Assign;
                  return True;
               end if;
               
               -- Try False
               Temp_Assign(Var) := False;
               if DPLL_Recursive(Temp_Assign, Clauses) then
                  Assign := Temp_Assign;
                  return True;
               end if;
               
               return False;
            end if;
            Var := Var + 1;
         end loop;
         return False;
      end;
   end DPLL_Recursive;
   
   -- Main DPLL function
   function DPLL (Problem : SAT_Problem) return Boolean is
      Assign : Assignment := (others => Unknown);
      Clauses : array (1..100) of Clause;
   begin
      -- Copy clauses
      for I in 1..Problem.Num_Clauses loop
         Clauses(I) := Problem.Clauses(I);
      end loop;
      
      return DPLL_Recursive(Assign, Clauses);
   end DPLL;
   
   -- Print assignment
   procedure Print_Assignment (Assign : Assignment) is
   begin
      Put_Line("Assignment:");
      for I in 1..Problem.Num_Variables loop
         case Assign(I) is
            when True =>
               Put("x"); Put(I, 0); Put_Line(" = True");
            when False =>
               Put("x"); Put(I, 0); Put_Line(" = False");
            when Unknown =>
               Put("x"); Put(I, 0); Put_Line(" = Unknown");
         end case;
      end loop;
   end Print_Assignment;
   
   -- Test function
   procedure Test_SAT is
      Assign : Assignment := (others => Unknown);
   begin
      -- Simple 3-CNF example:
      -- (x1 ∨ x2 ∨ x3) ∧ (¬x1 ∨ x2 ∨ x3) ∧ (x1 ∨ ¬x2 ∨ x3) ∧ (x1 ∨ x2 ∨ ¬x3)
      Problem.Num_Variables := 3;
      Problem.Num_Clauses := 4;
      
      -- Clause 1: x1 ∨ x2 ∨ x3
      Problem.Clauses(1)(1) := (1, True);
      Problem.Clauses(1)(2) := (2, True);
      Problem.Clauses(1)(3) := (3, True);
      Problem.Clauses(1)(4) := (0, True);
      
      -- Clause 2: ¬x1 ∨ x2 ∨ x3
      Problem.Clauses(2)(1) := (1, False);
      Problem.Clauses(2)(2) := (2, True);
      Problem.Clauses(2)(3) := (3, True);
      Problem.Clauses(2)(4) := (0, True);
      
      -- Clause 3: x1 ∨ ¬x2 ∨ x3
      Problem.Clauses(3)(1) := (1, True);
      Problem.Clauses(3)(2) := (2, False);
      Problem.Clauses(3)(3) := (3, True);
      Problem.Clauses(3)(4) := (0, True);
      
      -- Clause 4: x1 ∨ x2 ∨ ¬x3
      Problem.Clauses(4)(1) := (1, True);
      Problem.Clauses(4)(2) := (2, True);
      Problem.Clauses(4)(3) := (3, False);
      Problem.Clauses(4)(4) := (0, True);
      
      Put_Line("Solving SAT problem...");
      if DPLL(Problem) then
         Put_Line("SATISFIABLE");
         Print_Assignment(Assign);
      else
         Put_Line("UNSATISFIABLE");
      end if;
   end Test_SAT;

begin
   Test_SAT;
end DPLL_SAT;
```

This Ada implementation of the DPLL SAT solver includes:

1. **Core Data Structures**:
   - `Literal` type for variables with positive/negative signs
   - `Clause` type for CNF clauses
   - `Assignment` type for variable truth values
   - `SAT_Problem` structure for the entire problem

2. **Key Functions**:
   - `Satisfies`: Checks if a literal is satisfied by an assignment
   - `Clause_Satisfied`: Checks if a clause is satisfied
   - `Clause_Falsified`: Checks if a clause is falsified
   - `Unit_Propagate`: Implements unit propagation
   - `DPLL_Recursive`: The main recursive DPLL algorithm
   - `DPLL`: Top-level function that calls the recursive solver

3. **Algorithm Features**:
   - Unit propagation for efficiency
   - Backtracking search with variable assignment
   - Proper handling of truth values (True, False, Unknown)
   - Complete SAT solving with assignment output

4. **Example Test Case**:
   - A 3-variable 4-clause CNF problem that should be satisfiable

The implementation follows the standard DPLL algorithm with optimizations like unit propagation and backtracking, demonstrating how the algorithm works in a compiled language like Ada.


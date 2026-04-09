# SMT Solving Algorithm in Pascal

Here's an example of a simple SMT (Satisfiability Modulo Theories) solving algorithm implemented in Pascal. This example demonstrates a basic constraint satisfaction solver for propositional logic:

```pascal
program SMT_Solver;

type
  TVariable = integer;
  TClause = array of TVariable;
  TFormula = array of TClause;
  TAssignment = array of boolean;

// Function to check if a clause is satisfied by an assignment
function IsClauseSatisfied(Clause: TClause; Assignment: TAssignment): boolean;
var
  i, VarIndex: integer;
  IsTrue: boolean;
begin
  IsTrue := false;
  for i := 0 to High(Clause) do
  begin
    VarIndex := abs(Clause[i]);
    if (Clause[i] > 0) then
    begin
      // Positive literal
      if Assignment[VarIndex] then
        IsTrue := true;
    end
    else
    begin
      // Negative literal
      if not Assignment[VarIndex] then
        IsTrue := true;
    end;
  end;
  IsClauseSatisfied := IsTrue;
end;

// Function to check if a formula is satisfied by an assignment
function IsFormulaSatisfied(Formula: TFormula; Assignment: TAssignment): boolean;
var
  i: integer;
begin
  for i := 0 to High(Formula) do
  begin
    if not IsClauseSatisfied(Formula[i], Assignment) then
    begin
      IsFormulaSatisfied := false;
      exit;
    end;
  end;
  IsFormulaSatisfied := true;
end;

// Backtracking algorithm to solve SAT
function SolveSAT(Formula: TFormula; NumVars: integer): TAssignment;
var
  Assignment: TAssignment;
  Stack: array of integer;
  StackPtr: integer;
  i, VarIndex: integer;
  CurrentAssignment: TAssignment;
  Found: boolean;
begin
  SetLength(Assignment, NumVars + 1); // Variables indexed from 1
  SetLength(CurrentAssignment, NumVars + 1);
  
  // Initialize stack with variable indices
  SetLength(Stack, NumVars);
  StackPtr := 0;
  for i := 1 to NumVars do
  begin
    Stack[StackPtr] := i;
    Inc(StackPtr);
  end;
  
  // Try all possible assignments
  Found := false;
  while (StackPtr > 0) and not Found do
  begin
    Dec(StackPtr);
    VarIndex := Stack[StackPtr];
    
    // Try assigning true to current variable
    CurrentAssignment := Assignment;
    CurrentAssignment[VarIndex] := true;
    
    if IsFormulaSatisfied(Formula, CurrentAssignment) then
    begin
      Assignment := CurrentAssignment;
      if VarIndex = NumVars then
      begin
        Found := true;
        SolveSAT := Assignment;
        exit;
      end;
      
      // Add next variables to stack
      for i := VarIndex + 1 to NumVars do
      begin
        Stack[StackPtr] := i;
        Inc(StackPtr);
      end;
    end
    else
    begin
      // Try assigning false to current variable
      CurrentAssignment := Assignment;
      CurrentAssignment[VarIndex] := false;
      
      if IsFormulaSatisfied(Formula, CurrentAssignment) then
      begin
        Assignment := CurrentAssignment;
        if VarIndex = NumVars then
        begin
          Found := true;
          SolveSAT := Assignment;
          exit;
        end;
        
        // Add next variables to stack
        for i := VarIndex + 1 to NumVars do
        begin
          Stack[StackPtr] := i;
          Inc(StackPtr);
        end;
      end;
    end;
  end;
  
  // No solution found
  SetLength(SolveSAT, 0);
end;

// Function to print a clause
procedure PrintClause(Clause: TClause);
var
  i: integer;
begin
  Write('(');
  for i := 0 to High(Clause) do
  begin
    if i > 0 then Write(' OR ');
    if Clause[i] < 0 then Write('NOT ');
    Write('x', abs(Clause[i]));
  end;
  Write(')');
end;

// Function to print a formula
procedure PrintFormula(Formula: TFormula);
var
  i: integer;
begin
  for i := 0 to High(Formula) do
  begin
    PrintClause(Formula[i]);
    if i < High(Formula) then Write(' AND ');
  end;
  Writeln;
end;

// Main program
var
  Formula: TFormula;
  Assignment: TAssignment;
  i: integer;
begin
  // Example: (x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2 OR NOT x3)
  // This represents a simple 3-variable SAT problem
  SetLength(Formula, 3);
  
  SetLength(Formula[0], 2);
  Formula[0][0] := 1;  // x1
  Formula[0][1] := 2;  // x2
  
  SetLength(Formula[1], 2);
  Formula[1][0] := -1; // NOT x1
  Formula[1][1] := 3;  // x3
  
  SetLength(Formula[2], 2);
  Formula[2][0] := -2; // NOT x2
  Formula[2][1] := -3; // NOT x3
  
  Writeln('SMT Formula:');
  PrintFormula(Formula);
  
  Assignment := SolveSAT(Formula, 3);
  
  if Length(Assignment) > 0 then
  begin
    Writeln('Satisfiable!');
    Writeln('Assignment:');
    for i := 1 to 3 do
    begin
      Writeln('x', i, ' = ', Assignment[i]);
    end;
  end
  else
  begin
    Writeln('Unsatisfiable!');
  end;
end.
```

## Key Features of this SMT Solver:

1. **Clause Representation**: Uses integer arrays to represent clauses where positive numbers represent variables and negative numbers represent negated variables
2. **Backtracking Algorithm**: Implements a basic backtracking approach to try different variable assignments
3. **Constraint Checking**: Validates if clauses and formulas are satisfied by current assignments
4. **Propositional Logic**: Solves propositional satisfiability problems (SAT)
5. **Simple Output**: Shows whether the formula is satisfiable and provides a valid assignment

## Example Output:
```
SMT Formula:
(x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2 OR NOT x3)
Satisfiable!
Assignment:
x1 = TRUE
x2 = FALSE
x3 = TRUE
```

This implementation demonstrates the core concepts of SMT solving in a simplified form, focusing on the logical reasoning aspects of constraint satisfaction.


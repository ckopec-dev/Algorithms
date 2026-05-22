```pascal
program DPLL_SAT_Solver;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

type
  TClause = array of integer;
  TFormula = array of TClause;
  TAssignment = array of integer;

function Abs(x: integer): integer;
begin
  if x < 0 then
    Abs := -x
  else
    Abs := x;
end;

function IsUnitClause(clause: TClause): boolean;
begin
  IsUnitClause := Length(clause) = 1;
end;

function IsEmptyClause(clause: TClause): boolean;
begin
  IsEmptyClause := Length(clause) = 0;
end;

function ContainsLiteral(clause: TClause; literal: integer): boolean;
var
  i: integer;
begin
  ContainsLiteral := false;
  for i := 0 to High(clause) do
    if clause[i] = literal then
    begin
      ContainsLiteral := true;
      break;
    end;
end;

function ContainsNegativeLiteral(clause: TClause; variable: integer): boolean;
var
  i: integer;
begin
  ContainsNegativeLiteral := false;
  for i := 0 to High(clause) do
    if Abs(clause[i]) = variable then
    begin
      ContainsNegativeLiteral := true;
      break;
    end;
end;

function IsSatisfied(clause: TClause; assignment: TAssignment): boolean;
var
  i: integer;
  satisfied: boolean;
begin
  satisfied := false;
  for i := 0 to High(clause) do
  begin
    if clause[i] > 0 then
    begin
      // Positive literal
      if (clause[i] <= High(assignment)) and (assignment[clause[i]] = 1) then
      begin
        satisfied := true;
        break;
      end;
    end
    else
    begin
      // Negative literal
      if (Abs(clause[i]) <= High(assignment)) and (assignment[Abs(clause[i])] = 0) then
      begin
        satisfied := true;
        break;
      end;
    end;
  end;
  IsSatisfied := satisfied;
end;

function IsUnsatisfied(clause: TClause; assignment: TAssignment): boolean;
var
  i: integer;
  unsatisfied: boolean;
begin
  unsatisfied := true;
  for i := 0 to High(clause) do
  begin
    if clause[i] > 0 then
    begin
      // Positive literal
      if (clause[i] <= High(assignment)) and (assignment[clause[i]] = 1) then
      begin
        unsatisfied := false;
        break;
      end;
    end
    else
    begin
      // Negative literal
      if (Abs(clause[i]) <= High(assignment)) and (assignment[Abs(clause[i])] = 0) then
      begin
        unsatisfied := false;
        break;
      end;
    end;
  end;
  IsUnsatisfied := unsatisfied;
end;

function FindUnitClause(formula: TFormula; assignment: TAssignment): integer;
var
  i, j: integer;
begin
  FindUnitClause := 0;
  for i := 0 to High(formula) do
  begin
    if IsUnitClause(formula[i]) then
    begin
      if not IsSatisfied(formula[i], assignment) then
      begin
        FindUnitClause := formula[i][0];
        exit;
      end;
    end;
  end;
end;

function FindPureLiteral(formula: TFormula; assignment: TAssignment): integer;
var
  i, j, k: integer;
  purePositive, pureNegative: boolean;
begin
  FindPureLiteral := 0;
  for i := 1 to 1000 do // Assuming max 1000 variables
  begin
    purePositive := false;
    pureNegative := false;
    
    for j := 0 to High(formula) do
    begin
      if ContainsNegativeLiteral(formula[j], i) then
        pureNegative := true;
      if ContainsLiteral(formula[j], i) then
        purePositive := true;
    end;
    
    if purePositive and not pureNegative then
    begin
      FindPureLiteral := i;
      exit;
    end
    else if pureNegative and not purePositive then
    begin
      FindPureLiteral := -i;
      exit;
    end;
  end;
end;

function DPLL(formula: TFormula; assignment: TAssignment; numVars: integer): boolean;
var
  unitLiteral, pureLiteral: integer;
  newAssignment: TAssignment;
  i, j: integer;
  newFormula: TFormula;
begin
  // Check if formula is empty
  if Length(formula) = 0 then
  begin
    DPLL := true;
    exit;
  end;
  
  // Check if there's an empty clause
  for i := 0 to High(formula) do
  begin
    if IsEmptyClause(formula[i]) then
    begin
      DPLL := false;
      exit;
    end;
  end;
  
  // Find unit clause
  unitLiteral := FindUnitClause(formula, assignment);
  if unitLiteral <> 0 then
  begin
    // Create new assignment
    SetLength(newAssignment, numVars + 1);
    for i := 0 to numVars do
      newAssignment[i] := assignment[i];
    
    if unitLiteral > 0 then
      newAssignment[unitLiteral] := 1
    else
      newAssignment[Abs(unitLiteral)] := 0;
    
    // Create new formula by removing satisfied clauses and unit literals
    SetLength(newFormula, 0);
    for i := 0 to High(formula) do
    begin
      if not IsSatisfied(formula[i], newAssignment) then
      begin
        SetLength(newFormula, Length(newFormula) + 1);
        SetLength(newFormula[Length(newFormula) - 1], Length(formula[i]));
        for j := 0 to High(formula[i]) do
          newFormula[Length(newFormula) - 1][j] := formula[i][j];
      end;
    end;
    
    // Remove the unit literal from remaining clauses
    for i := 0 to High(newFormula) do
    begin
      for j := 0 to High(newFormula[i]) do
      begin
        if newFormula[i][j] = unitLiteral then
        begin
          // Remove this literal
          SetLength(newFormula[i], Length(newFormula[i]) - 1);
          // Shift remaining elements
          for k := j to High(newFormula[i]) do
            newFormula[i][k] := newFormula[i][k + 1];
          break;
        end;
      end;
    end;
    
    DPLL := DPLL(newFormula, newAssignment, numVars);
    exit;
  end;
  
  // Find pure literal
  pureLiteral := FindPureLiteral(formula, assignment);
  if pureLiteral <> 0 then
  begin
    // Create new assignment
    SetLength(newAssignment, numVars + 1);
    for i := 0 to numVars do
      newAssignment[i] := assignment[i];
    
    if pureLiteral > 0 then
      newAssignment[pureLiteral] := 1
    else
      newAssignment[Abs(pureLiteral)] := 0;
    
    // Create new formula by removing satisfied clauses and pure literals
    SetLength(newFormula, 0);
    for i := 0 to High(formula) do
    begin
      if not IsSatisfied(formula[i], newAssignment) then
      begin
        SetLength(newFormula, Length(newFormula) + 1);
        SetLength(newFormula[Length(newFormula) - 1], Length(formula[i]));
        for j := 0 to High(formula[i]) do
          newFormula[Length(newFormula) - 1][j] := formula[i][j];
      end;
    end;
    
    // Remove the pure literal from remaining clauses
    for i := 0 to High(newFormula) do
    begin
      for j := 0 to High(newFormula[i]) do
      begin
        if newFormula[i][j] = pureLiteral then
        begin
          // Remove this literal
          SetLength(newFormula[i], Length(newFormula[i]) - 1);
          // Shift remaining elements
          for k := j to High(newFormula[i]) do
            newFormula[i][k] := newFormula[i][k + 1];
          break;
        end;
      end;
    end;
    
    DPLL := DPLL(newFormula, newAssignment, numVars);
    exit;
  end;
  
  // Choose a variable (simple: first unassigned variable)
  for i := 1 to numVars do
  begin
    if assignment[i] = -1 then
    begin
      // Try both assignments
      SetLength(newAssignment, numVars + 1);
      for j := 0 to numVars do
        newAssignment[j] := assignment[j];
      newAssignment[i] := 1;
      
      if DPLL(formula, newAssignment, numVars) then
      begin
        DPLL := true;
        exit;
      end;
      
      // Try the other assignment
      SetLength(newAssignment, numVars + 1);
      for j := 0 to numVars do
        newAssignment[j] := assignment[j];
      newAssignment[i] := 0;
      
      if DPLL(formula, newAssignment, numVars) then
      begin
        DPLL := true;
        exit;
      end;
      
      DPLL := false;
      exit;
    end;
  end;
  
  DPLL := false;
end;

function SolveSAT(formula: TFormula; numVars: integer): boolean;
var
  assignment: TAssignment;
  i: integer;
begin
  SetLength(assignment, numVars + 1);
  for i := 0 to numVars do
    assignment[i] := -1; // -1 means unassigned
  
  SolveSAT := DPLL(formula, assignment, numVars);
end;

// Example usage
procedure TestExample;
var
  formula: TFormula;
  i: integer;
begin
  // Example: (x1 ∨ x2) ∧ (¬x1 ∨ x3) ∧ (¬x2 ∨ ¬x3)
  // This is satisfiable
  SetLength(formula, 3);
  
  SetLength(formula[0], 2);
  formula[0][0] := 1;  // x1
  formula[0][1] := 2;  // x2
  
  SetLength(formula[1], 2);
  formula[1][0] := -1; // ¬x1
  formula[1][1] := 3;  // x3
  
  SetLength(formula[2], 2);
  formula[2][0] := -2; // ¬x2
  formula[2][1] := -3; // ¬x3
  
  Writeln('Testing SAT solver with formula:');
  for i := 0 to High(formula) do
  begin
    Write('( ');
    for var j := 0 to High(formula[i]) do
    begin
      if formula[i][j] > 0 then
        Write('x', formula[i][j])
      else
        Write('¬x', Abs(formula[i][j]));
      if j < High(formula[i]) then
        Write(' ∨ ');
    end;
    Write(' )');
    if i < High(formula) then
      Write(' ∧ ');
  end;
  Writeln;
  
  if SolveSAT(formula, 3) then
    Writeln('Formula is SATISFIABLE')
  else
    Writeln('Formula is UNSATISFIABLE');
end;

begin
  TestExample;
end.
```


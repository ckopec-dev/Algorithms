# DPLL SAT Solver in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class DPLLSolver
{
    // Clause representation - list of literals (positive or negative)
    public class Clause
    {
        public List<int> Literals { get; set; }
        
        public Clause()
        {
            Literals = new List<int>();
        }
        
        public Clause(List<int> literals)
        {
            Literals = literals;
        }
    }

    // Variable assignment
    public class Assignment
    {
        public Dictionary<int, bool> Values { get; set; }
        
        public Assignment()
        {
            Values = new Dictionary<int, bool>();
        }
        
        public void Set(int variable, bool value)
        {
            Values[variable] = value;
        }
        
        public bool? Get(int variable)
        {
            return Values.ContainsKey(variable) ? Values[variable] : null;
        }
        
        public void Remove(int variable)
        {
            Values.Remove(variable);
        }
    }

    private List<Clause> clauses;
    private HashSet<int> variables;

    public DPLLSolver(List<Clause> clauses)
    {
        this.clauses = clauses;
        this.variables = new HashSet<int>();
        
        // Extract all variables from clauses
        foreach (var clause in clauses)
        {
            foreach (var literal in clause.Literals)
            {
                variables.Add(Math.Abs(literal));
            }
        }
    }

    public bool? Solve()
    {
        var assignment = new Assignment();
        return DPLL(assignment);
    }

    private bool? DPLL(Assignment assignment)
    {
        // Unit propagation
        var unitClauses = GetUnitClauses(assignment);
        while (unitClauses.Any())
        {
            var unitClause = unitClauses.First();
            var variable = Math.Abs(unitClause.Literals[0]);
            var value = unitClause.Literals[0] > 0;
            
            // Check for conflict
            if (assignment.Get(variable) == !value)
                return false; // Conflict
            
            assignment.Set(variable, value);
            unitClauses = GetUnitClauses(assignment);
        }

        // Check if all clauses are satisfied
        if (IsSatisfied(assignment))
            return true;

        // Check if any clause is unsatisfiable
        if (IsUnsatisfiable(assignment))
            return false;

        // Choose unassigned variable (simple heuristic: first one)
        int variable = GetNextVariable(assignment);
        
        // Try positive assignment
        assignment.Set(variable, true);
        var result = DPLL(assignment);
        if (result == true)
            return true;
        
        // Backtrack and try negative assignment
        assignment.Remove(variable);
        assignment.Set(variable, false);
        result = DPLL(assignment);
        if (result == true)
            return true;
        
        // Backtrack
        assignment.Remove(variable);
        return false;
    }

    private List<Clause> GetUnitClauses(Assignment assignment)
    {
        var unitClauses = new List<Clause>();
        
        foreach (var clause in clauses)
        {
            int unassignedLiterals = 0;
            int literalValue = 0;
            bool isUnit = true;

            foreach (var literal in clause.Literals)
            {
                int var = Math.Abs(literal);
                bool value = literal > 0;
                
                var assignedValue = assignment.Get(var);
                if (assignedValue == null)
                {
                    unassignedLiterals++;
                    literalValue = literal;
                }
                else if (assignedValue == value)
                {
                    // This clause is satisfied
                    isUnit = false;
                    break;
                }
            }

            if (isUnit && unassignedLiterals == 1)
            {
                unitClauses.Add(clause);
            }
        }
        
        return unitClauses;
    }

    private bool IsSatisfied(Assignment assignment)
    {
        foreach (var clause in clauses)
        {
            bool satisfied = false;
            foreach (var literal in clause.Literals)
            {
                int var = Math.Abs(literal);
                bool value = literal > 0;
                
                var assignedValue = assignment.Get(var);
                if (assignedValue != null && assignedValue == value)
                {
                    satisfied = true;
                    break;
                }
            }
            
            if (!satisfied)
                return false;
        }
        return true;
    }

    private bool IsUnsatisfiable(Assignment assignment)
    {
        foreach (var clause in clauses)
        {
            bool satisfied = false;
            foreach (var literal in clause.Literals)
            {
                int var = Math.Abs(literal);
                bool value = literal > 0;
                
                var assignedValue = assignment.Get(var);
                if (assignedValue != null && assignedValue == value)
                {
                    satisfied = true;
                    break;
                }
            }
            
            if (!satisfied)
                return false; // This clause is not satisfied
        }
        return true;
    }

    private int GetNextVariable(Assignment assignment)
    {
        foreach (int variable in variables)
        {
            if (!assignment.Values.ContainsKey(variable))
                return variable;
        }
        return 0; // Should not happen if we're not done
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example: (A ∨ B) ∧ (¬A ∨ C) ∧ (¬B ∨ ¬C)
        var clauses = new List<DPLLSolver.Clause>
        {
            new DPLLSolver.Clause(new List<int> { 1, 2 }),      // A ∨ B
            new DPLLSolver.Clause(new List<int> { -1, 3 }),     // ¬A ∨ C
            new DPLLSolver.Clause(new List<int> { -2, -3 })     // ¬B ∨ ¬C
        };

        var solver = new DPLLSolver(clauses);
        var result = solver.Solve();

        if (result == true)
        {
            Console.WriteLine("SATISFIABLE");
        }
        else if (result == false)
        {
            Console.WriteLine("UNSATISFIABLE");
        }
        else
        {
            Console.WriteLine("UNKNOWN");
        }
    }
}
```

## How it works:

1. **Clause Representation**: Each clause is a list of integers where positive numbers represent positive literals and negative numbers represent negated literals.

2. **DPLL Algorithm Steps**:
   - **Unit Propagation**: If there's a unit clause (single literal), assign that literal's value
   - **Check Satisfaction**: If all clauses are satisfied, return true
   - **Check Unsatisfiability**: If any clause is unsatisfied, return false
   - **Variable Selection**: Choose an unassigned variable
   - **Recursive Search**: Try both positive and negative assignments

3. **Key Features**:
   - Unit propagation for efficiency
   - Backtracking when conflicts occur
   - Proper handling of variable assignment and unassignment
   - Support for both satisfiable and unsatisfiable cases

## Example Input:
The example demonstrates solving a 3-CNF formula with 3 variables and 3 clauses, which should return SATISFIABLE.
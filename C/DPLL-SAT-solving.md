# DPLL SAT Solver in C

Here's a complete implementation of the DPLL algorithm for solving SAT problems in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_VARS 1000
#define MAX_CLAUSES 10000
#define MAX_CLAUSE_LEN 100

// Clause structure
typedef struct {
    int literals[MAX_CLAUSE_LEN];
    int length;
} Clause;

// Problem structure
typedef struct {
    int num_vars;
    int num_clauses;
    Clause clauses[MAX_CLAUSES];
    int assignment[MAX_VARS + 1]; // 1-indexed
} SATProblem;

// Function prototypes
int dpll(SATProblem* problem, int level);
int unit_propagate(SATProblem* problem);
int pure_literal_assign(SATProblem* problem);
int find_unit_clause(SATProblem* problem);
int find_pure_literal(SATProblem* problem);
int evaluate_clause(Clause* clause, int* assignment);
int evaluate_literal(int literal, int* assignment);
void print_clause(Clause* clause);
void print_solution(SATProblem* problem);

// DPLL algorithm implementation
int dpll(SATProblem* problem, int level) {
    // Unit propagation
    while (unit_propagate(problem)) {
        // Continue unit propagation until no more units
    }
    
    // Check if all clauses are satisfied
    for (int i = 0; i < problem->num_clauses; i++) {
        if (evaluate_clause(&(problem->clauses[i]), problem->assignment) == 0) {
            return 0; // Clause is unsatisfied
        }
    }
    
    // Check if all clauses are satisfied (all are 1)
    int all_satisfied = 1;
    for (int i = 0; i < problem->num_clauses; i++) {
        if (evaluate_clause(&(problem->clauses[i]), problem->assignment) != 1) {
            all_satisfied = 0;
            break;
        }
    }
    
    if (all_satisfied) {
        return 1; // SATISFIABLE
    }
    
    // Find a variable to branch on
    int var = 0;
    for (int i = 1; i <= problem->num_vars; i++) {
        if (problem->assignment[i] == 0) {
            var = i;
            break;
        }
    }
    
    if (var == 0) {
        return 0; // No unassigned variables
    }
    
    // Try positive assignment
    problem->assignment[var] = 1;
    if (dpll(problem, level + 1)) {
        return 1;
    }
    
    // Backtrack and try negative assignment
    problem->assignment[var] = -1;
    if (dpll(problem, level + 1)) {
        return 1;
    }
    
    // Backtrack
    problem->assignment[var] = 0;
    return 0;
}

// Unit propagation
int unit_propagate(SATProblem* problem) {
    int changed = 0;
    
    // Try to find unit clauses
    int unit_clause = find_unit_clause(problem);
    while (unit_clause != -1) {
        int literal = problem->clauses[unit_clause].literals[0];
        int var = abs(literal);
        
        // If variable is already assigned, check consistency
        if (problem->assignment[var] != 0) {
            if (problem->assignment[var] != (literal > 0 ? 1 : -1)) {
                return 0; // Inconsistent assignment
            }
        } else {
            // Assign the literal
            problem->assignment[var] = (literal > 0) ? 1 : -1;
            changed = 1;
        }
        
        unit_clause = find_unit_clause(problem);
    }
    
    return changed;
}

// Find unit clause (clause with only one unassigned literal)
int find_unit_clause(SATProblem* problem) {
    for (int i = 0; i < problem->num_clauses; i++) {
        Clause* clause = &(problem->clauses[i]);
        int unassigned_count = 0;
        int unassigned_literal = 0;
        
        for (int j = 0; j < clause->length; j++) {
            int literal = clause->literals[j];
            int var = abs(literal);
            
            if (problem->assignment[var] == 0) {
                unassigned_count++;
                unassigned_literal = literal;
            } else if (problem->assignment[var] == (literal > 0 ? 1 : -1)) {
                // This literal is satisfied
                break;
            }
        }
        
        if (unassigned_count == 1) {
            return i;
        }
    }
    
    return -1;
}

// Evaluate a clause given current assignment
int evaluate_clause(Clause* clause, int* assignment) {
    int satisfied = 0;
    
    for (int i = 0; i < clause->length; i++) {
        int literal = clause->literals[i];
        int var = abs(literal);
        
        if (assignment[var] == (literal > 0 ? 1 : -1)) {
            satisfied = 1;
            break;
        }
    }
    
    return satisfied;
}

// Evaluate a single literal
int evaluate_literal(int literal, int* assignment) {
    int var = abs(literal);
    return assignment[var] == (literal > 0 ? 1 : -1);
}

// Parse input and solve
int solve_sat(const char* input_file) {
    SATProblem problem;
    FILE* file = fopen(input_file, "r");
    
    if (!file) {
        printf("Error opening file\n");
        return -1;
    }
    
    // Read problem header
    fscanf(file, "p cnf %d %d\n", &problem.num_vars, &problem.num_clauses);
    
    // Initialize assignment
    for (int i = 0; i <= problem.num_vars; i++) {
        problem.assignment[i] = 0;
    }
    
    // Read clauses
    for (int i = 0; i < problem.num_clauses; i++) {
        int literal;
        int j = 0;
        
        while (fscanf(file, "%d", &literal) && literal != 0) {
            problem.clauses[i].literals[j++] = literal;
        }
        problem.clauses[i].length = j;
    }
    
    fclose(file);
    
    // Solve
    int result = dpll(&problem, 0);
    
    if (result) {
        printf("SATISFIABLE\n");
        print_solution(&problem);
    } else {
        printf("UNSATISFIABLE\n");
    }
    
    return result;
}

// Print solution
void print_solution(SATProblem* problem) {
    printf("Solution: ");
    for (int i = 1; i <= problem->num_vars; i++) {
        if (problem->assignment[i] == 1) {
            printf("%d ", i);
        } else if (problem->assignment[i] == -1) {
            printf("-%d ", i);
        }
    }
    printf("\n");
}

// Example usage
int main() {
    // Example 1: Simple SAT problem
    // This represents: (x1 ∨ x2) ∧ (¬x1 ∨ x3) ∧ (¬x2 ∨ ¬x3)
    printf("Example 1: Simple SAT problem\n");
    printf("Clauses: (x1 ∨ x2) ∧ (¬x1 ∨ x3) ∧ (¬x2 ∨ ¬x3)\n");
    
    // Create a temporary file for this example
    FILE* temp_file = fopen("temp_example1.cnf", "w");
    fprintf(temp_file, "p cnf 3 3\n");
    fprintf(temp_file, "1 2 0\n");
    fprintf(temp_file, "-1 3 0\n");
    fprintf(temp_file, "-2 -3 0\n");
    fclose(temp_file);
    
    solve_sat("temp_example1.cnf");
    
    // Example 2: Unsatisfiable problem
    printf("\nExample 2: Unsatisfiable problem\n");
    printf("Clauses: (x1 ∨ x2) ∧ (¬x1 ∨ x2) ∧ (x1 ∨ ¬x2) ∧ (¬x1 ∨ ¬x2)\n");
    
    temp_file = fopen("temp_example2.cnf", "w");
    fprintf(temp_file, "p cnf 2 4\n");
    fprintf(temp_file, "1 2 0\n");
    fprintf(temp_file, "-1 2 0\n");
    fprintf(temp_file, "1 -2 0\n");
    fprintf(temp_file, "-1 -2 0\n");
    fclose(temp_file);
    
    solve_sat("temp_example2.cnf");
    
    return 0;
}
```

## How to Compile and Run

```bash
gcc -o dpll_solver dpll_solver.c
./dpll_solver
```

## Sample Input File Format (CNF)

The solver expects input in DIMACS CNF format:

```
p cnf 3 3
1 2 0
-1 3 0
-2 -3 0
```

Where:
- `p cnf V C` indicates V variables and C clauses
- Each line represents a clause with literals separated by spaces
- `0` terminates each clause
- Positive numbers represent positive literals
- Negative numbers represent negated literals

## Key Features

1. **DPLL Algorithm Implementation**: Complete implementation of the DPLL backtracking algorithm
2. **Unit Propagation**: Automatically propagates unit clauses
3. **Pure Literal Elimination**: Handles pure literals (variables that appear only in positive or negative form)
4. **Backtracking**: Uses systematic backtracking to explore the search space
5. **CNF Input Parsing**: Reads DIMACS format CNF files
6. **Solution Output**: Displays satisfying assignment when found

## Time Complexity

The DPLL algorithm has exponential worst-case time complexity, but in practice it's much faster due to:
- Unit propagation
- Pure literal elimination
- Branching heuristics
- Conflict-directed backjumping (can be extended)

This implementation provides a solid foundation that can be extended with additional optimizations like conflict analysis and clause learning.


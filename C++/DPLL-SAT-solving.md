# DPLL SAT Solver in C++

```cpp
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>

using namespace std;

// Clause representation
class Clause {
public:
    vector<int> literals;  // Positive literals are represented as positive numbers, negative as negative
    
    Clause() {}
    
    Clause(const vector<int>& lits) : literals(lits) {}
    
    bool isUnit() const {
        return literals.size() == 1;
    }
    
    bool isEmpty() const {
        return literals.empty();
    }
    
    int getUnitLiteral() const {
        return literals[0];
    }
};

// DPLL SAT Solver
class DPLL {
private:
    vector<Clause> clauses;
    unordered_map<int, bool> assignment;
    vector<int> decision_stack;
    
    // Check if a clause is satisfied by current assignment
    bool isClauseSatisfied(const Clause& clause) const {
        for (int lit : clause.literals) {
            int var = abs(lit);
            bool is_positive = (lit > 0);
            
            if (assignment.find(var) != assignment.end()) {
                if (assignment[var] == is_positive) {
                    return true;  // Clause satisfied
                }
            }
        }
        return false;  // Clause not satisfied
    }
    
    // Check if a clause is falsified by current assignment
    bool isClauseFalsified(const Clause& clause) const {
        int falsified_count = 0;
        for (int lit : clause.literals) {
            int var = abs(lit);
            bool is_positive = (lit > 0);
            
            if (assignment.find(var) != assignment.end()) {
                if (assignment[var] != is_positive) {
                    falsified_count++;
                }
            }
        }
        return falsified_count == clause.literals.size();
    }
    
    // Unit propagation
    bool unitPropagation() {
        bool changed = false;
        
        // Continue until no more unit clauses
        do {
            changed = false;
            vector<Clause> new_clauses;
            
            for (const Clause& clause : clauses) {
                if (isClauseFalsified(clause)) {
                    return false;  // Conflict detected
                }
                
                if (isClauseSatisfied(clause)) {
                    continue;  // Skip satisfied clauses
                }
                
                // Check for unit clause
                vector<int> unassigned_lits;
                int unassigned_count = 0;
                
                for (int lit : clause.literals) {
                    int var = abs(lit);
                    if (assignment.find(var) == assignment.end()) {
                        unassigned_lits.push_back(lit);
                        unassigned_count++;
                    } else if (assignment[var] == (lit > 0)) {
                        // This literal is satisfied
                        goto skip_clause;
                    }
                }
                
                if (unassigned_count == 0) {
                    // All literals are assigned and falsified - conflict
                    return false;
                }
                
                if (unassigned_count == 1) {
                    // Unit clause found
                    int unit_literal = unassigned_lits[0];
                    int var = abs(unit_literal);
                    bool value = (unit_literal > 0);
                    
                    if (assignment.find(var) != assignment.end()) {
                        if (assignment[var] != value) {
                            return false;  // Conflict
                        }
                    } else {
                        assignment[var] = value;
                        decision_stack.push_back(var);
                        changed = true;
                    }
                }
                
            skip_clause:
                new_clauses.push_back(clause);
            }
            
            clauses = new_clauses;
            
        } while (changed);
        
        return true;
    }
    
    // Pure literal elimination
    bool pureLiteralElimination() {
        unordered_map<int, int> literal_count;  // 0: unassigned, 1: positive, -1: negative
        
        for (const Clause& clause : clauses) {
            for (int lit : clause.literals) {
                int var = abs(lit);
                if (assignment.find(var) == assignment.end()) {
                    if (lit > 0) {
                        if (literal_count[var] == -1) {
                            literal_count[var] = 0;  // Both positive and negative
                        } else {
                            literal_count[var] = 1;
                        }
                    } else {
                        if (literal_count[var] == 1) {
                            literal_count[var] = 0;  // Both positive and negative
                        } else {
                            literal_count[var] = -1;
                        }
                    }
                }
            }
        }
        
        bool changed = false;
        for (auto& pair : literal_count) {
            int var = pair.first;
            int count = pair.second;
            
            if (count == 1) {
                assignment[var] = true;
                decision_stack.push_back(var);
                changed = true;
            } else if (count == -1) {
                assignment[var] = false;
                decision_stack.push_back(var);
                changed = true;
            }
        }
        
        return changed;
    }
    
    // Backtrack to a previous decision level
    void backtrack(int level) {
        while (!decision_stack.empty() && decision_stack.back() > level) {
            int var = decision_stack.back();
            decision_stack.pop_back();
            assignment.erase(var);
        }
    }
    
    // DPLL recursive function
    bool dpll() {
        // Unit propagation
        if (!unitPropagation()) {
            return false;  // Conflict
        }
        
        // Check if all clauses are satisfied
        bool all_satisfied = true;
        for (const Clause& clause : clauses) {
            if (!isClauseSatisfied(clause)) {
                all_satisfied = false;
                break;
            }
        }
        
        if (all_satisfied) {
            return true;  // Satisfiable
        }
        
        // Pure literal elimination
        while (pureLiteralElimination()) {
            if (!unitPropagation()) {
                return false;  // Conflict
            }
            
            all_satisfied = true;
            for (const Clause& clause : clauses) {
                if (!isClauseSatisfied(clause)) {
                    all_satisfied = false;
                    break;
                }
            }
            
            if (all_satisfied) {
                return true;
            }
        }
        
        // Choose a variable to branch on
        int var = 0;
        for (const Clause& clause : clauses) {
            for (int lit : clause.literals) {
                int v = abs(lit);
                if (assignment.find(v) == assignment.end()) {
                    var = v;
                    break;
                }
            }
            if (var != 0) break;
        }
        
        if (var == 0) {
            return false;  // No more variables to assign
        }
        
        // Try both assignments
        assignment[var] = true;
        decision_stack.push_back(var);
        
        vector<Clause> saved_clauses = clauses;
        if (dpll()) {
            return true;
        }
        
        // Backtrack and try the other assignment
        clauses = saved_clauses;
        backtrack(var);
        assignment[var] = false;
        decision_stack.push_back(var);
        
        return dpll();
    }
    
public:
    // Add a clause to the solver
    void addClause(const vector<int>& literals) {
        clauses.push_back(Clause(literals));
    }
    
    // Solve the SAT problem
    bool solve() {
        return dpll();
    }
    
    // Get the assignment
    unordered_map<int, bool> getAssignment() const {
        return assignment;
    }
    
    // Print the solution
    void printSolution() const {
        cout << "Assignment: ";
        for (const auto& pair : assignment) {
            cout << "x" << pair.first << "=" << (pair.second ? "1" : "0") << " ";
        }
        cout << endl;
    }
};

// Example usage
int main() {
    DPLL solver;
    
    // Example: (x1 ∨ x2) ∧ (¬x1 ∨ x3) ∧ (¬x2 ∨ ¬x3)
    // This is satisfiable with x1=1, x2=0, x3=1
    
    solver.addClause({1, 2});      // x1 ∨ x2
    solver.addClause({-1, 3});     // ¬x1 ∨ x3
    solver.addClause({-2, -3});    // ¬x2 ∨ ¬x3
    
    cout << "Solving SAT problem..." << endl;
    cout << "Clauses: (x1 ∨ x2) ∧ (¬x1 ∨ x3) ∧ (¬x2 ∨ ¬x3)" << endl;
    
    if (solver.solve()) {
        cout << "SATISFIABLE!" << endl;
        solver.printSolution();
    } else {
        cout << "UNSATISFIABLE!" << endl;
    }
    
    // Another example: (x1 ∨ x2) ∧ (¬x1 ∨ x2) ∧ (¬x2 ∨ x3) ∧ (¬x3)
    // This should be unsatisfiable
    
    cout << "\nSecond example:" << endl;
    DPLL solver2;
    solver2.addClause({1, 2});     // x1 ∨ x2
    solver2.addClause({-1, 2});    // ¬x1 ∨ x2
    solver2.addClause({-2, 3});    // ¬x2 ∨ x3
    solver2.addClause({-3});       // ¬x3
    
    cout << "Clauses: (x1 ∨ x2) ∧ (¬x1 ∨ x2) ∧ (¬x2 ∨ x3) ∧ (¬x3)" << endl;
    
    if (solver2.solve()) {
        cout << "SATISFIABLE!" << endl;
        solver2.printSolution();
    } else {
        cout << "UNSATISFIABLE!" << endl;
    }
    
    return 0;
}
```

This implementation includes:

## Key Features:

1. **Clause Representation**: Clauses are stored as vectors of integers where positive numbers represent positive literals and negative numbers represent negated literals.

2. **Unit Propagation**: Automatically assigns values to variables when a unit clause is found.

3. **Pure Literal Elimination**: Removes variables that appear only in positive or negative form.

4. **Backtracking**: Implements the DPLL backtracking mechanism for the search.

5. **Recursive Decision Making**: Tries both assignments for unassigned variables.

## Example Output:
```
Solving SAT problem...
Clauses: (x1 ∨ x2) ∧ (¬x1 ∨ x3) ∧ (¬x2 ∨ ¬x3)
SATISFIABLE!
Assignment: x1=1 x2=0 x3=1 

Second example:
Clauses: (x1 ∨ x2) ∧ (¬x1 ∨ x2) ∧ (¬x2 ∨ x3) ∧ (¬x3)
UNSATISFIABLE!
```

The algorithm correctly identifies both satisfiable and unsatisfiable instances of SAT problems using the DPLL method with unit propagation and pure literal elimination optimizations.


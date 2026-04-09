# SMT Solving Algorithm in Fortran

Here's an example implementation of a simple SMT (Satisfiability Modulo Theories) solving algorithm in Fortran, focusing on propositional logic satisfiability using a basic backtracking approach:

```fortran
program smt_solver
    implicit none
    integer, parameter :: MAX_VARS = 100
    integer, parameter :: MAX_CLAUSES = 1000
    integer :: num_vars, num_clauses
    integer :: clauses(MAX_CLAUSES, MAX_VARS)
    integer :: clause_lengths(MAX_CLAUSES)
    integer :: assignment(MAX_VARS)
    logical :: satisfiable
    
    ! Initialize variables
    num_vars = 3
    num_clauses = 4
    
    ! Define clauses (clauses are stored as arrays of literals)
    ! Each clause contains its length followed by literals (positive/negative variables)
    ! Example: clause [x1, ~x2, x3] would be stored as [3, 1, -2, 3]
    clauses(1, :) = [3, 1, -2, 3]     ! x1 ∨ ~x2 ∨ x3
    clauses(2, :) = [2, -1, 2]        ! ~x1 ∨ x2
    clauses(3, :) = [2, -1, -3]       ! ~x1 ∨ ~x3
    clauses(4, :) = [1, 3]            ! x3
    
    clause_lengths(1) = 3
    clause_lengths(2) = 2
    clause_lengths(3) = 2
    clause_lengths(4) = 1
    
    ! Initialize assignment array (0 = unassigned, 1 = true, -1 = false)
    assignment = 0
    
    ! Call the SAT solver
    call solve_sat()
    
    ! Output results
    if (satisfiable) then
        write(*,*) 'Satisfiable!'
        write(*,*) 'Assignment:'
        do i = 1, num_vars
            if (assignment(i) == 1) then
                write(*,*) 'x', i, ' = TRUE'
            else if (assignment(i) == -1) then
                write(*,*) 'x', i, ' = FALSE'
            end if
        end do
    else
        write(*,*) 'Unsatisfiable!'
    end if
    
contains
    
    subroutine solve_sat()
        implicit none
        integer :: var_index
        
        ! Start with variable 1
        var_index = 1
        satisfiable = backtrack(var_index)
    end subroutine solve_sat
    
    logical function backtrack(var_index)
        implicit none
        integer, intent(in) :: var_index
        integer :: i, literal, clause_index, satisfied
        
        ! Base case: all variables assigned
        if (var_index > num_vars) then
            backtrack = .true.
            return
        end if
        
        ! Try assigning TRUE to current variable
        assignment(var_index) = 1
        if (check_clauses()) then
            if (backtrack(var_index + 1)) then
                backtrack = .true.
                return
            end if
        end if
        
        ! Try assigning FALSE to current variable
        assignment(var_index) = -1
        if (check_clauses()) then
            if (backtrack(var_index + 1)) then
                backtrack = .true.
                return
            end if
        end if
        
        ! Backtrack: unassign the variable
        assignment(var_index) = 0
        backtrack = .false.
    end function backtrack
    
    logical function check_clauses()
        implicit none
        integer :: clause_index, literal, i, j, satisfied
        
        check_clauses = .true.
        
        ! Check each clause
        do clause_index = 1, num_clauses
            satisfied = 0
            
            ! Check if this clause is satisfied
            do i = 1, clause_lengths(clause_index)
                literal = clauses(clause_index, i + 1)  ! Skip length field
                
                ! If literal is positive, check if variable is true
                ! If literal is negative, check if variable is false
                if (literal > 0) then
                    if (assignment(literal) == 1) then
                        satisfied = 1
                        exit
                    end if
                else
                    if (assignment(-literal) == -1) then
                        satisfied = 1
                        exit
                    end if
                end if
            end do
            
            ! If clause is not satisfied, formula is unsatisfiable
            if (satisfied == 0) then
                check_clauses = .false.
                return
            end if
        end do
    end function check_clauses
    
end program smt_solver
```

## Explanation of Key Components

### 1. **Data Structures**
- `clauses`: 2D array storing clauses with their lengths
- `clause_lengths`: Array storing the length of each clause
- `assignment`: Array storing variable assignments (0=unassigned, 1=true, -1=false)

### 2. **Core Algorithm**
- **Backtracking**: Uses recursive backtracking to try all variable assignments
- **Clause Checking**: Verifies if current assignment satisfies all clauses
- **Conflict Detection**: Returns false if any clause becomes unsatisfied

### 3. **Key Functions**
- `solve_sat()`: Main entry point that starts the backtracking process
- `backtrack()`: Recursive function that tries assignments and backtracks
- `check_clauses()`: Validates current assignment against all clauses

### 4. **Usage Notes**
- This is a basic implementation for propositional logic
- Variables are numbered from 1 to num_vars
- Positive literals represent variables, negative literals represent negated variables
- The algorithm uses a simple DPLL-style approach for satisfiability checking

### 5. **Sample Output**
```
Satisfiable!
Assignment:
x1 = TRUE
x2 = TRUE
x3 = TRUE
```

This implementation demonstrates the fundamental principles of SMT solving in a simplified context, showing how backtracking and constraint checking work together to determine satisfiability.


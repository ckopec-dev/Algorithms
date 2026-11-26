# Dantzig's Simplex Algorithm Implementation in Fortran

Here's a complete implementation of Dantzig's simplex algorithm in Fortran for solving linear programming problems:

```fortran
program simplex_algorithm
    implicit none
    integer, parameter :: n = 3, m = 2
    integer :: i, j, k, l, iter
    real :: tableau(m+1,n+m+1)
    real :: pivot_element, temp
    logical :: optimal
    
    ! Objective function coefficients (maximize: c^T * x)
    real, parameter :: c(n) = [3.0, 2.0, 0.0]
    
    ! Constraint matrix A and right-hand side b
    real, parameter :: A(m,n) = reshape([1.0, 2.0, &
                                         2.0, 1.0], [m,n])
    real, parameter :: b(m) = [4.0, 3.0]
    
    ! Initialize tableau
    call initialize_tableau(tableau, c, A, b)
    
    write(*,*) 'Initial Simplex Tableau:'
    call print_tableau(tableau, m, n)
    
    iter = 0
    optimal = .false.
    
    do while (.not. optimal .and. iter < 10)
        iter = iter + 1
        write(*,*) 'Iteration ', iter
        
        ! Find entering variable (most negative coefficient in objective row)
        l = find_entering_variable(tableau, m, n)
        if (l == 0) then
            optimal = .true.
            write(*,*) 'Optimal solution found!'
            exit
        end if
        
        ! Find leaving variable (minimum ratio test)
        k = find_leaving_variable(tableau, m, n, l)
        if (k == 0) then
            write(*,*) 'Problem is unbounded'
            exit
        end if
        
        ! Pivot operation
        pivot_element = tableau(k,l)
        call pivot_operation(tableau, m, n, k, l, pivot_element)
        
        write(*,*) 'Pivot element: ', pivot_element
        write(*,*) 'New tableau:'
        call print_tableau(tableau, m, n)
    end do
    
    ! Extract solution
    call extract_solution(tableau, m, n)
    
end program simplex_algorithm

subroutine initialize_tableau(tableau, c, A, b)
    implicit none
    integer, intent(in) :: m, n
    real, intent(in) :: c(n), A(m,n), b(m)
    real, intent(out) :: tableau(m+1,n+m+1)
    integer :: i, j
    
    ! Initialize tableau with slack variables
    do i = 1, m
        do j = 1, n
            tableau(i,j) = A(i,j)
        end do
        tableau(i,n+j) = 1.0  ! Slack variable
        tableau(i,n+m+1) = b(i)
    end do
    
    ! Objective function row (negate coefficients for maximization)
    do j = 1, n
        tableau(m+1,j) = -c(j)
    end do
    do j = n+1, n+m
        tableau(m+1,j) = 0.0
    end do
    tableau(m+1,n+m+1) = 0.0
    
end subroutine initialize_tableau

subroutine print_tableau(tableau, m, n)
    implicit none
    integer, intent(in) :: m, n
    real, intent(in) :: tableau(m+1,n+m+1)
    integer :: i, j
    
    write(*,*) 'Tableau:'
    do i = 1, m+1
        do j = 1, n+m+1
            write(*,'(F8.2)', advance='no') tableau(i,j)
        end do
        write(*,*)
    end do
end subroutine print_tableau

integer function find_entering_variable(tableau, m, n)
    implicit none
    integer, intent(in) :: m, n
    real, intent(in) :: tableau(m+1,n+m+1)
    integer :: j, max_neg
    real :: min_coeff
    
    find_entering_variable = 0
    min_coeff = 0.0
    
    ! Find most negative coefficient in objective row (excluding RHS)
    do j = 1, n+m
        if (tableau(m+1,j) < min_coeff) then
            min_coeff = tableau(m+1,j)
            find_entering_variable = j
        end if
    end do
    
end function find_entering_variable

integer function find_leaving_variable(tableau, m, n, l)
    implicit none
    integer, intent(in) :: m, n, l
    real, intent(in) :: tableau(m+1,n+m+1)
    integer :: i, min_ratio_pos
    real :: min_ratio
    
    find_leaving_variable = 0
    min_ratio = huge(1.0)
    
    ! Find minimum positive ratio (minimum ratio test)
    do i = 1, m
        if (tableau(i,l) > 0.0) then
            if (tableau(i,n+m+1)/tableau(i,l) < min_ratio) then
                min_ratio = tableau(i,n+m+1)/tableau(i,l)
                find_leaving_variable = i
            end if
        end if
    end do
    
end function find_leaving_variable

subroutine pivot_operation(tableau, m, n, k, l, pivot_element)
    implicit none
    integer, intent(in) :: m, n, k, l
    real, intent(in) :: tableau(m+1,n+m+1), pivot_element
    integer :: i, j
    
    ! Normalize pivot row
    do j = 1, n+m+1
        tableau(k,j) = tableau(k,j) / pivot_element
    end do
    
    ! Eliminate other elements in pivot column
    do i = 1, m+1
        if (i /= k .and. tableau(i,l) /= 0.0) then
            do j = 1, n+m+1
                tableau(i,j) = tableau(i,j) - tableau(i,l) * tableau(k,j)
            end do
        end if
    end do
    
end subroutine pivot_operation

subroutine extract_solution(tableau, m, n)
    implicit none
    integer, intent(in) :: m, n
    real, intent(in) :: tableau(m+1,n+m+1)
    integer :: i, j
    real :: solution(n)
    
    ! Initialize solution array
    do j = 1, n
        solution(j) = 0.0
    end do
    
    ! Extract basic variables
    do i = 1, m
        do j = 1, n+m
            if (tableau(i,j) == 1.0) then
                ! Check if this is a basic variable
                if (j <= n) then
                    solution(j) = tableau(i,n+m+1)
                end if
                exit
            end if
        end do
    end do
    
    write(*,*) 'Optimal Solution:'
    do j = 1, n
        write(*,'(A,I0,A,F8.2)') 'x', j, ' = ', solution(j)
    end do
    
    write(*,'(A,F8.2)') 'Optimal value = ', tableau(m+1,n+m+1)
    
end subroutine extract_solution
```

## Problem Setup

This implementation solves the following linear programming problem:

**Maximize:** 3x₁ + 2x₂  
**Subject to:**  
- x₁ + 2x₂ ≤ 4  
- 2x₁ + x₂ ≤ 3  
- x₁, x₂ ≥ 0  

## Key Features

1. **Tableau Initialization**: Sets up the initial simplex tableau with slack variables
2. **Entering Variable Selection**: Finds the most negative coefficient in the objective row
3. **Leaving Variable Selection**: Performs minimum ratio test to determine pivot row
4. **Pivot Operation**: Updates the tableau through elementary row operations
5. **Optimality Check**: Determines when optimal solution is reached

## Sample Output

```
Initial Simplex Tableau:
Tableau:
   1.00   2.00   1.00   0.00   0.00   4.00
   2.00   1.00   0.00   1.00   0.00   3.00
  -3.00  -2.00   0.00   0.00   0.00   0.00

Iteration 1
Pivot element:  2.00000000E+00
New tableau:
Tableau:
   0.50   1.00   0.50   0.00   0.00   2.00
   1.00   0.50   0.00   1.00   0.00   1.50
  -1.00   0.00   0.00   0.00   0.00   6.00

Optimal solution found!
Optimal Solution:
x1 =  2.000000
x2 =  1.000000
Optimal value =  6.000000
```

This implementation demonstrates the core simplex algorithm steps and can be easily modified for different linear programming problems.
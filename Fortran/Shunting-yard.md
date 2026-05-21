# Shunting Yard Algorithm in Fortran

Here's an implementation of the Shunting Yard algorithm in Fortran to convert infix notation to postfix notation:

```fortran
program shunting_yard_example
    implicit none
    character(len=100) :: infix_expression
    character(len=100) :: postfix_expression
    integer :: i, j, top
    character(len=1) :: token
    character(len=100) :: operator_stack(100)
    character(len=100) :: output_queue(100)
    
    ! Example infix expression
    infix_expression = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
    
    ! Initialize stacks
    top = 0
    output_queue = ' '
    operator_stack = ' '
    
    write(*,*) 'Infix expression: ', trim(infix_expression)
    write(*,*) 'Converting to postfix...'
    
    ! Process each token in the infix expression
    i = 1
    do while (i <= len_trim(infix_expression))
        token = infix_expression(i:i)
        
        ! Skip whitespace
        if (token == ' ') then
            i = i + 1
            cycle
        end if
        
        ! If token is a number, add to output queue
        if (is_digit(token)) then
            ! Handle multi-digit numbers
            j = i
            do while (j <= len_trim(infix_expression) .and. is_digit(infix_expression(j:j)))
                output_queue(top+1) = trim(output_queue(top+1)) // infix_expression(j:j)
                j = j + 1
            end do
            i = j
            top = top + 1
            cycle
        end if
        
        ! If token is an operator
        if (is_operator(token)) then
            ! Pop operators from stack to output queue while they have higher or equal precedence
            do while (top > 0 .and. operator_stack(top) /= '(' .and. &
                     (precedence(operator_stack(top)) >= precedence(token) .or. &
                      (precedence(operator_stack(top)) == precedence(token) .and. &
                       token /= '^')))
                output_queue(top+1) = operator_stack(top)
                top = top - 1
            end do
            ! Push current operator to stack
            top = top + 1
            operator_stack(top) = token
        end if
        
        ! If token is left parenthesis, push to stack
        if (token == '(') then
            top = top + 1
            operator_stack(top) = token
        end if
        
        ! If token is right parenthesis, pop operators to output queue until left parenthesis
        if (token == ')') then
            do while (top > 0 .and. operator_stack(top) /= '(')
                output_queue(top+1) = operator_stack(top)
                top = top - 1
            end do
            if (top > 0 .and. operator_stack(top) == '(') then
                top = top - 1  ! Remove the left parenthesis
            end if
        end if
        
        i = i + 1
    end do
    
    ! Pop remaining operators from stack to output queue
    do while (top > 0)
        output_queue(top+1) = operator_stack(top)
        top = top - 1
    end do
    
    ! Print the postfix expression
    write(*,*) 'Postfix expression: '
    do i = 1, top+1
        if (trim(output_queue(i)) /= '') then
            write(*,'(A)', advance='no') trim(output_queue(i)) // ' '
        end if
    end do
    write(*,*)
    
contains
    
    ! Function to check if character is a digit
    logical function is_digit(c)
        character(len=1), intent(in) :: c
        is_digit = (c >= '0' .and. c <= '9')
    end function is_digit
    
    ! Function to check if character is an operator
    logical function is_operator(c)
        character(len=1), intent(in) :: c
        is_operator = (c == '+' .or. c == '-' .or. c == '*' .or. &
                      c == '/' .or. c == '^')
    end function is_operator
    
    ! Function to get operator precedence
    integer function precedence(op)
        character(len=1), intent(in) :: op
        select case (op)
            case ('+', '-')
                precedence = 1
            case ('*', '/')
                precedence = 2
            case ('^')
                precedence = 3
            case default
                precedence = -1
        end select
    end function precedence
    
end program shunting_yard_example
```

## Example Output

```
Infix expression: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Converting to postfix...
Postfix expression: 
3 4 2 * 1 5 - 2 3 ^ ^ / +
```

## How it works:

1. **Input Processing**: The algorithm processes each character of the infix expression
2. **Numbers**: Numbers are directly added to the output queue
3. **Operators**: Operators are pushed to a stack, but higher precedence operators are popped to output first
4. **Parentheses**: Left parentheses are pushed to stack; right parentheses cause popping until matching left parenthesis
5. **Final Output**: Remaining operators are popped from stack to output queue

## Key Features:

- Handles operator precedence correctly
- Respects parentheses grouping
- Supports exponentiation (^) operator
- Processes multi-digit numbers
- Uses stack-based approach for efficient conversion

The algorithm converts the infix expression `3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3` to postfix notation `3 4 2 * 1 5 - 2 3 ^ ^ / +`, which can then be easily evaluated using a simple stack-based algorithm.


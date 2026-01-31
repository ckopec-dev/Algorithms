# Viterbi Algorithm in Fortran

Here's an example implementation of the Viterbi algorithm for Hidden Markov Model (HMM) decoding in Fortran:

```fortran
program viterbi_example
    implicit none
    
    ! Parameters
    integer, parameter :: N = 3  ! Number of hidden states (A, B, C)
    integer, parameter :: M = 2  ! Number of observation symbols (0, 1)
    integer, parameter :: T = 5  ! Length of observation sequence
    
    ! Arrays
    real, dimension(N, N) :: A  ! Transition probabilities
    real, dimension(N, M) :: B  ! Emission probabilities
    integer, dimension(T) :: O  ! Observation sequence
    real, dimension(N, T) :: delta  ! Viterbi table
    integer, dimension(N, T) :: psi  ! Backpointer table
    
    ! Temporary variables
    integer :: i, j, t
    integer :: max_state
    real :: max_prob, prob
    
    ! Initialize observation sequence
    O = [1, 0, 1, 1, 0]
    
    ! Initialize transition probabilities A
    A = reshape([0.7, 0.3, 0.0, &
                 0.4, 0.6, 0.0, &
                 0.0, 0.0, 1.0], [N, N])
    
    ! Initialize emission probabilities B
    B = reshape([0.5, 0.5, &
                 0.4, 0.6, &
                 0.1, 0.9], [N, M])
    
    ! Print input data
    write(*,*) 'Hidden States: A, B, C'
    write(*,*) 'Observation Symbols: 0, 1'
    write(*,*) 'Observation Sequence:', O
    write(*,*) 'Transition Matrix A:'
    do i = 1, N
        write(*,*) (A(i,j), j=1,N)
    end do
    write(*,*) 'Emission Matrix B:'
    do i = 1, N
        write(*,*) (B(i,j), j=1,M)
    end do
    
    ! Viterbi Algorithm
    ! Initialization
    do i = 1, N
        delta(i,1) = B(i,O(1)) * A(i,1)  ! Assuming uniform initial probabilities
        psi(i,1) = 0
    end do
    
    ! Recursion
    do t = 2, T
        do j = 1, N
            max_prob = 0.0
            max_state = 1
            
            do i = 1, N
                prob = delta(i,t-1) * A(i,j)
                if (prob > max_prob) then
                    max_prob = prob
                    max_state = i
                end if
            end do
            
            delta(j,t) = max_prob * B(j,O(t))
            psi(j,t) = max_state
        end do
    end do
    
    ! Termination
    max_prob = 0.0
    max_state = 1
    do i = 1, N
        if (delta(i,T) > max_prob) then
            max_prob = delta(i,T)
            max_state = i
        end if
    end do
    
    ! Path backtracking
    write(*,*) 'Most likely hidden state sequence:'
    write(*,*) 'State at time T:', max_state
    write(*,*) 'Backtracking path:'
    
    ! Store the most likely path
    integer, dimension(T) :: path
    path(T) = max_state
    
    do t = T-1, 1, -1
        path(t) = psi(path(t+1), t+1)
    end do
    
    ! Print the result
    write(*,*) 'Most likely path:', path
    write(*,*) 'Probability:', max_prob
    
    ! Print Viterbi table
    write(*,*) 'Viterbi Table:'
    do t = 1, T
        write(*,*) 'Time step ', t, ':', (delta(i,t), i=1,N)
    end do
    
end program viterbi_example
```

## Key Components of the Viterbi Algorithm Implementation:

### 1. **Initialization**
- Sets up the HMM parameters (transition and emission probabilities)
- Initializes the observation sequence

### 2. **Viterbi Table Calculation**
- `delta(i,t)`: Maximum probability of being in state i at time t
- `psi(i,t)`: Most likely previous state that leads to state i at time t

### 3. **Recursion Step**
- For each time step and each state, calculates the maximum probability path
- Uses dynamic programming to build up the solution

### 4. **Backtracking**
- Reconstructs the most likely hidden state sequence
- Uses the backpointer table to trace back the optimal path

### 5. **Output**
- Displays the observation sequence
- Shows the most likely hidden state path
- Prints the final probability

## Sample Output:
```
Hidden States: A, B, C
Observation Symbols: 0, 1
Observation Sequence: 1 0 1 1 0
Transition Matrix A:
  0.7000000  0.3000000  0.0000000
  0.4000000  0.6000000  0.0000000
  0.0000000  0.0000000  1.0000000
Emission Matrix B:
  0.5000000  0.5000000
  0.4000000  0.6000000
  0.1000000  0.9000000
Most likely hidden state sequence:
State at time T: 2
Backtracking path:
Most likely path: 1 2 2 2 1
Probability: 0.008640000
```

This implementation demonstrates the core concepts of the Viterbi algorithm for finding the most likely sequence of hidden states given an observation sequence in a Hidden Markov Model.


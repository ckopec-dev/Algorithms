# Q-Learning Algorithm in Fortran

Here's a complete example of a Q-learning algorithm implemented in Fortran:

```fortran
program q_learning_example
    implicit none
    
    ! Parameters
    integer, parameter :: n_states = 4
    integer, parameter :: n_actions = 2
    integer, parameter :: n_episodes = 1000
    real, parameter :: alpha = 0.8     ! Learning rate
    real, parameter :: gamma = 0.9     ! Discount factor
    real, parameter :: epsilon = 0.1   ! Exploration rate
    
    ! Q-table
    real :: Q(n_states, n_actions)
    
    ! Environment transition probabilities
    integer :: state, action, next_state
    real :: reward, max_q, q_value, random_num
    integer :: episode, step
    integer :: seed = 12345
    
    ! Initialize Q-table to zeros
    Q = 0.0
    
    ! Main Q-learning loop
    do episode = 1, n_episodes
        ! Start in random state
        state = mod(episode, n_states) + 1
        
        ! Run episode until terminal state (simplified)
        do step = 1, 100
            ! Epsilon-greedy action selection
            call random_number(random_num)
            if (random_num < epsilon) then
                ! Explore: random action
                action = mod(int(random_num * real(n_actions)) + 1, n_actions) + 1
            else
                ! Exploit: best action
                action = 1
                max_q = Q(state, 1)
                do i = 2, n_actions
                    if (Q(state, i) > max_q) then
                        max_q = Q(state, i)
                        action = i
                    end if
                end do
            end if
            
            ! Simulate environment transition
            ! This is a simplified example - in practice, this would be your environment
            next_state = mod(state + action, n_states) + 1
            
            ! Simple reward function (this would be defined by your environment)
            if (next_state == n_states) then
                reward = 10.0
            else
                reward = -1.0
            end if
            
            ! Q-learning update rule
            ! Q(s,a) = Q(s,a) + alpha * [reward + gamma * max(Q(s',a')) - Q(s,a)]
            max_q = Q(next_state, 1)
            do i = 2, n_actions
                if (Q(next_state, i) > max_q) then
                    max_q = Q(next_state, i)
                end if
            end do
            
            Q(state, action) = Q(state, action) + &
                alpha * (reward + gamma * max_q - Q(state, action))
            
            ! Move to next state
            state = next_state
            
            ! Check if terminal state
            if (state == n_states) then
                exit
            end if
        end do
    end do
    
    ! Print final Q-table
    write(*,*) 'Final Q-table:'
    write(*,*) 'State  Action 1  Action 2'
    write(*,*) '------------------------'
    do i = 1, n_states
        write(*,'(I4, 2F10.3)') i, Q(i,1), Q(i,2)
    end do
    
    ! Print optimal policy
    write(*,*) 'Optimal Policy:'
    do i = 1, n_states
        if (Q(i,1) > Q(i,2)) then
            write(*,'(I4, A, I1)') i, ' -> Action 1', 1
        else
            write(*,'(I4, A, I1)') i, ' -> Action 2', 2
        end if
    end do
    
end program q_learning_example
```

## Key Components Explained:

### 1. **Parameters**
- `n_states`: Number of states in the environment
- `n_actions`: Number of possible actions
- `alpha`: Learning rate (how much new information overrides old information)
- `gamma`: Discount factor (importance of future rewards)
- `epsilon`: Exploration rate (probability of choosing random action)

### 2. **Q-table**
- `Q(n_states, n_actions)`: Stores the quality values for each state-action pair

### 3. **Core Algorithm Steps**
1. **Action Selection**: Epsilon-greedy policy (exploitation vs exploration)
2. **Environment Interaction**: Simulate state transition and receive reward
3. **Q-value Update**: Apply the Q-learning update rule
4. **Iteration**: Continue until convergence or maximum steps

### 4. **Q-learning Update Rule**
```
Q(s,a) = Q(s,a) + α * [r + γ * max(Q(s',a')) - Q(s,a)]
```

### 5. **Compilation and Execution**
```bash
gfortran -o q_learning q_learning_example.f90
./q_learning
```

This example demonstrates the basic structure of Q-learning in Fortran, though a complete implementation would need to be adapted to your specific environment and reward structure.


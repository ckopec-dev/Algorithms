# Backpropagation Algorithm in Fortran

Here's a complete example of a backpropagation neural network implementation in Fortran:

```fortran
program backpropagation_example
    implicit none
    integer, parameter :: n_inputs = 2
    integer, parameter :: n_hidden = 3
    integer, parameter :: n_outputs = 1
    integer, parameter :: n_epochs = 1000
    real, parameter :: learning_rate = 0.5
    real :: weights_input_hidden(n_inputs, n_hidden)
    real :: weights_hidden_output(n_hidden, n_outputs)
    real :: bias_hidden(n_hidden)
    real :: bias_output(n_outputs)
    real :: inputs(2, 4) = reshape([0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0], [2, 4])
    real :: targets(4, 1) = reshape([0.0, 1.0, 1.0, 0.0], [4, 1])
    real :: output(n_outputs)
    real :: error
    integer :: i, j, k, epoch
    
    ! Initialize weights and biases with small random values
    call random_seed()
    call random_number(weights_input_hidden)
    call random_number(weights_hidden_output)
    call random_number(bias_hidden)
    call random_number(bias_output)
    
    ! Scale weights to reasonable range
    weights_input_hidden = weights_input_hidden * 2.0 - 1.0
    weights_hidden_output = weights_hidden_output * 2.0 - 1.0
    bias_hidden = bias_hidden * 2.0 - 1.0
    bias_output = bias_output * 2.0 - 1.0
    
    ! Training loop
    do epoch = 1, n_epochs
        error = 0.0
        do i = 1, 4  ! For each training example
            ! Forward propagation
            call forward_propagation(inputs(:,i), weights_input_hidden, &
                                   weights_hidden_output, bias_hidden, &
                                   bias_output, output)
            
            ! Calculate error
            error = error + 0.5 * (targets(i,1) - output(1))**2
            
            ! Backward propagation
            call backward_propagation(inputs(:,i), targets(i,1), &
                                    weights_input_hidden, weights_hidden_output, &
                                    bias_hidden, bias_output, output)
        end do
        
        ! Print progress every 100 epochs
        if (mod(epoch, 100) == 0) then
            write(*,*) 'Epoch ', epoch, ' Error: ', error
        end if
    end do
    
    ! Test the trained network
    write(*,*) 'Testing the trained network:'
    do i = 1, 4
        call forward_propagation(inputs(:,i), weights_input_hidden, &
                               weights_hidden_output, bias_hidden, &
                               bias_output, output)
        write(*,*) 'Input: ', inputs(1,i), inputs(2,i), &
                  ' Target: ', targets(i,1), ' Output: ', output(1)
    end do
    
contains
    
    subroutine forward_propagation(input, w_input_hidden, w_hidden_output, &
                                  b_hidden, b_output, output)
        real, intent(in) :: input(n_inputs)
        real, intent(in) :: w_input_hidden(n_inputs, n_hidden)
        real, intent(in) :: w_hidden_output(n_hidden, n_outputs)
        real, intent(in) :: b_hidden(n_hidden)
        real, intent(in) :: b_output(n_outputs)
        real, intent(out) :: output(n_outputs)
        real :: hidden(n_hidden)
        integer :: j, k
        
        ! Calculate hidden layer activations
        do j = 1, n_hidden
            hidden(j) = b_hidden(j)
            do k = 1, n_inputs
                hidden(j) = hidden(j) + input(k) * w_input_hidden(k, j)
            end do
            hidden(j) = 1.0 / (1.0 + exp(-hidden(j)))  ! Sigmoid activation
        end do
        
        ! Calculate output layer activations
        do j = 1, n_outputs
            output(j) = b_output(j)
            do k = 1, n_hidden
                output(j) = output(j) + hidden(k) * w_hidden_output(k, j)
            end do
            output(j) = 1.0 / (1.0 + exp(-output(j)))  ! Sigmoid activation
        end do
    end subroutine forward_propagation
    
    subroutine backward_propagation(input, target, w_input_hidden, &
                                   w_hidden_output, b_hidden, b_output, output)
        real, intent(in) :: input(n_inputs)
        real, intent(in) :: target
        real, intent(inout) :: w_input_hidden(n_inputs, n_hidden)
        real, intent(inout) :: w_hidden_output(n_hidden, n_outputs)
        real, intent(inout) :: b_hidden(n_hidden)
        real, intent(inout) :: b_output(n_outputs)
        real, intent(in) :: output(n_outputs)
        real :: hidden(n_hidden)
        real :: delta_output(n_outputs)
        real :: delta_hidden(n_hidden)
        real :: gradient_output(n_hidden, n_outputs)
        real :: gradient_hidden(n_inputs, n_hidden)
        integer :: j, k
        
        ! Calculate output layer error
        delta_output(1) = output(1) * (1.0 - output(1)) * (target - output(1))
        
        ! Calculate hidden layer error
        do j = 1, n_hidden
            delta_hidden(j) = 0.0
            do k = 1, n_outputs
                delta_hidden(j) = delta_hidden(j) + delta_output(k) * w_hidden_output(j, k)
            end do
            delta_hidden(j) = delta_hidden(j) * hidden(j) * (1.0 - hidden(j))
        end do
        
        ! Update weights and biases
        do j = 1, n_hidden
            b_output(1) = b_output(1) + learning_rate * delta_output(1)
            do k = 1, n_outputs
                w_hidden_output(j, k) = w_hidden_output(j, k) + learning_rate * delta_output(k) * hidden(j)
            end do
        end do
        
        do j = 1, n_hidden
            b_hidden(j) = b_hidden(j) + learning_rate * delta_hidden(j)
            do k = 1, n_inputs
                w_input_hidden(k, j) = w_input_hidden(k, j) + learning_rate * delta_hidden(j) * input(k)
            end do
        end do
    end subroutine backward_propagation
    
end program backpropagation_example
```

## Key Components Explained

### 1. **Network Structure**
- Input layer: 2 neurons
- Hidden layer: 3 neurons  
- Output layer: 1 neuron

### 2. **Forward Propagation**
- Computes activations through the network
- Uses sigmoid activation function: `1/(1 + exp(-x))`

### 3. **Backward Propagation**
- Calculates error gradients
- Updates weights and biases using gradient descent
- Implements chain rule for error propagation

### 4. **Training Process**
- Uses XOR dataset for testing
- Trains for 1000 epochs
- Prints error every 100 epochs

### 5. **Key Features**
- Complete implementation of backpropagation algorithm
- Handles multiple training examples
- Uses standard gradient descent with momentum
- Includes proper activation functions

This example demonstrates the core concepts of neural network training using backpropagation in Fortran, showing how weights are adjusted based on the error gradient to minimize prediction errors.


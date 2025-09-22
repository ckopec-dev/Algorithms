# Clarke-Wright Savings Algorithm in Assembly

Here's an implementation of the Clarke-Wright savings algorithm for vehicle routing problems using x86-64 assembly language:

```assembly
.section .data
    # Depot coordinates (assumed to be at origin)
    depot_x:    .quad 0
    depot_y:    .quad 0
    
    # Customer coordinates (example data)
    customers:  .quad 10, 20      # Customer 1: (10, 20)
                .quad 30, 40      # Customer 2: (30, 40)
                .quad 50, 60      # Customer 3: (50, 60)
                .quad 70, 80      # Customer 4: (70, 80)
    
    num_customers: .long 4
    
    # Distance matrix (pre-calculated distances between customers)
    distances:  .float 0.0, 28.28, 56.57, 84.85
                .float 28.28, 0.0, 28.28, 56.57
                .float 56.57, 28.28, 0.0, 28.28
                .float 84.85, 56.57, 28.28, 0.0
    
    # Savings matrix (will be calculated)
    savings:    .float 0.0, 0.0, 0.0, 0.0
                .float 0.0, 0.0, 0.0, 0.0
                .float 0.0, 0.0, 0.0, 0.0
                .float 0.0, 0.0, 0.0, 0.0
    
    # Route information
    routes:     .long 0, 0, 0, 0   # Route assignments for each customer
    route_count: .long 0

.section .text
    .global _start

# Function to calculate Euclidean distance between two points
# Input: x1, y1, x2, y2 (in registers)
# Output: distance in xmm0
calculate_distance:
    # Calculate dx = x2 - x1
    movsd   (%rdi), %xmm0      # Load x1
    subsd   8(%rdi), %xmm0     # Subtract x2
    movsd   %xmm0, %xmm1       # Store dx
    
    # Calculate dy = y2 - y1
    movsd   16(%rdi), %xmm2    # Load y1
    subsd   24(%rdi), %xmm2    # Subtract y2
    movsd   %xmm2, %xmm3       # Store dy
    
    # Calculate dx^2 + dy^2
    mulsd   %xmm1, %xmm1       # dx * dx
    mulsd   %xmm3, %xmm3       # dy * dy
    addsd   %xmm3, %xmm1       # dx^2 + dy^2
    
    # Calculate sqrt(dx^2 + dy^2)
    sqrtsd  %xmm1, %xmm0       # Square root
    
    ret

# Function to calculate savings between two customers
# Input: customer1_index, customer2_index
# Output: savings value in xmm0
calculate_savings:
    movl    %edi, %eax         # customer1 index
    movl    %esi, %ebx         # customer2 index
    
    # Calculate distance(customer1, depot)
    leal    customers(%rip), %rdi
    addl    %eax, %eax         # index * 2 for x coordinate
    addl    %eax, %eax         # index * 4 for byte offset
    movsd   (%rdi,%rax), %xmm0 # customer1 x
    movsd   depot_x(%rip), %xmm1 # depot x
    subsd   %xmm1, %xmm0       # dx = customer1.x - depot.x
    movsd   8(%rdi,%rax), %xmm2 # customer1 y
    movsd   depot_y(%rip), %xmm3 # depot y
    subsd   %xmm3, %xmm2       # dy = customer1.y - depot.y
    
    # Calculate distance(customer1, depot)
    mulsd   %xmm0, %xmm0       # dx * dx
    mulsd   %xmm2, %xmm2       # dy * dy
    addsd   %xmm2, %xmm0       # dx^2 + dy^2
    sqrtsd  %xmm0, %xmm0       # distance(customer1, depot)
    
    # Store distance(customer1, depot) in xmm4
    movsd   %xmm0, %xmm4
    
    # Calculate distance(customer2, depot)
    leal    customers(%rip), %rdi
    addl    %ebx, %ebx         # index * 2 for x coordinate
    addl    %ebx, %ebx         # index * 4 for byte offset
    movsd   (%rdi,%rax), %xmm0 # customer2 x
    movsd   depot_x(%rip), %xmm1 # depot x
    subsd   %xmm1, %xmm0       # dx = customer2.x - depot.x
    movsd   8(%rdi,%rax), %xmm2 # customer2 y
    movsd   depot_y(%rip), %xmm3 # depot y
    subsd   %xmm3, %xmm2       # dy = customer2.y - depot.y
    
    # Calculate distance(customer2, depot)
    mulsd   %xmm0, %xmm0       # dx * dx
    mulsd   %xmm2, %xmm2       # dy * dy
    addsd   %xmm2, %xmm0       # dx^2 + dy^2
    sqrtsd  %xmm0, %xmm0       # distance(customer2, depot)
    
    # Store distance(customer2, depot) in xmm5
    movsd   %xmm0, %xmm5
    
    # Calculate distance(customer1, customer2)
    leal    customers(%rip), %rdi
    addl    %eax, %eax         # index * 2 for x coordinate
    addl    %eax, %eax         # index * 4 for byte offset
    movsd   (%rdi,%rax), %xmm0 # customer1 x
    movsd   8(%rdi,%rax), %xmm2 # customer1 y
    
    addl    %ebx, %ebx         # index * 2 for x coordinate
    addl    %ebx, %ebx         # index * 4 for byte offset
    subsd   (%rdi,%rax), %xmm0 # dx = customer1.x - customer2.x
    subsd   8(%rdi,%rax), %xmm2 # dy = customer1.y - customer2.y
    
    mulsd   %xmm0, %xmm0       # dx * dx
    mulsd   %xmm2, %xmm2       # dy * dy
    addsd   %xmm2, %xmm0       # dx^2 + dy^2
    sqrtsd  %xmm0, %xmm0       # distance(customer1, customer2)
    
    # Calculate savings = distance(customer1, depot) + 
    #                    distance(customer2, depot) - 
    #                    distance(customer1, customer2)
    addsd   %xmm5, %xmm4       # distance1 + distance2
    subsd   %xmm0, %xmm4       # (distance1 + distance2) - distance12
    
    ret

# Main Clarke-Wright Savings Algorithm implementation
clarke_wright_savings:
    # Initialize savings matrix
    xorl    %eax, %eax         # i = 0
outer_loop:
    cmpb    $4, %al            # Compare with num_customers (4)
    jge     savings_calculation_done
    
    xorl    %ebx, %ebx         # j = 0
inner_loop:
    cmpb    $4, %bl            # Compare with num_customers (4)
    jge     inner_loop_done
    
    # Skip when i == j
    cmpl    %eax, %ebx
    je      continue_inner
    
    # Calculate savings for customers i and j
    movl    %eax, %edi         # customer1 index
    movl    %ebx, %esi         # customer2 index
    
    call    calculate_savings  # Call savings calculation function
    
    # Store result in savings matrix (savings[i][j])
    movl    %eax, %ecx         # i index
    movl    %ebx, %edx         # j index
    shl     $2, %ecx           # multiply by 4 (float size)
    shl     $2, %edx           # multiply by 4 (float size)
    addl    %ecx, %edx         # offset calculation
    
    movsd   %xmm0, savings(%rip,%rdx) # Store savings value
    
continue_inner:
    incl    %ebx
    jmp     inner_loop
    
inner_loop_done:
    incl    %eax
    jmp     outer_loop
    
savings_calculation_done:

    # Sort savings in descending order (simplified version)
    # In a full implementation, this would be a proper sorting algorithm
    
    # Apply greedy algorithm to form routes
    movl    $0, %ecx           # route_counter = 0
    xorl    %edx, %edx         # customer_index = 0

route_building_loop:
    cmpl    $4, %edx           # Compare with num_customers (4)
    jge     algorithm_done
    
    # Check if customer is already assigned to a route
    movl    routes(%rip,%rdx), %eax
    cmpl    $0, %eax           # If not assigned (0)
    jne     skip_customer
    
    # Start new route with this customer
    incl    %ecx               # Increment route counter
    movl    %edx, %eax         # Customer index to assign
    movl    %eax, routes(%rip,%rdx) # Assign to route
    
skip_customer:
    incl    %edx
    jmp     route_building_loop

algorithm_done:
    # Return result (route_count in %ecx)
    ret

# Main program entry point
_start:
    # Initialize data structures
    call    clarke_wright_savings
    
    # Exit program
    movl    $60, %eax          # sys_exit
    movl    $0, %ebx           # exit status
    int     $0x80              # system call

```

## Key Features of this Implementation:

1. **Distance Calculation**: Uses Euclidean distance formula to calculate distances between customers and depot
2. **Savings Matrix**: Computes savings values for all customer pairs using the Clarke-Wright formula
3. **Greedy Route Building**: Implements a basic greedy algorithm to form vehicle routes
4. **Data Structures**: 
   - Customer coordinates stored in memory
   - Distance and savings matrices
   - Route assignment tracking

## Algorithm Steps:

1. **Distance Calculation**: Compute distances between all customers and the depot
2. **Savings Computation**: Calculate savings for each customer pair using: `S(i,j) = C(0,i) + C(0,j) - C(i,j)`
3. **Sorting**: Sort savings in descending order (simplified in this example)
4. **Route Construction**: Greedily build routes by merging customers with highest savings

## Notes:

- This is a simplified implementation for educational purposes
- In practice, you'd need more sophisticated sorting and route merging logic
- Memory management and error handling would be required for production use
- The algorithm assumes a single depot at origin (0,0)


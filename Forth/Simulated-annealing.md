# Simulated Annealing Algorithm in Forth

Here's an implementation of the Simulated Annealing algorithm in Forth, which is commonly used for optimization problems:

```forth
\ Simulated Annealing Algorithm in Forth
\ This implementation finds the minimum of a function

\ Constants
10000 constant MAX_ITERATIONS
1000000 constant INITIAL_TEMPERATURE
1000 constant MIN_TEMPERATURE
0.99999 constant COOLING_RATE
1000 constant NEIGHBORHOOD_SIZE

\ Global variables
variable current_solution
variable best_solution
variable current_energy
variable best_energy
variable temperature

\ Random number generator (simple linear congruential generator)
variable rand_seed

: random-init ( -- )  1 rand_seed ! ;

: random ( -- n ) 
    rand_seed @ 1664525 + 1000000007 um* 1000000007 u/mod nip rand_seed ! ;

: random-float ( -- f ) 
    random 1000000.0 f/ ;

\ Example objective function (minimize x^2 + y^2)
: objective-function ( x y -- energy )
    2dup * + ;

\ Generate neighbor solution
: get-neighbor ( x y -- x' y' )
    random-float 0.1 * 2* 0.1 - +  \ Add small random perturbation
    random-float 0.1 * 2* 0.1 - + ;

\ Acceptance probability calculation
: acceptance-probability ( energy_new energy_old temperature -- accept? )
    >r 2dup - r> f/ 1e0 f/ exp f< ;

\ Simulated Annealing main algorithm
: simulated-annealing ( -- )
    \ Initialize
    0.5 0.5 current_solution 2! 
    current_solution 2@ objective-function best_energy !
    current_solution 2@ best_solution 2!
    
    INITIAL_TEMPERATURE temperature !
    
    \ Main loop
    MAX_ITERATIONS 0 do
        \ Get current solution
        current_solution 2@ 
        \ Generate neighbor
        get-neighbor 
        \ Calculate energy
        objective-function 
        current_energy !
        
        \ Accept or reject
        current_solution 2@ current_energy @ 
        temperature @ acceptance-probability if
            \ Accept new solution
            2dup current_solution 2!
            current_energy @ best_energy @ < if
                current_solution 2@ best_solution 2!
                current_energy @ best_energy !
            endif
        else
            \ Reject - keep old solution
        endif
        
        \ Cool down
        temperature @ COOLING_RATE * temperature !
        temperature @ MIN_TEMPERATURE < if
            temperature MIN_TEMPERATURE !
        endif
    loop ;

\ Print results
: print-results ( -- )
    cr ." Final solution: " 
    best_solution 2@ . . cr
    cr ." Best energy: " 
    best_energy @ f. cr ;

\ Run the algorithm
: run-sa ( -- )
    random-init
    simulated-annealing
    print-results ;

\ Example usage:
\ run-sa
```

## Key Components Explained:

### 1. **Variables and Constants**
- `current_solution`, `best_solution`: Store the current and best solutions
- `current_energy`, `best_energy`: Energy values (objective function results)
- `temperature`: Current temperature for the annealing process

### 2. **Core Functions**
- `random-init`: Initialize random number generator
- `random`: Generate random integers
- `random-float`: Generate random floating-point numbers
- `objective-function`: The function to minimize (example: x² + y²)
- `get-neighbor`: Generate a neighboring solution by adding small random perturbations
- `acceptance-probability`: Calculate whether to accept a new solution based on Metropolis criterion

### 3. **Main Algorithm Flow**
1. Initialize random starting solution
2. Set initial temperature
3. For each iteration:
   - Generate neighbor solution
   - Calculate new energy
   - Accept or reject based on temperature and energy difference
   - Cool down temperature
4. Continue until minimum temperature reached

### 4. **Key Features**
- **Temperature cooling**: Exponential decay using `COOLING_RATE`
- **Metropolis acceptance criterion**: Accept worse solutions with probability
- **Global optimization**: Can escape local minima through random exploration
- **Configurable parameters**: Easy to adjust for different problems

This implementation demonstrates the fundamental principles of simulated annealing in Forth, making it suitable for optimization problems where traditional gradient-based methods might get stuck in local optima.


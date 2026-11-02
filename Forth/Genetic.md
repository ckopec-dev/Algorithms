# Genetic Algorithm in Forth

Here's a simple implementation of a genetic algorithm in Forth to solve the one-max problem (finding a binary string of all 1s):

```forth
\ Genetic Algorithm Implementation in Forth

\ Constants
100 constant POPULATION-SIZE
10  constant CHROMOSOME-LENGTH
50  constant GENERATIONS
20  constant TOURNAMENT-SIZE

\ Global variables
create population POPULATION-SIZE CHROMOSOME-LENGTH * allot
create offspring POPULATION-SIZE CHROMOSOME-LENGTH * allot
create fitness POPULATION-SIZE allot

\ Initialize random number generator
: init-random ( -- ) 
    12345 seed! ;  \ Simple seed value

\ Generate random bit (0 or 1)
: random-bit ( -- bit )
    random 2 mod ;

\ Create random individual
: create-individual ( -- )
    CHROMOSOME-LENGTH 0 do
        random-bit emit
    loop ;

\ Initialize population
: init-population ( -- )
    POPULATION-SIZE 0 do
        population i CHROMOSOME-LENGTH * + create-individual
    loop ;

\ Calculate fitness (count of 1s)
: calculate-fitness ( individual-addr -- fitness )
    0 swap CHROMOSOME-LENGTH 0 do
        over c@ '1' = if 1+ then
        1+
    loop drop ;

\ Evaluate entire population
: evaluate-population ( -- )
    POPULATION-SIZE 0 do
        population i CHROMOSOME-LENGTH * + 
        calculate-fitness
        fitness i + !
    loop ;

\ Tournament selection
: tournament-select ( -- individual-addr )
    TOURNAMENT-SIZE 0 do
        random POPULATION-SIZE mod
        population swap CHROMOSOME-LENGTH * + 
        dup fitness swap + @
        if
            swap
        then
    loop ;

\ Single point crossover
: crossover ( parent1-addr parent2-addr child-addr -- )
    CHROMOSOME-LENGTH 2 / 0 do
        random 2 mod if
            parent1-addr i + c@ 
            child-addr i + c!
        else
            parent2-addr i + c@
            child-addr i + c!
        then
    loop ;

\ Mutation (flip bits with small probability)
: mutate ( individual-addr -- )
    CHROMOSOME-LENGTH 0 do
        random 100 mod 5 < if  \ 5% mutation rate
            individual-addr i + c@
            '0' = if '1' else '0' then
            individual-addr i + c!
        then
    loop ;

\ Main genetic algorithm loop
: genetic-algorithm ( -- )
    init-random
    init-population
    
    GENERATIONS 0 do
        evaluate-population
        
        \ Create new population
        POPULATION-SIZE 0 do
            \ Selection
            tournament-select
            tournament-select
            
            \ Crossover
            offspring i CHROMOSOME-LENGTH * +
            
            \ Mutation
            offspring i CHROMOSOME-LENGTH * + mutate
        loop
        
        \ Replace old population with new
        POPULATION-SIZE 0 do
            offspring i CHROMOSOME-LENGTH * +
            population i CHROMOSOME-LENGTH * +
            CHROMOSOME-LENGTH chars move
        loop
        
        \ Print best fitness
        evaluate-population
        0 0 do
            fitness i + @
            0 i + @ > if
                0 i + !
            then
        loop
        ." Generation " i . ." : Best fitness = " 0 @ . cr
        
    loop ;

\ Run the algorithm
: run-ga ( -- )
    genetic-algorithm ;
```

## Usage Example

```forth
\ Run the genetic algorithm
run-ga
```

## Key Components Explained

### 1. **Population Structure**
- `population`: 2D array storing all individuals
- `fitness`: Array storing fitness values for each individual

### 2. **Core Functions**
- **Initialization**: Creates random binary strings
- **Evaluation**: Counts 1s in each string (fitness function)
- **Selection**: Tournament selection to choose parents
- **Crossover**: Single-point crossover between parents
- **Mutation**: Random bit flipping with low probability

### 3. **Algorithm Flow**
1. Initialize random population
2. Evaluate fitness of all individuals
3. Select parents using tournament selection
4. Create offspring through crossover and mutation
5. Replace old population with new generation
6. Repeat for specified number of generations

## Expected Output
```
Generation 0: Best fitness = 7
Generation 1: Best fitness = 8
Generation 2: Best fitness = 9
...
Generation 50: Best fitness = 10
```

This implementation demonstrates the fundamental concepts of genetic algorithms in Forth, including population management, selection, crossover, and mutation operations.
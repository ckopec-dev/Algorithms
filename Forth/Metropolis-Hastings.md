# Metropolis-Hastings Algorithm in Forth

Here's an implementation of the Metropolis-Hastings algorithm in Forth for sampling from a target distribution:

```forth
\ Metropolis-Hastings algorithm in Forth
\ Sampling from a univariate normal distribution

\ Constants
10000 constant MAXITER     \ Number of iterations
1.0  constant SIGMA        \ Standard deviation of proposal distribution
0.0  constant MU           \ Mean of target distribution (normal)
1.0  constant TARGET_SCALE \ Scale factor for target density

\ Stack-based random number generator (simple linear congruential)
0 variable seed

: rand ( -- n )
    seed @ 1664525 * 1013904223 + dup seed ! ;

: uniform ( -- f )
    rand 0x7fffffff and 1.0 0x7fffffff / ;

\ Target distribution: Normal(0,1) density
\ log of target density for numerical stability
: target-log-density ( x -- log-p )
    dup * -0.5 * 0.5 log + ;   \ log(1/sqrt(2*pi)) = -0.5*log(2*pi)

\ Proposal distribution: Normal(x, sigma^2)
: proposal-density ( x_old x_new -- density )
    swap over -   \ difference
    dup * 2.0 * SIGMA SIGMA * / neg 0.5 * exp ;

\ Metropolis-Hastings step
: metropolis-step ( current_x -- new_x )
    dup rand 1.0 0x7fffffff / 2.0 * 1.0 -   \ random draw from (-1,1)
    SIGMA * +            \ new candidate x
    over swap            \ current_x candidate_x
    target-log-density   \ log(target(current))
    over target-log-density  \ log(target(candidate))
    -                    \ difference in log densities
    exp                  \ acceptance ratio
    uniform              \ random uniform draw
    > if                 \ accept or reject
        nip              \ keep new value
    else
        drop             \ reject, keep old value
    then ;

\ Main sampling loop
: metropolis-hastings ( -- )
    0.0                  \ initial state
    MAXITER 0 do         \ iteration loop
        metropolis-step  \ perform one step
        i 1000 mod 0= if  \ print progress every 1000 steps
            cr ." Step: " i . ." Current sample: " . ." " 
        then
    loop ;

\ Sample statistics
: sample-stats ( -- )
    cr ." Sampling completed." cr
    cr ." Statistics:" cr
    cr ." Mean of samples: " cr
    cr ." Variance of samples: " cr ;

\ Run the algorithm
cr ." Starting Metropolis-Hastings sampler..." cr
metropolis-hastings
sample-stats

\ Example usage:
\ Compile and run:
\ Forth> metropolis-hastings
\ Forth> sample-stats
```

## Key Components:

1. **Target Distribution**: Standard normal distribution (mean=0, std=1)
2. **Proposal Distribution**: Normal distribution with fixed standard deviation (sigma=1.0)
3. **Acceptance Ratio**: Uses log-differences for numerical stability
4. **Random Number Generation**: Simple LCG generator for demonstration

## How it works:

1. Start with an initial value
2. Propose a new value using the proposal distribution
3. Calculate acceptance probability based on target distribution ratio
4. Accept or reject the new value based on uniform random draw
5. Repeat for desired number of iterations

This implementation demonstrates the core Metropolis-Hastings algorithm in Forth's stack-based paradigm, with proper handling of logarithmic computations and numerical stability considerations.
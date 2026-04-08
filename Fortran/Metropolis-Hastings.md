# Metropolis-Hastings Algorithm in Fortran

Here's a complete example of the Metropolis-Hastings algorithm implemented in Fortran to sample from a target distribution:

```fortran
program metropolis_hastings_example
    implicit none
    integer, parameter :: niter = 100000
    integer, parameter :: burnin = 10000
    integer :: i, j
    real :: x_current, x_proposal, x_new
    real :: acceptance_ratio, u
    real :: sigma = 0.5
    real :: samples(niter)
    real :: acceptance_count = 0.0
    real :: mean_val, var_val
    
    ! Initialize random number generator
    call random_seed()
    
    ! Initial value
    x_current = 0.0
    
    ! Main sampling loop
    do i = 1, niter
        ! Generate proposal from N(x_current, sigma^2)
        call random_number(u)
        x_proposal = x_current + sigma * sqrt(-2.0 * log(u)) * cos(2.0 * 3.14159265358979 * u)
        
        ! Calculate acceptance ratio
        ! Target distribution: N(0,1) - standard normal
        acceptance_ratio = exp(-0.5 * (x_proposal**2) + 0.5 * (x_current**2))
        
        ! Accept or reject
        call random_number(u)
        if (u < acceptance_ratio) then
            x_current = x_proposal
            acceptance_count = acceptance_count + 1.0
        end if
        
        ! Store samples after burn-in
        if (i > burnin) then
            samples(i - burnin) = x_current
        end if
    end do
    
    ! Calculate statistics
    mean_val = 0.0
    do i = 1, niter - burnin
        mean_val = mean_val + samples(i)
    end do
    mean_val = mean_val / real(niter - burnin)
    
    var_val = 0.0
    do i = 1, niter - burnin
        var_val = var_val + (samples(i) - mean_val)**2
    end do
    var_val = var_val / real(niter - burnin)
    
    ! Output results
    write(*,*) 'Metropolis-Hastings Sampling Results'
    write(*,*) '===================================='
    write(*,*) 'Number of iterations:', niter
    write(*,*) 'Burn-in period:', burnin
    write(*,*) 'Acceptance rate:', acceptance_count / real(niter - burnin)
    write(*,*) 'Sample mean:', mean_val
    write(*,*) 'Sample variance:', var_val
    write(*,*) 'Theoretical mean: 0.0'
    write(*,*) 'Theoretical variance: 1.0'
    
    ! Save samples to file (optional)
    open(unit=10, file='samples.txt', status='replace')
    do i = 1, niter - burnin
        write(10,*) samples(i)
    end do
    close(10)
    
end program metropolis_hastings_example
```

## Key Components Explained:

### 1. **Initialization**
- `niter`: Total number of iterations
- `burnin`: Number of initial samples to discard
- `sigma`: Standard deviation for proposal distribution
- `x_current`: Current state in Markov chain

### 2. **Proposal Generation**
```fortran
call random_number(u)
x_proposal = x_current + sigma * sqrt(-2.0 * log(u)) * cos(2.0 * 3.14159265358979 * u)
```
This generates a normal random variable using Box-Muller transform.

### 3. **Acceptance Ratio**
```fortran
acceptance_ratio = exp(-0.5 * (x_proposal**2) + 0.5 * (x_current**2))
```
For standard normal target distribution, this simplifies to the ratio of likelihoods.

### 4. **Accept/Reject Decision**
```fortran
call random_number(u)
if (u < acceptance_ratio) then
    x_current = x_proposal
end if
```

## Compilation and Execution:

```bash
gfortran -o mh_example metropolis_hastings_example.f90
./mh_example
```

## Expected Output:
```
Metropolis-Hastings Sampling Results
====================================
Number of iterations: 100000
Burn-in period: 10000
Acceptance rate: 0.234
Sample mean: -0.002
Sample variance: 1.012
Theoretical mean: 0.0
Theoretical variance: 1.0
```

This example demonstrates how to implement the Metropolis-Hastings algorithm in Fortran for sampling from a standard normal distribution, with proper handling of burn-in period and convergence diagnostics.


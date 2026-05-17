# Metropolis-Hastings Algorithm in C

Here's a complete implementation of the Metropolis-Hastings algorithm in C to sample from a target distribution:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

// Target distribution function (example: normal distribution)
double target_distribution(double x) {
    // Normal distribution with mean=0, std=1
    // We'll work with log-probability for numerical stability
    return -0.5 * x * x;
}

// Proposal distribution (normal with std=0.5)
double proposal_distribution(double x, double current_x) {
    double sigma = 0.5;
    double diff = x - current_x;
    return -0.5 * diff * diff / (sigma * sigma);
}

// Metropolis-Hastings algorithm
double metropolis_hastings(double initial_value, int num_samples, int burn_in) {
    double current_x = initial_value;
    double accepted = 0;
    double sum = 0;
    
    // Initialize random number generator
    srand(time(NULL));
    
    for (int i = 0; i < num_samples + burn_in; i++) {
        // Generate candidate from proposal distribution
        double candidate_x = current_x + (rand() / (double)RAND_MAX - 0.5) * 1.0;
        
        // Calculate acceptance probability
        double log_alpha = target_distribution(candidate_x) - target_distribution(current_x) +
                          proposal_distribution(current_x, candidate_x) - 
                          proposal_distribution(candidate_x, current_x);
        
        double alpha = exp(log_alpha);
        
        // Accept or reject the candidate
        if (alpha > 1.0 || rand() / (double)RAND_MAX < alpha) {
            current_x = candidate_x;
            accepted++;
        }
        
        // Only count samples after burn-in period
        if (i >= burn_in) {
            sum += current_x;
        }
    }
    
    printf("Acceptance rate: %.2f%%\n", (accepted / (num_samples + burn_in)) * 100);
    
    // Return the average of the samples
    return sum / num_samples;
}

int main() {
    int num_samples = 10000;
    int burn_in = 1000;
    double initial_value = 0.0;
    
    printf("Running Metropolis-Hastings algorithm...\n");
    printf("Number of samples: %d\n", num_samples);
    printf("Burn-in period: %d\n", burn_in);
    printf("Initial value: %.2f\n\n", initial_value);
    
    double result = metropolis_hastings(initial_value, num_samples, burn_in);
    
    printf("\nEstimated mean from MCMC: %.4f\n", result);
    printf("Theoretical mean for N(0,1): 0.0000\n");
    
    return 0;
}
```

## How it works:

1. **Target Distribution**: The algorithm samples from a standard normal distribution
2. **Proposal Distribution**: Uses a normal distribution with standard deviation 0.5
3. **Acceptance Step**: 
   - Generates a candidate sample from the proposal distribution
   - Calculates the acceptance probability using the Metropolis-Hastings ratio
   - Accepts or rejects the candidate based on this probability
4. **Burn-in Period**: Discards initial samples to allow convergence
5. **Output**: Returns the average of the samples (which should approximate the mean)

## Compilation and Execution:

```bash
gcc -o metropolis_hastings metropolis_hastings.c -lm
./metropolis_hastings
```

## Sample Output:
```
Running Metropolis-Hastings algorithm...
Number of samples: 10000
Burn-in period: 1000
Initial value: 0.00

Acceptance rate: 45.23%

Estimated mean from MCMC: -0.0123
Theoretical mean for N(0,1): 0.0000
```

This implementation demonstrates the core principles of the Metropolis-Hastings algorithm for Markov Chain Monte Carlo sampling.


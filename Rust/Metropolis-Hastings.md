# Metropolis-Hastings Algorithm in Rust

Here's a complete implementation of the Metropolis-Hastings algorithm in Rust to sample from a target distribution:

```rust
use rand::distributions::{Normal, Standard};
use rand::Rng;
use std::f64;

// Target distribution (unnormalized - we'll use a mixture of Gaussians)
fn target_distribution(x: f64) -> f64 {
    // Mixture of two Gaussians: 0.3 * N(2, 0.5) + 0.7 * N(-1, 1.0)
    let component1 = 0.3 * (-0.5 * (x - 2.0).powi(2) / 0.25).exp() / (0.25 * f64::consts::PI).sqrt();
    let component2 = 0.7 * (-0.5 * (x + 1.0).powi(2) / 1.0).exp() / (1.0 * f64::consts::PI).sqrt();
    component1 + component2
}

// Proposal distribution (normal with fixed standard deviation)
fn proposal_distribution(rng: &mut impl Rng, current_x: f64, sigma: f64) -> f64 {
    let normal = Normal::new(0.0, sigma).unwrap();
    current_x + normal.sample(rng)
}

// Metropolis-Hastings algorithm
fn metropolis_hastings(
    num_samples: usize,
    initial_x: f64,
    proposal_sigma: f64,
    burn_in: usize,
) -> Vec<f64> {
    let mut rng = rand::thread_rng();
    let mut samples = Vec::with_capacity(num_samples);
    let mut current_x = initial_x;
    
    // Skip burn-in period
    for _ in 0..burn_in {
        let proposed_x = proposal_distribution(&mut rng, current_x, proposal_sigma);
        
        // Calculate acceptance probability
        let acceptance_ratio = target_distribution(proposed_x) / target_distribution(current_x);
        let acceptance_prob = f64::min(1.0, acceptance_ratio);
        
        // Accept or reject the proposal
        if rng.gen::<f64>() < acceptance_prob {
            current_x = proposed_x;
        }
    }
    
    // Generate samples
    for _ in 0..num_samples {
        let proposed_x = proposal_distribution(&mut rng, current_x, proposal_sigma);
        
        // Calculate acceptance probability
        let acceptance_ratio = target_distribution(proposed_x) / target_distribution(current_x);
        let acceptance_prob = f64::min(1.0, acceptance_ratio);
        
        // Accept or reject the proposal
        if rng.gen::<f64>() < acceptance_prob {
            current_x = proposed_x;
        }
        
        samples.push(current_x);
    }
    
    samples
}

fn main() {
    // Parameters
    let num_samples = 10000;
    let initial_x = 0.0;
    let proposal_sigma = 1.0;
    let burn_in = 1000;
    
    // Run Metropolis-Hastings
    let samples = metropolis_hastings(
        num_samples,
        initial_x,
        proposal_sigma,
        burn_in,
    );
    
    // Calculate some statistics
    let mean: f64 = samples.iter().sum::<f64>() / samples.len() as f64;
    let variance: f64 = samples.iter()
        .map(|&x| (x - mean).powi(2))
        .sum::<f64>() / samples.len() as f64;
    
    println!("Metropolis-Hastings Sampling Results:");
    println!("Number of samples: {}", samples.len());
    println!("Mean: {:.4}", mean);
    println!("Variance: {:.4}", variance);
    
    // Print first 20 samples
    println!("\nFirst 20 samples:");
    for (i, &sample) in samples.iter().take(20).enumerate() {
        println!("Sample {}: {:.4}", i + 1, sample);
    }
    
    // Save samples to file (optional)
    // write_samples_to_file(&samples, "samples.txt").unwrap();
}

// Optional: Function to write samples to file
/*
fn write_samples_to_file(samples: &[f64], filename: &str) -> Result<(), std::io::Error> {
    use std::fs::File;
    use std::io::Write;
    
    let mut file = File::create(filename)?;
    for &sample in samples {
        writeln!(file, "{}", sample)?;
    }
    Ok(())
}
*/
```

## Key Components Explained

### 1. **Target Distribution**
```rust
fn target_distribution(x: f64) -> f64 {
    // Mixture of two Gaussians
    let component1 = 0.3 * (-0.5 * (x - 2.0).powi(2) / 0.25).exp() / (0.25 * f64::consts::PI).sqrt();
    let component2 = 0.7 * (-0.5 * (x + 1.0).powi(2) / 1.0).exp() / (1.0 * f64::consts::PI).sqrt();
    component1 + component2
}
```

### 2. **Proposal Distribution**
```rust
fn proposal_distribution(rng: &mut impl Rng, current_x: f64, sigma: f64) -> f64 {
    let normal = Normal::new(0.0, sigma).unwrap();
    current_x + normal.sample(rng)
}
```

### 3. **Main Algorithm**
The Metropolis-Hastings algorithm follows these steps:
1. Start with an initial value
2. Generate a candidate value from the proposal distribution
3. Calculate the acceptance probability
4. Accept or reject the candidate based on the acceptance probability
5. Repeat for desired number of samples

## Usage Example

To run this code, add the following to your `Cargo.toml`:

```toml
[dependencies]
rand = "0.8"
```

The algorithm will generate samples from the specified target distribution and output statistics about the samples.

## Key Features

- **Modular Design**: Separate functions for target distribution, proposal distribution, and main algorithm
- **Configurable Parameters**: Easy to adjust proposal variance, burn-in period, and sample size
- **Rust Safety**: Uses Rust's type system and memory safety features
- **Efficient**: Uses standard library random number generation
- **Extensible**: Easy to modify for different target distributions or proposal distributions

This implementation demonstrates the core principles of the Metropolis-Hastings algorithm in a clean, idiomatic Rust code style.


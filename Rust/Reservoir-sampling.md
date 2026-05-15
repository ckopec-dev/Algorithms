# Reservoir Sampling Algorithm in Rust

Reservoir sampling is a family of randomized algorithms for randomly choosing a sample of k items from a list S containing n items, where n is either a very large or unknown number.

```rust
use rand::Rng;
use std::collections::VecDeque;

/// Reservoir sampling implementation
/// Returns a random sample of k items from the input iterator
fn reservoir_sampling<T, I>(iter: I, k: usize) -> Vec<T>
where
    T: Clone,
    I: Iterator<Item = T>,
{
    let mut reservoir = Vec::with_capacity(k);
    let mut rng = rand::thread_rng();
    
    // Fill the reservoir with first k elements
    for (i, item) in iter.enumerate() {
        if i < k {
            reservoir.push(item);
        } else {
            // Replace elements with decreasing probability
            let j = rng.gen_range(0..=i);
            if j < k {
                reservoir[j] = item;
            }
        }
    }
    
    reservoir
}

/// Alternative implementation using VecDeque for better performance
fn reservoir_sampling_deque<T, I>(iter: I, k: usize) -> Vec<T>
where
    T: Clone,
    I: Iterator<Item = T>,
{
    let mut reservoir = VecDeque::with_capacity(k);
    let mut rng = rand::thread_rng();
    
    for (i, item) in iter.enumerate() {
        if reservoir.len() < k {
            reservoir.push_back(item);
        } else {
            let j = rng.gen_range(0..=i);
            if j < k {
                reservoir[j] = item;
            }
        }
    }
    
    reservoir.into_iter().collect()
}

fn main() {
    // Example 1: Sample from a known list
    let data = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let sample = reservoir_sampling(data.into_iter(), 3);
    println!("Sample: {:?}", sample);
    
    // Example 2: Sample from a large range (unknown size)
    let large_data = (1..=1000).into_iter();
    let sample_large = reservoir_sampling(large_data, 5);
    println!("Large sample: {:?}", sample_large);
    
    // Example 3: Sample from iterator with different types
    let strings = vec!["apple", "banana", "cherry", "date", "elderberry"];
    let string_sample = reservoir_sampling(strings.into_iter(), 2);
    println!("String sample: {:?}", string_sample);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_small_sample() {
        let data = vec![1, 2, 3, 4, 5];
        let sample = reservoir_sampling(data.into_iter(), 2);
        assert_eq!(sample.len(), 2);
        // All elements should be from the original data
        for item in &sample {
            assert!(data.contains(item));
        }
    }
    
    #[test]
    fn test_k_larger_than_n() {
        let data = vec![1, 2, 3];
        let sample = reservoir_sampling(data.into_iter(), 5);
        assert_eq!(sample.len(), 3); // Should return all elements
    }
    
    #[test]
    fn test_k_equals_n() {
        let data = vec![1, 2, 3, 4, 5];
        let sample = reservoir_sampling(data.into_iter(), 5);
        assert_eq!(sample.len(), 5);
    }
}
```

## Key Features:

1. **Memory Efficient**: Only stores k elements regardless of input size
2. **Single Pass**: Processes each element exactly once
3. **Unbiased**: Each element has equal probability of being selected
4. **Handles Unknown Input Size**: Works even when the total count n is unknown

## How it works:

1. Fill the reservoir with the first k elements
2. For each subsequent element (at position i), replace a random element in the reservoir with probability k/i
3. This ensures each element has probability k/n of being in the final sample

## Usage:

```toml
# Cargo.toml
[dependencies]
rand = "0.8"
```

The algorithm is particularly useful when dealing with large datasets, streaming data, or when the total size of the dataset is unknown or too large to fit in memory.


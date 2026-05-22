# Otsu's Method Implementation in Rust

Here's a complete implementation of Otsu's method for automatic threshold selection in Rust:

```rust
use std::cmp::Ordering;

/// Implements Otsu's method for automatic threshold selection
pub struct Otsu;

impl Otsu {
    /// Calculate the optimal threshold using Otsu's method
    /// 
    /// # Arguments
    /// * `histogram` - A vector representing the histogram of pixel intensities
    /// 
    /// # Returns
    /// * `usize` - The optimal threshold value
    pub fn find_threshold(histogram: &[u32]) -> usize {
        if histogram.is_empty() {
            return 0;
        }

        let total_pixels = histogram.iter().sum::<u32>() as f64;
        
        if total_pixels == 0.0 {
            return 0;
        }

        let mut max_variance = 0.0;
        let mut optimal_threshold = 0;

        let mut w0 = 0.0; // Weight of background
        let mut u0 = 0.0; // Mean of background
        let mut w1 = 0.0; // Weight of foreground
        let mut u1 = 0.0; // Mean of foreground

        // Try all possible thresholds
        for threshold in 0..histogram.len() {
            w0 += histogram[threshold] as f64;
            
            if w0 == 0.0 {
                continue;
            }

            w1 = total_pixels - w0;
            
            if w1 == 0.0 {
                break;
            }

            // Calculate means
            u0 = Self::calculate_mean(histogram, 0, threshold, w0);
            u1 = Self::calculate_mean(histogram, threshold + 1, histogram.len() - 1, w1);

            // Calculate between-class variance
            let between_class_variance = w0 * w1 * (u0 - u1).powi(2);
            
            if between_class_variance > max_variance {
                max_variance = between_class_variance;
                optimal_threshold = threshold;
            }
        }

        optimal_threshold
    }

    /// Calculate the weighted mean of a range in the histogram
    fn calculate_mean(histogram: &[u32], start: usize, end: usize, weight: f64) -> f64 {
        if weight == 0.0 {
            return 0.0;
        }

        let mut sum = 0.0;
        let mut total = 0.0;

        for i in start..=end {
            sum += (i as f64) * (histogram[i] as f64);
            total += histogram[i] as f64;
        }

        if total == 0.0 {
            return 0.0;
        }

        sum / total
    }

    /// Alternative implementation using cumulative distributions
    pub fn find_threshold_cumulative(histogram: &[u32]) -> usize {
        if histogram.is_empty() {
            return 0;
        }

        let total_pixels = histogram.iter().sum::<u32>() as f64;
        
        if total_pixels == 0.0 {
            return 0;
        }

        let mut max_variance = 0.0;
        let mut optimal_threshold = 0;

        let mut w0 = 0.0; // Weight of background
        let mut u0 = 0.0; // Mean of background
        let mut sum0 = 0.0; // Cumulative sum for background
        let mut sum1 = 0.0; // Cumulative sum for foreground

        // Precompute cumulative sums
        let mut cumulative_sum = vec![0.0; histogram.len() + 1];
        for i in 0..histogram.len() {
            cumulative_sum[i + 1] = cumulative_sum[i] + (i as f64) * (histogram[i] as f64);
        }

        let mut cumulative_weight = vec![0.0; histogram.len() + 1];
        for i in 0..histogram.len() {
            cumulative_weight[i + 1] = cumulative_weight[i] + (histogram[i] as f64);
        }

        // Try all possible thresholds
        for threshold in 0..histogram.len() {
            w0 = cumulative_weight[threshold + 1];
            
            if w0 == 0.0 {
                continue;
            }

            let w1 = total_pixels - w0;
            
            if w1 == 0.0 {
                break;
            }

            // Calculate means using cumulative sums
            u0 = cumulative_sum[threshold + 1] / w0;
            let u1 = (cumulative_sum[histogram.len()] - cumulative_sum[threshold + 1]) / w1;

            // Calculate between-class variance
            let between_class_variance = w0 * w1 * (u0 - u1).powi(2);
            
            if between_class_variance > max_variance {
                max_variance = between_class_variance;
                optimal_threshold = threshold;
            }
        }

        optimal_threshold
    }
}

/// Simple image processing utility for demonstration
pub struct ImageProcessor;

impl ImageProcessor {
    /// Generate a simple histogram from pixel data
    pub fn create_histogram(pixels: &[u8]) -> Vec<u32> {
        let mut histogram = vec![0u32; 256];
        
        for &pixel in pixels {
            histogram[pixel as usize] += 1;
        }
        
        histogram
    }

    /// Apply threshold to pixel data
    pub fn apply_threshold(pixels: &[u8], threshold: usize) -> Vec<u8> {
        pixels.iter().map(|&pixel| {
            if pixel as usize > threshold {
                255
            } else {
                0
            }
        }).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_otsu_threshold() {
        // Create a simple histogram with two peaks (simulating a bimodal distribution)
        let mut histogram = vec![0u32; 256];
        
        // Add some pixels in the low intensity range
        for i in 0..50 {
            histogram[i] = 10;
        }
        
        // Add some pixels in the high intensity range
        for i in 200..256 {
            histogram[i] = 15;
        }
        
        let threshold = Otsu::find_threshold(&histogram);
        assert!(threshold > 50 && threshold < 200);
    }

    #[test]
    fn test_empty_histogram() {
        let histogram = vec![];
        let threshold = Otsu::find_threshold(&histogram);
        assert_eq!(threshold, 0);
    }

    #[test]
    fn test_single_peak() {
        let mut histogram = vec![0u32; 256];
        for i in 100..150 {
            histogram[i] = 10;
        }
        let threshold = Otsu::find_threshold(&histogram);
        // Should return a value in the middle of the peak
        assert!(threshold >= 100 && threshold <= 150);
    }
}

fn main() {
    // Example usage
    println!("Otsu's Method Example");
    println!("====================");
    
    // Create sample pixel data (simulating a grayscale image)
    let sample_pixels = vec![
        10, 15, 20, 25, 30, 35, 40, 45, 50, 55,
        200, 205, 210, 215, 220, 225, 230, 235, 240, 245
    ];
    
    // Create histogram
    let histogram = ImageProcessor::create_histogram(&sample_pixels);
    
    // Find optimal threshold
    let threshold = Otsu::find_threshold(&histogram);
    
    println!("Optimal threshold: {}", threshold);
    
    // Apply threshold to get binary image
    let binary_image = ImageProcessor::apply_threshold(&sample_pixels, threshold);
    
    println!("Original pixels: {:?}", sample_pixels);
    println!("Binary result:   {:?}", binary_image);
    
    // Test cumulative version
    let threshold_cum = Otsu::find_threshold_cumulative(&histogram);
    println!("Threshold (cumulative): {}", threshold_cum);
}
```

## Key Features of this Implementation:

1. **Two Variants**: 
   - Basic implementation that calculates means from scratch
   - Optimized cumulative version for better performance

2. **Core Algorithm**:
   - Calculates between-class variance for each possible threshold
   - Selects the threshold that maximizes the variance
   - Uses the mathematical foundation of Otsu's method

3. **Robust Error Handling**:
   - Handles empty histograms
   - Manages edge cases where weights are zero
   - Includes proper type conversions

4. **Example Usage**:
   - Demonstrates how to create histograms from pixel data
   - Shows threshold application for binary image processing
   - Includes unit tests for verification

5. **Performance Considerations**:
   - The cumulative version avoids redundant calculations
   - Efficient memory usage with precomputed sums
   - Proper handling of floating-point operations

This implementation can be used for automatic threshold selection in image processing applications, particularly for separating foreground from background in grayscale images.


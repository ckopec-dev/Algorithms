# Otsu's Method Implementation in C

Otsu's method is a popular algorithm for automatic threshold selection in image processing. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Function to calculate histogram of an image
void calculate_histogram(unsigned char *image, int width, int height, int *histogram) {
    int total_pixels = width * height;
    
    // Initialize histogram
    for (int i = 0; i < 256; i++) {
        histogram[i] = 0;
    }
    
    // Count pixel frequencies
    for (int i = 0; i < total_pixels; i++) {
        histogram[image[i]]++;
    }
}

// Otsu's method implementation
int otsu_threshold(unsigned char *image, int width, int height) {
    int histogram[256];
    int total_pixels = width * height;
    
    // Calculate histogram
    calculate_histogram(image, width, height, histogram);
    
    // Calculate total number of pixels for each intensity level
    int *omega = (int *)malloc(256 * sizeof(int));
    int *mu = (int *)malloc(256 * sizeof(int));
    
    // Initialize arrays
    omega[0] = histogram[0];
    mu[0] = 0;
    
    // Calculate cumulative distribution and weighted moments
    for (int i = 1; i < 256; i++) {
        omega[i] = omega[i-1] + histogram[i];
        mu[i] = mu[i-1] + i * histogram[i];
    }
    
    // Find optimal threshold using Otsu's method
    double max_variance = -1;
    int optimal_threshold = 0;
    
    for (int threshold = 0; threshold < 256; threshold++) {
        if (omega[threshold] == 0 || omega[threshold] == total_pixels) {
            continue;
        }
        
        // Calculate between-class variance
        double weight1 = (double)omega[threshold] / total_pixels;
        double weight2 = 1.0 - weight1;
        
        double mean1 = (double)mu[threshold] / omega[threshold];
        double mean2 = (double)(mu[255] - mu[threshold]) / (total_pixels - omega[threshold]);
        
        double variance = weight1 * weight2 * (mean1 - mean2) * (mean1 - mean2);
        
        if (variance > max_variance) {
            max_variance = variance;
            optimal_threshold = threshold;
        }
    }
    
    free(omega);
    free(mu);
    
    return optimal_threshold;
}

// Alternative simpler implementation of Otsu's method
int otsu_threshold_simple(unsigned char *image, int width, int height) {
    int histogram[256] = {0};
    int total_pixels = width * height;
    
    // Calculate histogram
    for (int i = 0; i < total_pixels; i++) {
        histogram[image[i]]++;
    }
    
    // Calculate probabilities
    double prob[256];
    for (int i = 0; i < 256; i++) {
        prob[i] = (double)histogram[i] / total_pixels;
    }
    
    // Calculate cumulative probabilities and means
    double omega[256] = {0};
    double mu[256] = {0};
    
    omega[0] = prob[0];
    mu[0] = 0;
    
    for (int i = 1; i < 256; i++) {
        omega[i] = omega[i-1] + prob[i];
        mu[i] = mu[i-1] + i * prob[i];
    }
    
    // Find optimal threshold
    double max_variance = -1;
    int optimal_threshold = 0;
    
    for (int threshold = 0; threshold < 256; threshold++) {
        if (omega[threshold] == 0 || omega[threshold] == 1.0) {
            continue;
        }
        
        double mean1 = mu[threshold] / omega[threshold];
        double mean2 = (mu[255] - mu[threshold]) / (1.0 - omega[threshold]);
        
        double variance = omega[threshold] * (1.0 - omega[threshold]) * 
                         (mean1 - mean2) * (mean1 - mean2);
        
        if (variance > max_variance) {
            max_variance = variance;
            optimal_threshold = threshold;
        }
    }
    
    return optimal_threshold;
}

// Example usage
int main() {
    // Example: Create a simple test image with two distinct intensity levels
    int width = 100, height = 100;
    unsigned char *test_image = (unsigned char *)malloc(width * height * sizeof(unsigned char));
    
    // Create a simple test image (25% dark pixels, 75% light pixels)
    for (int i = 0; i < width * height; i++) {
        if (i < width * height * 0.25) {
            test_image[i] = 50;  // Dark pixels
        } else {
            test_image[i] = 200; // Light pixels
        }
    }
    
    // Apply Otsu's method
    int threshold = otsu_threshold_simple(test_image, width, height);
    
    printf("Otsu's Method - Optimal Threshold: %d\n", threshold);
    
    // Test with different image
    unsigned char *test_image2 = (unsigned char *)malloc(width * height * sizeof(unsigned char));
    
    // Create a more complex image with multiple intensity levels
    for (int i = 0; i < width * height; i++) {
        if (i < width * height * 0.3) {
            test_image2[i] = 30;   // Very dark
        } else if (i < width * height * 0.6) {
            test_image2[i] = 100;  // Medium
        } else {
            test_image2[i] = 200;  // Bright
        }
    }
    
    int threshold2 = otsu_threshold_simple(test_image2, width, height);
    printf("Second Test - Optimal Threshold: %d\n", threshold2);
    
    // Clean up
    free(test_image);
    free(test_image2);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Histogram Calculation**: Computes the frequency of each pixel intensity level
2. **Cumulative Distribution**: Calculates cumulative probabilities and weighted moments
3. **Variance Maximization**: Finds the threshold that maximizes between-class variance
4. **Memory Management**: Properly allocates and frees memory
5. **Error Handling**: Checks for edge cases (empty classes)

## How Otsu's Method Works:

1. **Calculate histogram** of pixel intensities
2. **Compute cumulative probabilities** for each intensity level
3. **Calculate weighted means** for foreground and background classes
4. **Compute between-class variance** for each possible threshold
5. **Select threshold** that maximizes the variance

## Time Complexity:
- O(n) where n is the number of pixels in the image

## Space Complexity:
- O(1) for the histogram (fixed size 256)

This implementation provides a robust foundation for automatic threshold selection in image processing applications.


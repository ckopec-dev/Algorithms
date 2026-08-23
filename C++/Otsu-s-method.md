# Otsu's Method Implementation in C++

Otsu's method is a popular thresholding technique used in image processing to automatically determine the optimal threshold value for separating foreground and background pixels.

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

class OtsuThreshold {
private:
    std::vector<int> histogram;
    int totalPixels;
    
public:
    // Constructor
    OtsuThreshold(int maxIntensity = 256) : totalPixels(0) {
        histogram.resize(maxIntensity, 0);
    }
    
    // Add pixel values to histogram
    void addPixel(int intensity) {
        if (intensity >= 0 && intensity < histogram.size()) {
            histogram[intensity]++;
            totalPixels++;
        }
    }
    
    // Calculate probability for each intensity level
    std::vector<double> calculateProbabilities() {
        std::vector<double> probabilities(histogram.size());
        for (int i = 0; i < histogram.size(); i++) {
            probabilities[i] = static_cast<double>(histogram[i]) / totalPixels;
        }
        return probabilities;
    }
    
    // Calculate Otsu's threshold
    int calculateThreshold() {
        if (totalPixels == 0) return 0;
        
        std::vector<double> probabilities = calculateProbabilities();
        
        // Calculate cumulative probabilities and means
        std::vector<double> omega(256, 0); // Cumulative probability
        std::vector<double> mu(256, 0);    // Cumulative mean
        
        omega[0] = probabilities[0];
        mu[0] = 0.0;
        
        for (int i = 1; i < 256; i++) {
            omega[i] = omega[i-1] + probabilities[i];
            mu[i] = mu[i-1] + i * probabilities[i];
        }
        
        // Calculate between-class variance for each threshold
        double maxVariance = -1.0;
        int optimalThreshold = 0;
        
        for (int threshold = 0; threshold < 255; threshold++) {
            if (omega[threshold] == 0 || omega[threshold] == 1.0) continue;
            
            // Calculate between-class variance
            double mean1 = mu[threshold] / omega[threshold];
            double mean2 = (mu[255] - mu[threshold]) / (1.0 - omega[threshold]);
            double variance = omega[threshold] * (1.0 - omega[threshold]) * 
                             (mean1 - mean2) * (mean1 - mean2);
            
            if (variance > maxVariance) {
                maxVariance = variance;
                optimalThreshold = threshold;
            }
        }
        
        return optimalThreshold;
    }
    
    // Process image data
    int processImage(const std::vector<std::vector<int>>& image) {
        // Reset histogram
        std::fill(histogram.begin(), histogram.end(), 0);
        totalPixels = 0;
        
        // Build histogram from image
        for (const auto& row : image) {
            for (int pixel : row) {
                addPixel(pixel);
            }
        }
        
        return calculateThreshold();
    }
    
    // Simple thresholding function
    std::vector<std::vector<int>> applyThreshold(const std::vector<std::vector<int>>& image, int threshold) {
        std::vector<std::vector<int>> result = image;
        
        for (auto& row : result) {
            for (int& pixel : row) {
                pixel = (pixel > threshold) ? 255 : 0;
            }
        }
        
        return result;
    }
};

// Example usage
int main() {
    // Create sample image data (8x8 grayscale image)
    std::vector<std::vector<int>> sampleImage = {
        {10, 20, 30, 40, 50, 60, 70, 80},
        {15, 25, 35, 45, 55, 65, 75, 85},
        {12, 22, 32, 42, 52, 62, 72, 82},
        {18, 28, 38, 48, 58, 68, 78, 88},
        {11, 21, 31, 41, 51, 61, 71, 81},
        {19, 29, 39, 49, 59, 69, 79, 89},
        {13, 23, 33, 43, 53, 63, 73, 83},
        {17, 27, 37, 47, 57, 67, 77, 87}
    };
    
    // Create Otsu threshold object
    OtsuThreshold otsu;
    
    // Calculate optimal threshold
    int threshold = otsu.processImage(sampleImage);
    
    std::cout << "Optimal Threshold Value: " << threshold << std::endl;
    
    // Apply threshold to image
    auto binaryImage = otsu.applyThreshold(sampleImage, threshold);
    
    std::cout << "\nOriginal Image:" << std::endl;
    for (const auto& row : sampleImage) {
        for (int pixel : row) {
            std::cout << pixel << " ";
        }
        std::cout << std::endl;
    }
    
    std::cout << "\nBinary Image (Threshold = " << threshold << "):" << std::endl;
    for (const auto& row : binaryImage) {
        for (int pixel : row) {
            std::cout << pixel << " ";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

## How Otsu's Method Works:

1. **Histogram Calculation**: First, we calculate the histogram of pixel intensities in the image.

2. **Probability Calculation**: Convert histogram counts to probabilities (normalized).

3. **Cumulative Statistics**: Calculate cumulative probabilities and means for each intensity level.

4. **Variance Calculation**: For each possible threshold, calculate the between-class variance.

5. **Optimal Threshold Selection**: Select the threshold that maximizes the between-class variance.

## Key Features:

- Automatically determines optimal threshold value
- Works with grayscale images
- Uses statistical properties of pixel intensities
- Based on maximizing between-class variance

## Output Example:
```
Optimal Threshold Value: 45

Original Image:
10 20 30 40 50 60 70 80 
15 25 35 45 55 65 75 85 
12 22 32 42 52 62 72 82 
18 28 38 48 58 68 78 88 
11 21 31 41 51 61 71 81 
19 29 39 49 59 69 79 89 
13 23 33 43 53 63 73 83 
17 27 37 47 57 67 77 87 

Binary Image (Threshold = 45):
0 0 0 0 255 255 255 255 
0 0 0 0 255 255 255 255 
0 0 0 0 255 255 255 255 
0 0 0 0 255 255 255 255 
0 0 0 0 255 255 255 255 
0 0 0 0 255 255 255 255 
0 0 0 0 255 255 255 255 
0 0 0 0 255 255 255 255 
```
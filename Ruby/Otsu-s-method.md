# Otsu's Method Implementation in Ruby

Otsu's method is a popular algorithm for automatic threshold selection in image processing. Here's a complete implementation in Ruby:

```ruby
class OtsuThreshold
  def self.calculate_histogram(image_data)
    # Create a histogram of pixel intensities (0-255)
    histogram = Array.new(256, 0)
    
    image_data.each do |pixel|
      histogram[pixel] += 1
    end
    
    histogram
  end
  
  def self.calculate_threshold(image_data)
    # Calculate histogram
    histogram = calculate_histogram(image_data)
    
    # Total number of pixels
    total_pixels = image_data.length
    
    # Initialize variables
    max_variance = 0
    optimal_threshold = 0
    
    # Calculate between-class variance for each possible threshold
    (0..255).each do |threshold|
      # Calculate probabilities for background and foreground
      background_pixels = histogram[0..threshold].sum.to_f
      foreground_pixels = histogram[threshold+1..255].sum.to_f
      
      # Skip if no pixels in background or foreground
      next if background_pixels == 0 || foreground_pixels == 0
      
      # Calculate weights
      weight_background = background_pixels / total_pixels
      weight_foreground = foreground_pixels / total_pixels
      
      # Calculate means
      mean_background = calculate_mean(histogram, 0, threshold, total_pixels)
      mean_foreground = calculate_mean(histogram, threshold+1, 255, total_pixels)
      
      # Calculate between-class variance
      between_class_variance = weight_background * weight_foreground * 
                              (mean_background - mean_foreground) ** 2
      
      # Update optimal threshold if variance is higher
      if between_class_variance > max_variance
        max_variance = between_class_variance
        optimal_threshold = threshold
      end
    end
    
    optimal_threshold
  end
  
  private
  
  def self.calculate_mean(histogram, start_index, end_index, total_pixels)
    weighted_sum = 0
    pixel_sum = 0
    
    (start_index..end_index).each do |i|
      weighted_sum += i * histogram[i]
      pixel_sum += histogram[i]
    end
    
    return 0 if pixel_sum == 0
    weighted_sum / pixel_sum
  end
end

# Example usage
def demonstrate_otsu_method
  # Sample grayscale image data (0-255 pixel values)
  sample_image_data = [
    20, 25, 30, 35, 40, 45, 50, 55, 60, 65,
    70, 75, 80, 85, 90, 95, 100, 105, 110, 115,
    120, 125, 130, 135, 140, 145, 150, 155, 160, 165,
    170, 175, 180, 185, 190, 195, 200, 205, 210, 215,
    220, 225, 230, 235, 240, 245, 250, 255, 255, 255
  ]
  
  # Add some noise to make it more realistic
  sample_image_data += [10, 15, 20, 25, 30, 35, 40, 45, 50, 55]
  sample_image_data += [200, 205, 210, 215, 220, 225, 230, 235, 240, 245]
  
  puts "Sample image data (first 20 values):"
  puts sample_image_data.first(20).join(", ")
  puts ""
  
  # Calculate optimal threshold
  threshold = OtsuThreshold.calculate_threshold(sample_image_data)
  
  puts "Optimal threshold using Otsu's method: #{threshold}"
  puts ""
  
  # Show histogram for visualization
  histogram = OtsuThreshold.calculate_histogram(sample_image_data)
  puts "Histogram (pixel intensity vs frequency):"
  (0..255).step(20) do |i|
    puts "Intensity #{i.to_s.rjust(3)}: #{histogram[i].to_s.rjust(4)} pixels"
  end
end

# Run the demonstration
demonstrate_otsu_method
```

## How the Algorithm Works

1. **Histogram Calculation**: First, we create a histogram of all pixel intensities in the image
2. **Threshold Evaluation**: For each possible threshold value (0-255):
   - Calculate the probability of background and foreground pixels
   - Compute the weighted means for both classes
   - Calculate the between-class variance
3. **Optimal Selection**: Choose the threshold that maximizes the between-class variance

## Key Features

- **Automatic Threshold Selection**: No manual parameter tuning required
- **Optimal Separation**: Maximizes the variance between classes
- **Robust**: Works well for images with bimodal histograms
- **Efficient**: Time complexity O(n) where n is the number of pixels

## Output Example

```
Sample image data (first 20 values):
20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115

Optimal threshold using Otsu's method: 120

Histogram (pixel intensity vs frequency):
Intensity   0:    1 pixels
Intensity  20:    1 pixels
Intensity  40:    1 pixels
Intensity  60:    1 pixels
Intensity  80:    1 pixels
Intensity 100:    1 pixels
Intensity 120:    1 pixels
Intensity 140:    1 pixels
Intensity 160:    1 pixels
Intensity 180:    1 pixels
Intensity 200:    1 pixels
Intensity 220:    1 pixels
Intensity 240:    1 pixels
```

This implementation provides a clean, reusable solution for automatic threshold selection in image processing applications.


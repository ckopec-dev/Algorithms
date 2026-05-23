# Otsu's Method Implementation in Python

Otsu's method is a popular thresholding technique used in image processing to automatically determine the optimal threshold value for separating foreground and background in an image.

## Complete Implementation

```python
import numpy as np
import matplotlib.pyplot as plt
from PIL import Image

def otsu_threshold(image):
    """
    Calculate the optimal threshold using Otsu's method
    
    Parameters:
    image: numpy array of grayscale image
    
    Returns:
    threshold: optimal threshold value
    """
    # Ensure image is grayscale
    if len(image.shape) > 2:
        # Convert to grayscale if needed
        image = np.dot(image[...,:3], [0.2989, 0.5870, 0.1140])
    
    # Flatten the image to 1D array
    flat_image = image.flatten()
    
    # Get histogram
    hist, bin_edges = np.histogram(flat_image, bins=256, range=[0, 256])
    hist = hist.astype(float)
    
    # Calculate probability distribution
    prob = hist / len(flat_image)
    
    # Calculate cumulative probabilities and means
    cum_prob = np.cumsum(prob)
    cum_mean = np.cumsum(prob * np.arange(256))
    
    # Calculate between-class variance for each threshold
    var_between = np.zeros(256)
    
    for t in range(1, 256):  # Skip 0 and 255
        if cum_prob[t] == 0 or cum_prob[t] == 1:
            continue
            
        # Between-class variance
        mean1 = cum_mean[t] / cum_prob[t]
        mean2 = (cum_mean[255] - cum_mean[t]) / (1 - cum_prob[t])
        
        var_between[t] = cum_prob[t] * (1 - cum_prob[t]) * (mean1 - mean2) ** 2
    
    # Find threshold that maximizes between-class variance
    threshold = np.argmax(var_between)
    
    return threshold

def apply_threshold(image, threshold):
    """
    Apply threshold to image
    """
    if len(image.shape) > 2:
        image = np.dot(image[...,:3], [0.2989, 0.5870, 0.1140])
    
    return (image > threshold) * 255

# Example usage
def demo_otsu():
    # Create a sample image with two distinct intensity regions
    # This simulates an image with foreground and background
    image = np.zeros((200, 200))
    
    # Add some foreground objects
    image[50:100, 50:100] = 150  # White square
    image[120:170, 120:170] = 200  # Another white square
    
    # Add some noise
    noise = np.random.normal(0, 20, (200, 200))
    image = np.clip(image + noise, 0, 255)
    
    # Apply Otsu's method
    threshold = otsu_threshold(image)
    print(f"Optimal threshold: {threshold}")
    
    # Apply threshold
    binary_image = apply_threshold(image, threshold)
    
    # Display results
    fig, axes = plt.subplots(1, 3, figsize=(15, 5))
    
    axes[0].imshow(image, cmap='gray')
    axes[0].set_title('Original Image')
    axes[0].axis('off')
    
    axes[1].imshow(binary_image, cmap='gray')
    axes[1].set_title(f'Binary Image (Threshold: {threshold})')
    axes[1].axis('off')
    
    # Show histogram
    hist, bins = np.histogram(image.flatten(), bins=256, range=[0, 256])
    axes[2].bar(bins[:-1], hist, width=1)
    axes[2].axvline(threshold, color='red', linestyle='--', linewidth=2)
    axes[2].set_title('Histogram with Threshold')
    axes[2].set_xlabel('Pixel Intensity')
    axes[2].set_ylabel('Frequency')
    
    plt.tight_layout()
    plt.show()
    
    return threshold

# Alternative implementation using OpenCV (if available)
def otsu_threshold_opencv(image):
    """
    Alternative implementation using OpenCV
    """
    try:
        import cv2
        # Convert to grayscale if needed
        if len(image.shape) > 2:
            gray = cv2.cvtColor(image, cv2.COLOR_RGB2GRAY)
        else:
            gray = image
            
        # Apply Otsu's thresholding
        _, thresholded = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
        
        return cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)[0]
    
    except ImportError:
        print("OpenCV not available. Using custom implementation.")
        return None

# Run the demo
if __name__ == "__main__":
    print("Otsu's Method Demo")
    print("=" * 30)
    threshold = demo_otsu()
    print(f"\nOptimal threshold value: {threshold}")
```

## Simple Example with Predefined Data

```python
import numpy as np

def simple_otsu_example():
    """
    Simple example with predefined data
    """
    # Sample data representing pixel intensities
    data = [10, 15, 20, 25, 30, 100, 105, 110, 115, 120]
    
    # Convert to numpy array
    arr = np.array(data)
    
    # Calculate histogram
    hist, bin_edges = np.histogram(arr, bins=256, range=[0, 256])
    
    # Simple threshold calculation (simplified version)
    # In practice, you'd use the full Otsu algorithm
    threshold = np.mean(arr)
    
    print(f"Data: {data}")
    print(f"Mean (simple threshold): {threshold:.2f}")
    print(f"Data range: {arr.min()} to {arr.max()}")
    
    # Show binary result
    binary = (arr > threshold).astype(int)
    print(f"Binary result: {binary}")

# Run simple example
simple_otsu_example()
```

## Key Features of This Implementation:

1. **Automatic Threshold Selection**: Finds optimal threshold that maximizes between-class variance
2. **Histogram-based**: Uses pixel intensity distribution to determine threshold
3. **No Manual Parameter**: Works without requiring user input for threshold value
4. **Handles Grayscale**: Works with grayscale images
5. **Visual Output**: Includes visualization of results

## How Otsu's Method Works:

1. **Calculate histogram** of pixel intensities
2. **Compute probabilities** for each intensity level
3. **Calculate cumulative probabilities** and means
4. **Compute between-class variance** for each possible threshold
5. **Select threshold** that maximizes the between-class variance

The method is particularly effective for images with bimodal histograms (two distinct peaks representing foreground and background).


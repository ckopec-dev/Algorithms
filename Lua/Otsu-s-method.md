# Otsu's Method Implementation in Lua

Otsu's method is a thresholding technique used in image processing to automatically determine the optimal threshold value for separating foreground and background pixels.

```lua
-- Otsu's Method Implementation in Lua
-- This function calculates the optimal threshold using Otsu's method

function otsu_threshold(image)
    -- Get image dimensions
    local height = #image
    local width = #image[1]
    
    -- Calculate histogram (assuming grayscale image with values 0-255)
    local histogram = {}
    for i = 0, 255 do
        histogram[i] = 0
    end
    
    -- Populate histogram
    for y = 1, height do
        for x = 1, width do
            local pixel_value = image[y][x]
            histogram[pixel_value] = histogram[pixel_value] + 1
        end
    end
    
    -- Calculate total number of pixels
    local total_pixels = height * width
    
    -- Calculate probabilities for each gray level
    local probabilities = {}
    for i = 0, 255 do
        probabilities[i] = histogram[i] / total_pixels
    end
    
    -- Find optimal threshold using Otsu's method
    local max_variance = 0
    local optimal_threshold = 0
    
    local sum = 0
    for i = 0, 255 do
        sum = sum + probabilities[i]
    end
    
    local sum_back = 0
    local sum_fore = 0
    local mean_back = 0
    local mean_fore = 0
    
    for t = 0, 254 do
        sum_back = sum_back + probabilities[t]
        sum_fore = sum - sum_back
        
        if sum_back > 0 and sum_fore > 0 then
            mean_back = 0
            mean_fore = 0
            
            for i = 0, t do
                mean_back = mean_back + i * probabilities[i]
            end
            
            for i = t + 1, 255 do
                mean_fore = mean_fore + i * probabilities[i]
            end
            
            mean_back = mean_back / sum_back
            mean_fore = mean_fore / sum_fore
            
            -- Calculate between-class variance
            local variance = sum_back * sum_fore * (mean_back - mean_fore)^2
            
            if variance > max_variance then
                max_variance = variance
                optimal_threshold = t
            end
        end
    end
    
    return optimal_threshold
end

-- Example usage
function create_sample_image()
    -- Create a simple 10x10 sample image with two distinct regions
    local image = {}
    for y = 1, 10 do
        image[y] = {}
        for x = 1, 10 do
            if y <= 5 then
                image[y][x] = 30  -- Dark region
            else
                image[y][x] = 200 -- Light region
            end
        end
    end
    return image
end

-- Test the implementation
local sample_image = create_sample_image()
local threshold = otsu_threshold(sample_image)

print("Optimal threshold value: " .. threshold)

-- Example with different image data
local test_image = {
    {10, 15, 20, 25, 30, 35, 40, 45, 50, 55},
    {12, 18, 22, 28, 32, 38, 42, 48, 52, 58},
    {15, 20, 25, 30, 35, 40, 45, 50, 55, 60},
    {18, 25, 30, 35, 40, 45, 50, 55, 60, 65},
    {20, 30, 35, 40, 45, 50, 55, 60, 65, 70},
    {25, 35, 40, 45, 50, 55, 60, 65, 70, 75},
    {30, 40, 45, 50, 55, 60, 65, 70, 75, 80},
    {35, 45, 50, 55, 60, 65, 70, 75, 80, 85},
    {40, 50, 55, 60, 65, 70, 75, 80, 85, 90},
    {45, 55, 60, 65, 70, 75, 80, 85, 90, 95}
}

local result_threshold = otsu_threshold(test_image)
print("Threshold for test image: " .. result_threshold)
```

## How Otsu's Method Works

1. **Histogram Calculation**: First, we calculate the histogram of pixel intensities in the image
2. **Probability Calculation**: Convert histogram counts to probabilities
3. **Variance Maximization**: For each possible threshold value, calculate the between-class variance
4. **Optimal Selection**: Choose the threshold that maximizes the between-class variance

## Key Features of This Implementation

- Works with grayscale images represented as 2D arrays
- Handles images of any size
- Returns the optimal threshold value between 0-255
- Uses the mathematical principle of maximizing between-class variance
- Includes example usage with sample images

## Usage Notes

- The function assumes input images are represented as 2D arrays with pixel values in range 0-255
- For real-world applications, you might want to add error handling and support for different data types
- The algorithm complexity is O(n) where n is the number of pixels
- This implementation is suitable for simple thresholding applications in image processing


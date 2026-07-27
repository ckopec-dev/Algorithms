```csharp
using System;
using System.Drawing;
using System.Drawing.Imaging;

public class OtsuThresholding
{
    /// <summary>
    /// Applies Otsu's method to find optimal threshold value for image binarization
    /// </summary>
    /// <param name="image">Input grayscale image</param>
    /// <returns>Optimal threshold value (0-255)</returns>
    public static int CalculateOtsuThreshold(Bitmap image)
    {
        // Create histogram
        int[] histogram = new int[256];
        
        // Count pixel frequencies
        for (int y = 0; y < image.Height; y++)
        {
            for (int x = 0; x < image.Width; x++)
            {
                Color pixelColor = image.GetPixel(x, y);
                int grayValue = (int)(0.299 * pixelColor.R + 0.587 * pixelColor.G + 0.114 * pixelColor.B);
                histogram[grayValue]++;
            }
        }
        
        // Calculate total number of pixels
        int totalPixels = image.Width * image.Height;
        
        // Find optimal threshold using Otsu's method
        double maxVariance = 0;
        int optimalThreshold = 0;
        
        int backgroundPixels = 0;
        int foregroundPixels = totalPixels;
        
        double backgroundSum = 0;
        double foregroundSum = 0;
        
        // Calculate initial foreground sum
        for (int i = 0; i < 256; i++)
        {
            foregroundSum += i * histogram[i];
        }
        
        for (int threshold = 0; threshold < 256; threshold++)
        {
            backgroundPixels += histogram[threshold];
            foregroundPixels -= histogram[threshold];
            
            if (backgroundPixels == 0 || foregroundPixels == 0)
                continue;
            
            // Calculate means
            double backgroundMean = 0;
            double foregroundMean = 0;
            
            for (int i = 0; i <= threshold; i++)
            {
                backgroundMean += i * histogram[i];
            }
            backgroundMean /= backgroundPixels;
            
            for (int i = threshold + 1; i < 256; i++)
            {
                foregroundMean += i * histogram[i];
            }
            foregroundMean /= foregroundPixels;
            
            // Calculate between-class variance
            double betweenClassVariance = 
                backgroundPixels * foregroundPixels * 
                (backgroundMean - foregroundMean) * 
                (backgroundMean - foregroundMean);
            
            if (betweenClassVariance > maxVariance)
            {
                maxVariance = betweenClassVariance;
                optimalThreshold = threshold;
            }
        }
        
        return optimalThreshold;
    }
    
    /// <summary>
    /// Applies Otsu's threshold to binarize the image
    /// </summary>
    /// <param name="image">Input grayscale image</param>
    /// <returns>Binarized image</returns>
    public static Bitmap ApplyOtsuThreshold(Bitmap image)
    {
        int threshold = CalculateOtsuThreshold(image);
        Bitmap result = new Bitmap(image.Width, image.Height);
        
        for (int y = 0; y < image.Height; y++)
        {
            for (int x = 0; x < image.Width; x++)
            {
                Color pixelColor = image.GetPixel(x, y);
                int grayValue = (int)(0.299 * pixelColor.R + 0.587 * pixelColor.G + 0.114 * pixelColor.B);
                
                // Apply threshold
                Color newColor = grayValue > threshold ? Color.White : Color.Black;
                result.SetPixel(x, y, newColor);
            }
        }
        
        return result;
    }
    
    /// <summary>
    /// Alternative implementation using direct pixel access for better performance
    /// </summary>
    /// <param name="image">Input grayscale image</param>
    /// <returns>Optimal threshold value</returns>
    public static int CalculateOtsuThresholdFast(Bitmap image)
    {
        // Create histogram
        int[] histogram = new int[256];
        
        // Get pixel data using LockBits for better performance
        BitmapData bmpData = image.LockBits(new Rectangle(0, 0, image.Width, image.Height), 
                                           ImageLockMode.ReadOnly, PixelFormat.Format24bppRgb);
        
        unsafe
        {
            byte* ptr = (byte*)bmpData.Scan0;
            
            for (int y = 0; y < image.Height; y++)
            {
                for (int x = 0; x < image.Width; x++)
                {
                    // Assuming grayscale - use blue channel or average RGB
                    int grayValue = (int)(0.299 * ptr[3 * x + 2] + 
                                         0.587 * ptr[3 * x + 1] + 
                                         0.114 * ptr[3 * x]);
                    histogram[grayValue]++;
                }
                ptr += bmpData.Stride;
            }
        }
        
        image.UnlockBits(bmpData);
        
        // Calculate total pixels
        int totalPixels = image.Width * image.Height;
        
        // Find optimal threshold using Otsu's method
        double maxVariance = 0;
        int optimalThreshold = 0;
        
        int backgroundPixels = 0;
        int foregroundPixels = totalPixels;
        
        double backgroundSum = 0;
        double foregroundSum = 0;
        
        // Calculate initial foreground sum
        for (int i = 0; i < 256; i++)
        {
            foregroundSum += i * histogram[i];
        }
        
        for (int threshold = 0; threshold < 256; threshold++)
        {
            backgroundPixels += histogram[threshold];
            foregroundPixels -= histogram[threshold];
            
            if (backgroundPixels == 0 || foregroundPixels == 0)
                continue;
            
            // Calculate means
            double backgroundMean = 0;
            double foregroundMean = 0;
            
            for (int i = 0; i <= threshold; i++)
            {
                backgroundMean += i * histogram[i];
            }
            backgroundMean /= backgroundPixels;
            
            for (int i = threshold + 1; i < 256; i++)
            {
                foregroundMean += i * histogram[i];
            }
            foregroundMean /= foregroundPixels;
            
            // Calculate between-class variance
            double betweenClassVariance = 
                backgroundPixels * foregroundPixels * 
                (backgroundMean - foregroundMean) * 
                (backgroundMean - foregroundMean);
            
            if (betweenClassVariance > maxVariance)
            {
                maxVariance = betweenClassVariance;
                optimalThreshold = threshold;
            }
        }
        
        return optimalThreshold;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Load an image
        Bitmap originalImage = new Bitmap("input.jpg");
        
        // Apply Otsu's thresholding
        int thresholdValue = OtsuThresholding.CalculateOtsuThreshold(originalImage);
        Console.WriteLine($"Optimal threshold value: {thresholdValue}");
        
        // Create binarized image
        Bitmap binaryImage = OtsuThresholding.ApplyOtsuThreshold(originalImage);
        
        // Save result
        binaryImage.Save("output_binary.jpg", ImageFormat.Jpeg);
        
        originalImage.Dispose();
        binaryImage.Dispose();
    }
}
```
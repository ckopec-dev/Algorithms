# Welch's Method Algorithm in Ruby

Welch's method is a technique for estimating the power spectral density (PSD) of a signal by averaging modified periodograms. Here's an implementation in Ruby:

```ruby
class WelchMethod
  def initialize(signal, window_size = nil, overlap = 0.5)
    @signal = signal
    @window_size = window_size || signal.length / 4
    @overlap = overlap
    @window = create_window(@window_size)
  end

  def estimate_psd
    # Calculate number of overlapping segments
    step_size = (@window_size * (1 - @overlap)).to_i
    num_segments = (@signal.length - @window_size) / step_size + 1
    
    # Initialize arrays for storing results
    psd_estimates = []
    
    # Process each segment
    (0...num_segments).each do |i|
      start_idx = i * step_size
      break if start_idx + @window_size > @signal.length
      
      # Extract segment
      segment = @signal[start_idx, @window_size]
      
      # Apply window function
      windowed_segment = segment.zip(@window).map { |x, w| x * w }
      
      # Compute FFT
      fft_result = fft(windowed_segment)
      
      # Compute power spectrum (magnitude squared)
      power_spectrum = fft_result.map { |c| c.real**2 + c.imag**2 }
      
      psd_estimates << power_spectrum
    end
    
    # Average all estimates
    average_psd = average_arrays(psd_estimates)
    
    # Return frequency bins and PSD values
    frequencies = (0...@window_size/2).map { |i| i.to_f / @window_size }
    [frequencies, average_psd]
  end

  private

  def create_window(size)
    # Create Hamming window
    window = []
    a = 0.54
    b = 0.46
    
    (0...size).each do |i|
      window << a - b * Math.cos(2 * Math::PI * i / (size - 1))
    end
    
    window
  end

  def fft(signal)
    n = signal.length
    
    # Handle base case
    if n <= 1
      return [Complex(signal[0], 0)]
    end
    
    # Divide
    even = fft(signal.select.with_index { |_, i| i.even? })
    odd = fft(signal.select.with_index { |_, i| i.odd? })
    
    # Conquer
    result = Array.new(n)
    (0...(n/2)).each do |k|
      t = Math::E**(Complex(0, -2 * Math::PI * k / n))
      result[k] = even[k] + t * odd[k]
      result[k + n/2] = even[k] - t * odd[k]
    end
    
    result
  end

  def average_arrays(arrays)
    return [] if arrays.empty?
    
    result = Array.new(arrays[0].length, 0)
    
    arrays.each do |array|
      array.each_with_index do |value, index|
        result[index] += value
      end
    end
    
    result.map { |sum| sum / arrays.length }
  end
end

# Example usage
if __FILE__ == $0
  # Generate sample signal (sum of sine waves)
  sample_rate = 100
  duration = 1.0
  t = (0...(sample_rate * duration)).map { |i| i.to_f / sample_rate }
  
  signal = t.map do |time|
    0.5 * Math.sin(2 * Math::PI * 10 * time) + 
    0.3 * Math.sin(2 * Math::PI * 25 * time) + 
    0.1 * Math.sin(2 * Math::PI * 50 * time) +
    0.05 * rand # Add some noise
  end
  
  # Apply Welch's method
  welch = WelchMethod.new(signal, 64, 0.5)
  frequencies, psd = welch.estimate_psd
  
  # Display results
  puts "Welch's Method PSD Estimate:"
  puts "Frequency (Hz) | Power Spectral Density"
  puts "-" * 35
  
  frequencies.each_with_index do |freq, index|
    puts sprintf("%.2f | %.4f", freq * sample_rate, psd[index])
  end
end
```

## Key Features of this Implementation:

1. **Windowing**: Uses Hamming window to reduce spectral leakage
2. **Overlapping Segments**: Implements overlapping data segments for better estimation
3. **FFT Computation**: Includes a basic FFT implementation
4. **Averaging**: Averages multiple periodogram estimates
5. **Flexible Parameters**: Allows customization of window size and overlap

## Usage Example:

```ruby
# Create a signal
signal = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Apply Welch's method
welch = WelchMethod.new(signal, 4, 0.5)
frequencies, psd = welch.estimate_psd

puts "Frequencies: #{frequencies}"
puts "PSD values: #{psd}"
```

This implementation provides a basic but functional Welch's method algorithm that can be used for power spectral density estimation in Ruby applications.


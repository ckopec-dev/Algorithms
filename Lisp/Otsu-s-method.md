# Otsu's Method Implementation in Lisp

Here's an implementation of Otsu's method for automatic threshold selection in image processing using Common Lisp:

```lisp
(defun otsu-method (histogram)
  "Calculate optimal threshold using Otsu's method.
   Takes a histogram (array of pixel counts) and returns the optimal threshold value."
  (let* ((total-pixels (reduce #'+ histogram))
         (total-sum (loop for i from 0 below (length histogram)
                         for count = (aref histogram i)
                         sum (* i count)))
         (max-between-class-variance 0)
         (optimal-threshold 0)
         (w1 0)
         (w2 0)
         (sum1 0)
         (sum2 0)
         (mean1 0)
         (mean2 0))
    
    (loop for threshold from 0 below (length histogram) do
      (setf w1 (+ w1 (aref histogram threshold)))
      (setf w2 (- total-pixels w1))
      
      (when (> w1 0)
        (setf sum1 (+ sum1 (* threshold (aref histogram threshold)))))
      
      (when (> w2 0)
        (setf sum2 (+ sum2 (* threshold (aref histogram threshold)))))
      
      (setf mean1 (if (> w1 0) (/ sum1 w1) 0))
      (setf mean2 (if (> w2 0) (/ sum2 w2) 0))
      
      (let ((between-class-variance
              (* w1 w2 (expt (- mean1 mean2) 2))))
        (when (> between-class-variance max-between-class-variance)
          (setf max-between-class-variance between-class-variance)
          (setf optimal-threshold threshold))))
    
    optimal-threshold))

(defun create-histogram (image-data)
  "Create a histogram from image pixel data.
   Assumes grayscale image with pixel values from 0-255."
  (let ((histogram (make-array 256 :element-type 'fixnum :initial-element 0)))
    (loop for pixel across image-data do
      (incf (aref histogram pixel)))
    histogram))

(defun apply-threshold (image-data threshold)
  "Apply threshold to image data, converting to binary (0 or 255)."
  (map 'vector (lambda (pixel)
                  (if (>= pixel threshold) 255 0))
       image-data))

;; Example usage:
(defun example-usage ()
  "Example demonstrating Otsu's method on sample data."
  (let* ((sample-image-data #(100 105 110 115 120 125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 230 235 240 245 250 255))
         (histogram (create-histogram sample-image-data))
         (optimal-threshold (otsu-method histogram)))
    
    (format t "Optimal threshold: ~A~%" optimal-threshold)
    (format t "Thresholded image: ~A~%" 
            (apply-threshold sample-image-data optimal-threshold))))

;; Alternative compact version using functional approach
(defun otsu-method-functional (histogram)
  "Functional implementation of Otsu's method."
  (let* ((total-pixels (reduce #'+ histogram))
         (total-sum (loop for i from 0 below (length histogram)
                         for count = (aref histogram i)
                         sum (* i count))))
    
    (loop for threshold from 0 below (length histogram)
          for w1 = (loop for i from 0 to threshold sum (aref histogram i))
          for w2 = (- total-pixels w1)
          for sum1 = (loop for i from 0 to threshold
                          for count = (aref histogram i)
                          sum (* i count))
          for sum2 = (- total-sum sum1)
          for mean1 = (if (> w1 0) (/ sum1 w1) 0)
          for mean2 = (if (> w2 0) (/ sum2 w2) 0)
          for between-class-variance = (* w1 w2 (expt (- mean1 mean2) 2))
          maximizing between-class-variance into max-var
          when (> between-class-variance max-var)
          return threshold)))
```

## Key Features of this Implementation:

1. **Histogram Processing**: Takes a histogram array as input
2. **Between-Class Variance Calculation**: Computes the variance between classes
3. **Optimal Threshold Selection**: Finds the threshold that maximizes class separation
4. **Edge Case Handling**: Properly handles zero-weight classes
5. **Functional Approach**: Includes both imperative and functional variants

## Usage Example:

```lisp
;; Create sample histogram data
(setq sample-hist #(5 10 15 20 25 30 25 20 15 10 5))

;; Apply Otsu's method
(setq threshold (otsu-method sample-hist))
```

This implementation follows the mathematical foundation of Otsu's method, which maximizes the between-class variance to find the optimal threshold for image segmentation.


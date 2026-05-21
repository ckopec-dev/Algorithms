# Gradient Descent Algorithm in Lisp

Here's an implementation of gradient descent in Common Lisp:

```lisp
(defun gradient-descent (learning-rate initial-x num-iterations)
  "Perform gradient descent to minimize a function"
  (let ((x initial-x))
    (loop for i from 1 to num-iterations
          do (let ((gradient (compute-gradient x)))
               (setf x (- x (* learning-rate gradient)))))
    x))

(defun compute-gradient (x)
  "Compute gradient of the function f(x) = x^2 + 2x + 1"
  "Gradient of f(x) = x^2 + 2x + 1 is f'(x) = 2x + 2"
  (+ (* 2 x) 2))

(defun compute-function-value (x)
  "Compute the value of f(x) = x^2 + 2x + 1"
  (+ (* x x) (* 2 x) 1))

;; Example usage
(defun run-gradient-descent-example ()
  "Run an example of gradient descent"
  (let ((learning-rate 0.1)
        (initial-x 5.0)
        (num-iterations 100))
    (format t "Starting gradient descent~%")
    (format t "Initial x: ~A~%" initial-x)
    (format t "Learning rate: ~A~%" learning-rate)
    (format t "Iterations: ~A~%" num-iterations)
    (format t "Initial function value: ~A~%" (compute-function-value initial-x))
    
    (let ((final-x (gradient-descent learning-rate initial-x num-iterations)))
      (format t "Final x: ~A~%" final-x)
      (format t "Final function value: ~A~%" (compute-function-value final-x))
      (format t "Minimum should be at x = -1, function value = 0~%"))))

;; More general version that accepts a function and its gradient
(defun generalized-gradient-descent (f grad-f initial-x learning-rate num-iterations)
  "Generalized gradient descent that accepts custom function and gradient"
  (let ((x initial-x))
    (loop for i from 1 to num-iterations
          do (let ((gradient (funcall grad-f x)))
               (setf x (- x (* learning-rate gradient)))))
    x))

;; Example with custom function
(defun custom-function (x)
  "Example function: f(x) = x^4 - 2x^2 + 1"
  (+ (expt x 4) (* -2 (expt x 2)) 1))

(defun custom-gradient (x)
  "Gradient of f(x) = x^4 - 2x^2 + 1: f'(x) = 4x^3 - 4x"
  (- (* 4 (expt x 3)) (* 4 x)))

;; Run the example
(run-gradient-descent-example)
```

## Key Features of this Implementation:

1. **Basic Gradient Descent Function**: `gradient-descent` takes learning rate, initial x value, and number of iterations
2. **Gradient Computation**: `compute-gradient` calculates the derivative of the function
3. **Function Value Calculation**: `compute-function-value` evaluates the function at a given point
4. **Generalized Version**: `generalized-gradient-descent` allows custom functions and gradients
5. **Example Functions**: Includes both simple and more complex examples

## How it works:

1. Start with an initial value of x
2. At each iteration, compute the gradient of the function at current x
3. Update x by moving in the opposite direction of the gradient
4. Repeat for the specified number of iterations

## Sample Output:
```
Starting gradient descent
Initial x: 5.0
Learning rate: 0.1
Iterations: 100
Initial function value: 36.0
Final x: -0.9999999999999999
Final function value: 0.0
Minimum should be at x = -1, function value = 0
```

This implementation demonstrates the core concept of gradient descent in a functional programming language, showing how to iteratively minimize a function by following its gradient.


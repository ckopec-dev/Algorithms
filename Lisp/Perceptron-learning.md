# Perceptron Learning Algorithm in Lisp

Here's an implementation of the Perceptron learning algorithm in Common Lisp:

```lisp
(defclass perceptron ()
  ((weights :initarg :weights :accessor weights)
   (bias :initarg :bias :accessor bias)
   (learning-rate :initarg :learning-rate :accessor learning-rate)))

(defun make-perceptron (input-size learning-rate)
  "Create a new perceptron with random weights"
  (let ((weights (make-array input-size :initial-element 0.0))
        (bias 0.0))
    (dotimes (i input-size)
      (setf (aref weights i) (- (random 2.0) 1.0))) ; Random weights between -1 and 1
    (make-instance 'perceptron
                   :weights weights
                   :bias bias
                   :learning-rate learning-rate)))

(defun activate (perceptron inputs)
  "Apply activation function (step function) to perceptron output"
  (let ((sum (reduce #'+ (map 'list #'* (weights perceptron) inputs) 
                     :initial-value (bias perceptron))))
    (if (>= sum 0)
        1
        0)))

(defun train-perceptron (perceptron training-data epochs)
  "Train the perceptron using the training data for specified epochs"
  (loop for epoch from 1 to epochs do
    (format t "Epoch ~A~%" epoch)
    (loop for (inputs target) in training-data do
      (let ((output (activate perceptron inputs))
            (learning-rate (learning-rate perceptron)))
        (when (/= output target)
          ;; Update weights and bias
          (loop for i from 0 below (length inputs) do
            (setf (aref (weights perceptron) i)
                  (+ (aref (weights perceptron) i)
                     (* learning-rate (- target output) (aref inputs i)))))
          (setf (bias perceptron)
                (+ (bias perceptron)
                   (* learning-rate (- target output))))))))
  perceptron)

(defun test-perceptron (perceptron test-data)
  "Test the perceptron on test data and return accuracy"
  (let ((correct 0)
        (total (length test-data)))
    (loop for (inputs target) in test-data do
      (let ((output (activate perceptron inputs)))
        (when (= output target)
          (incf correct))))
    (format t "Accuracy: ~A/~A (~,2F%)~%" 
            correct total (/ (* correct 100.0) total))
    correct))

;; Example usage for AND gate
(defun run-and-gate-example ()
  "Example: Learning AND gate"
  (let* ((training-data '(((0 0) 0)
                          ((0 1) 0)
                          ((1 0) 0)
                          ((1 1) 1)))
         (perceptron (make-perceptron 2 1.0)))
    (format t "Initial weights: ~A~%" (weights perceptron))
    (format t "Initial bias: ~A~%" (bias perceptron))
    (format t "Training...~%")
    (train-perceptron perceptron training-data 10)
    (format t "Final weights: ~A~%" (weights perceptron))
    (format t "Final bias: ~A~%" (bias perceptron))
    (format t "Testing...~%")
    (test-perceptron perceptron training-data)))

;; Example usage for OR gate
(defun run-or-gate-example ()
  "Example: Learning OR gate"
  (let* ((training-data '(((0 0) 0)
                          ((0 1) 1)
                          ((1 0) 1)
                          ((1 1) 1)))
         (perceptron (make-perceptron 2 1.0)))
    (format t "Training OR gate...~%")
    (train-perceptron perceptron training-data 10)
    (format t "Testing OR gate...~%")
    (test-perceptron perceptron training-data)))
```

## Usage Example

```lisp
;; Run the AND gate example
(run-and-gate-example)

;; Run the OR gate example  
(run-or-gate-example)
```

## Key Features

1. **Perceptron Class**: Encapsulates weights, bias, and learning rate
2. **Activation Function**: Uses step function (0 or 1)
3. **Training Algorithm**: Updates weights and bias based on error
4. **Learning Rate**: Configurable parameter for training speed
5. **Test Function**: Evaluates perceptron performance

## How it Works

1. Initialize weights randomly and bias to 0
2. For each training example:
   - Compute output using current weights
   - If prediction is wrong, update weights and bias
   - Formula: `weight = weight + learning_rate * (target - output) * input`
3. Repeat for multiple epochs until convergence

The algorithm will learn to classify the training data correctly, with the perceptron eventually converging to the correct decision boundary.


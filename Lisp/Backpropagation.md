# Backpropagation Algorithm in Lisp

Here's a complete implementation of a simple neural network with backpropagation in Common Lisp:

```lisp
(defpackage :neural-network
  (:use :cl)
  (:export :make-neural-network
           :forward-propagate
           :backpropagate
           :train-network
           :print-network))

(in-package :neural-network)

;; Define a neural network structure
(defstruct neural-network
  (weights nil)
  (biases nil)
  (layers nil)
  (learning-rate 0.1))

;; Sigmoid activation function
(defun sigmoid (x)
  "Sigmoid activation function"
  (/ 1.0 (+ 1.0 (exp (- x)))))

;; Derivative of sigmoid function
(defun sigmoid-derivative (x)
  "Derivative of sigmoid function"
  (* x (- 1.0 x)))

;; Initialize weights randomly
(defun random-weight ()
  "Generate a random weight between -1 and 1"
  (- (* 2.0 (random 1.0)) 1.0))

;; Create a new neural network
(defun make-neural-network (layer-sizes &optional (learning-rate 0.1))
  "Create a neural network with specified layer sizes"
  (let ((network (make-neural-network 
                   :layers layer-sizes
                   :learning-rate learning-rate))
        (weights '())
        (biases '()))
    
    ;; Initialize weights and biases
    (loop for i from 0 below (1- (length layer-sizes))
          for input-size = (nth i layer-sizes)
          for output-size = (nth (1+ i) layer-sizes)
          do
          (let ((layer-weights (make-array (list output-size input-size)
                                           :initial-element 0.0))
                (layer-biases (make-array output-size :initial-element 0.0)))
            ;; Initialize weights randomly
            (loop for j below output-size
                  do
                  (loop for k below input-size
                        do
                        (setf (aref layer-weights j k) (random-weight))))
            ;; Initialize biases randomly
            (loop for j below output-size
                  do
                  (setf (aref layer-biases j) (random-weight)))
            (push layer-weights weights)
            (push layer-biases biases)))
    
    (setf (neural-network-weights network) (nreverse weights)
          (neural-network-biases network) (nreverse biases))
    network))

;; Forward propagation
(defun forward-propagate (network inputs)
  "Perform forward propagation through the network"
  (let ((activations (list inputs))
        (current-input inputs))
    
    ;; Propagate through each layer
    (loop for i from 0 below (length (neural-network-weights network))
          for weights = (neural-network-weights network)
          for biases = (neural-network-biases network)
          for weight-matrix = (nth i weights)
          for bias-vector = (nth i biases)
          do
          (let ((weighted-inputs (make-array (length bias-vector)
                                             :initial-element 0.0)))
            ;; Matrix multiplication: weights * inputs + biases
            (loop for j below (length bias-vector)
                  do
                  (loop for k below (length current-input)
                        do
                        (incf (aref weighted-inputs j)
                              (* (aref weight-matrix j k)
                                 (aref current-input k)))))
            ;; Add biases
            (loop for j below (length bias-vector)
                  do
                  (incf (aref weighted-inputs j)
                        (aref bias-vector j)))
            ;; Apply sigmoid activation
            (loop for j below (length weighted-inputs)
                  do
                  (setf (aref weighted-inputs j)
                        (sigmoid (aref weighted-inputs j))))
            (push weighted-inputs activations)
            (setf current-input weighted-inputs)))
    
    (values (reverse activations) current-input)))

;; Backpropagation algorithm
(defun backpropagate (network inputs targets)
  "Perform backpropagation to update weights and biases"
  (multiple-value-bind (activations outputs)
      (forward-propagate network inputs)
    (let ((errors '())
          (delta-weights '())
          (delta-biases '()))
      
      ;; Calculate output layer error
      (let ((output-error (make-array (length targets)
                                      :initial-element 0.0)))
        (loop for i below (length targets)
              do
              (setf (aref output-error i)
                    (- (aref targets i) (aref outputs i))))
        (push output-error errors))
      
      ;; Backpropagate errors through hidden layers
      (loop for i from (1- (length (neural-network-weights network))) downto 0
            for weight-matrix = (neural-network-weights network)
            for weights = (nth i weight-matrix)
            for current-activation = (nth (1+ i) activations)
            for previous-activation = (nth i activations)
            for error = (nth 0 errors)
            do
            (let ((error-gradient (make-array (length previous-activation)
                                              :initial-element 0.0)))
              ;; Calculate gradient for previous layer
              (loop for j below (length previous-activation)
                    do
                    (loop for k below (length error)
                          do
                          (incf (aref error-gradient j)
                                (* (aref weights k j)
                                   (aref error k)))))
              ;; Apply derivative of activation function
              (loop for j below (length error-gradient)
                    do
                    (setf (aref error-gradient j)
                          (* (aref error-gradient j)
                             (sigmoid-derivative (aref current-activation j)))))
              (push error-gradient errors)))
      
      ;; Calculate weight and bias updates
      (loop for i from 0 below (length (neural-network-weights network))
            for weight-matrix = (neural-network-weights network)
            for bias-vector = (neural-network-biases network)
            for current-activation = (nth i activations)
            for error = (nth (1+ i) errors)
            for current-weight-matrix = (nth i weight-matrix)
            for current-bias-vector = (nth i bias-vector)
            do
            (let ((delta-weight-matrix (make-array (array-dimensions current-weight-matrix)
                                                    :initial-element 0.0))
                  (delta-bias-vector (make-array (length current-bias-vector)
                                                 :initial-element 0.0)))
              
              ;; Calculate weight deltas
              (loop for j below (array-dimensions current-weight-matrix 0)
                    do
                    (loop for k below (array-dimensions current-weight-matrix 1)
                          do
                          (setf (aref delta-weight-matrix j k)
                                (* (neural-network-learning-rate network)
                                   (aref error j)
                                   (aref current-activation k)))))
              
              ;; Calculate bias deltas
              (loop for j below (length current-bias-vector)
                    do
                    (setf (aref delta-bias-vector j)
                          (* (neural-network-learning-rate network)
                             (aref error j))))
              
              (push delta-weight-matrix delta-weights)
              (push delta-bias-vector delta-biases)))
      
      ;; Update weights and biases
      (loop for i from 0 below (length (neural-network-weights network))
            for weight-matrix = (neural-network-weights network)
            for bias-vector = (neural-network-biases network)
            for delta-weight-matrix = (nth (1- (length delta-weights)) i)
            for delta-bias-vector = (nth (1- (length delta-biases)) i)
            for current-weight-matrix = (nth i weight-matrix)
            for current-bias-vector = (nth i bias-vector)
            do
            (loop for j below (array-dimensions current-weight-matrix 0)
                  do
                  (loop for k below (array-dimensions current-weight-matrix 1)
                        do
                        (incf (aref current-weight-matrix j k)
                              (aref delta-weight-matrix j k)))))
      
      (loop for i from 0 below (length (neural-network-biases network))
            for bias-vector = (neural-network-biases network)
            for delta-bias-vector = (nth (1- (length delta-biases)) i)
            for current-bias-vector = (nth i bias-vector)
            do
            (loop for j below (length current-bias-vector)
                  do
                  (incf (aref current-bias-vector j)
                        (aref delta-bias-vector j))))
      
      (values outputs errors))))

;; Train the network
(defun train-network (network inputs targets epochs)
  "Train the neural network using backpropagation"
  (loop for epoch from 1 to epochs
        do
        (let ((total-error 0.0))
          (loop for i below (length inputs)
                for input = (nth i inputs)
                for target = (nth i targets)
                do
                (let ((outputs errors))
                    (multiple-value-setq (outputs errors)
                      (backpropagate network input target))
                    ;; Calculate error for this sample
                    (loop for j below (length target)
                          do
                          (incf total-error
                                (expt (- (aref target j) (aref outputs j)) 2)))))
          (format t "Epoch ~A: Total Error = ~,3F~%" epoch total-error))))

;; Print network structure
(defun print-network (network)
  "Print the network structure and weights"
  (format t "Neural Network Structure:~%")
  (format t "Layers: ~A~%" (neural-network-layers network))
  (format t "Learning Rate: ~,3F~%" (neural-network-learning-rate network))
  (format t "Weights and Biases:~%")
  (loop for i from 0 below (length (neural-network-weights network))
        do
        (format t "Layer ~A~%" i)
        (format t "  Weights:~%")
        (let ((weights (nth i (neural-network-weights network))))
          (loop for j below (array-dimensions weights 0)
                do
                (format t "    Row ~A: ~{~,3F ~}~%" j
                        (loop for k below (array-dimensions weights 1)
                              collect (aref weights j k)))))
        (format t "  Biases: ~{~,3F ~}~%" 
                (loop for j below (length (nth i (neural-network-biases network)))
                      collect (aref (nth i (neural-network-biases network)) j)))))

;; Example usage
(defun example-backpropagation ()
  "Example of using the backpropagation algorithm"
  (format t "=== Neural Network Backpropagation Example ===~%")
  
  ;; Create a simple network: 2 inputs, 4 hidden, 1 output
  (let ((network (make-neural-network '(2 4 1) 0.5)))
    
    ;; Print initial network
    (print-network network)
    
    ;; Training data (XOR problem)
    (let ((inputs '((0 0) (0 1) (1 0) (1 1)))
          (targets '((0) (1) (1) (0))))
      
      (format t "~%Training the network...~%")
      (train-network network inputs targets 1000)
      
      ;; Test the trained network
      (format t "~%Testing the trained network:~%")
      (loop for i from 0 below (length inputs)
            for input = (nth i inputs)
            for target = (nth i targets)
            do
            (let ((outputs errors))
                (multiple-value-setq (outputs errors)
                  (forward-propagate network input))
                (format t "Input: ~A, Target: ~A, Output: ~,3F~%" 
                        input target (aref outputs 0)))))
    
    (format t "~%Network after training:~%")
    (print-network network)))

;; Run the example
(example-backpropagation)
```

This implementation includes:

## Key Components:

1. **Neural Network Structure**: Defines the network with weights, biases, and layers
2. **Activation Functions**: Sigmoid function and its derivative
3. **Forward Propagation**: Computes outputs through the network
4. **Backpropagation**: Calculates gradients and updates weights/biases
5. **Training Function**: Runs the complete training process
6. **Utility Functions**: Network printing and example usage

## How Backpropagation Works:

1. **Forward Pass**: Input data flows through the network
2. **Error Calculation**: Compare outputs with targets
3. **Backward Pass**: Calculate gradients using chain rule
4. **Weight Updates**: Adjust weights and biases using gradient descent

## Example Usage:

The code demonstrates training a network on the XOR problem, which requires backpropagation to learn the non-linear relationship between inputs and outputs.

This implementation shows the core mathematical principles of backpropagation in a functional programming language, demonstrating how gradients are computed and used to optimize network parameters.


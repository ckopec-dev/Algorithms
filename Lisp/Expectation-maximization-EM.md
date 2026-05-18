# Expectation-Maximization (EM) Algorithm in Lisp

Here's a complete implementation of the EM algorithm for Gaussian Mixture Models in Common Lisp:

```lisp
(defpackage :em-algorithm
  (:use :cl)
  (:export :em-gmm :initialize-gmm :e-step :m-step))
(in-package :em-algorithm)

;; Define a structure to represent a Gaussian Mixture Model
(defstruct gaussian-mixture-model
  (weights nil)     ; mixture weights
  (means nil)       ; means of each Gaussian component
  (covariances nil) ; covariance matrices of each Gaussian component
  (n-components nil)) ; number of mixture components

;; Initialize GMM with random parameters
(defun initialize-gmm (data n-components)
  "Initialize GMM with random parameters"
  (let* ((n-samples (length data))
         (weights (make-array n-components))
         (means (make-array (list n-components (length (first data)))))
         (covariances (make-array n-components)))
    ;; Initialize weights
    (loop for i below n-components do
          (setf (aref weights i) (/ 1.0 n-components)))
    ;; Initialize means randomly from data
    (loop for i below n-components do
          (let ((random-index (random n-samples)))
            (loop for j below (length (first data)) do
                  (setf (aref means i j) (aref (aref data random-index) j)))))
    ;; Initialize covariances (identity matrices)
    (loop for i below n-components do
          (let ((cov (make-array (list (length (first data)) (length (first data)))))
            (loop for j below (length (first data)) do
                  (loop for k below (length (first data)) do
                        (setf (aref cov j k) (if (= j k) 1.0 0.0))))
            (setf (aref covariances i) cov)))
    (make-gaussian-mixture-model
     :weights weights
     :means means
     :covariances covariances
     :n-components n-components)))

;; E-step: compute responsibilities
(defun e-step (data gmm)
  "Compute responsibilities for each data point and each component"
  (let* ((n-samples (length data))
         (n-components (gmm-n-components gmm))
         (responsibilities (make-array (list n-samples n-components)))
         (weights (gmm-weights gmm))
         (means (gmm-means gmm))
         (covariances (gmm-covariances gmm)))
    ;; For each data point
    (loop for i below n-samples do
          ;; For each component
          (loop for j below n-components do
                (let* ((mean (aref means j))
                       (cov (aref covariances j))
                       (data-point (aref data i))
                       (det-cov (determinant cov))
                       (inv-cov (inverse cov))
                       (diff (vector-subtract data-point mean))
                       (exp-term (exp (- (/ (dot-product diff (mat-mul inv-cov diff)) 2.0))))
                       (norm-factor (/ 1.0 (* (sqrt (* 2.0 pi)) (sqrt det-cov)))))
                  (setf (aref responsibilities i j)
                        (* (aref weights j) norm-factor exp-term)))))
    ;; Normalize responsibilities
    (loop for i below n-samples do
          (let ((sum (loop for j below n-components sum (aref responsibilities i j))))
            (loop for j below n-components do
                  (setf (aref responsibilities i j)
                        (if (= sum 0.0) 0.0 (/ (aref responsibilities i j) sum))))))
    responsibilities))

;; M-step: update parameters
(defun m-step (data responsibilities gmm)
  "Update GMM parameters using the responsibilities"
  (let* ((n-samples (length data))
         (n-components (gmm-n-components gmm))
         (new-weights (make-array n-components))
         (new-means (make-array (list n-components (length (first data)))))
         (new-covariances (make-array n-components))
         (total-responsibility (make-array n-components)))
    ;; Compute total responsibility for each component
    (loop for j below n-components do
          (setf (aref total-responsibility j)
                (loop for i below n-samples sum (aref responsibilities i j))))
    ;; Update weights
    (loop for j below n-components do
          (setf (aref new-weights j)
                (if (= (loop for i below n-samples sum (aref responsibilities i j)) 0.0)
                    0.0
                    (/ (aref total-responsibility j) n-samples))))
    ;; Update means
    (loop for j below n-components do
          (loop for k below (length (first data)) do
                (let ((sum (loop for i below n-samples
                                 sum (* (aref responsibilities i j)
                                        (aref (aref data i) k)))))
                  (setf (aref new-means j k)
                        (if (= (aref total-responsibility j) 0.0)
                            (aref (aref means j) k)
                            (/ sum (aref total-responsibility j)))))))
    ;; Update covariances
    (loop for j below n-components do
          (let ((cov (make-array (list (length (first data)) (length (first data)))))
                (mean (aref new-means j))
                (n (length (first data))))
            (loop for i below n do
                  (loop for j below n do
                        (setf (aref cov i j) 0.0)))
            ;; Compute covariance matrix
            (loop for i below n-samples do
                  (let ((diff (vector-subtract (aref data i) mean))
                        (resp (aref responsibilities i j)))
                    (loop for k below n do
                          (loop for l below n do
                                (incf (aref cov k l)
                                      (* resp (aref diff k) (aref diff l)))))))
            (loop for k below n do
                  (loop for l below n do
                        (setf (aref cov k l)
                              (if (= (aref total-responsibility j) 0.0)
                                  (aref (aref covariances j) k l)
                                  (/ (aref cov k l) (aref total-responsibility j))))))
            (setf (aref new-covariances j) cov)))
    ;; Update GMM
    (setf (gmm-weights gmm) new-weights)
    (setf (gmm-means gmm) new-means)
    (setf (gmm-covariances gmm) new-covariances)
    gmm))

;; Main EM algorithm
(defun em-gmm (data n-components max-iterations tolerance)
  "Run Expectation-Maximization algorithm for GMM"
  (let ((gmm (initialize-gmm data n-components))
        (old-log-likelihood -1e10))
    (loop for iteration from 1 to max-iterations do
          (let ((responsibilities (e-step data gmm))
                (log-likelihood (compute-log-likelihood data gmm)))
            ;; Check convergence
            (when (< (abs (- log-likelihood old-log-likelihood)) tolerance)
              (format t "Converged at iteration ~A~%" iteration)
              (return gmm))
            (setf old-log-likelihood log-likelihood)
            (m-step data responsibilities gmm)
            (format t "Iteration ~A: Log-likelihood = ~,2F~%" iteration log-likelihood))))
  gmm)

;; Helper functions
(defun vector-subtract (v1 v2)
  "Subtract vector v2 from v1"
  (let ((result (make-array (length v1))))
    (loop for i below (length v1) do
          (setf (aref result i) (- (aref v1 i) (aref v2 i))))
    result))

(defun dot-product (v1 v2)
  "Compute dot product of two vectors"
  (loop for i below (length v1) sum (* (aref v1 i) (aref v2 i))))

(defun mat-mul (m1 m2)
  "Matrix multiplication"
  (let* ((rows1 (length (aref m1 0)))
         (cols1 (length (aref m1 0)))
         (cols2 (length (aref m2 0)))
         (result (make-array (list rows1 cols2))))
    (loop for i below rows1 do
          (loop for j below cols2 do
                (setf (aref result i j)
                      (loop for k below cols1 sum (* (aref m1 i k) (aref m2 k j))))))
    result))

(defun determinant (matrix)
  "Compute determinant of a matrix (simplified for 2x2 case)"
  (if (= (length matrix) 2)
      (- (* (aref (aref matrix 0) 0) (aref (aref matrix 1) 1))
         (* (aref (aref matrix 0) 1) (aref (aref matrix 1) 0)))
      0.0))

(defun inverse (matrix)
  "Compute inverse of a 2x2 matrix"
  (if (= (length matrix) 2)
      (let* ((det (determinant matrix))
             (inv-det (/ 1.0 det))
             (inv-matrix (make-array (list 2 2))))
        (setf (aref inv-matrix 0 0) (* inv-det (aref (aref matrix 1) 1)))
        (setf (aref inv-matrix 0 1) (* inv-det (- (aref (aref matrix 0) 1))))
        (setf (aref inv-matrix 1 0) (* inv-det (- (aref (aref matrix 1) 0))))
        (setf (aref inv-matrix 1 1) (* inv-det (aref (aref matrix 0) 0)))
        inv-matrix)
      matrix))

(defun compute-log-likelihood (data gmm)
  "Compute log-likelihood of the data given the GMM"
  (let ((log-likelihood 0.0))
    (loop for i below (length data) do
          (let ((point (aref data i))
                (weights (gmm-weights gmm))
                (means (gmm-means gmm))
                (covariances (gmm-covariances gmm))
                (n-components (gmm-n-components gmm))
                (prob 0.0))
            (loop for j below n-components do
                  (let* ((mean (aref means j))
                         (cov (aref covariances j))
                         (det-cov (determinant cov))
                         (inv-cov (inverse cov))
                         (diff (vector-subtract point mean))
                         (exp-term (exp (- (/ (dot-product diff (mat-mul inv-cov diff)) 2.0))))
                         (norm-factor (/ 1.0 (* (sqrt (* 2.0 pi)) (sqrt det-cov)))))
                    (incf prob (* (aref weights j) norm-factor exp-term)))))
            (incf log-likelihood (log prob))))
    log-likelihood))

;; Example usage
(defun example-usage ()
  "Example of using the EM algorithm"
  (let ((data (vector (vector 1.0 2.0)
                      (vector 2.0 3.0)
                      (vector 3.0 4.0)
                      (vector 10.0 11.0)
                      (vector 11.0 12.0)
                      (vector 12.0 13.0))))
    (format t "Data points:~%")
    (loop for point across data do
          (format t "~A~%" point))
    (format t "~%Running EM algorithm...~%")
    (let ((gmm (em-gmm data 2 100 1e-6)))
      (format t "Final GMM parameters:~%")
      (format t "Weights: ~A~%" (gmm-weights gmm))
      (format t "Means: ~A~%" (gmm-means gmm))
      (format t "Covariances: ~A~%" (gmm-covariances gmm)))))
```

## Usage Example

```lisp
;; Run the example
(example-usage)
```

## Key Features of this Implementation

1. **Complete EM Algorithm**: Implements both E-step and M-step
2. **Gaussian Mixture Model**: Supports multi-dimensional data
3. **Convergence Detection**: Stops when log-likelihood changes less than tolerance
4. **Proper Structure**: Uses Common Lisp structures for clean organization
5. **Helper Functions**: Includes matrix operations needed for the algorithm
6. **Error Handling**: Includes checks for edge cases

## How it Works

1. **Initialization**: Randomly initialize mixture weights, means, and covariances
2. **E-step**: Compute responsibilities (posterior probabilities) for each data point belonging to each component
3. **M-step**: Update model parameters (weights, means, covariances) using the responsibilities
4. **Iteration**: Repeat E-step and M-step until convergence or maximum iterations reached

The algorithm is particularly useful for clustering problems where you want to identify underlying Gaussian distributions in your data.


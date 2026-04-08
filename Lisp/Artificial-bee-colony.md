# Artificial Bee Colony Algorithm in Lisp

Here's an implementation of the Artificial Bee Colony (ABC) algorithm in Common Lisp:

```lisp
(defpackage :abc-algorithm
  (:use :cl)
  (:export :abc-optimize :create-employee-bee :create-onlooker-bee
           :create-scout-bee :evaluate-fitness :generate-random-solution))

(in-package :abc-algorithm)

;; Define a bee structure
(defstruct bee
  solution
  fitness
  trial-counter
  age)

;; Objective function to minimize (example: sphere function)
(defun objective-function (solution)
  "Sphere function: sum of squares of all elements"
  (reduce #'+ (mapcar (lambda (x) (* x x)) solution)))

;; Evaluate fitness of a solution
(defun evaluate-fitness (solution)
  "Convert objective function value to fitness (higher is better)"
  (if (zerop (objective-function solution))
      most-positive-fixnum
      (/ 1.0 (+ 1.0 (objective-function solution)))))

;; Generate random solution within bounds
(defun generate-random-solution (dimensions lower-bound upper-bound)
  "Generate a random solution vector"
  (loop for i from 1 to dimensions
        collect (+ lower-bound (random (- upper-bound lower-bound)))))

;; Create an employee bee
(defun create-employee-bee (dimensions lower-bound upper-bound)
  "Create an employee bee with random solution"
  (let ((solution (generate-random-solution dimensions lower-bound upper-bound)))
    (make-bee :solution solution
              :fitness (evaluate-fitness solution)
              :trial-counter 0
              :age 0)))

;; Create an onlooker bee
(defun create-onlooker-bee (dimensions lower-bound upper-bound)
  "Create an onlooker bee with random solution"
  (let ((solution (generate-random-solution dimensions lower-bound upper-bound)))
    (make-bee :solution solution
              :fitness (evaluate-fitness solution)
              :trial-counter 0
              :age 0)))

;; Create a scout bee
(defun create-scout-bee (dimensions lower-bound upper-bound)
  "Create a scout bee with random solution"
  (let ((solution (generate-random-solution dimensions lower-bound upper-bound)))
    (make-bee :solution solution
              :fitness (evaluate-fitness solution)
              :trial-counter 0
              :age 0)))

;; Generate neighbor solution using random selection
(defun generate-neighbor-solution (current-solution bee-index dimensions lower-bound upper-bound)
  "Generate a neighbor solution using the ABC algorithm"
  (let* ((random-index (random dimensions))
         (random-bee (random (length current-solution)))
         (step-size 0.5)) ; Step size parameter
    (loop for i from 0 to (1- dimensions)
          for new-value = (if (= i random-index)
                              (+ (aref current-solution i)
                                 (* step-size (- (aref current-solution random-bee)
                                                (aref current-solution i))))
                              (aref current-solution i))
          collect new-value)))

;; Employed bee phase
(defun employed-bee-phase (bees dimensions lower-bound upper-bound max-trials)
  "Employed bee phase - bees search for better solutions"
  (loop for bee in bees
        for i from 0
        do (let* ((current-solution (bee-solution bee))
                  (neighbor-solution (generate-neighbor-solution 
                                     current-solution i dimensions lower-bound upper-bound))
                  (current-fitness (bee-fitness bee))
                  (neighbor-fitness (evaluate-fitness neighbor-solution)))
             (if (> neighbor-fitness current-fitness)
                 (progn
                   (setf (bee-solution bee) neighbor-solution
                         (bee-fitness bee) neighbor-fitness
                         (bee-trial-counter bee) 0)
                   (format t "Improved solution found! Fitness: ~A~%" neighbor-fitness))
                 (incf (bee-trial-counter bee))
                 (when (> (bee-trial-counter bee) max-trials)
                   (setf (bee-solution bee) (generate-random-solution dimensions lower-bound upper-bound)
                         (bee-fitness bee) (evaluate-fitness (bee-solution bee))
                         (bee-trial-counter bee) 0)))))
  bees)

;; Onlooker bee phase
(defun onlooker-bee-phase (bees dimensions lower-bound upper-bound)
  "Onlooker bee phase - bees probabilistically select solutions"
  (let* ((total-fitness (reduce #'+ bees 
                               :key #'bee-fitness 
                               :initial-value 0.0))
         (num-bees (length bees)))
    (loop for i from 0 to (1- num-bees)
          for bee = (nth i bees)
          for probability = (/ (bee-fitness bee) total-fitness)
          when (> (random 1.0) probability)
          do (let* ((current-solution (bee-solution bee))
                    (neighbor-solution (generate-neighbor-solution 
                                       current-solution i dimensions lower-bound upper-bound))
                    (current-fitness (bee-fitness bee))
                    (neighbor-fitness (evaluate-fitness neighbor-solution)))
               (if (> neighbor-fitness current-fitness)
                   (progn
                     (setf (bee-solution bee) neighbor-solution
                           (bee-fitness bee) neighbor-fitness
                           (bee-trial-counter bee) 0))
                   (incf (bee-trial-counter bee)))))
    bees))

;; Scout bee phase
(defun scout-bee-phase (bees dimensions lower-bound upper-bound max-trials)
  "Scout bee phase - abandoned solutions are replaced"
  (loop for bee in bees
        do (when (> (bee-trial-counter bee) max-trials)
             (setf (bee-solution bee) (generate-random-solution dimensions lower-bound upper-bound)
                   (bee-fitness bee) (evaluate-fitness (bee-solution bee))
                   (bee-trial-counter bee) 0)))
  bees)

;; Main ABC algorithm function
(defun abc-optimize (dimensions lower-bound upper-bound population-size iterations max-trials)
  "Main ABC optimization function"
  (let* ((bees (loop for i from 1 to population-size
                     collect (create-employee-bee dimensions lower-bound upper-bound)))
         (best-solution (first bees))
         (best-fitness (bee-fitness best-solution)))
    (format t "Starting ABC optimization...~%")
    
    (loop for i from 1 to iterations
          do (progn
               ;; Employed bee phase
               (setf bees (employed-bee-phase bees dimensions lower-bound upper-bound max-trials))
               
               ;; Onlooker bee phase
               (setf bees (onlooker-bee-phase bees dimensions lower-bound upper-bound))
               
               ;; Scout bee phase
               (setf bees (scout-bee-phase bees dimensions lower-bound upper-bound max-trials))
               
               ;; Track best solution
               (let ((current-best (reduce (lambda (a b) 
                                            (if (> (bee-fitness a) (bee-fitness b)) a b))
                                         bees)))
                 (when (> (bee-fitness current-best) best-fitness)
                   (setf best-solution current-best
                         best-fitness (bee-fitness current-best))
                   (format t "Iteration ~A: New best fitness = ~A~%" i best-fitness)))))
    
    (format t "Optimization completed!~%")
    (format t "Best solution: ~A~%" (bee-solution best-solution))
    (format t "Best fitness: ~A~%" best-fitness)
    (format t "Best objective value: ~A~%" (objective-function (bee-solution best-solution)))
    best-solution))

;; Example usage
(defun run-example ()
  "Run a simple example of ABC optimization"
  (format t "Running Artificial Bee Colony Algorithm Example~%")
  (format t "Optimizing sphere function in 5 dimensions~%")
  (abc-optimize 5 -10.0 10.0 20 100 10))

;; Run the example
(run-example)
```

## Key Features of this Implementation:

1. **Bee Structure**: Each bee has a solution, fitness value, trial counter, and age
2. **Three Bee Types**: Employee bees, Onlooker bees, and Scout bees
3. **Four Phases**: 
   - Employed bee phase
   - Onlooker bee phase  
   - Scout bee phase
   - Solution evaluation
4. **Objective Function**: Sphere function (sum of squares)
5. **Parameters**: Dimensions, bounds, population size, iterations, max trials

## Usage Example:

```lisp
;; Optimize a 5-dimensional sphere function
(abc-optimize 5 -10.0 10.0 20 100 10)
```

This implementation demonstrates the core concepts of the Artificial Bee Colony algorithm while maintaining the functional programming style typical of Lisp. The algorithm searches for the minimum of the objective function through the collaborative behavior of the bee colony.


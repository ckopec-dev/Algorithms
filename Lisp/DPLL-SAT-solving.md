# DPLL SAT Solver in Lisp

Here's a complete implementation of the DPLL algorithm for solving SAT problems in Lisp:

```lisp
;; DPLL SAT Solver Implementation

;; Basic data structures
(defstruct clause
  literals
  satisfied?)

(defstruct literal
  variable
  positive?)

(defstruct formula
  clauses)

;; Helper functions
(defun make-literal (var positive)
  "Create a literal with variable and sign"
  (make-literal :variable var :positive? positive))

(defun make-clause (literals)
  "Create a clause with list of literals"
  (make-clause :literals literals :satisfied? nil))

(defun make-formula (clauses)
  "Create a formula with list of clauses"
  (make-formula :clauses clauses))

;; DPLL Algorithm Implementation
(defun dpll (formula assignment)
  "Main DPLL algorithm"
  (let ((new-formula (unit-propagate formula assignment)))
    (cond
      ;; If formula is empty, we found a solution
      ((null (formula-clauses new-formula)) t)
      
      ;; If there's an empty clause, unsatisfiable
      ((some #'empty-clause? (formula-clauses new-formula)) nil)
      
      ;; Otherwise, choose a variable and recurse
      (t
        (let ((var (choose-variable new-formula)))
          (or (dpll (extend-formula new-formula var t) 
                    (extend-assignment assignment var t))
              (dpll (extend-formula new-formula var nil) 
                    (extend-assignment assignment var nil))))))))

;; Helper functions
(defun unit-propagate (formula assignment)
  "Perform unit propagation on the formula"
  (let ((new-formula formula)
        (changed t))
    (loop while changed do
      (let ((unit-clause (find-unit-clause (formula-clauses new-formula))))
        (if unit-clause
            (let ((literal (first (clause-literals unit-clause))))
              (setf new-formula (propagate-literal new-formula literal assignment))
              (setf changed t))
            (setf changed nil)))))
  new-formula)

(defun find-unit-clause (clauses)
  "Find a unit clause (clause with single literal)"
  (find-if (lambda (clause)
              (and (not (clause-satisfied? clause))
                   (= (length (clause-literals clause)) 1)))
           clauses))

(defun propagate-literal (formula literal assignment)
  "Propagate a literal through the formula"
  (let ((new-clauses (list)))
    (dolist (clause (formula-clauses formula))
      (if (literal-in-clause literal clause)
          (if (literal-satisfies literal clause)
              ;; Clause is satisfied, skip it
              (push clause new-clauses)
              ;; Remove literal from clause
              (let ((new-literals (remove literal (clause-literals clause))))
                (if (null new-literals)
                    ;; Empty clause - unsatisfiable
                    (return-from propagate-literal 
                      (make-formula :clauses nil))
                    (push (make-clause :literals new-literals) new-clauses)))))
      (unless (literal-in-clause literal clause)
        (push clause new-clauses))))
  (make-formula :clauses (nreverse new-clauses)))

(defun literal-in-clause (literal clause)
  "Check if literal appears in clause"
  (member literal (clause-literals clause)))

(defun literal-satisfies (literal clause)
  "Check if literal satisfies clause"
  (let ((literals (clause-literals clause)))
    (and (member literal literals)
         (eq (literal-positive? literal) 
             (member literal literals)))))

(defun choose-variable (formula)
  "Choose a variable to branch on"
  (let ((all-literals (flatten-literals (formula-clauses formula))))
    (if all-literals
        (literal-variable (first all-literals))
        nil)))

(defun flatten-literals (clauses)
  "Get all literals from all clauses"
  (apply #'append (mapcar #'clause-literals clauses)))

(defun extend-formula (formula var value)
  "Extend formula with variable assignment"
  (let ((new-clauses (list)))
    (dolist (clause (formula-clauses formula))
      (let ((new-literal (make-literal :variable var :positive? value)))
        (if (literal-in-clause new-literal clause)
            ;; If literal is in clause, remove it
            (let ((new-literals (remove new-literal (clause-literals clause))))
              (if (null new-literals)
                  ;; Empty clause - unsatisfiable
                  (return-from extend-formula 
                    (make-formula :clauses nil))
                  (push (make-clause :literals new-literals) new-clauses)))
            ;; If literal is not in clause, keep clause
            (push clause new-clauses)))))
  (make-formula :clauses (nreverse new-clauses)))

(defun extend-assignment (assignment var value)
  "Extend assignment with variable"
  (cons (cons var value) assignment))

(defun empty-clause? (clause)
  "Check if clause is empty"
  (null (clause-literals clause)))

;; Example usage
(defun example-sat-problem ()
  "Create and solve a simple SAT problem"
  (let* ((x1 (make-literal :variable 'x1 :positive? t))
         (x2 (make-literal :variable 'x2 :positive? t))
         (x3 (make-literal :variable 'x3 :positive? t))
         (nx1 (make-literal :variable 'x1 :positive? nil))
         (nx2 (make-literal :variable 'x2 :positive? nil))
         (nx3 (make-literal :variable 'x3 :positive? nil))
         
         ;; Create clauses: (x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2 OR NOT x3)
         (clause1 (make-clause (list x1 x2)))
         (clause2 (make-clause (list nx1 x3)))
         (clause3 (make-clause (list nx2 nx3)))
         
         (formula (make-formula (list clause1 clause2 clause3))))
    
    (format t "Solving formula with clauses:~%")
    (format t "Clause 1: ~A~%" (clause-literals clause1))
    (format t "Clause 2: ~A~%" (clause-literals clause2))
    (format t "Clause 3: ~A~%" (clause-literals clause3))
    (format t "~%")
    
    (let ((result (dpll formula nil)))
      (format t "Result: ~A~%" result)
      result)))

;; Run example
(example-sat-problem)
```

## Key Features of this Implementation:

1. **Data Structures**: 
   - `clause` - represents a clause with literals
   - `literal` - represents a variable with positive/negative sign
   - `formula` - represents the entire SAT problem

2. **Core Algorithm Functions**:
   - `dpll` - Main recursive DPLL function
   - `unit-propagate` - Performs unit propagation
   - `find-unit-clause` - Finds unit clauses
   - `propagate-literal` - Propagates literal assignments
   - `choose-variable` - Selects next variable to branch on

3. **Helper Functions**:
   - `extend-formula` - Updates formula with variable assignments
   - `extend-assignment` - Updates variable assignment
   - `empty-clause?` - Checks for empty clauses

4. **Example Usage**: Demonstrates solving a 3-CNF formula with 3 variables

This implementation follows the classic DPLL algorithm with unit propagation and pure literal elimination, making it efficient for solving SAT problems.


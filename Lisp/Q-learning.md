# Q-Learning Algorithm in Lisp

Here's an implementation of a Q-learning algorithm in Common Lisp:

```lisp
(defclass q-learner ()
  ((q-table :initarg :q-table :accessor q-table)
   (learning-rate :initarg :learning-rate :accessor learning-rate)
   (discount-factor :initarg :discount-factor :accessor discount-factor)
   (epsilon :initarg :epsilon :accessor epsilon)
   (actions :initarg :actions :accessor actions)))

(defun make-q-learner (actions learning-rate discount-factor epsilon)
  "Create a new Q-learner with given parameters"
  (make-instance 'q-learner
                :q-table (make-hash-table :test 'equal)
                :actions actions
                :learning-rate learning-rate
                :discount-factor discount-factor
                :epsilon epsilon))

(defun get-q-value (learner state action)
  "Get the Q-value for a state-action pair"
  (let ((state-table (gethash state (q-table learner))))
    (if state-table
        (gethash action state-table)
        0.0)))

(defun set-q-value (learner state action value)
  "Set the Q-value for a state-action pair"
  (let ((state-table (gethash state (q-table learner))))
    (if (null state-table)
        (setf (gethash state (q-table learner))
              (make-hash-table :test 'equal))
        (setf (gethash action state-table) value))))

(defun max-q-value (learner state)
  "Get the maximum Q-value for a given state"
  (let ((max-value 0.0)
        (actions (actions learner)))
    (dolist (action actions)
      (let ((q-value (get-q-value learner state action)))
        (when (> q-value max-value)
          (setf max-value q-value))))
    max-value))

(defun choose-action (learner state)
  "Choose an action using epsilon-greedy policy"
  (if (and (random 1.0) (< (random 1.0) (epsilon learner)))
      ;; Explore: choose random action
      (let ((actions (actions learner)))
        (nth (random (length actions)) actions))
      ;; Exploit: choose best action
      (let ((best-action nil)
            (best-value -1e10))
        (dolist (action (actions learner))
          (let ((q-value (get-q-value learner state action)))
            (when (> q-value best-value)
              (setf best-value q-value
                    best-action action))))
        best-action)))

(defun update-q-value (learner state action reward next-state)
  "Update Q-value using Q-learning update rule"
  (let* ((current-q (get-q-value learner state action))
         (max-next-q (max-q-value learner next-state))
         (new-q (+ current-q
                   (* (learning-rate learner)
                      (- (+ reward
                            (* (discount-factor learner)
                               max-next-q))
                         current-q)))))
    (set-q-value learner state action new-q)))

(defun q-learning-step (learner state action reward next-state)
  "Perform one step of Q-learning update"
  (update-q-value learner state action reward next-state))

(defun q-learning-episode (learner initial-state reward-function transition-function)
  "Run one episode of Q-learning"
  (let ((current-state initial-state)
        (total-reward 0.0))
    (loop while (not (funcall reward-function current-state))
          do (let ((action (choose-action learner current-state))
                   (next-state (funcall transition-function current-state action)))
               (let ((reward (funcall reward-function current-state action)))
                 (setf total-reward (+ total-reward reward))
                 (q-learning-step learner current-state action reward next-state)
                 (setf current-state next-state))))
    total-reward))

;; Example usage:
(defun example-q-learning ()
  "Example of using Q-learning"
  (let* ((actions '(up down left right))
         (learner (make-q-learner actions 0.1 0.9 0.1))
         (initial-state 's1))
    
    ;; Simple reward function (returns nil when terminal state)
    (defun simple-reward (state &optional action)
      (if (equal state 'terminal)
          nil
          -1.0))
    
    ;; Simple transition function
    (defun simple-transition (state action)
      (case state
        ('s1 (case action
               (up 's2)
               (left 's3)
               (otherwise 's1)))
        ('s2 (case action
               (down 's1)
               (right 's4)
               (otherwise 's2)))
        ('s3 (case action
               (right 's1)
               (up 's4)
               (otherwise 's3)))
        ('s4 (case action
               (down 's3)
               (left 's2)
               (otherwise 's4)))
        (otherwise 's1)))
    
    ;; Run several episodes
    (loop for i from 1 to 100
          do (q-learning-episode learner initial-state
                                #'simple-reward
                                #'simple-transition))
    
    ;; Print some Q-values
    (format t "Q-values for state s1:~%")
    (dolist (action actions)
      (format t "  Action ~A: ~A~%" action (get-q-value learner 's1 action)))
    
    learner))
```

## Key Components:

1. **Q-Learner Class**: Stores Q-table, learning parameters, and actions
2. **Q-Table**: Hash table storing state-action pairs and their Q-values
3. **Epsilon-Greedy Policy**: Balances exploration and exploitation
4. **Q-Value Update**: Implements the Bellman equation update rule
5. **Episode Execution**: Runs one complete learning episode

## Usage:
The example demonstrates a simple grid world where the agent learns to navigate from state `s1` to a terminal state, learning optimal actions through trial and error.

This implementation shows how Q-learning can be implemented in Lisp with proper object-oriented design and functional programming concepts.


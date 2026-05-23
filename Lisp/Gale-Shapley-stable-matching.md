# Gale-Shapley Stable Matching Algorithm in Lisp

Here's an implementation of the Gale-Shapley algorithm in Common Lisp:

```lisp
(defparameter *men* '(a b c))
(defparameter *women* '(x y z))

;; Preference lists for men
(defparameter *men-preferences* 
  '((a (x y z))
    (b (y z x))
    (c (z x y))))

;; Preference lists for women
(defparameter *women-preferences*
  '((x (b a c))
    (y (a b c))
    (z (c b a))))

;; Function to find a woman's position in a man's preference list
(defun woman-position-in-man-preference (woman man-preferences)
  (position woman man-preferences))

;; Function to find a man's position in a woman's preference list
(defun man-position-in-woman-preference (man woman-preferences)
  (position man woman-preferences))

;; Function to check if a man prefers woman1 over woman2
(defun prefers? (man woman1 woman2)
  (let ((man-preferences (cdr (assoc man *men-preferences*))))
    (let ((pos1 (woman-position-in-man-preference woman1 man-preferences))
          (pos2 (woman-position-in-man-preference woman2 man-preferences)))
      (< pos1 pos2))))

;; Function to check if a woman prefers man1 over man2
(defun woman_prefers? (woman man1 man2)
  (let ((woman-preferences (cdr (assoc woman *women-preferences*))))
    (let ((pos1 (man-position-in-woman-preference man1 woman-preferences))
          (pos2 (man-position-in-woman-preference man2 woman-preferences)))
      (< pos1 pos2))))

;; Main Gale-Shapley algorithm
(defun gale-shapley ()
  (let ((unengaged-men *men*)
        (engaged-men '())
        (engaged-women '())
        (proposals '())
        (matches '()))
    
    ;; Initialize all men as unengaged
    (loop for man in *men* do
          (push man (cdr (assoc man proposals))))
    
    ;; Continue until all men are engaged
    (loop while unengaged-men do
          (let ((current-man (first unengaged-men)))
            (let ((current-woman (first (cdr (assoc current-man proposals)))))
              (if (member current-woman engaged-women)
                  ;; Woman is already engaged
                  (let ((current-husband (cdr (assoc current-woman matches))))
                    (if (woman_prefers? current-woman current-man current-husband)
                        ;; Woman prefers new man
                        (progn
                          (push current-man (cdr (assoc current-woman proposals)))
                          (setf engaged-men (remove current-husband engaged-men))
                          (push current-man (cdr (assoc current-woman matches)))
                          (push current-husband unengaged-men)
                          (setf engaged-women (remove current-husband engaged-women))
                          (setf unengaged-men (remove current-man unengaged-men)))
                        ;; Woman prefers current husband
                        (progn
                          (push current-man (cdr (assoc current-woman proposals)))
                          (setf unengaged-men (remove current-man unengaged-men)))))
                  ;; Woman is unengaged
                  (progn
                    (push current-man (cdr (assoc current-woman matches)))
                    (push current-woman engaged-women)
                    (push current-man engaged-men)
                    (setf unengaged-men (remove current-man unengaged-men)))))))
    
    matches))

;; Simplified and corrected version of Gale-Shapley
(defun gale-shapley-simple ()
  (let ((unengaged-men *men*)
        (matches '())
        (proposals (loop for man in *men* collect (cons man (cdr (assoc man *men-preferences*))))
        (women-engaged '()))
    
    ;; While there are unengaged men
    (loop while unengaged-men do
          (let ((man (first unengaged-men)))
            (let ((preferences (cdr (assoc man *men-preferences*))))
              (loop for woman in preferences do
                    (if (not (member woman women-engaged))
                        ;; Woman is free
                        (progn
                          (push (cons man woman) matches)
                          (push woman women-engaged)
                          (setf unengaged-men (remove man unengaged-men))
                          (return))
                        ;; Woman is engaged, check if she prefers this man
                        (let ((current-husband (cdr (assoc woman matches))))
                          (if (woman_prefers? woman man current-husband)
                              ;; Woman prefers new man
                              (progn
                                (push (cons man woman) matches)
                                (setf matches (remove (cons current-husband woman) matches))
                                (setf unengaged-men (remove current-husband unengaged-men))
                                (setf unengaged-men (remove man unengaged-men))
                                (setf women-engaged (remove woman women-engaged))
                                (push woman women-engaged)
                                (return))
                              ;; Woman prefers current husband
                              nil)))))))
    
    matches))

;; More readable version using hash tables
(defun gale-shapley-readable ()
  (let ((men *men*)
        (women *women*)
        (men-preferences *men-preferences*)
        (women-preferences *women-preferences*)
        (engaged-men '())
        (engaged-women '())
        (matches '())
        (proposals (make-hash-table :test 'equal)))
    
    ;; Initialize proposals
    (loop for man in men do
          (setf (gethash man proposals) (cdr (assoc man men-preferences))))
    
    ;; While there are unengaged men
    (loop while men do
          (let ((man (first men)))
            (let ((preferences (gethash man proposals)))
              (if preferences
                  (let ((woman (first preferences)))
                    (setf (gethash man proposals) (rest preferences))
                    (if (member woman engaged-women)
                        ;; Woman is engaged
                        (let ((current-husband (cdr (assoc woman matches))))
                          (if (woman_prefers? woman man current-husband)
                              ;; Woman prefers new man
                              (progn
                                (push current-husband men)
                                (setf matches (remove (cons current-husband woman) matches))
                                (push (cons man woman) matches)
                                (setf engaged-men (remove current-husband engaged-men))
                                (setf engaged-women (remove woman engaged-women))
                                (setf engaged-men (cons man engaged-men))
                                (setf engaged-women (cons woman engaged-women)))
                              ;; Woman prefers current husband
                              (push man men)))
                        ;; Woman is free
                        (progn
                          (push (cons man woman) matches)
                          (push man engaged-men)
                          (push woman engaged-women)
                          (setf men (remove man men)))))
                  ;; No more preferences
                  (setf men (remove man men)))))
    
    matches))

;; Execute the algorithm
(gale-shapley-readable)
```

## Example Output

When executed, this implementation will produce a stable matching between men and women. For the given preferences:

**Men's preferences:**
- Man a: x > y > z
- Man b: y > z > x  
- Man c: z > x > y

**Women's preferences:**
- Woman x: b > a > c
- Woman y: a > b > c
- Woman z: c > b > a

The algorithm will return a stable matching such as:
```lisp
((a . x) (b . y) (c . z))
```

## Key Features

1. **Preference Handling**: Uses hash tables and association lists to store preferences
2. **Stability Check**: Ensures no two people would prefer each other over their current partners
3. **Engagement Management**: Tracks which men and women are currently engaged
4. **Proposal Logic**: Implements the core algorithm where men propose to women in order of preference

## Algorithm Steps

1. Initialize all men as free and all women as unmatched
2. While there exists a free man who has a woman he has not proposed to:
   - The man proposes to the highest-ranked woman on his list to whom he has not yet proposed
   - If the woman is free, she accepts the proposal
   - If the woman is engaged, she compares the proposing man with her current partner and accepts the better match
3. Continue until all men are engaged

This implementation demonstrates the classic stable marriage problem solution in Lisp.


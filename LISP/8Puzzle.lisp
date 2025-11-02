;;; eightpuzzle.lisp
;;; 8-puzzle A* solver (Common Lisp)
;;; Representation: state is a list of 9 integers (0 = blank).
;;; Goal: (1 2 3 4 5 6 7 8 0)

(in-package :cl-user)

(defparameter goal-state '(1 2 3 4 5 6 7 8 0))

(defun index->rowcol (idx)
  "Return (row col) for index 0..8"
  (values (truncate idx 3) (mod idx 3)))

(defun manhattan (state)
  "Manhattan distance heuristic for a state (list of 9)."
  (let ((sum 0))
    (dotimes (i 9 sum)
      (let ((tile (nth i state)))
        (when (/= tile 0)
          (multiple-value-bind (r1 c1) (index->rowcol i)
            (let ((goal-pos (1- tile)))  ; tile 1..8 -> goal index 0..7
              (multiple-value-bind (r2 c2) (index->rowcol goal-pos)
                (incf sum (+ (abs (- r1 r2)) (abs (- c1 c2))))))))))))

(defun swap-at (state i j)
  "Return new state with indices i and j swapped."
  (let ((v (coerce state 'vector)))
    (rotatef (aref v i) (aref v j))
    (coerce v 'list)))

(defun neighbors (state)
  "Return list of neighbor states (one move)."
  (let* ((zero-idx (position 0 state :test #'=))
         (row (truncate zero-idx 3))
         (col (mod zero-idx 3))
         (moves '()))
    (when (> row 0) (push (swap-at state zero-idx (- zero-idx 3)) moves)) ; up
    (when (< row 2) (push (swap-at state zero-idx (+ zero-idx 3)) moves)) ; down
    (when (> col 0) (push (swap-at state zero-idx (- zero-idx 1)) moves)) ; left
    (when (< col 2) (push (swap-at state zero-idx (+ zero-idx 1)) moves)) ; right
    moves))

(defun gethash-default (key table &optional (default most-positive-fixnum))
  "Helper: gethash with default numeric return."
  (multiple-value-bind (v found) (gethash key table)
    (if found v default)))

(defun argmin-by-f (list-states f-table)
  "Return state from list-states with smallest f-score stored in f-table."
  (reduce (lambda (a b)
            (let ((fa (gethash-default a f-table))
                  (fb (gethash-default b f-table)))
              (if (< fa fb) a b)))
          list-states))

(defun reconstruct-path (came-from current)
  "Reconstruct path by following came-from hash-table until nil."
  (let ((path '()))
    (loop
      (push current path)
      (multiple-value-bind (prev found) (gethash current came-from)
        (if (not found)
            (return path)
            (setf current prev))))))

(defun remove-from-list (item list &key (test #'equal))
  (remove item list :test test))

(defun inversion-count (state)
  "Count number of inversions ignoring 0."
  (let ((arr (remove 0 state)))
    (loop for i from 0 below (length arr)
          sum (loop for j from (1+ i) below (length arr)
                    count (> (nth i arr) (nth j arr))))))

(defun solvable-p (state)
  "Return T if puzzle is solvable."
  (evenp (inversion-count state)))

(defun a-star (start)
  "A* search from start state to goal-state.
Returns list of states from start to goal, or NIL if unsolvable/failed."
  (if (not (solvable-p start))
      (progn
        (format t "~%This puzzle configuration is unsolvable.~%")
        nil)
      (let ((open (list start))
            (came-from (make-hash-table :test #'equal))
            (g-score (make-hash-table :test #'equal))
            (f-score (make-hash-table :test #'equal))
            (closed (make-hash-table :test #'equal)))
        (setf (gethash start g-score) 0)
        (setf (gethash start f-score) (manhattan start))
        (loop while open do
          (let ((current (argmin-by-f open f-score)))
            (when (equal current goal-state)
              (return-from a-star (reconstruct-path came-from current)))
            (setf open (remove-from-list current open :test #'equal))
            (setf (gethash current closed) t)
            (dolist (nbr (neighbors current))
              (when (not (gethash nbr closed))
                (let* ((tentative-g (+ 1 (gethash-default current g-score)))
                       (old-g (gethash-default nbr g-score)))
                  (when (< tentative-g old-g)
                    (setf (gethash nbr came-from) current)
                    (setf (gethash nbr g-score) tentative-g)
                    (setf (gethash nbr f-score) (+ tentative-g (manhattan nbr)))
                    (unless (find nbr open :test #'equal)
                      (push nbr open)))))))))))

;;; Utility: pretty-print a state as 3x3
(defun print-state (state)
  (format t "-----------~%")
  (dotimes (r 3)
    (dotimes (c 3)
      (let ((v (nth (+ (* r 3) c) state)))
        (format t "~3a" (if (= v 0) "_" v))))
    (terpri))
  (format t "-----------~%"))

;;; Example usage
(defun solve-and-print (start)
  (format t "~%Start:~%")
  (print-state start)
  (let ((path (a-star start)))
    (if (null path)
        (format t "No solution found.~%")
        (progn
          (format t "Solution found in ~d moves.~%" (1- (length path)))
          (loop for s in path do (print-state s))))))

;;; Example initial states
(defparameter example-1 '(1 2 3 4 5 6 7 8 0)) ; already solved
(defparameter example-2 '(1 2 3 4 5 6 0 7 8)) ; two moves
(defparameter example-3 '(7 2 4 5 0 6 8 3 1)) ; harder

;;; Run one example when file executed directly
(when (and (ignore-errors (sb-ext:posix-getenv "RUN_EIGHT_PUZZLE")) t)
  (solve-and-print example-3))

;;; If loaded in REPL, call manually:
;;; (solve-and-print example-3)

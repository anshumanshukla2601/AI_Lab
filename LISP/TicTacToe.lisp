(defun make-board ()
  (make-array 9 :initial-element nil))

(defun print-board (board)
  (dotimes (i 9)
    (format t "~A " (or (aref board i) "-"))
    (when (= (mod (1+ i) 3) 0) (terpri)))
  (force-output))

(defun winner (board)
  (let ((lines '((0 1 2) (3 4 5) (6 7 8)
                 (0 3 6) (1 4 7) (2 5 8)
                 (0 4 8) (2 4 6))))
    (dolist (line lines)
      (let ((a (aref board (first line)))
            (b (aref board (second line)))
            (c (aref board (third line))))
        (when (and a (equal a b) (equal b c))
          (return a))))))

(defun full-board-p (board)
  (notany #'null board))

;; minimax: is-maximizing => AI's turn (O). depth used to prefer faster wins.
(defun minimax (board depth is-maximizing)
  (let ((win (winner board)))
    (cond
      ;; O (AI) wins -> positive score, prefer faster wins
      ((equal win 'O) (- 10 depth))
      ;; X (human) wins -> negative score, prefer slower losses
      ((equal win 'X) (- depth 10))
      ((full-board-p board) 0)
      (t
       (if is-maximizing
           (let ((best -10000))
             (dotimes (i 9 best)
               (when (null (aref board i))
                 (setf (aref board i) 'O)
                 (setf best (max best (minimax board (1+ depth) nil)))
                 (setf (aref board i) nil))))
           (let ((best 10000))
             (dotimes (i 9 best)
               (when (null (aref board i))
                 (setf (aref board i) 'X)
                 (setf best (min best (minimax board (1+ depth) t)))
                 (setf (aref board i) nil)))))))))

;; return -1 if no move found; caller must guard before writing to board
(defun best-move (board)
  (let ((best -10000) (move -1))
    (dotimes (i 9 move)
      (when (null (aref board i))
        (setf (aref board i) 'O)
        ;; after placing O, it's human's turn -> minimizer (is-maximizing = NIL)
        (let ((score (minimax board 0 nil)))
          (when (> score best)
            (setf best score)
            (setf move i)))
        (setf (aref board i) nil)))))

(defun valid-int-input-p (s)
  "Return parsed integer 1..9 or NIL."
  (when s
    (let ((n (ignore-errors (parse-integer s :junk-allowed t))))
      (and n (<= 1 n 9) n))))

(defun play ()
  (let ((board (make-board)))
    (loop
      (print-board board)
      (when (winner board)
        (format t "~A wins!~%" (winner board))
        (return))
      (when (full-board-p board)
        (format t "Draw!~%")
        (return))
      ;; User input 1-9
      (format t "Enter your move (1-9): ")
      (force-output)
      (let* ((line (read-line))
             (move (valid-int-input-p line)))
        (when move
          (let ((idx (1- move)))
            (when (null (aref board idx))
              (setf (aref board idx) 'X)))))
      ;; check for user's win/draw immediately
      (when (or (winner board) (full-board-p board))
        (print-board board)
        (if (winner board)
            (format t "~A wins!~%" (winner board))
            (format t "Draw!~%"))
        (return))
      ;; Computer move: choose best and guard
      (let ((comp-move (best-move board)))
        (if (and (integerp comp-move) (<= 0 comp-move 8) (null (aref board comp-move)))
            (progn
              (setf (aref board comp-move) 'O)
              ;; optional: print a message
              (format t "Computer plays at ~D~%" (1+ comp-move)))
            ;; fallback: no valid move (shouldn't happen), treat as draw/stop
            (progn
              (format t "No valid computer move found, terminating.~%")
              (return)))))))

;; start game
(play)

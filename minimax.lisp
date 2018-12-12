;; minimax state depth currentMove
(load "helpers.lisp")

(defvar *grafTest* '(
            ("A" ( (0 "X") (1 "X") (2 "-") (3 "-"))) 
         ("B" ( (0 "-") (1 "X") (2 "-") (3 "-") (4 "-"))) 
      ("C" ( (0 "X") (1 "-") (2 "-") (3 "X") (4 "X") (5 "-"))) 
    ("D" ( (0 "-") (1 "-") (2 "-") (3 "-") (4 "-") (5 "-") (6 "-"))) 
      ("E" ( (1 "-") (2 "-") (3 "-") (4 "-") (5 "-") (6 "-"))) 
         ("F" ( (2 "-") (3 "-") (4 "-") (5 "-") (6 "-"))) 
            ("G" ( (3 "-") (4 "-") (5 "-") (6 "-"))) 
))

(defun minimax (state depth currentMove)
    (let ((movesList (returnPossibleStates state))(comparator (if (string= "X" currentMove) '> '<)))
         (cond ((or (zerop depth) (null movesList)) (list state (getAssessment state)))
                (t (max-min-state (list (mapcar (lambda (x) (minimax x (1- depth) (if (string= currentMove "X") "O"  "X"))) movesList)) comparator)))))

;; determines how good is the state
(defun getAssessment(state)
    (random (list-length state)))

(defun max-min-state (lsv comparator)
    (max-min-state-i (cdr lsv) (car lsv) comparator)) 

(defun max-min-state-i (lsv state-value comparator)
    (cond ((null lsv) state-value)
        ((funcall comparator (cadar lsv) (cadr state-value))
            (max-min-state-i (cdr lsv) (car lsv)))
        (t (max-min-state-i (cdr lsv) state-value))))


(print (returnPossibleStates *grafTest*))
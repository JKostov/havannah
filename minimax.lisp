;; minimax state depth currentMove

(defvar *grafTest* '(
            ("A" ( (0 "X") (1 "X") (2 "-") (3 "-"))) 
         ("B" ( (0 "-") (1 "X") (2 "-") (3 "-") (4 "-"))) 
      ("C" ( (0 "X") (1 "-") (2 "-") (3 "X") (4 "X") (5 "-"))) 
    ("D" ( (0 "-") (1 "-") (2 "-") (3 "-") (4 "-") (5 "-") (6 "-"))) 
      ("E" ( (1 "-") (2 "-") (3 "-") (4 "-") (5 "-") (6 "-"))) 
         ("F" ( (2 "-") (3 "-") (4 "-") (5 "-") (6 "-"))) 
            ("G" ( (3 "-") (4 "-") (5 "-") (6 "-"))) 
))

(setf *random-state* (make-random-state t)) ;Used in random function to generate random number

;; determines how good is the state
(defun getAssessment(state)
    (random 10)
)

(defun minimax (state depth currentMove)
    (let (
            (lp (returnPossibleStates state))
            (f  (if (string= "X" currentMove) 'max-state 'min-state))
        )
        (cond 
            ( (or (zerop depth) (null lp) ) (list state (getAssessment state)))
            (t (apply f (list (mapcar (lambda (x) (minimax x (1- depth) (if (string= currentMove "X") "O"  "X"))) lp))))
        )
    )
) 

(defun min-state (lsv)
    (min-state-i (cdr lsv) (car lsv))
) 

(defun min-state-i (lsv state-value)
    (cond  
        ( (null lsv) state-value )
        ( (< (cadar lsv) (cadr state-value)) (min-state-i (cdr lsv) (car lsv)) ) 
        ( t (min-state-i (cdr lsv) state-value) )
    )
)

(defun max-state(lsv)
    (max-state-i (cdr lsv) (car lsv))
) 

(defun max-state-i (lsv state-value)
    (cond  
        ( (null lsv) state-value )
        ( (> (cadar lsv) (cadr state-value)) (max-state-i (cdr lsv) (car lsv)) ) 
        ( t (max-state-i (cdr lsv) state-value) )
    )
)

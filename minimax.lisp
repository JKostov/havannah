(setf *random-state* (make-random-state t)) ;Used in random function to generate random number

;; determines how good is the state
(defun getAssessment(state)
    (random 100)
)         

(defun minimax (state depth alpha beta currentPlayer)
    (cond
        ((zerop depth)  (list state (getAssessment state)))
        (t   
            (let (
                    (lp (returnPossibleStates state))
                    (f  (if (string= "X" currentPlayer) 'max-state 'min-state))
                )
                (cond 
                    ( (null lp) (list state (getAssessment state)))
                    ( t (apply f (list lp depth alpha beta currentPlayer '())) )
                )
            )
        )
    )
)

(defun min-state (lp depth alpha beta currentPlayer state)
    (cond
        ( ( null lp) (list state beta) )
        ( t 
            (let* 
                (
                    (maxState (minimax (car lp) (1- depth) alpha beta (if (string= currentPlayer "X") "O"  "X")))
                    (nb (min beta (cadr maxState)))
                    (newState (if (< nb beta) (car lp) state))
                )
                (if  (> nb alpha) (min-state (cdr lp) depth alpha nb currentPlayer newState) (list newState nb) )
            )
        )
    )
)

(defun max-state(lp depth alpha beta currentPlayer state)
    (cond
        ( (null lp) (list state alpha) )
        ( t 
            (let*
                (
                    (minState (minimax (car lp) (1- depth) alpha beta (if (string= currentPlayer "X") "O"  "X")))
                    (na (max alpha (cadr minState)))
                    (newState (if (> na alpha)  (car lp) state))
                )
                (if (< na beta) (max-state (cdr lp) depth na beta currentPlayer newState) (list newState na))
            )
        )
    )
)        

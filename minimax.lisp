(setf *random-state* (make-random-state t)) ;Used in random function to generate random number

;; determines how good is the state
(defun getAssessment(state)
    (random 100)
)         

(defun minimax (state depth alpha beta currentPlayer numberOfCells)
    (cond
        ((zerop depth)  (list state (getAssessment state)))
        (t   
            (let (
                    (lp (returnPossibleStates state currentPlayer numberOfCells))
                    (f  (if (string= "X" currentPlayer) 'max-state 'min-state))
                )
                (cond 
                    ( (null lp) (list state (getAssessment state)))
                    ( t (apply f (list lp depth alpha beta currentPlayer '() numberOfCells)) )
                )
            )
        )
    )
)

(defun min-state (lp depth alpha beta currentPlayer state numberOfCells)
    (cond
        ( ( null lp) (list state beta) )
        ( t 
            (let* 
                (
                    (maxState (minimax (car lp) (1- depth) alpha beta (if (string= currentPlayer "X") "O"  "X") numberOfCells))
                    (nb (min beta (cadr maxState)))
                    (newState (if (< nb beta) (car lp) state))
                )
                (if  (> nb alpha) (min-state (cdr lp) depth alpha nb currentPlayer newState numberOfCells) (list newState nb) )
            )
        )
    )
)

(defun max-state(lp depth alpha beta currentPlayer state numberOfCells)
    (cond
        ( (null lp) (list state alpha) )
        ( t 
            (let*
                (
                    (minState (minimax (car lp) (1- depth) alpha beta (if (string= currentPlayer "X") "O"  "X") numberOfCells))
                    (na (max alpha (cadr minState)))
                    (newState (if (> na alpha)  (car lp) state))
                )
                (if (< na beta) (max-state (cdr lp) depth na beta currentPlayer newState numberOfCells) (list newState na))
            )
        )
    )
)        

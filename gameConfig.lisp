
(defvar *numberOfCells*) ;Global read-only variable that contains the table size
(defvar *firstPlayer*) ;Global read-only variable that contains the first player - Human or Computer
(defvar *currentPlayer* "X") ;Global variable that contains the sign of the current player - X or O

;Function used for initialization of game configuration
(defun gameConfig ()
    (print "Insert number of cells (6-12):")
    (setq *numberOfCells* (read))
    (if (or (< *numberOfCells* 6) (> *numberOfCells* 12)) (setq *numberOfCells* 6) )
    (print "Insert who will be playing first H for human and C for computer (default first human):")
    (setq *firstPlayer* (string (read)))
    (if (not (or (string= *firstPlayer* "H") (string= *firstPlayer* "C"))) (setq *firstPlayer* "H"))
)

;Function used for changing the current player after a valid move
(defun changePlayer ()
    (cond
        ( (equal *currentPlayer* "X") (setq *currentPlayer* "O"))
        (t (setq *currentPlayer* "X"))
    )
)

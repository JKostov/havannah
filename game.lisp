
(defvar *state* '()) ;List used for current game state representation

(defun initRow (numberOfCells realNumberOfCells)
    (cond
        ( (= numberOfCells 64) '() )
        ( t (append (initRow (1- numberOfCells) realNumberOfCells) (list (list (string (code-char numberOfCells)) (initColumn realNumberOfCells) )) ) )
    )
)

(defun initColumn (numberOfCells)
    (cond
        ( (< numberOfCells 0) '() )
        ( t (append (initColumn (1- numberOfCells)) (list (list numberOfCells "-")) ) )
    )
)

(defun initGame (numberOfCells)

    (setq *state* (initRow (+ (* 2 numberOfCells) 63) (- (* 2 numberOfCells) 2) ))
)
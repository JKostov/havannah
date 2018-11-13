
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

(defun printState (state numberOfCells)
    (format t "~%")
    (printEmptySpace (div numberOfCells 2))
    (printNumbersLabel 0 numberOfCells)
    (cond
        ( (null state) state)
        ( t (format t "~%"))
    )
)

(defun printEmptySpace (number)
    (cond
        ( (= number 0) '() )
        ( t (format t " " ) (printEmptySpace (1- number)) )
    )
)

(defun printNumbersLabel (tmp numberOfCells)
    (cond
        ( (> tmp numberOfCells) '() )
        ( t (format t "~a " tmp) (printNumbersLabel (1+ tmp) numberOfCells) )
    )
)

(defun div (x y)
    (/ (- x (mod x y)) y)
)
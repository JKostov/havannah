
(defvar *state* '()) ;List used for current game state representation

(defun initRow (numberOfCells realNumberOfCells start stop)
    (cond
        ( (> numberOfCells (+ 65 (* 2 (1- realNumberOfCells)))) '() )
        ( (not (= stop (* 2 (1- realNumberOfCells)))) (append (initRow (1+ numberOfCells) realNumberOfCells start (1+ stop)) (list (list (string (code-char numberOfCells)) (initColumn start stop) )) )  )
        ( t (append (initRow (1+ numberOfCells) realNumberOfCells (1+ start) stop) (list (list (string (code-char numberOfCells)) (initColumn start stop) )) )  )
    )
)

(defun initColumn (start stop)
    (cond
        ( (> start stop) '() )
        ( t (append (initColumn (1+ start) stop ) (list (list start "-")) ) )
    )
)

(defun initGame (numberOfCells)

    (setq *state* (reverse (initRow 65 numberOfCells 0 (1- numberOfCells))))
    
)

(defun printState (state numberOfCells)
    (format t "~%")
    (printEmptySpace numberOfCells)
    (printNumbersLabel 0 numberOfCells)
    (printRow numberOfCells state)
)

(defun printEmptySpace (number)
    (cond
        ( (= number 0) '() )
        ( t (format t " " ) (printEmptySpace (1- number)) )
    )
)

(defun printNumbersLabel (tmp numberOfCells)
    (cond
        ( (> tmp numberOfCells) (format t "~%") )
        ( t (format t "~a " tmp) (printNumbersLabel (1+ tmp) numberOfCells) )
    )
)

(defun div (x y)
    (/ (- x (mod x y)) y)
)

(defun printRow (rowNumber state)
    (cond
        ( (null state) '() )
        (t (format t "~a " (caar state)) (printEmptySpace (- (* 2 *numberOfCells*) (mod (* 2 *numberOfCells*) rowNumber))) (printNumber rowNumber state (cadar state)) )
    )
)

(defun printNumber (rowNumber state rowList)
    (cond
        ( (null rowList) (if (< rowNumber (- (* 2  *numberOfCells*) 2)) (format t "~a" (1+ rowNumber))) (format t "~%") (printRow (1+ rowNumber) (cdr state)) )
        ( t (format t "~a" (cadar rowList)) (printNumber rowNumber state (cdr rowList)) )
    )
)
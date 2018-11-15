
(defvar *state* '()) ;List used for current game state representation
(defvar *minus*); Used for printing

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
        ( t (append (list (list start "-")) (initColumn (1+ start) stop ) ) )
    )
)

(defun initGame (numberOfCells)

    (setq *state* (reverse (initRow 65 numberOfCells 0 (1- numberOfCells))))
    
)

(defun printGame (state numberOfCells)
    (setq *minus* 1)
    (format t "~%")
    (printEmptySpace (+ 2 numberOfCells))
    (printNumbersLabel 0 (1- numberOfCells))
    (printRow (1- numberOfCells) state numberOfCells)
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

(defun printRow (rowNumber state emptySpaceNumber)
    (cond
        ( (null state) '() )
        (t (format t "~a " (caar state)) (printEmptySpace emptySpaceNumber) (printNumber rowNumber state (cadar state) emptySpaceNumber) )
    )
)

(defun printNumber (rowNumber state rowList emptySpaceNumber)
    (cond
        ( (null rowList) (if (< rowNumber (- (* 2  *numberOfCells*) 2)) (format t "~a" (1+ rowNumber))) (if (= emptySpaceNumber 1) (setq *minus* 0)) (format t "~%") (printRow (1+ rowNumber) (cdr state) (if (= 1 *minus*) (1- emptySpaceNumber) (1+ emptySpaceNumber) )) )
        ( t (format t "~a " (cadar rowList)) (printNumber rowNumber state (cdr rowList) emptySpaceNumber) )
    )
)

(defun enterMove ()
    (format t "~%~a is on the move. Insert your move (A 1):" *currentPlayer*)
    (setq move (read))
    (if (null (checkIfValidAndPlay (car move) (cadr move))) (progn (format t "~%Invalid move!") (enterMove)) (changePlayer) )
)

(defun changePlayer ()
    (cond
        ( (equal *currentPlayer* "X") (setq *currentPlayer* "O"))
        (t (setq *currentPlayer* "X"))
    )
)
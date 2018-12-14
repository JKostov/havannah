(defvar *states* '()) ;List used to store game states based on players moves, format:  ( (("A" ( (1 "-") (2 "-") ... )) ("B" ( (1 "-") (2 "-") ... )) ...) )
(defvar *gameOver* '()) ;Global variable used to signalize when the game is over

;Recursive function used for row initialization (("A" ( (1 "-") (2 "-") ... )) ("B" ( (1 "-") (2 "-") ... )) ...)
;For each row calls initColumn function for row columns initialization
(defun initRow (currentLetter numberOfCells start stop)
    (cond
        ( (> currentLetter (+ 65 (* 2 (1- numberOfCells)))) '() )
        ;while stop < 2 * (numberOfCells - 1) => call initRow with start and stop += 1
        ( (not (= stop (* 2 (1- numberOfCells)))) (append (initRow (1+ currentLetter) numberOfCells start (1+ stop)) (list (list (string (code-char currentLetter)) (initColumn start stop) )) )  )
        ;If stop = 2 * (numbertOfCells - 1) call initRow with start += 1 and stop
        ( t (append (initRow (1+ currentLetter) numberOfCells (1+ start) stop) (list (list (string (code-char currentLetter)) (initColumn start stop) )) )  )
    )
)

;Recursive function used for row columns state initialization: ( (1 "-") (2 "-") )
(defun initColumn (start stop)
    (cond
        ( (> start stop) '() )
        ( t (append (list (list start "-")) (initColumn (1+ start) stop ) ) )
    )
)

;Function that sets the global variable state with the state representation list
(defun initGame (numberOfCells)

    (setq *states* (list (reverse (initRow 65 numberOfCells 0 (1- numberOfCells)))))
)

;Function used for printing the game board
(defun printGame (state numberOfCells)
    (format t "~%")
    ;Prints empty space before numbers label
    (printEmptySpace (+ 2 numberOfCells))
    (printNumbersLabel 0 (1- numberOfCells))
    (printRow (1- numberOfCells) state numberOfCells numberOfCells 1)
)

;Recursive function used for printing empty space
(defun printEmptySpace (number)
    (cond
        ( (= number 0) '() )
        ( t (format t " " ) (printEmptySpace (1- number)) )
    )
)

;Recursive function used for printing numbers label format: tmp tmp+1 tmp+2 ...
(defun printNumbersLabel (tmp numberOfCells)
    (cond
        ( (> tmp numberOfCells) (format t "~%") )
        ( t (format t "~a " tmp) (printNumbersLabel (1+ tmp) numberOfCells) )
    )
)

;Recursive function used for printing one row format: Letter EmptySpace RowColumns
(defun printRow (rowNumber state emptySpaceNumber numberOfCells minus)
    (cond
        ( (null state) '() )
        (t (format t "~a " (caar state)) (printEmptySpace emptySpaceNumber) (printRowColumns rowNumber state (cadar state) emptySpaceNumber numberOfCells (if (or (= emptySpaceNumber 1) (= minus 0) ) 0 1)) )
    )
)

;Recursive function used for printing rows columns
;If minus => emptySpaceNumber -= 1 else emptySpaceNumber += 1
(defun printRowColumns (rowNumber state rowList emptySpaceNumber numberOfCells minus)
    (cond
        ( (null rowList) (if (< rowNumber (- (* 2  numberOfCells) 2)) (format t "~a" (1+ rowNumber))) (format t "~%") (printRow (1+ rowNumber) (cdr state) (if (= 1 minus) (1- emptySpaceNumber) (1+ emptySpaceNumber) ) numberOfCells minus) )
        ( t (format t "~a " (cadar rowList)) (printRowColumns rowNumber state (cdr rowList) emptySpaceNumber numberOfCells minus) )
    )
)

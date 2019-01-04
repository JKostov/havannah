(defvar *edges* '()) ;Global read-only variable that is initialized when the game starts and it contais list with edge elements
(defvar *sides* '()) ;Global read-only variable that is initialized when the game starts and it contains list that has one list per side with side elements

;; generates list of all edges depending on number of cells
(defun generateEdges (numberOfCells)
    (setq *edges* 
        (append
            (list (list "A" 0))
            (list (list "A" (1- numberOfCells)))
            (list (list (string (code-char (+ 65 (1- numberOfCells)))) 0))
            (list (list (string (code-char (+ 65 (1- numberOfCells)))) (* 2 (1- numberOfCells)) ))
            (list (list (string (code-char (+ 65 (* 2 (1- numberOfCells))))) (1- numberOfCells) ))
            (list (list (string (code-char (+ 65 (* 2 (1- numberOfCells))))) (* 2 (1- numberOfCells)) ))
        )
    )
)

;;generates list of sides
(defun generateSides (numberOfCells)
    (setq *sides* 
        (removeAllElements *edges*
            (append
                (list (getSideListPlusNumber (+ 65 numberOfCells) 1 numberOfCells))
                (list (getSideListPlusLetterDown (+ 65 numberOfCells) (* 2 (1- numberOfCells)) numberOfCells))
                (list (generateSideList 65 1 (- numberOfCells 2)))
                (list (getSideListPlusLetter 66 0 numberOfCells))
                (list (getSideListPlusNumberTop 66 numberOfCells numberOfCells))
                (list (generateSideList (+ 65 (* 2 (1- numberOfCells))) numberOfCells (1- (* 2 (1- numberOfCells))) ))
            )
        )
    )
)

;; helper for generating list of sides
;---------------------------------------------------------------------------

(defun generateSideList (numberOfLetter start stop)
    (cond
        ( (> start stop) '() )
        ( t (append (list (list (string (code-char numberOfLetter)) start)) (generateSideList numberOfLetter (1+ start) stop ) ) )
    )
)

(defun getSideListPlusLetter (letter number numberOfCells)
    (cond
        ( (= letter (+ 65 (1- numberOfCells))) '() )
        (t (append (list (list (string (code-char letter)) number)) (getSideListPlusLetter (1+ letter) number numberOfCells) ))
    )
)

(defun getSideListPlusNumber (letter number numberOfCells)
    (cond
        ( (= letter (+ 65 (* 2 (1- numberOfCells)))) '() )
        (t (append (list (list (string (code-char letter)) number)) (getSideListPlusNumber (1+ letter) (1+ number) numberOfCells) ))
    )
)


(defun getSideListPlusNumberTop (letter number numberOfCells)
    (cond
        ( (= letter (+ 65 (1- numberOfCells))) '() )
        (t (append (list (list (string (code-char letter)) number)) (getSideListPlusNumberTop (1+ letter) (1+ number) numberOfCells) ))
    )
)

(defun getSideListPlusLetterDown (letter number numberOfCells)
    (cond
        ( (= letter (+ 65 (* 2 (1- numberOfCells)))) '() )
        (t (append (list (list (string (code-char letter)) number)) (getSideListPlusLetterDown (1+ letter) number numberOfCells) ))
    )
)

;---------------------------------------------------------------

;; group of functions that are used for move to be passed to the list of sides and his side should be removed
;; function is used for fork game over check

(defun removeAllElements (elements list)
    (cond
        ( (null list) list )
        ( (member (car list) elements :test 'equal) (removeAllElements elements (cdr list)) )
        ( t (cons (car list) (removeAllElements elements (cdr list)) ))
    )
)

(defun checkMoveIsSideAndRemoveSide (sides move)
    (cond
        ( (null sides) '() )
        ( (member move (car sides) :test 'equal) (checkMoveIsSideAndRemoveSide (cdr sides) move) )
        ( t (cons (car sides) (checkMoveIsSideAndRemoveSide (cdr sides) move)) )
    )
)

(defun removeSidesForMoves (sides moves)
    (cond
        ( (null moves) sides )
        (t (removeSidesForMoves (checkMoveIsSideAndRemoveSide sides (car moves)) (cdr moves) ))
    )
)